package org.bykn.bosatsu

import cats.data.{Kleisli, Validated, ValidatedNel, NonEmptyList}
import fastparse.all._

import org.bykn.fastparse_cats.StringInstances._
import cats.implicits._

object Parser {
  /**
   * This is an indentation aware
   * parser, the input is the string that
   * should be parsed after a new-line to
   * continue the current indentation block
   */
  type Indy[A] = Kleisli[P, String, A]

  object Indy {
    def apply[A](fn: String => P[A]): Indy[A] =
      Kleisli(fn)

    def lift[A](p: P[A]): Indy[A] =
      Kleisli.liftF(p)

    def suspend[A](ind: => Indy[A]): Indy[A] = {
      lazy val force = ind
      Indy { i => force(i) }
    }

    /**
     * Without parsing anything return
     * the current indentation level
     */
    val currentIndentation: Indy[String] =
      Kleisli.ask

    /**
     * Parse exactly the current indentation
     * starting now
     */
    val parseIndent: Indy[Unit] =
      apply(indent => P(indent))

    val toEOLIndent: Indy[Unit] =
      lift(toEOL) *> parseIndent

    implicit class IndyMethods[A](val toKleisli: Indy[A]) extends AnyVal {
      def region: Indy[(Region, A)] =
        toKleisli.mapF(_.region)

      def ? : Indy[Option[A]] =
        toKleisli.mapF(_.? : P[Option[A]])

      def rep(min: Int = 0, sepIndy: Indy[Unit]): Indy[Seq[A]] =
        Indy { indent =>
          val pa = toKleisli(indent)
          val sep = sepIndy(indent)
          pa.rep(min, sep = sep)
        }

      def nonEmptyList(sepIndy: Indy[Unit]): Indy[NonEmptyList[A]] =
        rep(1, sepIndy).map { as =>
          as.toList match {
            case h :: tail => NonEmptyList(h, tail)
            case Nil => sys.error("rep 1 matched 0")
          }
        }

      def cutThen[B](that: Indy[B]): Indy[(A, B)] =
        Indy { indent =>
          toKleisli(indent) ~/ that(indent)
        }
    }
  }

  sealed trait Error {
    def showContext: Option[String] =
      this match {
        case Error.PartialParse(_, pos, locations) =>
          locations.showContext(pos)
        case Error.ParseFailure(pos, locations) =>
          locations.showContext(pos)
      }
  }

  object Error {
    case class PartialParse[A](got: A, position: Int, locations: LocationMap) extends Error
    case class ParseFailure(position: Int, locations: LocationMap) extends Error
  }

  def parse[A](p: P[A], str: String): ValidatedNel[Error, (LocationMap, A)] = {
    val lm = LocationMap(str)
    p.parse(str) match {
      case Parsed.Success(a, idx) if idx == str.length =>
        Validated.valid((lm, a))
      case Parsed.Success(a, idx) =>
        Validated.invalidNel(Error.PartialParse(a, idx, lm))
      case Parsed.Failure(_, idx, _) =>
        Validated.invalidNel(Error.ParseFailure(idx, lm))
    }
  }

  def inRange(lower: Char, c: Char, upper: Char): Boolean =
    (lower <= c) && (c <= upper)

  def isNum(c: Char): Boolean =
    inRange('0', c, '9')

  def isLower(c: Char): Boolean =
    inRange('a', c, 'z')

  def isUpper(c: Char): Boolean =
    inRange('A', c, 'Z')

  def isSpace(c: Char): Boolean =
    (c == ' ') | (c == '\t')

  def identifierChar(c: Char): Boolean =
    isNum(c) || isUpper(c) || isLower(c) || (c == '_')

  val spaces: P[Unit] = P(CharsWhile(isSpace _))
  val nonSpaces: P[String] = P(CharsWhile { c => !isSpace(c) }.!)
  val maybeSpace: P[Unit] = spaces.?

  /** prefer to parse Right, then Left
   */
  def either[A, B](pb: P[B], pa: P[A]): P[Either[B, A]] =
    pa.map(Right(_)) | pb.map(Left(_))

  def maybeIndentedOrSpace(indent: String): P[Unit] =
    (Parser.spaces | P("\n" ~ indent)).rep().map(_ => ())

  val spacesAndLines: P[Unit] = P(CharsWhile { c =>
    c.isWhitespace
  }).opaque("spacesAndLines")

  val maybeSpacesAndLines: P[Unit] =
    spacesAndLines.?.opaque("maybeSpacesAndLines")

  val lowerIdent: P[String] =
    P(CharIn('a' to 'z').! ~ CharsWhile(identifierChar _).?.!)
      .map { case (a, b) => a + b }

  val upperIdent: P[String] =
    P(CharIn('A' to 'Z').! ~ CharsWhile(identifierChar _).?.!)
      .map { case (a, b) => a + b }

  def tokenP[T](s: String, t: T): P[T] = P(s).map(_ => t)

  /**
   * This parser allows _ between any two digits to allow
   * literals such as:
   * 1_000_000
   *
   * It will also parse terrible examples like:
   * 1_0_0_0_0_0_0
   * but I think banning things like that shouldn't
   * be done by the parser
   */
  val integerString: P[String] = {

    val digit1 = CharIn('1' to '9')
    val digit0 = CharIn('0' to '9')
    val rest = P("_".? ~ digit0).rep()
    val nonZero: P[String] = P(digit1 ~ rest).!

    val positive: P[String] = tokenP("0", "0") | nonZero
    P(CharIn("+-").!.? ~ positive)
      .map {
        case (None, rest) => rest
        case (Some(s), rest) => s + rest
      }
  }

  def escapedString(q: Char): P[String] =
    StringUtil.escapedString(q)

  def escape(quoteChar: Char, str: String): String =
    StringUtil.escape(quoteChar, str)

  def unescape(str: String): Either[Int, String] =
    StringUtil.unescape(str)

  def nonEmptyListToList[T](p: P[NonEmptyList[T]]): P[List[T]] =
    p.?.map {
      case None => Nil
      case Some(ne) => ne.toList
    }

  /**
   * Parse python-like dicts: delimited by curlies "{" "}" and
   * keys separated by colon
   */
  def dictLikeParser[K, V](pkey: P[K], pvalue: P[V]): P[List[(K, V)]] = {
    val ws = maybeSpacesAndLines
    val kv = P(pkey ~ ws ~ ":" ~ ws ~ pvalue)
    val kvs = kv.nonEmptyListOfWs(ws, 1)
    nonEmptyListToList(kvs)
      .bracketed(P("{" ~ ws), P(ws ~ "}"))
  }

  implicit class Combinators[T](val item: P[T]) extends AnyVal {
    def list: P[List[T]] = listN(0)

    def listN(min: Int): P[List[T]] =
      if (min == 0) nonEmptyListToList(nonEmptyList)
      else nonEmptyListOf(min).map(_.toList)

    def nonEmptyList: P[NonEmptyList[T]] = nonEmptyListOf(1)

    def nonEmptyListOf(min: Int): P[NonEmptyList[T]] =
      nonEmptyListOfWs(maybeSpace, min)

    def nonEmptyListOfWs(ws: P[Unit], min: Int): P[NonEmptyList[T]] =
      nonEmptyListOfWsSep(ws, P(","), allowTrailing = true, min)

    def maybeAp(fn: P[T => T]): P[T] =
      (item ~ fn.?)
        .map {
          case (a, None) => a
          case (a, Some(f)) => f(a)
        }

    def nonEmptyListOfWsSep(ws: P[Unit], sep: P[Unit], allowTrailing: Boolean, min: Int): P[NonEmptyList[T]] = {
      require(min >= 1, s"min is too small: $min")
      val wsSep = ws ~ sep ~ ws
      val many = item.rep(sep = wsSep, min = min - 1)
      val trail = if (allowTrailing) (ws ~ sep).? else Pass
      P(item ~ (wsSep ~ many).? ~ trail)
        .map {
          case (h, None) => NonEmptyList(h, Nil)
          case (h, Some(nel)) => NonEmptyList(h, nel.toList)
        }
    }

    def bracketed(left: P[Unit], right: P[Unit]): P[T] =
      left ~ item ~ right

    def nonEmptyListSyntax: P[NonEmptyList[T]] = {
      val ws = maybeSpacesAndLines
      nonEmptyListOfWs(ws, 1).bracketed(P("[" ~ ws), P(ws ~ "]"))
    }

    def listSyntax: P[List[T]] = {
      val ws = maybeSpacesAndLines
      nonEmptyListToList(nonEmptyListOfWs(ws, 1))
        .bracketed(P("[" ~ ws), P(ws ~ "]"))
    }

    def region: P[(Region, T)] =
      P(Index ~ item ~ Index).map { case (s, t, e) =>
        (Region(s, e), t)
      }

    def wrappedSpace(left: P[Unit], right: P[Unit]): P[T] =
      P(left ~ maybeSpace ~ item ~ maybeSpace ~ right)

    def parens: P[T] =
      wrappedSpace("(", ")")

    def parensLines1: P[NonEmptyList[T]] = {
      val nel = item.nonEmptyListOfWs(maybeSpacesAndLines, 1)
      P("(" ~ maybeSpacesAndLines ~ nel ~ maybeSpacesAndLines ~ ")")
    }

    def parensLines1Cut: P[NonEmptyList[T]] = {
      val nel = item.nonEmptyListOfWs(maybeSpacesAndLines, 1)
      P("(" ~/ maybeSpacesAndLines ~ nel ~ maybeSpacesAndLines ~ ")")
    }

    /**
     * Parse a python-like tuple or a parens
     */
    def tupleOrParens: P[Either[T, List[T]]] = {
      val ws = maybeSpacesAndLines
      val single = item ~ ws
      val sep = P("," ~ ws)
      val twoAndMore = (item ~ ws).rep(sep = sep)
      val trailing = sep

      (single ~ (sep ~ twoAndMore.? ~ trailing.?).?).?
        .map {
          case None => Right(Nil)
          case Some((h, None)) => Left(h)
          case Some((h, Some(None))) => Right(h :: Nil)
          case Some((h, Some(Some(tail)))) => Right(h :: tail.toList)
        }
        .bracketed(P("(" ~ ws), P(ws ~ ")"))

    }
  }

  /**
   * Parse until the end of a line or to end of file.
   * WARNING: never use this with .rep because
   * repeatedly parsing End will OOM
   */
  val toEOL: P[Unit] = P(maybeSpace ~ ("\n" | End))
}
