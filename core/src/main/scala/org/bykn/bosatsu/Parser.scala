package org.bykn.bosatsu

import cats.data.{Kleisli, Validated, ValidatedNel, NonEmptyList}
import org.typelevel.paiges.Doc

import org.bykn.bosatsu.parser.{Parser => P, Parser1 => P1}

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

      def cutLeftP(that: P[Any]): Indy[A] =
        Indy { indent =>
          (toKleisli(indent) ~/ that).map(_._1)
        }

      def cutRight[B](that: Indy[B]): Indy[B] =
        cutThen(that).map(_._2)


      /**
       * This optionally allows extra indentation that starts now
       */
      def maybeMore: Parser.Indy[A] =
        Parser.Indy { indent =>
          // run this one time, not each spaces are parsed
          val noIndent = toKleisli.run(indent)
          val someIndent = Parser
            .spaces.!
            .flatMap { thisIndent =>
              toKleisli.run(indent + thisIndent)
            }

          someIndent | noIndent
        }
    }
  }

  sealed trait Error {
    def showContext(errColor: LocationMap.Colorize): Option[Doc] =
      this match {
        case Error.PartialParse(_, pos, locations) =>
          locations.showContext(pos, 2, errColor)
        case Error.ParseFailure(pos, locations) =>
          locations.showContext(pos, 2, errColor)
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

  // used to parse possibly empty indentation
  def indentation(str: String): P[Unit] =
    if (str.isEmpty) P.unit
    else P.expect(str).void

  // parse one or more space characters
  val spaces: P1[Unit] = P.charsWhile1(isSpace _).void
  val nonSpaces: P1[String] = P.charsWhile1 { c => !isSpace(c) }
  val maybeSpace: P[Unit] = spaces.?.void

  /** prefer to parse Right, then Left
   */
  def either[A, B](pb: P[B], pa: P[A]): P[Either[B, A]] =
    pa.map(Right(_)) | pb.map(Left(_))

  def maybeIndentedOrSpace(indent: String): P[Unit] =
    spaces.orElse(P.expect("\n" + indent)).rep.void

  val spacesAndLines: P1[Unit] =
    P.charsWhile1(_.isWhitespace)

  val maybeSpacesAndLines: P[Unit] =
    spacesAndLines.?.void

  val lowerIdent: P1[String] =
    (P.charIn('a' to 'z') ~ P.charsWhile(identifierChar _)).string

  val upperIdent: P1[String] =
    (P.charIn('A' to 'Z') ~ P.charsWhile(identifierChar _)).string

  val py2Ident: P1[String] =
    (P.charIn('_' :: ('A' to 'Z').toList ::: ('a' to 'z').toList) ~ P.charsWhile(identifierChar _)).string

  // requires a string longer than 1
  def tokenP[T](s: String, t: T): P1[T] =
    P.expect(s).as(t)

  val digit19: P1[Char] = P.charIn('1' to '9')
  val digit09: P1[Char] = P.charIn('0' to '9')
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
  val integerString: P1[String] = {

    val rest = (P.char('_').?.with1 ~ digit09).rep
    val nonZero: P1[Unit] = (digit19 ~ rest).void

    val positive: P1[Unit] = P.char('0').orElse1(nonZero)
    (P.charIn("+-") ~ positive).string
  }

  object JsonNumber {
    /**
     *  from: https://tools.ietf.org/html/rfc4627
     *     number = [ minus ] int [ frac ] [ exp ]
     *     decimal-point = %x2E       ; .
     *     digit1-9 = %x31-39         ; 1-9
     *     e = %x65 / %x45            ; e E
     *     exp = e [ minus / plus ] 1*DIGIT
     *     frac = decimal-point 1*DIGIT
     *     int = zero / ( digit1-9 *DIGIT )
     *     minus = %x2D               ; -
     *     plus = %x2B                ; +
     *     zero = %x30                ; 0
     */
    val digits: P[Unit] = digit09.rep.void
    val digits1: P1[Unit] = digit09.rep1.void
    val int: P1[Any] = P.char('0').orElse(digit19 ~ digits)
    val frac: P1[Any] = P.char('.') ~ digits1
    val exp: P1[Unit] = (P.charIn("eE") ~ P.charIn("+-").? ~ digits1).void

    val parser: P1[String] =
      (P.char('-').? ~ int ~ frac.? ~ exp.?).string

    // this gives you the individual parts of a floating point string
    case class Parts(negative: Boolean, leftOfPoint: String, floatingPart: String, exp: String) {
      def asString: String = {
        val neg = if (negative) "-" else ""
        s"$neg$leftOfPoint$floatingPart$exp"
      }
    }

    val partsParser: P1[Parts] =
      (P.char('-').as(true).?.with1 ~ int.string ~ frac.string.? ~ exp.string.?)
        .map { case (((optNeg, left), float), exp) =>
          Parts(optNeg.isDefined, left, float.getOrElse(""), exp.getOrElse(""))
        }
  }

  def escapedString(q: Char): P1[String] =
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
    val kvs = kv.nonEmptyListOfWs(ws)
    nonEmptyListToList(kvs)
      .bracketed(P("{" ~ ws), P(ws ~ "}"))
  }

  implicit class Combinators[T](val item: P[T]) extends AnyVal {
    def list: P[List[T]] =
      nonEmptyListToList(nonEmptyList)

    def nonEmptyList: P[NonEmptyList[T]] =
      nonEmptyListOfWs(maybeSpace)

    def nonEmptyListOfWs(ws: P[Unit]): P[NonEmptyList[T]] =
      nonEmptyListOfWsSep(ws, P(","), allowTrailing = true)

    def maybeAp(fn: P[T => T]): P[T] =
      (item ~ fn.?)
        .map {
          case (a, None) => a
          case (a, Some(f)) => f(a)
        }

    def nonEmptyListOfWsSep(ws: P[Unit], sep: P[Unit], allowTrailing: Boolean): P[NonEmptyList[T]] = {
      val wsSep = ws ~ sep ~ ws
      val rest = {
          // if min == 1, many can parse 0, but that allows
          // wsSep to be parsed by itselt, which isn't okay
          val many1 = item.rep(sep = wsSep, min = 1)
          (wsSep ~ many1).?
        }
      val trail = if (allowTrailing) (ws ~ sep).? else Pass
      P(item ~ rest ~ trail)
        .map {
          case (h, None) => NonEmptyList(h, Nil)
          case (h, Some(nel)) => NonEmptyList(h, nel.toList)
        }
    }

    def bracketed(left: P[Unit], right: P[Unit]): P[T] =
      left ~ item ~ right

    def nonEmptyListSyntax: P[NonEmptyList[T]] = {
      val ws = maybeSpacesAndLines
      nonEmptyListOfWs(ws).bracketed(P("[" ~/ ws), P(ws ~ "]"))
    }

    def listSyntax: P[List[T]] = {
      val ws = maybeSpacesAndLines
      nonEmptyListToList(nonEmptyListOfWs(ws))
        .bracketed(P("[" ~/ ws), P(ws ~ "]"))
    }

    def region: P[(Region, T)] =
      P(Index ~ item ~ Index).map { case (s, t, e) =>
        (Region(s, e), t)
      }

    def wrappedSpace(left: P[Unit], right: P[Unit]): P[T] =
      P(left ~ maybeSpace ~ item ~ maybeSpace ~ right)

    def parens: P[T] =
      wrappedSpace("(", ")")

    def parensCut: P[T] =
      P("(" ~/ maybeSpacesAndLines ~ item ~ maybeSpacesAndLines ~ ")")

    def parensLines1: P[NonEmptyList[T]] = {
      val nel = item.nonEmptyListOfWs(maybeSpacesAndLines)
      P("(" ~ maybeSpacesAndLines ~ nel ~ maybeSpacesAndLines ~ ")")
    }

    def parensLines1Cut: P[NonEmptyList[T]] = {
      val nel = item.nonEmptyListOfWs(maybeSpacesAndLines)
      P("(" ~/ maybeSpacesAndLines ~ nel ~ maybeSpacesAndLines ~ ")")
    }

    /**
     * either: a, b, c, ..
     * or (a, b, c, ) where we allow newlines:
     * return true if we do have parens
     */
    def itemsMaybeParens: P[(Boolean, NonEmptyList[T])] = {
      val withP = item.parensLines1Cut.map((true, _))
      val noP = item.nonEmptyListOfWs(maybeSpace).map((false, _))
      withP | noP
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

  val newline: P1[Unit] = P.void(P.charIn('\n'))
  val toEOL: P[Unit] = (maybeSpace ~ newline.orElse(P.end)).void

  def optionParse[A](pa: P[A], str: String): Option[A] =
    pa.parse(str) match {
      case Right(("", a)) => Some(a)
      case _ => None
    }

  def unsafeParse[A](pa: P[A], str: String): A =
    pa.parse(str) match {
      case Right(("", a)) => a
      // $COVERAGE-OFF$
      case Right((rest, _)) =>
        sys.error(s"partial parse of $str ignores: $rest")
      case Left(err) =>
        val idx = err.failedAtOffset
        sys.error(s"failed to parse: $str: at $idx: (${str.substring(idx)}) with errors: ${err.expected}")
      // $COVERAGE-ON$
    }

}
