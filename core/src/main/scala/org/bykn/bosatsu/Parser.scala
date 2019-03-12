package org.bykn.bosatsu

import cats.data.{Kleisli, Validated, ValidatedNel, NonEmptyList}
import fastparse.all._
import java.nio.file.{Files, Path}
import scala.util.{ Failure, Success, Try }

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

    /**
     * A: B or
     * A:
     *   B
     */
    def block[A, B](first: Indy[A], next: Indy[B]): Indy[(A, OptIndent[B])] =
      blockLike(first, next, ":")

    def blockLike[A, B](first: Indy[A], next: Indy[B], sep: String): Indy[(A, OptIndent[B])] =
      (first <* lift(P(sep ~ maybeSpace)))
        .product(OptIndent.indy(next))

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
    }
  }

  sealed trait Error {
    def showContext: Option[String] =
      this match {
        case Error.PartialParse(_, pos, loc, _) =>
          loc.showContext(pos)
        case Error.ParseFailure(pos, loc, _) =>
          loc.showContext(pos)
        case Error.FileError(_, _) =>
          None
      }
  }
  object Error {
     case class PartialParse[A](got: A, position: Int, locations: LocationMap, path: Option[Path]) extends Error
     case class ParseFailure(position: Int, locations: LocationMap, path: Option[Path]) extends Error
     case class FileError(readPath: Path, error: Throwable) extends Error
  }

  def parse[A](p: P[A], str: String): ValidatedNel[Error, (LocationMap, A)] = {
    val lm = LocationMap(str)
    p.parse(str) match {
      case Parsed.Success(a, idx) if idx == str.length =>
        Validated.valid((lm, a))
      case Parsed.Success(a, idx) =>
        Validated.invalidNel(Error.PartialParse(a, idx, lm, None))
      case Parsed.Failure(_, idx, _) =>
        Validated.invalidNel(Error.ParseFailure(idx, lm, None))
    }
  }

  def parseFile[A](p: P[A], path: Path): ValidatedNel[Error, (LocationMap, A)] =
    Try(new String(Files.readAllBytes(path), "utf-8")) match {
      case Success(str) => parse(p, str).leftMap { nel =>
        nel.map {
          case pp@Error.PartialParse(_, _, _, _) => pp.copy(path = Some(path))
          case pf@Error.ParseFailure(_, _, _) => pf.copy(path = Some(path))
          case other => other
        }
      }
      case Failure(err) => Validated.invalidNel(Error.FileError(path, err))
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


  private val decodeTable: Map[Char, Char] =
    Map(
      ('\\', '\\'),
      ('\'', '\''),
      ('\"', '\"'),
      ('a', 7.toChar), // bell
      ('b', 8.toChar), // backspace
      ('f', 12.toChar), // form-feed
      ('n', '\n'),
      ('r', '\r'),
      ('t', '\t'),
      ('v', 11.toChar)) // vertical tab

  private val encodeTable = decodeTable.iterator.map { case (v, k) => (k, s"\\$v") }.toMap

  private val nonPrintEscape: Array[String] =
    (0 until 32).map { c =>
      val strHex = c.toHexString
      val strPad = List.fill(4 - strHex.length)('0').mkString
      s"\\u$strPad$strHex"
    }.toArray

  private val escapeString: P[Unit] = {
    val escapes = CharIn(decodeTable.keys.toSeq)
    val oct = CharIn('0' until '8')
    val hex = CharIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'))
    val octP = P("o" ~ oct ~ oct)
    val hexP = P("x" ~ hex ~ hex)
    val u4 = P("u" ~ hex.rep(4))
    val u8 = P("U" ~ hex.rep(8))
    val after = escapes | octP | hexP | u4 | u8
    P("\\" ~ after)
  }

  def escapedString(q: Char): P[String] = {
    val qstr = q.toString
    val char = P(escapeString | (!qstr ~ AnyChar)).!
    P(qstr ~ char.rep() ~ qstr).map(_.mkString)
      .flatMap { str =>
        unescape(str) match {
          case Right(str1) => PassWith(str1)
          case Left(_) => Fail
        }
      }
  }

  def escape(quoteChar: Char, str: String): String = {
    // We can ignore escaping the opposite character used for the string
    // x isn't escaped anyway and is kind of a hack here
    val ignoreEscape = if (quoteChar == '\'') '"' else if (quoteChar == '"') '\'' else 'x'
    str.flatMap { c =>
      if (c == ignoreEscape) c.toString
      else encodeTable.get(c) match {
        case None =>
          if (c < ' ') nonPrintEscape(c.toInt)
          else c.toString
        case Some(esc) => esc
      }
    }
  }

  def unescape(str: String): Either[Int, String] = {
    val sb = new java.lang.StringBuilder
    def decodeNum(idx: Int, size: Int, base: Int): Int = {
      val end = idx + size
      if (end <= str.length) {
        val intStr = str.substring(idx, end)
        val asInt =
          try Integer.parseInt(intStr, base)
          catch { case _: NumberFormatException => ~idx }
        sb.append(asInt.toChar)
        end
      } else ~(str.length)
    }
    @annotation.tailrec
    def loop(idx: Int): Option[Int] =
      if (idx >= str.length) None
      else if (idx < 0) Some(~idx) // error from decodeNum
      else {
        val c0 = str.charAt(idx)
        if (c0 != '\\') {
          sb.append(c0)
          loop(idx + 1)
        }
        else {
          val nextIdx = idx + 1
          if (nextIdx >= str.length) Some(idx)
          else {
            val c = str.charAt(nextIdx)
            decodeTable.get(c) match {
              case Some(d) =>
                sb.append(d)
                loop(idx + 2)
              case None =>
                c match {
                  case 'o' => loop(decodeNum(idx + 2, 2, 8))
                  case 'x' => loop(decodeNum(idx + 2, 2, 16))
                  case 'u' => loop(decodeNum(idx + 2, 4, 16))
                  case 'U' => loop(decodeNum(idx + 2, 8, 16))
                  case _ => Some(idx)
                }
            }
          }
        }
      }

    loop(0) match {
      case None => Right(sb.toString)
      case Some(err) => Left(err)
    }
  }

  def nonEmptyListToList[T](p: P[NonEmptyList[T]]): P[List[T]] =
    p.?.map {
      case None => Nil
      case Some(ne) => ne.toList
    }

  implicit class Combinators[T](val item: P[T]) extends AnyVal {
    def list: P[List[T]] = listN(0)

    def listN(min: Int): P[List[T]] =
      if (min == 0) nonEmptyListToList(nonEmptyList)
      else nonEmptyListOf(min).map(_.toList)

    def nonEmptyList: P[NonEmptyList[T]] = nonEmptyListOf(1)

    def nonEmptyListOf(min: Int): P[NonEmptyList[T]] =
      nonEmptyListOfWs(maybeSpace, min)

    def nonEmptyListOfWs(ws: P[Unit], min: Int): P[NonEmptyList[T]] = {
      require(min >= 1, s"min is too small: $min")
      val many = P(("," ~ ws ~ item ~ ws).rep(min = min - 1))
      P(item ~ ws ~ many.? ~ (",".?))
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

  val toEOL: P[Unit] = P(maybeSpace ~ "\n")
}
