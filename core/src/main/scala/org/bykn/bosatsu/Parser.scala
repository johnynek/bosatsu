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
      (first <* lift(P(sep ~/ maybeSpace)))
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
    isSpace(c) || (c == '\n' || c == '\r')
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
    val nonZero: P[String] = P(CharIn('1' to '9').! ~ (CharsWhile(isNum _).!.?))
      .map {
        case (f, None) => f
        case (f, Some(r)) => f + r
      }

    val positive: P[String] = tokenP("0", "0") | nonZero
    P(CharIn("+-").!.? ~ positive)
      .map {
        case (None, rest) => rest
        case (Some(s), rest) => s + rest
      }
  }

  def escapedString(q: Char): P[String] = {
    val qstr = q.toString
    val char = P((!qstr ~ AnyChar) | ("\\" ~ AnyChar)).!
    P(qstr ~ char.rep() ~ qstr).map(_.mkString)
  }

  def escape(chars: Set[Char], str: String): String = {
    val escaped = chars + '\\'
    if (str.exists(escaped)) {
      str.flatMap {
        case c if chars(c) => s"\\$c"
        case c => c.toString
      }
    }
    else str
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
      val many = P(("," ~ ws ~ item ~ ws).rep())
      P(item ~ ws ~ many.? ~ (",".?))
        .map {
          case (h, None) => NonEmptyList(h, Nil)
          case (h, Some(nel)) => NonEmptyList(h, nel.toList)
        }
    }

    def bracketed(left: P[Unit], right: P[Unit]): P[T] =
      left ~ item ~ right

    def nonEmptyListSyntax: P[NonEmptyList[T]] = {
      val ws = P(CharsWhile(_.isWhitespace)).?
      nonEmptyListOfWs(ws, 1).bracketed(P("[" ~/ ws), P(ws ~ "]"))
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
      P("(" ~/ maybeSpace ~ item ~ maybeSpace ~ ")")
  }

  val toEOL: P[Unit] = P(maybeSpace ~ "\n")
}
