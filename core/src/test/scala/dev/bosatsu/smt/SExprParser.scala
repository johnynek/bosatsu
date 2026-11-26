package dev.bosatsu.smt

import cats.parse.{Parser => P, Parser0 => P0}

object SExprParser {
  import SExpr._

  sealed trait ParseError {
    def message: String
  }
  object ParseError {
    final case class InvalidInput(
        offset: Int,
        expected: String,
        details: String
    ) extends ParseError {
      def message: String =
        s"parse error at index $offset; expected: $expected; details: $details"
    }
  }

  private def isBareAtomChar(ch: Char): Boolean =
    (!ch.isWhitespace) && (ch != '(') && (ch != ')') && (ch != ';')

  private val ws: P[Unit] =
    P.charWhere(_.isWhitespace).void

  private val comment: P[Unit] =
    (P.char(';') *> P.charsWhile0(_ != '\n')).void

  private val ignored: P0[Unit] =
    (ws.orElse(comment)).rep0.void

  private val bareAtom: P[SExpr] =
    P.charsWhile(isBareAtomChar).map(Atom(_))

  private val escapedBar: P[Unit] =
    (P.char('|') ~ P.char('|')).void.backtrack

  private val nonBarChunk: P[Unit] =
    P.charsWhile(_ != '|').void

  private val barQuotedInnerChunk: P[Unit] =
    escapedBar.orElse(nonBarChunk)

  private val barQuotedAtom: P[SExpr] =
    (P.char('|') *> barQuotedInnerChunk.rep0 <* P.char('|')).string.map(Atom(_))

  private val escapedStringChunk: P[String] =
    (P.char('\\') ~ P.anyChar).string

  private val plainStringChunk: P[String] =
    P.charsWhile(ch => (ch != '"') && (ch != '\\'))

  private val stringInner: P0[Unit] =
    (escapedStringChunk.orElse(plainStringChunk)).rep0.void

  private val stringAtom: P[SExpr] =
    (P.char('"') *> stringInner <* P.char('"')).string.map(Atom(_))

  private lazy val sexpr: P[SExpr] =
    P.defer(list.orElse(barQuotedAtom).orElse(stringAtom).orElse(bareAtom))

  private lazy val list: P[SExpr] =
    ((P.char('(') *> ignored) *> (sexpr <* ignored).rep0 <* P.char(')')).map { xs =>
      List(xs.toVector)
    }

  private val topLevel: P0[Vector[SExpr]] =
    (ignored *> (sexpr <* ignored).rep0 <* P.end).map(_.toVector)

  def parseAll(input: String): Either[ParseError, Vector[SExpr]] =
    topLevel.parseAll(input).left.map { err =>
      ParseError.InvalidInput(
        offset = err.failedAtOffset,
        expected = err.expected.toString,
        details = err.toString
      )
    }
}
