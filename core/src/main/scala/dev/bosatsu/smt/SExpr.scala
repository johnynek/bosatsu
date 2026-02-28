package dev.bosatsu.smt

sealed trait SExpr derives CanEqual
object SExpr {
  final case class Atom(value: String) extends SExpr
  final case class List(values: Vector[SExpr]) extends SExpr
}

object SExprParser {
  import SExpr._

  sealed trait ParseError {
    def message: String
  }
  object ParseError {
    final case class UnexpectedEOF(context: String) extends ParseError {
      def message: String = s"unexpected EOF while parsing $context"
    }
    final case class UnexpectedChar(found: Char, at: Int, expected: String)
        extends ParseError {
      def message: String =
        s"unexpected character '$found' at index $at, expected $expected"
    }
    final case class EmptyAtom(at: Int) extends ParseError {
      def message: String = s"empty atom at index $at"
    }
  }

  def parseAll(input: String): Either[ParseError, Vector[SExpr]] = {
    val p = new Parser(input)
    p.parseAll()
  }

  private final class Parser(input: String) {
    private val len: Int = input.length
    private var idx: Int = 0

    private inline def eof: Boolean = idx >= len
    private inline def curr: Char = input.charAt(idx)

    private def skipWhitespaceAndComments(): Unit = {
      var keepGoing = true
      while (keepGoing && !eof) {
        if (curr.isWhitespace) idx += 1
        else if (curr == ';') {
          while (!eof && curr != '\n') idx += 1
        } else keepGoing = false
      }
    }

    def parseAll(): Either[ParseError, Vector[SExpr]] = {
      val b = Vector.newBuilder[SExpr]
      skipWhitespaceAndComments()
      while (!eof) {
        parseOne() match {
          case Left(err) =>
            return Left(err)
          case Right(sexpr) =>
            b += sexpr
            skipWhitespaceAndComments()
        }
      }
      Right(b.result())
    }

    private def parseOne(): Either[ParseError, SExpr] = {
      skipWhitespaceAndComments()
      if (eof) Left(ParseError.UnexpectedEOF("s-expression"))
      else {
        curr match {
          case '(' => parseList()
          case ')' =>
            Left(ParseError.UnexpectedChar(')', idx, "atom or '('"))
          case _ =>
            parseAtom()
        }
      }
    }

    private def parseList(): Either[ParseError, SExpr] = {
      idx += 1 // consume '('
      skipWhitespaceAndComments()
      val b = Vector.newBuilder[SExpr]
      while (!eof && curr != ')') {
        parseOne() match {
          case Left(err)   => return Left(err)
          case Right(item) => b += item
        }
        skipWhitespaceAndComments()
      }
      if (eof) Left(ParseError.UnexpectedEOF("list"))
      else {
        idx += 1 // consume ')'
        Right(List(b.result()))
      }
    }

    private def parseAtom(): Either[ParseError, SExpr] = {
      val start = idx
      if (curr == '|') parseBarQuotedAtom(start)
      else if (curr == '"') parseStringAtom(start)
      else {
        while (!eof && !curr.isWhitespace && curr != '(' && curr != ')' && curr != ';')
          idx += 1
        if (idx == start) Left(ParseError.EmptyAtom(start))
        else Right(Atom(input.substring(start, idx)))
      }
    }

    private def parseBarQuotedAtom(start: Int): Either[ParseError, SExpr] = {
      idx += 1 // opening '|'
      var closed = false
      while (!eof && !closed) {
        if (curr == '|') {
          idx += 1
          closed = true
        } else idx += 1
      }
      if (!closed) Left(ParseError.UnexpectedEOF("bar-quoted atom"))
      else Right(Atom(input.substring(start, idx)))
    }

    private def parseStringAtom(start: Int): Either[ParseError, SExpr] = {
      idx += 1 // opening '"'
      var closed = false
      while (!eof && !closed) {
        curr match {
          case '\\' =>
            idx += 1
            if (!eof) idx += 1
          case '"'  =>
            idx += 1
            closed = true
          case _    =>
            idx += 1
        }
      }
      if (!closed) Left(ParseError.UnexpectedEOF("string atom"))
      else Right(Atom(input.substring(start, idx)))
    }
  }
}
