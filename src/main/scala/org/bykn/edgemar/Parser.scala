package org.bykn.edgemar

import cats.data.NonEmptyList
import fastparse.all._

object Parser {
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

  val spaces: P[Unit] = P(CharsWhile(isSpace _))
  val maybeSpace: P[Unit] = spaces.?

  val lowerIdent: P[String] =
    P(CharIn('a' to 'z').! ~ CharsWhile(c => isNum(c) || isUpper(c) || isLower(c)).?.!)
      .map { case (a, b) => a + b }

  val upperIdent: P[String] =
    P(CharIn('A' to 'Z').! ~ CharsWhile(c => isNum(c) || isUpper(c) || isLower(c)).?.!)
      .map { case (a, b) => a + b }

  def tokenP[T](s: String, t: T): P[T] = P(s).map(_ => t)

  def integerString: P[String] = {
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

  implicit class Combinators[T](val item: P[T]) extends AnyVal {
    def list: P[List[T]] = listN(0)

    def listN(min: Int): P[List[T]] =
      if (min == 0) {
        nonEmptyList.?
          .map {
            case None => Nil
            case Some(nel) => nel.toList
          }
      } else nonEmptyListOf(min).map(_.toList)

    def nonEmptyList: P[NonEmptyList[T]] = nonEmptyListOf(1)

    def nonEmptyListOf(min: Int): P[NonEmptyList[T]] = {
      require(min >= 1, s"min is too small: $min")
      val many = P(("," ~ maybeSpace ~ item ~ maybeSpace).rep())
      P(item ~ maybeSpace ~ many.? ~ (",".?))
        .map {
          case (h, None) => NonEmptyList(h, Nil)
          case (h, Some(nel)) => NonEmptyList(h, nel.toList)
        }
    }

    def trailingSpace: P[T] =
      P(item ~ maybeSpace)

    def wrappedSpace(left: P[Unit], right: P[Unit]): P[T] =
      P(left ~ maybeSpace ~ item ~ maybeSpace ~ right)

    def parens: P[T] =
      wrappedSpace("(", ")")
  }


  val variable: P[Expr.Var] =
    P(CharIn('a' to 'z').! ~ CharsWhile { c => isLower(c) || isUpper(c) }.!.?)
      .map {
        case (a, None) => Expr.Var(a)
        case (a, Some(b)) => Expr.Var(a + b)
      }

  val operatorParse: P[Operator] =
    tokenP("+", Operator.Plus) |
      tokenP("-", Operator.Sub) |
      tokenP("*", Operator.Mul) |
      tokenP("==", Operator.Eql)


  val toEOL: P[Unit] = P(maybeSpace ~ "\n")

  // We assume indent was already parsed
  def ifParser(indent: String): P[Expr.If] = {
    val ifPart = P("if" ~ spaces ~/ expr ~ maybeSpace ~ ":\n")
    val thenPart = P(indent ~ spaces.! ~ expr ~ toEOL)
    val elseP = P(indent ~ "else" ~ maybeSpace ~/ ":" ~ toEOL)
    def elsePart(s: String) = P(indent ~ s ~ expr)
    P(ifPart ~ thenPart ~ elseP).flatMap {
      case (e1, (spaces, e2)) =>
        elsePart(spaces).map { Expr.If(e1, e2, _) }
    }
  }

  // We assume indent was already parsed
  def letParser(indent: String): P[Expr.Let] = {
    val line1 = P(variable ~ maybeSpace ~ "=" ~ (!"=") ~ maybeSpace ~/ expr ~ toEOL)
    val rest = P((indent ~ "\n").rep ~ indent ~ expr(indent))
    (line1 ~ rest).map {
      case (Expr.Var(v), e1, e2) => Expr.Let(v, e1, e2)
    }
  }

  def lambdaP(indent: String): P[Expr] = {
    val multiLine = P("\n" ~ indent ~ spaces.!).flatMap { i2 =>
      expr(indent + i2)
    }
    val body =
      P((maybeSpace ~ expr(indent)) | multiLine)

    def parseL(suffix: P[Unit]): P[Expr.Lambda] =
      P(variable ~ maybeSpace ~ (("," ~/ maybeSpace ~ P(parseL(suffix))) | (suffix ~ ":" ~ body)))
        .map { case (Expr.Var(v), body) => Expr.Lambda(v, body) }

    val lamP = P((("lambda" ~ spaces) | ("\\" ~ maybeSpace)) ~/ parseL(P("")))
    val defP = P("def" ~ spaces ~/ variable ~ "(" ~/ parseL(P(")" ~ maybeSpace)) ~ ("\n" ~ indent).rep(1) ~ expr(indent)).map {
      case (Expr.Var(v), lambda, in) => Expr.Let(v, lambda, in)
    }
    lamP | defP
  }

  val typeScheme: P[Scheme] = {
    def prim(s: String) = P(s).map(_ => Scheme(Nil, Type.Primitive(s)))

    val item = prim("Int") | prim("Bool")
    P(item ~ (spaces ~/ "->" ~ spaces ~ typeScheme).?).map {
      case (t, None) => t
      case (a, Some(b)) => Scheme(Nil, Type.Arrow(a.result, b.result)) // TODO this is ignoring type variables for now
    }
  }
  val ffiP: P[Expr.Ffi] = {
    val javaId = ('0' to '9').toSet | ('a' to 'z').toSet | ('A' to 'Z').toSet | Set('.', '$')
    P("ffi" ~ spaces ~/ ("\"java\""|"\"scala\"").! ~/ spaces ~ "\"" ~ P(CharsWhile(javaId)).! ~ "\"" ~/ spaces ~ typeScheme).map {
      case ("\"java\"", callsite, scheme) => Expr.Ffi("java", callsite, scheme)
      case ("\"scala\"", callsite, scheme) => Expr.Ffi("scala", callsite, scheme)
      case _ => sys.error("unreachable")
    }
  }

  val intP: P[Int] = P(CharsWhile(isNum _).!).map(_.toInt)
  val boolP: P[Boolean] =
    P("true").map(_ => true) |
      P("false").map(_ => false)

  val litP: P[Expr.Literal] =
    intP.map { i => Expr.Literal(Lit.Integer(i)) } |
      boolP.map { b => Expr.Literal(Lit.Bool(b)) }

  def nonInfixP(indent: String): P[Expr] =
    litP | lambdaP(indent) | ifParser(indent) | letParser(indent) | ffiP | variable | P(expr(indent).parens)

  def expr(indent: String): P[Expr] = {
    val item = P(nonInfixP(indent) ~ (maybeSpace ~ operatorParse ~/ maybeSpace).?).flatMap {
      case (first, None) => P("").map(_ => first)
      case (first, Some(op)) =>
        P(expr(indent)).map(Expr.Op(first, op, _))
    }
    P(item ~ P(expr(indent).parens.?)).map {
      case (e, None) => e
      case (fn, Some(a)) => Expr.App(fn, a)
    }
  }

  val expr: P[Expr] = expr("")
}
