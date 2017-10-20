package org.bykn.hindleymilner

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

  val variable: P[Expr.Var] =
    P(CharIn('a' to 'z').! ~ CharsWhile { c => isLower(c) || isUpper(c) }.!.?)
      .map {
        case (a, None) => Expr.Var(a)
        case (a, Some(b)) => Expr.Var(a + b)
      }

  val maybeSpace: P[Unit] = P(CharsWhile(_ == ' ').?)
  val spaces: P[Unit] = P(CharsWhile(_ == ' '))

  def tokenP[T](s: String, t: T): P[T] = P(s).map(_ => t)

  val operatorParse: P[Operator] =
    tokenP("+", Operator.Plus) |
      tokenP("-", Operator.Sub) |
      tokenP("*", Operator.Mul) |
      tokenP("==", Operator.Eql)

  def parens[T](p: P[T]): P[T] =
    P("(" ~/ maybeSpace ~ p ~ maybeSpace ~ ")")

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
    def con(s: String) = P(s).map(_ => Scheme(Nil, Type.Con(s)))

    val item = con("Int") | con("Bool")
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
    litP | lambdaP(indent) | ifParser(indent) | letParser(indent) | ffiP | variable | P(parens(expr(indent)))

  def expr(indent: String): P[Expr] = {
    val item = P(nonInfixP(indent) ~ (maybeSpace ~ operatorParse ~/ maybeSpace).?).flatMap {
      case (first, None) => P("").map(_ => first)
      case (first, Some(op)) =>
        P(expr(indent)).map(Expr.Op(first, op, _))
    }
    P(item ~ P(parens(expr(indent)).?)).map {
      case (e, None) => e
      case (fn, Some(a)) => Expr.App(fn, a)
    }
  }

  val expr: P[Expr] = expr("")
}
