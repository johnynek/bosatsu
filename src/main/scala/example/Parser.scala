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

  def apply(expr: P[Expr]): P[Expr.App] = {
    val item = P(variable | parens(expr))
    P(item ~ parens(item)).map { case (fn, arg) => Expr.App(fn, arg) }
  }

  val intP: P[Int] = P(CharsWhile(isNum _).!).map(_.toInt)
  val boolP: P[Boolean] =
    P("true").map(_ => true) |
      P("false").map(_ => false)

  val litP: P[Expr.Literal] =
    intP.map { i => Expr.Literal(Lit.Integer(i)) } |
      boolP.map { b => Expr.Literal(Lit.Bool(b)) }

  lazy val opP: P[Expr.Op] =
    P(expr ~ spaces ~ operatorParse ~/ spaces ~ expr)
      .map { case (a, o, b) => Expr.Op(a, o, b) }

  lazy val nonInfixP: P[Expr] =
    litP | P(parens(expr)) | P(apply(expr)) | variable

  lazy val expr: P[Expr] =
    P(nonInfixP ~ (maybeSpace ~ operatorParse ~/ maybeSpace).?).flatMap {
      case (first, None) => P("").map(_ => first)
      case (first, Some(op)) =>
        P(expr).map(Expr.Op(first, op, _))
    }
}
