package org.bykn.hindleymilner

import org.scalatest.FunSuite

import fastparse.all._

class ParserTest extends FunSuite {

  def i(in: Int): Expr = Expr.Literal(Lit.Integer(in))
  def b(bo: Boolean): Expr = Expr.Literal(Lit.Bool(bo))
  def v(str: String): Expr = Expr.Var(str)

  def parseTest[T](p: Parser[T], str: String, expected: T, exidx: Int) =
    p.parse(str) match {
      case Parsed.Success(t, idx) =>
        assert(t == expected)
        assert(idx == exidx)
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $str: $exp at $idx with trace: ${extra.traced.trace}")
    }

  def expectFail[T](p: Parser[T], str: String, atIdx: Int) =
    p.parse(str) match {
      case Parsed.Success(t, idx) => fail(s"parsed $t to: $idx")
      case Parsed.Failure(exp, idx, extra) =>
        assert(idx == atIdx)
    }

  test("we can parse Expr.Var") {
    parseTest(Parser.variable, "a", v("a"), 1)
    parseTest(Parser.variable, "ab", v("ab"), 2)
    expectFail(Parser.variable, " ab", 0)
  }

  test("we can parse Expr.App") {
    parseTest(Parser.apply(Parser.variable), "a(b)", Expr.App(v("a"), v("b")), 4)
    parseTest(Parser.apply(Parser.variable), "ab( ab )", Expr.App(v("ab"), v("ab")), 8)
    expectFail(Parser.apply(Parser.variable), " ab()", 0)

    parseTest(Parser.expr, "a(b)", Expr.App(v("a"), v("b")), 4)
    parseTest(Parser.expr, "ab( ab )", Expr.App(v("ab"), v("ab")), 8)
    expectFail(Parser.expr, " ab()", 0)
  }

  test("we can parse Expr.Literal") {
    parseTest(Parser.expr, "true", b(true), 4)
    parseTest(Parser.expr, "false", b(false), 5)
    parseTest(Parser.expr, "42", i(42), 2)
  }

  test("we can parse operators") {
    parseTest(Parser.expr, "1 + 2", Expr.Op(i(1), Operator.Plus, i(2)), 5)
    parseTest(Parser.expr, "1 * 2", Expr.Op(i(1), Operator.Mul, i(2)), 5)
    parseTest(Parser.expr, "(1 + 1) == 2", Expr.Op(Expr.Op(i(1), Operator.Plus, i(1)), Operator.Eql, i(2)), 12)
  }

}
