package org.bykn.hindleymilner

import org.scalatest._

import fastparse.all.Parsed

class InferTest extends FunSuite {
  def simpleMatch(e: Expr, t: Type) = {
    assert(Inference.inferExpr(e) == Right(Scheme(Nil, t)))
  }
  val i1 = Expr.Literal(Lit.Integer(1))
  val b1 = Expr.Literal(Lit.Bool(true))

  test("int") {
    simpleMatch(i1, Type.intT)
    simpleMatch(Expr.Op(i1, Operator.Plus, i1), Type.intT)
  }

  test("bool") {
    simpleMatch(b1, Type.boolT)
    simpleMatch(Expr.Op(i1, Operator.Eql, i1), Type.boolT)
  }

  def parseType(str: String, t: Type) =
    Parser.expr.parse(str) match {
      case Parsed.Success(exp, _) =>
        assert(Inference.inferExpr(exp) == Right(Scheme(Nil, t)))
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $str: $exp at $idx with trace: ${extra.traced.trace}")
    }

  test("type check some expressions") {
    parseType("1 + 1", Type.intT)
    parseType("1 == 1", Type.boolT)
    parseType("(1+2) == 1", Type.boolT)
  }
}
