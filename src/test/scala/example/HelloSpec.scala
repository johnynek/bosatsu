package org.bykn.hindleymilner

import org.scalatest._

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
}
