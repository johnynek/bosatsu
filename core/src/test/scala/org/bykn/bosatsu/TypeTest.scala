package org.bykn.bosatsu

import org.scalatest.FunSuite

import Type._

// TODO write property checks for more of these
class TypeTest extends FunSuite {
  test("simple varsIn examples") {
    val varA = Var("a")
    val varB = Var("b")
    assert(varA.varsIn == List(varA))
    assert(Substitutable[Type].typeVars(varA) == Set("a"))
    assert(Arrow(varA, varB).varsIn == List(varA, varB))
    assert(Substitutable[Type].typeVars(Arrow(varA, varB)) == Set("a", "b"))
    // a is bound and not a free variable here
    assert(TypeLambda("a", varA).varsIn == Nil)
    assert(Substitutable[Type].typeVars(TypeLambda("a", varA)).isEmpty)
    // b is a free variable
    assert(TypeLambda("a", varB).varsIn == List(varB))
    assert(Substitutable[Type].typeVars(TypeLambda("a", varB)) == Set("b"))
  }

  test("simple normalization examples") {
    assert(normalize(Var("a")) == Var("a"))
    assert(normalize(TypeLambda("z", Var("z"))) == TypeLambda("a", Var("a")))
    assert(normalize(TypeLambda("z", Var("b"))) == TypeLambda("a", Var("b")))
    assert(normalize(TypeLambda("z", TypeLambda("q", Var("b")))) == TypeLambda("a", TypeLambda("c", Var("b"))))
  }

  test("apply lambda simplifies") {

    assert(simplifyApply(TypeApply(TypeLambda("a", Var("a")), Var("b"))) == Var("b"))
  }
}
