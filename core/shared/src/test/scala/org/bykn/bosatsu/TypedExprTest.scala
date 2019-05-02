package org.bykn.bosatsu

import org.scalatest.FunSuite

import TestUtils.checkLast

import Identifier.Name

class TypedExprTest extends FunSuite {
  test("freeVars on simple cases works") {
    checkLast("""#
x = 1
def id(x): x
y = id(x)
""") { te => assert(TypedExpr.freeVars(te :: Nil) == List(Name("id"), Name("x"))) }

    checkLast("""#
x = 1
""") { te => assert(TypedExpr.freeVars(te :: Nil) == Nil) }

    checkLast("""#
struct Tup2(a, b)

x = Tup2(1, 2)
""") { te => assert(TypedExpr.freeVars(te :: Nil) == List(Identifier.Constructor("Tup2"))) }

    checkLast("""#
struct Tup2(a, b)

x = Tup2(1, 2)
y = match x:
  Tup2(a, _): a
""") { te => assert(TypedExpr.freeVars(te :: Nil) == List(Name("x"))) }
  }
}
