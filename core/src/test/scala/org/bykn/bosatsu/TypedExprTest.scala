package org.bykn.bosatsu

import org.scalatest.FunSuite

import TestUtils.checkLast

class TypedExprTest extends FunSuite {
  test("freeVars on simple cases works") {
    checkLast("""#
x = 1
def id(x): x
y = id(x)
""") { te => assert(TypedExpr.freeVars(te :: Nil) == List("id", "x")) }

    checkLast("""#
x = 1
""") { te => assert(TypedExpr.freeVars(te :: Nil) == Nil) }

    checkLast("""#
struct Tup2(a, b)

x = Tup2(1, 2)
""") { te => assert(TypedExpr.freeVars(te :: Nil) == List("Tup2")) }
  }
}
