package dev.bosatsu.rankn

import cats.data.NonEmptyList
import munit.FunSuite

class TypeRenderingTest extends FunSuite {
  test("rendering Unit function arguments uses () without extra parens") {
    val a = Type.TyVar(Type.Var.Bound("a"))
    val tpe = Type.Fun(NonEmptyList.one(Type.UnitType), a)
    val rendered = Type.fullyResolvedDocument.document(tpe).render(80)
    assertEquals(rendered, "() -> a")
  }
}
