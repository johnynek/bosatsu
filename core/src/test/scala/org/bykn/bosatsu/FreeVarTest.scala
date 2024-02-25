package org.bykn.bosatsu

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}

import org.scalatest.funsuite.AnyFunSuite

class FreeVarTest extends AnyFunSuite {
  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)
  // PropertyCheckConfiguration(minSuccessful = 300)
  // PropertyCheckConfiguration(minSuccessful = 5)

  def assertFreeVars(stmt: String, vars: List[String]) =
    Statement.parser.parseAll(stmt) match {
      case Right(t) =>
        val found = Statement.valuesOf(t).flatMap(_.freeVars).toList.sorted
        assert(found == vars.sorted.map(Identifier.Name(_)))
      case Left(errs) =>
        val idx = errs.failedAtOffset
        fail(s"failed to parse: $stmt: at $idx with errs: ${errs}")
    }

  test("freeVar examples") {
    assertFreeVars("""x = y""", List("y"))
    assertFreeVars("""y = 1""", Nil)
    assertFreeVars("""external foo: Int""", Nil)
    assertFreeVars("""def foo(x): y""", List("y"))
    assertFreeVars(
      """def foo(x):
  y = x
  y""",
      Nil
    )
  }

  test("freeVars is a subset of allNames") {
    forAll(Generators.genStatement(3)) { stmt =>
      Statement
        .valuesOf(stmt :: Nil)
        .foreach { v =>
          assert(v.freeVars.subsetOf(v.allNames))
        }
    }
  }
}
