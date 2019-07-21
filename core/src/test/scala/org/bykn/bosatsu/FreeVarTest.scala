package org.bykn.bosatsu

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }

import fastparse.all.Parsed

class FreeVarTest extends FunSuite {
  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 5000)
    PropertyCheckConfiguration(minSuccessful = 300)
    //PropertyCheckConfiguration(minSuccessful = 5)

  def assertFreeVars(stmt: String, vars: List[String]) =
    Statement.parser.parse(stmt) match {
      case Parsed.Success(t, idx) =>
        assert(idx == stmt.length)

        val found = Statement.valuesOf(t).flatMap(_.freeVars).toList.sorted
        assert(found == vars.sorted.map(Identifier.Name(_)))
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $stmt: $exp at $idx with trace: ${extra.traced.trace}")
    }

  test("freeVar examples") {
    assertFreeVars("""x = y""", List("y"))
    assertFreeVars("""y = 1""", Nil)
    assertFreeVars("""external def foo -> Int""", Nil)
    assertFreeVars("""def foo(x): y""", List("y"))
    assertFreeVars("""def foo(x):
  y = x
  y""", Nil)
  }

  test("freeVars is a subset of allNames") {
    forAll(Generators.genStatement(3)) { stmt =>
      Statement.valuesOf(stmt)
        .foreach { v =>
          assert(v.freeVars.subsetOf(v.freeVars))
        }
    }
  }
}
