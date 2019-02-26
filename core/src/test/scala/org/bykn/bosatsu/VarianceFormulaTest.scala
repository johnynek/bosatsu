package org.bykn.bosatsu

import org.bykn.bosatsu.rankn.TypeEnv
import org.scalatest.FunSuite

class VarianceFormulaTest extends FunSuite {

  def testVariance(teStr: String, variances: Map[String, List[Variance]]) = {
    val te = TestUtils.typeEnvOf(Predef.packageName, teStr)
    VarianceFormula.solve(TypeEnv.empty, te) match {
      case Left(errs) => fail(s"couldn't solve: $errs")
      case Right(teVar) =>
        variances.foreach { case (n, vs) =>
          val dt = teVar.definedTypes((Predef.packageName, TypeName(n)))
          assert(dt.annotatedTypeParams.map(_._2) == vs)
        }
    }
  }

  test("test some basic structs") {

    testVariance("""#
struct Foo(a)
""", Map("Foo" -> List(Variance.co)))

    // testVariance("""#
// struct Foo(a, b)
// """, Map("Foo" -> List(Variance.co, Variance.co)))

    testVariance("""#
struct Foo(a: x, b: x)
""", Map("Foo" -> List(Variance.co)))

    testVariance("""#
struct Foo(a: x -> y)
""", Map("Foo" -> List(Variance.contra, Variance.co)))

  }
}
