package org.bykn.bosatsu

import org.bykn.bosatsu.rankn.{DefinedType, TypeEnv}
import org.scalatest.FunSuite

class VarianceFormulaTest extends FunSuite {

  def testVariance(teStr: String, variances: Map[String, List[Variance]]) = {
    val te = TestUtils.typeEnvOf(Predef.packageName, teStr)
    VarianceFormula.solve(TypeEnv.empty, te.allDefinedTypes) match {
      case Left(errs) => fail(s"couldn't solve: $errs")
      case Right(teVar) =>
        val teMap = DefinedType.listToMap(teVar)
        variances.foreach { case (n, vs) =>
          val dt = teMap((Predef.packageName, TypeName(n)))
          assert(dt.annotatedTypeParams.map(_._2) == vs)
        }
    }
  }

  test("test some basic structs") {

    testVariance("""#
struct Foo(a)
""", Map("Foo" -> List(Variance.co)))

    testVariance("""#
struct Foo(a, b)
""", Map("Foo" -> List(Variance.co, Variance.co)))

    testVariance("""#
struct Foo(a: x, b: x)
""", Map("Foo" -> List(Variance.co)))

    testVariance("""#
struct Foo(a: x -> y)
""", Map("Foo" -> List(Variance.contra, Variance.co)))

    testVariance("""#
struct Foo(a: x -> x)
""", Map("Foo" -> List(Variance.in)))

    testVariance("""#
struct Foo(a: x -> y, b: z)
""", Map("Foo" -> List(Variance.contra, Variance.co, Variance.co)))

    testVariance("""#
struct Foo(a: x -> y, b: x)
""", Map("Foo" -> List(Variance.in, Variance.co)))

    testVariance("""#
enum Opt: None, Some(a)
""", Map("Opt" -> List(Variance.co)))

    testVariance("""#
enum Lst: E, NE(head: a, tail: Lst[a])
""", Map("Lst" -> List(Variance.co)))
  }
}
