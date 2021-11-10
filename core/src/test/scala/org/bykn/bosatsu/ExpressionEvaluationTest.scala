package org.bykn.bosatsu

import org.scalatest.funsuite.AnyFunSuite
import TestUtils.testInferred
import org.scalatest.Assertion
import org.scalatest.Assertions.{succeed, fail}
import cats.Eval

object ExpressionEvaluationTest {
  def evalTest(
      packages: List[String],
      mainPackS: String,
      ext: Externals,
      assertions: List[((Eval[Value], rankn.Type)) => Assertion]
  ) = {
    def inferredHandler(
        infPackMap: PackageMap.Inferred,
        mainPack: PackageName
    ) = ExpressionEvaluation(infPackMap, Predef.jvmExternals ++ ext)
      .evaluateLast(mainPack) match {
      case Some(res) => {

        assertions.foreach(_.apply(res))
        succeed
      }
      case None => fail("There should be a last expression")
    }

    testInferred(packages, mainPackS, inferredHandler(_, _))
  }
}

class ExpressionEvaluationTest extends AnyFunSuite {
  import ExpressionEvaluationTest.evalTest
  test("Literal") {
    evalTest(
      List("""
package LetFreeTest/String
main = "aa"
"""),
      "LetFreeTest/String",
      Externals(Map.empty),
      List()
    )
  }
}
