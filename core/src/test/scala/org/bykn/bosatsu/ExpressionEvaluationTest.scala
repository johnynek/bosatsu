package org.bykn.bosatsu

import org.scalatest.funsuite.AnyFunSuite
import TestUtils.testInferred
import org.scalatest.Assertion
import org.scalatest.Assertions.{succeed, fail}
import cats.Eval
import java.math.BigInteger
import Value.{SumValue, ConsValue, ExternalValue, UnitValue, FnValue}

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
      List({ x => assert(x._1.value == Value.ExternalValue("aa"), x._1.value) })
    )

    evalTest(
      List("""
package LetFreeTest/String
main = 22
"""),
      "LetFreeTest/String",
      Externals(Map.empty),
      List({ x =>
        assert(
          x._1.value == Value.ExternalValue(BigInteger.valueOf(22)),
          x._1.value
        )
      })
    )

    evalTest(
      List("""
package LetFreeTest/String
main = [23]
"""),
      "LetFreeTest/String",
      Externals(Map.empty),
      List({ x =>
        assert(
          x._1.value == SumValue(
            1,
            ConsValue(
              ExternalValue(BigInteger.valueOf(23)),
              ConsValue(SumValue(0, UnitValue), UnitValue)
            )
          ),
          x._1.value
        )
      })
    )
  }

  test("recurse") {
    evalTest(
      List("""
package Recur/Some
def foo(x):
  recur x:
    []: ["a","b","c"]
    ["a: ${bar}", *t]: NonEmptyList(bar, foo(t))
    [_, *t]: NonEmptyList("zero", foo(t))
out = foo
"""),
      "Recur/Some",
      Externals(Map.empty),
      List({ x =>
        x._1.value match {
          case FnValue(_) => succeed
          case _          => fail()
        }
      })
    )
  }

  test("foldLeft w/o loop applied") {
    evalTest(
      List("""
package Match/Basic

out = match [1,2,3]:
  EmptyList: 0
  NonEmptyList(head, _): head
"""),
      "Match/Basic",
      Externals(Map.empty),
      List({ x =>
        assert(
          x._1.value == Value.ExternalValue(BigInteger.valueOf(1)),
          x._1.value
        )
      })
    )
  }
}
