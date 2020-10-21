package org.bykn.bosatsu

import java.math.BigInteger
import org.scalatest.funsuite.AnyFunSuite
import cats.data.NonEmptyList
import scala.math.sqrt

class ExprFnTest extends AnyFunSuite {
  import TestUtils._

  test("test of built in externals") {
    letFreeEvaluateTest(
      List("""
package Ext/Add

out = [1,2,3].foldLeft(4, add)
"""),
      "Ext/Add",
      Externals(Map.empty),
      List(v =>
        assert(
          v.asExternal.toAny == BigInteger.valueOf(10),
          "should just be a number"
        )
      )
    )
  }
  test("test of custom externals") {
    letFreeEvaluateTest(
      List("""
package Ext/Sqrt

external def sqrt(x: Int) -> Int

out = [1,2,3,4].map_List(sqrt).foldLeft(0, add)
"""),
      "Ext/Sqrt",
      Externals(
        Map(
          (PackageName(NonEmptyList.of("Ext", "Sqrt")), "sqrt") -> FfiCall.Fn1(
            v =>
              Value.ExternalValue(
                BigInteger.valueOf(
                  sqrt(
                    v.asExternal.toAny
                      .asInstanceOf[BigInteger]
                      .longValue()
                      .toDouble
                  ).toLong
                )
              )
          )
        )
      ),
      List(v =>
        assert(
          v.asExternal.toAny == BigInteger.valueOf(5),
          "should just be a number"
        )
      )
    )
  }
  
  test("test of expression function externals") {
    letFreeEvaluateTest(
      List("""
package Ext/ExprListFilter

external def expr_list_filter(lst: List[a], fn: a -> Bool) -> Int

out = [1,2,3,4].expr_list_filter(\_ -> True)
"""),
      "Ext/ExprListFilter",
      Externals(
        Map(
          (PackageName(NonEmptyList.of("Ext", "ExprListFilter")), "expr_list_filter") -> LetFreeEvaluation.exprFn(2,
            {
              case (t, args) => BigInteger.ONE
            }
          )
        )
      ),
      List(v =>
        assert(
          v.asExternal.toAny == BigInteger.valueOf(1),
          "should just be a number"
        )
      )
    )
  }
}
