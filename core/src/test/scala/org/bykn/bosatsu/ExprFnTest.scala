package org.bykn.bosatsu

import java.math.BigInteger
import org.scalatest.funsuite.AnyFunSuite
import cats.data.NonEmptyList

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
                v.asExternal.toAny.asInstanceOf[BigInteger].sqrt()
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
}
