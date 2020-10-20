package org.bykn.bosatsu

import java.math.BigInteger
import org.scalatest.funsuite.AnyFunSuite

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
      List(v => assert(v.asExternal.toAny == BigInteger.valueOf(10), "should just be a number"))
    )
  }
  test("test of custom externals") {
    letFreeEvaluateTest(
      List("""
package Ext/Add

out = [1,2,3].foldLeft(4, add)   
"""),
      "Ext/Add",
      Externals(Map.empty),
      List(v => assert(v.asExternal.toAny == BigInteger.valueOf(10), "should just be a number"))
    )
  }
}
