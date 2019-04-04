package org.bykn.bosatsu

import org.scalatest.FunSuite

class NormalizationTest extends FunSuite {
  import TestUtils._
  import NormalExpression._
  import Lit._
  import Normalization._

/*
struct Pair(first, second)

def bar(x):
  baz = \y -> Pair(x, y)
  baz(10)
*/

  test("simple package normalizes") {
      normalizeTest(
        List("""
package NormTest/String

main = "aa"
"""
        ), "NormTest/String", NormalExpressionTag(Literal(Str("aa")), Set())
      )
  }
}
