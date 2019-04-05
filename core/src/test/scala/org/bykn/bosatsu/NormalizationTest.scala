package org.bykn.bosatsu

import org.scalatest.FunSuite
import java.math.BigInteger

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

  test("Literal") {
      normalizeTest(
        List("""
package NormTest/String

main = "aa"
"""
        ), "NormTest/String", NormalExpressionTag(Literal(Str("aa")), Set())
      )

      normalizeTest(
        List("""
package NormTest/Int

main = 22
"""
        ), "NormTest/Int", NormalExpressionTag(Literal(Integer(BigInteger.valueOf(22))), Set())
      )

      normalizeTest(
        List("""
package NormTest/List

main = ["aa"]
"""
        ), "NormTest/List", NormalExpressionTag(
          App(App(
            Lambda(Lambda(Struct(1,List(LambdaVar(1), LambdaVar(0)))))
            ,Literal(Str("aa"))),
          Struct(0,List())),
        Set(
          Lambda(Lambda(Struct(1,List(LambdaVar(1), LambdaVar(0))))),
          Literal(Str("aa")),
          App(Lambda(Lambda(Struct(1,List(LambdaVar(1), LambdaVar(0))))),Literal(Str("aa"))),
          Struct(0,List())
        ))
      )
  }
  test("Lambda") {
    normalizeTest(
      List("""
package Lambda/Identity

out = \x -> x
"""
      ), "Lambda/Identity", NormalExpressionTag(
        Lambda(LambdaVar(0)), Set(LambdaVar(0))
      )
    )
    normalizeTest(
      List("""
package Lambda/Always

out = \x -> \y -> x
"""
      ), "Lambda/Always", NormalExpressionTag(
        Lambda(Lambda(LambdaVar(1))), Set(Lambda(LambdaVar(1)), LambdaVar(1))
      )
    )
    normalizeTest(
      List("""
package Lambda/Always

out = \x -> \y -> y
"""
      ), "Lambda/Always", NormalExpressionTag(
        Lambda(Lambda(LambdaVar(0))), Set(Lambda(LambdaVar(0)), LambdaVar(0))
      )
    )

    normalizeTest(
      List("""
package Lambda/Identity

def foo(x):
  x
out = foo
"""
      ), "Lambda/Identity", NormalExpressionTag(
        Lambda(LambdaVar(0)), Set(LambdaVar(0))
      )
    )
    normalizeTest(
      List("""
package Lambda/Identity

def foo(x):
  y = x
  y
out = foo
"""
      ), "Lambda/Identity", NormalExpressionTag(
        Lambda(LambdaVar(0)), Set(LambdaVar(0))
      )
    )
    normalizeTest(
      List("""
package Lambda/Always

def foo(x,y):
  x
out = foo
"""
      ), "Lambda/Always", NormalExpressionTag(
        Lambda(Lambda(LambdaVar(1))), Set(Lambda(LambdaVar(1)), LambdaVar(1))
      )
    )
    normalizeTest(
      List("""
package Lambda/Always

def foo(x,y):
  y
out = foo
"""
      ), "Lambda/Always", NormalExpressionTag(
        Lambda(Lambda(LambdaVar(0))), Set(Lambda(LambdaVar(0)), LambdaVar(0))
      )
    )
  }
}
