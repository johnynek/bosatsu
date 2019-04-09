package org.bykn.bosatsu

import org.scalatest.FunSuite
import java.math.BigInteger
import cats.data.NonEmptyList

class NormalizationTest extends FunSuite {
  import TestUtils._
  import NormalExpression._
  import Lit._
  import Normalization._
  import NormalPattern.{PositionalStruct, Var}

/*
struct Pair(first, second)

def bar(x):
  baz = \y -> Pair(x, y)
  baz(10)
*/

  test("Literal") {
      normalTagTest(
        List("""
package NormTest/String

main = "aa"
"""
        ), "NormTest/String", NormalExpressionTag(Literal(Str("aa")), Set())
      )

      normalTagTest(
        List("""
package NormTest/Int

main = 22
"""
        ), "NormTest/Int", NormalExpressionTag(Literal(Integer(BigInteger.valueOf(22))), Set())
      )

      normalTagTest(
        List("""
package NormTest/List

main = ["aa"]
"""
        ), "NormTest/List", NormalExpressionTag(
          Struct(1,List(Literal(Str("aa")), Struct(0,List()))),
          Set(
            Lambda(Lambda(Struct(1,List(LambdaVar(1), LambdaVar(0))))),
            Literal(Str("aa")),
            Lambda(Struct(1,List(Literal(Str("aa")), LambdaVar(0)))),
            Struct(0,List())
          )
        )
      )
  }
  test("Lambda") {
    normalTagTest(
      List("""
package Lambda/Identity

out = \x -> x
"""
      ), "Lambda/Identity", NormalExpressionTag(
        Lambda(LambdaVar(0)), Set(LambdaVar(0))
      )
    )
    normalTagTest(
      List("""
package Lambda/Always

out = \x -> \y -> x
"""
      ), "Lambda/Always", NormalExpressionTag(
        Lambda(Lambda(LambdaVar(1))), Set(Lambda(LambdaVar(1)), LambdaVar(1))
      )
    )
    normalTagTest(
      List("""
package Lambda/Always

out = \x -> \y -> y
"""
      ), "Lambda/Always", NormalExpressionTag(
        Lambda(Lambda(LambdaVar(0))), Set(Lambda(LambdaVar(0)), LambdaVar(0))
      )
    )

    normalTagTest(
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
    normalTagTest(
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
    normalTagTest(
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
    normalTagTest(
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
  test("Match") {
    normalExpressionTest(
      List("""
package Match/Vars

def result(x, c):
  match x:
    (a, b): (b, c, a)

out=result
"""
      ), "Match/Vars", 
    Lambda(Lambda(Match(LambdaVar(1),NonEmptyList.fromList(List(
      (
        PositionalStruct(None,List(Var(0), PositionalStruct(None,List(Var(1), PositionalStruct(None,List()))))),
        Lambda(Lambda(Struct(0,List(LambdaVar(1), Struct(0,List(LambdaVar(2), Struct(0,List(LambdaVar(0), Struct(0,List())))))))))
      )
    )).get))))
    normalExpressionTest(
      List("""
package Match/Structs

struct Pair(f, s)
struct Trip(f, s, t)

out=match Pair(1, "two"):
  Pair(f, s): Trip(3, s, f)

"""
      ), "Match/Structs",
      Struct(0,List(Literal(Integer(BigInteger.valueOf(3))), Literal(Str("two")), Literal(Integer(BigInteger.valueOf(1)))))
    )
    normalExpressionTest(
      List("""
package Match/None

out=match None:
  Some(_): "some"
  _: "not some"

"""
      ), "Match/None",
      Literal(Str("not some"))
    )
  }
  test("imports") {
    normalExpressionTest(
      List("""
package Imp/First
export [fizz]

def fizz(f, s):
  (s, f)
""",
"""
package Imp/Second
import Imp/First [fizz]

out=fizz(1,2)
"""
      ), "Imp/Second",
      Struct(0,List(Literal(Integer(BigInteger.valueOf(2))), Struct(0,List(Literal(Integer(BigInteger.valueOf(1))), Struct(0,List())))))
    )
    normalExpressionTest(
      List("""
package Imp/First
import Imp/Second [fizz]

out=fizz(1,2)
""",
"""
package Imp/Second
export [fizz]

def fizz(f, s):
  (s, f)
"""
      ), "Imp/First",
      Struct(0,List(Literal(Integer(BigInteger.valueOf(2))), Struct(0,List(Literal(Integer(BigInteger.valueOf(1))), Struct(0,List())))))
    )
  }
}
