package org.bykn.bosatsu

import org.scalatest.FunSuite
import java.math.BigInteger
import cats.data.NonEmptyList

class NormalizationTest extends FunSuite {
  import TestUtils._
  import NormalExpression._
  import Lit._
  import Normalization._
  import NormalPattern.{PositionalStruct, Var, ListPat, WildCard}

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
  test("recurse") {
    normalExpressionTest(
      List("""
package Recur/Some

def foo(x):
  recur x:
    []: ["a","b","c"]
    [h, *t]: NonEmptyList("zero", foo(t))

out = foo
"""
      ), "Recur/Some", Recursion(Lambda(Lambda(Match(LambdaVar(0),NonEmptyList.fromList(List(
        (ListPat(List()),Struct(1,List(Literal(Str("a")), Struct(1,List(Literal(Str("b")), Struct(1,List(Literal(Str("c")), Struct(0,List())))))))),
        (ListPat(List(Right(Var(1)), Left(Some(0)))),Lambda(Lambda(Struct(1,List(Literal(Str("zero")), App(LambdaVar(3),LambdaVar(0)))))))
      )).get))))
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
  normalExpressionTest(
    List("""
package Match/List

out = match [1,2,3]:
  [first, second, last]: last
  _: 0
"""
      ), "Match/List",
      Literal(Integer(BigInteger.valueOf(3)))
    )
  normalExpressionTest(
    List("""
package Match/List

out = match [1,2,3,4,5]:
  [*first_few, _, _, last]: last
  _: 0
"""
      ), "Match/List",
      Literal(Integer(BigInteger.valueOf(5)))
    )
  normalExpressionTest(
    List("""
package Match/List

out = match ["a","b","c","d","e"]:
  [h, *t]: Some((h, t))
  []: None
"""
      ), "Match/List",
      Struct(1,List(
        Struct(0,List(
          Literal(Str("a")), 
          Struct(0,List(
            Struct(1,List(Literal(Str("b")), Struct(1,List(Literal(Str("c")), Struct(1,List(Literal(Str("d")), Struct(1,List(Literal(Str("e")), Struct(0,List()))))))))),
            Struct(0,List())
          ))
        ))
      ))

    )
  normalExpressionTest(
    List("""
package Match/Union

enum Bar:
  Baz(a), Fizz(a), Buzz

out = match Baz("abc"):
  Baz(x) | Fizz(x): x
  Buzz: "buzzzzzzz"
"""
      ), "Match/Union",
      Literal(Str("abc"))
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
  test("external") {
    normalExpressionTest(
      List(
"""
package Extern/Simple

external def foo(x: String) -> List[String]

out = foo
"""
      ), "Extern/Simple",
      ExternalVar(PackageName(NonEmptyList.fromList(List("Extern", "Simple")).get),Identifier.Name("foo"))
    )
    normalExpressionTest(
      List(
"""
package Extern/Apply

external def foo(x: String) -> List[String]

out = foo("bar")
"""
    ), "Extern/Apply",
    App(ExternalVar(PackageName(NonEmptyList.fromList(List("Extern", "Apply")).get),Identifier.Name("foo")),Literal(Str("bar")))
    )
    normalExpressionTest(
      List(
"""
package Extern/Match

external def foo(x: String) -> List[String]

out = match foo("bar"):
  [a, _, _]: a
  _: "boom"
"""
    ), "Extern/Match",
    Match(
      App(
        ExternalVar(PackageName(NonEmptyList.fromList(List("Extern", "Match")).get),Identifier.Name("foo")),
        Literal(Str("bar"))),
      NonEmptyList.fromList(List((ListPat(List(Right(Var(0)), Right(WildCard), Right(WildCard))),Lambda(LambdaVar(0))), (WildCard,Literal(Str("boom"))))).get
    )
    )
  }
}
