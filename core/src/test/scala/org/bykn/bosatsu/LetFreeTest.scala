package org.bykn.bosatsu

import java.math.BigInteger
import cats.data.NonEmptyList
import org.scalatest.funsuite.AnyFunSuite
import rankn.Type
import rankn.DataFamily.{Enum, Struct => DFStruct}
import TestUtils._
import LetFreeExpression._
import Lit._

class LetFreeTest extends AnyFunSuite {
  test("Literal") {
    normalExpressionTest(
      List("""
package LetFreeTest/String

main = "aa"
"""),
      "LetFreeTest/String",
      Left(Literal(Str("aa")))
    )

    normalExpressionTest(
      List("""
package LetFreeTest/Int

main = 22
"""),
      "LetFreeTest/Int",
      Left(Literal(Integer(BigInteger.valueOf(22))))
    )

    normalExpressionTest(
      List("""
package LetFreeTest/List

main = ["aa"]
"""),
      "LetFreeTest/List",
      Left(Struct(1, List(Literal(Str("aa")), Struct(0, List(), Enum)), Enum))
    )
  }
  test("recurse") {
    normalExpressionTest(
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
      Right(789742180)
    )
  }
  test("foldLeft w/o loop") {
    normalExpressionTest(
      List("""
package Recur/FoldLeft

def foldLeft(lst: List[a], item: b, fn: b -> a -> b) -> b:
  recur lst:
    EmptyList: item
    NonEmptyList(head, tail): foldLeft(tail, fn(item, head), fn)

out = foldLeft
"""),
      "Recur/FoldLeft",
      Right(873201835)
    )
  }
  test("foldLeft w/o loop applied") {
    normalExpressionTest(
      List("""
package Recur/FoldLeft

def foldLeft(lst: List[a], item: b, fn: b -> a -> b) -> b:
  recur lst:
    EmptyList: item
    NonEmptyList(head, tail): foldLeft(tail, fn(item, head), fn)

out = [1,2,3].foldLeft(4, add)
"""),
      "Recur/FoldLeft",
      Right(-213085200)
    )
  }
  test("foldLeft") {
    normalExpressionTest(
      List("""
package Recur/FoldLeft

def foldLeft(lst: List[a], item: b, fn: b -> a -> b) -> b:
  # make the loop function as small as possible
  def loop(lst, item):
    recur lst:
      EmptyList: item
      NonEmptyList(head, tail): loop(tail, fn(item, head))
  loop(lst, item)

out = foldLeft
"""),
      "Recur/FoldLeft",
      Right(1762482636)
    )
  }
  test("foldLeft applied") {
    normalExpressionTest(
      List("""
package Recur/FoldLeft

lst = [1,2,3]

def foldLeft(lst: List[a], item: b, fn: b -> a -> b) -> b:
  # make the loop function as small as possible
  def loop(lst, item):
    recur lst:
      EmptyList: item
      NonEmptyList(head, tail): loop(tail, fn(item, head))
  loop(lst, item)

out = lst.foldLeft(9, add)
"""),
      "Recur/FoldLeft",
      Right(771154806)
    )
  }
  test("Lambda") {
    normalExpressionTest(
      List("""
package Lambda/Identity

out = \x -> x
"""),
      "Lambda/Identity",
      Left(Lambda(LambdaVar(0)))
    )
    normalExpressionTest(
      List("""
package Lambda/Always

out = \x -> \_ -> x
"""),
      "Lambda/Always",
      Left(Lambda(Lambda(LambdaVar(1))))
    )
    normalExpressionTest(
      List("""
package Lambda/Always

out = \_ -> \y -> y
"""),
      "Lambda/Always",
      Left(Lambda(Lambda(LambdaVar(0))))
    )

    normalExpressionTest(
      List("""
package Lambda/Identity

def foo(x):
  x
out = foo
"""),
      "Lambda/Identity",
      Left(Lambda(LambdaVar(0)))
    )
    normalExpressionTest(
      List("""
package Lambda/Identity

def foo(x):
  y = x
  y
out = foo
"""),
      "Lambda/Identity",
      Left(Lambda(LambdaVar(0)))
    )
    normalExpressionTest(
      List("""
package Lambda/Always

def foo(x, _):
  x
out = foo
"""),
      "Lambda/Always",
      Left(Lambda(Lambda(LambdaVar(1))))
    )
    normalExpressionTest(
      List("""
package Lambda/Always

def foo(_, y):
  y
out = foo
"""),
      "Lambda/Always",
      Left(Lambda(Lambda(LambdaVar(0))))
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
"""),
      "Match/Vars",
      Right(1735444640)
    )

    normalExpressionTest(
      List("""
package Match/Structs

struct Pair(f, s)
struct Trip(f, s, t)

out=match Pair(1, "two"):
  Pair(f, s): Trip(3, s, f)

"""),
      "Match/Structs",
      Left(
        Struct(
          0,
          List(Literal(Lit(3)), Literal(Lit.Str("two")), Literal(Lit(1))),
          DFStruct
        )
      )
    )
    normalExpressionTest(
      List("""
package Match/None

out=match None:
  Some(_): "some"
  _: "not some"

"""),
      "Match/None",
      Left(Literal(Str("not some")))
    )
    normalExpressionTest(
      List("""
package Match/List

out = match [1,2,3]:
  [_, _, last]: last
  _: 0
"""),
      "Match/List",
      Left(Literal(Integer(BigInteger.valueOf(3))))
    )
    normalExpressionTest(
      List("""
package Match/List

out = match [1,2,3,4,5]:
  [*_, _, _, last]: last
  _: 0
"""),
      "Match/List",
      Left(Literal(Integer(BigInteger.valueOf(5))))
    )
    normalExpressionTest(
      List("""
package Match/List

out = match ["a","b","c","d","e"]:
  [h, *t]: Some((h, t))
  []: None
"""),
      "Match/List",
      Right(-616285537)
    )
    normalExpressionTest(
      List("""
package Match/Union

enum Bar:
  Baz(a), Fizz(a), Buzz

out = match Baz("abc"):
  Baz(x) | Fizz(x): x
  Buzz: "buzzzzzzz"
"""),
      "Match/Union",
      Left(Literal(Str("abc")))
    )
    normalExpressionTest(
      List("""
package Match/Union

enum Bar:
  Baz(a), Fizz(a), Buzz

out = match Buzz:
  Baz(x) | Fizz(x): x
  Buzz: "buzzzzzzz"
"""),
      "Match/Union",
      Left(Literal(Str("buzzzzzzz")))
    )
    normalExpressionTest(
      List("""
package Match/ListPat

list = [1,2,3,4,5]

out = match list:
  [*start, 4, *_]: start
  _ : []
"""),
      "Match/ListPat",
      Right(-859671442)
    )
    normalExpressionTest(
      List("""
package Match/ListPat

external def fn(x: String) -> List[Int]
list = [1, *fn("bar")]

out = match list:
  [*start, 4, *_]: start
  _ : []
"""),
      "Match/ListPat",
      Right(-172975627)
    )

  }

  test("imports") {
    normalExpressionTest(
      List("""
package Imp/First
export fizz

def fizz(f, s):
  (s, f)
""", """
package Imp/Second
from Imp/First import fizz

out=fizz(1,2)
"""),
      "Imp/Second",
      Right(501925842)
    )
    normalExpressionTest(
      List("""
package Imp/First
from Imp/Second import fizz

out=fizz(1,2)
""", """
package Imp/Second
export fizz

def fizz(f, s):
  (s, f)
"""),
      "Imp/First",
      Right(501925842)
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
      ),
      "Extern/Simple",
      Right(1331104799)
    )
    normalExpressionTest(
      List(
        """
package Extern/Apply

external def foo(x: String) -> List[String]

out = foo("bar")
"""
      ),
      "Extern/Apply",
      Right(-2134255842)
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
      ),
      "Extern/Match",
      Right(1388824635)
    )

    normalExpressionTest(
      List(
        """
package Extern/Eta

external def foo(x: String) -> List[String]

out = \y -> foo(y)
"""
      ),
      "Extern/Eta",
      Left(
        ExternalVar(
          PackageName(NonEmptyList.of("Extern", "Eta")),
          Identifier.Name("foo"),
          Type.Fun(Type.StrType, Type.TyApply(Type.ListType, Type.StrType))
        )
      )
    )
    normalExpressionTest(
      List("""
package Extern/List

external def foo(x: String) -> List[(String, Int)]

out = match foo("arg"):
  [*_, _, _, (last, 1)]: last
  _: "'zero\\'"
"""),
      "Extern/List",
      Right(518564136)
    )

    normalExpressionTest(
      List("""
package Extern/List

external def foo(x: String) -> List[String]

out = match foo("arg"):
  [*_, _, _, last]: last
  _: "zero"
"""),
      "Extern/List",
      Right(1997121119)
    )

    normalExpressionTest(
      List("""
package Extern/NamedMatch

external def foo(x: String) -> List[String]

struct Stuff(a,b)

out = match Stuff(foo("c"), "d"):
  Stuff(lst@[_], _): lst
  Stuff(_, y): [y]
"""),
      "Extern/NamedMatch",
      Right(1989857311)
    )

    normalExpressionTest(
      List("""
package Extern/LitMatch

external def foo(x: String) -> String

out = match foo("c"):
  x@("d" | "dd"): x
  _: "f"
"""),
      "Extern/LitMatch",
      Right(-2045553259)
    )
  }
  test("Lambda Substitution") {
    normalExpressionTest(
      List("""
package Substitution/Lambda

def rec_fn(lst1):
  recur lst1:
    []: True
    [_, *t1]: rec_fn(t1)

lst1 = ["zooom"]
main = (rec_fn(lst1), rec_fn(lst1))
"""),
      "Substitution/Lambda",
      Right(819406151)
    )
    normalExpressionTest(
      List("""
package Substitution/Eta

out = \x,y -> x(y)
"""),
      "Substitution/Eta",
      Left(Lambda(LambdaVar(0)))
    )
    normalExpressionTest(
      List("""
package Substitution/Eta2

out = \x,y -> x(y, y)
"""),
      "Substitution/Eta2",
      Left(Lambda(Lambda(App(App(LambdaVar(1), LambdaVar(0)), LambdaVar(0)))))
    )
  }
  test("match simplification") {
    normalExpressionTest(
      List("""
package Match/Or

operator || = \a,b ->
  match (a,b):
    (True, _): True
    (False, result): result

operator < = \a, b -> a.cmp_Int(b) matches LT

out = \x -> True || (x < 5)
"""),
      "Match/Or",
      Left(Lambda(Struct(1, Nil, Enum)))
    )
  }

  test("varSet") {
    letFreeVarSetTest(
      List(
        """
package VarSet/External

external def foo(x: String) -> List[String]

def bar(_):
  foo

out = bar
"""
      ),
      "VarSet/External",
      1,
      Set.empty
    )

    letFreeVarSetTest(
      List(
        """
package VarSet/Match

def bar(y, _, x):
  match x:
    2: y
    _: "boom"

out = bar
"""
      ),
      "VarSet/Match",
      3,
      Set(0, 2)
    )

    letFreeVarSetTest(
      List(
        """
package VarSet/Match

def bar(y, _, _, x):
  match x:
    a@[b, *c, d, 2 | 3]: (a, b, c, d)
    _: (y, 0, y, 0)

out = bar
"""
      ),
      "VarSet/Match",
      4,
      Set(0, 3)
    )

    letFreeVarSetTest(
      List(
        """
package VarSet/Struct

struct Foo(a, b)

def bar(x):
  Foo(1, x)

out = bar
"""
      ),
      "VarSet/Struct",
      1,
      Set(0)
    )

    letFreeVarSetTest(
      List(
        """
package VarSet/Recursion

enum Thing:
  Thing1, Thing2(a: Int, t: Thing)

def foo(k: Int):
  def bar(x):
    recur x:
      Thing1: k
      Thing2(_, t): bar(t)
  bar

out = foo
"""
      ),
      "VarSet/Recursion",
      1,
      Set(0)
    )

    letFreeVarSetTest(
      List(
        """
package VarSet/LambdaVar

def foo(k, _):
  k

out = foo
"""
      ),
      "VarSet/LambdaVar",
      2,
      Set(1)
    )

    letFreeVarSetTest(
      List(
        """
package VarSet/Literal

def foo(_, _):
  25

out = foo
"""
      ),
      "VarSet/Literal",
      2,
      Set.empty
    )
  }

  test("LetFreeExpression conversions") {
    assert(
      LetFreeConversion.structListAsList(
        App(
          ExternalVar(
            PackageName(NonEmptyList.of("Extern", "LitMatch")),
            Identifier.Name("foo"),
            Type.Fun(Type.StrType, Type.ListT(Type.StrType))
          ),
          Literal(Str("bar"))
        )
      ) == None,
      "Should not be able to convert"
    )
  }
}
