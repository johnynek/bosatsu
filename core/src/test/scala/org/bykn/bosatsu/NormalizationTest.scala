package org.bykn.bosatsu

import org.scalatest.FunSuite
import java.math.BigInteger
import cats.data.NonEmptyList

class NormalizationTest extends FunSuite {
  import TestUtils._
  import NormalExpression._
  import Lit._
  import Normalization._
  import NormalPattern.{PositionalStruct, Var, WildCard}

  test("Literal") {
      normalTagTest(
        List("""
package NormTest/String

main = "aa"
"""
        ), "NormTest/String", NormalExpressionTag(Literal(Str("aa")), Set()), Some("Literal('aa')")
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
        ),
        Some("Struct(1,Literal('aa'),Struct(0,))")
      )
  }
  test("recurse") {
    normalExpressionTest(
      List("""
package Recur/Some

def foo(x):
  recur x:
    []: ["a","b","c"]
    [_, *t]: NonEmptyList("zero", foo(t))

out = foo
"""
      ), "Recur/Some", Recursion(Lambda(Lambda(Match(LambdaVar(0),NonEmptyList.of(
        (PositionalStruct(Some(0), Nil),Struct(1,List(Literal(Str("a")), Struct(1,List(Literal(Str("b")), Struct(1,List(Literal(Str("c")), Struct(0,List())))))))),
        (PositionalStruct(Some(1), List(WildCard, Var(0))),Lambda(Struct(1,List(Literal(Str("zero")), App(LambdaVar(2),LambdaVar(0))))))
      )))))
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
"""
      ), "Recur/FoldLeft", Recursion(Lambda(Lambda(Match(LambdaVar(0),NonEmptyList.of(
        (PositionalStruct(Some(0),List()),Lambda(Lambda(LambdaVar(1)))),
        (PositionalStruct(Some(1),List(Var(0), Var(1))),Lambda(Lambda(Lambda(Lambda(App(App(App(LambdaVar(5),LambdaVar(3)),App(App(LambdaVar(0),LambdaVar(1)),LambdaVar(2))),LambdaVar(0)))))))
      )))))
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
"""
      ), "Recur/FoldLeft",
      App(
        App(
          App(
            Recursion(Lambda(Lambda(Match(LambdaVar(0),NonEmptyList.of(
              (PositionalStruct(Some(0),List()),Lambda(Lambda(LambdaVar(1)))),
              (PositionalStruct(Some(1),List(Var(0), Var(1))),Lambda(Lambda(Lambda(Lambda(App(App(App(LambdaVar(5),LambdaVar(3)),App(App(LambdaVar(0),LambdaVar(1)),LambdaVar(2))),LambdaVar(0)))))))
            ))))),
            Struct(1,List(Literal(Integer(BigInteger.valueOf(1))), Struct(1,List(Literal(Integer(BigInteger.valueOf(2))), Struct(1,List(Literal(Integer(BigInteger.valueOf(3))), Struct(0,List())))))))
          ),
          Literal(Integer(BigInteger.valueOf(4)))
        ),
        ExternalVar(PackageName(NonEmptyList.of("Bosatsu", "Predef")),Identifier.Name("add"))
      )
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
"""
      ), "Recur/FoldLeft", Lambda(Lambda(Lambda(App(App(Recursion(Lambda(Lambda(Match(LambdaVar(0),NonEmptyList.of(
        (PositionalStruct(Some(0),List()),Lambda(LambdaVar(0))),
        (PositionalStruct(Some(1),List(Var(0), Var(1))),Lambda(Lambda(Lambda(App(App(LambdaVar(4),LambdaVar(2)),App(App(LambdaVar(5),LambdaVar(0)),LambdaVar(1)))))))
      ))))),LambdaVar(3)),LambdaVar(2)))))
    )
  }
  test("foldLeft applied") {
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

out = [1,2,3].foldLeft(4, add)
"""
      ), "Recur/FoldLeft",
    App(
      App(
        Recursion(Lambda(Lambda(Match(LambdaVar(0),NonEmptyList.of(
          (PositionalStruct(Some(0),List()),Lambda(LambdaVar(0))),
          (PositionalStruct(Some(1),List(Var(0), Var(1))),
            Lambda(Lambda(Lambda(
              App(App(LambdaVar(4),LambdaVar(2)),App(App(ExternalVar(PackageName(NonEmptyList.of("Bosatsu", "Predef")),Identifier.Name("add")),LambdaVar(0)),LambdaVar(1)))
            )))
          )
        ))))),
        LambdaVar(0)
      ),
      Struct(1,List(Literal(Integer(BigInteger.valueOf(1))), Struct(1,List(Literal(Integer(BigInteger.valueOf(2))), Struct(1,List(Literal(Integer(BigInteger.valueOf(3))), Struct(0,List())))))))
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

out = \x -> \_ -> x
"""
      ), "Lambda/Always", NormalExpressionTag(
        Lambda(Lambda(LambdaVar(1))), Set(Lambda(LambdaVar(1)), LambdaVar(1))
      )
    )
    normalTagTest(
      List("""
package Lambda/Always

out = \_ -> \y -> y
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

def foo(x, _):
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

def foo(_, y):
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
        Lambda(Match(LambdaVar(0),NonEmptyList.of((PositionalStruct(None,List(Var(0), PositionalStruct(None,List(Var(1), PositionalStruct(None,List()))))),Lambda(Lambda(Lambda(Struct(0,List(LambdaVar(2), Struct(0,List(LambdaVar(0), Struct(0,List(LambdaVar(1), Struct(0,List()))))))))))))))
      )

    normalExpressionTest(
      List("""
package Match/Structs

struct Pair(f, s)
struct Trip(f, s, t)

out=match Pair(1, "two"):
  Pair(f, s): Trip(3, s, f)

"""
      ), "Match/Structs",
      Struct(0,List(Literal(Lit(3)), Literal(Lit.Str("two")), Literal(Lit(1))))
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
  [_, _, last]: last
  _: 0
"""
      ), "Match/List",
      Literal(Integer(BigInteger.valueOf(3)))
    )
  normalExpressionTest(
    List("""
package Match/List

out = match [1,2,3,4,5]:
  [*_, _, _, last]: last
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
  normalExpressionTest(
    List("""
package Match/Union

enum Bar:
  Baz(a), Fizz(a), Buzz

out = match Buzz:
  Baz(x) | Fizz(x): x
  Buzz: "buzzzzzzz"
"""
      ), "Match/Union",
      Literal(Str("buzzzzzzz"))
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
    /*
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
    App(ExternalVar(PackageName(NonEmptyList.of("Extern", "Apply")),Identifier.Name("foo")),Literal(Str("bar")))
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
    Match(App(ExternalVar(PackageName(NonEmptyList.of(Extern, Match)),Name("foo")),Literal(Str("bar"))),NonEmptyList.of(PositionalStruct(Some(1),List(Var(0), PositionalStruct(Some(1),List(WildCard, PositionalStruct(Some(1),List(WildCard, PositionalStruct(Some(0),List()))))))),Lambda(LambdaVar(0))), (WildCard,Literal(Str("boom")))))
   */

    normalExpressionTest(
      List(
"""
package Extern/Eta

external def foo(x: String) -> List[String]

out = \y -> foo(y)
"""
    ), "Extern/Eta",
    ExternalVar(PackageName(NonEmptyList.fromList(List("Extern", "Eta")).get),Identifier.Name("foo")),
    Some("ExternalVar('Extern/Eta','foo')")
    )
    /*
    normalExpressionTest(
      List("""
package Extern/List

external def foo(x: String) -> List[String]

out = match foo("arg"):
  [*_, _, _, last]: last
  _: "'zero\\'"
"""
      ), "Extern/List",
      Match(
        App(ExternalVar(PackageName(NonEmptyList.fromList(List("Extern", "List")).get),Identifier.Name("foo")),Literal(Str("arg"))),
        NonEmptyList.fromList(List(
          (ListPat(List(Left(None), Right(WildCard), Right(WildCard), Right(Var(0)))),Lambda(LambdaVar(0))),
        (WildCard,Literal(Str("'zero\\'")))
        )).get
      ),
      Some("Match(App(ExternalVar('Extern/List','foo'),Literal('arg')),ListPat(Left(),Right(WildCard),Right(WildCard),Right(Var(0))),Lambda(LambdaVar(0)),WildCard,Literal('\\'zero\\\\\\''))")
    )
    normalExpressionTest(
      List("""
package Extern/List

external def foo(x: String) -> List[String]

out = match foo("arg"):
  [*_, _, _, last]: last
  _: "zero"
"""
        ), "Extern/List",
      Match(
        App(ExternalVar(PackageName(NonEmptyList.fromList(List("Extern", "List")).get),Identifier.Name("foo")),Literal(Str("arg"))),
        NonEmptyList.fromList(List(
          (ListPat(List(Left(None), Right(WildCard), Right(WildCard), Right(Var(0)))),Lambda(LambdaVar(0))),
          (WildCard,Literal(Str("zero")))
        )).get
      )
    )
    normalExpressionTest(
      List("""
package Extern/NamedMatch

external def foo(x: String) -> List[String]

struct Stuff(a,b)

out = match Stuff(foo("c"), "d"):
  Stuff(lst@[_], _): lst
  Stuff(_, y): [y]
"""
        ), "Extern/NamedMatch",
      Match(
        Struct(0,List(App(ExternalVar(PackageName(NonEmptyList.fromList(List("Extern", "NamedMatch")).get),Identifier.Name("foo")),Literal(Str("c"))), Literal(Str("d")))),
        NonEmptyList.fromList(List(
          (PositionalStruct(None,List(Named(0,ListPat(List(Right(WildCard)))), WildCard)),Lambda(LambdaVar(0))),
          (PositionalStruct(None,List(WildCard, Var(0))),Lambda(Struct(1,List(LambdaVar(0), Struct(0,List())))))
        )).get
      )
    )
    */
    normalExpressionTest(
      List("""
package Extern/LitMatch

external def foo(x: String) -> String

out = match foo("c"):
  "d": "e"
  _: "f"
"""
        ), "Extern/LitMatch",
      Match(
        App(ExternalVar(PackageName(NonEmptyList.fromList(List("Extern", "LitMatch")).get),Identifier.Name("foo")),Literal(Str("c"))),
        NonEmptyList.fromList(List(
          (NormalPattern.Literal(Str("d")),Literal(Str("e"))),
          (WildCard,Literal(Str("f")))
        )).get
      )
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
"""
      ), "Substitution/Lambda",
      Struct(0,
        List(
          App(Recursion(Lambda(Lambda(Match(LambdaVar(0),NonEmptyList.of((PositionalStruct(Some(0), Nil),Struct(1,List())), (PositionalStruct(Some(1), List(WildCard, Var(0))),Lambda(App(LambdaVar(2), LambdaVar(0))))))))),Struct(1,List(Literal(Str("zooom")),Struct(0,List())))),
          Struct(0,List(
            App(Recursion(Lambda(Lambda(Match(LambdaVar(0),NonEmptyList.of((PositionalStruct(Some(0), Nil),Struct(1,List())),(PositionalStruct(Some(1), List(WildCard, Var(0))),Lambda(App(LambdaVar(2), LambdaVar(0))))))))),Struct(1,List(Literal(Str("zooom")), Struct(0,List())))),
            Struct(0,List())
          ))
        )
      )
    )
    normalExpressionTest(
      List("""
package Substitution/Eta

out = \x,y -> x(y)
"""
      ), "Substitution/Eta", Lambda(LambdaVar(0))
    )
    normalExpressionTest(
      List("""
package Substitution/Eta2

out = \x,y -> x(y, y)
"""
      ), "Substitution/Eta2", Lambda(Lambda(App(App(LambdaVar(1), LambdaVar(0)), LambdaVar(0))))
    )
  }
}
