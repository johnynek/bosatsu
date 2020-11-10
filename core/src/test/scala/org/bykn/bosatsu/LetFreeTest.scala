package org.bykn.bosatsu

import java.math.BigInteger
import cats.data.NonEmptyList
import org.scalatest.funsuite.AnyFunSuite
import rankn.Type
import rankn.DataFamily.{Enum, Struct => DFStruct}

class LetFreeTest extends AnyFunSuite {
  import TestUtils._
  import LetFreeExpression._
  import Lit._
  import LetFreePattern.{
    PositionalStruct,
    Var,
    WildCard,
    ListPat,
    Named,
    StrPat,
    StrPart
  }

  test("Literal") {
    normalExpressionTest(
      List("""
package LetFreeTest/String

main = "aa"
"""),
      "LetFreeTest/String",
      Literal(Str("aa"))
    )

    normalExpressionTest(
      List("""
package LetFreeTest/Int

main = 22
"""),
      "LetFreeTest/Int",
      Literal(Integer(BigInteger.valueOf(22)))
    )

    normalExpressionTest(
      List("""
package LetFreeTest/List

main = ["aa"]
"""),
      "LetFreeTest/List",
      Struct(1, List(Literal(Str("aa")), Struct(0, List(), Enum)), Enum)
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
      Recursion(
        Lambda(
          Lambda(
            Match(
              LambdaVar(0),
              NonEmptyList.of(
                (
                  PositionalStruct(Some(0), Nil, Enum),
                  Struct(
                    1,
                    List(
                      Literal(Str("a")),
                      Struct(
                        1,
                        List(
                          Literal(Str("b")),
                          Struct(
                            1,
                            List(Literal(Str("c")), Struct(0, List(), Enum)),
                            Enum
                          )
                        ),
                        Enum
                      )
                    ),
                    Enum
                  )
                ),
                (
                  PositionalStruct(
                    Some(1),
                    List(
                      StrPat(
                        NonEmptyList
                          .of(StrPart.LitStr("a: "), StrPart.NamedStr(0))
                      ),
                      Var(1)
                    ),
                    Enum
                  ),
                  Lambda(
                    Lambda(
                      Struct(
                        1,
                        List(LambdaVar(0), App(LambdaVar(3), LambdaVar(1))),
                        Enum
                      )
                    )
                  )
                ),
                (
                  PositionalStruct(Some(1), List(WildCard, Var(0)), Enum),
                  Lambda(
                    Struct(
                      1,
                      List(
                        Literal(Str("zero")),
                        App(LambdaVar(2), LambdaVar(0))
                      ),
                      Enum
                    )
                  )
                )
              )
            )
          )
        )
      ),
      List({ lfe: LetFreeExpression =>
        assert(
          lfe.asString ==
            "Recursion(Lambda(Lambda(Match(LambdaVar(0),PositionalStruct(0,),Struct(1,Literal('a'),Struct(1,Literal('b'),Struct(1,Literal('c'),Struct(0,)))),PositionalStruct(1,StrPat(LitStr(a: ),NamedStr(0)),Var(1)),Lambda(Lambda(Struct(1,LambdaVar(0),App(LambdaVar(3),LambdaVar(1))))),PositionalStruct(1,WildCard,Var(0)),Lambda(Struct(1,Literal('zero'),App(LambdaVar(2),LambdaVar(0))))))))",
          "Serializations test"
        )
      })
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
      Recursion(
        Lambda(
          Lambda(
            Match(
              LambdaVar(0),
              NonEmptyList.of(
                (
                  PositionalStruct(Some(0), List(), Enum),
                  Lambda(Lambda(LambdaVar(1)))
                ),
                (
                  PositionalStruct(Some(1), List(Var(0), Var(1)), Enum),
                  Lambda(
                    Lambda(
                      Lambda(
                        Lambda(
                          App(
                            App(
                              App(LambdaVar(5), LambdaVar(3)),
                              App(App(LambdaVar(0), LambdaVar(1)), LambdaVar(2))
                            ),
                            LambdaVar(0)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
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
      App(
        App(
          App(
            Recursion(
              Lambda(
                Lambda(
                  Match(
                    LambdaVar(0),
                    NonEmptyList.of(
                      (
                        PositionalStruct(Some(0), List(), Enum),
                        Lambda(Lambda(LambdaVar(1)))
                      ),
                      (
                        PositionalStruct(Some(1), List(Var(0), Var(1)), Enum),
                        Lambda(
                          Lambda(
                            Lambda(
                              Lambda(
                                App(
                                  App(
                                    App(LambdaVar(5), LambdaVar(3)),
                                    App(
                                      App(LambdaVar(0), LambdaVar(1)),
                                      LambdaVar(2)
                                    )
                                  ),
                                  LambdaVar(0)
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            Struct(
              1,
              List(
                Literal(Integer(BigInteger.valueOf(1))),
                Struct(
                  1,
                  List(
                    Literal(Integer(BigInteger.valueOf(2))),
                    Struct(
                      1,
                      List(
                        Literal(Integer(BigInteger.valueOf(3))),
                        Struct(0, List(), Enum)
                      ),
                      Enum
                    )
                  ),
                  Enum
                )
              ),
              Enum
            )
          ),
          Literal(Integer(BigInteger.valueOf(4)))
        ),
        ExternalVar(
          PackageName(NonEmptyList.of("Bosatsu", "Predef")),
          Identifier.Name("add"),
          Type.Fun(Type.IntType, Type.Fun(Type.IntType, Type.IntType))
        )
      ),
      List({ lfe: LetFreeExpression =>
        assert(
          lfe.asString == "App(App(App(Recursion(Lambda(Lambda(Match(LambdaVar(0),PositionalStruct(0,),Lambda(Lambda(LambdaVar(1))),PositionalStruct(1,Var(0),Var(1)),Lambda(Lambda(Lambda(Lambda(App(App(App(LambdaVar(5),LambdaVar(3)),App(App(LambdaVar(0),LambdaVar(1)),LambdaVar(2))),LambdaVar(0)))))))))),Struct(1,Literal(1),Struct(1,Literal(2),Struct(1,Literal(3),Struct(0,))))),Literal(4)),ExternalVar('Bosatsu/Predef','add', 'Bosatsu/Predef::Int -> Bosatsu/Predef::Int -> Bosatsu/Predef::Int'))",
          s"Serializations test got: ${lfe.asString}"
        )
      })
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
      Lambda(
        Lambda(
          Lambda(
            App(
              App(
                Recursion(
                  Lambda(
                    Lambda(
                      Match(
                        LambdaVar(0),
                        NonEmptyList.of(
                          (
                            PositionalStruct(Some(0), List(), Enum),
                            Lambda(LambdaVar(0))
                          ),
                          (
                            PositionalStruct(
                              Some(1),
                              List(Var(0), Var(1)),
                              Enum
                            ),
                            Lambda(
                              Lambda(
                                Lambda(
                                  App(
                                    App(LambdaVar(4), LambdaVar(2)),
                                    App(
                                      App(LambdaVar(5), LambdaVar(0)),
                                      LambdaVar(1)
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                LambdaVar(2)
              ),
              LambdaVar(1)
            )
          )
        )
      )
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
      App(
        App(
          Recursion(
            Lambda(
              Lambda(
                Match(
                  LambdaVar(0),
                  NonEmptyList.of(
                    (
                      PositionalStruct(Some(0), List(), Enum),
                      Lambda(LambdaVar(0))
                    ),
                    (
                      PositionalStruct(Some(1), List(Var(0), Var(1)), Enum),
                      Lambda(
                        Lambda(
                          Lambda(
                            App(
                              App(LambdaVar(4), LambdaVar(2)),
                              App(
                                App(
                                  ExternalVar(
                                    PackageName(
                                      NonEmptyList.of("Bosatsu", "Predef")
                                    ),
                                    Identifier.Name("add"),
                                    Type.Fun(
                                      Type.IntType,
                                      Type.Fun(Type.IntType, Type.IntType)
                                    )
                                  ),
                                  LambdaVar(0)
                                ),
                                LambdaVar(1)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          Struct(
            1,
            List(
              Literal(Integer(BigInteger.valueOf(1))),
              Struct(
                1,
                List(
                  Literal(Integer(BigInteger.valueOf(2))),
                  Struct(
                    1,
                    List(
                      Literal(Integer(BigInteger.valueOf(3))),
                      Struct(0, List(), Enum)
                    ),
                    Enum
                  )
                ),
                Enum
              )
            ),
            Enum
          )
        ),
        Literal(Integer(BigInteger.valueOf(9)))
      )
    )
  }
  test("Lambda") {
    normalExpressionTest(
      List("""
package Lambda/Identity

out = \x -> x
"""),
      "Lambda/Identity",
      Lambda(LambdaVar(0))
    )
    normalExpressionTest(
      List("""
package Lambda/Always

out = \x -> \_ -> x
"""),
      "Lambda/Always",
      Lambda(Lambda(LambdaVar(1)))
    )
    normalExpressionTest(
      List("""
package Lambda/Always

out = \_ -> \y -> y
"""),
      "Lambda/Always",
      Lambda(Lambda(LambdaVar(0)))
    )

    normalExpressionTest(
      List("""
package Lambda/Identity

def foo(x):
  x
out = foo
"""),
      "Lambda/Identity",
      Lambda(LambdaVar(0))
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
      Lambda(LambdaVar(0))
    )
    normalExpressionTest(
      List("""
package Lambda/Always

def foo(x, _):
  x
out = foo
"""),
      "Lambda/Always",
      Lambda(Lambda(LambdaVar(1)))
    )
    normalExpressionTest(
      List("""
package Lambda/Always

def foo(_, y):
  y
out = foo
"""),
      "Lambda/Always",
      Lambda(Lambda(LambdaVar(0)))
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
      Lambda(
        Match(
          LambdaVar(0),
          NonEmptyList.of(
            (
              PositionalStruct(
                None,
                List(
                  Var(0),
                  PositionalStruct(
                    None,
                    List(Var(1), PositionalStruct(None, List(), DFStruct)),
                    DFStruct
                  )
                ),
                DFStruct
              ),
              Lambda(
                Lambda(
                  Lambda(
                    Struct(
                      0,
                      List(
                        LambdaVar(2),
                        Struct(
                          0,
                          List(
                            LambdaVar(0),
                            Struct(
                              0,
                              List(LambdaVar(1), Struct(0, List(), DFStruct)),
                              DFStruct
                            )
                          ),
                          DFStruct
                        )
                      ),
                      DFStruct
                    )
                  )
                )
              )
            )
          )
        )
      )
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
      Struct(
        0,
        List(Literal(Lit(3)), Literal(Lit.Str("two")), Literal(Lit(1))),
        DFStruct
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
      Literal(Str("not some"))
    )
    normalExpressionTest(
      List("""
package Match/List

out = match [1,2,3]:
  [_, _, last]: last
  _: 0
"""),
      "Match/List",
      Literal(Integer(BigInteger.valueOf(3)))
    )
    normalExpressionTest(
      List("""
package Match/List

out = match [1,2,3,4,5]:
  [*_, _, _, last]: last
  _: 0
"""),
      "Match/List",
      Literal(Integer(BigInteger.valueOf(5)))
    )
    normalExpressionTest(
      List("""
package Match/List

out = match ["a","b","c","d","e"]:
  [h, *t]: Some((h, t))
  []: None
"""),
      "Match/List",
      Struct(
        1,
        List(
          Struct(
            0,
            List(
              Literal(Str("a")),
              Struct(
                0,
                List(
                  Struct(
                    1,
                    List(
                      Literal(Str("b")),
                      Struct(
                        1,
                        List(
                          Literal(Str("c")),
                          Struct(
                            1,
                            List(
                              Literal(Str("d")),
                              Struct(
                                1,
                                List(
                                  Literal(Str("e")),
                                  Struct(0, List(), Enum)
                                ),
                                Enum
                              )
                            ),
                            Enum
                          )
                        ),
                        Enum
                      )
                    ),
                    Enum
                  ),
                  Struct(0, List(), DFStruct)
                ),
                DFStruct
              )
            ),
            DFStruct
          )
        ),
        Enum
      )
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
"""),
      "Match/Union",
      Literal(Str("buzzzzzzz"))
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
      Struct(
        1,
        List(
          Literal(Integer(BigInteger.valueOf(1))),
          Struct(
            1,
            List(
              Literal(Integer(BigInteger.valueOf(2))),
              Struct(
                1,
                List(
                  Literal(Integer(BigInteger.valueOf(3))),
                  Struct(0, List(), Enum)
                ),
                Enum
              )
            ),
            Enum
          )
        ),
        Enum
      )
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
      Match(
        Struct(
          1,
          List(
            Literal(Integer(BigInteger.valueOf(1))),
            App(
              ExternalVar(
                PackageName(NonEmptyList.of("Match", "ListPat")),
                Identifier.Name("fn"),
                Type.Fun(Type.StrType, Type.ListT(Type.IntType))
              ),
              Literal(Str("bar"))
            )
          ),
          Enum
        ),
        NonEmptyList.of(
          (
            ListPat(
              List(
                Left(Some(0)),
                Right(LetFreePattern.Literal(Integer(BigInteger.valueOf(4)))),
                Left(None)
              )
            ),
            Lambda(LambdaVar(0))
          ),
          (WildCard, Struct(0, List(), Enum))
        )
      )
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
      Struct(
        0,
        List(
          Literal(Integer(BigInteger.valueOf(2))),
          Struct(
            0,
            List(
              Literal(Integer(BigInteger.valueOf(1))),
              Struct(0, List(), DFStruct)
            ),
            DFStruct
          )
        ),
        DFStruct
      )
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
      Struct(
        0,
        List(
          Literal(Integer(BigInteger.valueOf(2))),
          Struct(
            0,
            List(
              Literal(Integer(BigInteger.valueOf(1))),
              Struct(0, List(), DFStruct)
            ),
            DFStruct
          )
        ),
        DFStruct
      )
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
      ExternalVar(
        PackageName(
          NonEmptyList
            .fromList(
              List("Extern", "Simple")
            )
            .get
        ),
        Identifier.Name("foo"),
        Type.Fun(Type.StrType, Type.ListT(Type.StrType))
      )
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
      App(
        ExternalVar(
          PackageName(NonEmptyList.of("Extern", "Apply")),
          Identifier.Name("foo"),
          Type.Fun(Type.StrType, Type.ListT(Type.StrType))
        ),
        Literal(Str("bar"))
      )
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
      Match(
        App(
          ExternalVar(
            PackageName(NonEmptyList.of("Extern", "Match")),
            Identifier.Name("foo"),
            Type.Fun(Type.StrType, Type.ListT(Type.StrType))
          ),
          Literal(Str("bar"))
        ),
        NonEmptyList.of(
          (
            PositionalStruct(
              Some(1),
              List(
                Var(0),
                PositionalStruct(
                  Some(1),
                  List(
                    WildCard,
                    PositionalStruct(
                      Some(1),
                      List(
                        WildCard,
                        PositionalStruct(
                          Some(0),
                          List(),
                          Enum
                        )
                      ),
                      Enum
                    )
                  ),
                  Enum
                )
              ),
              Enum
            ),
            Lambda(LambdaVar(0))
          ),
          (WildCard, Literal(Str("boom")))
        )
      )
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
      ExternalVar(
        PackageName(NonEmptyList.fromList(List("Extern", "Eta")).get),
        Identifier.Name("foo"),
        Type.Fun(Type.StrType, Type.TyApply(Type.ListType, Type.StrType))
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
      Match(
        App(
          ExternalVar(
            PackageName(NonEmptyList.of("Extern", "List")),
            Identifier.Name("foo"),
            Type.Fun(
              Type.StrType,
              Type.ListT(Type.Tuple(List(Type.StrType, Type.IntType)))
            )
          ),
          Literal(Str("arg"))
        ),
        NonEmptyList.of(
          (
            PositionalStruct(
              Some(1),
              List(
                WildCard,
                PositionalStruct(
                  Some(1),
                  List(
                    WildCard,
                    ListPat(
                      List(
                        Left(None),
                        Right(
                          PositionalStruct(
                            None,
                            List(
                              Var(0),
                              PositionalStruct(
                                None,
                                List(
                                  LetFreePattern.Literal(
                                    Integer(BigInteger.valueOf(1))
                                  ),
                                  PositionalStruct(None, List(), DFStruct)
                                ),
                                DFStruct
                              )
                            ),
                            DFStruct
                          )
                        )
                      )
                    )
                  ),
                  Enum
                )
              ),
              Enum
            ),
            Lambda(LambdaVar(0))
          ),
          (WildCard, Literal(Str("'zero\\'")))
        )
      ),
      List({ lfe: LetFreeExpression =>
        assert(
          lfe.asString ==
            "Match(App(ExternalVar('Extern/List','foo', 'Bosatsu/Predef::String -> Bosatsu/Predef::List[(Bosatsu/Predef::String, Bosatsu/Predef::Int)]'),Literal('arg')),PositionalStruct(1,WildCard,PositionalStruct(1,WildCard,ListPat(Left(),Right(PositionalStruct(,Var(0),PositionalStruct(,Literal(1),PositionalStruct(,))))))),Lambda(LambdaVar(0)),WildCard,Literal('\\'zero\\\\\\''))",
          "Serializations test"
        )
      })
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
      Match(
        App(
          ExternalVar(
            PackageName(NonEmptyList.of("Extern", "List")),
            Identifier.Name("foo"),
            Type.Fun(Type.StrType, Type.ListT(Type.StrType))
          ),
          Literal(Str("arg"))
        ),
        NonEmptyList.of(
          (
            PositionalStruct(
              Some(1),
              List(
                WildCard,
                PositionalStruct(
                  Some(1),
                  List(WildCard, ListPat(List(Left(None), Right(Var(0))))),
                  Enum
                )
              ),
              Enum
            ),
            Lambda(LambdaVar(0))
          ),
          (WildCard, Literal(Str("zero")))
        )
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
"""),
      "Extern/NamedMatch",
      Match(
        Struct(
          0,
          List(
            App(
              ExternalVar(
                PackageName(NonEmptyList.of("Extern", "NamedMatch")),
                Identifier.Name("foo"),
                Type.Fun(Type.StrType, Type.ListT(Type.StrType))
              ),
              Literal(Str("c"))
            ),
            Literal(Str("d"))
          ),
          DFStruct
        ),
        NonEmptyList.of(
          (
            PositionalStruct(
              None,
              List(
                Named(
                  0,
                  PositionalStruct(
                    Some(1),
                    List(WildCard, PositionalStruct(Some(0), List(), Enum)),
                    Enum
                  )
                ),
                WildCard
              ),
              DFStruct
            ),
            Lambda(LambdaVar(0))
          ),
          (
            PositionalStruct(None, List(WildCard, Var(0)), DFStruct),
            Lambda(Struct(1, List(LambdaVar(0), Struct(0, List(), Enum)), Enum))
          )
        )
      )
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
      Match(
        App(
          ExternalVar(
            PackageName(NonEmptyList.of("Extern", "LitMatch")),
            Identifier.Name("foo"),
            Type.Fun(Type.StrType, Type.StrType)
          ),
          Literal(Str("c"))
        ),
        NonEmptyList.of(
          (
            LetFreePattern.Named(
              0,
              LetFreePattern.Union(
                LetFreePattern.Literal(Str("d")),
                NonEmptyList.of(LetFreePattern.Literal(Str("dd")))
              )
            ),
            Lambda(LambdaVar(0))
          ),
          (WildCard, Literal(Str("f")))
        )
      ),
      List({ lfe: LetFreeExpression =>
        assert(
          lfe.asString ==
            "Match(App(ExternalVar('Extern/LitMatch','foo', 'Bosatsu/Predef::String -> Bosatsu/Predef::String'),Literal('c')),Named(0,Union(Literal('d'),Literal('dd'))),Lambda(LambdaVar(0)),WildCard,Literal('f'))",
          "Serializations test"
        )
      })
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
      Struct(
        0,
        List(
          App(
            Recursion(
              Lambda(
                Lambda(
                  Match(
                    LambdaVar(0),
                    NonEmptyList.of(
                      (
                        PositionalStruct(Some(0), Nil, Enum),
                        Struct(1, List(), Enum)
                      ),
                      (
                        PositionalStruct(Some(1), List(WildCard, Var(0)), Enum),
                        Lambda(App(LambdaVar(2), LambdaVar(0)))
                      )
                    )
                  )
                )
              )
            ),
            Struct(
              1,
              List(Literal(Str("zooom")), Struct(0, List(), Enum)),
              Enum
            )
          ),
          Struct(
            0,
            List(
              App(
                Recursion(
                  Lambda(
                    Lambda(
                      Match(
                        LambdaVar(0),
                        NonEmptyList.of(
                          (
                            PositionalStruct(Some(0), Nil, Enum),
                            Struct(1, List(), Enum)
                          ),
                          (
                            PositionalStruct(
                              Some(1),
                              List(WildCard, Var(0)),
                              Enum
                            ),
                            Lambda(App(LambdaVar(2), LambdaVar(0)))
                          )
                        )
                      )
                    )
                  )
                ),
                Struct(
                  1,
                  List(Literal(Str("zooom")), Struct(0, List(), Enum)),
                  Enum
                )
              ),
              Struct(0, List(), DFStruct)
            ),
            DFStruct
          )
        ),
        DFStruct
      )
    )
    normalExpressionTest(
      List("""
package Substitution/Eta

out = \x,y -> x(y)
"""),
      "Substitution/Eta",
      Lambda(LambdaVar(0))
    )
    normalExpressionTest(
      List("""
package Substitution/Eta2

out = \x,y -> x(y, y)
"""),
      "Substitution/Eta2",
      Lambda(Lambda(App(App(LambdaVar(1), LambdaVar(0)), LambdaVar(0))))
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
      Lambda(Struct(1, Nil, Enum))
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
