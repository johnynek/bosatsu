package org.bykn.bosatsu

import org.scalatest.funsuite.AnyFunSuite
import TestUtils.testInferred
import org.scalatest.Assertion
import org.scalatest.Assertions.{succeed, fail}
import cats.Eval
import java.math.BigInteger
import Value.{SumValue, ConsValue, ExternalValue, UnitValue, FnValue}
import org.bykn.bosatsu.TypedExpr.AnnotatedLambda
import cats.data.NonEmptyList

object ExpressionEvaluationTest {
  def evalTest(
      packages: List[String],
      mainPackS: String,
      ext: Externals,
      assertions: List[
        (
            (Eval[Value], rankn.Type),
            ExpressionEvaluation[Declaration]
        ) => Assertion
      ]
  ) = {
    def inferredHandler(
        infPackMap: PackageMap.Inferred,
        mainPack: PackageName
    ) = {
      val ev = ExpressionEvaluation(infPackMap, Predef.jvmExternals ++ ext)
      ev.evaluateLast(mainPack) match {
        case Some(res) => {

          assertions.foreach(_.apply(res, ev))
          succeed
        }
        case None => fail("There should be a last expression")
      }
    }
    testInferred(packages, mainPackS, inferredHandler(_, _))
  }
}

class ExpressionEvaluationTest extends AnyFunSuite {
  import ExpressionEvaluationTest.evalTest
  test("Literal") {
    evalTest(
      List("""
package LetFreeTest/String
main = "aa"
"""),
      "LetFreeTest/String",
      Externals(Map.empty),
      List({ (x, _) =>
        assert(x._1.value == Value.ExternalValue("aa"), x._1.value)
      })
    )

    evalTest(
      List("""
package LetFreeTest/String
main = 22
"""),
      "LetFreeTest/String",
      Externals(Map.empty),
      List({ (x, _) =>
        assert(
          x._1.value == Value.ExternalValue(BigInteger.valueOf(22)),
          x._1.value
        )
      })
    )

    evalTest(
      List("""
package LetFreeTest/String
main = [23]
"""),
      "LetFreeTest/String",
      Externals(Map.empty),
      List({ (x, _) =>
        assert(
          x._1.value == SumValue(
            1,
            ConsValue(
              ExternalValue(BigInteger.valueOf(23)),
              ConsValue(SumValue(0, UnitValue), UnitValue)
            )
          ),
          x._1.value
        )
      })
    )
  }

  test("recurse") {
    evalTest(
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
      Externals(Map.empty),
      List({ (x, _) =>
        x._1.value match {
          case FnValue(_) => succeed
          case _          => fail()
        }
      })
    )
  }

  test("basic match") {
    evalTest(
      List("""
package Match/Basic

out = match [10,2,3]:
  []: 0
  [_, h2, *_]: h2
  [head, *_]: head
"""),
      "Match/Basic",
      Externals(Map.empty),
      List({ (x, _) =>
        assert(
          x._1.value == Value.ExternalValue(BigInteger.valueOf(2)),
          x._1.value
        )
      })
    )
  }

  test("predef applied") {
    evalTest(
      List("""
package Predef/Applied

out = 1.sub(4)
"""),
      "Predef/Applied",
      Externals(Map.empty),
      List({ (x, _) =>
        assert(x._1.value == Value.ExternalValue(BigInteger.valueOf(-3)))
      })
    )
  }

  test("apply function") {
    evalTest(
      List("""
package Apply/Function
def first(_, a):
  a
out = first(1,2)
"""),
      "Apply/Function",
      Externals(Map.empty),
      List({ (x, _) =>
        assert(x._1.value == Value.ExternalValue(BigInteger.valueOf(2)))
      })
    )
  }

  test("Positional struct") {
    evalTest(
      List("""
package Positional/Minus
def floorMinus(pair):
  match pair:
    (a, b): match cmp_Int(a,b):
      LT: b.sub(a)
      _: -1
out = floorMinus((2,5))
"""),
      "Positional/Minus",
      Externals(Map.empty),
      List({ (x, _) =>
        assert(x._1.value == Value.ExternalValue(BigInteger.valueOf(3)))
      })
    )
  }

  test("foldLeft applied") {
    evalTest(
      List("""
package Recur/FoldLeft
def foldLeft(lst: List[a], item: b, fn: b -> a -> b) -> b:
  recur lst:
    EmptyList: item
    NonEmptyList(head, tail): foldLeft(tail, fn(item, head), fn)
out = [1,2,3].foldLeft(4, add)
"""),
      "Recur/FoldLeft",
      Externals(Map.empty),
      List({ (x, _) =>
        assert(x._1.value == Value.ExternalValue(BigInteger.valueOf(10)))
      })
    )
  }

  test("nested foldLeft applied") {
    evalTest(
      List("""
package Recur/FoldLeft
def foldLeft(lst: List[a], item: b, fn: b -> a -> b) -> b:
  def loop(lstL: List[a], itemL: b) -> b:
    recur lstL:
      EmptyList: itemL
      NonEmptyList(head, tail): loop(tail, fn(itemL, head))
  loop(lst, item)
out = [1,2,3].foldLeft(4, add)
"""),
      "Recur/FoldLeft",
      Externals(Map.empty),
      List({ (x, _) =>
        assert(x._1.value == Value.ExternalValue(BigInteger.valueOf(10)))
      })
    )
  }

  test("matching") {
    evalTest(
      List("""
package Match/Structs
struct Pair(f, s)
struct Trip(f, s, t)
out=match Pair(1, "two"):
  Pair(f, s): Trip(3, s, f)
"""),
      "Match/Structs",
      Externals(Map.empty),
      List({ (x, _) =>
        assert(
          x._1.value == ConsValue(
            ExternalValue(BigInteger.valueOf(3)),
            ConsValue(
              ExternalValue("two"),
              ConsValue(ExternalValue(BigInteger.valueOf(1)), UnitValue)
            )
          )
        )
      })
    )
  }

  test("Lambda") {
    evalTest(
      List("""
package Lambda/Identity

out = \x -> x
"""),
      "Lambda/Identity",
      Externals(Map.empty),
      List({ (x, ev) =>
        x._1.value match {
          case fn: FnValue =>
            fn.arg match {
              case efn: ev.ExpressionFnValue =>
                assert(efn.arg.asString == "x")
                efn.lambda match {
                  case TypedExpr.Local(name, _, _) =>
                    assert(name.asString == "x")
                  case notLocal =>
                    throw new Exception(s"not a local: $notLocal")
                }
              case _ => fail()
            }
          case _ => fail()
        }

      })
    )
    evalTest(
      List("""
package Lambda/Always
out = \x -> \_ -> x
"""),
      "Lambda/Always",
      Externals(Map.empty),
      List({ (x, ev) =>
        x._1.value match {
          case fn: FnValue =>
            fn.arg match {
              case efn: ev.ExpressionFnValue =>
                assert(efn.arg.asString == "x")
                efn.lambda match {
                  case TypedExpr.AnnotatedLambda(
                        _,
                        _,
                        TypedExpr.Local(name, _, _),
                        _
                      ) =>
                    assert(name.asString == "x")
                  case notLambda =>
                    throw new Exception(s"not a lambda: $notLambda")
                }
              case _ => fail()
            }
          case _ => fail()
        }

      })
    )

    evalTest(
      List("""
package Lambda/Always
out = \_ -> \y -> y
"""),
      "Lambda/Always",
      Externals(Map.empty),
      List({ (x, ev) =>
        x._1.value match {
          case fn: FnValue =>
            fn.arg match {
              case efn: ev.ExpressionFnValue =>
                assert(efn.arg.asString == "a")
                efn.lambda match {
                  case TypedExpr.AnnotatedLambda(
                        Identifier.Name(arg),
                        _,
                        TypedExpr.Local(name, _, _),
                        _
                      ) =>
                    assert(arg == "y")
                    assert(name.asString == "y")
                  case notLambda =>
                    throw new Exception(s"not a lambda: $notLambda")
                }
              case _ => fail()
            }
          case _ => fail()
        }

      })
    )
    evalTest(
      List("""
package Lambda/Identity

def foo(x):
  x
out = foo
"""),
      "Lambda/Identity",
      Externals(Map.empty),
      List({ (x, ev) =>
        x._1.value match {
          case fn: FnValue =>
            fn.arg match {
              case efn: ev.ExpressionFnValue =>
                assert(efn.arg.asString == "x")
                efn.lambda match {
                  case TypedExpr.Local(name, _, _) =>
                    assert(name.asString == "x")
                  case notLocal =>
                    throw new Exception(s"not a local: $notLocal")
                }
              case _ => fail()
            }
          case _ => fail()
        }

      })
    )
    evalTest(
      List("""
package Match/Vars
def result(x, c):
  match x:
    (a, b): (b, c, a)
out=result
"""),
      "Match/Vars",
      Externals(Map.empty),
      List({ (x, ev) =>
        x._1.value match {
          case fn: FnValue =>
            fn.arg match {
              case efn: ev.ExpressionFnValue =>
                assert(efn.arg.asString == "x")
                efn.lambda match {
                  case TypedExpr.Match(
                        TypedExpr.Local(name, _, _),
                        NonEmptyList(
                          (
                            _,
                            TypedExpr
                              .AnnotatedLambda(Identifier.Name(c), _, _, _)
                          ),
                          Nil
                        ),
                        _
                      ) =>
                    assert(name.asString == "x")
                    assert(c == "c")
                  case notMatch =>
                    throw new Exception(s"not the match I wanted: $notMatch")
                }
              case _ => fail()
            }
          case _ => fail()
        }

      })
    )

    evalTest(
      List("""
package Match/Structs
struct Pair(f, s)
struct Trip(f, s, t)
out=match Pair(1, "two"):
  Pair(f, s): Trip(3, s, f)
"""),
      "Match/Structs",
      Externals(Map.empty),
      List({ (x, ev) =>
        assert(
          x._1.value == ConsValue(
            Value.ExternalValue(BigInteger.valueOf(3)),
            ConsValue(
              Value.ExternalValue("two"),
              ConsValue(Value.ExternalValue(BigInteger.valueOf(1)), UnitValue)
            )
          )
        )

      })
    )
    evalTest(
      List("""
package Match/None
out=match None:
  Some(_): "some"
  _: "not some"
"""),
      "Match/None",
      Externals(Map.empty),
      List({ (x, ev) => assert(x._1.value == Value.ExternalValue("not some")) })
    )

    evalTest(
      List("""
package Match/List
out = match [1,2,3]:
  [_, _, last]: last
  _: 0
"""),
      "Match/List",
      Externals(Map.empty),
      List({ (x, ev) =>
        assert(x._1.value == Value.ExternalValue(BigInteger.valueOf(3)))
      })
    )

    evalTest(
      List("""
package Match/List
out = match ["a","b","c","d","e"]:
  [h, *t]: Some((h, t))
  []: None
"""),
      "Match/List",
      Externals(Map.empty),
      List(
        (
            (
                x,
                ev
            ) =>
              x._1.value match {
                case Value.VOption(
                      Some(
                        Value.TupleCons(
                          ExternalValue("a"),
                          Value.TupleCons(Value.VList(list), UnitValue)
                        )
                      )
                    ) =>
                  assert(
                    list == List(
                      ExternalValue("b"),
                      ExternalValue("c"),
                      ExternalValue("d"),
                      ExternalValue("e")
                    )
                  )
                case _ => fail()
              }
        )
      )
    )

    evalTest(
      List("""
package Match/Union
enum Bar:
  Baz(a), Fizz(a), Buzz
out = match Baz("abc"):
  Baz(x) | Fizz(x): x
  Buzz: "buzzzzzzz"
"""),
      "Match/Union",
      Externals(Map.empty),
      List(
        (
            (
                x,
                ev
            ) => assert(x._1.value == ExternalValue("abc"))
        )
      )
    )

    evalTest(
      List("""
package Match/Union
enum Bar:
  Baz(a), Fizz(a), Buzz
out = match Buzz:
  Baz(x) | Fizz(x): x
  Buzz: "buzzzzzzz"
"""),
      "Match/Union",
      Externals(Map.empty),
      List(
        (
            (
                x,
                ev
            ) => assert(x._1.value == ExternalValue("buzzzzzzz"))
        )
      )
    )
  }

  test("imports") {
    evalTest(
      List(
        """
package Imp/First
export fizz
def fizz(f, s):
  (s, f)
""",
        """
package Imp/Second
from Imp/First import fizz
out=fizz(1,2)
"""
      ),
      "Imp/Second",
      Externals(Map.empty),
      List((v, ev) =>
        v._1.value match {
          case Value.Tuple(List(a, b)) => {
            assert(a == ExternalValue(BigInteger.valueOf(2)))
            assert(b == ExternalValue(BigInteger.valueOf(1)))
          }
          case _ => fail()
        }
      )
    )

    evalTest(
      List(
        """
package Imp/First
from Imp/Second import fizz
out=fizz(1,2)
""",
        """
package Imp/Second
export fizz
def fizz(f, s):
  (s, f)
"""
      ),
      "Imp/First",
      Externals(Map.empty),
      List((v, ev) =>
        v._1.value match {
          case Value.Tuple(List(a, b)) => {
            assert(a == ExternalValue(BigInteger.valueOf(2)))
            assert(b == ExternalValue(BigInteger.valueOf(1)))
          }
          case _ => fail()
        }
      )
    )
  }

  test("external") {
    val fn1 = FfiCall.Fn1(x => Value.VList(List(x)))
    evalTest(
      List("""
package Extern/Simple
external def foo(x: String) -> List[String]
out = foo
"""),
      "Extern/Simple",
      Externals(
        Map((PackageName(NonEmptyList("Extern", List("Simple"))), "foo") -> fn1)
      ),
      List({ case ((v, t), ev) => assert(v.value == fn1.call(t)) })
    )
  }

  test("recursion complexity") {
    var calls: Map[(BigInteger, BigInteger), Int] = Map.empty
    val fn2 = FfiCall.Fn2({ case (Value.VInt(x), Value.VInt(y)) =>
      println(s"$x $y")
      val cnt = calls.get((x, y)) match {
        case None    => 1
        case Some(n) => n + 1
      }
      /*if (cnt > 1) {
        throw new Error(s"called $x $y")
      }*/
      calls = calls + ((x, y) -> cnt)
      println(calls)

      Value.VInt(x.add(y))
    })

    evalTest(
      List("""
package Rec/Fib
external def addLog(x: Int, y: Int) -> Int

def fib(n):
  def loop(ins, outs):
    recur ins:
      []: outs
      [_, *rest]: match outs:
        [h1, h2, *_]:
          loop(rest, [h1.addLog(h2), *outs])
        []: loop(rest, [1])
        [h]: loop(rest, [2, h])
  loop(range(n), [])

out = fib(12)
"""),
      "Rec/Fib",
      Externals(
        Map((PackageName(NonEmptyList("Rec", List("Fib"))), "addLog") -> fn2)
      ),
      List({ case ((v, t), ev) =>
        assert(
          v.value == Value.VList(
            List(233, 144, 89, 55, 34, 21, 13, 8, 5, 3, 2, 1).map(Value.VInt(_))
          )
        )
        assert(calls.values.sum == 10)
      })
    )
  }
}
