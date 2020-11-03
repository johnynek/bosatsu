package org.bykn.bosatsu

import java.math.BigInteger
import org.scalatest.funsuite.AnyFunSuite
import cats.data.NonEmptyList
import scala.math.sqrt
import scala.annotation.tailrec
import scala.collection.mutable
import LetFreeEvaluation.LazyValue
import LetFreeExpression.Lambda
import Value.ExternalValue
import org.bykn.bosatsu.LetFreeEvaluation.LetFreeValue
import org.bykn.bosatsu.LetFreeEvaluation.ComputedValue

class ExprFnTest extends AnyFunSuite {
  import TestUtils._

  def sumValueToList(sv: Value): List[Value] = {
    @tailrec
    def loop(v: Value, acc: List[Value]): List[Value] =
      v match {
        case Value.VList.VNil          => acc
        case Value.VList.Cons(h, tail) => loop(tail, h :: acc)
      }
    loop(sv, Nil).reverse
  }

  def listToSumValue(lst: List[Value]): Value = {
    lst.foldRight(Value.VList.VNil) { (head, acc) =>
      Value.VList.Cons(head, acc)
    }
  }

  def externalFilter(counter: mutable.Map[Boolean, Int]) = {
    def boringFilter(lst: LetFreeValue, fn: Value => Value) = {
      val actualList = sumValueToList(lst.toValue).filter { x =>
        val res = fn(x).asSum == Value.True
        val cnt = counter.get(res).getOrElse(0)
        counter += (res -> (cnt + 1))
        res
      }
      listToSumValue(actualList)
    }
    LetFreeEvaluation.exprFn(
      2,
      {
        case (t, List(lst, fnValue @ LazyValue(_, _))) => {
          val applyable = fnValue.toLeaf
          applyable match {
            case LetFreeEvaluation.Leaf.Lambda(Lambda(expr), scope, _, _)
                if !expr.varSet.contains(0) =>
              LetFreeEvaluation.evalToValue(
                expr,
                LetFreeEvaluation.ComputedValue(Value.UnitValue) :: scope
              )(fnValue.extEnv, fnValue.cache) match {
                case Value.True  => lst.toValue
                case Value.False => Value.VList.VNil
                case _           => sys.error("this should always be a boolean")
              }
            case LetFreeEvaluation.Leaf.Lambda(lambda, scope, _, _) => {
              val fn = fnValue.toValue.asFn
              boringFilter(lst, fn)
            }
            case LetFreeEvaluation.Leaf.Value(ComputedValue(v)) => {
              val fn = v.asFn
              boringFilter(lst, fn)
            }
          }
        }
        case (t, List(lst, fnValue)) => {
          val fn = fnValue.toValue.asFn
          boringFilter(lst, fn)
        }
      }
    )
  }

  test("Nat Data struct args") {
    letFreeEvaluateTest(
      List(
        """
package Nat/Struct

enum Count:
  Inc(x: Count), Zero

four = Inc(Inc(Inc(Inc(Zero))))

def toInt(count):
  recur count:
    Zero: 0
    Inc(rest): toInt(rest).add(1)

result = toInt(four)      
      """
      ),
      "Nat/Struct",
      Externals(Map.empty),
      List(v =>
        assert(
          v.toValue.asExternal.toAny == BigInteger.valueOf(4),
          "should just be a number"
        )
      )
    )
  }
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
          v.toValue.asExternal.toAny == BigInteger.valueOf(10),
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
                BigInteger.valueOf(
                  sqrt(
                    v.asExternal.toAny
                      .asInstanceOf[BigInteger]
                      .longValue()
                      .toDouble
                  ).toLong
                )
              )
          )
        )
      ),
      List(v =>
        assert(
          v.toValue.asExternal.toAny == BigInteger.valueOf(5),
          "should just be a number"
        )
      )
    )
  }

  test("expression filter function that doesn't do row checks: False") {
    val counter = mutable.Map[Boolean, Int]()
    letFreeEvaluateTest(
      List("""
package Ext/ExprListFilter

external def expr_list_filter(lst: List[a], fn: a -> Bool) -> List[a]

out = [1,2,3,4].expr_list_filter(\_ -> False)
"""),
      "Ext/ExprListFilter",
      Externals(
        Map(
          (
            PackageName(NonEmptyList.of("Ext", "ExprListFilter")),
            "expr_list_filter"
          ) -> externalFilter(counter)
        )
      ),
      List(
        v =>
          assert(
            v.toValue.asSum == Value.VList.VNil,
            "should be empty list"
          ),
        v =>
          assert(
            counter.toSet == Set.empty,
            "no counts"
          )
      )
    )
  }

  test("expression filter function that doesn't do row checks: True") {
    val counter = mutable.Map[Boolean, Int]()
    letFreeEvaluateTest(
      List("""
package Ext/ExprListFilter

external def expr_list_filter(lst: List[a], fn: a -> Bool) -> List[a]

out = [1,2,3,4].expr_list_filter(\_ -> True)
"""),
      "Ext/ExprListFilter",
      Externals(
        Map(
          (
            PackageName(NonEmptyList.of("Ext", "ExprListFilter")),
            "expr_list_filter"
          ) -> externalFilter(counter)
        )
      ),
      List(
        v =>
          assert(
            v.toValue.asSum == listToSumValue(
              List(1L, 2L, 3L, 4L).map(k =>
                ExternalValue(BigInteger.valueOf(k))
              )
            ),
            "should just be a number"
          ),
        v =>
          assert(
            counter.toSet == Set.empty,
            "no counts"
          )
      )
    )
  }

  test(
    "expression filter function that doesn't do row checks even though it happens in an int_loop"
  ) {
    val counter = mutable.Map[Boolean, Int]()
    letFreeEvaluateTest(
      List("""
package Ext/ExprListIntLoopFilter

external def expr_list_filter(lst: List[a], fn: a -> Bool) -> List[a]

out = int_loop(2, [1,2,3,4], \i, lst -> (i, lst.expr_list_filter(\_ -> True)))
"""),
      "Ext/ExprListIntLoopFilter",
      Externals(
        Map(
          (
            PackageName(NonEmptyList.of("Ext", "ExprListIntLoopFilter")),
            "expr_list_filter"
          ) -> externalFilter(counter)
        )
      ),
      List(
        v =>
          assert(
            v.toValue.asSum == listToSumValue(
              List(1L, 2L, 3L, 4L).map(k =>
                ExternalValue(BigInteger.valueOf(k))
              )
            ),
            "should just be a number"
          ),
        v =>
          assert(
            counter.toSet == Set.empty,
            "no counts"
          )
      )
    )
  }

  test(
    "expression filter function that wouldn't do row checks except inital calling is happening in an external"
  ) {
    val counter = mutable.Map[Boolean, Int]()
    letFreeEvaluateTest(
      List("""
package Ext/ExprListExtCaller

external def expr_list_filter(lst: List[a], fn: a -> Bool) -> List[a]
external def external_caller(fn: a -> b, x: a)-> b

out = external_caller(expr_list_filter([1, 2, 3, 4]), \_ -> True)
"""),
      "Ext/ExprListExtCaller",
      Externals(
        Map(
          (
            PackageName(NonEmptyList.of("Ext", "ExprListExtCaller")),
            "expr_list_filter"
          ) -> externalFilter(counter),
          (
            PackageName(NonEmptyList.of("Ext", "ExprListExtCaller")),
            "external_caller"
          ) -> FfiCall.Fn2((fnV, v) => fnV.asFn.apply(v))
        )
      ),
      List(
        v =>
          assert(
            v.toValue.asSum == listToSumValue(
              List(1L, 2L, 3L, 4L).map(k =>
                ExternalValue(BigInteger.valueOf(k))
              )
            ),
            "should just be a number"
          ),
        v =>
          assert(
            counter.toSet == Set(true -> 4),
            "all rows true"
          )
      )
    )
  }

  test("expression filter function that does do row checks") {
    val counter = mutable.Map[Boolean, Int]()
    letFreeEvaluateTest(
      List("""
package Ext/ExprListFilter

operator < = \a, b -> a.cmp_Int(b) matches LT

external def expr_list_filter(lst: List[a], fn: a -> Bool) -> List[a]

out = [1,2,3,4].expr_list_filter(\x -> x < 3)
"""),
      "Ext/ExprListFilter",
      Externals(
        Map(
          (
            PackageName(NonEmptyList.of("Ext", "ExprListFilter")),
            "expr_list_filter"
          ) -> externalFilter(counter)
        )
      ),
      List(
        v =>
          assert(
            v.toValue.asSum == listToSumValue(
              List(1L, 2L).map(k => ExternalValue(BigInteger.valueOf(k)))
            ),
            "should just be a number"
          ),
        v =>
          assert(
            counter.toSet == Set(true -> 2, false -> 2),
            "no counts"
          )
      )
    )
  }

  test("expression filter function that does do row checks and reverses") {
    val counter = mutable.Map[Boolean, Int]()
    letFreeEvaluateTest(
      List("""
package Ext/ExprListFilter

operator < = \a, b -> a.cmp_Int(b) matches LT

external def expr_list_filter(lst: List[a], fn: a -> Bool) -> List[a]

out = [1,2,3,4].expr_list_filter(\x -> x < 3).reverse
"""),
      "Ext/ExprListFilter",
      Externals(
        Map(
          (
            PackageName(NonEmptyList.of("Ext", "ExprListFilter")),
            "expr_list_filter"
          ) -> externalFilter(counter)
        )
      ),
      List(
        v =>
          assert(
            v.toValue.asSum == listToSumValue(
              List(2L, 1L).map(k => ExternalValue(BigInteger.valueOf(k)))
            ),
            "should just be a number"
          ),
        v =>
          assert(
            counter.toSet == Set(true -> 2, false -> 2),
            "no counts"
          )
      )
    )
  }
  test("expression filter function that's a bit more complicated'") {
    val counter = mutable.Map[Boolean, Int]()
    letFreeEvaluateTest(
      List("""
package Ext/ExprListFilter

operator < = \a, b -> a.cmp_Int(b) matches LT

operator || = \a, b -> match (a,b):
  (True, _): True
  (False, result): result

external def expr_list_filter(lst: List[a], fn: a -> Bool) -> List[a]

list_of_filters = [\x -> True || (x < 2), \x -> False || (x < 2)]

out = list_of_filters.flat_map_List(\fn -> [1,2,3,4].expr_list_filter(fn))
"""),
      "Ext/ExprListFilter",
      Externals(
        Map(
          (
            PackageName(NonEmptyList.of("Ext", "ExprListFilter")),
            "expr_list_filter"
          ) -> externalFilter(counter)
        )
      ),
      List(
        v =>
          assert(
            v.toValue.asSum == listToSumValue(
              List(1L, 2L, 3L, 4L, 1L).map(k =>
                ExternalValue(BigInteger.valueOf(k))
              )
            ),
            "should just be a number"
          ),
        v =>
          assert(
            counter.toSet == Set(true -> 1, false -> 3),
            "no counts"
          )
      )
    )
  }
}
