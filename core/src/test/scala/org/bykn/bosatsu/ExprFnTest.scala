package org.bykn.bosatsu

import java.math.BigInteger
import org.scalatest.funsuite.AnyFunSuite
import cats.data.NonEmptyList
import scala.math.sqrt
import scala.annotation.tailrec
import scala.collection.mutable
import LetFreeEvaluation.LazyValue
import LetFreeExpression.Lambda

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

  def externalFilter(counter: mutable.Map[Boolean, Int]) =
    LetFreeEvaluation.exprFn(
      2,
      {
        case (t, List(lst, fnValue @ LazyValue(Lambda(expr), scope, _)))
            if !expr.varSet.contains(0) => {
          LetFreeEvaluation.evalToValue(
            expr,
            LetFreeEvaluation.ComputedValue(Value.UnitValue) :: scope
          )(fnValue.extEnv, fnValue.cache) match {
            case Value.True  => lst.toValue
            case Value.False => Value.VList.VNil
            case _           => sys.error("this should always be a boolean")
          }
        }
        case (t, List(lst, fnValue)) => {
          val actualList = sumValueToList(lst.toValue).filter { x =>
            val res = fnValue.toValue.asFn(x).asSum == Value.True
            val cnt = counter.get(res).getOrElse(0)
            counter += (res -> (cnt + 1))
            res
          }
          listToSumValue(actualList)

        }
      }
    )

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
          v.asExternal.toAny == BigInteger.valueOf(10),
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
          v.asExternal.toAny == BigInteger.valueOf(5),
          "should just be a number"
        )
      )
    )
  }

  test("test of expression function externals") {
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
            v.asSum == Value.VList.VNil,
            "should just be a number"
          ),
        v =>
          assert(
            counter.toList == Nil,
            "no counts"
          )
      )
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
          v.asExternal.toAny == BigInteger.valueOf(4),
          "should just be a number"
        )
      )
    )
  }
}
