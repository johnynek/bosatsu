package org.bykn.bosatsu

import rankn.Type
import scala.concurrent.Future
import scala.collection.concurrent.{Map => CMap}

object LetFreeEvaluation {
  sealed trait LetFreeValue {}
  case class ComputedValue(value: Value) extends LetFreeValue

  case class ExprFnValue(toExprFn: (LetFreeValue, Cache, ToLFV) => Value)
      extends Value.FnValue.Arg {
    val toFn: Value => Value = { v: Value =>
      toExprFn(ComputedValue(v), None, None)
    }
  }

  def attemptExprFn(
      v: Value
  ): Either[(LetFreeValue, Cache, ToLFV) => Value, Value => Value] = v match {
    case fv @ Value.FnValue(f) =>
      fv.arg match {
        case ExprFnValue(ef) => Left(ef)
        case _               => Right(f)
      }
    case other =>
      // $COVERAGE-OFF$this should be unreachable
      sys.error(s"invalid cast to Fn: $other")
    // $COVERAGE-ON$

  }

  type Cache = Option[CMap[String, (Future[Value], Type)]]
  type ToLFV = Option[LetFreeValue => Future[Value]]


  def exprFn(wrapper: (LetFreeEvaluation.LetFreeValue, rankn.Type, LetFreeEvaluation.Cache, LetFreeEvaluation.ToLFV) => Any): FfiCall = {

    def evalExprFn(t: rankn.Type): ExprFnValue = ExprFnValue({ (e1, cache, eval) => Value.ExternalValue(wrapper(e1, t, cache, eval)) })

    FfiCall.FromFn { t => new Value.FnValue(evalExprFn(t)) }
  }
}
