package org.bykn.bosatsu

sealed abstract class FfiCall {
  def call(t: rankn.Type): Value
}

object FfiCall {
  final case class Fn1(fn: Value => Value) extends FfiCall {
    import Value.FnValue

    private[this] val evalFn: FnValue = FnValue(fn)

    def call(t: rankn.Type): Value = evalFn
  }
  final case class Fn2(fn: (Value, Value) => Value) extends FfiCall {
    import Value.FnValue

    private[this] val evalFn: FnValue =
      FnValue { e1 =>
        FnValue { e2 =>
          fn(e1, e2)
        }
      }

    def call(t: rankn.Type): Value = evalFn
  }
  final case class Fn3(fn: (Value, Value, Value) => Value) extends FfiCall {
    import Value.FnValue

    private[this] val evalFn: FnValue =
      FnValue { e1 =>
        FnValue { e2 =>
          FnValue { e3 =>
            fn(e1, e2, e3)
          }
        }
      }

    def call(t: rankn.Type): Value = evalFn
  }

  final case class ExprFn(wrapper: (LetFreeEvaluation.LetFreeValue, rankn.Type, LetFreeEvaluation.Cache, LetFreeEvaluation.ToLFV) => Any) extends FfiCall {
    import LetFreeEvaluation.ExprFnValue

    private[this] def evalExprFn(t: rankn.Type): ExprFnValue = ExprFnValue({ (e1, cache, eval) => Value.ExternalValue(wrapper(e1, t, cache, eval)) })

    def call(t: rankn.Type): Value = evalExprFn(t)
  }

  def getJavaType(t: rankn.Type): List[Class[_]] = {
    def loop(t: rankn.Type, top: Boolean): List[Class[_]] = {
      t match {
        case rankn.Type.Fun(a, b) if top =>
          loop(a, false) match {
            case at :: Nil => at :: loop(b, top)
            case function => sys.error(s"unsupported function type $function in $t")
          }
        case rankn.Type.ForAll(_, t) =>
          loop(t, top)
        case _ => classOf[Value] :: Nil
      }
    }
    loop(t, true)
  }
}

case class Externals(toMap: Map[(PackageName, String), FfiCall]) {
  def add(pn: PackageName, value: String, f: FfiCall): Externals =
    Externals(toMap + ((pn, value) -> f))

  def ++(that: Externals): Externals =
    Externals(toMap ++ that.toMap)
}

object Externals {
  def empty: Externals = Externals(Map.empty)
}
