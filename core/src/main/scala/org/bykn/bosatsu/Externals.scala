package org.bykn.bosatsu

import cats.Eval

import cats.implicits._

import Evaluation.Value

sealed abstract class FfiCall {
  def call(t: rankn.Type, pn: PackageName, dn: Identifier): Eval[Value]
}

object FfiCall {
  final case class Fn1(fn: Value => Value) extends FfiCall {
    import Value.FnValue

    private[this] def evalFn(pn: PackageName, dn: Identifier): Eval[FnValue] =
      Eval.now(FnValue(NormalExpression.ExternalVar(pn, dn), Nil) { e1 => Eval.defer(e1.map(fn)) })

    def call(t: rankn.Type, pn: PackageName, dn: Identifier): Eval[Value] = evalFn(pn, dn)
  }
  final case class Fn2(fn: (Value, Value) => Value) extends FfiCall {
    import Value.FnValue
    import NormalExpression.{App, ExternalVar, LambdaVar}

    private[this] def evalFn(pn: PackageName, dn: Identifier): Eval[FnValue] = {
      val ev =  ExternalVar(pn, dn)
      Eval.now(FnValue(ev, Nil) { e1 =>
        Eval.now(FnValue(App(ev, LambdaVar(0)), List(e1)) { e2 =>
          Eval.defer((e1, e2).mapN(fn(_, _)))
        })
      })
    }

    def call(t: rankn.Type, pn: PackageName, dn: Identifier): Eval[Value] = evalFn(pn, dn)
  }
  final case class Fn3(fn: (Value, Value, Value) => Value) extends FfiCall {
    import Value.FnValue
    import NormalExpression.{App, ExternalVar, LambdaVar}

    private[this] def evalFn(pn: PackageName, dn: Identifier): Eval[FnValue] = {
      val ev =  ExternalVar(pn, dn)
      Eval.now(FnValue(ev, Nil) { e1 =>
        Eval.now(FnValue(App(ev, LambdaVar(0)), List(e1)) { e2 =>
          Eval.now(FnValue(App(App(ev, LambdaVar(1)), LambdaVar(0)), List(e2, e1))  { e3 =>
            Eval.defer((e1, e2, e3).mapN(fn(_, _, _)))
          })
        })
      })
    }

    def call(t: rankn.Type, pn: PackageName, dn: Identifier): Eval[Value] = evalFn(pn, dn)
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
        case _ => classOf[Evaluation.Value] :: Nil
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
