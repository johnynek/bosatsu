package org.bykn.bosatsu

import cats.Eval

import cats.implicits._

import Evaluation.Value

sealed abstract class FfiCall[+T] {
  def call(t: rankn.Type, pn: PackageName, dn: Identifier): Eval[Value[T]]
}

object FfiCall {
  final case class Fn1[T](fn: Value[T] => Value[T]) extends FfiCall {
    import Value.FnValue

    private[this] def evalFn(pn: PackageName, dn: Identifier)(
      implicit externalFnTag: (PackageName, Identifier) => Int => T
    ): Eval[FnValue[T]] =
      Eval.now(FnValue((e1 => Eval.defer(e1.map(fn))), externalFnTag(pn, dn)(0)))
// (NormalExpression.ExternalVar(pn, dn), Nil)
    def call(t: rankn.Type, pn: PackageName, dn: Identifier)(
      implicit externalFnTag: (PackageName, Identifier) => Int => T
    ): Eval[Value[T]] = evalFn(pn, dn)
  }
  final case class Fn2[T](fn: (Value[T], Value[T]) => Value[T]) extends FfiCall {
    import Value.FnValue

    private[this] def evalFn(pn: PackageName, dn: Identifier)(
      implicit externalFnTag: (PackageName, Identifier) => Int => T
    ): Eval[FnValue[T]] = {
      val ev =  externalFnTag(pn, dn)
      Eval.now(FnValue(ev(0)) { e1 =>
        Eval.now(FnValue(ev(1)) { e2 =>
          Eval.defer((e1, e2).mapN(fn(_, _)))
        })
      })
    }

    def call(t: rankn.Type, pn: PackageName, dn: Identifier)(
      implicit externalFnTag: (PackageName, Identifier) => Int => T
    ): Eval[Value[T]] = evalFn(pn, dn)
  }
  final case class Fn3[T](fn: (Value[T], Value[T], Value[T]) => Value[T]) extends FfiCall {
    import Value.FnValue

    private[this] def evalFn(pn: PackageName, dn: Identifier)(
      implicit externalFnTag: (PackageName, Identifier) => Int => T
    ): Eval[FnValue[T]] = {
      val ev = externalFnTag(pn, dn)
      Eval.now(FnValue(ev(0)) { e1 =>
        Eval.now(FnValue(ev(1)) { e2 =>
          Eval.now(FnValue(ev(2))  { e3 =>
            Eval.defer((e1, e2, e3).mapN(fn(_, _, _)))
          })
        })
      })
    }

    def call(t: rankn.Type, pn: PackageName, dn: Identifier)(
      implicit externalFnTag: (PackageName, Identifier) => Int => T
    ): Eval[Value[T]] = evalFn(pn, dn)
  }

  def getJavaType[T](t: rankn.Type): List[Class[_]] = {
    def loop(t: rankn.Type, top: Boolean): List[Class[_]] = {
      t match {
        case rankn.Type.Fun(a, b) if top =>
          loop(a, false) match {
            case at :: Nil => at :: loop(b, top)
            case function => sys.error(s"unsupported function type $function in $t")
          }
        case rankn.Type.ForAll(_, t) =>
          loop(t, top)
        case _ => classOf[Evaluation.Value[T]] :: Nil
      }
    }
    loop(t, true)
  }
}

case class Externals[+T](toMap: Map[(PackageName, String), FfiCall[T]]) {
  def add(pn: PackageName, value: String, f: FfiCall[T]): Externals[T] =
    Externals(toMap + ((pn, value) -> f))

  def ++(that: Externals[T]): Externals[T] =
    Externals(toMap ++ that.toMap)
}

object Externals {
  def empty: Externals[Nothing] = Externals(Map.empty)
}
