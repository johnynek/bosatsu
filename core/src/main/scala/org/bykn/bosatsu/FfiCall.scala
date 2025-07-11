package org.bykn.bosatsu

import cats.data.NonEmptyList

sealed abstract class FfiCall(val arity: Int) {
  def call(t: rankn.Type): Value
}

object FfiCall {
  final case class Fn1(fn: Value => Value) extends FfiCall(1) {
    import Value.FnValue

    private[this] val evalFn: FnValue = FnValue { case NonEmptyList(a, _) =>
      fn(a)
    }

    def call(t: rankn.Type): Value = evalFn
  }
  final case class Fn2(fn: (Value, Value) => Value) extends FfiCall(2) {
    import Value.FnValue

    private[this] val evalFn: FnValue =
      FnValue {
        case NonEmptyList(e1, e2 :: _) => fn(e1, e2)
        case badShape => sys.error(s"expected two arguments, found: $badShape")
      }

    def call(t: rankn.Type): Value = evalFn
  }
  final case class Fn3(fn: (Value, Value, Value) => Value) extends FfiCall(3) {
    import Value.FnValue

    private[this] val evalFn: FnValue =
      FnValue {
        case NonEmptyList(e1, e2 :: e3 :: _) => fn(e1, e2, e3)
        case badShape                        =>
          sys.error(s"expected three arguments, found: $badShape")
      }

    def call(t: rankn.Type): Value = evalFn
  }

  def getJavaType(t: rankn.Type): List[Class[_]] = {
    def one(t: rankn.Type): Option[Class[_]] =
      loop(t, false) match {
        case c :: Nil => Some(c)
        case _        => None
      }

    def loop(t: rankn.Type, top: Boolean): List[Class[_]] =
      t match {
        case rankn.Type.Fun(as, b) if top =>
          val ats = as.map { a =>
            one(a) match {
              case Some(at) => at
              case function =>
                sys.error(s"unsupported function type $function in $t")
            }
          }
          val res =
            one(b) match {
              case Some(at) => at
              case function =>
                sys.error(s"unsupported function type $function in $t")
            }
          ats.toList ::: res :: Nil
        case rankn.Type.ForAll(_, t) =>
          loop(t, top)
        case _ => classOf[Value] :: Nil
      }
    loop(t, true)
  }
}
