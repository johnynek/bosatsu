package dev.bosatsu

import cats.data.NonEmptyList

sealed abstract class FfiCall(val arity: Int) {
  def call(t: rankn.Type): Value
}

object FfiCall {
  final case class Const(value: Value) extends FfiCall(0) {
    def call(t: rankn.Type): Value = value
  }

  final case class Fn1(fn: Value => Value) extends FfiCall(1) {
    import Value.FnValue

    private val evalFn: FnValue = FnValue { case NonEmptyList(a, _) =>
      fn(a)
    }

    def call(t: rankn.Type): Value = evalFn
  }
  final case class Fn2(fn: (Value, Value) => Value) extends FfiCall(2) {
    import Value.FnValue

    private val evalFn: FnValue =
      FnValue {
        case NonEmptyList(e1, e2 :: _) => fn(e1, e2)
        case badShape => sys.error(s"expected two arguments, found: $badShape")
      }

    def call(t: rankn.Type): Value = evalFn
  }
  final case class Fn3(fn: (Value, Value, Value) => Value) extends FfiCall(3) {
    import Value.FnValue

    private val evalFn: FnValue =
      FnValue {
        case NonEmptyList(e1, e2 :: e3 :: _) => fn(e1, e2, e3)
        case badShape                        =>
          sys.error(s"expected three arguments, found: $badShape")
      }

    def call(t: rankn.Type): Value = evalFn
  }

  final case class Fn4(fn: (Value, Value, Value, Value) => Value)
      extends FfiCall(4) {
    import Value.FnValue

    private val evalFn: FnValue =
      FnValue {
        case NonEmptyList(e1, e2 :: e3 :: e4 :: _) => fn(e1, e2, e3, e4)
        case badShape                              =>
          sys.error(s"expected four arguments, found: $badShape")
      }

    def call(t: rankn.Type): Value = evalFn
  }
}
