package org.bykn.bosatsu.rankn

/** How is a non-external data type represented
  */

sealed abstract class DataRepr

object DataRepr {
  sealed abstract class Nat(val isZero: Boolean) extends DataRepr

  case object ZeroNat extends Nat(true)
  case object SuccNat extends Nat(false)

  case class Enum(variant: Int, arity: Int, familyArities: List[Int])
      extends DataRepr
  // a struct with arity 1 can be elided, and is called a new-type
  case class Struct(arity: Int) extends DataRepr {
    require(arity != 1)
  }
  case object NewType extends DataRepr

  // todo: optimize cases where all enum variants have arity 0
}

sealed abstract class DataFamily

object DataFamily {
  case object Nat extends DataFamily
  case object Enum extends DataFamily
  case object Struct extends DataFamily
  case object NewType extends DataFamily
}
