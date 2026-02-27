package dev.bosatsu.set

sealed abstract class Rel derives CanEqual { lhs =>

  import Rel.{Sub, Super, Same, Intersects, Disjoint}

  def isStrictSubtype: Boolean =
    this == Sub

  def isSubtype: Boolean =
    this match {
      case Sub | Same => true
      case _          => false
    }

  def isStrictSupertype: Boolean =
    this == Super

  def isSupertype: Boolean =
    this match {
      case Super | Same => true
      case _            => false
    }

  def invert: Rel =
    this match {
      case Sub   => Super
      case Super => Sub
      case _     => this
    }

  // implements transitivity of comparisons
  //
  // encodes element-wise combination, so if the relation for key
  // "foo" is Sub, and for key "bar" is Super, the correct result is
  // Intersects.
  //
  // e.g. (a < b) and (b < c) imply (a < c)
  def |+|(rhs: Rel): Rel =
    (lhs, rhs) match {
      case (x, y) if x == y              => x
      case (Disjoint, _) | (_, Disjoint) => Disjoint
      case (Same, _)                     => rhs
      case (_, Same)                     => lhs
      case _                             => Intersects
    }

  final inline def lazyCombine(inline rhs: => Rel): Rel =
    if (lhs == Disjoint) Disjoint
    else (lhs |+| rhs)
}

object Rel {
  sealed abstract class SuperOrSame extends Rel

  case object Same extends SuperOrSame
  case object Sub extends Rel
  case object Super extends SuperOrSame
  case object Intersects extends Rel
  case object Disjoint extends Rel
}
