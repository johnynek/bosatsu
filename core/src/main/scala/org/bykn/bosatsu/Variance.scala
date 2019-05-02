package org.bykn.bosatsu

import cats.kernel.BoundedSemilattice

sealed abstract class Variance {
  import Variance._

  def unary_-(): Variance =
    this match {
      case Contravariant => Covariant
      case Covariant => Contravariant
      case topOrBottom => topOrBottom
    }

  def *(that: Variance): Variance =
    (this, that) match {
      case (Phantom, _) => Phantom
      case (_, Phantom) => Phantom
      case (Invariant, _) => Invariant
      case (_, Invariant) => Invariant
      case (Covariant, r) => r
      case (Contravariant, Contravariant) => Covariant
      case (Contravariant, Covariant) => Contravariant
    }

  /**
   * Variance forms a lattice with Phantom at the bottom and Invariant at the top.
   */
  def +(that: Variance): Variance =
    (this, that) match {
      case (Phantom, r) => r
      case (r, Phantom) => r
      case (Invariant, _) => Invariant
      case (_, Invariant) => Invariant
      case (Covariant, Covariant) => Covariant
      case (Contravariant, Contravariant) => Contravariant
      case (Covariant, Contravariant) => Invariant
      case (Contravariant, Covariant) => Invariant
    }
}
object Variance {

  case object Phantom extends Variance
  case object Covariant extends Variance
  case object Contravariant extends Variance
  case object Invariant extends Variance

  def phantom: Variance = Phantom
  def co: Variance = Covariant
  def contra: Variance = Contravariant
  def in: Variance = Invariant

  implicit val varianceBoundedSemilattice: BoundedSemilattice[Variance] =
    new BoundedSemilattice[Variance] {
      override def empty = Phantom
      override def combine(a: Variance, b: Variance): Variance =
        a + b

      override def combineAllOption(as: TraversableOnce[Variance]): Option[Variance] =
        if (as.isEmpty) None
        else Some {
          val iter = as.toIterator
          var res = iter.next()
          // Invariant is the top, so we can stop when we see it.
          while(iter.hasNext && res != Invariant) {
            res = res + iter.next()
          }
          res
        }
    }

}
