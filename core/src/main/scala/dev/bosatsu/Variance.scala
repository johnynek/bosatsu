package dev.bosatsu

import cats.kernel.{BoundedSemilattice, Order}

sealed abstract class Variance derives CanEqual {
  import Variance._

  def unary_- : Variance =
    this match {
      case Contravariant => Covariant
      case Covariant     => Contravariant
      case topOrBottom   => topOrBottom
    }

  // if you have f[x] the variance of the result is the arg of f times variance of x
  def *(that: Variance): Variance =
    (this, that) match {
      case (Phantom, _)                   => Phantom
      case (_, Phantom)                   => Phantom
      case (Invariant, _)                 => Invariant
      case (_, Invariant)                 => Invariant
      case (Covariant, r)                 => r
      case (Contravariant, Contravariant) => Covariant
      case (Contravariant, Covariant)     => Contravariant
    }

  /** Variance forms a lattice with Phantom at the bottom and Invariant at the
    * top.
    */
  def +(that: Variance): Variance =
    (this, that) match {
      case (Phantom, r)                   => r
      case (r, Phantom)                   => r
      case (Invariant, _)                 => Invariant
      case (_, Invariant)                 => Invariant
      case (Covariant, Covariant)         => Covariant
      case (Contravariant, Contravariant) => Contravariant
      case (Covariant, Contravariant)     => Invariant
      case (Contravariant, Covariant)     => Invariant
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

  val all: List[Variance] =
    Phantom :: Covariant :: Contravariant :: Invariant :: Nil

  implicit val varianceBoundedSemilattice: BoundedSemilattice[Variance] =
    new BoundedSemilattice[Variance] {
      override def empty = phantom
      override def combine(a: Variance, b: Variance): Variance =
        a + b
    }

  implicit val orderVariance: Order[Variance] =
    new Order[Variance] {
      def compare(left: Variance, right: Variance): Int =
        left match {
          case Phantom   => if (right == Phantom) 0 else -1
          case Covariant =>
            right match {
              case Phantom                   => 1
              case Covariant                 => 0
              case Contravariant | Invariant => -1
            }
          case Contravariant =>
            right match {
              case Phantom | Covariant => 1
              case Contravariant       => 0
              case Invariant           => -1
            }
          case Invariant => if (right == Invariant) 0 else 1
        }
    }
}
