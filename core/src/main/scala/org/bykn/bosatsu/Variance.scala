package org.bykn.bosatsu

import cats.Applicative
import cats.kernel.BoundedSemilattice
import org.bykn.bosatsu.rankn.Type

import cats.implicits._

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
      case (Covariant, Covariant) => Covariant
      case (Contravariant, Contravariant) => Covariant
      case (Covariant, Contravariant) => Contravariant
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

  def varianceOf[F[_]: Applicative](
    v: Type.Var.Bound,
    in: Type)(
    forDef: Type.Const.Defined => F[Option[Stream[Variance]]]): F[Option[Variance]] = {

    val F = Applicative[F]
    /*
     * Fn => Contravariant, Covariant
     * Id => Covariant
     * Int => Phantom or None
     * struct Foo(a: a, lst: List[a]), Foo => Covariant
     * forall a. Foo[a] => Phantom or None
     */
    def tpeVar(in: Type): F[Option[Stream[Variance]]] =
      in match {
        case Type.TyConst(defined@Type.Const.Defined(_, _)) =>
          forDef(defined)
        case Type.ForAll(_, inner) =>
          // can we really ignore the bound variables here?
          // what about forall a, b -> a[b]
          tpeVar(inner)
        case Type.TyVar(_) =>
          // we have no idea of the arity of the variance here
          // but can just return an infinite streams of Invariants as a worst case
          F.pure(Some(Stream.continually(Invariant)))
        case Type.TyMeta(_) =>
          // who knows what this points to
          // we should have a better error here
          F.pure(None)
        case Type.TyApply(tc, _) =>
          F.map(tpeVar(tc))(_.flatMap {
            case e if e.isEmpty =>
              // We can't apply one more if the type arity is 0:
              None
            case nonEmpty =>
              Some(nonEmpty.tail)
          })
      }

    in match {
      case Type.TyConst(_) => F.pure(Some(Phantom))
      case Type.ForAll(bound, inner) =>
        if (bound.toList.contains(v)) F.pure(Some(Phantom))
        else varianceOf(v, inner)(forDef)
      case Type.TyVar(thisVar) =>
        if (thisVar == v) F.pure(Some(Covariant))
        else F.pure(Some(Phantom))
      case Type.TyMeta(_) =>
        // who knows what this points to
        // we should have a better error here
        F.pure(None)
      case Type.TyApply(tc, targ) =>
        val FOpt = Applicative[F].compose[Option]
        val rightSide = FOpt.map2(tpeVar(tc), varianceOf(v, targ)(forDef)) {
          case (Stream.Empty, _) =>
            // this is an error, since we have no more type parameters
            // we should have a better error here, or possibly just
            // allow it as a Phantom variance
            None
          case (_, Phantom) =>
            Some(Phantom)
          case (hv #:: _, argv) =>
            Some(hv * argv)
        }
        .map(_.flatten)

        val leftSide = varianceOf(v, tc)(forDef)

        FOpt.map2(leftSide, rightSide)(_ + _)
    }
  }
}
