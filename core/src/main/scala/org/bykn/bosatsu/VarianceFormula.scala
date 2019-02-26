package org.bykn.bosatsu

import cats.{Applicative, Traverse}
import cats.data.State
import org.bykn.bosatsu.rankn.{DefinedType, Type, TypeEnv}

import cats.implicits._

sealed abstract class VarianceFormula {
  import VarianceFormula._
  import Variance._

  def *(that: VarianceFormula): VarianceFormula =
    (this, that) match {
      case (Known(a), Known(b)) => Known(a * b)
      case (p@Known(Phantom), _) => p
      case (_, p@Known(Phantom)) => p
      // at least one unknown, and they could be phantom:
      case (a, b) => Times(a, b)
    }

  def +(that: VarianceFormula): VarianceFormula =
    (this, that) match {
      case (Known(Phantom), r) => r
      case (l, Known(Phantom)) => l
      case (i@Known(Invariant), _) => i
      case (_, i@Known(Invariant)) => i
      case (Known(a), Known(b)) => Known(a + b)
      // at least one unknown, and they could be invariant:
      case (a, b) => Plus(a, b)
    }

  def isKnown: Boolean =
    this match {
      case Known(_) => true
      case _ => false
    }
}

object VarianceFormula {
  import Variance._

  sealed abstract class Identifier {
    def toLong: Long
  }

  object Identifier {
    private case class IdImpl(toLong: Long) extends Identifier
    def nextIdent: State[SolutionState, Identifier] =
      State { ss: SolutionState =>
        require(ss.next.toLong < Long.MaxValue, "we shouldn't run long enough to overflow Long")
        (ss.copy(next = IdImpl(ss.next.toLong + 1L)), ss.next)
      }

    val init: Identifier = IdImpl(0L)
  }

  case class SolutionState(
    next: Identifier,
    constraints: List[(VarianceFormula, VarianceFormula)],
    solutions: Map[Unknown, Variance],
    unknowns: Set[Unknown]) {


    def isSolved: Boolean = unknowns.isEmpty

    // add constraints and update any knowns
    def constrain(left: VarianceFormula, right: VarianceFormula): SolutionState = ???
  }

  object SolutionState {
    val empty: SolutionState = SolutionState(Identifier.init, Nil, Map.empty, Set.empty)
  }

  case class Known(toVariance: Variance) extends VarianceFormula
  case class Unknown(ident: Identifier) extends VarianceFormula
  case class Times(left: VarianceFormula, right: VarianceFormula) extends VarianceFormula {
    require(!(left.isKnown && right.isKnown), s"both $left and $right cannot be known")
  }
  case class Plus(left: VarianceFormula, right: VarianceFormula) extends VarianceFormula {
    require(!(left.isKnown && right.isKnown), s"both $left and $right cannot be known")
  }

  implicit class VarianceExtensions(val variance: Variance) extends AnyVal {
    def toF: Known = Known(variance)
  }

  val newUnknown: State[SolutionState, Unknown] =
    Identifier.nextIdent.flatMap { id =>
      val u = Unknown(id)
      State { case s@SolutionState(_, _, _, unknowns) => (s.copy(unknowns = unknowns + u), u) }
    }

  def constrain(left: VarianceFormula, right: VarianceFormula): State[SolutionState, Unit] =
    State { ss: SolutionState => (ss.constrain(left, right), ()) }

  def solve(imports: TypeEnv[Variance], current: TypeEnv[Unit]): Either[List[DefinedType[Unit]], TypeEnv[Variance]] = {
    implicit val travTE: Traverse[TypeEnv] = ???

    val initImport = imports.map(Known(_))

    def constrain(dt: DefinedType[Unknown]): State[SolutionState, Unit] = ???

    def finish(te: TypeEnv[Unknown], ss: SolutionState): Either[List[DefinedType[Unit]], TypeEnv[Variance]] = ???

    val state = for {
      initCurrent <- current.traverse(_ => newUnknown)
      dts = initCurrent.definedTypes.toList.sortBy(_._1).map(_._2)
      _ <- dts.traverse_(constrain)
      ss <- State.get[SolutionState]
    } yield finish(initCurrent, ss)

    state.run(SolutionState.empty).value._2
  }

  def varianceOf[F[_]: Applicative](
    v: Type.Var.Bound,
    in: Type)(
    forDef: Type.Const.Defined => F[Option[Stream[VarianceFormula]]]): F[Option[VarianceFormula]] = {

    val F = Applicative[F]
    /*
     * Fn => Contravariant, Covariant
     * Id => Covariant
     * Int => Phantom or None
     * struct Foo(a: a, lst: List[a]), Foo => Covariant
     * forall a. Foo[a] => Phantom or None
     */
    def tpeVar(in: Type): F[Option[Stream[VarianceFormula]]] =
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
          F.pure(Some(Stream.continually(Invariant.toF)))
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
      case Type.TyConst(_) => F.pure(Some(Phantom.toF))
      case Type.ForAll(bound, inner) =>
        if (bound.toList.contains(v)) F.pure(Some(Phantom.toF))
        else varianceOf(v, inner)(forDef)
      case Type.TyVar(thisVar) =>
        if (thisVar == v) F.pure(Some(Covariant.toF))
        else F.pure(Some(Phantom.toF))
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
          case (_, kp@Known(Phantom)) =>
            Some(kp)
          case (hv #:: _, argv) =>
            Some(hv * argv)
        }
        .map(_.flatten)

        val leftSide = varianceOf(v, tc)(forDef)

        FOpt.map2(leftSide, rightSide)(_ + _)
    }
  }
}
