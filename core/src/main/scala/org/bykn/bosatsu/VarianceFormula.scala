package org.bykn.bosatsu

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
      // at least one unknown, and they could be phantom, which acts like a zero
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
    constraints: Map[Unknown, VarianceFormula],
    solutions: Map[Unknown, Variance],
    unknowns: Set[Unknown]) {

    /**
     * After we have processed all the constraints, then all the remaining unknowns must be Phantom
     */
    def isSolved: Boolean = constraints.isEmpty

    def isUnsolved(u: Unknown): Boolean = constraints.get(u).isDefined

    /**
     * constrain the left to equal the right
     */
    def constrain(left: Unknown, right: VarianceFormula): SolutionState = {
      val newRight = constraints.get(left) match {
        case Some(ex) => ex + right
        case None => right
      }
      copy(constraints = constraints.updated(left, right))
    }

    /**
     * remove any constraints we can solve for. This is the main meat
     * of the algorithm
     */
    def simplify: SolutionState = {
      @annotation.tailrec
      def loop(ss: SolutionState): SolutionState = {
        // look for anything solved:
        val solved = ss.constraints.iterator.collect { case (u, Known(v)) => (u, v) }.toMap
        if (solved.isEmpty) ss
        else {
          // we can substitute those known values in:
          val newConstraints = solved.foldLeft(ss.constraints) { case (cons, (u, v)) =>
            cons
              .iterator
              .collect {
                case (u1, f) if u1 != u => (u1, substitute(f, u, v))
              }
              .toMap
          }

          val nextSs = ss.copy(
            constraints = newConstraints,
            solutions = ss.solutions ++ solved,
            unknowns = ss.unknowns -- solved.keys)

          loop(nextSs)
        }
      }

      loop(this)
    }
  }

  object SolutionState {
    val empty: SolutionState = SolutionState(Identifier.init, Map.empty, Map.empty, Set.empty)
  }

  case class Known(toVariance: Variance) extends VarianceFormula
  case class Unknown(ident: Identifier) extends VarianceFormula
  case class Times(left: VarianceFormula, right: VarianceFormula) extends VarianceFormula {
    require(!(left.isKnown && right.isKnown), s"both $left and $right cannot be known")
  }
  case class Plus(left: VarianceFormula, right: VarianceFormula) extends VarianceFormula {
    require(!(left.isKnown && right.isKnown), s"both $left and $right cannot be known")
  }

  def substitute(in: VarianceFormula, uk: Unknown, v: Variance): VarianceFormula =
    in match {
      case k@Known(_) => k
      case u0@Unknown(_) => if (u0 == uk) Known(v) else u0
      case Times(left, right) => substitute(left, uk, v) * substitute(right, uk, v)
      case Plus(left, right) => substitute(left, uk, v) + substitute(right, uk, v)
    }

  implicit class VarianceExtensions(val variance: Variance) extends AnyVal {
    def toF: Known = Known(variance)
  }

  val newUnknown: State[SolutionState, Unknown] =
    Identifier.nextIdent.flatMap { id =>
      val u = Unknown(id)
      State { case s@SolutionState(_, _, _, unknowns) => (s.copy(unknowns = unknowns + u), u) }
    }

  def constrain(left: Unknown, right: VarianceFormula): State[SolutionState, Unit] =
    State { ss: SolutionState => (ss.constrain(left, right), ()) }

  def solve(imports: TypeEnv[Variance], current: TypeEnv[Unit]): Either[List[DefinedType[Unit]], TypeEnv[Variance]] = {
    val initImport = imports.map(Known(_))

    def constrain(unknowns: TypeEnv[Unknown], dt: DefinedType[Unknown]): State[SolutionState, Unit] = {
      import Type._

      val umap: Map[Var, Unknown] = dt.annotatedTypeParams.toMap

      def constructorVariance(tpe: Type): Stream[VarianceFormula] =
        tpe match {
          case FnType => Stream(Contravariant.toF, Covariant.toF)
          case TyConst(Const.Defined(p, t)) =>
            val fullName = (p, TypeName(t))
            // TODO need error handling if we don't know about this type
            val thisDt: DefinedType[VarianceFormula] =
              unknowns.definedTypes.get(fullName)
                .orElse(imports.definedTypes.get(fullName).map(_.map(Known(_))))
                .getOrElse(sys.error(s"unknown: $fullName in $dt"))

            thisDt.annotatedTypeParams.map(_._2).toStream
          case TyApply(_, _) => sys.error(s"invariant violation: never called on TyApply: $tpe")
          case TyVar(_) =>
            // TODO: we assume the worst case: this is invariant, is this safe?
            Stream.continually(Invariant.toF)
          case TyMeta(_) =>
            // TODO: error here:
            Stream.continually(Invariant.toF)
          case ForAll(_, _) => ???
        }

      // What are the constraints on bound with the given unknown, in the current dt
      def constrainTpe(bound: Type.Var.Bound, u: Unknown, tpe: Type): VarianceFormula = {

        val unitState = State.pure[SolutionState, Unit](())

        def loop(shadows: Set[Type.Var], tpe: Type): VarianceFormula =
          tpe match {
            case ForAll(vars, tpe) => loop(shadows ++ vars.toList, tpe)
            case TyConst(dt@Const.Defined(_, _)) =>
              // this is just a raw type
              Phantom.toF
            case TyVar(b) =>
              if (shadows(b)) Phantom.toF
              else if (b == bound) Covariant.toF
              else Phantom.toF
            case TyMeta(_) =>
              // TODO, need to error here
              Phantom.toF
            case app@TyApply(_, _) =>
              def unroll(a: Type): (Type, List[Type]) =
                a match {
                  case TyApply(on, arg) =>
                    val (left, args) = unroll(on)
                    (left, arg :: args)
                  case notApply => (notApply, Nil)
                }
              val (cons, revArgs) = unroll(app)
              val cv = constructorVariance(cons)
              // TODO: check that the variance matches:
              cv.iterator.zip(revArgs.reverse.iterator)
                .foldLeft(Phantom.toF: VarianceFormula) { case (acc, (c, v)) =>
                  acc + (c * loop(shadows, v))
                }
          }

        loop(Set.empty, tpe)
      }

      dt.annotatedTypeParams.traverse_ { case (b, u) =>
        dt.constructors.traverse_ { case (_, argType, _) =>
          argType.traverse_ { case (_, tpe) =>
            val formula = constrainTpe(b, u, tpe)
            State.modify[SolutionState](_.constrain(u, formula))
          }
        }
      }
    }

    // invariant, assume we have a solved ss
    def finish(te: TypeEnv[Unknown], ss: SolutionState): Either[List[DefinedType[Unit]], TypeEnv[Variance]] =
      if (ss.isSolved) Right {
        te.map(ss.solutions.getOrElse(_, Phantom))
      }
      else
        Left {
          te.definedTypes
            .iterator
            .filter { case (_, dt) => dt.annotatedTypeParams.exists { case (_, u) => ss.isUnsolved(u) } }
            .toList
            .sortBy(_._1)
            .map(_._2.as(()))
        }

    val state = for {
      initCurrent <- current.traverse(_ => newUnknown)
      dts = initCurrent.definedTypes.toList.sortBy(_._1).map(_._2)
      _ <- dts.traverse_(constrain(initCurrent, _))
      ss <- State.get[SolutionState]
      simplified = ss.simplify
    } yield finish(initCurrent, simplified)

    state.run(SolutionState.empty).value._2
  }
}
