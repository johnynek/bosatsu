package org.bykn.bosatsu

import cats.Traverse
import cats.data.{NonEmptyList, State}
import org.bykn.bosatsu.rankn.{DefinedType, Type, TypeEnv}
import scala.collection.immutable.SortedMap

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

  def substitute(uk: Unknown, v: Variance): VarianceFormula =
    this match {
      case k@Known(_) => k
      case u0@Unknown(_) => if (u0 == uk) Known(v) else u0
      case Times(left, right) => left.substitute(uk, v) * right.substitute(uk, v)
      case Plus(left, right) => left.substitute(uk, v) + right.substitute(uk, v)
    }
}

object VarianceFormula {
  import Variance._

  sealed abstract class Identifier {
    def toLong: Long
    override def toString = s"Identifier($toLong)"
  }

  private object Identifier {
    private case class IdImpl(toLong: Long) extends Identifier

    val nextIdent: State[SolutionState, Identifier] =
      State { ss: SolutionState =>
        require(ss.next.toLong < Long.MaxValue, "we shouldn't run long enough to overflow Long")
        (ss.copy(next = IdImpl(ss.next.toLong + 1L)), ss.next)
      }

    val init: Identifier = IdImpl(0L)
  }

  private case class SolutionState(
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
      copy(constraints = constraints.updated(left, newRight))
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
        if (solved.isEmpty) {
          if (!ss.isSolved) {
            val (solves, _, finish) =
              iteration(ss.constraints.mapValues(_ => Phantom), ss.constraints, ss.constraints.size + 100)

            if (finish) {
              copy(constraints = Map.empty,
                solutions = ss.solutions ++ solves,
                unknowns = unknowns -- solves.keys)
            }
            else ss
          }
          else {
            ss
          }
        }
        else {
          // we can substitute those known values in:
          val newConstraints = solved.foldLeft(ss.constraints) { case (cons, (u, v)) =>
            cons
              .iterator
              .collect {
                case (u1, f) if u1 != u => (u1, f.substitute(u, v))
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

      // do power iteration starting from Phantom to finish
      @annotation.tailrec
      def iteration(m: Map[Unknown, Variance],
        constraints: Map[Unknown, VarianceFormula], trials: Int): (Map[Unknown, Variance], Map[Unknown, VarianceFormula], Boolean) = {
        if (trials <= 0) (m, constraints, false)
        else {
          var diff: Boolean = false

          def evaluate(vf: VarianceFormula): Variance =
            vf match {
              case Known(v) => v
              case u@Unknown(_) => m.getOrElse(u, Phantom)
              case Times(left, right) => evaluate(left) * evaluate(right)
              case Plus(left, right) => evaluate(left) + evaluate(right)
            }


          val nextM: Map[Unknown, Variance] =
            m.iterator.map { case (u, v) =>
              val nextV = constraints.get(u) match {
                case None => Phantom
                case Some(vf) => evaluate(vf)
              }
              diff = diff || (nextV != v)
              (u, nextV)
            }.toMap

          if (diff) iteration(nextM, constraints, trials - 1)
          else (nextM, constraints, true)
        }
      }

      loop(this)
    }
  }

  private object SolutionState {
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


  implicit class VarianceExtensions(val variance: Variance) extends AnyVal {
    def toF: Known = Known(variance)
  }

  private val newUnknown: State[SolutionState, Unknown] =
    Identifier.nextIdent.flatMap { id =>
      val u = Unknown(id)
      State { case s@SolutionState(_, _, _, unknowns) => (s.copy(unknowns = unknowns + u), u) }
    }

  /**
   * Solve the variance of the given list of current items or return
   * a list of of those items we cannot infer the variance of.
   *
   * It may be the case that we can always infer the variance, we don't actually
   * have a proof on how often this fails (if ever).
   */
  def solve(imports: TypeEnv[Variance], current: List[DefinedType[Unit]]): Either[NonEmptyList[DefinedType[Unit]], List[DefinedType[Variance]]] = {
    val travListDT = Traverse[NonEmptyList].compose[DefinedType]

    def constrain(unknowns: SortedMap[(PackageName, TypeName), DefinedType[Unknown]], dt: DefinedType[Unknown]): State[SolutionState, Unit] = {
      import Type._

      /*
       * We assume all higher kinded type variables are invariant, which is a worst
       * case assumption, although also meeting with expectations for a generic type
       */
      val unit: State[SolutionState, Unit] = State.pure(())
      def constrainHKinParam(tpe: Type, umap: Map[Var, Unknown]): State[SolutionState, Unit] =
        tpe match {
          case TyApply(TyVar(v), right) =>
            val left = umap.get(v) match {
              case Some(u) =>
                State.modify[SolutionState](_.constrain(u, Invariant.toF))
              case None => unit
            }
            left *> constrainHKinParam(right, umap)
          case TyApply(left, right) =>
            constrainHKinParam(left, umap) *> constrainHKinParam(right, umap)
          case TyConst(_) => unit
          case TyVar(_) => unit // not in the apply position
          case TyMeta(_) => unit // probably should be an error
          case ForAll(bound, in) =>
            // remove these bound variables:
            constrainHKinParam(in, umap -- bound.toList)
        }

      val umap: Map[Var, Unknown] = dt.annotatedTypeParams.toMap
      val hks = dt.constructors.traverse_ { case (_, ps, _) =>
        ps.traverse_ { case (_, tpe) =>
          constrainHKinParam(tpe, umap)
        }
      }

      def constructorVariance(tpe: Type): Stream[VarianceFormula] =
        tpe match {
          case FnType => Stream(Contravariant.toF, Covariant.toF)
          case TyConst(Const.Defined(p, t)) =>
            val tn = TypeName(t)
            // TODO need error handling if we don't know about this type
            val thisDt: DefinedType[VarianceFormula] =
              unknowns.get((p, tn))
                .orElse(imports.getType(p, tn).map(_.map(Known(_))))
                .getOrElse(sys.error(s"unknown: $p, $tn in $dt"))

            thisDt.annotatedTypeParams.map(_._2).toStream
          case TyApply(tpe, _) =>
            constructorVariance(tpe).tail
          case TyVar(_) =>
            // TODO: we assume the worst case: this is invariant, is this safe?
            Stream.continually(Invariant.toF)
          case TyMeta(_) =>
            // TODO: error here:
            Stream.continually(Invariant.toF)
          case ForAll(_, tpe) => constructorVariance(tpe)
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

      hks *> dt.annotatedTypeParams.traverse_ { case (b, u) =>
        dt.constructors.traverse_ { case (_, argType, _) =>
          argType.traverse_ { case (_, tpe) =>
            val formula = constrainTpe(b, u, tpe)
            State.modify[SolutionState](_.constrain(u, formula))
          }
        }
      }
    }

    // invariant, assume we have a solved ss
    def finish(te: NonEmptyList[DefinedType[Unknown]], ss: SolutionState): Either[NonEmptyList[DefinedType[Unit]], List[DefinedType[Variance]]] = {
      def isUn(dt: DefinedType[Unknown]): Boolean =
        dt.annotatedTypeParams.exists { case (_, u) => ss.isUnsolved(u) }

      val unknowns = te.toList.filter(isUn)
      NonEmptyList.fromList(unknowns) match {
        case None =>
          /*
           * TODO: external defs have no constructors, we should assume their types
           * are invariant, but this may be setting them to phantom, which is unsound
           */
          Right(travListDT.map(te)(ss.solutions.getOrElse(_, Phantom)).toList)
        case Some(err) =>
          Left(err.sortBy { dt => (dt.packageName, dt.name) }.map(_.as(())))
      }
    }

    NonEmptyList.fromList(current) match {
      case None => Right(Nil)
      case Some(current) =>
        val state = for {
          dts <- travListDT.traverse(current)(_ => newUnknown)
          dtsMap = DefinedType.listToMap(dts.toList)
          _ <- dts.traverse_(constrain(dtsMap, _))
          ss <- State.get[SolutionState]
          simplified = ss.simplify
        } yield finish(dts, simplified)

        state.run(SolutionState.empty).value._2
    }
  }
}
