package org.bykn.bosatsu

import cats.Semigroup
import cats.data.{NonEmptyLazyList, NonEmptyList, EitherNec, ValidatedNec}
import org.bykn.bosatsu.rankn.{
  ConstructorFn,
  Ref,
  RefSpace,
  TypeEnv,
  DefinedType
}
import org.bykn.bosatsu.Shape.KnownShape
import scala.collection.immutable.{LongMap, SortedSet}

import cats.syntax.all._
import cats.data.Validated

sealed abstract class KindFormula

object KindFormula {
  case class Var(id: Long)

  case class Arg(variance: Var, kind: KindFormula)

  case object Type extends KindFormula
  case class Cons(arg: Arg, result: KindFormula) extends KindFormula

  sealed abstract class Error
  object Error {
    case class Unsatisfiable(
        constraints: LongMap[NonEmptyList[Constraint]],
        existing: LongMap[Variance],
        unknowns: SortedSet[Long]
    ) extends Error
  }

  sealed abstract class Sat
  object Sat {
    case object No extends Sat
    case object Yes extends Sat
    case object Maybe extends Sat

    def apply(bool: Boolean): Sat =
      if (bool) Yes else No
  }

  sealed abstract class Constraint {
    def depends: List[Var]
    def satisfied(known: LongMap[Variance], value: Variance): Sat
  }
  object Constraint {
    case class DeclaredParam(
        idx: Int,
        kindArg: Kind.Arg
    ) extends Constraint {
      def depends = Nil
      def satisfied(known: LongMap[Variance], value: Variance) =
        if (value == kindArg.variance) Sat.Yes
        else Sat.No
    }

    case class DeclaredType(
        cfn: ConstructorFn,
        constructorIdx: Int,
        forAll: rankn.Type.ForAll,
        bound: rankn.Type.Var.Bound,
        kindArg: Kind.Arg
    ) extends Constraint {
      def depends = Nil
      def satisfied(known: LongMap[Variance], value: Variance) =
        if (value == kindArg.variance) Sat.Yes
        else Sat.No
    }

    case class ImportedConst(
        cfn: ConstructorFn,
        constructorIdx: Int,
        const: rankn.Type.Const,
        kind: Kind,
        kindArg: Kind.Arg
    ) extends Constraint {
      def depends = Nil
      def satisfied(known: LongMap[Variance], value: Variance) =
        if (value == kindArg.variance) Sat.Yes
        else Sat.No
    }

    // arg idx of a given constructor function
    case class Accessor(cfn: ConstructorFn, idx: Int) extends Constraint {
      def depends = Nil
      def satisfied(known: LongMap[Variance], value: Variance) =
        if (value == Variance.co || value == Variance.in) Sat.Yes
        else Sat.No
    }

    case class RecursiveView(cfn: ConstructorFn, idx: Int) extends Constraint {
      def depends = Nil
      def satisfied(known: LongMap[Variance], value: Variance) =
        if (value == Variance.co) Sat.Yes
        else Sat.No
    }

    case class HasView(
        cfn: ConstructorFn,
        idx: Int,
        bound: rankn.Type.Var.Bound,
        view: Var
    ) extends Constraint {
      def depends = view :: Nil
      def satisfied(known: LongMap[Variance], value: Variance) =
        known.get(view.id) match {
          case Some(viewVariance) =>
            value match {
              case Variance.Invariant => Sat.Yes
              case Variance.Covariant =>
                Sat(
                  (viewVariance == Variance.co) || (viewVariance == Variance.phantom)
                )
              case Variance.Contravariant =>
                Sat(
                  (viewVariance == Variance.contra) || (viewVariance == Variance.phantom)
                )
              case Variance.Phantom =>
                Sat(viewVariance == Variance.phantom)
            }
          case None => Sat.Maybe
        }
    }

    case class UnifyVariance(
        cfn: ConstructorFn,
        idx: Int,
        inType: rankn.Type,
        variance: Variance
    ) extends Constraint {
      def depends = Nil
      def satisfied(known: LongMap[Variance], value: Variance) =
        Sat(value == variance)
    }

    case class UnifyVar(
        cfn: ConstructorFn,
        idx: Int,
        inType: rankn.Type,
        variance: Var
    ) extends Constraint {
      def depends = variance :: Nil
      def satisfied(known: LongMap[Variance], value: Variance) =
        known.get(variance.id) match {
          case Some(v) => Sat(v == value)
          case None    => Sat.Maybe
        }
    }

    // if constrainted by this v + variance == v
    case class VarSubsumes(
        cfn: ConstructorFn,
        idx: Int,
        inType: rankn.Type,
        variance: Var
    ) extends Constraint {
      def depends = variance :: Nil
      def satisfied(known: LongMap[Variance], value: Variance) =
        known.get(variance.id) match {
          case Some(v) => Sat((value + v) == value)
          case None    => Sat.Maybe
        }
    }

    case class IsProduct(
        view: Var,
        argVariance: Var,
        tapply: rankn.Type.TyApply
    ) extends Constraint {
      def depends = view :: argVariance :: Nil
      def satisfied(known: LongMap[Variance], value: Variance) =
        (known.get(view.id), known.get(argVariance.id)) match {
          case (Some(v1), Some(v2)) =>
            Sat((v1 * v2) == value)
          case _ => Sat.Maybe
        }
    }
  }

  def solveKind(
      imports: TypeEnv[Kind.Arg],
      dt: DefinedType[Either[KnownShape, Kind.Arg]]
  ): ValidatedNec[Error, DefinedType[Kind.Arg]] = {
    import Impl._

    (for {
      state <- Impl.newState(imports, dt)
      dtFormula <- dt.zipWithIndex.traverse {
        case (Left(ks), _) => state.shapeToArg(Direction.PhantomUp, ks)
        case (Right(ka), idx) =>
          state.kindArgToArg(Direction.PhantomUp, ka) { ka =>
            Constraint.DeclaredParam(idx, ka)
          }
      }
      kindMap = dtFormula.annotatedTypeParams.iterator.map { case (k, v) =>
        (k, Impl.BoundState.IsArg(v))
      }.toMap
      thisKind = dtFormula.annotatedTypeParams.foldRight(Type: KindFormula) {
        case ((_, a), t) => Cons(a, t)
      }
      _ <- dtFormula.constructors.traverse { cfn =>
        state.addConstraints(thisKind, cfn, kindMap)
      }
      constraints <- state.getConstraints
      dirs <- state.getDirections
      topo = Impl.combineTopo(constraints)
      maybeRes = Impl.go(constraints, dirs, topo).map { vars =>
        dtFormula.map(Impl.unformula(_, vars))
      }
    } yield maybeRes).run.value
  }

  private object Impl {
    type Cons = LongMap[NonEmptyList[Constraint]]

    sealed abstract class BoundState
    object BoundState {
      case class IsKind(
          kind: Kind,
          forAll: rankn.Type.ForAll,
          bound: rankn.Type.Var.Bound
      ) extends BoundState
      case class IsFormula(formula: KindFormula) extends BoundState
      case class IsArg(arg: Arg) extends BoundState
    }

    sealed abstract class Direction(val variances: List[Variance]) {
      def reverse: Direction
    }
    object Direction {
      case object PhantomUp
          extends Direction(
            Variance.phantom :: Variance.contra :: Variance.co :: Variance.in :: Nil
          ) {
        def reverse: Direction = InvariantDown
      }
      case object InvariantDown
          extends Direction(
            Variance.in :: Variance.co :: Variance.contra :: Variance.phantom :: Nil
          ) {
        def reverse: Direction = PhantomUp
      }
    }

    implicit def longMapSemi[V]: Semigroup[LongMap[V]] =
      new Semigroup[LongMap[V]] {
        def combine(x: LongMap[V], y: LongMap[V]): LongMap[V] =
          y.foldLeft(x) { case (x, (k, v)) => x.updated(k, v) }
      }
    // dagify the constraint graph, and then topologically sort it
    def combineTopo(cons: Cons): List[NonEmptyList[SortedSet[Long]]] = ???

    // Report Errors, or enumerate possibilities for subgraph
    // invariant: all subgraph values must be valid keys in the result
    // we can process the subgraph list in parallel. Those are all the indepentent next values
    def allSolutionChunk(
        cons: Cons,
        existing: LongMap[Variance],
        directions: LongMap[Direction],
        subgraph: SortedSet[Long]
    ): ValidatedNec[Error, NonEmptyLazyList[LongMap[Variance]]] =
      if (subgraph.isEmpty) Validated.valid(NonEmptyLazyList(existing))
      else {

        // go through in the correct direction, removing any
        // already invalid cases
        def varsFor(l: Long): LazyList[Variance] =
          directions(l).variances.to(LazyList).filter { v =>
            cons(l).forall { c =>
              c.satisfied(existing, v) != Sat.No
            }
          }

        // all possible choices enumerated in the correct directions
        // excluding the existing variances
        def allVariances(lst: List[Long]): LazyList[LongMap[Variance]] =
          lst match {
            case Nil => LongMap.empty[Variance] #:: LazyList.empty
            case h :: tail =>
              val hvs = varsFor(h)
              val tails = allVariances(tail)
              // Make sure not to re-evaluate the above
              (hvs, tails).mapN { (v, tailMap) => tailMap.updated(h, v) }
          }

        // does this satisfy all constraints for the subgraph
        def isValid(lm: LongMap[Variance]): Boolean =
          subgraph.forall { id =>
            val value = lm(id)
            cons(id).forall { c =>
              c.satisfied(existing, value) == Sat.Yes
            }
          }

        val validVariances: LazyList[LongMap[Variance]] =
          allVariances(subgraph.toList).map(existing ++ _).filter(isValid(_))

        NonEmptyLazyList.fromLazyList(validVariances) match {
          case Some(nel) => Validated.valid(nel)
          case None =>
            Validated.invalidNec(Error.Unsatisfiable(cons, existing, subgraph))
        }
      }

    def allSolutions(
        cons: Cons,
        existing: LongMap[Variance],
        directions: LongMap[Direction],
        subgraph: NonEmptyList[SortedSet[Long]]
    ): EitherNec[Error, NonEmptyLazyList[LongMap[Variance]]] =
      subgraph
        .traverse(allSolutionChunk(cons, existing, directions, _))
        .map(KindFormula.mergeCrossProduct(_))
        .toEither

    def go(
        cons: Cons,
        directions: LongMap[Direction],
        topo: List[NonEmptyList[SortedSet[Long]]]
    ): ValidatedNec[Error, LongMap[Variance]] =
      topo.foldM(NonEmptyLazyList(LongMap.empty[Variance])) {
        (sols, subgraph) =>
          // if there is at least one good solution, we can keep going
          val next = sols.map(allSolutions(cons, _, directions, subgraph))
          val nextPaths: LazyList[LongMap[Variance]] =
            next.toLazyList.flatMap {
              case Right(ll) => ll.toLazyList
              case _         => LazyList.empty
            }
          NonEmptyLazyList.fromLazyList(nextPaths) match {
            case Some(good) => Right(good)
            case None       =>
              // if there are no good ones, we can take the first error
              // but maybe we should somehow report all the errors?
              next.collectFirst { case err @ Left(_) => err }.get
          }
      } match {
        case Right(neLL) =>
          Validated.valid(neLL.head) // non-empty makes this safe
        case Left(errs) => Validated.invalid(errs)
      }

    def newState(
        imports: TypeEnv[Kind.Arg],
        dt: DefinedType[Either[KnownShape, Kind.Arg]]
    ): RefSpace[State] =
      (
        RefSpace.allocCounter,
        RefSpace.newRef(LongMap.empty[NonEmptyList[Constraint]]),
        RefSpace.newRef(Map.empty[rankn.Type.Const, KindFormula]),
        RefSpace.newRef(LongMap.empty[Direction])
      )
        .mapN { (longCnt, cons, consts, dirs) =>
          new State(imports, dt, longCnt, cons, consts, dirs)
        }

    private def unformulaKind(k: KindFormula, vars: LongMap[Variance]): Kind =
      k match {
        case Type       => Kind.Type
        case Cons(a, b) => Kind.Cons(unformula(a, vars), unformulaKind(b, vars))
      }

    def unformula(a: Arg, vars: LongMap[Variance]): Kind.Arg =
      Kind.Arg(vars(a.variance.id), unformulaKind(a.kind, vars))

    class State(
        imports: TypeEnv[Kind.Arg],
        dt: DefinedType[Either[KnownShape, Kind.Arg]],
        nextId: RefSpace[Long],
        cons: Ref[Cons],
        constFormulas: Ref[Map[rankn.Type.Const, KindFormula]],
        directions: Ref[LongMap[Direction]]
    ) {

      def nextVar(dir: Direction): RefSpace[Var] =
        for {
          id <- nextId
          _ <- directions.update { dirs => (dirs.updated(id, dir), ()) }
        } yield Var(id)

      def getConstraints: RefSpace[Cons] = cons.get
      def getDirections: RefSpace[LongMap[Direction]] = directions.get

      def shapeToFormula(
          dir: Direction,
          ks: KnownShape
      ): RefSpace[KindFormula] =
        ks match {
          case Shape.Type => RefSpace.pure(Type)
          case Shape.KnownCons(a, b) =>
            (shapeToArg(dir.reverse, a), shapeToFormula(dir, b))
              .mapN(Cons(_, _))
        }

      def shapeToArg(dir: Direction, ks: KnownShape): RefSpace[Arg] =
        for {
          v <- nextVar(dir)
          kf <- shapeToFormula(dir, ks)
        } yield Arg(v, kf)

      def addCons(
          v: Var,
          constraint: Constraint
      ): RefSpace[Unit] =
        cons.get.flatMap { cs =>
          val next = cs.get(v.id) match {
            case Some(nel) => constraint :: nel
            case None      => NonEmptyList(constraint, Nil)
          }

          cons.set(cs.updated(v.id, next))
        }

      def kindToFormula(
          dir: Direction,
          kind: Kind
      )(fn: Kind.Arg => Constraint): RefSpace[KindFormula] =
        kind match {
          case Kind.Type => RefSpace.pure(Type)
          case Kind.Cons(ka, b) =>
            for {
              a1 <- kindArgToArg(dir.reverse, ka)(fn)
              b1 <- kindToFormula(dir, b)(fn)
            } yield Cons(a1, b1)
        }

      def kindArgToArg(
          dir: Direction,
          ka: Kind.Arg
      )(fn: Kind.Arg => Constraint): RefSpace[Arg] =
        for {
          v <- nextVar(dir)
          _ <- addCons(v, fn(ka))
          form <- kindToFormula(dir, ka.kind)(fn)
        } yield Arg(v, form)

      def unifyKind(
          cfn: ConstructorFn,
          cfnIdx: Int,
          tpe: rankn.Type,
          kind: Kind,
          kf: KindFormula
      ): RefSpace[Unit] =
        (kind, kf) match {
          case (Kind.Type, Type) => RefSpace.unit
          case (Kind.Cons(Kind.Arg(v, i), r), Cons(Arg(vf, inf), rf)) =>
            addCons(vf, Constraint.UnifyVariance(cfn, cfnIdx, tpe, v)) *>
              unifyKind(cfn, cfnIdx, tpe, i, inf) *>
              unifyKind(cfn, cfnIdx, tpe, r, rf)
          // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
          case _ =>
            sys.error(
              s"invariant violation: shape violation: left = $kind right = $kf"
            )
          // $COVERAGE-ON$
        }

      def unifyKindFormula(
          cfn: ConstructorFn,
          cfnIdx: Int,
          tpe: rankn.Type,
          left: KindFormula,
          right: KindFormula
      ): RefSpace[Unit] =
        (left, right) match {
          case (Type, Type) => RefSpace.unit
          case (Cons(Arg(vl, il), rl), Cons(Arg(vr, ir), rr)) =>
            addCons(vl, Constraint.UnifyVar(cfn, cfnIdx, tpe, vr)) *>
              addCons(vr, Constraint.UnifyVar(cfn, cfnIdx, tpe, vl)) *>
              unifyKindFormula(cfn, cfnIdx, tpe, il, ir) *>
              unifyKindFormula(cfn, cfnIdx, tpe, rl, rr)
          // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
          case _ =>
            sys.error(
              s"invariant violation: shape violation: left = $left right = $right"
            )
          // $COVERAGE-ON$
        }

      def leftSubsumesRightKindFormula(
          cfn: ConstructorFn,
          cfnIdx: Int,
          tpe: rankn.Type,
          left: KindFormula,
          right: KindFormula
      ): RefSpace[Unit] =
        (left, right) match {
          case (Type, Type)                                   => RefSpace.unit
          case (Cons(Arg(vl, il), rl), Cons(Arg(vr, ir), rr)) =>
            // we know vl + vr == vl
            addCons(vl, Constraint.VarSubsumes(cfn, cfnIdx, tpe, vr)) *>
              // we switch the order here
              leftSubsumesRightKindFormula(cfn, cfnIdx, tpe, ir, il) *>
              leftSubsumesRightKindFormula(cfn, cfnIdx, tpe, rl, rr)
          // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
          case _ =>
            sys.error(
              s"invariant violation: shape violation: left = $left right = $right"
            )
          // $COVERAGE-ON$
        }

      def kindOfType(
          direction: Direction,
          thisKind: KindFormula,
          cfn: ConstructorFn,
          idx: Int,
          tpe: rankn.Type,
          kinds: Map[rankn.Type.Var.Bound, BoundState]
      ): RefSpace[KindFormula] =
        tpe match {
          case fa @ rankn.Type.ForAll(vs, t) =>
            val newKindMap = kinds ++ vs.toList.iterator.map { case (b, k) =>
              b -> BoundState.IsKind(k, fa, b)
            }
            kindOfType(direction, thisKind, cfn, idx, t, newKindMap)

          case rankn.Type.TyApply(on, _) =>
            // we don't need to unify here,
            // (k1 -> k2)[k1] == k2
            kindOfType(direction.reverse, thisKind, cfn, idx, on, kinds).map {
              case Cons(_, result) => result
              // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
              case Type =>
                sys.error(
                  s"invariant violation: shape violation found * expected k1 -> k2 in dt=$dt, cfn=$cfn, tpe=$tpe"
                )
              // $COVERAGE-ON$
            }
          case rankn.Type.TyConst(c) =>
            if ((tpe: rankn.Type) === dt.toTypeTyConst) RefSpace.pure(thisKind)
            else {
              // Has to be in the imports
              constFormulas.get.flatMap { consts =>
                consts.get(c) match {
                  case Some(kf) => RefSpace.pure(kf)
                  case None =>
                    imports.toDefinedType(c) match {
                      case Some(thisDt) =>
                        // we reset direct direction for a constant type
                        for {
                          kf <- kindToFormula(
                            Direction.PhantomUp,
                            thisDt.kindOf
                          )(
                            Constraint.ImportedConst(
                              cfn,
                              idx,
                              c,
                              thisDt.kindOf,
                              _
                            )
                          )
                          _ <- constFormulas.set(consts.updated(c, kf))
                        } yield kf
                      // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
                      case None =>
                        sys.error(
                          s"invariant violation: unknown const $c in dt=$dt, cfn=$cfn, tpe=$tpe"
                        )
                      // $COVERAGE-ON$
                    }
                }
              }
            }
          case rankn.Type.TyVar(b @ rankn.Type.Var.Bound(_)) =>
            kinds.get(b) match {
              case Some(BoundState.IsArg(a))     => RefSpace.pure(a.kind)
              case Some(BoundState.IsFormula(f)) => RefSpace.pure(f)
              case Some(BoundState.IsKind(k, fa, b)) =>
                kindToFormula(direction, k)(
                  Constraint.DeclaredType(cfn, idx, fa, b, _)
                )
              // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
              case None =>
                sys.error(
                  s"invariant violation: shape violation unbound var: $b dt=$dt, cfn=$cfn idx=$idx"
                )
              // $COVERAGE-ON$
            }
          // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
          case rankn.Type.TyVar(_) | rankn.Type.TyMeta(_) =>
            sys.error(
              s"invariant violation: inference type in declaration: $tpe"
            )
          // $COVERAGE-ON$
        }

      def addTypeConstraints(
          dir: Direction,
          thisKind: KindFormula,
          cfn: ConstructorFn,
          idx: Int,
          view: Var,
          tpe: rankn.Type,
          tpeKind: KindFormula,
          kinds: Map[rankn.Type.Var.Bound, BoundState]
      ): RefSpace[Unit] =
        tpe match {
          case fa @ rankn.Type.ForAll(vs, t) =>
            val newKindMap = kinds ++ vs.toList.iterator.map { case (b, k) =>
              b -> BoundState.IsKind(k, fa, b)
            }
            addTypeConstraints(
              dir,
              thisKind,
              cfn,
              idx,
              view,
              t,
              tpeKind,
              newKindMap
            )
          case ta @ rankn.Type.TyApply(on, arg) =>
            // on must have kind k -> tpeKind
            // arg must have kind k
            // an invariant is that shapes match
            kindOfType(dir.reverse, thisKind, cfn, idx, on, kinds).flatMap {
              case Cons(Arg(v, leftKf), res) =>
                for {
                  argKf <- kindOfType(dir, thisKind, cfn, idx, arg, kinds)
                  // views are never unconstrained
                  newView <- nextVar(Direction.PhantomUp)
                  _ <- addCons(newView, Constraint.IsProduct(view, v, ta))
                  _ <- leftSubsumesRightKindFormula(
                    cfn,
                    idx,
                    tpe,
                    leftKf,
                    argKf
                  )
                  _ <- addTypeConstraints(
                    dir.reverse,
                    thisKind,
                    cfn,
                    idx,
                    view,
                    on,
                    leftKf,
                    kinds
                  )
                  _ <- addTypeConstraints(
                    dir,
                    thisKind,
                    cfn,
                    idx,
                    newView,
                    arg,
                    argKf,
                    kinds
                  )
                  _ <- unifyKindFormula(cfn, idx, tpe, res, tpeKind)
                } yield ()
              // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
              case Type =>
                sys.error(
                  s"invariant violation: shape violation found * expected k1 -> k2 in dt=$dt, cfn=$cfn, tpe=$tpe"
                )
              // $COVERAGE-ON$

            }
          case tpe @ rankn.Type.TyConst(c) =>
            if ((tpe: rankn.Type) === dt.toTypeTyConst) {
              addCons(view, Constraint.RecursiveView(cfn, idx)) *>
                unifyKindFormula(cfn, idx, tpe, thisKind, tpeKind)
            } else {
              // Has to be in the imports
              imports.toDefinedType(c) match {
                case Some(thisDt) =>
                  unifyKind(cfn, idx, tpe, thisDt.kindOf, tpeKind)
                // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
                case None =>
                  sys.error(
                    s"invariant violation: unknown const $c in dt=$dt, cfn=$cfn, tpe=$tpe"
                  )
                // $COVERAGE-ON$
              }
            }
          case rankn.Type.TyVar(b @ rankn.Type.Var.Bound(_)) =>
            kinds.get(b) match {
              case Some(BoundState.IsArg(Arg(v, k))) =>
                // update the view constraints on arg
                addCons(v, Constraint.HasView(cfn, idx, b, view)) *>
                  unifyKindFormula(cfn, idx, tpe, k, tpeKind)
              case Some(BoundState.IsFormula(kind)) =>
                unifyKindFormula(cfn, idx, tpe, kind, tpeKind)
              case Some(BoundState.IsKind(kind, _, _)) =>
                unifyKind(cfn, idx, tpe, kind, tpeKind)
              // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
              case None =>
                sys.error(
                  s"invariant violation: unbound variable $b in dt=$dt, cfn=$cfn, idx = $idx"
                )
              // $COVERAGE-ON$
            }
          // $COVERAGE-OFF$ this should be unreachable due to typechecking
          case rankn.Type.TyVar(_) | rankn.Type.TyMeta(_) =>
            sys.error(
              s"invariant violation: inference type in declaration: $tpe"
            )
          // $COVERAGE-ON$
        }

      def addConstraints(
          thisKind: KindFormula,
          cfn: ConstructorFn,
          kinds: Map[rankn.Type.Var.Bound, BoundState]
      ): RefSpace[Unit] =
        cfn.args.zipWithIndex.traverse_ { case ((_, tpe), idx) =>
          for {
            v <- nextVar(Direction.PhantomUp)
            _ <- addCons(v, Constraint.Accessor(cfn, idx))
            _ <- addTypeConstraints(
              Direction.PhantomUp,
              thisKind,
              cfn,
              idx,
              v,
              tpe,
              Type,
              kinds
            )
          } yield ()
        }

    }
  }

  @annotation.tailrec
  final def mergeCrossProduct[A: Semigroup](
      nel: NonEmptyList[NonEmptyLazyList[A]]
  ): NonEmptyLazyList[A] =
    nel.tail match {
      case Nil      => nel.head
      case th :: tt =>
        // do the full cross of the first
        mergeCrossProduct(
          NonEmptyList((nel.head, th).mapN(Semigroup.combine(_, _)), tt)
        )
    }
}
