package org.bykn.bosatsu

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

  sealed abstract class Constraint
  object Constraint {
    case class DeclaredParam(
        idx: Int,
        kindArg: Kind.Arg
    ) extends Constraint

    case class DeclaredType(
        cfn: ConstructorFn,
        constructorIdx: Int,
        forAll: rankn.Type.ForAll,
        bound: rankn.Type.Var.Bound,
        kindArg: Kind.Arg
    ) extends Constraint

    case class ImportedConst(
        cfn: ConstructorFn,
        constructorIdx: Int,
        const: rankn.Type.Const,
        kind: Kind,
        kindArg: Kind.Arg
    ) extends Constraint

    // arg idx of a given constructor function
    case class Accessor(cfn: ConstructorFn, idx: Int) extends Constraint

    case class RecursiveView(cfn: ConstructorFn, idx: Int) extends Constraint

    case class HasView(
        cfn: ConstructorFn,
        idx: Int,
        bound: rankn.Type.Var.Bound,
        view: Var
    ) extends Constraint

    case class UnifyVariance(
        cfn: ConstructorFn,
        idx: Int,
        inType: rankn.Type,
        variance: Variance
    ) extends Constraint

    case class UnifyVar(
        cfn: ConstructorFn,
        idx: Int,
        inType: rankn.Type,
        variance: Var
    ) extends Constraint

    // if constrainted by this v + variance == v
    case class VarSubsumes(
        cfn: ConstructorFn,
        idx: Int,
        inType: rankn.Type,
        variance: Var
    ) extends Constraint

    case class IsProduct(
        view: Var,
        argVariance: Var,
        tapply: rankn.Type.TyApply
    ) extends Constraint
  }

  def solveKind(
      imports: TypeEnv[Kind.Arg],
      dt: DefinedType[Either[KnownShape, Kind.Arg]]
  ): ValidatedNec[Error, DefinedType[Kind.Arg]] = {

    (for {
      state <- Impl.newState(imports, dt)
      dtFormula <- dt.zipWithIndex.traverse {
        case (Left(ks), _) => state.shapeToArg(ks)
        case (Right(ka), idx) =>
          state.kindArgToArg(ka) { ka =>
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
      topo = Impl.combineTopo(constraints)
      maybeRes = Impl.go(constraints, topo).map { vars =>
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

    // dagify the constraint graph, and then topologically sort it
    def combineTopo(cons: Cons): List[List[SortedSet[Long]]] = ???

    // Report Errors, or enumerate possibilities for subgraph
    // invariant: all subgraph values must be valid keys in the result
    // we can process the subgraph list in parallel. Those are all the indepentent next values
    def allSolutions(
        cons: Cons,
        existing: LongMap[Variance],
        subgraph: List[SortedSet[Long]]
    ): EitherNec[Error, NonEmptyLazyList[LongMap[Variance]]] = ???

    def go(
        cons: Cons,
        topo: List[List[SortedSet[Long]]]
    ): ValidatedNec[Error, LongMap[Variance]] =
      topo.foldM(NonEmptyLazyList(LongMap.empty[Variance])) {
        (sols, subgraph) =>
          // if there is at least one good solution, we can keep going
          val next = sols.map(allSolutions(cons, _, subgraph))
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
        RefSpace.newRef(Map.empty[rankn.Type.Const, KindFormula])
      )
        .mapN { (longCnt, cons, consts) =>
          new State(imports, dt, longCnt.map(Var(_)), cons, consts)
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
        nextVar: RefSpace[Var],
        cons: Ref[Cons],
        constFormulas: Ref[Map[rankn.Type.Const, KindFormula]]
    ) {

      def getConstraints: RefSpace[Cons] = cons.get

      def shapeToFormula(
          ks: KnownShape
      ): RefSpace[KindFormula] =
        ks match {
          case Shape.Type => RefSpace.pure(Type)
          case Shape.KnownCons(a, b) =>
            (shapeToArg(a), shapeToFormula(b)).mapN(Cons(_, _))
        }

      def shapeToArg(ks: KnownShape): RefSpace[Arg] =
        for {
          v <- nextVar
          kf <- shapeToFormula(ks)
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
          kind: Kind
      )(fn: Kind.Arg => Constraint): RefSpace[KindFormula] =
        kind match {
          case Kind.Type => RefSpace.pure(Type)
          case Kind.Cons(ka, b) =>
            for {
              a1 <- kindArgToArg(ka)(fn)
              b1 <- kindToFormula(b)(fn)
            } yield Cons(a1, b1)
        }

      def kindArgToArg(
          ka: Kind.Arg
      )(fn: Kind.Arg => Constraint): RefSpace[Arg] =
        for {
          v <- nextVar
          _ <- addCons(v, fn(ka))
          form <- kindToFormula(ka.kind)(fn)
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
            kindOfType(thisKind, cfn, idx, t, newKindMap)

          case rankn.Type.TyApply(on, _) =>
            // we don't need to unify here,
            // (k1 -> k2)[k1] == k2
            kindOfType(thisKind, cfn, idx, on, kinds).map {
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
                        for {
                          kf <- kindToFormula(thisDt.kindOf)(
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
                kindToFormula(k)(Constraint.DeclaredType(cfn, idx, fa, b, _))
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
            kindOfType(thisKind, cfn, idx, on, kinds).flatMap {
              case Cons(Arg(v, leftKf), res) =>
                for {
                  argKf <- kindOfType(thisKind, cfn, idx, arg, kinds)
                  newView <- nextVar
                  _ <- addCons(newView, Constraint.IsProduct(view, v, ta))
                  _ <- leftSubsumesRightKindFormula(
                    cfn,
                    idx,
                    tpe,
                    leftKf,
                    argKf
                  )
                  _ <- addTypeConstraints(
                    thisKind,
                    cfn,
                    idx,
                    view,
                    on,
                    leftKf,
                    kinds
                  )
                  _ <- addTypeConstraints(
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
            v <- nextVar
            _ <- addCons(v, Constraint.Accessor(cfn, idx))
            _ <- addTypeConstraints(
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
}
