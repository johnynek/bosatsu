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
    case class Declared(
        idx: Int,
        variance: Variance,
        kind: Kind
    ) extends Constraint

    // arg idx of a given constructor function
    case class Accessor(cfn: ConstructorFn, idx: Int) extends Constraint
  }

  def solveKind(
      imports: TypeEnv[Kind.Arg],
      dt: DefinedType[Either[KnownShape, Kind.Arg]]
  ): ValidatedNec[Error, DefinedType[Kind.Arg]] = {

    def shapeToFormula(
        nextId: RefSpace[Long],
        ks: KnownShape
    ): RefSpace[KindFormula] =
      ks match {
        case Shape.Type => RefSpace.pure(Type)
        case Shape.KnownCons(a, b) =>
          (shapeToArg(nextId, a), shapeToFormula(nextId, b)).mapN(Cons(_, _))
      }

    def shapeToArg(nextId: RefSpace[Long], ks: KnownShape): RefSpace[Arg] =
      for {
        id <- nextId
        kf <- shapeToFormula(nextId, ks)
      } yield Arg(Var(id), kf)

    type Cons = LongMap[NonEmptyList[Constraint]]

    def addCons(
        v: Var,
        constraint: Constraint,
        cons: Ref[Cons]
    ): RefSpace[Unit] =
      cons.get.flatMap { cs =>
        val next = cs.get(v.id) match {
          case Some(nel) => constraint :: nel
          case None      => NonEmptyList(constraint, Nil)
        }

        cons.set(cs.updated(v.id, next))
      }

    def kindArgToFormula(
        nextId: RefSpace[Long],
        constraints: Ref[Cons],
        kind: Kind,
        idx: Int
    ): RefSpace[KindFormula] =
      kind match {
        case Kind.Type => RefSpace.pure(Type)
        case Kind.Cons(ka, b) =>
          for {
            a1 <- kindArgToArg(nextId, constraints, ka, idx)
            b1 <- kindArgToFormula(nextId, constraints, b, idx)
          } yield Cons(a1, b1)
      }

    def kindArgToArg(
        nextId: RefSpace[Long],
        constraints: Ref[Cons],
        ka: Kind.Arg,
        idx: Int
    ): RefSpace[Arg] =
      for {
        id <- nextId
        v = Var(id)
        _ <- addCons(
          v,
          Constraint.Declared(idx, ka.variance, ka.kind),
          constraints
        )
        form <- kindArgToFormula(nextId, constraints, ka.kind, idx)
      } yield Arg(v, form)

    def addTypeConstraints(
        thisKind: KindFormula,
        cfn: ConstructorFn,
        idx: Int,
        view: Var,
        tpe: rankn.Type,
        tpeKind: KindFormula,
        nextId: RefSpace[Long],
        constraints: Ref[Cons],
        kinds: Map[rankn.Type.Var.Bound, Arg]
    ): RefSpace[Unit] =
      tpe match {
        case rankn.Type.ForAll(vs, t)    => ???
        case rankn.Type.TyApply(on, arg) =>
          // on must have kind k -> tpeKind
          // arg must have kind k
          ???
        case tpe @ rankn.Type.TyConst(c) =>
          if ((tpe: rankn.Type) === dt.toTypeTyConst) {
            // if this is a recursive call, view must be covariant
            ???
          } else {
            // Has to be in the imports
            ???
          }
        case rankn.Type.TyVar(b @ rankn.Type.Var.Bound(_)) =>
          kinds.get(b) match {
            case Some(arg) =>
              // update the view constraints on arg
              ???
            case None =>
              // unbound variable
              ???
          }
        // $COVERAGE-OFF$ this should be unreachable due to typechecking
        case rankn.Type.TyVar(_) | rankn.Type.TyMeta(_) =>
          sys.error(s"invariant violation: inference type in declaration: $tpe")
        // $COVERAGE-ON$
      }

    def addConstraints(
        thisKind: KindFormula,
        cfn: ConstructorFn,
        nextId: RefSpace[Long],
        constraints: Ref[Cons],
        kinds: Map[rankn.Type.Var.Bound, Arg]
    ): RefSpace[Unit] =
      cfn.args.zipWithIndex.traverse_ { case ((_, tpe), idx) =>
        for {
          id <- nextId
          v = Var(id)
          _ <- addCons(v, Constraint.Accessor(cfn, idx), constraints)
          _ <- addTypeConstraints(
            thisKind,
            cfn,
            idx,
            v,
            tpe,
            Type,
            nextId,
            constraints,
            kinds
          )
        } yield ()
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

    def unformulaKind(k: KindFormula, vars: LongMap[Variance]): Kind =
      k match {
        case Type => Kind.Type
        case Cons(a, b) => Kind.Cons(unformula(a, vars), unformulaKind(b, vars))
      }

    def unformula(a: Arg, vars: LongMap[Variance]): Kind.Arg =
        Kind.Arg(vars(a.variance.id), unformulaKind(a.kind, vars))

    (for {
      nextId <- RefSpace.allocCounter
      varMapRef <- RefSpace.newRef(LongMap.empty[NonEmptyList[Constraint]])
      dtFormula <- dt.zipWithIndex.traverse {
        case (Left(ks), _)    => shapeToArg(nextId, ks)
        case (Right(ka), idx) => kindArgToArg(nextId, varMapRef, ka, idx)
      }
      kindMap = dtFormula.annotatedTypeParams.toMap
      thisKind = dtFormula.annotatedTypeParams.foldRight(Type: KindFormula) {
        case ((_, a), t) => Cons(a, t)
      }
      _ <- dtFormula.constructors.traverse { cfn =>
        addConstraints(thisKind, cfn, nextId, varMapRef, kindMap)
      }
      constraints <- varMapRef.get
      topo = combineTopo(constraints)
      maybeRes = go(constraints, topo).map { vars =>
        dtFormula.map(unformula(_, vars))
      }
    } yield maybeRes).run.value
  }
}
