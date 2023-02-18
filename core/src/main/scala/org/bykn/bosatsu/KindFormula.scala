package org.bykn.bosatsu

import cats.data.{NonEmptyLazyList, EitherNec, ValidatedNec}
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

  def solveKind(
      imports: TypeEnv[Kind.Arg],
      dt: DefinedType[Either[KnownShape, Kind.Arg]]
  ): ValidatedNec[Error, DefinedType[Kind.Arg]] = {

    def shapeToFormula(nextId: RefSpace[Long], ks: KnownShape): RefSpace[Arg] =
      ???
    def kindArgToFormula(
        nextId: RefSpace[Long],
        constraints: Ref[LongMap[Constraint]],
        ka: Kind.Arg
    ): RefSpace[Arg] = ???

    def addConstraints(
        cfn: ConstructorFn,
        constraints: Ref[LongMap[Constraint]],
        kinds: Map[rankn.Type.Var.Bound, Arg]
    ): RefSpace[Unit] = ???

    def recursionConstraint(
        constraints: Ref[LongMap[Constraint]],
        kinds: Map[rankn.Type.Var.Bound, Arg]
    ): RefSpace[Unit] = ???

    // dagify the constraint graph, and then topologically sort it
    def combineTopo(cons: LongMap[Constraint]): List[SortedSet[Long]] = ???

    // Report Errors, or enumerate possibilities for subgraph
    // invariant: all subgraph values must be valid keys in the result
    def allSolutions(
        cons: LongMap[Constraint],
        existing: LongMap[Variance],
        subgraph: SortedSet[Long]
    ): EitherNec[Error, NonEmptyLazyList[LongMap[Variance]]] = ???

    def go(
        cons: LongMap[Constraint],
        topo: List[SortedSet[Long]]
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

    def unformula(a: Arg, vars: LongMap[Variance]): Kind.Arg = ???

    (for {
      nextId <- RefSpace.allocCounter
      varMapRef <- RefSpace.newRef(LongMap.empty[Constraint])
      dtFormula <- dt.traverse {
        case Left(ks)  => shapeToFormula(nextId, ks)
        case Right(ka) => kindArgToFormula(nextId, varMapRef, ka)
      }
      kindMap = dtFormula.annotatedTypeParams.toMap
      _ <- dtFormula.constructors.traverse { cfn =>
        addConstraints(cfn, varMapRef, kindMap)
      }
      _ <- recursionConstraint(varMapRef, kindMap)
      constraints <- varMapRef.get
      topo = combineTopo(constraints)
      maybeRes = go(constraints, topo).map { vars =>
        dtFormula.map(unformula(_, vars))
      }
    } yield maybeRes).run.value
  }
}
