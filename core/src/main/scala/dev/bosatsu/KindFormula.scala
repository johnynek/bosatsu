package dev.bosatsu

import cats.{Foldable, Semigroup}
import cats.data.{
  Ior,
  IorNec,
  NonEmptyLazyList,
  NonEmptyList,
  EitherNec,
  ValidatedNec
}
import dev.bosatsu.rankn.{
  Ref,
  RefSpace,
  Type => RankNType,
  TypeEnv,
  DefinedType,
  ParsedTypeEnv,
  TypeAlias,
  TypeDecl
}
import dev.bosatsu.graph.Dag
import dev.bosatsu.Shape.KnownShape
import scala.collection.immutable.{LongMap, SortedSet}

import cats.syntax.all._
import cats.data.Validated

sealed abstract class KindFormula derives CanEqual

object KindFormula {
  given cats.Eq[KindFormula] = cats.Eq.fromUniversalEquals

  type Source = Shape.Source
  val Source: Shape.Source.type = Shape.Source

  case class Var(id: Long)
  object Var {
    given cats.Eq[Var] = cats.Eq.fromUniversalEquals
  }

  case class Arg(variance: Var, kind: KindFormula)

  case object Type extends KindFormula
  case class Cons(arg: Arg, result: KindFormula) extends KindFormula

  sealed abstract class Error {
    def typeDecl: TypeDecl[Option[Kind.Arg]]
  }
  object Error {
    case class Unsatisfiable(
        dte: TypeDecl[Either[KnownShape, Kind.Arg]],
        constraints: LongMap[NonEmptyList[Constraint]],
        existing: LongMap[Variance],
        unknowns: SortedSet[Long]
    ) extends Error {
      def typeDecl: TypeDecl[Option[Kind.Arg]] =
        dte.mapAnnotations(_.toOption)
    }

    case class FromShapeError(shapeError: Shape.Error) extends Error {
      def typeDecl = shapeError.typeDecl
    }
  }

  sealed abstract class Sat derives CanEqual
  object Sat {
    case object No extends Sat
    case object Yes extends Sat
    case object Maybe extends Sat

    given cats.Eq[Sat] = cats.Eq.fromUniversalEquals

    inline def apply(bool: Boolean): Sat =
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
      def depends = List.empty[Var]
      def satisfied(known: LongMap[Variance], value: Variance) =
        if (value == kindArg.variance) Sat.Yes
        else Sat.No
    }

    case class DeclaredType(
        source: Source,
        constructorIdx: Int,
        quantified: rankn.Type,
        bound: rankn.Type.Var.Bound,
        kindArg: Kind.Arg
    ) extends Constraint {
      def depends = List.empty[Var]
      def satisfied(known: LongMap[Variance], value: Variance) =
        Sat(value == kindArg.variance)
    }

    case class ImportedConst(
        source: Source,
        constructorIdx: Int,
        const: rankn.Type.Const,
        kind: Kind,
        kindArg: Kind.Arg
    ) extends Constraint {
      def depends = List.empty[Var]
      def satisfied(known: LongMap[Variance], value: Variance) =
        Sat(value == kindArg.variance)
    }

    // arg idx of a given constructor function
    case class Accessor(source: Source, idx: Int) extends Constraint {
      def depends = List.empty[Var]
      def satisfied(known: LongMap[Variance], value: Variance) =
        Sat(value == Variance.co || value == Variance.in)
    }

    case class RecursiveView(source: Source, idx: Int)
        extends Constraint {
      def depends = List.empty[Var]
      def satisfied(known: LongMap[Variance], value: Variance) =
        Sat(value == Variance.co)
    }

    case class HasView(
        source: Source,
        idx: Int,
        bound: rankn.Type.Var.Bound,
        view: Var
    ) extends Constraint {
      def depends = view :: Nil
      def satisfied(known: LongMap[Variance], value: Variance) =
        known.get(view.id) match {
          case Some(viewVariance) =>
            Sat((viewVariance + value) == value)
          case None => Sat.Maybe
        }
    }

    case class UnifyVariance(
        source: Source,
        idx: Int,
        inType: rankn.Type,
        variance: Variance
    ) extends Constraint {
      def depends = List.empty[Var]
      def satisfied(known: LongMap[Variance], value: Variance) =
        Sat(value == variance)
    }

    case class UnifyVar(
        source: Source,
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
        source: Source,
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

  trait IsTypeEnv[E] {
    def getKind(env: E, tc: rankn.Type.Const): Option[Kind]

    final def toShapeEnv: Shape.IsShapeEnv[E] =
      new Shape.IsShapeEnv[E] {
        def getShape(e: E, tc: rankn.Type.Const): Option[Shape.KnownShape] =
          getKind(e, tc).map(Shape.shapeOf)
      }
  }

  object IsTypeEnv {
    def apply[E](implicit ite: IsTypeEnv[E]): IsTypeEnv[E] = ite

    implicit val typeEnvIsTypeEnv: IsTypeEnv[TypeEnv[Kind.Arg]] =
      new IsTypeEnv[TypeEnv[Kind.Arg]] {
        def getKind(
            env: TypeEnv[Kind.Arg],
            tc: rankn.Type.Const
        ): Option[Kind] =
          env
            .toDefinedType(tc)
            .map(_.kindOf)
            .orElse(env.toTypeAlias(tc).map(_.kindOf))
      }

    implicit def tuple2TypeEnv[A: IsTypeEnv, B: IsTypeEnv]: IsTypeEnv[(A, B)] =
      new IsTypeEnv[(A, B)] {
        def getKind(
            env: (A, B),
            tc: rankn.Type.Const
        ): Option[Kind] =
          IsTypeEnv[A]
            .getKind(env._1, tc)
            .orElse(IsTypeEnv[B].getKind(env._2, tc))
      }

    implicit val singleTypeEnv: IsTypeEnv[DefinedType[Kind.Arg]] =
      new IsTypeEnv[DefinedType[Kind.Arg]] {
        def getKind(
            dt: DefinedType[Kind.Arg],
            tc: rankn.Type.Const
        ): Option[Kind] =
          if ((dt.toTypeConst: rankn.Type.Const) == tc) Some(dt.kindOf)
          else None
      }

    implicit val singleAliasTypeEnv: IsTypeEnv[TypeAlias[Kind.Arg]] =
      new IsTypeEnv[TypeAlias[Kind.Arg]] {
        def getKind(
            ta: TypeAlias[Kind.Arg],
            tc: rankn.Type.Const
        ): Option[Kind] =
          if ((ta.toTypeConst: rankn.Type.Const) == tc) Some(ta.kindOf)
          else None
      }

    implicit def foldableTypeEnv[F[_]: Foldable, E: IsTypeEnv]
        : IsTypeEnv[F[E]] =
      new IsTypeEnv[F[E]] {
        def getKind(env: F[E], tc: rankn.Type.Const) =
          env.collectFirstSomeM[cats.Id, Kind](
            IsTypeEnv[E].getKind(_, tc)
          )
      }

    implicit val emptyTypeEnv: IsTypeEnv[Unit] =
      new IsTypeEnv[Unit] {
        def getKind(env: Unit, tc: rankn.Type.Const) =
          Option.empty[Kind]
      }
  }

  def solveShapesAndKinds[E: IsTypeEnv](
      imports: E,
      dts: List[DefinedType[Option[Kind.Arg]]]
  ): IorNec[Error, List[DefinedType[Kind.Arg]]] = {
    implicit val shapeEnv: Shape.IsShapeEnv[E] = IsTypeEnv[E].toShapeEnv
    Shape
      .solveAll(imports, dts)
      .leftMap(_.map(Error.FromShapeError(_)))
      .flatMap(solveAll(imports, _))
  }

  def solveAliasShapes[E: IsTypeEnv](
      imports: E,
      alias: TypeAlias[Option[Kind.Arg]]
  ): ValidatedNec[Error, TypeAlias[Either[KnownShape, Kind.Arg]]] = {
    implicit val shapeEnv: Shape.IsShapeEnv[E] = IsTypeEnv[E].toShapeEnv
    Shape.solveAlias(imports, alias).leftMap(_.map(Error.FromShapeError(_)))
  }

  def solveAliasKinds[E: IsTypeEnv](
      imports: E,
      alias: TypeAlias[Either[KnownShape, Kind.Arg]]
  ): ValidatedNec[Error, TypeAlias[Kind.Arg]] = {
    import Impl._

    (for {
      state <- Impl.newState(imports, alias, allowSelfReference = false)
      params <- alias.annotatedTypeParams.zipWithIndex.traverse {
        case ((v, Left(ks)), _)    =>
          state.shapeToArg(Direction.PhantomUp, ks).map(v -> _)
        case ((v, Right(ka)), idx) =>
          state.kindArgToArg(ka)(Constraint.DeclaredParam(idx, _)).map(v -> _)
      }
      aliasFormula = alias.copy(annotatedTypeParams = params)
      kindMap = aliasFormula.annotatedTypeParams.iterator.map { case (k, v) =>
        (k, Impl.BoundState.IsArg(v))
      }.toMap
      thisKind = aliasFormula.annotatedTypeParams.foldRight(Type: KindFormula) {
        case ((_, a), t) => Cons(a, t)
      }
      view <- state.nextVar(Direction.PhantomUp)
      _ <- state.addCons(
        view,
        Constraint.UnifyVariance(Source.AliasBody, 0, alias.rhs, Variance.co)
      )
      _ <- state.addTypeConstraints(
        Direction.PhantomUp,
        thisKind,
        Source.AliasBody,
        0,
        view,
        alias.rhs,
        Type,
        kindMap
      )
      constraints <- state.getConstraints
      dirs <- state.getDirections
      varCount <- state.nextId
      topo = Impl.combineTopo(varCount, constraints)
      maybeRes = Impl.go(alias, constraints, dirs, topo).map { vars =>
        aliasFormula.copy(
          annotatedTypeParams =
            aliasFormula.annotatedTypeParams.map { case (tv, arg) =>
              (tv, Impl.unformula(arg, vars))
            }
        )
      }
    } yield maybeRes).run.value
  }

  def solveShapesAndKinds[E: IsTypeEnv](
      imports: E,
      parsedTypeEnv: ParsedTypeEnv[Option[Kind.Arg]]
  ): IorNec[Error, ParsedTypeEnv[Kind.Arg]] = {
    implicit val shapeEnv: Shape.IsShapeEnv[E] = IsTypeEnv[E].toShapeEnv

    def stmtTyConst(
        stmt: ParsedTypeEnv.TypeStatement[?]
    ): RankNType.TyConst =
      RankNType.TyConst(stmt.toTypeConst)

    def stmtDependsOn(
        stmt: ParsedTypeEnv.TypeStatement[?]
    ): List[RankNType.TyConst] =
      stmt match {
        case ParsedTypeEnv.TypeStatement.Defined(dt) => dt.dependsOn
        case ParsedTypeEnv.TypeStatement.Alias(ta)   => ta.dependsOn
      }

    parsedTypeEnv.orderedTypes.reverse
      .foldM((List.empty[DefinedType[Kind.Arg]], List.empty[TypeAlias[Kind.Arg]], List.empty[
        ParsedTypeEnv.TypeStatement[Kind.Arg]
      ], Set.empty[RankNType.TyConst])) {
        case ((dtsAcc, aliasAcc, orderedAcc, failed), stmt) =>
          val priorEnv = ((imports, dtsAcc), aliasAcc)
          if (failed.nonEmpty && stmtDependsOn(stmt).exists(failed)) {
            // Skip solving statements that depend on previously failed items.
            // This preserves the old behavior for non-alias code and avoids
            // follow-on "unknown type" noise for local names that already failed.
            Ior.Right((dtsAcc, aliasAcc, orderedAcc, failed + stmtTyConst(stmt)))
          } else {
            stmt match {
              case ParsedTypeEnv.TypeStatement.Defined(dt) =>
                val res =
                  Shape
                    .solveShape(priorEnv, dt)
                    .leftMap(_.map(Error.FromShapeError(_)))
                    .andThen(solveKind(priorEnv, _))
                res match {
                  case Validated.Valid(good)   =>
                    Ior.Right(
                      (
                        good :: dtsAcc,
                        aliasAcc,
                        ParsedTypeEnv.TypeStatement.Defined(good) :: orderedAcc,
                        failed
                      )
                    )
                  case Validated.Invalid(errs) =>
                    Ior.Both(
                      errs,
                      (
                        dtsAcc,
                        aliasAcc,
                        orderedAcc,
                        failed + stmtTyConst(stmt)
                      )
                    )
                }
              case ParsedTypeEnv.TypeStatement.Alias(alias) =>
                val res =
                  Shape
                    .solveAlias(priorEnv, alias)
                    .leftMap(_.map(Error.FromShapeError(_)))
                    .andThen(solveAliasKinds(priorEnv, _))
                res match {
                  case Validated.Valid(good)   =>
                    Ior.Right(
                      (
                        dtsAcc,
                        good :: aliasAcc,
                        ParsedTypeEnv.TypeStatement.Alias(good) :: orderedAcc,
                        failed
                      )
                    )
                  case Validated.Invalid(errs) =>
                    Ior.Both(
                      errs,
                      (
                        dtsAcc,
                        aliasAcc,
                        orderedAcc,
                        failed + stmtTyConst(stmt)
                      )
                    )
                }
            }
          }
      }
      .map { case (dts, aliases, ordered, _) =>
        ParsedTypeEnv(
          allDefinedTypes = dts.reverse,
          typeAliases = aliases.reverse,
          orderedTypes = ordered.reverse,
          externalDefs = parsedTypeEnv.externalDefs
        )
      }
  }

  def solveAll[E: IsTypeEnv](
      imports: E,
      dts: List[DefinedType[Either[KnownShape, Kind.Arg]]]
  ): IorNec[Error, List[DefinedType[Kind.Arg]]] =
    dts
      .foldM(
        (List.empty[DefinedType[Kind.Arg]], Set.empty[RankNType.TyConst])
      ) { case ((acc, failed), dt) =>
        // don't evaluate dependsOn if failed is empty
        if (failed.nonEmpty && dt.dependsOn.exists(failed)) {
          // If this type depends on a previously failed type, skip solving it and
          // mark it as failed too. This prevents later types from seeing this local
          // type as "unknown const" when it was only skipped due to dependency failure.
          Ior.Right((acc, failed + dt.toTypeTyConst))
        } else {
          solveKind((imports, acc), dt) match {
            case Validated.Valid(good)   => Ior.Right((good :: acc, failed))
            case Validated.Invalid(errs) =>
              Ior.Both(errs, (acc, failed + dt.toTypeTyConst))
          }
        }
      }
      .map(_._1.reverse)

  def solveKind[Env: IsTypeEnv](
      imports: Env,
      dt: DefinedType[Either[KnownShape, Kind.Arg]]
  ): ValidatedNec[Error, DefinedType[Kind.Arg]] = {
    import Impl._

    (for {
      state <- Impl.newState(imports, dt, allowSelfReference = true)
      dtFormula <- dt.zipWithIndex.traverse {
        case (Left(ks), _)    => state.shapeToArg(Direction.PhantomUp, ks)
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
        val localKindMap = kindMap ++ cfn.exists.iterator.map { case (k, v) =>
          (k, Impl.BoundState.IsArg(v))
        }
        state.addConstraints(
          thisKind,
          Source.ConstructorFn(cfn),
          cfn.args.map(_.tpe),
          localKindMap
        )
      }
      constraints <- state.getConstraints
      dirs <- state.getDirections
      // we have allocated all variance variables now, we can see how many
      varCount <- state.nextId
      topo = Impl.combineTopo(varCount, constraints)
      maybeRes = Impl.go(dt, constraints, dirs, topo).map { vars =>
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
          quant: rankn.Type,
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
    def combineTopo(
        varCount: Long,
        cons: Cons
    ): Vector[NonEmptyList[SortedSet[Long]]] = {
      val nextFn: Long => Iterator[Long] = { l =>
        cons.get(l) match {
          case Some(consNel) =>
            consNel.toList.iterator.flatMap(_.depends).map(_.id)
          case None => Iterator.empty
        }
      }
      val (_, dag) = Dag.dagify(0L until varCount)(nextFn)
      dag.toToposorted.layers
    }

    // Report Errors, or enumerate possibilities for subgraph
    // invariant: all subgraph values must be valid keys in the result
    // we can process the subgraph list in parallel. Those are all the indepentent next values
    def allSolutionChunk(
        typeDecl: TypeDecl[Either[Shape.KnownShape, Kind.Arg]],
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
            // some values are unconstrained
            val e1 = existing.updated(l, v)
            cons.get(l) match {
              case Some(cs) =>
                cs.forall(_.satisfied(e1, v) != Sat.No)
              case None => true
            }
          }

        // all possible choices enumerated in the correct directions
        // excluding the existing variances
        def allVariances(lst: List[Long]): LazyList[LongMap[Variance]] =
          lst match {
            case Nil       => LongMap.empty[Variance] #:: LazyList.empty
            case h :: tail =>
              val hvs = varsFor(h)
              val tails = allVariances(tail)
              // Make sure not to re-evaluate the above
              (hvs, tails).mapN((v, tailMap) => tailMap.updated(h, v))
          }

        // does this satisfy all constraints for the subgraph
        def isValid(lm: LongMap[Variance]): Boolean =
          // println(s"trying: $lm")
          subgraph.forall { id =>
            val value = lm(id)
            // some values are unconstrained
            cons.get(id) match {
              case Some(cs) =>
                // println(s"constraints for $id = $value")
                cs.forall { c =>
                  val res = c.satisfied(lm, value)
                  // println(s"constraint: $c == $res")
                  res == Sat.Yes
                }
              case None =>
                // println(s"no constraints for $id = $value")
                true
            }
          }

        val validVariances: LazyList[LongMap[Variance]] =
          allVariances(subgraph.toList).map(existing ++ _).filter(isValid(_))

        NonEmptyLazyList.fromLazyList(validVariances) match {
          case Some(nel) => Validated.valid(nel)
          case None      =>
            Validated.invalidNec(
              Error.Unsatisfiable(typeDecl, cons, existing, subgraph)
            )
        }
      }

    def allSolutions(
        typeDecl: TypeDecl[Either[Shape.KnownShape, Kind.Arg]],
        cons: Cons,
        existing: LongMap[Variance],
        directions: LongMap[Direction],
        subgraph: NonEmptyList[SortedSet[Long]]
    ): EitherNec[Error, NonEmptyLazyList[LongMap[Variance]]] =
      subgraph
        .traverse(allSolutionChunk(typeDecl, cons, existing, directions, _))
        .map(KindFormula.mergeCrossProduct(_))
        .toEither

    def go[F[_]: Foldable](
        typeDecl: TypeDecl[Either[Shape.KnownShape, Kind.Arg]],
        cons: Cons,
        directions: LongMap[Direction],
        topo: F[NonEmptyList[SortedSet[Long]]]
    ): ValidatedNec[Error, LongMap[Variance]] =
      topo.foldM(NonEmptyLazyList(LongMap.empty[Variance])) {
        (sols, subgraph) =>
          // if there is at least one good solution, we can keep going
          val next =
            sols.map(allSolutions(typeDecl, cons, _, directions, subgraph))
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

    def newState[E: IsTypeEnv](
        imports: E,
        typeDecl: TypeDecl[Either[KnownShape, Kind.Arg]],
        allowSelfReference: Boolean
    ): RefSpace[State[E]] =
      (
        RefSpace.allocCounter,
        RefSpace.newRef(LongMap.empty[NonEmptyList[Constraint]]),
        RefSpace.newRef(Map.empty[rankn.Type.Const, KindFormula]),
        RefSpace.newRef(LongMap.empty[Direction])
      )
        .mapN { (longCnt, cons, consts, dirs) =>
          new State(
            imports,
            typeDecl,
            allowSelfReference,
            longCnt,
            cons,
            consts,
            dirs
          )
        }

    private def unformulaKind(k: KindFormula, vars: LongMap[Variance]): Kind =
      k match {
        case Type       => Kind.Type
        case Cons(a, b) => Kind.Cons(unformula(a, vars), unformulaKind(b, vars))
      }

    def unformula(a: Arg, vars: LongMap[Variance]): Kind.Arg =
      Kind.Arg(vars(a.variance.id), unformulaKind(a.kind, vars))

    class State[E: IsTypeEnv](
        imports: E,
        typeDecl: TypeDecl[Either[KnownShape, Kind.Arg]],
        allowSelfReference: Boolean,
        val nextId: RefSpace[Long],
        cons: Ref[Cons],
        constFormulas: Ref[Map[rankn.Type.Const, KindFormula]],
        directions: Ref[LongMap[Direction]]
    ) {

      def nextVar(dir: Direction): RefSpace[Var] =
        for {
          id <- nextId
          _ <- directions.update(dirs => (dirs.updated(id, dir), ()))
        } yield Var(id)

      def getConstraints: RefSpace[Cons] = cons.get
      def getDirections: RefSpace[LongMap[Direction]] = directions.get

      def shapeToFormula(
          dir: Direction,
          ks: KnownShape
      ): RefSpace[KindFormula] =
        ks match {
          case Shape.Type            => RefSpace.pure(Type)
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
          // println(s"addCons(v = $v, $constraint)")
          val next = cs.get(v.id) match {
            case Some(nel) => constraint :: nel
            case None      => NonEmptyList.one(constraint)
          }

          cons.set(cs.updated(v.id, next))
        }

      def kindToFormula(
          kind: Kind
      )(fn: Kind.Arg => Constraint): RefSpace[KindFormula] =
        kind match {
          case Kind.Type        => RefSpace.pure(Type)
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
          v <- nextVar(
            Direction.PhantomUp
          ) // We know exactly which value this is, so direction doesn't matter
          _ <- addCons(v, fn(ka))
          form <- kindToFormula(ka.kind)(fn)
        } yield Arg(v, form)

      def unifyKind(
          source: Source,
          sourceIdx: Int,
          tpe: rankn.Type,
          kind: Kind,
          kf: KindFormula
      ): RefSpace[Unit] =
        (kind, kf) match {
          case (Kind.Type, Type) => RefSpace.unit
          case (Kind.Cons(Kind.Arg(v, i), r), Cons(Arg(vf, inf), rf)) =>
            addCons(vf, Constraint.UnifyVariance(source, sourceIdx, tpe, v)) *>
              unifyKind(source, sourceIdx, tpe, i, inf) *>
              unifyKind(source, sourceIdx, tpe, r, rf)
          // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
          case _ =>
            sys.error(
              s"invariant violation: $source, idx = $sourceIdx, tpe=$tpe shape violation: left = $kind right = $kf"
            )
          // $COVERAGE-ON$
        }

      def unifyKindFormula(
          source: Source,
          sourceIdx: Int,
          tpe: rankn.Type,
          left: KindFormula,
          right: KindFormula
      ): RefSpace[Unit] =
        (left, right) match {
          case (a, b) if a == b                               => RefSpace.unit
          case (Cons(Arg(vl, il), rl), Cons(Arg(vr, ir), rr)) =>
            val vs = if (vl =!= vr) {
              addCons(vl, Constraint.UnifyVar(source, sourceIdx, tpe, vr)) *>
                addCons(vr, Constraint.UnifyVar(source, sourceIdx, tpe, vl))
            } else RefSpace.unit

            vs *>
              unifyKindFormula(source, sourceIdx, tpe, il, ir) *>
              unifyKindFormula(source, sourceIdx, tpe, rl, rr)
          // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
          case _ =>
            sys.error(
              s"invariant violation: $source, idx = $sourceIdx, tpe=$tpe shape violation: left = $left right = $right"
            )
          // $COVERAGE-ON$
        }

      def leftSubsumesRightKindFormula(
          source: Source,
          sourceIdx: Int,
          tpe: rankn.Type,
          left: KindFormula,
          right: KindFormula
      ): RefSpace[Unit] =
        (left, right) match {
          case (a, b) if a == b                               => RefSpace.unit
          case (Cons(Arg(vl, il), rl), Cons(Arg(vr, ir), rr)) =>
            // we know vl + vr == vl
            val vs =
              if (vl =!= vr)
                addCons(vl, Constraint.VarSubsumes(source, sourceIdx, tpe, vr))
              else RefSpace.unit

            vs *>
              // we switch the order here
              leftSubsumesRightKindFormula(source, sourceIdx, tpe, ir, il) *>
              leftSubsumesRightKindFormula(source, sourceIdx, tpe, rl, rr)
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
          source: Source,
          idx: Int,
          tpe: rankn.Type,
          kinds: Map[rankn.Type.Var.Bound, BoundState]
      ): RefSpace[KindFormula] =
        tpe match {
          case fa @ rankn.Type.ForAll(vars, in) =>
            val newKindMap = kinds ++ vars.toList.iterator.map { case (b, k) =>
              b -> BoundState.IsKind(k, fa, b)
            }
            kindOfType(direction, thisKind, source, idx, in, newKindMap)
          case ex @ rankn.Type.Exists(vars, in) =>
            val newKindMap = kinds ++ vars.toList.iterator.map { case (b, k) =>
              b -> BoundState.IsKind(k, ex, b)
            }
            kindOfType(direction, thisKind, source, idx, in, newKindMap)

          case rankn.Type.TyApply(on, _) =>
            // we don't need to unify here,
            // (k1 -> k2)[k1] == k2
            kindOfType(direction.reverse, thisKind, source, idx, on, kinds).map {
              case Cons(_, result) => result
              // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
              case Type =>
                sys.error(
                  s"invariant violation: shape violation found * expected k1 -> k2 in typeDecl=$typeDecl, source=$source, tpe=$tpe"
                )
              // $COVERAGE-ON$
            }
          case rankn.Type.TyConst(c) =>
            if (allowSelfReference && ((tpe: rankn.Type) == typeDecl.toTypeTyConst))
              RefSpace.pure(thisKind)
            else {
              // Has to be in the imports
              constFormulas.get.flatMap { consts =>
                consts.get(c) match {
                  case Some(kf) => RefSpace.pure(kf)
                  case None     =>
                    val kind = IsTypeEnv[E]
                      .getKind(imports, c) match {
                      case Some(kind) => kind
                      // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
                      case None =>
                        sys.error(
                          s"invariant violation (line 674): unknown const $c in typeDecl=$typeDecl, source=$source, tpe=$tpe"
                        )
                      // $COVERAGE-ON$
                    }
                    // we reset direct direction for a constant type
                    for {
                      kf <- kindToFormula(kind)(
                        Constraint.ImportedConst(
                          source,
                          idx,
                          c,
                          kind,
                          _
                        )
                      )
                      _ <- constFormulas.set(consts.updated(c, kf))
                    } yield kf
                }
              }
            }
          case rankn.Type.TyVar(b @ rankn.Type.Var.Bound(_)) =>
            kinds.get(b) match {
              case Some(BoundState.IsArg(a))         => RefSpace.pure(a.kind)
              case Some(BoundState.IsFormula(f))     => RefSpace.pure(f)
              case Some(BoundState.IsKind(k, fa, b)) =>
                kindToFormula(k)(
                  Constraint.DeclaredType(source, idx, fa, b, _)
                )
              // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
              case None =>
                sys.error(
                  s"invariant violation: shape violation unbound var: $b typeDecl=$typeDecl source=$source idx=$idx"
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
          source: Source,
          idx: Int,
          view: Var,
          tpe: rankn.Type,
          tpeKind: KindFormula,
          kinds: Map[rankn.Type.Var.Bound, BoundState]
      ): RefSpace[Unit] = {
        // println(s"addTypeConstraints(\ndir=$dir\nthisKind=$thisKind\nsource=$source\nidx=$idx\nview=$view\ntpe=$tpe\ntpeKind=$tpeKind\nkinds=$kinds\n)")
        tpe match {
          case fa @ rankn.Type.ForAll(vars, in) =>
            val newKindMap = kinds ++ vars.iterator.map { case (b, k) =>
              b -> BoundState.IsKind(k, fa, b)
            }
            addTypeConstraints(
              dir,
              thisKind,
              source,
              idx,
              view,
              in,
              tpeKind,
              newKindMap
            )
          case ex @ rankn.Type.Exists(vars, in) =>
            val newKindMap = kinds ++ vars.iterator.map { case (b, k) =>
              b -> BoundState.IsKind(k, ex, b)
            }
            addTypeConstraints(
              dir,
              thisKind,
              source,
              idx,
              view,
              in,
              tpeKind,
              newKindMap
            )
          case ta @ rankn.Type.TyApply(on, arg) =>
            // on must have kind k -> tpeKind
            // arg must have kind k
            // an invariant is that shapes match
            kindOfType(dir.reverse, thisKind, source, idx, on, kinds).flatMap {
              case onKind @ Cons(Arg(v, leftKf), res) =>
                for {
                  argKf <- kindOfType(dir, thisKind, source, idx, arg, kinds)
                  // views are never unconstrained
                  newView <- nextVar(Direction.PhantomUp)
                  _ <- addCons(newView, Constraint.IsProduct(view, v, ta))
                  _ <- leftSubsumesRightKindFormula(
                    source,
                    idx,
                    tpe,
                    leftKf,
                    argKf
                  )
                  _ <- addTypeConstraints(
                    dir,
                    thisKind,
                    source,
                    idx,
                    newView,
                    arg,
                    argKf,
                    kinds
                  )
                  _ <- addTypeConstraints(
                    dir.reverse,
                    thisKind,
                    source,
                    idx,
                    view,
                    on,
                    onKind,
                    kinds
                  )
                  _ <- leftSubsumesRightKindFormula(source, idx, tpe, res, tpeKind)
                } yield ()
              // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
              case Type =>
                sys.error(
                  s"invariant violation: shape violation found * expected k1 -> k2 in typeDecl=$typeDecl, source=$source, tpe=$tpe"
                )
              // $COVERAGE-ON$
            }
          case tpe @ rankn.Type.TyConst(c) =>
            if (allowSelfReference && ((tpe: rankn.Type) == typeDecl.toTypeTyConst)) {
              addCons(view, Constraint.RecursiveView(source, idx)) *>
                unifyKindFormula(source, idx, tpe, thisKind, tpeKind)
            } else {
              // Has to be in the imports
              val kind = IsTypeEnv[E]
                .getKind(imports, c) match {
                case Some(k) => k
                // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
                case None =>
                  sys.error(
                    s"invariant violation (line 805): unknown const $c in typeDecl=$typeDecl, source=$source, tpe=$tpe"
                  )
                // $COVERAGE-ON$
              }
              unifyKind(source, idx, tpe, kind, tpeKind)
            }
          case rankn.Type.TyVar(b @ rankn.Type.Var.Bound(_)) =>
            kinds.get(b) match {
              case Some(BoundState.IsArg(Arg(v, k))) =>
                // update the view constraints on arg
                addCons(v, Constraint.HasView(source, idx, b, view)) *>
                  unifyKindFormula(source, idx, tpe, k, tpeKind)
              case Some(BoundState.IsFormula(kind)) =>
                unifyKindFormula(source, idx, tpe, kind, tpeKind)
              case Some(BoundState.IsKind(kind, _, _)) =>
                unifyKind(source, idx, tpe, kind, tpeKind)
              // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
              case None =>
                sys.error(
                  s"invariant violation: unbound variable $b in typeDecl=$typeDecl, source=$source, idx = $idx"
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
      }

      def addConstraints(
          thisKind: KindFormula,
          source: Source,
          tpes: List[rankn.Type],
          kinds: Map[rankn.Type.Var.Bound, BoundState]
      ): RefSpace[Unit] =
        tpes.zipWithIndex.traverse_ { case (tpe, idx) =>
          for {
            v <- nextVar(Direction.PhantomUp)
            _ <- addCons(v, Constraint.Accessor(source, idx))
            _ <- addTypeConstraints(
              Direction.PhantomUp,
              thisKind,
              source,
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
