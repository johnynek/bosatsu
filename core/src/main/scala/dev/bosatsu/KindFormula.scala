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
  ConstructorFn,
  Ref,
  RefSpace,
  Type => RankNType,
  TypeEnv,
  DefinedType
}
import dev.bosatsu.graph.Dag
import dev.bosatsu.Shape.KnownShape
import scala.collection.immutable.{LongMap, SortedSet}

import cats.syntax.all._
import cats.data.Validated

sealed abstract class KindFormula derives CanEqual

object KindFormula {
  given cats.Eq[KindFormula] = cats.Eq.fromUniversalEquals

  case class Var(id: Long)
  object Var {
    given cats.Eq[Var] = cats.Eq.fromUniversalEquals
  }

  case class Arg(variance: Var, kind: KindFormula)

  case object Type extends KindFormula
  case class Cons(arg: Arg, result: KindFormula) extends KindFormula

  sealed abstract class Error {
    def dt: DefinedType[Option[Kind.Arg]]
  }
  object Error {
    case class Unsatisfiable(
        dte: DefinedType[Either[KnownShape, Kind.Arg]],
        constraints: LongMap[NonEmptyList[Constraint]],
        existing: LongMap[Variance],
        unknowns: SortedSet[Long]
    ) extends Error {
      def dt: DefinedType[Option[Kind.Arg]] = dte.map(_.toOption)
    }

    case class FromShapeError(shapeError: Shape.Error) extends Error {
      def dt = shapeError.dt
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
        cfn: ConstructorFn[?],
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
        cfn: ConstructorFn[?],
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
    case class Accessor(cfn: ConstructorFn[?], idx: Int) extends Constraint {
      def depends = List.empty[Var]
      def satisfied(known: LongMap[Variance], value: Variance) =
        Sat(value == Variance.co || value == Variance.in)
    }

    case class RecursiveView(cfn: ConstructorFn[?], idx: Int) extends Constraint {
      def depends = List.empty[Var]
      def satisfied(known: LongMap[Variance], value: Variance) =
        Sat(value == Variance.co)
    }

    case class HasView(
        cfn: ConstructorFn[?],
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
        cfn: ConstructorFn[?],
        idx: Int,
        inType: rankn.Type,
        variance: Variance
    ) extends Constraint {
      def depends = List.empty[Var]
      def satisfied(known: LongMap[Variance], value: Variance) =
        Sat(value == variance)
    }

    case class UnifyVar(
        cfn: ConstructorFn[?],
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
        cfn: ConstructorFn[?],
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
    def getDefinedType(
        env: E,
        tc: rankn.Type.Const
    ): Option[DefinedType[Kind.Arg]]

    final def toShapeEnv: Shape.IsShapeEnv[E] =
      new Shape.IsShapeEnv[E] {
        def getShape(e: E, tc: rankn.Type.Const): Option[Shape.KnownShape] =
          getDefinedType(e, tc).map(Shape.ShapeOf(_))
      }
  }

  object IsTypeEnv {
    def apply[E](implicit ite: IsTypeEnv[E]): IsTypeEnv[E] = ite

    implicit val typeEnvIsTypeEnv: IsTypeEnv[TypeEnv[Kind.Arg]] =
      new IsTypeEnv[TypeEnv[Kind.Arg]] {
        def getDefinedType(
            env: TypeEnv[Kind.Arg],
            tc: rankn.Type.Const
        ): Option[DefinedType[Kind.Arg]] =
          env.toDefinedType(tc)
      }

    implicit def tuple2TypeEnv[A: IsTypeEnv, B: IsTypeEnv]: IsTypeEnv[(A, B)] =
      new IsTypeEnv[(A, B)] {
        def getDefinedType(
            env: (A, B),
            tc: rankn.Type.Const
        ): Option[DefinedType[Kind.Arg]] =
          IsTypeEnv[A]
            .getDefinedType(env._1, tc)
            .orElse(IsTypeEnv[B].getDefinedType(env._2, tc))
      }

    implicit val singleTypeEnv: IsTypeEnv[DefinedType[Kind.Arg]] =
      new IsTypeEnv[DefinedType[Kind.Arg]] {
        def getDefinedType(
            dt: DefinedType[Kind.Arg],
            tc: rankn.Type.Const
        ) =
          if ((dt.toTypeConst: rankn.Type.Const) == tc) Some(dt)
          else None
      }

    implicit def foldableTypeEnv[F[_]: Foldable, E: IsTypeEnv]
        : IsTypeEnv[F[E]] =
      new IsTypeEnv[F[E]] {
        def getDefinedType(env: F[E], tc: rankn.Type.Const) =
          env.collectFirstSomeM[cats.Id, DefinedType[Kind.Arg]](
            IsTypeEnv[E].getDefinedType(_, tc)
          )
      }

    implicit val emptyTypeEnv: IsTypeEnv[Unit] =
      new IsTypeEnv[Unit] {
        def getDefinedType(env: Unit, tc: rankn.Type.Const) =
          Option.empty[DefinedType[Kind.Arg]]
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

  def solveAll[E: IsTypeEnv](
      imports: E,
      dts: List[DefinedType[Either[KnownShape, Kind.Arg]]]
  ): IorNec[Error, List[DefinedType[Kind.Arg]]] =
    dts
      .foldM(
        (List.empty[DefinedType[Kind.Arg]], Set.empty[RankNType.TyConst])
      ) { case (st @ (acc, failed), dt) =>
        // don't evaluate dependsOn if failed is empty
        if (failed.nonEmpty && dt.dependsOn.exists(failed)) {
          // there was at least one failure already, just return and let that failure signal
          Ior.Right(st)
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
      state <- Impl.newState(imports, dt)
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
        state.addConstraints(thisKind, cfn, localKindMap)
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
        dt: DefinedType[Either[Shape.KnownShape, Kind.Arg]],
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
              Error.Unsatisfiable(dt, cons, existing, subgraph)
            )
        }
      }

    def allSolutions(
        dt: DefinedType[Either[Shape.KnownShape, Kind.Arg]],
        cons: Cons,
        existing: LongMap[Variance],
        directions: LongMap[Direction],
        subgraph: NonEmptyList[SortedSet[Long]]
    ): EitherNec[Error, NonEmptyLazyList[LongMap[Variance]]] =
      subgraph
        .traverse(allSolutionChunk(dt, cons, existing, directions, _))
        .map(KindFormula.mergeCrossProduct(_))
        .toEither

    def go[F[_]: Foldable](
        dt: DefinedType[Either[Shape.KnownShape, Kind.Arg]],
        cons: Cons,
        directions: LongMap[Direction],
        topo: F[NonEmptyList[SortedSet[Long]]]
    ): ValidatedNec[Error, LongMap[Variance]] =
      topo.foldM(NonEmptyLazyList(LongMap.empty[Variance])) {
        (sols, subgraph) =>
          // if there is at least one good solution, we can keep going
          val next = sols.map(allSolutions(dt, cons, _, directions, subgraph))
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
        dt: DefinedType[Either[KnownShape, Kind.Arg]]
    ): RefSpace[State[E]] =
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

    class State[E: IsTypeEnv](
        imports: E,
        dt: DefinedType[Either[KnownShape, Kind.Arg]],
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
          cfn: ConstructorFn[?],
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
              s"invariant violation: $cfn, idx = $cfnIdx, tpe=$tpe shape violation: left = $kind right = $kf"
            )
          // $COVERAGE-ON$
        }

      def unifyKindFormula(
          cfn: ConstructorFn[?],
          cfnIdx: Int,
          tpe: rankn.Type,
          left: KindFormula,
          right: KindFormula
      ): RefSpace[Unit] =
        (left, right) match {
          case (a, b) if a == b                               => RefSpace.unit
          case (Cons(Arg(vl, il), rl), Cons(Arg(vr, ir), rr)) =>
            val vs = if (vl =!= vr) {
              addCons(vl, Constraint.UnifyVar(cfn, cfnIdx, tpe, vr)) *>
                addCons(vr, Constraint.UnifyVar(cfn, cfnIdx, tpe, vl))
            } else RefSpace.unit

            vs *>
              unifyKindFormula(cfn, cfnIdx, tpe, il, ir) *>
              unifyKindFormula(cfn, cfnIdx, tpe, rl, rr)
          // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
          case _ =>
            sys.error(
              s"invariant violation: $cfn, idx = $cfnIdx, tpe=$tpe shape violation: left = $left right = $right"
            )
          // $COVERAGE-ON$
        }

      def leftSubsumesRightKindFormula(
          cfn: ConstructorFn[?],
          cfnIdx: Int,
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
                addCons(vl, Constraint.VarSubsumes(cfn, cfnIdx, tpe, vr))
              else RefSpace.unit

            vs *>
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
          cfn: ConstructorFn[?],
          idx: Int,
          tpe: rankn.Type,
          kinds: Map[rankn.Type.Var.Bound, BoundState]
      ): RefSpace[KindFormula] =
        tpe match {
          case fa @ rankn.Type.ForAll(vars, in) =>
            val newKindMap = kinds ++ vars.toList.iterator.map {
              case (b, k) =>
                b -> BoundState.IsKind(k, fa, b)
            }
            kindOfType(direction, thisKind, cfn, idx, in, newKindMap)
          case ex @ rankn.Type.Exists(vars, in) =>
            val newKindMap = kinds ++ vars.toList.iterator.map {
              case (b, k) =>
                b -> BoundState.IsKind(k, ex, b)
            }
            kindOfType(direction, thisKind, cfn, idx, in, newKindMap)

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
            if ((tpe: rankn.Type) == dt.toTypeTyConst) RefSpace.pure(thisKind)
            else {
              // Has to be in the imports
              constFormulas.get.flatMap { consts =>
                consts.get(c) match {
                  case Some(kf) => RefSpace.pure(kf)
                  case None     =>
                    val kind = IsTypeEnv[E].getDefinedType(imports, c) match {
                      case Some(thisDt) => thisDt.kindOf
                      case None         =>
                        // some test code relies on syntax but doesn't import predef
                        // TODO remove the built ins here
                        rankn.Type.builtInKinds.get(c.toDefined) match {
                          case Some(kind) => kind
                          // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
                          case None =>
                            sys.error(
                              s"invariant violation (line 674): unknown const $c in dt=$dt, cfn=$cfn, tpe=$tpe"
                            )
                        }
                      // $COVERAGE-ON$
                    }
                    // we reset direct direction for a constant type
                    for {
                      kf <- kindToFormula(kind)(
                        Constraint.ImportedConst(
                          cfn,
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
          cfn: ConstructorFn[?],
          idx: Int,
          view: Var,
          tpe: rankn.Type,
          tpeKind: KindFormula,
          kinds: Map[rankn.Type.Var.Bound, BoundState]
      ): RefSpace[Unit] = {
        // println(s"addTypeConstraints(\ndir=$dir\nthisKind=$thisKind\ncfn=$cfn\nidx=$idx\nview=$view\ntpe=$tpe\ntpeKind=$tpeKind\nkinds=$kinds\n)")
        tpe match {
          case fa @ rankn.Type.ForAll(vars, in) =>
            val newKindMap = kinds ++ vars.iterator.map { case (b, k) =>
              b -> BoundState.IsKind(k, fa, b)
            }
            addTypeConstraints(
              dir,
              thisKind,
              cfn,
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
              cfn,
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
            kindOfType(dir.reverse, thisKind, cfn, idx, on, kinds).flatMap {
              case onKind @ Cons(Arg(v, leftKf), res) =>
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
                    dir,
                    thisKind,
                    cfn,
                    idx,
                    newView,
                    arg,
                    argKf,
                    kinds
                  )
                  _ <- addTypeConstraints(
                    dir.reverse,
                    thisKind,
                    cfn,
                    idx,
                    view,
                    on,
                    onKind,
                    kinds
                  )
                  _ <- leftSubsumesRightKindFormula(cfn, idx, tpe, res, tpeKind)
                } yield ()
              // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
              case Type =>
                sys.error(
                  s"invariant violation: shape violation found * expected k1 -> k2 in dt=$dt, cfn=$cfn, tpe=$tpe"
                )
              // $COVERAGE-ON$
            }
          case tpe @ rankn.Type.TyConst(c) =>
            if ((tpe: rankn.Type) == dt.toTypeTyConst) {
              addCons(view, Constraint.RecursiveView(cfn, idx)) *>
                unifyKindFormula(cfn, idx, tpe, thisKind, tpeKind)
            } else {
              // Has to be in the imports
              val kind = IsTypeEnv[E]
                .getDefinedType(imports, c)
                .map(_.kindOf)
                .orElse(rankn.Type.builtInKinds.get(c.toDefined)) match {
                case Some(k) => k
                // $COVERAGE-OFF$ this should be unreachable due to shapechecking happening first
                case None =>
                  sys.error(
                    s"invariant violation (line 805): unknown const $c in dt=$dt, cfn=$cfn, tpe=$tpe"
                  )
                // $COVERAGE-ON$
              }
              unifyKind(cfn, idx, tpe, kind, tpeKind)
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
      }

      def addConstraints(
          thisKind: KindFormula,
          cfn: ConstructorFn[?],
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
