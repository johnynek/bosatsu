package dev.bosatsu

import cats.{Apply, Foldable}
import cats.data.{Validated, ValidatedNec, Ior, IorNec}
import dev.bosatsu.rankn.{
  ConstructorFn,
  DefinedType,
  Ref,
  RefSpace,
  Type,
  TypeAlias,
  TypeDecl
}
import org.typelevel.paiges.Doc

import cats.syntax.all._

// Shape is Kind without variance, and variables
sealed abstract class Shape derives CanEqual

object Shape {
  sealed abstract class KnownShape extends Shape derives CanEqual
  sealed abstract class NotKnownShape extends Shape derives CanEqual

  case object Type extends KnownShape

  def cons(arg: Shape, res: Shape): Shape =
    (arg, res) match {
      case (ksa: KnownShape, kres: KnownShape) => KnownCons(ksa, kres)
      case _                                   => Cons(arg, res)
    }

  case class KnownCons(arg: KnownShape, res: KnownShape) extends KnownShape

  case class Cons(arg: Shape, res: Shape) extends NotKnownShape {
    Require(
      !(arg.isInstanceOf[KnownShape] && res.isInstanceOf[KnownShape]),
      s"cons must have one unknown: $this"
    )
  }

  sealed abstract class UnknownState derives CanEqual
  object UnknownState {
    case object Free extends UnknownState
    case class Fixed(toKnown: KnownShape) extends UnknownState
    case class Linked(notKnowns: Set[NotKnownShape]) extends UnknownState

    def free: UnknownState = Free
  }

  case class Unknown(
      bound: Either[rankn.Type.TyApply, rankn.Type.Var.Bound],
      ref: Ref[UnknownState]
  ) extends NotKnownShape

  enum Source derives CanEqual {
    case ConstructorFn(cfn: dev.bosatsu.rankn.ConstructorFn[?])
    case AliasBody
  }

  def shapeDoc(s: Shape): Doc =
    s match {
      case Type           => Doc.char('*')
      case Cons(Type, s2) => Doc.text("* -> ") + shapeDoc(s2)
      case Cons(s1, s2)   =>
        Doc.char('(') + shapeDoc(s1) + Doc.char(')') + Doc.text(
          " -> "
        ) + shapeDoc(s2)
      case KnownCons(Type, s2) => Doc.text("* -> ") + shapeDoc(s2)
      case KnownCons(s1, s2)   =>
        Doc.char('(') + shapeDoc(s1) + Doc.char(')') + Doc.text(
          " -> "
        ) + shapeDoc(s2)
      case Unknown(bound, _) =>
        val tpe = bound match {
          case Right(b) => rankn.Type.TyVar(b)
          case Left(ta) => ta
        }
        val tdoc = rankn.Type.fullyResolvedDocument.document(tpe)
        Doc.text("kind(") + tdoc + Doc.char(')')
    }

  sealed abstract class Error {
    def typeDecl: TypeDecl[Option[Kind.Arg]]
  }

  case class ShapeLoop(
      typeDecl: TypeDecl[Option[Kind.Arg]],
      bound: Either[rankn.Type.TyApply, rankn.Type.Var.Bound],
      path: Set[Shape]
  ) extends Error

  case class UnificationError(
      typeDecl: TypeDecl[Option[Kind.Arg]],
      source: Source,
      left: Shape,
      right: Shape
  ) extends Error

  case class FinishFailure(
      typeDecl: TypeDecl[Option[Kind.Arg]],
      left: Shape,
      right: Shape
  ) extends Error

  case class UnknownConst(
      typeDecl: TypeDecl[Option[Kind.Arg]],
      source: Source,
      const: rankn.Type.Const
  ) extends Error

  case class UnboundVar(
      typeDecl: TypeDecl[Option[Kind.Arg]],
      source: Source,
      bound: rankn.Type.Var.Bound
  ) extends Error

  case class ShapeMismatch(
      typeDecl: TypeDecl[Option[Kind.Arg]],
      source: Source,
      outer: rankn.Type,
      tapply: rankn.Type.TyApply,
      rightShape: Shape
  ) extends Error

  def shapeOf(k: Kind): KnownShape =
    k match {
      case Kind.Type                    => Type
      case Kind.Cons(Kind.Arg(_, a), b) =>
        KnownCons(shapeOf(a), shapeOf(b))
    }

  trait ShapeOf[A] {
    def shape(a: A): KnownShape
  }

  object ShapeOf {
    implicit val kindShapeOf: ShapeOf[Kind] = { (k: Kind) => shapeOf(k) }

    implicit val kindArgShapeOf: ShapeOf[Kind.Arg] = { (k: Kind.Arg) =>
      shapeOf(k.kind)
    }

    implicit val knownShapeOf: ShapeOf[KnownShape] = { (k: KnownShape) => k }

    implicit def definedTypeShapeOf[S: ShapeOf]: ShapeOf[DefinedType[S]] = {
      (dt: DefinedType[S]) =>
        dt.annotatedTypeParams
          .foldRight(Type: KnownShape) { (s, ks) =>
            KnownCons(ShapeOf(s._2), ks)
          }
    }

    implicit def typeAliasShapeOf[S: ShapeOf]: ShapeOf[TypeAlias[S]] = {
      (ta: TypeAlias[S]) =>
        ta.annotatedTypeParams
          .foldRight(Type: KnownShape) { (s, ks) =>
            KnownCons(ShapeOf(s._2), ks)
          }
    }

    implicit def eitherShapeOf[A: ShapeOf, B: ShapeOf]
        : ShapeOf[Either[A, B]] = { (e: Either[A, B]) =>
      e match {
        case Right(a) => ShapeOf(a)
        case Left(b)  => ShapeOf(b)
      }
    }

    def apply[A](a: A)(implicit s: ShapeOf[A]): KnownShape =
      s.shape(a)
  }

  trait IsShapeEnv[E] {
    def getShape(env: E, tc: rankn.Type.Const): Option[KnownShape]
  }

  object IsShapeEnv {
    def apply[E](implicit ise: IsShapeEnv[E]): IsShapeEnv[E] = ise

    implicit def tuple2IsShapeEnv[A: IsShapeEnv, B: IsShapeEnv]
        : IsShapeEnv[(A, B)] =
      new IsShapeEnv[(A, B)] {
        def getShape(env: (A, B), tc: rankn.Type.Const): Option[KnownShape] =
          IsShapeEnv[A]
            .getShape(env._1, tc)
            .orElse(IsShapeEnv[B].getShape(env._2, tc))
      }

    implicit def singleShapeEnv[S: ShapeOf]: IsShapeEnv[DefinedType[S]] =
      new IsShapeEnv[DefinedType[S]] {
        def getShape(
            dt: DefinedType[S],
            tc: rankn.Type.Const
        ): Option[KnownShape] =
          if ((dt.toTypeConst: rankn.Type.Const) == tc) Some(ShapeOf(dt))
          else None
      }

    implicit def typeAliasShapeEnv[S: ShapeOf]: IsShapeEnv[TypeAlias[S]] =
      new IsShapeEnv[TypeAlias[S]] {
        def getShape(
            ta: TypeAlias[S],
            tc: rankn.Type.Const
        ): Option[KnownShape] =
          if ((ta.toTypeConst: rankn.Type.Const) == tc) Some(ShapeOf(ta))
          else None
      }

    implicit def foldableShapeEnv[F[_]: Foldable, E: IsShapeEnv]
        : IsShapeEnv[F[E]] =
      new IsShapeEnv[F[E]] {
        def getShape(env: F[E], tc: rankn.Type.Const): Option[KnownShape] =
          env.collectFirstSomeM[cats.Id, KnownShape](
            IsShapeEnv[E].getShape(_, tc)
          )
      }

    implicit val emptyShapeEnv: IsShapeEnv[Unit] =
      new IsShapeEnv[Unit] {
        def getShape(env: Unit, tc: rankn.Type.Const) = Option.empty[KnownShape]
      }
  }

  def solveAll[E: IsShapeEnv](
      imports: E,
      dts: List[DefinedType[Option[Kind.Arg]]]
  ): IorNec[Error, List[DefinedType[Either[KnownShape, Kind.Arg]]]] =
    dts
      .foldM(List.empty[DefinedType[Either[KnownShape, Kind.Arg]]]) {
        (acc, dt) =>
          solveShape((imports, acc), dt) match {
            case Validated.Valid(good)   => Ior.Right(good :: acc)
            case Validated.Invalid(errs) => Ior.Both(errs, acc)
          }
      }
      .map(_.reverse)

  private final class Solver[E: IsShapeEnv](
      imports: E,
      typeDecl: TypeDecl[Option[Kind.Arg]],
      allowSelfReference: Boolean
  ) {
    type Env = Type => Option[Shape]

    private val cache = scala.collection.mutable.Map.empty[Type, Option[Shape]]
    private val validUnit = Validated.valid(())
    private val pureUnit = RefSpace.pure(validUnit)

    def scopeFor(
        lst: List[(rankn.Type.Var.Bound, Either[Shape, Kind.Arg])]
    ): Env = {
      val smap: Map[rankn.Type.Var.Bound, Shape] =
        lst.iterator.map {
          case (v, Right(ka)) => (v, ShapeOf(ka))
          case (v, Left(s))   => (v, s)
        }.toMap

      val selfShape =
        Option.when(allowSelfReference) {
          lst.foldRight(Shape.Type: Shape) {
            case ((_, Right(k)), res) =>
              Shape.cons(ShapeOf(k), res)
            case ((_, Left(s)), res)  =>
              Shape.cons(s, res)
          }
        }

      { (t: Type) =>
        cache.getOrElseUpdate(
          t,
          t match {
            case rankn.Type.TyVar(v @ rankn.Type.Var.Bound(_)) => smap.get(v)
            case rankn.Type.TyConst(const)                     =>
              if (allowSelfReference && (const == (typeDecl.toTypeConst: rankn.Type.Const)))
                selfShape
              else IsShapeEnv[E].getShape(imports, const)
            case _ =>
              None
          }
        )
      }
    }

    def shapeToKnown(s: Shape): RefSpace[ValidatedNec[Error, KnownShape]] = {
      def maybeKnownShape(
          s: Shape,
          checked: Set[Shape]
      ): RefSpace[Option[KnownShape]] =
        if (checked(s)) RefSpace.pure(None)
        else
          s match {
            case ks: KnownShape     => RefSpace.pure(Some(ks))
            case not: NotKnownShape => maybeKnown(not :: Nil, checked)
          }

      def maybeKnown(
          s: List[NotKnownShape],
          checked: Set[Shape]
      ): RefSpace[Option[KnownShape]] =
        s match {
          case h :: tail if checked(h)     => maybeKnown(tail, checked)
          case (cons @ Cons(a, s)) :: tail =>
            val c1 = checked + cons
            (maybeKnownShape(a, c1), maybeKnownShape(s, c1))
              .flatMapN { (oa, os) =>
                val cons = (oa, os).mapN(KnownCons(_, _))
                if (cons.isDefined) RefSpace.pure(cons)
                else maybeKnown(tail, c1 + a + s)
              }
          case (u @ Unknown(_, ref)) :: rest =>
            ref.get.flatMap {
              case UnknownState.Free =>
                RefSpace.pure(Some(Type))
              case UnknownState.Fixed(f)   => RefSpace.pure(Some(f))
              case UnknownState.Linked(ls) =>
                maybeKnown(ls.toList ::: rest, checked + u)
            }
          case Nil => RefSpace.pure(None)
        }
      def generalizeOnNotKnown(
          s: Shape,
          checked: Set[Shape],
          bound: KnownShape
      ): RefSpace[KnownShape] =
        s match {
          case ks: KnownShape     => RefSpace.pure(ks)
          case not: NotKnownShape => generalize(not :: Nil, checked, bound)
        }

      def generalize(
          s: List[NotKnownShape],
          checked: Set[Shape],
          shape: KnownShape
      ): RefSpace[KnownShape] =
        s match {
          case h :: tail if checked(h)  => generalize(tail, checked, shape)
          case (c @ Cons(a, s)) :: tail =>
            val (in, out) = shape match {
              case Type            => (Type, Type)
              case KnownCons(i, o) => (i, o)
            }
            (
              generalizeOnNotKnown(a, checked, in),
              generalizeOnNotKnown(s, checked, out)
            )
              .flatMapN { (oa, os) =>
                generalize(tail, checked + c + a + s, KnownCons(oa, os))
              }
          case (u @ Unknown(_, ref)) :: rest =>
            ref.get.flatMap {
              case UnknownState.Free =>
                generalize(rest, checked + u, shape)
              case UnknownState.Fixed(f) =>
                RefSpace.pure(f)
              case UnknownState.Linked(ls) =>
                generalize(ls.toList ::: rest, checked + u, shape)
            }
          case Nil => RefSpace.pure(shape)
        }
      s match {
        case ks: KnownShape =>
          RefSpace.pure(Validated.valid(ks))
        case not: NotKnownShape =>
          maybeKnownShape(not, Set.empty).flatMap {
            case Some(known) =>
              unifyShape(known, not)(FinishFailure(typeDecl, _, _))
                .map(_.as(known))
            case None =>
              generalize(not :: Nil, Set.empty, Type)
                .flatMap { known =>
                  unifyShape(known, s)(FinishFailure(typeDecl, _, _))
                    .map(_.as(known))
                }
          }
      }
    }

    def unifyShape(
        s1: Shape,
        s2: Shape
    )(mkErr: (Shape, Shape) => Error): RefSpace[ValidatedNec[Error, Unit]] = {
      inline def error =
        RefSpace.pure(Validated.invalidNec(mkErr(s1, s2)))

      inline def unifyCons(
          a: Shape,
          b: Shape,
          c: Shape,
          d: Shape,
          visited: Set[(Shape, Shape)]
      ): RefSpace[ValidatedNec[Error, Unit]] =
        Apply[RefSpace].map2(loop(a, c, visited), loop(b, d, visited))(_ *> _)

      def unifyKnown(
          ks: KnownShape,
          u: Unknown,
          visited: Set[(Shape, Shape)]
      ): RefSpace[ValidatedNec[Error, Unit]] =
        u.ref.get.flatMap {
          case UnknownState.Free =>
            u.ref.set(UnknownState.Fixed(ks)).as(validUnit)
          case UnknownState.Fixed(ks1) =>
            loop(ks, ks1, visited)
          case UnknownState.Linked(notKnown) =>
            notKnown.toList
              .traverse(loop(ks, _, visited))
              .map(combineError(_))
              .flatMap {
                case v @ Validated.Valid(_) =>
                  u.ref.set(UnknownState.Fixed(ks)).map(_ => v)
                case invalid => RefSpace.pure(invalid)
              }
        }

      def loop(
          s1: Shape,
          s2: Shape,
          visited0: Set[(Shape, Shape)]
      ): RefSpace[ValidatedNec[Error, Unit]] =
        if (visited0((s1, s2))) pureUnit
        else {
          val visited = visited0 + ((s1, s2))
          s1 match {
            case Type =>
              s2 match {
                case Type                         => pureUnit
                case Cons(_, _) | KnownCons(_, _) => error
                case u @ Unknown(_, _)            =>
                  unifyKnown(Type, u, visited)
              }
            case kc @ KnownCons(a, b) =>
              s2 match {
                case Type       => error
                case Cons(c, d) =>
                  unifyCons(a, b, c, d, visited)
                case KnownCons(c, d) =>
                  unifyCons(a, b, c, d, visited)
                case u @ Unknown(_, _) =>
                  unifyKnown(kc, u, visited)
              }
            case c @ Cons(a, b) =>
              s2 match {
                case Type       => error
                case Cons(c, d) =>
                  unifyCons(a, b, c, d, visited)
                case KnownCons(c, d) =>
                  unifyCons(a, b, c, d, visited)
                case Unknown(_, ref) =>
                  ref.get.flatMap {
                    case UnknownState.Free =>
                      ref.set(UnknownState.Linked(Set(c))).as(validUnit)
                    case UnknownState.Fixed(ks) =>
                      loop(ks, c, visited)
                    case UnknownState.Linked(us) =>
                      ref.set(UnknownState.Linked(us + c)).as(validUnit)
                  }
              }
            case u1 @ Unknown(_, ref1) =>
              s2 match {
                case ks: KnownShape =>
                  unifyKnown(ks, u1, visited)
                case cons @ Cons(_, _) =>
                  loop(cons, u1, visited)
                case u2 @ Unknown(_, ref2) =>
                  if (ref1 eq ref2) pureUnit
                  else
                    ref1.get.flatMap {
                      case UnknownState.Free =>
                        ref2.get.flatMap {
                          case UnknownState.Free =>
                            val set = ref1.set(UnknownState.Linked(Set(u2))) *>
                              ref2.set(UnknownState.Linked(Set(u1)))
                            set.as(validUnit)
                          case notFree =>
                            ref1.set(notFree).as(validUnit)
                        }
                      case UnknownState.Fixed(f1) =>
                        loop(f1, u2, visited)
                      case linked @ UnknownState.Linked(l1) =>
                        ref2.get.flatMap {
                          case UnknownState.Free =>
                            ref2.set(linked).as(validUnit)
                          case UnknownState.Fixed(f2) =>
                            loop(u1, f2, visited)
                          case UnknownState.Linked(l2) =>
                            val set = ref1.set(UnknownState.Linked(l1 + u2)) *>
                              ref2.set(UnknownState.Linked(l2 + u1))
                            set.as(validUnit)
                        }
                    }
              }
          }
        }

      loop(s1, s2, Set.empty)
    }

    def applyShape(
        source: Source,
        outer: rankn.Type,
        inner: rankn.Type.TyApply,
        s1: Shape,
        s2: Shape
    ): RefSpace[ValidatedNec[Error, Shape]] = {
      val mkErr = (s1: Shape, s2: Shape) =>
        UnificationError(typeDecl, source, s1, s2)
      s1 match {
        case Type =>
          RefSpace.pure(
            Validated.invalidNec(ShapeMismatch(typeDecl, source, outer, inner, s2))
          )
        case Cons(arg, res) =>
          unifyShape(arg, s2)(mkErr).map(_.as(res))
        case KnownCons(arg, res) =>
          unifyShape(arg, s2)(mkErr).map(_.as(res))
        case u1 @ Unknown(_, _) =>
          RefSpace
            .newRef(UnknownState.free)
            .flatMap { ref =>
              val u = Unknown(Left(inner), ref)
              unifyShape(u1, Cons(s2, u))(mkErr).map(_.as(u))
            }
      }
    }

    def shapeOfType(
        source: Source,
        scope: Env,
        initialLocal: Map[rankn.Type.Var.Bound, Shape],
        tpe: rankn.Type
    ): RefSpace[ValidatedNec[Error, Shape]] = {
      def loop(
          inner: rankn.Type,
          local: Map[rankn.Type.Var.Bound, Shape]
      ): RefSpace[ValidatedNec[Error, Shape]] =
        inner match {
          case rankn.Type.ForAll(vars, in) =>
            loop(
              in,
              local ++ vars.toList.map { case (b, k) => (b, shapeOf(k)) }
            )
          case rankn.Type.Exists(vars, in) =>
            loop(
              in,
              local ++ vars.toList.map { case (b, k) => (b, shapeOf(k)) }
            )
          case ta @ rankn.Type.TyApply(on, arg) =>
            for {
              v1 <- loop(on, local)
              v2 <- loop(arg, local)
              res <- (v1, v2).tupled match {
                case Validated.Valid((s1, s2)) =>
                  applyShape(source, tpe, ta, s1, s2)
                case Validated.Invalid(err) =>
                  RefSpace.pure(Validated.invalid(err))
              }
            } yield res
          case const @ rankn.Type.TyConst(c) =>
            scope(const) match {
              case Some(shape) =>
                RefSpace.pure(Validated.valid(shape))
              case None =>
                RefSpace.pure(
                  Validated.invalidNec(
                    UnknownConst(typeDecl, source, c)
                  )
                )
            }
          case rankn.Type.TyVar(rankn.Type.Var.Skolem(_, k, _, _)) =>
            RefSpace.pure(Validated.valid(shapeOf(k)))
          case tv @ rankn.Type.TyVar(v @ rankn.Type.Var.Bound(_)) =>
            local.get(v) match {
              case Some(s) =>
                RefSpace.pure(Validated.valid(s))
              case None =>
                scope(tv) match {
                  case Some(shape) =>
                    RefSpace.pure(Validated.valid(shape))
                  case None =>
                    RefSpace.pure(
                      Validated.invalidNec(
                        UnboundVar(typeDecl, source, v)
                      )
                    )
                }
            }
          case rankn.Type.TyMeta(rankn.Type.Meta(k, _, _, _)) =>
            RefSpace.pure(Validated.valid(shapeOf(k)))
        }

      loop(tpe, initialLocal)
    }

    def combineError[F[_]: cats.Foldable](
        errs: F[ValidatedNec[Error, Unit]]
    ): ValidatedNec[Error, Unit] =
      errs.foldLeft(Validated.valid(()): ValidatedNec[Error, Unit])(_ *> _)

    def constrainTypes(
        scope: Env,
        source: Source,
        binders: List[(rankn.Type.Var.Bound, Either[Shape, Kind.Arg])],
        tpes: List[rankn.Type]
    ): RefSpace[ValidatedNec[Error, Unit]] = {
      val local: Map[rankn.Type.Var.Bound, Shape] =
        binders.iterator.map {
          case (b, Left(s))   => (b, s)
          case (b, Right(ka)) => (b, shapeOf(ka.kind))
        }.toMap

      tpes
        .traverse { tpe => shapeOfType(source, scope, local, tpe) }
        .flatMap {
          _.sequence match {
            case Validated.Valid(shapeList) =>
              shapeList
                .traverse(unifyShape(_, Type) { (s1, s2) =>
                  UnificationError(typeDecl, source, s1, s2)
                })
                .map(combineError(_))
            case Validated.Invalid(errs) =>
              RefSpace.pure(Validated.Invalid(errs))
          }
        }
    }

    def shapeOrKnown(
        v: rankn.Type.Var.Bound,
        optK: Option[Kind.Arg]
    ): RefSpace[(rankn.Type.Var.Bound, Either[Shape, Kind.Arg])] =
      optK match {
        case None =>
          RefSpace
            .newRef(UnknownState.free)
            .map(ref => (v, Left(Unknown(Right(v), ref): Shape)))
        case Some(ka) =>
          RefSpace.pure((v, Right(ka)))
      }

    def knownParam(
        item: (rankn.Type.Var.Bound, Either[Shape, Kind.Arg])
    ): RefSpace[ValidatedNec[Error, (rankn.Type.Var.Bound, Either[KnownShape, Kind.Arg])]] =
      item match {
        case (v, Left(s))   =>
          shapeToKnown(s).map(_.map(k => (v, Left(k))))
        case (v, Right(ka)) =>
          RefSpace.pure(Validated.valid((v, Right(ka))))
      }
  }

  def solveShape[E: IsShapeEnv](
      imports: E,
      dt: DefinedType[Option[Kind.Arg]]
  ): ValidatedNec[Error, DefinedType[Either[KnownShape, Kind.Arg]]] = {
    val solver = new Solver(imports, dt, allowSelfReference = true)
    val rf = for {
      topShapes <- dt.annotatedTypeParams.traverse { case (v, optK) =>
        solver.shapeOrKnown(v, optK)
      }
      consShapes <- dt.constructors.traverse { cfn =>
        cfn.exists
          .traverse { case (v, optK) =>
            solver.shapeOrKnown(v, optK)
          }
          .map(exists => cfn.copy(exists = exists))
      }
      dtShapes = dt.copy(
        annotatedTypeParams = topShapes,
        constructors = consShapes
      )
      scope = solver.scopeFor(dtShapes.annotatedTypeParams)
      checks <- dtShapes.constructors.traverse { cfn =>
        solver.constrainTypes(
          scope,
          Source.ConstructorFn(cfn),
          cfn.exists,
          cfn.args.map(_.tpe)
        )
      }
      check = solver.combineError(checks)
      topKnowns <- dtShapes.annotatedTypeParams.traverse {
        solver.knownParam(_)
      }
      consKnowns <- dtShapes.constructors.traverse { cfn =>
        cfn.exists
          .traverse(solver.knownParam(_))
          .map(_.sequence.map(ex => cfn.copy(exists = ex)))
      }
    } yield {
      val topKnown = topKnowns.sequence
      val consKnown = consKnowns.sequence
      (check, topKnown, consKnown).mapN { (_, tparams, constructors) =>
        dt.copy(annotatedTypeParams = tparams, constructors = constructors)
      }
    }

    rf.run.value
  }

  def solveAlias[E: IsShapeEnv](
      imports: E,
      alias: TypeAlias[Option[Kind.Arg]]
  ): ValidatedNec[Error, TypeAlias[Either[KnownShape, Kind.Arg]]] = {
    val solver = new Solver(imports, alias, allowSelfReference = false)
    val rf = for {
      topShapes <- alias.annotatedTypeParams.traverse { case (v, optK) =>
        solver.shapeOrKnown(v, optK)
      }
      scope = solver.scopeFor(topShapes)
      check <- solver.constrainTypes(
        scope,
        Source.AliasBody,
        Nil,
        alias.rhs :: Nil
      )
      topKnowns <- topShapes.traverse(solver.knownParam(_))
    } yield {
      (check, topKnowns.sequence).mapN { (_, tparams) =>
        alias.copy(annotatedTypeParams = tparams)
      }
    }

    rf.run.value
  }
}
