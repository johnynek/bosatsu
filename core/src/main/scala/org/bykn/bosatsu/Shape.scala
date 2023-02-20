package org.bykn.bosatsu

import cats.Foldable
import cats.data.{Validated, ValidatedNec, Ior, IorNec}
import org.bykn.bosatsu.rankn.{
  ConstructorFn,
  DefinedType,
  Type,
  TypeEnv,
  Ref,
  RefSpace
}
import org.typelevel.paiges.Doc

import cats.syntax.all._

// Shape is Kind without variance, and variables
sealed abstract class Shape

object Shape {
  sealed abstract class KnownShape extends Shape

  case object Type extends KnownShape

  def cons(arg: Shape, res: Shape): Shape =
    (arg, res) match {
      case (ksa: KnownShape, kres: KnownShape) => KnownCons(ksa, kres)
      case _                                   => Cons(arg, res)
    }

  case class KnownCons(arg: KnownShape, res: KnownShape) extends KnownShape

  case class Cons(arg: Shape, res: Shape) extends Shape {
    require(
      !(arg.isInstanceOf[KnownShape] && res.isInstanceOf[KnownShape]),
      s"cons must have one unknown: $this"
    )
  }
  case class Unknown(
      bound: Either[rankn.Type.TyApply, rankn.Type.Var.Bound],
      ref: Ref[Option[Shape]]
  ) extends Shape

  def shapeDoc(s: Shape): Doc =
    s match {
      case Type           => Doc.char('*')
      case Cons(Type, s2) => Doc.text("* -> ") + shapeDoc(s2)
      case Cons(s1, s2) =>
        Doc.char('(') + shapeDoc(s1) + Doc.char(')') + Doc.text(
          " -> "
        ) + shapeDoc(s2)
      case KnownCons(Type, s2) => Doc.text("* -> ") + shapeDoc(s2)
      case KnownCons(s1, s2) =>
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

  sealed abstract class Error
  case class ShapeLoop(
      dt: DefinedType[Option[Kind.Arg]],
      bound: Either[rankn.Type.TyApply, rankn.Type.Var.Bound],
      path: Set[Shape]
  ) extends Error

  case class UnificationError(
      dt: DefinedType[Option[Kind.Arg]],
      cfn: ConstructorFn,
      left: Shape,
      right: Shape
  ) extends Error

  case class UnknownConst(
      dt: DefinedType[Option[Kind.Arg]],
      cfn: ConstructorFn,
      const: rankn.Type.Const
  ) extends Error

  case class UnboundVar(
      dt: DefinedType[Option[Kind.Arg]],
      cfn: ConstructorFn,
      bound: rankn.Type.Var.Bound
  ) extends Error

  case class ShapeMismatch(
      dt: DefinedType[Option[Kind.Arg]],
      cfn: ConstructorFn,
      outer: rankn.Type,
      tapply: rankn.Type.TyApply,
      leftShape: Shape,
      rightShape: Shape
  ) extends Error

  def shapeOf(k: Kind): KnownShape =
    k match {
      case Kind.Type => Type
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

    implicit def typeEnvIsShapeEnv[S: ShapeOf]: IsShapeEnv[TypeEnv[S]] =
      new IsShapeEnv[TypeEnv[S]] {
        def getShape(
            env: TypeEnv[S],
            tc: rankn.Type.Const
        ): Option[KnownShape] =
          env.toDefinedType(tc).map { dt => ShapeOf(dt) }
      }

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
          if (dt.toTypeConst == tc) Some(ShapeOf(dt))
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
        def getShape(env: Unit, tc: rankn.Type.Const) = None
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

  def solveShape[E: IsShapeEnv](
      imports: E,
      dt: DefinedType[Option[Kind.Arg]]
  ): ValidatedNec[Error, DefinedType[Either[KnownShape, Kind.Arg]]] = {

    type Env = Type => Option[Shape]

    val cache = scala.collection.mutable.Map.empty[Type, Option[Shape]]

    def thisScope(
        lst: List[(rankn.Type.Var.Bound, Either[Shape, Kind.Arg])]
    ): Env = {
      val smap: Map[rankn.Type.Var.Bound, Shape] =
        lst.iterator.map {
          case (v, Right(ka)) => (v, ShapeOf(ka))
          case (v, Left(s))   => (v, s)
        }.toMap

      val thisShape: Shape =
        lst.foldRight(Shape.Type: Shape) {
          case ((_, Right(k)), res) =>
            Shape.cons(ShapeOf(k), res)
          case ((_, Left(s)), res) =>
            Shape.Cons(s, res)
        }

      { (t: Type) =>
        cache.getOrElseUpdate(
          t, {
            t match {
              case rankn.Type.TyVar(v @ rankn.Type.Var.Bound(_)) => smap.get(v)
              case rankn.Type.TyConst(const) =>
                if (const == dt.toTypeConst) Some(thisShape)
                else IsShapeEnv[E].getShape(imports, const)
              case _ =>
                // nothing else is in-scope
                None
            }
          }
        )
      }
    }

    def shapeToKnown(s: Shape): RefSpace[ValidatedNec[Error, KnownShape]] = {
      def toKnown(
          s: Shape,
          path: Set[Shape],
          allUK: Boolean
      ): RefSpace[ValidatedNec[Error, KnownShape]] =
        s match {
          case ks: KnownShape => RefSpace.pure(Validated.valid(ks))
          case Cons(s1, s2) =>
            for {
              k1 <- toKnown(s1, path + s, false)
              k2 <- toKnown(s2, path + s, false)
            } yield (k1, k2).mapN(KnownCons(_, _))
          case u @ Unknown(b, ref) =>
            @inline def finish: RefSpace[ValidatedNec[Error, KnownShape]] =
              ref.set(Some(Type)).as(Validated.valid(Type))

            ref.get.flatMap {
              case None =>
                // this is an unconstrained shape, so we assume Type because we don't have Kind polymorphism
                finish
              case Some(s) =>
                val nextPath = path + u
                if (nextPath(s)) {
                  // we have a loop
                  if (allUK) {
                    // we have already reached this node, so it is unconstrained, all these types are *
                    finish
                  } else {
                    // we have a fixed point kind
                    RefSpace.pure(
                      Validated.invalidNec(ShapeLoop(dt, b, nextPath))
                    )
                  }
                } else {
                  toKnown(s, nextPath, allUK).flatMap {
                    case v @ Validated.Valid(ks) =>
                      ref.set(Some(ks)).as(v)
                    case invalid => RefSpace.pure(invalid)
                  }
                }
            }
        }

      toKnown(s, Set.empty, true)
    }

    val validUnit = Validated.valid(())
    val pureUnit = RefSpace.pure(validUnit)

    def unifyCons(
        cfn: ConstructorFn,
        a: Shape,
        b: Shape,
        c: Shape,
        d: Shape
    ): RefSpace[ValidatedNec[Error, Unit]] =
      (unifyShape(cfn, a, c), unifyShape(cfn, b, d)).mapN(_ *> _)

    def unifyShape(
        cfn: ConstructorFn,
        s1: Shape,
        s2: Shape
    ): RefSpace[ValidatedNec[Error, Unit]] = {
      @inline def error =
        RefSpace.pure(Validated.invalidNec(UnificationError(dt, cfn, s1, s2)))
      s1 match {
        case Type =>
          s2 match {
            case Type            => pureUnit
            case Cons(_, _)      => error
            case KnownCons(_, _) => error
            case Unknown(_, ref) =>
              ref.get.flatMap {
                case None =>
                  // we can update
                  ref.set(Some(Type)).as(validUnit)
                case Some(s) =>
                  unifyShape(cfn, s1, s)
              }
          }
        case Cons(a, b) =>
          s2 match {
            case Type => error
            case Cons(c, d) =>
              unifyCons(cfn, a, b, c, d)
            case KnownCons(c, d) =>
              unifyCons(cfn, a, b, c, d)
            case Unknown(_, ref) =>
              ref.get.flatMap {
                case None =>
                  // we can update
                  ref.set(Some(s1)).as(validUnit)
                case Some(s) =>
                  unifyShape(cfn, s1, s)
              }
          }
        case KnownCons(a, b) =>
          s2 match {
            case Type => error
            case Cons(c, d) =>
              unifyCons(cfn, a, b, c, d)
            case KnownCons(c, d) =>
              unifyCons(cfn, a, b, c, d)
            case Unknown(_, ref) =>
              ref.get.flatMap {
                case None =>
                  // we can update
                  ref.set(Some(s1)).as(validUnit)
                case Some(s) =>
                  unifyShape(cfn, s1, s)
              }
          }
        case Unknown(_, ref1) =>
          s2 match {
            case Unknown(_, ref2) =>
              if (ref1 == ref2) pureUnit
              else
                (ref1.get, ref2.get).flatMapN {
                  case (None, None) =>
                    // both are unset, link them
                    // maybe we just link the one with the smaller bound? idk
                    ref1.set(Some(s2)) *> ref2.set(Some(s1)).as(validUnit)
                  case (someA, None) =>
                    ref2.set(someA).as(validUnit)
                  case (None, someB) =>
                    ref1.set(someB).as(validUnit)
                  case (Some(a), Some(b)) =>
                    unifyShape(cfn, a, b)
                }
            case _ =>
              ref1.get.flatMap {
                case None =>
                  // we can update
                  ref1.set(Some(s2)).as(validUnit)
                case Some(s1) =>
                  unifyShape(cfn, s1, s2)
              }
          }
      }
    }

    // we can only apply s1(s2) if s1 is (k1 -> k2)k1
    def applyShape(
        cfn: ConstructorFn,
        outer: rankn.Type,
        inner: rankn.Type.TyApply,
        s1: Shape,
        s2: Shape
    ): RefSpace[ValidatedNec[Error, Shape]] =
      s1 match {
        case Type =>
          // kind mismatch
          RefSpace.pure(
            Validated.invalidNec(ShapeMismatch(dt, cfn, outer, inner, s1, s2))
          )
        case Cons(arg, res) =>
          // s1 == (arg -> res)
          // s1(s2) => res
          unifyShape(cfn, arg, s2).map(_.as(res))
        case KnownCons(arg, res) =>
          // s1 == (arg -> res)
          // s1(s2) => res
          unifyShape(cfn, arg, s2).map(_.as(res))
        case u1 @ Unknown(_, _) =>
          // we allocate a new unknown u, and unify s1 with s2 -> u and return u
          RefSpace
            .newRef(Option.empty[Shape])
            .flatMap { ref =>
              val u = Unknown(Left(inner), ref)
              unifyShape(cfn, u1, Cons(s2, u)).map(_.as(u))
            }
      }

    def shapeOfType(
        cfn: ConstructorFn,
        scope: Env,
        tpe: rankn.Type
    ): RefSpace[ValidatedNec[Error, Shape]] = {
      def loop(
          inner: rankn.Type,
          local: Map[rankn.Type.Var.Bound, Kind]
      ): RefSpace[ValidatedNec[Error, Shape]] =
        inner match {
          case rankn.Type.ForAll(vars, t) =>
            loop(t, local ++ vars.toList)
          case ta @ rankn.Type.TyApply(on, arg) =>
            for {
              v1 <- loop(on, local)
              v2 <- loop(arg, local)
              res <- (v1, v2).tupled match {
                case Validated.Valid((s1, s2)) =>
                  applyShape(cfn, tpe, ta, s1, s2)
                case Validated.Invalid(err) =>
                  RefSpace.pure(Validated.invalid(err))
              }
            } yield res
          case const @ rankn.Type.TyConst(c) =>
            scope(const) match {
              case Some(shape) =>
                RefSpace.pure(Validated.valid(shape))
              case None =>
                // Unknown type name
                RefSpace.pure(
                  Validated.invalidNec(
                    UnknownConst(dt, cfn, c)
                  )
                )
            }
          case rankn.Type.TyVar(rankn.Type.Var.Skolem(_, k, _)) =>
            RefSpace.pure(Validated.valid(shapeOf(k)))
          case tv @ rankn.Type.TyVar(v @ rankn.Type.Var.Bound(_)) =>
            local.get(v) match {
              case Some(k) =>
                RefSpace.pure(Validated.valid(shapeOf(k)))
              case None =>
                scope(tv) match {
                  case Some(shape) =>
                    RefSpace.pure(Validated.valid(shape))
                  case None =>
                    // Unbound var
                    RefSpace.pure(
                      Validated.invalidNec(
                        UnboundVar(dt, cfn, v)
                      )
                    )
                }
            }
          case rankn.Type.TyMeta(rankn.Type.Meta(k, _, _)) =>
            RefSpace.pure(Validated.valid(shapeOf(k)))
        }

      loop(tpe, Map.empty)
    }

    def combineError[F[_]: cats.Foldable](
        errs: F[ValidatedNec[Error, Unit]]
    ): ValidatedNec[Error, Unit] =
      errs.foldLeft(Validated.valid(()): ValidatedNec[Error, Unit])(_ *> _)

    def constrainFn(
        scope: Env,
        cfn: ConstructorFn
    ): RefSpace[ValidatedNec[Error, Unit]] =
      cfn.args
        .traverse { case (_, tpe) => shapeOfType(cfn, scope, tpe) }
        .flatMap {
          _.sequence match {
            case Validated.Valid(shapeList) =>
              // all the args to a function arg kind Type
              shapeList
                .traverse(unifyShape(cfn, _, Type))
                .map(combineError(_))
            case Validated.Invalid(errs) =>
              // we are changing the right type here from List[Shape] to Unit
              RefSpace.pure(Validated.Invalid(errs))
          }
        }

    def constrainAll(scope: Env): RefSpace[ValidatedNec[Error, Unit]] =
      dt.constructors
        .traverse(constrainFn(scope, _))
        .map(combineError[List](_))

    val rf = for {
      shapes <- dt.annotatedTypeParams.traverse {
        case (v, None) =>
          RefSpace
            .newRef(Option.empty[Shape])
            .map { ref => (v, Left(Unknown(Right(v), ref): Shape)) }
        case (v, Some(ka)) =>
          RefSpace.pure((v, Right(ka)))
      }
      check <- constrainAll(thisScope(shapes))
      params1: List[ValidatedNec[
        Error,
        (rankn.Type.Var.Bound, Either[KnownShape, Kind.Arg])
      ]] <-
        shapes.traverse {
          case (v, Left(s)) =>
            shapeToKnown(s)
              .map { k => k.map { s => (v, Left(s)) } }
          case (v, Right(ka)) =>
            RefSpace.pure(Validated.valid((v, Right(ka))))
        }
    } yield check *> params1.sequence.map { p =>
      dt.copy(annotatedTypeParams = p)
    }

    rf.run.value
  }
}
