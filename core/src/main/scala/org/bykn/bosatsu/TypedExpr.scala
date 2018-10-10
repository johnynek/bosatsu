package org.bykn.bosatsu

import cats.Applicative
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.implicits._

sealed abstract class TypedExpr[T] {
  import TypedExpr._

  def tag: T
  /**
   * For any well typed expression, i.e.
   * one that has already gone through type
   * inference, we should be able to get a type
   * for each expression
   *
   */
  def getType: rankn.Type =
    this match {
      case Generic(params, expr, _) =>
        rankn.Type.forAll(params.toList, expr.getType)
      case Annotation(_, tpe, _) =>
        tpe
      case a@AnnotatedLambda(arg, tpe, res, _) =>
        rankn.Type.Fun(tpe, res.getType)
      case Var(_, tpe, _) => tpe
      case App(_, _, tpe, _) => tpe
      case Let(_, _, in, _) =>
        in.getType
      case Literal(_, tpe, _) =>
        tpe
      case If(_, ift, _, _) =>
        // all branches have the same type:
        ift.getType
      case Match(_, branches, _) =>
        // all branches have the same type:
        branches.head._2.getType
    }

  def traverseType[F[_]: Applicative](fn: rankn.Type => F[rankn.Type]): F[TypedExpr[T]] =
    this match {
      case Generic(params, expr, tag) =>
        // The parameters are are like strings, but this
        // is a bit unsafe... we only use it for zonk which
        // ignores Bounds
        expr.traverseType(fn).map(Generic(params, _, tag))
      case Annotation(of, tpe, tag) =>
        (of.traverseType(fn), fn(tpe)).mapN(Annotation(_, _, tag))
      case AnnotatedLambda(arg, tpe, res, tag) =>
        (fn(tpe), res.traverseType(fn)).mapN {
          AnnotatedLambda(arg, _, _, tag)
        }
      case Var(v, tpe, tag) =>
        fn(tpe).map(Var(v, _, tag))
      case App(f, arg, tpe, tag) =>
        (f.traverseType(fn), arg.traverseType(fn), fn(tpe)).mapN {
          App(_, _, _, tag)
        }
      case Let(v, exp, in, tag) =>
        (exp.traverseType(fn), in.traverseType(fn)).mapN {
          Let(v, _, _, tag)
        }
      case Literal(lit, tpe, tag) =>
        fn(tpe).map(Literal(lit, _, tag))
      case If(cond, ift, iff, tag) =>
        // all branches have the same type:
        (cond.traverseType(fn),
          ift.traverseType(fn),
          iff.traverseType(fn)).mapN(If(_, _, _, tag))
      case Match(expr, branches, tag) =>
        // all branches have the same type:
        val tbranch = branches.traverse {
          case (p, t) =>
            p.traverseType(fn).product(t.traverseType(fn))
        }
        (expr.traverseType(fn), tbranch).mapN(Match(_, _, tag))
    }
}

object TypedExpr {

  type Rho[A] = TypedExpr[A] // an expression with a Rho type (no top level forall)
  /**
   * This says that the resulting term is generic on a given param
   *
   * The paper says to add TyLam and TyApp nodes, but it never mentions what to do with them
   */
  case class Generic[T](typeVars: NonEmptyList[rankn.Type.Var.Bound], in: TypedExpr[T], tag: T) extends TypedExpr[T]
  case class Annotation[T](term: TypedExpr[T], coerce: rankn.Type, tag: T) extends TypedExpr[T]
  case class AnnotatedLambda[T](arg: String, tpe: rankn.Type, expr: TypedExpr[T], tag: T) extends TypedExpr[T]
  case class Var[T](name: String, tpe: rankn.Type, tag: T) extends TypedExpr[T]
  case class App[T](fn: TypedExpr[T], arg: TypedExpr[T], result: rankn.Type, tag: T) extends TypedExpr[T]
  case class Let[T](arg: String, expr: TypedExpr[T], in: TypedExpr[T], tag: T) extends TypedExpr[T]
  case class Literal[T](lit: Lit, tpe: rankn.Type, tag: T) extends TypedExpr[T]
  case class If[T](cond: TypedExpr[T], ifTrue: TypedExpr[T], ifFalse: TypedExpr[T], tag: T) extends TypedExpr[T]
  case class Match[T](arg: TypedExpr[T], branches: NonEmptyList[(Pattern[(PackageName, ConstructorName), rankn.Type], TypedExpr[T])], tag: T) extends TypedExpr[T]


  type Coerce = FunctionK[TypedExpr, TypedExpr]
  def coerceRho(tpe: rankn.Type.Rho): Coerce =
    new FunctionK[TypedExpr, TypedExpr] { self =>
      def apply[A](expr: TypedExpr[A]) =
        expr match {
          case Generic(params, expr, tag) =>
            // This definitely feels wrong,
            // but without this, I don't see what else we can do
            Generic(params, self(expr), tag)
          case Annotation(t, _, tag) => Annotation(self(t), tpe, tag)
          case Var(name, _, t) => Var(name, tpe, t)
          case AnnotatedLambda(arg, argT, res, tag) =>
            // only some coercions would make sense here
            // how to handle?
            // one way out could be to return a type to Annotation
            // and just wrap it in this case, could it be that simple?
            Annotation(expr, tpe, expr.tag)
          case App(fn, arg, _, tag) =>
            // do we need to coerce fn into the right shape?
            App(fn, arg, tpe, tag)
          case Let(arg, argE, in, tag) =>
            Let(arg, argE, self(in), tag)
          case Literal(l, _, tag) => Literal(l, tpe, tag)
          case If(c, ift, iff, tag) =>
            If(c, self(ift), self(iff), tag)
          case Match(arg, branches, tag) =>
            Match(arg, branches.map { case (p, expr) => (p, self(expr)) }, tag)
        }
    }

  def coerceFn(arg: rankn.Type, result: rankn.Type.Rho, coarg: Coerce, cores: Coerce): Coerce =
    new FunctionK[TypedExpr, TypedExpr] { self =>
      def apply[A](expr: TypedExpr[A]) =
        AnnotatedLambda("x", arg, cores(App(expr, coarg(Var("x", arg, expr.tag)), result, expr.tag)), expr.tag)
    }


  def forAll[A](params: NonEmptyList[rankn.Type.Var.Bound], expr: TypedExpr[A]): TypedExpr[A] =
    Generic(params, expr, expr.tag)

  implicit def typedExprHasRegion[T: HasRegion]: HasRegion[TypedExpr[T]] =
    HasRegion.instance[TypedExpr[T]] { e => HasRegion.region(e.tag) }
}
