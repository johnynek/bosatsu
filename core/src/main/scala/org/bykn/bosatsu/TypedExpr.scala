package org.bykn.bosatsu

import cats.arrow.FunctionK
import cats.implicits._
import cats.evidence.Is
import cats.data.NonEmptyList
import cats.{Applicative, Eval, Traverse}

sealed abstract class TypedExpr[T] {
  import TypedExpr._

  def tag: T
  // def setTag(t: T): TypedExpr[T] =
  //   this match {
  //     case tl@TyLam(_, _, _) => tl.copy(tag = t)
  //     case ta@TyApp(_, _, _) => ta.copy(tag = t)
  //     case v@Var(_, _, _) => v.copy(tag = t)
  //     case a@App(_, _, _) => a.copy(tag = t)
  //     case a@Annotation(_, _, _) => a.copy(tag = t)
  //     case a@AnnotatedLambda(_, _, _, _) => a.copy(tag = t)
  //     case l@Let(_, _, _, _) => l.copy(tag = t)
  //     case l@Literal(_, _, _) => l.copy(tag = t)
  //     case i@If(_, _, _, _) => i.copy(tag = t)
  //     case m@Match(_, _, _) => m.copy(tag = t)
  //   }

  /**
   * For any well typed expression, i.e.
   * one that has already gone through type
   * inference, we should be able to get a type
   * for each expression
   *
   */
  def getType: rankn.Type =
    this match {
      // case TyLam(v, e, _) =>
      //   val bound = rankn.Type.Var.Bound(v)
      //   e.getType match {
      //     case rankn.Type.ForAll(vars, in) =>
      //       rankn.Type.ForAll(bound :: vars, in)
      //     case notForAll =>
      //       rankn.Type.ForAll(NonEmptyList.of(bound), notForAll)
      //   }
      // case TyApp(e, tpe, _) =>
      //   ???
      case Annotation(t, _) =>
        // ignore the original annotation and return
        // the type from the underlying thing, but this might be wrong
        t.getType
      case a@AnnotatedLambda(arg, tpe, res, _) =>
        rankn.Type.Fun(tpe, res.getType)
      case Var(_, tpe, _) => tpe
      case App(fn, arg, _) =>
        /**
         * this is a bit tricky
         * if we have fn: forall a. a -> Foo[a]
         * then fn(1): Foo[Int].
         */
        ???
      case Let(_, _, in, _) =>
        in.getType
      case Literal(_, tpe, _) =>
        tpe
      case If(_, ift, _, _) =>
        // all branches have the same type:
        ift.getType
      case Match(_, branches, _) =>
        // all branches have the same type:
        // TODO: this is probably not right
        // since the patterns set an environment
        // of types
        //branches.head._2.getType(env)
        ???
    }

  def typeTraverse[F[_]: Applicative](fn: rankn.Type => F[rankn.Type]): F[TypedExpr[T]] =
    ???
}

object TypedExpr {

  type Rho[A] = TypedExpr[A] // an expression with a Rho type (no top level forall)
  // /**
  //  * This says that the resulting term is generic on a given param
  //  */
  // case class TyLam[T](varType: String, in: TypedExpr[T], tag: T) extends TypedExpr[T]
  // /**
  //  * This says that we have a generic type, and we are applying a specfic Tau (no
  //  * forall anywhere) into the type
  //  */
  // case class TyApp[T](generic: TypedExpr[T], typeArg: rankn.Type.Tau, tag: T) extends TypedExpr[T]
  case class Annotation[T](term: TypedExpr[T], tag: T) extends TypedExpr[T]
  case class AnnotatedLambda[T](arg: String, tpe: rankn.Type, expr: TypedExpr[T], tag: T) extends TypedExpr[T]
  case class Var[T](name: String, tpe: rankn.Type, tag: T) extends TypedExpr[T]
  case class App[T](fn: TypedExpr[T], arg: TypedExpr[T], tag: T) extends TypedExpr[T]
  case class Let[T](arg: String, expr: TypedExpr[T], in: TypedExpr[T], tag: T) extends TypedExpr[T]
  case class Literal[T](lit: Lit, tpe: rankn.Type, tag: T) extends TypedExpr[T]
  case class If[T](cond: TypedExpr[T], ifTrue: TypedExpr[T], ifFalse: TypedExpr[T], tag: T) extends TypedExpr[T]
  case class Match[T](arg: TypedExpr[T], branches: NonEmptyList[(Pattern[(PackageName, ConstructorName), rankn.Type], TypedExpr[T])], tag: T) extends TypedExpr[T]


  type Coerce = FunctionK[TypedExpr, TypedExpr]
  def coerceRho(tpe: rankn.Type.Rho): Coerce = ???

  def coerceFn(arg: rankn.Type.Rho, coarg: Coerce, cores: Coerce): Coerce = ???

  def forAll[A](params: NonEmptyList[rankn.Type.Var], expr: TypedExpr[A]): TypedExpr[A] = ???

  implicit def typedExprHasRegion[T: HasRegion]: HasRegion[TypedExpr[T]] =
    HasRegion.instance[TypedExpr[T]] { e => HasRegion.region(e.tag) }

  /**
   * Return a value so next(e).tag == e and also this is true
   * recursively
   */
  // def nest[T](e: TypedExpr[T]): TypedExpr[TypedExpr[T]] =
  //   e match {
  //     case TyLam(v, te, _) =>
  //       TyLam(v, nest(te), e)
  //     case TyApp(te, tpe, _) =>
  //       TyApp(nest(te), tpe, e)
  //     case AnnotatedLambda(arg, tpe, expr, _) =>
  //       AnnotatedLambda(arg, tpe, nest(expr), e)
  //     case Var(s, tpe, _) =>
  //       Var(s, tpe, e)
  //     case App(fn, a, _) =>
  //       App(nest(fn), nest(a), e)
  //     case Let(arg, exp, in, _) =>
  //       Let(arg, nest(exp), nest(in), e)
  //     case Literal(lit, tpe, _) =>
  //       Literal(lit, tep, e)
  //     case If(cond, ift, iff, _) =>
  //       If(nest(cond), nest(ift), nest(iff), e)
  //     case Match(arg, branches, _) =>
  //       Match(nest(arg), branches.map {
  //         case (pat, exp) => (pat, nest(exp))
  //       }, e)
  //   }

//   implicit val typedExprTraverse: Traverse[TypedExpr] =
//     new Traverse[TypedExpr] {

//       // Traverse on NonEmptyList[(Pattern[_], TypedExpr[?])]
//       private lazy val tne = {
//         type Tup[T] = (Pattern[(PackageName, ConstructorName), rankn.Type], T)
//         type TupTypedExpr[T] = (Pattern[(PackageName, ConstructorName), rankn.Type], TypedExpr[T])
//         val tup: Traverse[TupTypedExpr] = Traverse[Tup].compose(typedExprTraverse)
//         Traverse[NonEmptyList].compose(tup)
//       }

//       def traverse[G[_]: Applicative, A, B](fa: TypedExpr[A])(f: A => G[B]): G[TypedExpr[B]] =
//         fa match {
//           case TyLam(v, e, a) =>
//             (e.traverse(f), f(a)).mapN(TyLam(v, _, _))
//           case TyApp(e, tpe, a) =>
//             (e.traverse(f), f(a)).mapN(TyApp(_, tpe, _))
//           case AnnotatedLambda(arg, tpe, expr, a) =>
//             (expr.traverse(f), f(a)).mapN(AnnotatedLambda(arg, tpe, _, _))
//           case Var(s, tpe, t) =>
//             f(t).map(Var(s, tpe, _))
//           case App(fn, a, t) =>
//             (fn.traverse(f), a.traverse(f), f(t)).mapN { (fn1, a1, b) =>
//               App(fn1, a1, b)
//             }
//           case Let(arg, exp, in, tag) =>
//             (exp.traverse(f), in.traverse(f), f(tag)).mapN { (e1, i1, t1) =>
//               Let(arg, e1, i1, t1)
//             }
//           case Literal(lit, tag) =>
//             f(tag).map(Literal(lit, _))
//           case If(cond, ift, iff, tag) =>
//             (cond.traverse(f), ift.traverse(f), iff.traverse(f), f(tag))
//               .mapN(If(_, _, _, _))
//           case Match(arg, branches, tag) =>
//             val argB = arg.traverse(f)
//             val branchB = tne.traverse(branches)(f)
//             (argB, branchB, f(tag)).mapN { (a, bs, t) =>
//               Match(a, bs, t)
//             }
//         }

//       def foldLeft[A, B](fa: TypedExpr[A], b: B)(f: (B, A) => B): B =
//         fa match {
//           case TyLam(_, e, tag) =>
//             val b1 = foldLeft(e, b)(f)
//             f(b1, tag)
//           case TyApp(e, _, tag) =>
//             val b1 = foldLeft(e, b)(f)
//             f(b1, tag)
//           case AnnotatedLambda(_, _, e, tag) =>
//             val b1 = foldLeft(e, b)(f)
//             f(b1, tag)
//           case Var(_, _, tag) => f(b, tag)
//           case App(fn, a, tag) =>
//             val b1 = foldLeft(fn, b)(f)
//             val b2 = foldLeft(a, b1)(f)
//             f(b2, tag)
//           case Let(_, exp, in, tag) =>
//             val b1 = foldLeft(exp, b)(f)
//             val b2 = foldLeft(in, b1)(f)
//             f(b2, tag)
//           case Literal(_, tag) =>
//             f(b, tag)
//           case If(cond, ift, iff, tag) =>
//             val b1 = foldLeft(cond, b)(f)
//             val b2 = foldLeft(ift, b1)(f)
//             val b3 = foldLeft(iff, b2)(f)
//             f(b3, tag)
//           case Match(arg, branches, tag) =>
//             val b1 = foldLeft(arg, b)(f)
//             val b2 = tne.foldLeft(branches, b1)(f)
//             f(b2, tag)
//         }

//       def foldRight[A, B](fa: TypedExpr[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
//         fa match {
//           case TyLam(_, e, tag) =>
//             val lb1 = foldRight(e, lb)(f)
//             f(tag, lb1)
//           case TyApp(e, _, tag) =>
//             val lb1 = foldRight(e, lb)(f)
//             f(tag, lb1)
//           case AnnotatedLambda(_, _, e, tag) =>
//             val lb1 = foldRight(e, lb)(f)
//             f(tag, lb1)
//           case Var(_, _, tag) => f(tag, lb)
//           case App(fn, a, tag) =>
//             val b1 = f(tag, lb)
//             val b2 = foldRight(a, b1)(f)
//             foldRight(fn, b2)(f)
//           case Let(_, exp, in, tag) =>
//             val b1 = f(tag, lb)
//             val b2 = foldRight(in, b1)(f)
//             foldRight(exp, b2)(f)
//           case Literal(_, tag) =>
//             f(tag, lb)
//           case If(cond, ift, iff, tag) =>
//             val b1 = f(tag, lb)
//             val b2 = foldRight(iff, b1)(f)
//             val b3 = foldRight(ift, b2)(f)
//             foldRight(cond, b3)(f)
//           case Match(arg, branches, tag) =>
//             val b1 = f(tag, lb)
//             val b2 = tne.foldRight(branches, b1)(f)
//             foldRight(arg, b2)(f)
//         }
//     }
}
