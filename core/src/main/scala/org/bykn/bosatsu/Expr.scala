package org.bykn.bosatsu

/**
 * This is a scala port of the example of Hindley Milner inference
 * here: http://dev.stephendiehl.com/fun/006_hindley_milner.html
 */

import cats.implicits._
import cats.data.NonEmptyList
import cats.{Applicative, Eval, Traverse}

import Identifier.{Bindable, Constructor}

sealed abstract class Expr[T] {
  def tag: T
}

object Expr {
  case class Annotation[T](expr: Expr[T], tpe: rankn.Type, tag: T) extends Expr[T]
  case class AnnotatedLambda[T](arg: Bindable, tpe: rankn.Type, expr: Expr[T], tag: T) extends Expr[T]
  case class Var[T](pack: Option[PackageName], name: Identifier, tag: T) extends Expr[T]
  case class App[T](fn: Expr[T], arg: Expr[T], tag: T) extends Expr[T]
  case class Lambda[T](arg: Bindable, expr: Expr[T], tag: T) extends Expr[T]
  case class Let[T](arg: Bindable, expr: Expr[T], in: Expr[T], recursive: RecursionKind, tag: T) extends Expr[T]
  case class Literal[T](lit: Lit, tag: T) extends Expr[T]
  case class Match[T](arg: Expr[T], branches: NonEmptyList[(Pattern[(PackageName, Constructor), rankn.Type], Expr[T])], tag: T) extends Expr[T]

  implicit def hasRegion[T: HasRegion]: HasRegion[Expr[T]] =
    HasRegion.instance[Expr[T]] { e => HasRegion.region(e.tag) }

  /*
   * Allocate these once
   */
  private[this] val TruePat: Pattern[(PackageName, Constructor), rankn.Type] =
    Pattern.PositionalStruct((Predef.packageName, Constructor("True")), Nil)
  private[this] val FalsePat: Pattern[(PackageName, Constructor), rankn.Type] =
    Pattern.PositionalStruct((Predef.packageName, Constructor("False")), Nil)
  /**
   * build a Match expression that is equivalent to if/else using Predef::True and Predef::False
   */
  def ifExpr[T](cond: Expr[T], ifTrue: Expr[T], ifFalse: Expr[T], tag: T): Expr[T] =
    Match(cond, NonEmptyList.of((TruePat, ifTrue), (FalsePat, ifFalse)), tag)

  /**
   * Build an apply expression by appling these args left to right
   */
  @annotation.tailrec
  def buildApp[A](fn: Expr[A], args: List[Expr[A]], appTag: A): Expr[A] =
    args match {
      case Nil => fn
      case h :: tail =>
        buildApp(App(fn, h, appTag), tail, appTag)
    }

  def traverseType[T, F[_]](expr: Expr[T], fn: rankn.Type => F[rankn.Type])(implicit F: Applicative[F]): F[Expr[T]] =
    expr match {
      case Annotation(e, tpe, a) =>
        (traverseType(e, fn), fn(tpe)).mapN(Annotation(_, _, a))
      case AnnotatedLambda(arg, tpe, expr, a) =>
        (fn(tpe), traverseType(expr, fn)).mapN(AnnotatedLambda(arg, _, _, a))
      case v@Var(_, _, _) => F.pure(v)
      case App(f, a, t) =>
        (traverseType(f, fn), traverseType(a, fn)).mapN(App(_, _, t))
      case Lambda(arg, expr, t) =>
        traverseType(expr, fn).map(Lambda(arg, _, t))
      case Let(arg, exp, in, rec, tag) =>
        (traverseType(exp, fn), traverseType(in, fn)).mapN(Let(arg, _, _, rec, tag))
      case l@Literal(_, _) => F.pure(l)
      case Match(arg, branches, tag) =>
        val argB = traverseType(arg, fn)
        type B = (Pattern[(PackageName, Constructor), rankn.Type], Expr[T])
        def branchFn(b: B): F[B] =
          b match {
            case (pat, expr) =>
              pat.traverseType(fn)
                .product(traverseType(expr, fn))
          }
        val branchB = branches.traverse(branchFn _)
        (argB, branchB).mapN(Match(_, _, tag))
    }

  implicit val exprTraverse: Traverse[Expr] =
    new Traverse[Expr] {

      // Traverse on NonEmptyList[(Pattern[_], Expr[?])]
      private lazy val tne = {
        type Tup[T] = (Pattern[(PackageName, Constructor), rankn.Type], T)
        type TupExpr[T] = (Pattern[(PackageName, Constructor), rankn.Type], Expr[T])
        val tup: Traverse[TupExpr] = Traverse[Tup].compose(exprTraverse)
        Traverse[NonEmptyList].compose(tup)
      }

      def traverse[G[_]: Applicative, A, B](fa: Expr[A])(f: A => G[B]): G[Expr[B]] =
        fa match {
          case Annotation(e, tpe, a) =>
            (e.traverse(f), f(a)).mapN(Annotation(_, tpe, _))
          case AnnotatedLambda(arg, tpe, expr, a) =>
            (expr.traverse(f), f(a)).mapN(AnnotatedLambda(arg, tpe, _, _))
          case Var(p, s, t) =>
            f(t).map(Var(p, s, _))
          case App(fn, a, t) =>
            (fn.traverse(f), a.traverse(f), f(t)).mapN { (fn1, a1, b) =>
              App(fn1, a1, b)
            }
          case Lambda(arg, expr, t) =>
            (expr.traverse(f), f(t)).mapN { (e1, t1) =>
              Lambda(arg, e1, t1)
            }
          case Let(arg, exp, in, rec, tag) =>
            (exp.traverse(f), in.traverse(f), f(tag)).mapN { (e1, i1, t1) =>
              Let(arg, e1, i1, rec, t1)
            }
          case Literal(lit, tag) =>
            f(tag).map(Literal(lit, _))
          case Match(arg, branches, tag) =>
            val argB = arg.traverse(f)
            val branchB = tne.traverse(branches)(f)
            (argB, branchB, f(tag)).mapN { (a, bs, t) =>
              Match(a, bs, t)
            }
        }

      def foldLeft[A, B](fa: Expr[A], b: B)(f: (B, A) => B): B =
        fa match {
          case Annotation(e, _, tag) =>
            val b1 = foldLeft(e, b)(f)
            f(b1, tag)
          case AnnotatedLambda(_, _, e, tag) =>
            val b1 = foldLeft(e, b)(f)
            f(b1, tag)
          case Var(_, _, tag) => f(b, tag)
          case App(fn, a, tag) =>
            val b1 = foldLeft(fn, b)(f)
            val b2 = foldLeft(a, b1)(f)
            f(b2, tag)
          case Lambda(_, expr, tag) =>
            val b1 = foldLeft(expr, b)(f)
            f(b1, tag)
          case Let(_, exp, in, _, tag) =>
            val b1 = foldLeft(exp, b)(f)
            val b2 = foldLeft(in, b1)(f)
            f(b2, tag)
          case Literal(_, tag) =>
            f(b, tag)
          case Match(arg, branches, tag) =>
            val b1 = foldLeft(arg, b)(f)
            val b2 = tne.foldLeft(branches, b1)(f)
            f(b2, tag)
        }

      def foldRight[A, B](fa: Expr[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa match {
          case Annotation(e, _, tag) =>
            val lb1 = foldRight(e, lb)(f)
            f(tag, lb1)
          case AnnotatedLambda(_, _, e, tag) =>
            val lb1 = foldRight(e, lb)(f)
            f(tag, lb1)
          case Var(_, _, tag) => f(tag, lb)
          case App(fn, a, tag) =>
            val b1 = f(tag, lb)
            val b2 = foldRight(a, b1)(f)
            foldRight(fn, b2)(f)
          case Lambda(_, expr, tag) =>
            val b1 = f(tag, lb)
            foldRight(expr, b1)(f)
          case Let(_, exp, in, _, tag) =>
            val b1 = f(tag, lb)
            val b2 = foldRight(in, b1)(f)
            foldRight(exp, b2)(f)
          case Literal(_, tag) =>
            f(tag, lb)
          case Match(arg, branches, tag) =>
            val b1 = f(tag, lb)
            val b2 = tne.foldRight(branches, b1)(f)
            foldRight(arg, b2)(f)
        }
    }

  def buildLambda[A](args: NonEmptyList[(Bindable, Option[rankn.Type])], body: Expr[A], outer: A): Expr[A] =
    args match {
      case NonEmptyList((arg, None), Nil) =>
        Expr.Lambda(arg, body, outer)
      case NonEmptyList((arg, Some(tpe)), Nil) =>
        Expr.AnnotatedLambda(arg, tpe, body, outer)
      case NonEmptyList(arg, h :: tail) =>
        val body1 = buildLambda(NonEmptyList(h, tail), body, outer)
        buildLambda(NonEmptyList.of(arg), body1, outer)
    }

  def buildPatternLambda[A](
    args: NonEmptyList[Pattern[(PackageName, Constructor), rankn.Type]],
    body: Expr[A],
    outer: A): Expr[A] = {

    def makeBindBody(matchPat: Pattern[(PackageName, Constructor), rankn.Type]): (Bindable, Expr[A]) =
      // We don't need to worry about shadowing here
      // because we immediately match the pattern but still this is ugly
      matchPat match {
        case Pattern.Var(arg) =>
          (arg, body)
        case _ =>
          val anonBind: Bindable = Identifier.Name("$anon") // TODO we should have better ways to gensym
          val matchBody: Expr[A] =
            Match(Var(None, anonBind, outer), NonEmptyList.of((matchPat, body)), outer)
          (anonBind, matchBody)
      }

    args match {
      case NonEmptyList(Pattern.Annotation(pat, tpe), Nil) =>
        val (arg, newBody) = makeBindBody(pat)
        Expr.AnnotatedLambda(arg, tpe, newBody, outer)
      case NonEmptyList(matchPat, Nil) =>
        val (arg, newBody) = makeBindBody(matchPat)
        Expr.Lambda(arg, newBody, outer)
      case NonEmptyList(arg, h :: tail) =>
        val body1 = buildPatternLambda(NonEmptyList(h, tail), body, outer)
        buildPatternLambda(NonEmptyList.of(arg), body1, outer)
    }
  }
}

