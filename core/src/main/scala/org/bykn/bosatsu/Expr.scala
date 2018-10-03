package org.bykn.bosatsu

/**
 * This is a scala port of the example of Hindley Milner inference
 * here: http://dev.stephendiehl.com/fun/006_hindley_milner.html
 */

import cats.implicits._
import cats.evidence.Is
import cats.data.NonEmptyList
import cats.{Applicative, Eval, Traverse}

sealed abstract class Expr[T] {
  import Expr._

  def tag: T
  def setTag(t: T): Expr[T] =
    this match {
      case a@Annotation(_, _, _) => a.copy(tag = t)
      case v@Var(_, _) => v.copy(tag = t)
      case a@App(_, _, _) => a.copy(tag = t)
      case l@Lambda(_, _, _) => l.copy(tag = t)
      case a@AnnotatedLambda(_, _, _, _) => a.copy(tag = t)
      case l@Let(_, _, _, _) => l.copy(tag = t)
      case l@Literal(_, _) => l.copy(tag = t)
      case i@If(_, _, _, _) => i.copy(tag = t)
      case m@Match(_, _, _) => m.copy(tag = t)
    }
}

object Expr {
  case class Annotation[T](expr: Expr[T], tpe: rankn.Type, tag: T) extends Expr[T]
  case class AnnotatedLambda[T](arg: String, tpe: rankn.Type, expr: Expr[T], tag: T) extends Expr[T] {
    def toLambda: Lambda[T] = Lambda(arg, expr, tag)
  }
  case class Var[T](name: String, tag: T) extends Expr[T]
  case class App[T](fn: Expr[T], arg: Expr[T], tag: T) extends Expr[T]
  case class Lambda[T](arg: String, expr: Expr[T], tag: T) extends Expr[T]
  case class Let[T](arg: String, expr: Expr[T], in: Expr[T], tag: T) extends Expr[T]
  case class Literal[T](lit: Lit, tag: T) extends Expr[T]
  case class If[T](cond: Expr[T], ifTrue: Expr[T], ifFalse: Expr[T], tag: T) extends Expr[T]
  case class Match[T](arg: Expr[T], branches: NonEmptyList[(Pattern[(PackageName, ConstructorName), rankn.Type], Expr[T])], tag: T) extends Expr[T]

  implicit def hasRegion[T: HasRegion]: HasRegion[Expr[T]] =
    HasRegion.instance[Expr[T]] { e => HasRegion.region(e.tag) }

  /**
   * Return a value so next(e).tag == e and also this is true
   * recursively
   */
  def nest[T](e: Expr[T]): Expr[Expr[T]] =
    e match {
      case Annotation(expr, tpe, _) =>
        Annotation(nest(expr), tpe, e)
      case AnnotatedLambda(arg, tpe, expr, _) =>
        AnnotatedLambda(arg, tpe, nest(expr), e)
      case Var(s, _) =>
        Var(s, e)
      case App(fn, a, _) =>
        App(nest(fn), nest(a), e)
      case Lambda(arg, expr, _) =>
        Lambda(arg, nest(expr), e)
      case Let(arg, exp, in, _) =>
        Let(arg, nest(exp), nest(in), e)
      case Literal(lit, _) =>
        Literal(lit, e)
      case If(cond, ift, iff, _) =>
        If(nest(cond), nest(ift), nest(iff), e)
      case Match(arg, branches, _) =>
        Match(nest(arg), branches.map {
          case (pat, exp) => (pat, nest(exp))
        }, e)
    }

  implicit val exprTraverse: Traverse[Expr] =
    new Traverse[Expr] {

      // Traverse on NonEmptyList[(Pattern[_], Expr[?])]
      private lazy val tne = {
        type Tup[T] = (Pattern[(PackageName, ConstructorName), rankn.Type], T)
        type TupExpr[T] = (Pattern[(PackageName, ConstructorName), rankn.Type], Expr[T])
        val tup: Traverse[TupExpr] = Traverse[Tup].compose(exprTraverse)
        Traverse[NonEmptyList].compose(tup)
      }

      def traverse[G[_]: Applicative, A, B](fa: Expr[A])(f: A => G[B]): G[Expr[B]] =
        fa match {
          case Annotation(e, tpe, a) =>
            (e.traverse(f), f(a)).mapN(Annotation(_, tpe, _))
          case AnnotatedLambda(arg, tpe, expr, a) =>
            (expr.traverse(f), f(a)).mapN(AnnotatedLambda(arg, tpe, _, _))
          case Var(s, t) =>
            f(t).map(Var(s, _))
          case App(fn, a, t) =>
            (fn.traverse(f), a.traverse(f), f(t)).mapN { (fn1, a1, b) =>
              App(fn1, a1, b)
            }
          case Lambda(arg, expr, t) =>
            (expr.traverse(f), f(t)).mapN { (e1, t1) =>
              Lambda(arg, e1, t1)
            }
          case Let(arg, exp, in, tag) =>
            (exp.traverse(f), in.traverse(f), f(tag)).mapN { (e1, i1, t1) =>
              Let(arg, e1, i1, t1)
            }
          case Literal(lit, tag) =>
            f(tag).map(Literal(lit, _))
          case If(cond, ift, iff, tag) =>
            (cond.traverse(f), ift.traverse(f), iff.traverse(f), f(tag))
              .mapN(If(_, _, _, _))
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
          case Var(_, tag) => f(b, tag)
          case App(fn, a, tag) =>
            val b1 = foldLeft(fn, b)(f)
            val b2 = foldLeft(a, b1)(f)
            f(b2, tag)
          case Lambda(_, expr, tag) =>
            val b1 = foldLeft(expr, b)(f)
            f(b1, tag)
          case Let(_, exp, in, tag) =>
            val b1 = foldLeft(exp, b)(f)
            val b2 = foldLeft(in, b1)(f)
            f(b2, tag)
          case Literal(_, tag) =>
            f(b, tag)
          case If(cond, ift, iff, tag) =>
            val b1 = foldLeft(cond, b)(f)
            val b2 = foldLeft(ift, b1)(f)
            val b3 = foldLeft(iff, b2)(f)
            f(b3, tag)
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
          case Var(_, tag) => f(tag, lb)
          case App(fn, a, tag) =>
            val b1 = f(tag, lb)
            val b2 = foldRight(a, b1)(f)
            foldRight(fn, b2)(f)
          case Lambda(_, expr, tag) =>
            val b1 = f(tag, lb)
            foldRight(expr, b1)(f)
          case Let(_, exp, in, tag) =>
            val b1 = f(tag, lb)
            val b2 = foldRight(in, b1)(f)
            foldRight(exp, b2)(f)
          case Literal(_, tag) =>
            f(tag, lb)
          case If(cond, ift, iff, tag) =>
            val b1 = f(tag, lb)
            val b2 = foldRight(iff, b1)(f)
            val b3 = foldRight(ift, b2)(f)
            foldRight(cond, b3)(f)
          case Match(arg, branches, tag) =>
            val b1 = f(tag, lb)
            val b2 = tne.foldRight(branches, b1)(f)
            foldRight(arg, b2)(f)
        }
    }

  def buildLambda[A](args: NonEmptyList[(String, Option[rankn.Type])], body: Expr[A], outer: A): Expr[A] =
    args match {
      case NonEmptyList((arg, None), Nil) =>
        Expr.Lambda(arg, body, outer)
      case NonEmptyList((arg, Some(tpe)), Nil) =>
        Expr.AnnotatedLambda(arg, tpe, body, outer)
      case NonEmptyList(arg, h :: tail) =>
        val body1 = buildLambda(NonEmptyList(h, tail), body, outer)
        buildLambda(NonEmptyList.of(arg), body1, outer)
    }

}

case class Program[D, S](types: TypeEnv, lets: List[(String, Expr[D])], from: S) {
  private[this] lazy val letMap: Map[String, Expr[D]] = lets.toMap

  def getLet(name: String): Option[Expr[D]] = letMap.get(name)
  /**
   * main is the thing we evaluate. It is the last thing defined
   */
  def getMain(fn: (String, D, D) => D): Option[Expr[D]] = {
    @annotation.tailrec
    def loop(ls: List[(String, Expr[D])], acc: Expr[D]): Expr[D] =
      ls match {
        case Nil => acc
        case (nm, expr) :: tail =>
          val decl = fn(nm, expr.tag, acc.tag)
          loop(tail, Expr.Let(nm, expr, acc, decl))
      }

    lets.reverse match {
      case (_, h) :: tail =>
        Some(loop(tail, h))
      case Nil =>
        None
    }
  }

  def getMainDecl(implicit ev: D Is Declaration): Option[Expr[Declaration]] = {

    val fn = { (nm: String, v: D, in: D) =>
      val r = ev.coerce(v).region + ev.coerce(in).region
      ev.flip.coerce(Declaration.Binding(
        BindingStatement(nm, ev.coerce(v), Padding(0, ev.coerce(in))))(r))
    }

    type F[T] = Option[Expr[T]]
    ev.substitute[F](getMain(fn))
  }
}
