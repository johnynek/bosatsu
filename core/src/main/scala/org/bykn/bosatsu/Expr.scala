package org.bykn.bosatsu

/**
 * This is a scala port of the example of Hindley Milner inference
 * here: http://dev.stephendiehl.com/fun/006_hindley_milner.html
 */

import cats.implicits._
import cats.evidence.Is
import cats.data.NonEmptyList
import cats.{Applicative, Eval, Traverse}

sealed abstract class Lit
object Lit {
  case class Integer(toInt: Int) extends Lit
  case class Str(toStr: String) extends Lit
}

sealed abstract class Expr[T] {
  import Expr._

  def tag: T
  def setTag(t: T): Expr[T] =
    this match {
      case v@Var(_, _) => v.copy(tag = t)
      case a@App(_, _, _) => a.copy(tag = t)
      case l@Lambda(_, _, _) => l.copy(tag = t)
      case l@Let(_, _, _, _) => l.copy(tag = t)
      case l@Literal(_, _) => l.copy(tag = t)
      case m@Match(_, _, _) => m.copy(tag = t)
    }
}

object Expr {
  case class Var[T](name: String, tag: T) extends Expr[T]
  case class App[T](fn: Expr[T], arg: Expr[T], tag: T) extends Expr[T]
  case class Lambda[T](arg: String, expr: Expr[T], tag: T) extends Expr[T]
  case class Let[T](arg: String, expr: Expr[T], in: Expr[T], tag: T) extends Expr[T]
  case class Literal[T](lit: Lit, tag: T) extends Expr[T]
  case class Match[T](arg: Expr[T], branches: NonEmptyList[(Pattern[(PackageName, ConstructorName)], Expr[T])], tag: T) extends Expr[T]

  implicit def hasRegion[T: HasRegion]: HasRegion[Expr[T]] =
    HasRegion.instance[Expr[T]] { e => HasRegion.region(e.tag) }

  /**
   * Return a value so next(e).tag == e and also this is true
   * recursively
   */
  def nest[T](e: Expr[T]): Expr[Expr[T]] =
    e match {
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
      case Match(arg, branches, _) =>
        Match(nest(arg), branches.map {
          case (pat, exp) => (pat, nest(exp))
        }, e)
    }

  implicit val exprTraverse: Traverse[Expr] =
    new Traverse[Expr] {

      // Traverse on NonEmptyList[(Pattern[_], Expr[?])]
      private lazy val tne = {
        type Tup[T] = (Pattern[(PackageName, ConstructorName)], T)
        type TupExpr[T] = (Pattern[(PackageName, ConstructorName)], Expr[T])
        val tup: Traverse[TupExpr] = Traverse[Tup].compose(exprTraverse)
        Traverse[NonEmptyList].compose(tup)
      }

      def traverse[G[_]: Applicative, A, B](fa: Expr[A])(f: A => G[B]): G[Expr[B]] =
        fa match {
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
          case Match(arg, branches, tag) =>
            val argB = arg.traverse(f)
            val branchB = tne.traverse(branches)(f)
            (argB, branchB, f(tag)).mapN { (a, bs, t) =>
              Match(a, bs, t)
            }
        }

      def foldLeft[A, B](fa: Expr[A], b: B)(f: (B, A) => B): B =
        fa match {
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
          case Match(arg, branches, tag) =>
            val b1 = foldLeft(arg, b)(f)
            val b2 = tne.foldLeft(branches, b1)(f)
            f(b2, tag)
        }

      def foldRight[A, B](fa: Expr[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa match {
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
          case Match(arg, branches, tag) =>
            val b1 = f(tag, lb)
            val b2 = tne.foldRight(branches, b1)(f)
            foldRight(arg, b2)(f)
        }
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
