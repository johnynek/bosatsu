package org.bykn.bosatsu

/**
 * This is a scala port of the example of Hindley Milner inference
 * here: http://dev.stephendiehl.com/fun/006_hindley_milner.html
 */

import cats.implicits._
import cats.data.{Chain, Writer, NonEmptyList}
import cats.{Applicative, Eval, Traverse}
import scala.collection.immutable.SortedSet
import org.bykn.bosatsu.rankn.Type

import Identifier.{Bindable, Constructor}

sealed abstract class Expr[T] {
  def tag: T
}

object Expr {
  sealed abstract class Name[T] extends Expr[T]

  case class Annotation[T](expr: Expr[T], tpe: Type, tag: T) extends Expr[T]
  case class AnnotatedLambda[T](arg: Bindable, tpe: Type, expr: Expr[T], tag: T) extends Expr[T]
  case class Local[T](name: Bindable, tag: T) extends Name[T]
  case class Global[T](pack: PackageName, name: Identifier, tag: T) extends Name[T]
  case class App[T](fn: Expr[T], arg: Expr[T], tag: T) extends Expr[T]
  case class Lambda[T](arg: Bindable, expr: Expr[T], tag: T) extends Expr[T]
  case class Let[T](arg: Bindable, expr: Expr[T], in: Expr[T], recursive: RecursionKind, tag: T) extends Expr[T]
  case class Literal[T](lit: Lit, tag: T) extends Expr[T]
  case class Match[T](arg: Expr[T], branches: NonEmptyList[(Pattern[(PackageName, Constructor), Type], Expr[T])], tag: T) extends Expr[T]


  /**
   * Report all the Bindable names refered to in the given Expr.
   * this can be used to allocate names that can never shadow
   * anything being used in the expr
   */
  final def allNames[A](expr: Expr[A]): SortedSet[Bindable] =
    expr match {
      case Annotation(e, _, _) => allNames(e)
      case AnnotatedLambda(arg, _, expr, _) => allNames(expr) + arg
      case Local(name, _) => SortedSet(name)
      case Global(_, _, _) => SortedSet.empty
      case App(fn, a, _) => allNames(fn) | allNames(a)
      case Lambda(arg, e, _) => allNames(e) + arg
      case Let(arg, expr, in, _, _) => allNames(expr) | allNames(in) + arg
      case Literal(_, _) => SortedSet.empty
      case Match(exp, branches, _) =>
        allNames(exp) | branches.foldMap { case (pat, res) => allNames(res) ++ pat.names }
    }

  implicit def hasRegion[T: HasRegion]: HasRegion[Expr[T]] =
    HasRegion.instance[Expr[T]] { e => HasRegion.region(e.tag) }

  /*
   * Allocate these once
   */
  private[this] val TruePat: Pattern[(PackageName, Constructor), Type] =
    Pattern.PositionalStruct((PackageName.PredefName, Constructor("True")), Nil)
  private[this] val FalsePat: Pattern[(PackageName, Constructor), Type] =
    Pattern.PositionalStruct((PackageName.PredefName, Constructor("False")), Nil)
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

  def traverseType[T, F[_]](expr: Expr[T], fn: Type => F[Type])(implicit F: Applicative[F]): F[Expr[T]] =
    expr match {
      case Annotation(e, tpe, a) =>
        (traverseType(e, fn), fn(tpe)).mapN(Annotation(_, _, a))
      case AnnotatedLambda(arg, tpe, expr, a) =>
        (fn(tpe), traverseType(expr, fn)).mapN(AnnotatedLambda(arg, _, _, a))
      case v: Name[T] => F.pure(v)
      case App(f, a, t) =>
        (traverseType(f, fn), traverseType(a, fn)).mapN(App(_, _, t))
      case Lambda(arg, expr, t) =>
        traverseType(expr, fn).map(Lambda(arg, _, t))
      case Let(arg, exp, in, rec, tag) =>
        (traverseType(exp, fn), traverseType(in, fn)).mapN(Let(arg, _, _, rec, tag))
      case l@Literal(_, _) => F.pure(l)
      case Match(arg, branches, tag) =>
        val argB = traverseType(arg, fn)
        type B = (Pattern[(PackageName, Constructor), Type], Expr[T])
        def branchFn(b: B): F[B] =
          b match {
            case (pat, expr) =>
              pat.traverseType(fn)
                .product(traverseType(expr, fn))
          }
        val branchB = branches.traverse(branchFn _)
        (argB, branchB).mapN(Match(_, _, tag))
    }

  def substExpr[A](keys: NonEmptyList[Type.Var], vals: NonEmptyList[Type.Rho], expr: Expr[A]): Expr[A] = {
    val fn = Type.substTy(keys, vals)
    Expr.traverseType[A, cats.Id](expr, fn)
  }

  /**
   * Here we substitute any free bound variables with skolem variables
   *
   * This is a deviation from the paper.
   * We are allowing a syntax like:
   *
   * def identity(x: a) -> a:
   *   x
   *
   * or:
   *
   * def foo(x: a): x
   *
   * We handle this by converting a to a skolem variable,
   * running inference, then quantifying over that skolem
   * variable.
   */
  def skolemizeFreeVars[F[_]: Applicative, A](expr: Expr[A])(newSkolemTyVar: Type.Var => F[Type.Var.Skolem]): Option[F[(NonEmptyList[Type.Var.Skolem], Expr[A])]] = {
    val w = Expr.traverseType(expr, { t =>
      val frees = Chain.fromSeq(Type.freeBoundTyVars(t :: Nil))
      Writer(frees, t)
    })
    val frees = w.written.iterator.toList.distinct
    NonEmptyList.fromList(frees)
      .map { tvs =>
        tvs.traverse(newSkolemTyVar)
          .map { skVs =>
            val sksT = skVs.map(Type.TyVar(_))
            val expr1 = substExpr(tvs, sksT, expr)
            (skVs, expr1)
          }
      }
  }

  /*
   * We have seen some intermitten CI failures if this isn't lazy
   * presumably due to initialiazation order
   */
  implicit lazy val exprTraverse: Traverse[Expr] =
    new Traverse[Expr] {

      // Traverse on NonEmptyList[(Pattern[_], Expr[?])]
      private lazy val tne = {
        type Tup[T] = (Pattern[(PackageName, Constructor), Type], T)
        type TupExpr[T] = (Pattern[(PackageName, Constructor), Type], Expr[T])
        val tup: Traverse[TupExpr] = Traverse[Tup].compose(exprTraverse)
        Traverse[NonEmptyList].compose(tup)
      }

      def traverse[G[_]: Applicative, A, B](fa: Expr[A])(f: A => G[B]): G[Expr[B]] =
        fa match {
          case Annotation(e, tpe, a) =>
            (e.traverse(f), f(a)).mapN(Annotation(_, tpe, _))
          case AnnotatedLambda(arg, tpe, expr, a) =>
            (expr.traverse(f), f(a)).mapN(AnnotatedLambda(arg, tpe, _, _))
          case Local(s, t) =>
            f(t).map(Local(s, _))
          case Global(p, s, t) =>
            f(t).map(Global(p, s, _))
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
          case n: Name[A] => f(b, n.tag)
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
          case n: Name[A] => f(n.tag, lb)
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

  def buildPatternLambda[A](
    args: NonEmptyList[Pattern[(PackageName, Constructor), Type]],
    body: Expr[A],
    outer: A): Expr[A] = {

    /*
     * compute this once if needed, which is why it is lazy.
     * we don't want to traverse body if it is never needed
     */
    lazy val anons = Type
      .allBinders
      .iterator
      .map(_.name)
      .map(Identifier.Name(_))
      .filterNot(allNames(body) ++ args.toList.flatMap(_.names))

    def loop(
      args: NonEmptyList[Pattern[(PackageName, Constructor), Type]],
      body: Expr[A]): Expr[A] = {

      def makeBindBody(matchPat: Pattern[(PackageName, Constructor), Type]): (Bindable, Expr[A]) =
        // We don't need to worry about shadowing here
        // because we immediately match the pattern but still this is ugly
        matchPat match {
          case Pattern.Var(arg) =>
            (arg, body)
          case _ =>
            val anonBind: Bindable = anons.next()
            val matchBody: Expr[A] =
              Match(Local(anonBind, outer), NonEmptyList.of((matchPat, body)), outer)
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
          val body1 = loop(NonEmptyList(h, tail), body)
          loop(NonEmptyList.of(arg), body1)
      }
    }

    loop(args, body)
  }
}

