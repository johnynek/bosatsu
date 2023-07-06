package org.bykn.bosatsu

/** This is a scala port of the example of Hindley Milner inference here:
  * http://dev.stephendiehl.com/fun/006_hindley_milner.html
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
  case class Local[T](name: Bindable, tag: T) extends Name[T]
  case class Generic[T](
      typeVars: NonEmptyList[(Type.Var.Bound, Kind)],
      in: Expr[T]
  ) extends Expr[T] {
    def tag = in.tag
  }
  case class Global[T](pack: PackageName, name: Identifier, tag: T)
      extends Name[T]
  case class App[T](fn: Expr[T], arg: Expr[T], tag: T) extends Expr[T]
  case class Lambda[T](arg: Bindable, tpe: Option[Type], expr: Expr[T], tag: T)
      extends Expr[T]
  case class Let[T](
      arg: Bindable,
      expr: Expr[T],
      in: Expr[T],
      recursive: RecursionKind,
      tag: T
  ) extends Expr[T]
  case class Literal[T](lit: Lit, tag: T) extends Expr[T]
  case class Match[T](
      arg: Expr[T],
      branches: NonEmptyList[
        (Pattern[(PackageName, Constructor), Type], Expr[T])
      ],
      tag: T
  ) extends Expr[T]

  /** Report all the Bindable names refered to in the given Expr. this can be
    * used to allocate names that can never shadow anything being used in the
    * expr
    */
  final def allNames[A](expr: Expr[A]): SortedSet[Bindable] =
    expr match {
      case Annotation(e, _, _)      => allNames(e)
      case Local(name, _)           => SortedSet(name)
      case Generic(_, in)           => allNames(in)
      case Global(_, _, _)          => SortedSet.empty
      case App(fn, a, _)            => allNames(fn) | allNames(a)
      case Lambda(arg, _, e, _)     => allNames(e) + arg
      case Let(arg, expr, in, _, _) => allNames(expr) | allNames(in) + arg
      case Literal(_, _)            => SortedSet.empty
      case Match(exp, branches, _) =>
        allNames(exp) | branches.foldMap { case (pat, res) =>
          allNames(res) ++ pat.names
        }
    }

  implicit def hasRegion[T: HasRegion]: HasRegion[Expr[T]] =
    HasRegion.instance[Expr[T]] { e => HasRegion.region(e.tag) }

  /*
   * Allocate these once
   */
  private[this] val TruePat: Pattern[(PackageName, Constructor), Type] =
    Pattern.PositionalStruct((PackageName.PredefName, Constructor("True")), Nil)
  private[this] val FalsePat: Pattern[(PackageName, Constructor), Type] =
    Pattern.PositionalStruct(
      (PackageName.PredefName, Constructor("False")),
      Nil
    )

  /** build a Match expression that is equivalent to if/else using Predef::True
    * and Predef::False
    */
  def ifExpr[T](
      cond: Expr[T],
      ifTrue: Expr[T],
      ifFalse: Expr[T],
      tag: T
  ): Expr[T] =
    Match(cond, NonEmptyList.of((TruePat, ifTrue), (FalsePat, ifFalse)), tag)

  /** Build an apply expression by appling these args left to right
    */
  @annotation.tailrec
  def buildApp[A](fn: Expr[A], args: List[Expr[A]], appTag: A): Expr[A] =
    args match {
      case Nil => fn
      case h :: tail =>
        buildApp(App(fn, h, appTag), tail, appTag)
    }

  // Traverse all non-bound vars
  private def traverseType[T, F[_]](expr: Expr[T], bound: Set[Type.Var.Bound])(
      fn: (Type, Set[Type.Var.Bound]) => F[Type]
  )(implicit F: Applicative[F]): F[Expr[T]] =
    expr match {
      case Annotation(e, tpe, a) =>
        (traverseType(e, bound)(fn), fn(tpe, bound)).mapN(Annotation(_, _, a))
      case v: Name[T] => F.pure(v)
      case App(f, a, t) =>
        (traverseType(f, bound)(fn), traverseType(a, bound)(fn))
          .mapN(App(_, _, t))
      case Generic(bs, in) =>
        // Seems dangerous since we are hiding from fn that the Type.TyVar inside
        // matching these are not unbound
        val bound1 = bound ++ bs.toList.iterator.map(_._1)
        traverseType(in, bound1)(fn).map(Generic(bs, _))
      case Lambda(arg, optT, expr, t) =>
        (optT.traverse(fn(_, bound)), traverseType(expr, bound)(fn))
          .mapN(Lambda(arg, _, _, t))
      case Let(arg, exp, in, rec, tag) =>
        (traverseType(exp, bound)(fn), traverseType(in, bound)(fn))
          .mapN(Let(arg, _, _, rec, tag))
      case l @ Literal(_, _) => F.pure(l)
      case Match(arg, branches, tag) =>
        val argB = traverseType(arg, bound)(fn)
        type B = (Pattern[(PackageName, Constructor), Type], Expr[T])
        def branchFn(b: B): F[B] =
          b match {
            case (pat, expr) =>
              pat
                .traverseType(fn(_, bound))
                .product(traverseType(expr, bound)(fn))
          }
        val branchB = branches.traverse(branchFn _)
        (argB, branchB).mapN(Match(_, _, tag))
    }

  private def substExpr[A](
      keys: NonEmptyList[Type.Var],
      vals: NonEmptyList[Type.Rho],
      expr: Expr[A]
  ): Expr[A] = {
    val fn = Type.substTy(keys, vals)
    traverseType[A, cats.Id](expr, Set.empty) { (t, bound) =>
      // we have to remove any of the keys that are bound
      val isBound: Type.Var => Boolean = {
        case b @ Type.Var.Bound(_) => bound(b)
        case _                     => false
      }

      if (keys.exists(isBound)) {
        val kv1 = keys.zip(vals).toList.filter { case (b, _) => !isBound(b) }
        NonEmptyList.fromList(kv1) match {
          case Some(kv1Nel) =>
            val (k1, v1) = kv1Nel.unzip
            Type.substTy(k1, v1)(t)
          case None =>
            t
        }
      } else fn(t)
    }
  }

  def freeBoundTyVars[A](expr: Expr[A]): List[Type.Var.Bound] = {
    val w = traverseType(expr, Set.empty) { (t, bound) =>
      val frees = Chain.fromSeq(Type.freeBoundTyVars(t :: Nil))
      Writer(frees.filterNot(bound), t)
    }
    w.written.iterator.toList.distinct
  }

  /** Here we substitute any free bound variables with skolem variables
    *
    * This is a deviation from the paper. We are allowing a syntax like:
    *
    * def identity(x: a) -> a: x
    *
    * or:
    *
    * def foo(x: a): x
    *
    * We handle this by converting a to a skolem variable, running inference,
    * then quantifying over that skolem variable.
    */
  def skolemizeFreeVars[F[_]: Applicative, A](expr: Expr[A])(
      newSkolemTyVar: (Type.Var.Bound, Kind) => F[Type.Var.Skolem]
  ): Option[F[(NonEmptyList[Type.Var.Skolem], Expr[A])]] = {
    val frees = freeBoundTyVars(expr)
    NonEmptyList
      .fromList(frees)
      .map { tvs =>
        skolemizeVars[F, A](tvs.map { b => (b, Kind.Type) }, expr)(
          newSkolemTyVar
        )
      }
  }

  def skolemizeVars[F[_]: Applicative, A](
      vs: NonEmptyList[(Type.Var.Bound, Kind)],
      expr: Expr[A]
  )(
      newSkolemTyVar: (Type.Var.Bound, Kind) => F[Type.Var.Skolem]
  ): F[(NonEmptyList[Type.Var.Skolem], Expr[A])] = {
    vs.traverse { case (b, k) => newSkolemTyVar(b, k) }
      .map { skVs =>
        val sksT = skVs.map(Type.TyVar(_))
        val expr1 = substExpr(vs.map(_._1), sksT, expr)
        (skVs, expr1)
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

      def traverse[G[_]: Applicative, A, B](
          fa: Expr[A]
      )(f: A => G[B]): G[Expr[B]] =
        fa match {
          case Annotation(e, tpe, a) =>
            (e.traverse(f), f(a)).mapN(Annotation(_, tpe, _))
          case Local(s, t) =>
            f(t).map(Local(s, _))
          case Global(p, s, t) =>
            f(t).map(Global(p, s, _))
          case Generic(bs, e) =>
            traverse(e)(f).map(Generic(bs, _))
          case App(fn, a, t) =>
            (fn.traverse(f), a.traverse(f), f(t)).mapN { (fn1, a1, b) =>
              App(fn1, a1, b)
            }
          case Lambda(arg, tpe, expr, t) =>
            (expr.traverse(f), f(t)).mapN { (e1, t1) =>
              Lambda(arg, tpe, e1, t1)
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
          case n: Name[A] => f(b, n.tag)
          case App(fn, a, tag) =>
            val b1 = foldLeft(fn, b)(f)
            val b2 = foldLeft(a, b1)(f)
            f(b2, tag)
          case Generic(_, in) => foldLeft(in, b)(f)
          case Lambda(_, _, expr, tag) =>
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

      def foldRight[A, B](fa: Expr[A], lb: Eval[B])(
          f: (A, Eval[B]) => Eval[B]
      ): Eval[B] =
        fa match {
          case Annotation(e, _, tag) =>
            val lb1 = foldRight(e, lb)(f)
            f(tag, lb1)
          case n: Name[A] => f(n.tag, lb)
          case App(fn, a, tag) =>
            val b1 = f(tag, lb)
            val b2 = foldRight(a, b1)(f)
            foldRight(fn, b2)(f)
          case Generic(_, in) => foldRight(in, lb)(f)
          case Lambda(_, _, expr, tag) =>
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
      outer: A
  ): Expr[A] = {

    /*
     * compute this once if needed, which is why it is lazy.
     * we don't want to traverse body if it is never needed
     */
    lazy val anons = Type.allBinders.iterator
      .map(_.name)
      .map(Identifier.Name(_))
      .filterNot(allNames(body) ++ args.patternNames)

    def loop(
        args: NonEmptyList[Pattern[(PackageName, Constructor), Type]],
        body: Expr[A]
    ): Expr[A] = {

      def makeBindBody(
          matchPat: Pattern[(PackageName, Constructor), Type]
      ): (Bindable, Expr[A]) =
        // We don't need to worry about shadowing here
        // because we immediately match the pattern but still this is ugly
        matchPat match {
          case Pattern.Var(arg) =>
            (arg, body)
          case _ =>
            val anonBind: Bindable = anons.next()
            val matchBody: Expr[A] =
              Match(
                Local(anonBind, outer),
                NonEmptyList.of((matchPat, body)),
                outer
              )
            (anonBind, matchBody)
        }

      args match {
        case NonEmptyList(Pattern.Annotation(pat, tpe), Nil) =>
          val (arg, newBody) = makeBindBody(pat)
          Expr.Lambda(arg, Some(tpe), newBody, outer)
        case NonEmptyList(matchPat, Nil) =>
          val (arg, newBody) = makeBindBody(matchPat)
          Expr.Lambda(arg, None, newBody, outer)
        case NonEmptyList(arg, h :: tail) =>
          val body1 = loop(NonEmptyList(h, tail), body)
          loop(NonEmptyList.of(arg), body1)
      }
    }

    loop(args, body)
  }
}
