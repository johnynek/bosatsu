package dev.bosatsu

/** This is a scala port of the example of Hindley Milner inference here:
  * http://dev.stephendiehl.com/fun/006_hindley_milner.html
  */

import cats.implicits._
import cats.data.{Chain, Writer, NonEmptyList}
import cats.Applicative
import scala.collection.immutable.SortedSet
import dev.bosatsu.rankn.Type

import Identifier.{Bindable, Constructor}

sealed abstract class Expr[T] derives CanEqual {
  def tag: T

  /** All the free variables in this expression in order encountered and with
    * duplicates (to see how often they appear)
    */
  lazy val freeVarsDup: List[Bindable] = {
    import Expr._
    // nearly identical code to TypedExpr.freeVarsDup, bugs should be fixed in both places
    this match {
      case Generic(_, expr) =>
        expr.freeVarsDup
      case Annotation(t, _, _) =>
        t.freeVarsDup
      case Local(ident, _) =>
        ident :: Nil
      case Global(_, _, _) =>
        Nil
      case Lambda(args, res, _) =>
        val nameSet = args.toList.iterator.map(_._1).toSet
        ListUtil.filterNot(res.freeVarsDup)(nameSet)
      case App(fn, args, _) =>
        fn.freeVarsDup ::: args.reduceMap(_.freeVarsDup)
      case Let(arg, argE, in, rec, _) =>
        val argFree0 = argE.freeVarsDup
        val argFree =
          if (rec.isRecursive) {
            ListUtil.filterNot(argFree0)(_ == arg)
          } else argFree0

        argFree ::: (ListUtil.filterNot(in.freeVarsDup)(_ == arg))
      case Literal(_, _) =>
        Nil
      case Match(arg, branches, _) =>
        val argFree = arg.freeVarsDup

        val branchFrees = branches.toList.map { branch =>
          // these are not free variables in this branch
          val newBinds = branch.pattern.names.toSet
          val bfree = branch.guard.fold(Nil: List[Bindable])(_.freeVarsDup) ::: branch.expr.freeVarsDup
          if (newBinds.isEmpty) bfree
          else ListUtil.filterNot(bfree)(newBinds)
        }
        // we can only take one branch, so count the max on each branch:
        val branchFreeMax = branchFrees.zipWithIndex
          .flatMap { case (names, br) => names.map((_, br)) }
          // these groupBys are okay because we sort at the end
          .groupBy(identity) // group-by-name x branch
          .map { case ((name, branch), names) => (names.length, branch, name) }
          .groupBy(_._3) // group by just the name now
          .toList
          .flatMap { case (_, vs) =>
            val (cnt, branch, name) = vs.maxBy(_._1)
            List.fill(cnt)((branch, name))
          }
          .sorted
          .map(_._2)

        argFree ::: branchFreeMax
    }
  }

  lazy val globals: Set[Expr.Global[T]] = {
    import Expr._
    this match {
      case Generic(_, expr) =>
        expr.globals
      case Annotation(t, _, _) =>
        t.globals
      case Local(_, _)         => Set.empty
      case g @ Global(_, _, _) => Set.empty + g
      case Lambda(_, res, _)   => res.globals
      case App(fn, args, _)    =>
        fn.globals | args.reduceMap(_.globals)
      case Let(_, argE, in, _, _) =>
        argE.globals | in.globals
      case Literal(_, _)           => Set.empty
      case Match(arg, branches, _) =>
        arg.globals | branches.foldMap { branch =>
          branch.guard.fold(Set.empty[Expr.Global[T]])(_.globals) | branch.expr.globals
        }
    }
  }

  def replaceTag(t: T): Expr[T] = {
    import Expr._
    this match {
      case g @ Generic(_, e)       => g.copy(in = e.replaceTag(t))
      case a @ Annotation(_, _, _) => a.copy(tag = t)
      case l @ Local(_, _)         => l.copy(tag = t)
      case g @ Global(_, _, _)     => g.copy(tag = t)
      case l @ Lambda(_, _, _)     => l.copy(tag = t)
      case a @ App(_, _, _)        => a.copy(tag = t)
      case l @ Let(_, _, _, _, _)  => l.copy(tag = t)
      case l @ Literal(_, _)       => l.copy(tag = t)
      case m @ Match(_, _, _)      => m.copy(tag = t)
    }
  }

  def eraseTags: Expr[Unit] = {
    import Expr._
    this match {
      case Annotation(e, tpe, _) =>
        Annotation(e.eraseTags, tpe, ())
      case Local(ident, _) =>
        Local(ident, ())
      case Global(pack, name, _) =>
        Global(pack, name, ())
      case Generic(typeVars, in) =>
        Generic(typeVars, in.eraseTags)
      case Lambda(args, expr, _) =>
        Lambda(args, expr.eraseTags, ())
      case App(fn, args, _) =>
        App(fn.eraseTags, args.map(_.eraseTags), ())
      case Let(arg, expr, in, recursive, _) =>
        Let(arg, expr.eraseTags, in.eraseTags, recursive, ())
      case Literal(lit, _) =>
        Literal(lit, ())
      case Match(arg, branches, _) =>
        Match(
          arg.eraseTags,
          branches.map { b =>
            Branch(b.pattern, b.guard.map(_.eraseTags), b.expr.eraseTags)
          },
          ()
        )
    }
  }

  def notFree(b: Bindable): Boolean =
    !freeVarsDup.contains(b)
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
  case class App[T](fn: Expr[T], args: NonEmptyList[Expr[T]], tag: T)
      extends Expr[T]
  case class Lambda[T](
      args: NonEmptyList[(Bindable, Option[Type])],
      expr: Expr[T],
      tag: T
  ) extends Expr[T]
  case class Let[T](
      arg: Bindable,
      expr: Expr[T],
      in: Expr[T],
      recursive: RecursionKind,
      tag: T
  ) extends Expr[T] {
    def flatten
        : (NonEmptyList[(Bindable, RecursionKind, Expr[T], T)], Expr[T]) = {
      val thisLet = (arg, recursive, expr, tag)

      in match {
        case let @ Let(_, _, _, _, _) =>
          val (lets, finalIn) = let.flatten
          (thisLet :: lets, finalIn)
        case _ =>
          // this is the final let
          (NonEmptyList.one(thisLet), in)
      }
    }
  }
  case class Literal[T](lit: Lit, tag: T) extends Expr[T]
  case class Branch[T](
      pattern: Pattern[(PackageName, Constructor), Type],
      guard: Option[Expr[T]],
      expr: Expr[T]
  )
  case class Match[T](
      arg: Expr[T],
      branches: NonEmptyList[Branch[T]],
      tag: T
  ) extends Expr[T]

  // Inverse of `Let.flatten`
  def lets[T](
      binds: List[(Bindable, RecursionKind, Expr[T], T)],
      in: Expr[T]
  ): Expr[T] =
    binds match {
      case Nil                  => in
      case (b, r, e, t) :: tail =>
        val res = lets(tail, in)
        Let(b, e, res, r, t)
    }

  object Annotated {
    def unapply[A](expr: Expr[A]): Option[Type] =
      expr match {
        case Annotation(_, tpe, _)           => Some(tpe)
        case Lambda(args, Annotated(res), _) =>
          args
            .traverse { case (_, ot) => ot }
            .map { argTpes =>
              Type.Fun(argTpes, res)
            }
        case Literal(lit, _)               => Some(Type.getTypeOf(lit))
        case Let(_, _, Annotated(t), _, _) => Some(t)
        case Match(_, branches, _)         =>
          branches
            .traverse(branch => unapply(branch.expr))
            .flatMap { allAnnotated =>
              if (allAnnotated.tail.forall(_ == allAnnotated.head))
                Some(allAnnotated.head)
              else None
            }
        case _ => None
      }
  }

  def forAll[A](tpeArgs: List[(Type.Var.Bound, Kind)], expr: Expr[A]): Expr[A] =
    NonEmptyList.fromList(tpeArgs) match {
      case None      => expr
      case Some(nel) =>
        expr match {
          case Annotation(expr, tpe, tag) =>
            val tpeFrees = Type.freeBoundTyVars(tpe :: Nil).toSet
            // these are the frees that are also in tpeArgs
            val freeArgs = tpeArgs.filter { case (n, _) => tpeFrees(n) }
            Annotation(forAll(tpeArgs, expr), Type.forAll(freeArgs, tpe), tag)
          case Generic(typeVars, in) =>
            Generic(nel ::: typeVars, in)
          case notAnn => Generic(nel, notAnn)
        }
    }

  def quantifyFrees[A](expr: Expr[A]): Expr[A] =
    forAll(freeBoundTyVars(expr).map((_, Kind.Type)), expr)

  /** Report all the Bindable names refered to in the given Expr. this can be
    * used to allocate names that can never shadow anything being used in the
    * expr
    */
  final def allNames[A](expr: Expr[A]): SortedSet[Bindable] =
    expr match {
      case Annotation(e, _, _) => allNames(e)
      case Local(name, _)      => SortedSet(name)
      case Generic(_, in)      => allNames(in)
      case Global(_, _, _)     => SortedSet.empty
      case App(fn, args, _)    =>
        args.foldLeft(allNames(fn))((bs, e) => bs | allNames(e))
      case Lambda(args, e, _) => allNames(e) ++ args.toList.iterator.map(_._1)
      case Let(arg, expr, in, _, _) => allNames(expr) | allNames(in) + arg
      case Literal(_, _)            => SortedSet.empty
      case Match(exp, branches, _)  =>
        allNames(exp) | branches.foldMap { branch =>
          val b = allNames(branch.expr) ++ branch.pattern.names
          branch.guard.fold(b)(g => b | allNames(g))
        }
    }

  implicit def hasRegion[T: HasRegion]: HasRegion[Expr[T]] =
    HasRegion.instance[Expr[T]](e => HasRegion.region(e.tag))

  /*
   * Allocate these once
   */
  private val TruePat: Pattern[(PackageName, Constructor), Type] =
    Pattern.PositionalStruct((PackageName.PredefName, Constructor("True")), Nil)
  private val FalsePat: Pattern[(PackageName, Constructor), Type] =
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
    Match(
      cond,
      NonEmptyList.of(
        Branch(TruePat, None, ifTrue),
        Branch(FalsePat, None, ifFalse)
      ),
      tag
    )

  /** Build an apply expression by appling these args left to right
    */
  def buildApp[A](fn: Expr[A], args: List[Expr[A]], appTag: A): Expr[A] =
    args match {
      case head :: tail => App(fn, NonEmptyList(head, tail), appTag)
      case Nil          => fn
    }

  // Traverse all non-bound vars
  private def traverseType[T, F[_]](expr: Expr[T], bound: Set[Type.Var.Bound])(
      fn: (Type, Set[Type.Var.Bound]) => F[Type]
  )(implicit F: Applicative[F]): F[Expr[T]] =
    expr match {
      case Annotation(e, tpe, a) =>
        (traverseType[T, F](e, bound)(fn), fn(tpe, bound))
          .mapN(Annotation(_, _, a))
      case v: Name[T]      => F.pure(v)
      case App(f, args, t) =>
        (
          traverseType[T, F](f, bound)(fn),
          args.traverse(traverseType[T, F](_, bound)(fn))
        )
          .mapN(App(_, _, t))
      case Generic(bs, in) =>
        // Seems dangerous since we are hiding from fn that the Type.TyVar inside
        // matching these are not unbound
        val bound1 = bound ++ bs.toList.iterator.map(_._1)
        traverseType[T, F](in, bound1)(fn).map(Generic(bs, _))
      case Lambda(args, expr, t) =>
        (
          args.traverse { case (n, optT) =>
            optT.traverse(fn(_, bound)).map((n, _))
          },
          traverseType[T, F](expr, bound)(fn)
        ).mapN(Lambda(_, _, t))
      case Let(arg, exp, in, rec, tag) =>
        (traverseType[T, F](exp, bound)(fn), traverseType[T, F](in, bound)(fn))
          .mapN(Let(arg, _, _, rec, tag))
      case l @ Literal(_, _)         => F.pure(l)
      case Match(arg, branches, tag) =>
        val argB = traverseType[T, F](arg, bound)(fn)
        type B = Branch[T]
        def branchFn(b: B): F[B] =
          (
            b.pattern.traverseType(fn(_, bound)),
            b.guard.traverse(traverseType[T, F](_, bound)(fn)),
            traverseType[T, F](b.expr, bound)(fn)
          ).mapN(Branch(_, _, _))
        val branchB = branches.traverse(branchFn)
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

  // Returns a distinct list of free bound type variables
  // in the order they were encountered in traversal
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
  def skolemizeVars[F[_]: Applicative, A](
      vs: NonEmptyList[(Type.Var.Bound, Kind)],
      expr: Expr[A]
  )(
      newSkolemTyVar: (Type.Var.Bound, Kind) => F[Type.Var.Skolem]
  ): F[(NonEmptyList[Type.Var.Skolem], Expr[A])] =
    vs.traverse { case (b, k) => newSkolemTyVar(b, k) }
      .map { skVs =>
        val sksT = skVs.map(Type.TyVar(_))
        val expr1 = substExpr(vs.map(_._1), sksT, expr)
        (skVs, expr1)
      }

  private[bosatsu] def nameIterator(): Iterator[Bindable] =
    Type.allBinders.iterator
      .map(_.name)
      .map(Identifier.Name(_))

  def buildPatternLambda[A](
      args: NonEmptyList[Pattern[(PackageName, Constructor), Type]],
      body: Expr[A],
      outer: A
  ): Expr[A] = {

    /*
     * compute this once if needed, which is why it is lazy.
     * we don't want to traverse body if it is never needed
     */
    lazy val anons = nameIterator()
      .filterNot(allNames(body) ++ args.patternNames)

    type P = Pattern[(PackageName, Constructor), Type]
    def patToArg(p: P): ((Bindable, Option[Type]), Option[P]) =
      p match {
        case Pattern.Annotation(pat, tpe) =>
          val ((b, _), p) = patToArg(pat)
          ((b, Some(tpe)), p)
        case Pattern.Var(arg) =>
          ((arg, None), None)
        case notSimple =>
          val anonBind: Bindable = anons.next()
          ((anonBind, None), Some(notSimple))
      }

    val bindArgsWithP = args.map(patToArg)
    val justArgs = bindArgsWithP.map(_._1)
    val lambdaResult = bindArgsWithP.toList.foldRight(body) {
      case (((_, _), None), body)              => body
      case (((name, _), Some(matchPat)), body) =>
        Match(
          Local(name, outer),
          NonEmptyList.one(Branch(matchPat, None, body)),
          outer
        )
    }
    Lambda(justArgs, lambdaResult, outer)
  }
}
