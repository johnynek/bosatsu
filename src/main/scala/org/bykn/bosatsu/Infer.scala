package org.bykn.bosatsu

/**
 * This is a scala port of the example of Hindley Milner inference
 * here: http://dev.stephendiehl.com/fun/006_hindley_milner.html
 */

import cats.data.{ EitherT, RWST, StateT, NonEmptyList }
import cats.{ Eval, Monad, MonadError, Traverse }
import cats.implicits._

case class Unique(id: Long) {
  def next: Unique =
    if (id == Long.MaxValue) sys.error("overflow")
    else Unique(id + 1L)
}

case class Subst(toMap: Map[String, Type]) {
  def getOrElse(s: String, t: => Type): Type =
    toMap.getOrElse(s, t)

  def compose(that: Subst): Subst = {
    val m1 = that.toMap.iterator.map { case (s, t) =>
      s -> Substitutable[Type].apply(this, t)
    }.toMap

    Subst(m1 ++ toMap)
  }
}
object Subst {
  def empty: Subst = Subst(Map.empty)
}

case class Constraint(left: Type, right: Type, leftRegion: Region, rightRegion: Region)

case class Unifier(subst: Subst, constraints: List[Constraint])

object Unifier {
  def empty: Unifier = Unifier(Subst.empty, Nil)
}

object Inference {

  type Infer[A] = RWST[EitherT[Eval, TypeError, ?], TypeEnv, Set[Constraint], Unique, A]

  type Solve[A] = StateT[EitherT[Eval, TypeError, ?], Unifier, A]

  def addConstraint(t1: Type, t2: Type, lr: Region, rr: Region): Infer[Unit] =
    RWST.tell(Set(Constraint(t1, t2, lr, rr)))

  def inEnv[A](n: String, scheme: Scheme, inf: Infer[A]): Infer[A] =
    inf.local(_.updated(n, scheme))

  def closeOver(t: Type): Scheme =
    Substitutable.generalize((), t).normalized

  def runSolve(cs: List[Constraint]): Either[TypeError, Unifier] =
    solver.runS(Unifier(Subst.empty, cs)).value.value

  val fresh: Infer[Type] =
    for {
      u <- (RWST.get: Infer[Unique])
      u1 = u.next
      _ <- (RWST.set(u1): Infer[Unit])
    } yield Type.Var(s"anon${u.id}")

  def unifies(a: Type, b: Type, ar: Region, br: Region): Solve[Unifier] =
    (a, b) match {
      case (left, right) if left == right =>
        Monad[Solve].pure(Unifier.empty)
      case (Type.Var(tvar), t) => bind(tvar, t, ar, br)
      case (t, Type.Var(tvar)) => bind(tvar, t, br, ar)
      case (Type.Arrow(fa, ta), Type.Arrow(fb, tb)) =>
        unifyMany(List((fa, fb), (ta, tb)), ar, br)
      case (Type.TypeApply(fa, ta), Type.TypeApply(fb, tb)) =>
        unifyMany(List((fa, fb), (ta, tb)), ar, br)
      case (faila, failb) =>
        MonadError[Solve, TypeError].raiseError(TypeError.UnificationFail(faila, failb, ar, br))
    }

  // do a pointwise unification
  def unifyMany(ts: List[(Type, Type)], ar: Region, br: Region): Solve[Unifier] =
    ts match {
      case Nil => Monad[Solve].pure(Unifier.empty)
      case (ha, hb) :: tail =>
        for {
          sc1 <- unifies(ha, hb, ar, br)
          Unifier(s1, c1) = sc1
          tail1 = Substitutable[List[(Type, Type)]].apply(s1, tail)
          sc2 <- unifyMany(tail1, ar, br)
          Unifier(s2, c2) = sc2
        } yield Unifier(s2.compose(s1), c1 reverse_::: c2)
    }

  def bind(tvar: String, tpe: Type, varRegion: Region, tpeRegion: Region): Solve[Unifier] =
    tpe match {
      case Type.Var(v) if tvar == v =>
        Monad[Solve].pure(Unifier.empty)
      case t if Substitutable[Type].occurs(tvar, t) =>
        MonadError[Solve, TypeError].raiseError(TypeError.InfiniteType(tvar, t, varRegion, tpeRegion))
      case _ =>
        Monad[Solve].pure(Unifier(Subst(Map(tvar -> tpe)), Nil))
    }

  val solver: Solve[Subst] = {

    def step(unit: Unit): Solve[Either[Unit, Subst]] = {
      val u = StateT.get: Solve[Unifier]
      u.flatMap {
        case Unifier(sub, Nil) => Monad[Solve].pure(Right(sub))
        case Unifier(sub, Constraint(a, b, ra, rb) :: tail) =>
          for {
            su1 <- unifies(a, b, ra, rb)
            Unifier(s1, c1) = su1
            sub1 = s1.compose(sub)
            tail1 = Substitutable[List[Constraint]].apply(s1, tail)
            cs = c1 reverse_::: tail1
            newU = Unifier(sub1, cs)
            _ <- StateT.set(newU): Solve[Unit]
          } yield Left(unit) // use u so we don't get a warning... dammit
      }
    }

    Monad[Solve].tailRecM(())(step)
  }

  private def substFor(s: Scheme): Infer[Subst] =
    Traverse[List].traverse(s.vars)(_ => fresh)
      .map { ts =>
        Subst(s.vars.zip(ts).toMap)
      }

  def instantiate(s: Scheme): Infer[Type] =
    substFor(s).map { subst =>
      Substitutable[Type].apply(subst, s.result)
    }

  private def instantiateMatch[T: HasRegion](arg: Type,
    dt: DefinedType,
    branches: NonEmptyList[(Pattern[(PackageName, ConstructorName)], Expr[T])],
    matchTag: T): Infer[(Type, NonEmptyList[(Pattern[(PackageName, ConstructorName)], Expr[(T, Scheme)])])] = {

    def withBind(args: List[Option[String]], ts: List[Type], result: Expr[T]): Infer[(Type, Expr[(T, Scheme)])] =
      inferTypeTag(result)
        .local { te: TypeEnv =>
          args.zip(ts.map(Scheme.fromType _)).foldLeft(te) {
            case (te, (Some(varName), tpe)) => te.updated(varName, tpe)
            case (te, (None, _)) => te
          }
        }

    type Element[A] = (Pattern[(PackageName, ConstructorName)], Expr[A])
    def inferBranch(mp: Map[ConstructorName, List[Type]])(ce: Element[T]): Infer[(Type, Element[(T, Scheme)])] = {
      // TODO make sure we have proven that this map-get is safe:
      val (p@Pattern((_, cname), bindings), branchRes) = ce
      val tpes = mp(cname)
      withBind(bindings, tpes, branchRes).map { case (t, exp) =>
        (t, (p, exp))
      }
    }

    val scheme = dt.typeScheme
    substFor(scheme).flatMap { subst =>
      val cMap = dt.consMap(subst)

      val matchT = Substitutable[Type].apply(subst, scheme.result)

      for {
        branchTypesExpr <- branches.traverse(inferBranch(cMap))
        branchTypes = branchTypesExpr.map { case (tpe, (_, expr)) => (tpe, HasRegion.region(expr.tag._1)) }
        branchTR <- branchTypes.reduceLeftM(Monad[Infer].pure(_)) { case ((leftT, lr), (rightT, rr)) =>
            for {
              _ <- addConstraint(leftT, rightT, lr, rr)
            } yield (rightT, rr)
          }
        (branchT, branchR) = branchTR
        _ <- addConstraint(arg, matchT, HasRegion.region(matchTag), branchR)
      } yield (branchT, branchTypesExpr.map(_._2))
    }
  }

  def inferExpr[T: HasRegion](expr: Expr[T]): Either[TypeError, Expr[(T, Scheme)]] =
    inferExpr(TypeEnv.empty(PackageName(NonEmptyList.of("Infer", "InferExpr"))), expr)

  def inferExpr[T: HasRegion](te: TypeEnv, expr: Expr[T]): Either[TypeError, Expr[(T, Scheme)]] = {

    implicit val subT: Substitutable[T] = Substitutable.opaqueSubstitutable[T]
    implicit val subExpr: Substitutable[Expr[(T, Scheme)]] =
      Substitutable.fromMapFold[Expr, (T, Scheme)]

    runInfer(te, inferTypeTag(expr)).map { case (subT, exprS) =>
      val scheme = closeOver(subT)
      exprS.setTag((expr.tag, scheme))
    }
  }

  def runInfer[A: Substitutable](te: TypeEnv, infa: Infer[A]): Either[TypeError, A] = {
    // get the constraints
    val acE = infa
      .run(te, Unique(0L))
      .map { case (s, _, a) => (a, s) }
      .value
      .value

    // now solve
    for {
      ac <- acE
      (a, cons) = ac
      unif <- runSolve(cons.toList)
      Unifier(subs, _) = unif
      subA = Substitutable[A].apply(subs, a)
    } yield subA
  }

  def lookup(n: String, r: Region): Infer[Type] = {
    val it: Infer[TypeEnv] = RWST.ask
    it.flatMap { te =>
      te.schemeOf(n) match {
        case None => MonadError[Infer, TypeError].raiseError(TypeError.Unbound(n, r))
        case Some(scheme) => instantiate(scheme)
      }
    }
  }

  /**
   * Infer the type and generalize all free variables
   */
  def inferScheme[T: HasRegion](ex: Expr[T]): Infer[(Scheme, Expr[(T, Scheme)])] =
    for {
      env <- (RWST.ask: Infer[TypeEnv])
      // we need to see current constraits, since they are not free variables
      t1c <- inferTypeTag(ex).transform { (l, s, a) => (l, s, (a, l)) }
      ((t1, exprS), cons) = t1c
      scheme = Substitutable.generalize((env, cons), t1)
    } yield (scheme, exprS.setTag((ex.tag, scheme)))

  def infer[T: HasRegion](expr: Expr[T]): Infer[Type] =
    inferTypeTag(expr).map { case (t, _) => t }

  /**
   * Packages are generally just lists of lets, this allows you to infer
   * the scheme for each in the context of the list
   */
  def inferLets[T: HasRegion](ls: List[(String, Expr[T])]): Infer[List[(String, Expr[(T, Scheme)])]] =
    ls match {
      case Nil => Monad[Infer].pure(Nil)
      case (n, ex) :: tail =>
        for {
          scEx <- inferScheme(ex)
          (sc, exS) = scEx
          taili <- inEnv(n, sc, inferLets(tail))
        } yield (n, exS) :: taili
    }

  def inferTypeTag[T: HasRegion](expr: Expr[T]): Infer[(Type, Expr[(T, Scheme)])] =
    expr match {
      case Expr.Var(n, tag) =>
        lookup(n, HasRegion.region(expr.tag)).map { t =>
          (t, Expr.Var(n, (tag, Scheme.fromType(t))))
        }

      case Expr.Lambda(arg, e, tag) =>
        for {
          tv <- fresh
          te <- inEnv(arg, Scheme.fromType(tv), inferTypeTag(e))
          (t, expes) = te
          lt = Type.Arrow(tv, t)
        } yield (lt, Expr.Lambda(arg, expes, (tag, Scheme.fromType(lt))))

      case Expr.App(fn, arg, tag) =>
        for {
          ifn <- inferTypeTag(fn)
          iarg <- inferTypeTag(arg)
          (t1, efn) = ifn
          (t2, earg) = iarg
          tv <- fresh
          _ <- addConstraint(t1, Type.Arrow(t2, tv), HasRegion.region(fn.tag), HasRegion.region(arg.tag))
        } yield (tv, Expr.App(efn, earg, (tag, Scheme.fromType(tv))))

      case Expr.Let(n, ex, in, tag) =>
        for {
          scEx <- inferScheme(ex)
          (sc, exS) = scEx
          iin <- inEnv(n, sc, inferTypeTag(in))
          (t2, ein) = iin
        } yield (t2, Expr.Let(n, exS, ein, (tag, Scheme.fromType(t2))))

      case Expr.Literal(Lit.Integer(i), tag) =>
        Monad[Infer].pure((Type.intT, Expr.Literal(Lit.Integer(i), (tag, Scheme.fromType(Type.intT)))))
      case Expr.Literal(str@Lit.Str(_) ,tag) =>
        Monad[Infer].pure((Type.strT, Expr.Literal(str, (tag, Scheme.fromType(Type.strT)))))
      case Expr.Match(arg, branches, tag) =>
        for {
          env <- (RWST.ask: Infer[TypeEnv])
          dt <- MonadError[Infer, TypeError].fromEither(env.getDefinedType(branches.map { case (Pattern(pc, _), r) => (pc, r) }))
          iarg <- inferTypeTag(arg)
          (targ, earg) = iarg
          ibranch <- instantiateMatch(targ, dt, branches, tag)
          (tbranch, ebranches) = ibranch
        } yield (tbranch, Expr.Match(earg, ebranches, (tag, Scheme.fromType(tbranch))))
    }
}

