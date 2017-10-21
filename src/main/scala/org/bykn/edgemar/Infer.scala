package org.bykn.edgemar

/**
 * This is a scala port of the example of Hindley Milner inference
 * here: http://dev.stephendiehl.com/fun/006_hindley_milner.html
 */

import cats.data.{ EitherT, RWST, StateT }
import cats.{ Eval, Monad, MonadError, Traverse }
import cats.implicits._

case class Unique(id: Long) {
  def next: Unique =
    if (id == Long.MaxValue) sys.error("overflow")
    else Unique(id + 1L)
}

sealed abstract class TypeError
object TypeError {
  case class UnificationFail(left: Type, right: Type) extends TypeError
  // This is a logic error that should really never happen
  case class UnificationMismatch(lefts: List[Type], rights: List[Type]) extends TypeError
  case class InfiniteType(name: String, tpe: Type) extends TypeError
  case class Unbound(name: String) extends TypeError
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

case class Constraint(left: Type, right: Type)

case class Unifier(subst: Subst, constraints: List[Constraint])

object Unifier {
  def empty: Unifier = Unifier(Subst.empty, Nil)
}

object Inference {

  // this is a REPL test method we will remove
  // later.
  def infer(str: String): Scheme = {
    import fastparse.all._
    Parser.expr.parse(str) match {
       case Parsed.Success(exp, _) =>
         Expr.evaluate(exp).right.get._2
      case _ => sys.error("failed")
    }
  }

  type Infer[A] = RWST[EitherT[Eval, TypeError, ?], TypeEnv, Set[Constraint], Unique, A]

  type Solve[A] = StateT[EitherT[Eval, TypeError, ?], Unifier, A]

  def addConstraint(t1: Type, t2: Type): Infer[Unit] =
    RWST.tell(Set(Constraint(t1, t2)))

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

  def unifies(a: Type, b: Type): Solve[Unifier] =
    (a, b) match {
      case (left, right) if left == right =>
        Monad[Solve].pure(Unifier.empty)
      case (Type.Var(tvar), t) => bind(tvar, t)
      case (t, Type.Var(tvar)) => bind(tvar, t)
      case (Type.Arrow(fa, ta), Type.Arrow(fb, tb)) =>
        unifyMany(List(fa, ta), List(fb, tb))
      case (faila, failb) =>
        MonadError[Solve, TypeError].raiseError(TypeError.UnificationFail(faila, failb))
    }

  // do a pointwise unification
  def unifyMany(as: List[Type], bs: List[Type]): Solve[Unifier] =
    (as, bs) match {
      case (Nil, Nil) => Monad[Solve].pure(Unifier.empty)
      case (ha :: ta, hb :: tb) =>
        for {
          sc1 <- unifies(ha, hb)
          Unifier(s1, c1) = sc1
          ta1 = Substitutable[List[Type]].apply(s1, ta)
          tb1 = Substitutable[List[Type]].apply(s1, tb)
          sc2 <- unifyMany(ta1, tb1)
          Unifier(s2, c2) = sc2
        } yield Unifier(s2.compose(s1), c1 reverse_::: c2)
      case (ma, mb) =>
        MonadError[Solve, TypeError].raiseError(TypeError.UnificationMismatch(ma, mb))

    }

  def bind(tvar: String, tpe: Type): Solve[Unifier] =
    tpe match {
      case Type.Var(v) if tvar == v =>
        Monad[Solve].pure(Unifier.empty)
      case t if Substitutable[Type].occurs(tvar, t) =>
        MonadError[Solve, TypeError].raiseError(TypeError.InfiniteType(tvar, t))
      case _ =>
        Monad[Solve].pure(Unifier(Subst(Map(tvar -> tpe)), Nil))
    }

  val solver: Solve[Subst] = {

    def step(unit: Unit): Solve[Either[Unit, Subst]] = {
      val u = StateT.get: Solve[Unifier]
      u.flatMap {
        case Unifier(sub, Nil) => Monad[Solve].pure(Right(sub))
        case Unifier(sub, Constraint(a, b) :: tail) =>
          for {
            su1 <- unifies(a, b)
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

  def instantiate(s: Scheme): Infer[Type] =
    Traverse[List].traverse(s.vars)(_ => fresh)
      .map { ts =>
        val subst = Subst(s.vars.zip(ts).toMap)
        Substitutable[Type].apply(subst, s.result)
      }

  def inferExpr(expr: Expr): Either[TypeError, Scheme] =
    inferExpr(TypeEnv.empty, expr)

  def inferExpr(te: TypeEnv, expr: Expr): Either[TypeError, Scheme] = {
    // get the constraints
    val tcE = infer(expr)
      .run(te, Unique(0L))
      .map { case (s, _, a) => (a, s) }
      .value
      .value

    // now solve
    for {
      tc <- tcE
      (tpe, cons) = tc
      unif <- runSolve(cons.toList)
      Unifier(subs, cs) = unif
      st = Substitutable[Type].apply(subs, tpe)
    } yield closeOver(st)
  }

  def lookup(n: String): Infer[Type] = {
    val it: Infer[TypeEnv] = RWST.ask
    it.flatMap { te =>
      te.toMap.get(n) match {
        case None => MonadError[Infer, TypeError].raiseError(TypeError.Unbound(n))
        case Some(scheme) => instantiate(scheme)
      }
    }
  }

  /**
   * Infer the type and generalize all free variables
   */
  def inferScheme(ex: Expr): Infer[Scheme] =
    for {
      env <- (RWST.ask: Infer[TypeEnv])
      // we need to see current constraits, since they are not free variables
      t1c <- infer(ex).transform { (l, s, a) => (l, s, (a, l)) }
      (t1, cons) = t1c
    } yield Substitutable.generalize((env, cons), t1)

  def infer(expr: Expr): Infer[Type] =
    expr match {
      case Expr.Var(n) =>
        lookup(n)

      case Expr.Lambda(arg, e) =>
        for {
          tv <- fresh
          t <- inEnv(arg, Scheme.fromType(tv), infer(e))
        } yield Type.Arrow(tv, t)

      case Expr.Ffi(_, _, scheme) =>
        instantiate(scheme)

      case Expr.App(fn, arg) =>
        for {
          t1 <- infer(fn)
          t2 <- infer(arg)
          tv <- fresh
          _ <- addConstraint(t1, Type.Arrow(t2, tv))
        } yield tv

      case Expr.Let(n, ex, in) =>
        for {
          sc <- inferScheme(ex)
          t2 <- inEnv(n, sc, infer(in))
        } yield t2

      case Expr.Op(e1, op, e2) =>
        for {
          t1 <- infer(e1)
          t2 <- infer(e2)
          tv <- fresh
          u1 = Type.Arrow(t1, Type.Arrow(t2, tv))
          u2 = Operator.typeOf(op)
          _ <- addConstraint(u1, u2)
        } yield tv

      case Expr.Literal(Lit.Integer(_)) => Monad[Infer].pure(Type.intT)
      case Expr.Literal(Lit.Bool(_)) => Monad[Infer].pure(Type.boolT)
      case Expr.If(cond, te, fe) =>
        for {
          tc <- infer(cond)
          t1 <- infer(te)
          t2 <- infer(fe)
          _ <- addConstraint(Type.boolT, tc)
          _ <- addConstraint(t1, t2)
        } yield t2
    }
}

