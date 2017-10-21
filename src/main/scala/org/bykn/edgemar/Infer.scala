package org.bykn.edgemar

/**
 * This is a scala port of the example of Hindley Milner inference
 * here: http://dev.stephendiehl.com/fun/006_hindley_milner.html
 */

import cats.data.{ EitherT, State }
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

object Inference {
  type Infer[A] = EitherT[State[Unique, ?], TypeError, A]

  def closeOver(subs: Subst, t: Type): Scheme =
    generalize(TypeEnv(Map.empty), Substitutable[Type].apply(subs, t))
      .normalized

  def runInfer(in: Infer[(Subst, Type)]): Eval[Either[TypeError, Scheme]] = {
    val sres: Eval[(Unique, Either[TypeError, (Subst, Type)])] = in.value.run(Unique(0L))

    sres.map {
      case (_, Left(err)) => Left(err)
      case (_, Right((s, t))) => Right(closeOver(s, t))
    }
  }

  val fresh: Infer[Type] =
    EitherT[State[Unique, ?], TypeError, Type](
      for {
        u <- State.get[Unique]
        u1 = u.next
        _ <- State.set(u1)
      } yield Right(Type.Var(s"anon${u.id}")))

  def unify(a: Type, b: Type): Infer[Subst] =
    (a, b) match {
      case (Type.Arrow(fa, ta), Type.Arrow(fb, tb)) =>
        for {
          s1 <- unify(fa, fb)
          s2 <- unify(ta, tb)
        } yield s1.compose(s2)
      case (Type.Var(tvar), t) => bind(tvar, t)
      case (t, Type.Var(tvar)) => bind(tvar, t)
      case (Type.Con(a), Type.Con(b)) if a == b =>
        Monad[Infer].pure(Subst.empty)
      case (faila, failb) =>
        MonadError[Infer, TypeError].raiseError(TypeError.UnificationFail(faila, failb))
    }

  def bind(tvar: String, tpe: Type): Infer[Subst] =
    tpe match {
      case Type.Var(v) if tvar == v =>
        Monad[Infer].pure(Subst.empty)
      case t if Substitutable[Type].occurs(tvar, t) =>
        MonadError[Infer, TypeError].raiseError(TypeError.InfiniteType(tvar, t))
      case _ =>
        Monad[Infer].pure(Subst(Map(tvar -> tpe)))
    }

  def instantiate(s: Scheme): Infer[Type] =
    for {
      ts <- Traverse[List].traverse(s.vars)(_ => fresh)
      subst = Subst(s.vars.zip(ts).toMap)
    } yield Substitutable[Type].apply(subst, s.result)

  def generalize(te: TypeEnv, t: Type): Scheme = {
    val missing = Substitutable[Type].typeVars(t) -- Substitutable[TypeEnv].typeVars(te)
    Scheme(missing.toList.sorted, t)
  }

  def inferExpr(expr: Expr): Either[TypeError, Scheme] =
    runInfer(infer(TypeEnv(Map.empty), expr)).value

  def infer(typeEnv: TypeEnv, expr: Expr): Infer[(Subst, Type)] =
    expr match {
      case Expr.Var(n) => typeEnv.lookup(n)
      case Expr.Lambda(arg, e) =>
        for {
          tv <- fresh
          env2 = typeEnv.updated(arg, Scheme(Nil, tv))
          st <- infer(env2, e)
          (s1, t1) = st
        } yield (s1, Substitutable[Type].apply(s1, Type.Arrow(tv, t1)))
      case Expr.Ffi(_, _, scheme) =>
        Inference.instantiate(scheme).map((Subst.empty, _))
      case Expr.App(fn, arg) =>
        for {
          tv <- fresh
          stf <- infer(typeEnv, fn)
          (s1, t1) = stf
          env1 = Substitutable[TypeEnv].apply(s1, typeEnv)
          sta <- infer(env1, arg)
          (s2, t2) = sta
          t11 = Substitutable[Type].apply(s2, t1)
          s3 <- unify(t11, Type.Arrow(t2, tv)) // fn: t1 =:= t2 -> tv
          tvFinal = Substitutable[Type].apply(s3, tv)
        } yield (s3.compose(s2).compose(s1), tvFinal)

      case Expr.Let(n, ex, in) =>
        for {
          st <- infer(typeEnv, ex)
          (s, t) = st
          env1 = Substitutable[TypeEnv].apply(s, typeEnv)
          t1 = generalize(env1, t)
          st2 <- infer(env1.updated(n, t1), in)
          (s2, t2) = st2
        } yield (s2.compose(s), t2)

      case Expr.Op(e1, op, e2) =>
        for {
          st1 <- infer(typeEnv, e1)
          st2 <- infer(typeEnv, e2)
          (s1, t1) = st1
          (s2, t2) = st2
          tv <- fresh
          s3 <- unify(Type.Arrow(t1, Type.Arrow(t2, tv)), Operator.typeOf(op))
        } yield (s3.compose(s2).compose(s1), tv)

      case Expr.Literal(Lit.Integer(_)) =>
        Monad[Infer].pure((Subst.empty, Type.intT))
      case Expr.Literal(Lit.Bool(_)) =>
        Monad[Infer].pure((Subst.empty, Type.boolT))
      case Expr.If(cond, te, fe) =>
        for {
          st1 <- infer(typeEnv, cond)
          (s1, t1) = st1
          st2 <- infer(typeEnv, te)
          (s2, t2) = st2
          st3 <- infer(typeEnv, fe)
          (s3, t3) = st3
          s4 <- unify(t1, Type.boolT)
          s5 <- unify(t2, t3)
          tfinal = Substitutable[Type].apply(s5, t2)
        } yield (s5.compose(s4).compose(s3).compose(s2).compose(s1), tfinal)
    }
}

trait Substitutable[T] {
  def apply(sub: Subst, t: T): T
  def typeVars(t: T): Set[String]

  def occurs(tvar: String, t: T): Boolean =
    typeVars(t).contains(tvar)
}

object Substitutable {
  def apply[T](implicit s: Substitutable[T]): Substitutable[T] = s

  implicit val forType: Substitutable[Type] =
    new Substitutable[Type] {
      def apply(sub: Subst, t: Type): Type =
        t match {
          case c@Type.Con(_) => c
          case v@Type.Var(name) => sub.getOrElse(name, v)
          case Type.Arrow(from, to) => Type.Arrow(apply(sub, from), apply(sub, to))
        }

      def typeVars(t: Type) =
        t match {
          case Type.Con(_) => Set.empty
          case Type.Var(name) => Set(name)
          case Type.Arrow(from, to) => typeVars(from) | typeVars(to)
        }
    }


  implicit val forScheme: Substitutable[Scheme] =
    new Substitutable[Scheme] {
      def apply(sub: Subst, t: Scheme) = {
        // all the names in scheme are shadows so we need
        // to remove them:
        val newSubst = Subst(t.vars.foldLeft(sub.toMap)(_ - _))
        Scheme(t.vars, Substitutable[Type].apply(newSubst, t.result))
      }
      def typeVars(s: Scheme) =
        Substitutable[Type].typeVars(s.result) -- s.vars
    }

  implicit def forList[A: Substitutable]: Substitutable[List[A]] =
    new Substitutable[List[A]] {
      def apply(sub: Subst, t: List[A]): List[A] =
        t.map(Substitutable[A].apply(sub, _))

      def typeVars(as: List[A]) =
        as.foldLeft(Set.empty[String]) { _ | Substitutable[A].typeVars(_) }
    }

  implicit val forTypeEnv: Substitutable[TypeEnv] =
    new Substitutable[TypeEnv] {
      def apply(sub: Subst, te: TypeEnv): TypeEnv =
        TypeEnv(te.toMap.map { case (s, scheme) =>
          s -> Substitutable[Scheme].apply(sub, scheme)
        })

      def typeVars(te: TypeEnv) =
        te.toMap.values.foldLeft(Set.empty[String])(_ | Substitutable[Scheme].typeVars(_))
    }

}
