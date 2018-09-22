package org.bykn.bosatsu.rankn
import cats.Monad

import cats.implicits._

sealed abstract class Tc[+A] {
  def run(env: Tc.Env): RefSpace[Either[String, A]]

  final def flatMap[B](fn: A => Tc[B]): Tc[B] =
    Tc.FlatMap(this, fn)

  final def runVar(v: Map[String, Type]): RefSpace[Either[String, A]] =
    Tc.Env.init(v).flatMap(run(_))

  final def extendEnv(varName: String, tpe: Type): Tc[A] =
    Tc.ExtendEnv(varName, tpe, this)
}

object Tc {
  implicit val tcMonad: Monad[Tc] =
    new Monad[Tc] {
      def pure[A](a: A) = Tc.pure(a)
      def flatMap[A, B](fa: Tc[A])(fn: A => Tc[B]): Tc[B] = fa.flatMap(fn)
      def tailRecM[A, B](a: A)(fn: A => Tc[Either[A, B]]): Tc[B] =
        TailRecM(a, fn)
    }

  case class Env(uniq: Ref[Long], vars: Map[String, Type])

  object Env {
    def empty: RefSpace[Env] =
      init(Map.empty)

    def init(vars: Map[String, Type]): RefSpace[Env] =
      RefSpace.newRef(0L).map(Env(_, vars))
  }

  sealed abstract class Expected[A]
  object Expected {
    case class Infer[A](ref: Ref[Either[String, A]]) extends Expected[A]
    case class Check[A](value: A) extends Expected[A]
  }

  private case class FlatMap[A, B](fa: Tc[A], fn: A => Tc[B]) extends Tc[B] {
    def run(env: Env) =
      fa.run(env).flatMap {
        case Left(msg) => RefSpace.pure(Left(msg))
        case Right(a) => fn(a).run(env)
      }
  }
  private case class TailRecM[A, B](init: A, fn: A => Tc[Either[A, B]]) extends Tc[B] {
    def run(env: Env) = {
      def step(a: A): RefSpace[Either[A, Either[String, B]]] =
        fn(a).run(env).map {
          case Left(err) => Right(Left(err))
          case Right(Left(a)) => Left(a)
          case Right(Right(a)) => Right(Right(a))
        }

      Monad[RefSpace].tailRecM(init)(step _)
    }
  }

  private case object GetEnv extends Tc[Map[String, Type]] {
    def run(env: Env) = RefSpace.pure(Right(env.vars))
  }

  private case class ExtendEnv[A](varName: String, tpe: Type, in: Tc[A]) extends Tc[A] {
    def run(env: Env) = in.run(env.copy(vars = env.vars.updated(varName, tpe)))
  }

  private case class Lift[A](res: RefSpace[Either[String, A]]) extends Tc[A] {
    def run(env: Env) = res
  }

  private case object NextId extends Tc[Long] {
    def run(env: Env) =
      for {
        thisId <- env.uniq.get
        _ <- env.uniq.set(thisId + 1L)
      } yield Right(thisId)
  }

  def getEnv: Tc[Map[String, Type]] = GetEnv

  def lift[A](rs: RefSpace[A]): Tc[A] =
    Lift(rs.map(Right(_)))

  def fail(msg: String): Tc[Nothing] =
    Lift(RefSpace.pure(Left(msg)))

  def pure[A](a: A): Tc[A] =
    Lift(RefSpace.pure(Right(a)))

  def require(b: Boolean, msg: String): Tc[Unit] =
    if (b) pure(()) else fail(msg)

  // Fails if v is not in the env
  def lookupVarType(v: String): Tc[Type] =
    getEnv.flatMap { env =>
      env.get(v) match {
        case None => fail(s"$v not in scope: $env")
        case Some(t) => pure(t)
      }
    }

  def nextId: Tc[Long] = NextId

  def typeCheck(t: Term): Tc[Type] =
    inferSigma(t).flatMap(zonkType _)

  def getMetaTyVars(tpes: List[Type]): Tc[Set[Type.Meta]] =
    tpes.traverse(zonkType).map(Type.metaTvs(_))

  def quantify(forAlls: Set[Type.Meta], rho: Type.Rho): Tc[Type] = ???

  def inferSigma(e: Term): Tc[Type] =
    for {
      expTy <- inferRho(e)
      envTys <- getEnv
      envTypeVars <- getMetaTyVars(envTys.values.toList)
      resTypeVars <- getMetaTyVars(List(expTy))
      forAllTvs = resTypeVars -- envTypeVars
      q <- quantify(forAllTvs, expTy)
    } yield q

  def skolemize(t: Type): Tc[(List[Type.Var], Type.Rho)] = ???

  def getFreeTyVars(ts: List[Type]): Tc[Set[Type]] = ???

  def checkSigma(t: Term, tpe: Type): Tc[Unit] =
    for {
      skolRho <- skolemize(tpe)
      (skols, rho) = skolRho
      _ <- checkRho(t, rho)
      envTys <- getEnv
      escTvs <- getFreeTyVars(tpe :: envTys.values.toList)
      badTvs = skols.filter(escTvs.toSet)
      _ <- require(badTvs.isEmpty, s"type $tpe not polymorphic enough in $t")
    } yield ()


  def zonkType(t: Type): Tc[Type] = ???

  def checkRho(t: Term, rho: Type.Rho): Tc[Unit] =
    typeCheckRho(t, Expected.Check(rho))

  def inferRho(t: Term): Tc[Type.Rho] =
    for {
      ref <- lift(RefSpace.newRef[Either[String, Type.Rho]](Left(s"inferRho not complete for $t")))
      _ <- typeCheckRho(t, Expected.Infer(ref))
      rho <- (Lift(ref.get): Tc[Type.Rho])
    } yield rho

  def typeCheckRho(term: Term, expect: Expected[Type.Rho]): Tc[Unit] = ???

}
