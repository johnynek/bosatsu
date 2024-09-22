package org.bykn.bosatsu

import cats.Defer

sealed abstract class ToyIO[-R, +E, +A]

object ToyIO {
  case class Pure[A](get: A) extends ToyIO[Any, Nothing, A]
  case class Err[E](get: E) extends ToyIO[Any, E, Nothing]
  case class FlatMap[R, E, A, B](init: ToyIO[R, E, A], fn: A => ToyIO[R, E, B]) extends ToyIO[R, E, B]
  case class RecoverWith[R, E, E1, A](init: ToyIO[R, E, A], fn: E => ToyIO[R, E1, A]) extends ToyIO[R, E1, A]
  /**
   * fix(f) = f(fix(f))
   */
  case class ApplyFix[R, E, A, B](arg: A, fixed: (A => ToyIO[R, E, B]) => (A => ToyIO[R, E, B])) extends ToyIO[R, E, B] {
    def step: ToyIO[R, E, B] =
      // it is temping to simplify fixed(fix(fixed)) == fix(fixed)
      // but fix(af.fixed)(af.arg) == af
      // so that doesn't make progress. We need to apply
      // af.fixed at least once to create a new value
      fixed(Fix(fixed))(arg)
  }
  case class Fix[R, E, A, B](fn: (A => ToyIO[R, E, B]) => (A => ToyIO[R, E, B])) extends Function1[A, ToyIO[R, E, B]] {
    def apply(a: A): ToyIO[R, E, B] = ApplyFix(a, fn)
  }
  case class ReadEnv[R]() extends ToyIO[R, Nothing, R]
  case class RemapEnv[R1, R2, E, A](fn: R1 => R2, io: ToyIO[R2, E, A]) extends ToyIO[R1, E, A]

  implicit class ToyIOMethods[R, E, A](private val io: ToyIO[R, E, A]) extends AnyVal {
    def flatMap[R1 <: R, E1 >: E, B](fn: A => ToyIO[R1, E1, B]): ToyIO[R1, E1, B] =
      FlatMap(io, fn)

    def map[B](fn: A => B): ToyIO[R, E, B] = flatMap(a => Pure(fn(a)))
    def recoverWith[E1](fn: E => ToyIO[R, E1, A]): ToyIO[R, E1, A] =
      RecoverWith(io, fn)

    def remapEnv[R1](fn: R1 => R): ToyIO[R1, E, A] =
      RemapEnv(fn, io)

    def run(env: R): Either[E, A] = ToyIO.run(env, io)
  }

  val unit: ToyIO[Any, Nothing, Unit] = Pure(())

  def readEnv[R]: ToyIO[R, Nothing, R] = ReadEnv()

  def pure[A](a: A): ToyIO[Any, Nothing, A] = Pure(a)

  def defer[R, E, A](io: => ToyIO[R, E, A]): ToyIO[R, E, A] =
    unit.flatMap(_ => io)

  def delay[A](a: => A): ToyIO[Any, Nothing, A] =
    defer(Pure(a))

  def raiseError[E](e: E): ToyIO[Any, E, Nothing] = Err(e)

  // fix(f) = f(fix(f))
  def fix[R, E, A, B](recur: (A => ToyIO[R, E, B]) => (A => ToyIO[R, E, B])): A => ToyIO[R, E, B] =
    Fix(recur)

  sealed trait Stack[R, E, A, E1, A1]

  case class Done[R, E1, A1, E, A](ev: E1 =:= E, av: A1 =:= A) extends Stack[R, E1, A1, E, A]
  case class FMStep[R, E, E1, A, B, B1](fn: A => ToyIO[R, E, B], stack: Stack[R, E, B, E1, B1]) extends Stack[R, E, A, E1, B1]
  case class RecStep[R, E, E1, E2, A, B](fn: E => ToyIO[R, E1, A], stack: Stack[R, E1, A, E2, B]) extends Stack[R, E, A, E2, B]
  case class Restore[R, R1, E, A, E1, A1](env: R1, stack: Stack[R1, E1, A1, E, A]) extends Stack[R, E1, A1, E, A]

  def run[R, E, A](env: R, io: ToyIO[R, E, A]): Either[E, A] = {

    @annotation.tailrec
    def loop[R1, E1, A1](env: R1, arg: ToyIO[R1, E1, A1], stack: Stack[R1, E1, A1, E, A]): Either[E, A] =
      arg match {
        case FlatMap(init, fn) =>
          loop(env, init, FMStep(fn, stack))
        case p @ Pure(get) =>
          stack match {
            case Done(_, av) => Right(av(get))
            case FMStep(fn, stack) =>
              loop(env, fn(get), stack)
            case RecStep(_, stack) =>
              // this isn't an error, ignore recovery
              loop(env, p, stack)
            case Restore(env, stack) =>
              loop(env, p, stack)
          }
        case e @ Err(get) =>
          stack match {
            case Done(ev, _) => Left(ev(get))
            case FMStep(_, stack) =>
              // this is an error, unwind the stack
              loop(env, e, stack)
            case RecStep(fn, stack) =>
              loop(env, fn(get), stack)
            case Restore(env, stack) =>
              loop(env, e, stack)
          }
        case rw: RecoverWith[r, e, e1, a] =>
          loop(env, rw.init, RecStep(rw.fn, stack))
        case af: ApplyFix[r, e, a, b] =>
          // fixed(fix(fixed)) = fix(fixed)
          // take a step here,
          // this may never terminate, because there is no
          // promise that a general recursive function terminates,
          // but it won't blow the stack
          loop(env, af.step, stack)
        case _: ReadEnv[r] =>
          // r <:< R
          loop(env, Pure(env), stack)
        case remap: RemapEnv[r1, r2, e, a] =>
          val restore: Stack[r2, E1, A1, E, A] = Restore(env, stack)
          loop[r2, E1, A1](remap.fn(env), remap.io, restore)
      }

    loop(env, io, Done[R, E, A, E, A](implicitly, implicitly))
  }


  def loopFromDefer[F[_]: Defer, A, B](fn: (A => F[B]) => (A => F[B])): A => F[B] =
    new Function1[A, F[B]] { self =>
      lazy val loop = fn(self)

      def apply(a: A) = Defer[F].defer(loop(a))
    }
}