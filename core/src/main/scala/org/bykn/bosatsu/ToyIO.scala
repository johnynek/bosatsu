package org.bykn.bosatsu

sealed abstract class ToyIO[+E, +A]

object ToyIO {
  case class Pure[A](get: A) extends ToyIO[Nothing, A]
  case class Err[E](get: E) extends ToyIO[E, Nothing]
  case class FlatMap[E, A, B](init: ToyIO[E, A], fn: A => ToyIO[E, B]) extends ToyIO[E, B]
  case class RecoverWith[E, E1, A](init: ToyIO[E, A], fn: E => ToyIO[E1, A]) extends ToyIO[E1, A]
  /**
   * fix(f) = f(fix(f))
   */
  case class ApplyFix[E, A, B](arg: A, fixed: (A => ToyIO[E, B]) => (A => ToyIO[E, B])) extends ToyIO[E, B] {
    def step: ToyIO[E, B] =
      // it is temping to simplify fixed(fix(fixed)) == fix(fixed)
      // but fix(af.fixed)(af.arg) == af
      // so that doesn't make progress. We need to apply
      // af.fixed at least once to create a new value
      fixed(Fix(fixed))(arg)
  }
  case class Fix[E, A, B](fn: (A => ToyIO[E, B]) => (A => ToyIO[E, B])) extends Function1[A, ToyIO[E, B]] {
    def apply(a: A): ToyIO[E,B] = ApplyFix(a, fn)
  }

  implicit class ToyIOMethods[E, A](private val io: ToyIO[E, A]) extends AnyVal {
    def flatMap[E1 >: E, B](fn: A => ToyIO[E1, B]): ToyIO[E1, B] =
      FlatMap(io, fn)

    def map[B](fn: A => B): ToyIO[E, B] = flatMap(a => Pure(fn(a)))
    def recoverWith[E1](fn: E => ToyIO[E1, A]): ToyIO[E1, A] =
      RecoverWith(io, fn)

    def run: Either[E, A] = ToyIO.run(io)
  }

  val unit: ToyIO[Nothing, Unit] = Pure(())

  def pure[A](a: A): ToyIO[Nothing, A] = Pure(a)

  def defer[E, A](io: => ToyIO[E, A]): ToyIO[E, A] =
    unit.flatMap(_ => io)

  def delay[A](a: => A): ToyIO[Nothing, A] =
    defer(Pure(a))

  def raiseError[E](e: E): ToyIO[E, Nothing] = Err(e)

  // fix(f) = f(fix(f))
  def fix[E, A, B](recur: (A => ToyIO[E, B]) => (A => ToyIO[E, B])): A => ToyIO[E, B] =
    Fix(recur)

  sealed trait Stack[E, A, E1, A1]

  case class Done[E1, A1, E, A](ev: E1 =:= E, av: A1 =:= A) extends Stack[E1, A1, E, A]
  case class FMStep[E, E1, A, B, B1](fn: A => ToyIO[E, B], stack: Stack[E, B, E1, B1]) extends Stack[E, A, E1, B1]
  case class RecStep[E, E1, E2, A, B](fn: E => ToyIO[E1, A], stack: Stack[E1, A, E2, B]) extends Stack[E, A, E2, B]

  def run[E, A](io: ToyIO[E, A]): Either[E, A] = {

    @annotation.tailrec
    def loop[E1, A1](arg: ToyIO[E1, A1], stack: Stack[E1, A1, E, A]): Either[E, A] =
      arg match {
        case p @ Pure(get) =>
          stack match {
            case Done(_, av) => Right(av(get))
            case FMStep(fn, stack) =>
              loop(fn(get), stack)
            case RecStep(_, stack) =>
              loop(p, stack)
          }
        case e @ Err(get) =>
          stack match {
            case Done(ev, _) => Left(ev(get))
            case FMStep(_, stack) =>
              // unwind the stack
              loop(e, stack)
            case RecStep(fn, stack) =>
              loop(fn(get), stack)
          }
        case FlatMap(init, fn) =>
          loop(init, FMStep(fn, stack))
        case rw: RecoverWith[e, e1, a] =>
          loop(rw.init, RecStep(rw.fn, stack))
        case af: ApplyFix[e, a, b] =>
          // fixed(fix(fixed)) = fix(fixed)
          // take a step here,
          // this may never terminate, because there is no
          // promise that a general recursive function terminates,
          // but it won't blow the stack
          loop(af.step, stack)
      }

    loop(io, Done[E, A, E, A](implicitly, implicitly))
  }
}