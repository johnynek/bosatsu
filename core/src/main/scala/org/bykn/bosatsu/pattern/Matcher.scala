package org.bykn.bosatsu.pattern

import cats.Eq

trait Matcher[-P, -S, +R] { self =>
  def apply(p: P): S => Option[R]

  def contramapInput[S1](fn: S1 => S): Matcher[P, S1, R] =
    new Matcher[P, S1, R] {
      def apply(p: P): S1 => Option[R] =
        fn.andThen(self(p))
    }

  def contramapPattern[P1](fn: P1 => P): Matcher[P1, S, R] =
    new Matcher[P1, S, R] {
      def apply(p: P1): S => Option[R] =
        self.apply(fn(p))
    }

  def map[R1](fn: R => R1): Matcher[P, S, R1] =
    new Matcher[P, S, R1] {
      def apply(p: P): S => Option[R1] =
        self(p).andThen { optR => optR.map(fn) }
    }
}

object Matcher {
  implicit class InvariantMatcher[P, S, R](val self: Matcher[P, S, R]) extends AnyVal {
    def mapWithInput[R1](fn: (S, R) => R1): Matcher[P, S, R1] =
      new Matcher[P, S, R1] {
        def apply(p: P): S => Option[R1] = {
          val next = self(p)

          { s: S =>
            next(s) match {
              case None => None
              case Some(r) => Some(fn(s, r))
            }
          }
        }
      }
  }

  def eqMatcher[A](implicit eqA: Eq[A]): Matcher[A, A, Unit] =
    new Matcher[A, A, Unit] {
      val someUnit = Some(())

      def apply(a: A): A => Option[Unit] =
          { (s: A) => if (eqA.eqv(a, s)) someUnit else None }
    }
}
