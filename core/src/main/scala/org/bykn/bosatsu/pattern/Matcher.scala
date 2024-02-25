package org.bykn.bosatsu.pattern

import cats.Eq

trait Matcher[-P, -S, +R] { self =>
  def apply(p: P): S => Option[R]

  def map[R1](fn: R => R1): Matcher[P, S, R1] =
    new Matcher[P, S, R1] {
      def apply(p: P): S => Option[R1] =
        self(p).andThen { optR => optR.map(fn) }
    }
}

object Matcher {
  implicit class InvariantMatcher[P, S, R](val self: Matcher[P, S, R])
      extends AnyVal {
    def mapWithInput[R1](fn: (S, R) => R1): Matcher[P, S, R1] =
      new Matcher[P, S, R1] {
        def apply(p: P): S => Option[R1] = {
          val next = self(p)

          { (s: S) =>
            next(s) match {
              case None    => None
              case Some(r) => Some(fn(s, r))
            }
          }
        }
      }
  }

  private[this] val someUnit = Some(())

  def eqMatcher[A](implicit eqA: Eq[A]): Matcher[A, A, Unit] =
    new Matcher[A, A, Unit] {
      def apply(a: A): A => Option[Unit] = { (s: A) =>
        if (eqA.eqv(a, s)) someUnit else None
      }
    }

  val charMatcher: Matcher[Char, Char, Unit] = eqMatcher(
    Eq.fromUniversalEquals[Char]
  )

  def fnMatch[A]: Matcher[A => Boolean, A, Unit] =
    new Matcher[A => Boolean, A, Unit] {
      def apply(p: A => Boolean) = { (a: A) => if (p(a)) someUnit else None }
    }
}
