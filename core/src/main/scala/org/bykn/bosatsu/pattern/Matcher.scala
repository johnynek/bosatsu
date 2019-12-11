package org.bykn.bosatsu.pattern

trait Matcher[P, S, R] { self =>
  def apply(p: P): S => Option[R]

  def contramapS[S1](fn: S1 => S): Matcher[P, S1, R] =
    new Matcher[P, S1, R] {
      def apply(p: P): S1 => Option[R] =
        fn.andThen(self(p))
    }
}
