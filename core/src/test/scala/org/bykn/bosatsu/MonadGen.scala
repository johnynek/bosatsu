package org.bykn.bosatsu

import cats.{Monad, Defer}
import org.scalacheck.Gen

object MonadGen {
  implicit val genMonad: Monad[Gen] with Defer[Gen] =
    new Monad[Gen] with Defer[Gen] {
      def pure[A](a: A): Gen[A] = Gen.const(a)
      def defer[A](ga: => Gen[A]): Gen[A] = Gen.lzy(ga)
      override def product[A, B](ga: Gen[A], gb: Gen[B]) = Gen.zip(ga, gb)
      def flatMap[A, B](ga: Gen[A])(fn: A => Gen[B]) = ga.flatMap(fn)
      override def map[A, B](ga: Gen[A])(fn: A => B): Gen[B] = ga.map(fn)
      def tailRecM[A, B](a: A)(fn: A => Gen[Either[A, B]]): Gen[B] =
        Gen.tailRecM(a)(fn)
    }
}
