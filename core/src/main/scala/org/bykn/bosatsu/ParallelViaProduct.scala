package org.bykn.bosatsu

import cats.arrow.FunctionK
import cats.{Parallel, Applicative}

// Implement Parallel by implementing just Product
abstract class ParallelViaProduct[G[_]] extends Parallel[G] { self =>

  type F[A] = G[A]
  val parallel = new FunctionK[G, F] {
    def apply[A](ga: G[A]) = ga
  }

  val sequential = new FunctionK[F, G] {
    def apply[A](fa: F[A]) = fa
  }

  def parallelProduct[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  val applicative: Applicative[F] =
    new Applicative[F] { self =>
      def pure[A](a: A) = monad.pure(a)

      final def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
        monad.map(parallelProduct(ff, fa)) { case (fn, a) => fn(a) }

      override def map[A, B](fa: F[A])(fn: A => B) = monad.map(fa)(fn)

      override def product[A, B](fa: F[A], fb: F[B]) = parallelProduct(fa, fb)
    }
}
