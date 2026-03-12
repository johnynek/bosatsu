package dev.bosatsu

import cats.{Monad, Monoid, Semigroup}

type Writer[M, +A] = Writer.Tpe[M, A]

/**
 * An optimized Writer without the WriterT cost from cats
 */
object Writer {
  opaque type Tpe[M, +A] = (M, A)

  extension [M, A](tpe: Tpe[M, A]) {
    inline def tell(m: M)(using Semigroup[M]): Tpe[M, A] = {
      val m1 = Semigroup[M].combine(tpe._1, m)
      (m1, tpe._2)
    }

    inline def map[B](inline fn: A => B): Tpe[M, B] =
      (written, fn(value))

    inline def flatMap[B](inline fn: A => Tpe[M, B])(using Semigroup[M]): Tpe[M, B] = {
      val tpeB = fn(value)
      (Semigroup[M].combine(written, tpeB._1), tpeB._2)
    }

    inline def written: M = tpe._1
    inline def value: A = tpe._2

    inline def run: (M, A) = tpe
  }

  given [M: Monoid] => Monad[[X] =>> Tpe[M, X]]:
    def pure[A](a: A): (M, A) = (Monoid[M].empty, a)

    override def map[A, B](fa: (M, A))(fn: A => B): (M, B) =
      fa.map(fn)

    def flatMap[A, B](fa: (M, A))(fn: A => (M, B)): (M, B) =
      fa.flatMap(fn)

    def tailRecM[A, B](a: A)(fn: A => (M, Either[A, B])): (M, B) = {
      @annotation.tailrec
      def loop(m: M, a: A): (M, B) =
        val (m1, either) = fn(a)
        val mnext = Semigroup[M].combine(m, m1)
        either match {
          case Left(a) => loop(mnext, a)
          case Right(b) => (mnext, b)
        }

      loop(Monoid[M].empty, a)
    }

  inline def tell[M](m: M): Tpe[M, Unit] = (m, ())
  inline def apply[M, A](m: M, a: A): Tpe[M, A] = (m, a)
}
