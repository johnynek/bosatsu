package org.bykn.bosatsu

import cats.{Monad, Id}
import scala.concurrent.ExecutionContext

/**
 * This is an abstraction to handle parallel computation, not effectful
 * computation. It is used in places where we have parallelism in expensive
 * operations. Since scalajs cannot handle this, we use conditional build
 * to replace the scalajs with just running directly
 */
object Par {
  class Box[A] {
    private[this] var value: A = _
    def set(a: A): Unit = {
      value = a
    }
    def get: A = value
  }

  type F[A] = Id[A]
  type P[A] = Box[A]

  implicit def orgByknBosatsuParFMonad(implicit ec: ExecutionContext): Monad[F] =
    cats.catsInstancesForId

  @inline def start[A](a: => A)(implicit ec: ExecutionContext): F[A] =
    a

  @inline def now[A](a: A): F[A] = a

  @inline def await[A](f: F[A]): A = f

  @inline def promise[A]: P[A] = new Box[A]

  @inline def complete[A](p: P[A], f: F[A]): Unit = p.set(f)

  @inline def toF[A](pa: P[A]): F[A] = pa.get
}

