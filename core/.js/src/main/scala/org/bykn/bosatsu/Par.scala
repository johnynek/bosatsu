package org.bykn.bosatsu

import cats.{Monad, Id, Parallel}

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
  type EC = DummyImplicit
  type ExecutionService = Unit

  def newService(): ExecutionService = ()
  def shutdownService(es: ExecutionService): Unit = es
  def ecFromService(es: ExecutionService): EC = DummyImplicit.dummyImplicit

  def withEC[A](fn: EC => A): A = fn(DummyImplicit.dummyImplicit)

  implicit def orgByknBosatsuParFMonad: Monad[F] =
    cats.catsInstancesForId

  // since Future has already started, standard Parallel.identity is parallel
  implicit def orgByknBosatsuParParallel: Parallel[F] =
    Parallel.identity

  @inline def start[A](a: => A): F[A] = a

  @inline def now[A](a: A): F[A] = a

  @inline def await[A](f: F[A]): A = f

  @inline def promise[A]: P[A] = new Box[A]

  @inline def complete[A](p: P[A], f: F[A]): Unit = p.set(f)

  @inline def toF[A](pa: P[A]): F[A] = pa.get
}

