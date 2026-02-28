package dev.bosatsu.graph

trait CanPromise[F[_]] {
  type Promise[_] <: AnyRef

  def delay[A](a: => A): F[A]

  // Must only be called inside delay to be safe.
  def unsafeNewPromise[A]: Promise[A]

  def completeWith[A](p: Promise[A], fa: F[A]): F[Unit]
  def wait[A](p: Promise[A]): F[A]
}
