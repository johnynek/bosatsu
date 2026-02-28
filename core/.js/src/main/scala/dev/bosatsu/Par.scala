package dev.bosatsu
import scala.compiletime.uninitialized
import cats.{Monad, Parallel}
import dev.bosatsu.graph.CanPromise

/** This is an abstraction to handle parallel computation, not effectful
  * computation. It is used in places where we have parallelism in expensive
  * operations. Since scalajs cannot handle this, we use conditional build to
  * replace the scalajs with just running directly
  */
object Par {
  private class Box[A] {
    private var value: A = uninitialized
    def set(a: A): Unit =
      value = a
    def get: A = value
  }

  opaque type F[A] = cats.Id[A]
  opaque type P[A] <: AnyRef = Box[A]
  opaque type EC = Unit
  opaque type ExecutionService = Unit

  // There is no actual parallelism here, so we don't need any ExecutionContext
  implicit val alwaysEC: EC = ()

  implicit val monadF: Monad[F] =
    cats.catsInstancesForId

  implicit val parallelF: Parallel[F] =
    cats.Parallel.identity[F]

  given canPromiseF: CanPromise[F] with {
    type Promise[A] = P[A]

    def delay[A](a: => A): F[A] =
      start(a)

    def unsafeNewPromise[A]: Promise[A] =
      promise[A]

    def completeWith[A](p: Promise[A], fa: F[A]): F[Unit] = {
      complete(p, fa)
      ()
    }

    def wait[A](p: Promise[A]): F[A] =
      toF(p)
  }

  def newService(): ExecutionService = ()
  def shutdownService(es: ExecutionService): Unit = es
  def ecFromService(es: ExecutionService): EC = ()

  def withEC[A](fn: EC ?=> A): A = fn(using ())
  def noParallelism[A](fn: EC ?=> A): A = fn(using ())

  inline def start[A](a: => A): F[A] = a

  inline def await[A](f: F[A]): A = f

  def promise[A]: P[A] = new Box[A]

  def complete[A](p: P[A], f: F[A]): Unit = p.set(f)

  def toF[A](pa: P[A]): F[A] = pa.get
}
