package dev.bosatsu

import cats.Monad
import dev.bosatsu.graph.CanPromise
import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration.Duration

/** This is an abstraction to handle parallel computation, not effectful
  * computation. It is used in places where we have parallelism in expensive
  * operations. Since scalajs cannot handle this, we use conditional build to
  * replace the scalajs with just running directly
  */
object Par {
  opaque type F[A] = Future[A]
  opaque type P[A] <: AnyRef = Promise[A]
  opaque type EC = ExecutionContext
  opaque type ExecutionService = java.util.concurrent.ExecutorService

  implicit def ecFromExecutionContext(implicit ec: ExecutionContext): EC = ec

  implicit def monadF(implicit ec: EC): Monad[F] =
    cats.instances.future.catsStdInstancesForFuture

  given canPromiseF(using ec: EC): CanPromise[F] with {
    type Promise[A] = P[A]

    def delay[A](a: => A): F[A] =
      start(a)

    def unsafeNewPromise[A]: Promise[A] =
      promise[A]

    def completeWith[A](p: Promise[A], fa: F[A]): F[Unit] = {
      complete(p, fa)
      Future.successful(())
    }

    def wait[A](p: Promise[A]): F[A] =
      toF(p)
  }

  def newService(): ExecutionService =
    Executors.newWorkStealingPool()

  def shutdownService(es: ExecutionService): Unit =
    es.shutdown()

  def ecFromService(es: ExecutionService): EC =
    ExecutionContext.fromExecutor(es)

  // Used in testing generally, we don't want to make more than one of these per app
  def withEC[A](fn: EC ?=> A): A = {
    val srv = newService()
    try {
      val ec = ecFromService(srv)
      fn(using ec)
    } finally shutdownService(srv)
  }
  def noParallelism[A](fn: EC ?=> A): A =
    fn(using ecFromExecutionContext(using DirectEC.directEC))

  inline def start[A](a: => A)(implicit ec: EC): F[A] =
    Future(a)

  inline def await[A](f: F[A]): A =
    Await.result(f, Duration.Inf)

  inline def promise[A]: P[A] = Promise[A]()

  inline def complete[A](p: P[A], f: F[A]): Unit = {
    p.completeWith(f)
    ()
  }

  inline def toF[A](pa: P[A]): F[A] = pa.future
}
