package org.bykn.bosatsu

import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration.Duration

/** This is an abstraction to handle parallel computation, not effectful
  * computation. It is used in places where we have parallelism in expensive
  * operations. Since scalajs cannot handle this, we use conditional build to
  * replace the scalajs with just running directly
  */
object Par {
  type F[A] = Future[A]
  type P[A] = Promise[A]
  type EC = ExecutionContext
  type ExecutionService = java.util.concurrent.ExecutorService

  def newService(): ExecutionService =
    Executors.newWorkStealingPool()

  def shutdownService(es: ExecutionService): Unit =
    es.shutdown()

  def ecFromService(es: ExecutionService): EC =
    ExecutionContext.fromExecutor(es)

  // Used in testing generally, we don't want to make more than one of these per app
  def withEC[A](fn: EC => A): A = {
    val srv = newService()
    try {
      val ec = ecFromService(srv)
      fn(ec)
    } finally shutdownService(srv)
  }

  @inline def start[A](a: => A)(implicit ec: EC): F[A] =
    Future(a)

  @inline def await[A](f: F[A]): A =
    Await.result(f, Duration.Inf)

  @inline def promise[A]: P[A] = Promise[A]()

  @inline def complete[A](p: P[A], f: F[A]): Unit = {
    p.completeWith(f)
    ()
  }

  @inline def toF[A](pa: P[A]): F[A] = pa.future
}
