package dev.bosatsu

import cats.{Monad, Parallel}
import dev.bosatsu.graph.CanPromise
import java.util.concurrent.{
  ExecutorService,
  ForkJoinPool,
  ForkJoinWorkerThread
}
import scala.concurrent.{
  Await,
  BlockContext,
  CanAwait,
  ExecutionContext,
  Future,
  Promise
}
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
  opaque type ExecutionService = ExecutorService

  implicit def ecFromExecutionContext(implicit ec: ExecutionContext): EC = ec

  implicit def monadF(implicit ec: EC): Monad[F] =
    cats.instances.future.catsStdInstancesForFuture

  implicit def parallelF(using ec: EC): Parallel[F] =
    new ParallelViaProduct[Par.F] {
      val monad: Monad[Par.F] = monadF

      def parallelProduct[A, B](
          fa: Par.F[A],
          fb: Par.F[B]
      ): Par.F[(A, B)] =
        fa.zip(fb)
    }

  given canPromiseF(using ec: EC): CanPromise[F] with {
    type Promise[A] = P[A]

    def delay[A](a: => A): F[A] =
      start(a)

    def compute[A](a: => A): F[A] =
      start(a)

    def unsafeNewPromise[A]: Promise[A] =
      promise[A]

    def completeWith[A](p: Promise[A], fa: F[A]): F[Unit] = {
      complete(p, fa)
      Future.unit
    }

    def wait[A](p: Promise[A]): F[A] =
      toF(p)
  }

  def newService(): ExecutionService =
    newService(Parallelism.availableProcessors)

  /** A work-stealing pool that keeps `parallelism` workers running. A task may
    * await other tasks on the same pool: while a worker is blocked in `await`
    * the pool adds a replacement, so nested start/await cannot deadlock at any
    * parallelism.
    */
  def newService(parallelism: Parallelism): ExecutionService =
    new ForkJoinPool(
      parallelism.toInt,
      ManagedBlockingWorker.Factory,
      // no uncaught exception handler: Future captures task failures
      null,
      // FIFO order for tasks that are never joined, as in
      // Executors.newWorkStealingPool
      true
    )

  /** A pool worker that is also the `BlockContext` for code it runs.
    * `Await.result` blocks through `scala.concurrent.blocking`, which uses this
    * to wait inside `ForkJoinPool.managedBlock`. That lets the pool compensate
    * for the blocked worker. A plain `ForkJoinWorkerThread` would use
    * `BlockContext.DefaultBlockContext`, and its pool would lose the worker for
    * as long as it waits.
    */
  final private class ManagedBlockingWorker(pool: ForkJoinPool)
      extends ForkJoinWorkerThread(pool)
      with BlockContext {
    def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = {
      val blocker = new ManagedBlockingWorker.Blocker(() => thunk)
      ForkJoinPool.managedBlock(blocker)
      blocker.result
    }
  }

  private object ManagedBlockingWorker {
    object Factory extends ForkJoinPool.ForkJoinWorkerThreadFactory {
      def newThread(pool: ForkJoinPool): ForkJoinWorkerThread =
        new ManagedBlockingWorker(pool)
    }

    // Only the blocked worker calls these methods, so value needs no
    // synchronization.
    final class Blocker[T](thunk: () => T) extends ForkJoinPool.ManagedBlocker {
      private var value: Option[T] = None

      def block(): Boolean = {
        value = Some(thunk())
        true
      }

      def isReleasable: Boolean = value.isDefined

      def result: T =
        value.getOrElse(sys.error("managedBlock returned before block() ran"))
    }
  }

  def shutdownService(es: ExecutionService): Unit =
    es.shutdown()

  def ecFromService(es: ExecutionService): EC =
    ExecutionContext.fromExecutor(es)

  // Used in testing generally, we don't want to make more than one of these per app
  def withEC[A](fn: EC ?=> A): A =
    withParallelism(Parallelism.availableProcessors)(fn)

  def withParallelism[A](parallelism: Parallelism)(fn: EC ?=> A): A = {
    val srv = newService(parallelism)
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
