package org.bykn.bosatsu
import scala.compiletime.uninitialized
import cats.Monad

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
  opaque type EC = DummyImplicit
  opaque type ExecutionService = Unit

  implicit def ecFromExecutionContext(implicit ec: DummyImplicit): EC = ec

  implicit def monadF(implicit ec: EC): Monad[F] =
    cats.catsInstancesForId

  def newService(): ExecutionService = ()
  def shutdownService(es: ExecutionService): Unit = es
  def ecFromService(es: ExecutionService): EC = DummyImplicit.dummyImplicit

  def withEC[A](fn: EC => A): A = fn(DummyImplicit.dummyImplicit)

  @inline def start[A](a: => A): F[A] = a

  @inline def await[A](f: F[A]): A = f

  @inline def promise[A]: P[A] = new Box[A]

  @inline def complete[A](p: P[A], f: F[A]): Unit = p.set(f)

  @inline def toF[A](pa: P[A]): F[A] = pa.get
}
