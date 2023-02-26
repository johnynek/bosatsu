package org.bykn.bosatsu.rankn

import cats.{StackSafeMonad, Eval}
// java HashMap performs better than scala in most benchmarks
import scala.collection.mutable.{LongMap => MutableMap}
import java.util.concurrent.atomic.AtomicLong

/**
 * This gives a mutable reference in a monadic context
 */
sealed trait Ref[A] {
  def get: RefSpace[A]
  def set(a: A): RefSpace[Unit]
  def reset: RefSpace[Unit]
  def update[B](fn: A => (A, B)): RefSpace[B] =
    for {
      a <- get
      (a1, b) = fn(a)
      _ <- set(a1)
    } yield b
}

sealed abstract class RefSpace[+A] {
  final def run: Eval[A] =
    runState(new AtomicLong(0L), new MutableMap)

  protected def runState(al: AtomicLong, state: MutableMap[Any]): Eval[A]

  def map[B](fn: A => B): RefSpace[B] =
    RefSpace.Map(this, fn)

  def flatMap[B](fn: A => RefSpace[B]): RefSpace[B] =
    RefSpace.FlatMap(this, fn)
}

object RefSpace {
  private case class Pure[A](value: Eval[A]) extends RefSpace[A] {
    protected def runState(al: AtomicLong, state: MutableMap[Any]) =
      value
  }
  private case class AllocRef[A](handle: Long, init: A) extends RefSpace[A] with Ref[A] {
    def get = this
    def set(a: A) = SetRef(handle, a)
    val reset = Reset(handle)

    protected def runState(al: AtomicLong, state: MutableMap[Any]): Eval[A] =
      Eval.now {
        state.get(handle) match {
          case Some(res) =>
            // we know this must be an A
            res.asInstanceOf[A]
          case None => init
        }
      }
  }
  private case class SetRef(handle: Long, value: Any) extends RefSpace[Unit] {
    protected def runState(al: AtomicLong, state: MutableMap[Any]): Eval[Unit] =
      { state.put(handle, value); Eval.Unit }
  }
  private case class Reset(handle: Long) extends RefSpace[Unit] {
    protected def runState(al: AtomicLong, state: MutableMap[Any]): Eval[Unit] =
      { state.remove(handle); Eval.Unit }
  }
  private case class Alloc[A](init: A) extends RefSpace[Ref[A]] {
    protected def runState(al: AtomicLong, state: MutableMap[Any]) =
      Eval.now(AllocRef(al.getAndIncrement, init))
  }

  private case class Map[A, B](init: RefSpace[A], fn: A => B) extends RefSpace[B] {
    protected def runState(al: AtomicLong, state: MutableMap[Any]) =
      Eval.defer(init.runState(al, state)).map(fn)
  }

  private case class FlatMap[A, B](init: RefSpace[A], fn: A => RefSpace[B]) extends RefSpace[B] {
    protected def runState(al: AtomicLong, state: MutableMap[Any]): Eval[B] =
      Eval.defer(init.runState(al, state))
        .flatMap { a =>
          fn(a).runState(al, state)
        }
  }

  def pure[A](a: A): RefSpace[A] = liftEval(Eval.now(a))

  def liftEval[A](eval: Eval[A]): RefSpace[A] = Pure(eval)

  def newRef[A](initial: A): RefSpace[Ref[A]] =
    Alloc(initial)

  implicit val monadForRefSpace: StackSafeMonad[RefSpace] =
    new StackSafeMonad[RefSpace] {
      def pure[A](a: A): RefSpace[A] = RefSpace.pure(a)
      override def map[A, B](fa: RefSpace[A])(fn: A => B): RefSpace[B] =
        fa.map(fn)
      def flatMap[A, B](fa: RefSpace[A])(fn: A => RefSpace[B]): RefSpace[B] =
        fa.flatMap(fn)
    }

  val unit: RefSpace[Unit] = pure(())

  // a counter that starts at 0
  val allocCounter: RefSpace[RefSpace[Long]] =
    RefSpace.newRef(0L)
      .map { ref =>
        for {
          a <- ref.get
          _ <- ref.set(a + 1L)
        } yield a
      }

}
