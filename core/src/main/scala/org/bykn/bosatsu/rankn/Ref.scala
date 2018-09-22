package org.bykn.bosatsu.rankn
import cats.Monad
import cats.data.State
import scala.collection.mutable.{Map => MutableMap}

sealed abstract class Ref[A] {
  def get: RefSpace[A]
  def set(a: A): RefSpace[Unit]
}

sealed abstract class RefSpace[+A] {
  final def run: A =
    runState(MutableMap.empty)

  protected def runState(state: MutableMap[AnyRef, Any]): A

  def map[B](fn: A => B): RefSpace[B] =
    RefSpace.Map(this, fn)

  def flatMap[B](fn: A => RefSpace[B]): RefSpace[B] =
    RefSpace.FlatMap(this, fn)
}

object RefSpace {
  private case class Pure[A](value: A) extends RefSpace[A] {
    protected def runState(state: MutableMap[AnyRef, Any]): A =
      value
  }
  private case class AllocRef[A](handle: AnyRef, init: A) extends Ref[A] {
    val get = GetRef(handle, init)
    def set(a: A) = SetRef(handle, a)
  }
  private case class GetRef[A](handle: AnyRef, ifEmpty: A) extends RefSpace[A] {
    protected def runState(state: MutableMap[AnyRef, Any]): A =
      state.get(handle) match {
        case None => ifEmpty
        case Some(v) =>
          // we know this must be an A
          v.asInstanceOf[A]
      }
  }
  private case class SetRef(handle: AnyRef, value: Any) extends RefSpace[Unit] {
    protected def runState(state: MutableMap[AnyRef, Any]): Unit =
      { state.put(handle, value); () }
  }
  private case class Alloc[A](init: A) extends RefSpace[Ref[A]] {
    protected def runState(state: MutableMap[AnyRef, Any]): Ref[A] =
      AllocRef(new AnyRef, init)
  }

  private case class Map[A, B](init: RefSpace[A], fn: A => B) extends RefSpace[B] {
    protected def runState(state: MutableMap[AnyRef, Any]): B = fn(init.runState(state))
  }

  private case class FlatMap[A, B](init: RefSpace[A], fn: A => RefSpace[B]) extends RefSpace[B] {
    protected def runState(state: MutableMap[AnyRef, Any]): B =
      fn(init.runState(state)).runState(state)
  }
  private case class TailRecM[A, B](init: A, fn: A => RefSpace[Either[A, B]]) extends RefSpace[B] {
    protected def runState(state: MutableMap[AnyRef, Any]): B = {
      @annotation.tailrec
      def loop(a: A): B =
        fn(a).runState(state) match {
          case Left(a) => loop(a)
          case Right(b) => b
        }

      loop(init)
    }
  }

  def pure[A](a: A): RefSpace[A] = Pure(a)
  def newRef[A](initial: A): RefSpace[Ref[A]] =
    Alloc(initial)

  implicit val refSpaceMonad: Monad[RefSpace] =
    new Monad[RefSpace] {
      def pure[A](a: A) = Pure(a)
      override def map[A, B](fa: RefSpace[A])(fn: A => B): RefSpace[B] = fa.map(fn)
      def flatMap[A, B](fa: RefSpace[A])(fn: A => RefSpace[B]): RefSpace[B] = fa.flatMap(fn)
      def tailRecM[A, B](a: A)(fn: A => RefSpace[Either[A, B]]): RefSpace[B] =
        TailRecM(a, fn)
    }
}
