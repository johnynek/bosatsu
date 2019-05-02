package org.bykn.bosatsu.rankn

import cats.Eval
// java HashMap performs better than scala in most benchmarks
import java.util.{HashMap => MutableMap}

/**
 * This gives a mutable reference in a monadic context
 */
sealed abstract class Ref[A] {
  def get: RefSpace[A]
  def set(a: A): RefSpace[Unit]
  def reset: RefSpace[Unit]
}

sealed abstract class RefSpace[+A] {
  final def run: Eval[A] =
    runState(new MutableMap)

  protected def runState(state: MutableMap[AnyRef, Any]): Eval[A]

  def map[B](fn: A => B): RefSpace[B] =
    RefSpace.Map(this, fn)

  def flatMap[B](fn: A => RefSpace[B]): RefSpace[B] =
    RefSpace.FlatMap(this, fn)
}

object RefSpace {
  private case class Pure[A](value: Eval[A]) extends RefSpace[A] {
    protected def runState(state: MutableMap[AnyRef, Any]) =
      value
  }
  private case class AllocRef[A](handle: AnyRef, init: A) extends Ref[A] {
    val get = GetRef(handle, init)
    def set(a: A) = SetRef(handle, a)
    val reset = Reset(handle)
  }
  private case class GetRef[A](handle: AnyRef, ifEmpty: A) extends RefSpace[A] {
    protected def runState(state: MutableMap[AnyRef, Any]): Eval[A] =
      Eval.now {
        val res = state.get(handle)
        if (res == null) ifEmpty
        else {
          // we know this must be an A
          res.asInstanceOf[A]
        }
      }
  }
  private case class SetRef(handle: AnyRef, value: Any) extends RefSpace[Unit] {
    protected def runState(state: MutableMap[AnyRef, Any]): Eval[Unit] =
      { state.put(handle, value); Eval.Unit }
  }
  private case class Reset(handle: AnyRef) extends RefSpace[Unit] {
    protected def runState(state: MutableMap[AnyRef, Any]): Eval[Unit] =
      { state.remove(handle); Eval.Unit }
  }
  private case class Alloc[A](init: A) extends RefSpace[Ref[A]] {
    protected def runState(state: MutableMap[AnyRef, Any]) =
      Eval.now(AllocRef(new AnyRef, init))
  }

  private case class Map[A, B](init: RefSpace[A], fn: A => B) extends RefSpace[B] {
    protected def runState(state: MutableMap[AnyRef, Any]) =
      Eval.defer(init.runState(state)).map(fn)
  }

  private case class FlatMap[A, B](init: RefSpace[A], fn: A => RefSpace[B]) extends RefSpace[B] {
    protected def runState(state: MutableMap[AnyRef, Any]): Eval[B] =
      Eval.defer(init.runState(state))
        .flatMap { a =>
          fn(a).runState(state)
        }
  }

  def pure[A](a: A): RefSpace[A] = Pure(Eval.now(a))

  def newRef[A](initial: A): RefSpace[Ref[A]] =
    Alloc(initial)

}
