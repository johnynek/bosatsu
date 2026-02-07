package dev.bosatsu.rankn

import cats.{StackSafeMonad, Eval}
// java HashMap performs better than scala in most benchmarks
import scala.collection.mutable.{LongMap => MutableMap}
import java.util.concurrent.atomic.AtomicLong

/** This gives a mutable reference in a monadic context
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
    runState(new AtomicLong(0L), RefSpace.State.newEmpty())

  protected def runState(al: AtomicLong, state: RefSpace.State): Eval[A]

  def map[B](fn: A => B): RefSpace[B] =
    RefSpace.Map(this, fn)

  def flatMap[B](fn: A => RefSpace[B]): RefSpace[B] =
    RefSpace.FlatMap(this, fn)

  def resetOnLeft[B, C](fn: A => Either[B, C]): RefSpace[Either[B, C]] =
    RefSpace.ResetOnLeft(this, fn)
}

object RefSpace {
  private case class Pure[A](value: Eval[A]) extends RefSpace[A] {
    protected def runState(al: AtomicLong, state: State) =
      value
  }
  private case class AllocRef[A](handle: Long, init: A)
      extends RefSpace[A]
      with Ref[A] {
    def get: RefSpace[A] = this
    def set(a: A): RefSpace[Unit] = SetRef(handle, a)
    val reset: RefSpace[Unit] = Reset(handle)

    protected def runState(al: AtomicLong, state: State): Eval[A] =
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
    protected def runState(al: AtomicLong, state: State): Eval[Unit] = {
      state.put(handle, value); Eval.Unit
    }
  }
  private case class Reset(handle: Long) extends RefSpace[Unit] {
    protected def runState(al: AtomicLong, state: State): Eval[Unit] = {
      state.remove(handle); Eval.Unit
    }
  }
  private case class Alloc[A](init: A) extends RefSpace[Ref[A]] {
    protected def runState(al: AtomicLong, state: State): Eval[Ref[A]] =
      Eval.now(AllocRef(al.getAndIncrement, init))
  }

  private case class Map[A, B](init: RefSpace[A], fn: A => B)
      extends RefSpace[B] {
    protected def runState(al: AtomicLong, state: State) =
      Eval.defer(init.runState(al, state)).map(fn)
  }

  private case class FlatMap[A, B](init: RefSpace[A], fn: A => RefSpace[B])
      extends RefSpace[B] {
    protected def runState(al: AtomicLong, state: State): Eval[B] =
      Eval
        .defer(init.runState(al, state))
        .flatMap { a =>
          fn(a).runState(al, state)
        }
  }

  sealed abstract class State {
    def put(key: Long, value: Any): Unit
    def get(key: Long): Option[Any]
    def remove(key: Long): Unit
  }
  object State {
    // just to bypass discard warning. I assume this is inlined
    @inline private def discard[A](a: A): Unit = ()

    private[RefSpace] class FromMMap(map: MutableMap[Any]) extends State {
      def put(key: Long, value: Any): Unit =
        discard(map.put(key, value))
      def get(key: Long): Option[Any] = map.get(key)
      def remove(key: Long): Unit =
        discard(map.remove(key))
    }

    private[RefSpace] class Fork(under: State, over: MutableMap[Option[Any]])
        extends State {
      def put(key: Long, value: Any): Unit = discard(over.put(key, Some(value)))
      def get(key: Long): Option[Any] =
        over.get(key) match {
          case Some(s) => s
          case None    => under.get(key)
        }
      def remove(key: Long): Unit = discard(over.put(key, None))

      def flush(): Unit =
        over.foreach {
          case (k, Some(v)) => under.put(k, v)
          case (k, None)    => under.remove(k)
        }

      def reset: State = under
    }

    def newEmpty(): State = new FromMMap(MutableMap.empty[Any])
    def fork(state: State): Fork = new Fork(state, MutableMap.empty)
  }

  private case class ResetOnLeft[A, B, C](
      init: RefSpace[A],
      fn: A => Either[B, C]
  ) extends RefSpace[Either[B, C]] {
    protected def runState(al: AtomicLong, state: State): Eval[Either[B, C]] = {
      val forked = State.fork(state)
      init
        .runState(al, forked)
        .map { a =>
          fn(a) match {
            case r @ Right(_) =>
              forked.flush()
              r
            case l @ Left(_) =>
              // just let the forked state disappear
              l
          }
        }
    }
  }

  def pure[A](a: A): RefSpace[A] = liftEval(Eval.now(a))

  def liftEval[A](eval: Eval[A]): RefSpace[A] = Pure(eval)

  def newRef[A](initial: A): RefSpace[Ref[A]] =
    Alloc(initial)

  // This is a Ref that silently ignores all attempts to change it
  // it should almost never be used (really just for mocking or similar)
  def constRef[A](a: A): Ref[A] =
    new Ref[A] {
      def get: RefSpace[A] = Pure(Eval.now(a))
      def set(a: A): RefSpace[Unit] = Pure(Eval.Unit)
      def reset: RefSpace[Unit] = Pure(Eval.Unit)
    }

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
    RefSpace
      .newRef(0L)
      .map { ref =>
        for {
          a <- ref.get
          _ <- ref.set(a + 1L)
        } yield a
      }

}
