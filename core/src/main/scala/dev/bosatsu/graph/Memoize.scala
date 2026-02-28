package dev.bosatsu.graph

import java.util.concurrent.ConcurrentHashMap
import cats.Monad
import cats.syntax.all._
import dev.bosatsu.Par
import scala.collection.immutable.SortedMap

object Memoize {

  /** This memoizes using a sorted map (not a hashMap) in a non-threadsafe
    * manner returning None, means we cannot compute this function because it
    * loops forever
    */
  def memoizeSorted[A: Ordering, B](
      fn: (A, A => Option[B]) => Option[B]
  ): A => Option[B] = {
    var cache = SortedMap.empty[A, Option[B]]

    new Function[A, Option[B]] { self =>
      def apply(a: A) =
        cache.get(a) match {
          case None =>
            // we have never hit this branch, compute that we are working:
            // if we require this value while computing for a, it is an infinite
            // loop, and we can't compute it
            cache = cache.updated(a, None)
            val b = fn(a, self)
            cache = cache.updated(a, b)
            b
          case Some(b) => b
        }
    }
  }

  /** This memoizes using a hash map in a non-threadsafe manner this throws if
    * you don't have a dag
    */
  def memoizeDagHashed[A, B](fn: (A, A => B) => B): A => B = {
    var cache = Map.empty[A, Option[B]]

    new Function[A, B] { self =>
      def apply(a: A) =
        cache.get(a) match {
          case None =>
            // we have never hit this branch, compute that we are working:
            // if we require this value while computing for a, it is an infinite
            // loop, and we can't compute it
            cache = cache.updated(a, None)
            val b = fn(a, self)
            cache = cache.updated(a, Some(b))
            b
          case Some(Some(b)) => b
          case Some(None)    => sys.error(s"loop found evaluating $a")
        }
    }
  }

  /** This memoizes using a hash map in a threadsafe manner it may loop forever
    * and stack overflow if you don't have a DAG
    */
  def memoizeDagHashedConcurrent[A, B <: AnyRef](
      fn: (A, A => B) => B
  ): A => B = {
    val cache: ConcurrentHashMap[A, B] = new ConcurrentHashMap[A, B]()

    new Function[A, B] { self =>
      def apply(a: A) =
        val cached = cache.get(a)
        if (cached eq null) {
          // if this function is circular fn will loop here blowing
          // the stack
          val res = fn(a, self)
          val _ = cache.put(a, res)
          // doesn't matter if won this race or not
          // two people can concurrently race.
          res
        } else {
          cached
        }
    }
  }

  /** This memoizes using a hash map in a threadsafe manner if the dependencies
    * do not form a dag, you will deadlock
    */
  def memoizeDag[F[_]: CanPromise: Monad, A, B](
      fn: (A, A => F[B]) => F[B]
  ): A => F[B] = {
    val canPromise = summon[CanPromise[F]]
    val cache: ConcurrentHashMap[A, canPromise.Promise[B]] =
      new ConcurrentHashMap[A, canPromise.Promise[B]]()

    new Function[A, F[B]] { self =>
      def apply(a: A): F[B] =
        canPromise
          .delay {
            val prom = canPromise.unsafeNewPromise[B]
            val prevProm = cache.putIfAbsent(a, prom)
            (prom, prevProm)
          }
          .flatMap { case (prom, prevProm) =>
            if (prevProm eq null) {
              val resF = fn(a, self)
              canPromise.completeWith(prom, resF) *> resF
            } else {
              canPromise.wait(prevProm)
            }
          }
    }
  }

  /** This memoizes using a hash map in a threadsafe manner if the dependencies
    * do not form a dag, you will deadlock
    */
  def memoizeDagFuture[A, B](
      fn: (A, A => Par.F[B]) => Par.F[B]
  )(implicit ec: Par.EC): A => Par.F[B] =
    memoizeDag[Par.F, A, B](fn)
}
