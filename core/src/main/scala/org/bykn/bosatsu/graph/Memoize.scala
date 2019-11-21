package org.bykn.bosatsu.graph

import java.util.concurrent.ConcurrentHashMap
import org.bykn.bosatsu.Par
import scala.collection.immutable.SortedMap

object Memoize {

  /**
   * This memoizes using a sorted map (not a hashMap) in a non-threadsafe manner
   * returning None, means we cannot compute this function because it loops forever
   */
  def memoizeSorted[A: Ordering, B](fn: (A, A => Option[B]) => Option[B]): A => Option[B] = {
    // this should be volatile if fn uses threads, but
    // we are only using it below and can see that isn't the case
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

  /**
   * This memoizes using a hash map in a non-threadsafe manner
   * this throws if you don't have a dag
   */
  def memoizeDagHashed[A, B](fn: (A, A => B) => B): A => B = {
    // this should be volatile if fn uses threads, but
    // we are only using it below and can see that isn't the case
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
          case Some(None) => sys.error(s"loop found evaluating $a")
        }
    }
  }

  /**
   * This memoizes using a hash map in a threadsafe manner
   * if the dependencies do not form a dag, you will deadlock
   */
  def memoizeDagFuture[A, B](fn: (A, A => Par.F[B]) => Par.F[B]): A => Par.F[B] = {
    val cache: ConcurrentHashMap[A, Par.P[B]] = new ConcurrentHashMap[A, Par.P[B]]()

    new Function[A, Par.F[B]] { self =>
      def apply(a: A) = {
        val prom = Par.promise[B]
        val prevProm = cache.putIfAbsent(a, prom)
        if (prevProm eq null) {
          // no one was running this job, we have to
          val resFut = fn(a, self)
          Par.complete(prom, resFut)
          resFut
        }
        else {
          // someone else is already working:
          Par.toF(prevProm)
        }
      }
    }
  }
}
