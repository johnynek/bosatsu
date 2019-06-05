package org.bykn.bosatsu.graph

import scala.collection.immutable.SortedMap
import cats.implicits._

object Toposort {

  // returning None, means we cannot compute this function because it loops forever
  private def memoize[A: Ordering, B](fn: (A, A => Option[B]) => Option[B]): A => Option[B] = {
    // this should be volatie if fn uses threads, but
    // we are only using it below and can see that isn't the case
    var cache = SortedMap.empty[A, Option[B]]

    lazy val res: A => Option[B] = { (a: A) =>
      cache.get(a) match {
        case None =>
          // we have never hit this branch, compute that we are working:
          // if we require this value while computing for a, it is an infinite
          // loop, and we can't compute it
          cache = cache.updated(a, None)
          val b = fn(a, res)
          cache = cache.updated(a, b)
          b
        case Some(b) => b
      }
    }

    res
  }

  /**
   * A result is the subdag in layers,
   * as well as a set of loopNodes (a sorted list of nodes that don't form a dag)
   */
  sealed abstract class Result[A] {
    // these are the nodes which depend on a cyclic subgraph
    def loopNodes: List[A]
    // This returns the dag portion of the graph
    def layers: Vector[List[A]]
    // if this is success, return Some with the layers
    def toSuccess: Option[Vector[List[A]]] =
      this match {
        case Success(res) => Some(res)
        case Failure(_, _) => None
      }

    // true if each layer has exactly one item in it
    def layersAreTotalOrder: Boolean =
      layers.forall(_.lengthCompare(1) == 0)

    def isSuccess: Boolean =
      this match {
        case Success(_) => true
        case Failure(_, _) => false
      }

    def isFailure: Boolean = !isSuccess
  }
  final case class Success[A](layers: Vector[List[A]]) extends Result[A] {
    def loopNodes = Nil
  }
  final case class Failure[A](loopNodes: List[A], layers: Vector[List[A]]) extends Result[A]

  /**
   * Build a deterministic topological sort
   * of a graph. The items in the position i depend only
   * on things at position i-1 or less.
   *
   * return a result which tells us the layers of the dag, and the non-dag nodes
   */
  def sort[A: Ordering](n: Iterable[A])(fn: A => List[A]): Result[A] =
    if (n.isEmpty) Success(Vector.empty)
    else {
      // save this to avoid some realloations
      val someZero: Option[Int] = Some(0)
      val depth = memoize[A, Int] { (n, rec) =>
        fn(n) match {
          case Nil => someZero
          case nonEmpty =>
            nonEmpty.traverse(rec).map(_.max + 1)
        }
      }
      val res = n
        .toList
        // go through in a deterministic order
        .sorted
        .map { n =>
          depth(n) match {
            case None => Left(n)
            case Some(d) => Right((d, n))
          }
        }

      // we use a var to avoid traversing twice in the common case of good
      // dags
      var bad = false
      val goodRes = {
        val goodIt = res.iterator.collect { case Right((d, _)) => d }
        if (!goodIt.hasNext) {
          // we have to be bad if we aren't good
          bad = true
          Vector.empty
        }
        else {
          val len = goodIt.max + 1
          val ary = Array.fill(len)(List.newBuilder[A])
          res.foreach {
            case Right((idx, a)) => ary(idx) += a
            case Left(_) => bad = true
          }

          // the items are already sorted since we added them in sorted order
          ary.iterator.map(_.result()).toVector
        }
      }
      if (bad) Failure(res.collect { case Left(n) => n }, goodRes)
      else Success(goodRes)
    }
}
