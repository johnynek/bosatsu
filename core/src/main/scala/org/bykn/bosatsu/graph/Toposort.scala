package org.bykn.bosatsu.graph

import cats.data.NonEmptyList
import cats.syntax.all._

object Toposort {

  /** A result is the subdag in layers, as well as a set of loopNodes (a sorted
    * list of nodes that don't form a dag)
    */
  sealed abstract class Result[A] {
    // these are the nodes which depend on a cyclic subgraph
    def loopNodes: List[A]
    // This returns the dag portion of the graph
    def layers: Vector[NonEmptyList[A]]
    // if this is success, return Some with the layers
    def toSuccess: Option[Vector[NonEmptyList[A]]] =
      this match {
        case Success(res, _) => Some(res)
        case Failure(_, _)   => None
      }

    // true if each layer has exactly one item in it
    def layersAreTotalOrder: Boolean =
      layers.forall(_.tail.isEmpty)

    def isSuccess: Boolean =
      this match {
        case Success(_, _) => true
        case Failure(_, _) => false
      }

    def isFailure: Boolean = !isSuccess
  }
  final case class Success[A](
      layers: Vector[NonEmptyList[A]],
      nfn: A => List[A]
  ) extends Result[A] {
    def loopNodes = Nil
  }
  final case class Failure[A](
      loopNodes: List[A],
      layers: Vector[NonEmptyList[A]]
  ) extends Result[A]

  /** Build a deterministic topological sort of a graph. The items in the
    * position i depend only on things at position i-1 or less.
    *
    * return a result which tells us the layers of the dag, and the non-dag
    * nodes
    */
  def sort[A: Ordering](n: Iterable[A])(fn: A => List[A]): Result[A] =
    if (n.isEmpty) Success(Vector.empty, fn)
    else {
      // save this to avoid some realloations
      val someZero: Option[Int] = Some(0)
      val depth = Memoize.memoizeSorted[A, Int] { (n, rec) =>
        fn(n) match {
          case Nil => someZero
          case nonEmpty =>
            nonEmpty.traverse(rec).map(_.max + 1)
        }
      }
      val res = n.toList
        // go through in a deterministic order
        .sorted
        .map { n =>
          depth(n) match {
            case None    => Left(n)
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
        } else {
          val len = goodIt.max + 1
          val ary = Array.fill(len)(List.newBuilder[A])
          res.foreach {
            case Right((idx, a)) => ary(idx) += a
            case Left(_)         => bad = true
          }

          // the items are already sorted since we added them in sorted order
          ary.iterator.map { bldr =>
            // We know each layer must have at least 1 item
            NonEmptyList.fromListUnsafe(bldr.result())
          }.toVector
        }
      }
      if (bad) Failure(res.collect { case Left(n) => n }, goodRes)
      else Success(goodRes, fn)
    }
}
