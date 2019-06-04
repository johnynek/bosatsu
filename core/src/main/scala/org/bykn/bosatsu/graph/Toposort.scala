package org.bykn.bosatsu.graph

import com.stripe.dagon.Memoize

object Toposort {
  /**
   * Build a deterministic topological sort
   * of a graph. The items in the position i depend only
   * on things at position i-1 or less.
   *
   * If this is not a dag, you will stack overflow. If you need to check
   * if you have a dag, see Tree.dagToTree
   */
  def sort[A: Ordering](n: Iterable[A])(fn: A => List[A]): List[List[A]] =
    if (n.isEmpty) Nil
    else {
      // if we are a dag, we can compute the topological sort
      val depth = Memoize.function[A, Int] { case (n, rec) =>
        fn(n) match {
          case Nil => 0
          case nonEmpty => nonEmpty.map(rec).max + 1
        }
      }

      val depths = n.toList.sorted.map { n => (depth(n), n) }
      val len = depths.iterator.map(_._1).max + 1
      val ary = Array.fill(len)(List.newBuilder[A])
      depths.foreach { case (idx, a) =>
        ary(idx) += a
      }
      // the items are already sorted since we added them in sorted order
      ary.map(_.result()).toList
    }
}
