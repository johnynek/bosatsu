package org.bykn.bosatsu.pattern

/**
 * These are set operations we can do on patterns
 */
trait SetOps[A] {

  /**
   * a representation of the set with everything in it
   * not all sets have upper bounds we can represent
   */
  def top: Option[A]

  /**
   * if everything is <= A, maybe more than one representation of top
   */
  def isTop(a: A): Boolean

  /**
   * intersect two values and return a union represented as a list
   */
  def intersection(a1: A, a2: A): List[A]

  /**
   * remove a2 from a1 return a union represented as a list
   *
   * this should be the tightest upperbound we can find
   */
  def difference(a1: A, a2: A): List[A]

  /**
   * This should unify the union into the fewest number
   * of patterns without changing the meaning of the union
   */
  def unifyUnion(u: List[A]): List[A]

  /**
   * if true, all elements in a are in b,
   * if false, there is no promise
   *
   * this should be a reasonable cheap operation
   * that is allowed to say no in order
   * to avoid very expensive work
   */
  def subset(a: A, b: A): Boolean

  /**
   * Remove all items in p2 from all items in p1
   * and unify the remaining union
   */
  def differenceAll(p1: List[A], p2: List[A]): List[A] =
    p2.foldLeft(p1) { (p1s, p) =>
      // remove p from all of p1s
      p1s.flatMap(difference(_, p))
    }
}

