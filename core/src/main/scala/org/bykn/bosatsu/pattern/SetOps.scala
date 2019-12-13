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

  /**
   * if top is defined
   * a list of matches that would make the current set of matches total
   *
   * Note, a law here is that:
   * missingBranches(te, t, branches).flatMap { ms =>
   *   assert(missingBranches(te, t, branches ::: ms).isEmpty)
   * }
   */
  def missingBranches(top: List[A], branches: List[A]): List[A] = {
    val missing = branches.foldLeft(top) { (missing, nextBranch) =>
      differenceAll(missing, nextBranch :: Nil)
    }
    // filter any unreachable, which can happen when earlier items shadow later
    // ones
    val unreach = unreachableBranches(missing)
    missing.filterNot(unreach.toSet)
  }

  /**
   * if we match these branches in order, which of them
   * are completely covered by previous matches
   */
  def unreachableBranches(branches: List[A]): List[A] = {
    def withPrev(bs: List[A], prev: List[A]): List[(A, List[A])] =
      bs match {
        case Nil => Nil
        case h :: tail =>
          (h, prev.reverse) :: withPrev(tail, h :: prev)
      }

    withPrev(branches, Nil)
      .collect { case (p, prev) if differenceAll(p :: Nil, prev).isEmpty =>
        // if there is nothing, this is unreachable
        p
      }
  }
}

object SetOps {
  def distinct[A](implicit ordA: Ordering[A]): SetOps[A] =
    new SetOps[A] {
      def top: Option[A] = None
      def isTop(a: A): Boolean = false
      def intersection(a1: A, a2: A): List[A] =
        if (ordA.equiv(a1, a2)) a1 :: Nil
        else Nil

      def difference(a1: A, a2: A): List[A] =
        if (ordA.equiv(a1, a2)) Nil
        else a1 :: Nil

      def unifyUnion(u: List[A]): List[A] = {

        def nub(u: List[A]): List[A] =
          u match {
            case Nil | _ :: Nil => u
            case h1 :: (t1@(h2 :: _)) =>
              if (ordA.equiv(h1, h2)) nub(t1)
              else h1 :: nub(t1)
          }

        nub(u.sorted)
      }

      def subset(a: A, b: A): Boolean = ordA.equiv(a, b)
    }

}
