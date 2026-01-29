package dev.bosatsu.set

import cats.Eq
import cats.syntax.eq._

/** These are set operations we can do on patterns
  */
trait SetOps[A] extends Relatable[A] {

  /** a representation of the set with everything in it not all sets have upper
    * bounds we can represent
    */
  def top: Option[A]

  /** if everything is <= A, maybe more than one representation of top
    */
  def isTop(a: A): Boolean

  /** intersect two values and return a union represented as a list
    */
  def intersection(a1: A, a2: A): List[A]

  def relate(a1: A, a2: A): Rel

  /** Return true if a1 and a2 are disjoint
    */
  def disjoint(a1: A, a2: A): Boolean =
    relate(a1, a2) == Rel.Disjoint

  /** remove a2 from a1 return a union represented as a list
    *
    * this should be the tightest upperbound we can find
    */
  def difference(a1: A, a2: A): List[A]

  /** This should unify the union into the fewest number of patterns without
    * changing the meaning of the union
    */
  def unifyUnion(u: List[A]): List[A]

  /** if true, all elements in a are in b, if false, there is no promise
    *
    * this should be a reasonable cheap operation that is allowed to say no in
    * order to avoid very expensive work
    */
  def subset(a: A, b: A): Boolean =
    relate(a, b).isSubtype

  def equiv(a: A, b: A): Boolean =
    relate(a, b) == Rel.Same

  /** Remove all items in p2 from all items in p1 and unify the remaining union
    */
  def differenceAll(p1: List[A], p2: List[A]): List[A] =
    p2.foldLeft(p1) { (p1s, p) =>
      // remove p from all of p1s
      p1s.flatMap(difference(_, p))
    }

  /** if top is defined a list of matches that would make the current set of
    * matches total
    *
    * Note, a law here is that: missingBranches(te, t, branches).flatMap { ms =>
    * assert(missingBranches(te, t, branches ::: ms).isEmpty) }
    */
  def missingBranches(top: List[A], branches: List[A]): List[A] = {
    def clearSubs(branches: List[A], front: List[A]): List[A] =
      branches match {
        case Nil       => Nil
        case h :: tail =>
          if (
            tail.exists(relate(h, _).isSubtype) || front.exists(
              relate(h, _).isSubtype
            )
          ) clearSubs(tail, front)
          else h :: clearSubs(tail, h :: front)
      }
    // we can subtract in any order
    // since a - b - c = a - c - b

    // all permutations can blow up fast, so we only
    // take a fixed number
    // 3! = 6
    val lookahead = 3

    val superSetIsSmaller =
      new Ordering[List[A]] {
        def compare(x: List[A], y: List[A]): Int = {
          val rBigger = differenceAll(x, y).isEmpty
          val lBigger = differenceAll(y, x).isEmpty

          if (lBigger && !rBigger) -1
          else if (rBigger && !lBigger) 1
          else java.lang.Integer.compare(x.size, y.size)
        }
      }
    val normB = clearSubs(branches, Nil)
    given Eq[A] = Eq.instance(equiv)
    val missing = SetOps.greedySearch(lookahead, top, unifyUnion(normB))(
      differenceAll(_, _)
    )(using superSetIsSmaller)

    // filter any unreachable, which can happen when earlier items shadow later
    // ones
    val unreach = unreachableBranches(branches, missing)
    missing.filterNot(unreach.toSet)
  }

  /** if we match these branches in order, which of them are completely covered
    * by previous matches
    */
  def unreachableBranches(branches: List[A]): List[A] =
    unreachableBranches(init = Nil, branches = branches)

  private def unreachableBranches(init: List[A], branches: List[A]): List[A] = {
    def withPrev(bs: List[A], prev: List[A]): List[(A, List[A])] =
      bs match {
        case Nil       => Nil
        case h :: tail =>
          (h, prev.reverse) :: withPrev(tail, h :: prev)
      }

    withPrev(branches, init)
      .collect {
        case (p, prev) if differenceAll(p :: Nil, prev).isEmpty =>
          // if there is nothing, this is unreachable
          p
      }
  }
}

object SetOps {

  def allPerms[A](as: List[A]): List[List[A]] = {
    def loop(sz: Int, as: List[A]): List[List[A]] =
      as match {
        case a :: rest =>
          // now a could be anywhere in the resulting list
          for {
            lst <- loop(sz - 1, rest)
            offset <- 0 until sz
          } yield (lst.take(offset) ::: (a :: lst.drop(offset)))
        case Nil => Nil :: Nil
      }

    loop(as.size, as)
  }

  // we search for the best order to apply the diffs that minimizes the score
  @annotation.tailrec
  final def greedySearch[A: Ordering, B: Eq](
      lookahead: Int,
      union: A,
      diffs: List[B]
  )(fn: (A, List[B]) => A): A =
    diffs match {
      case Nil => union
      case _   =>
        val peek = diffs.take(lookahead)
        val trials = SetOps.allPerms(peek).map { peeks =>
          val u1 = fn(union, peeks)
          (u1, peek.head)
        }
        val smallest = trials.iterator.minBy(_._1)
        // choose a diff that starts the most
        // number of results that are the smallest
        val best = trials
          .collect {
            case (u1, p) if implicitly[Ordering[A]].equiv(u1, smallest._1) => p
          }
          .groupBy(identity)
          .map { case (k, v) => (k, v.size) }
          .maxBy(_._2)
          ._1

        val u1 = fn(union, best :: Nil)
        greedySearch(lookahead, u1, diffs.filterNot(b => Eq[B].eqv(b, best)))(
          fn
        )
    }

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
            case Nil | _ :: Nil         => u
            case h1 :: (t1 @ (h2 :: _)) =>
              if (ordA.equiv(h1, h2)) nub(t1)
              else h1 :: nub(t1)
          }

        nub(u.sorted)
      }

      override def subset(a: A, b: A): Boolean = ordA.equiv(a, b)
      override def relate(a1: A, a2: A): Rel =
        if (ordA.equiv(a1, a2)) Rel.Same
        else Rel.Disjoint
    }

  def fromFinite[A](items: Iterable[A]): SetOps[Set[A]] =
    new SetOps[Set[A]] {
      require(items.nonEmpty, "the empty set is not allowed")

      val itemsSet = items.toSet
      private given Eq[Set[A]] = Eq.fromUniversalEquals
      val top: Option[Set[A]] = Some(itemsSet)
      def isTop(a: Set[A]): Boolean = a === itemsSet

      def toList(s: Set[A]): List[Set[A]] =
        if (s.isEmpty) Nil else s :: Nil

      def intersection(a1: Set[A], a2: Set[A]): List[Set[A]] =
        toList(a1.intersect(a2))

      def difference(a1: Set[A], a2: Set[A]): List[Set[A]] =
        toList(a1 -- a2)

      def unifyUnion(u: List[Set[A]]): List[Set[A]] =
        toList(u.foldLeft(Set.empty[A])(_ | _))

      override def subset(a: Set[A], b: Set[A]): Boolean = a.subsetOf(b)
      private val rel = Relatable.fromSubsetIntersects[Set[A]](
        _.subsetOf(_),
        (a, b) => {
          // Disjoint or Intersects
          val sa = a.size
          val sb = b.size
          if (sa <= sb) a.exists(b)
          else b.exists(a)
        }
      )

      def relate(a: Set[A], b: Set[A]) = rel.relate(a, b)
    }

  // for types with only one value, Null, Unit, Nil
  def unit[A](topA: A): SetOps[A] =
    new SetOps[A] {
      val top: Option[A] = Some(topA)
      val intr = topA :: Nil
      def isTop(a: A): Boolean = true
      def intersection(a1: A, a2: A): List[A] =
        intr

      def difference(a1: A, a2: A): List[A] = Nil

      def unifyUnion(u: List[A]): List[A] =
        if (u.isEmpty) Nil else intr

      def relate(a1: A, a2: A): Rel = Rel.Same
    }

  def imap[A, B](sa: SetOps[A], fn: A => B, invf: B => A): SetOps[B] =
    new SetOps[B] {
      val top: Option[B] = sa.top.map(fn)
      def isTop(b: B): Boolean = sa.isTop(invf(b))
      def intersection(b1: B, b2: B): List[B] =
        sa.intersection(invf(b1), invf(b2)).map(fn)

      def difference(b1: B, b2: B): List[B] =
        sa.difference(invf(b1), invf(b2)).map(fn)

      def unifyUnion(u: List[B]): List[B] =
        sa.unifyUnion(u.map(invf)).map(fn)

      override def subset(a: B, b: B): Boolean =
        sa.subset(invf(a), invf(b))

      def relate(a1: B, a2: B): Rel =
        sa.relate(invf(a1), invf(a2))
    }

  def product[A, B](sa: SetOps[A], sb: SetOps[B]): SetOps[(A, B)] =
    new SetOps[(A, B)] {
      def top: Option[(A, B)] =
        (sa.top, sb.top) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _                  => None
        }

      def isTop(a: (A, B)): Boolean =
        sa.isTop(a._1) && sb.isTop(a._2)

      // a1 x a2 n b1 x b2 = (a1 n b1) x (a2 n b2)
      def intersection(a1: (A, B), a2: (A, B)): List[(A, B)] = {
        val i1 = sa.intersection(a1._1, a2._1)
        if (i1.isEmpty) Nil
        else {
          val i2 = sb.intersection(a1._2, a2._2)

          if (i2.isEmpty) Nil
          else {
            for {
              ia <- i1
              ib <- i2
            } yield (ia, ib)
          }
        }
      }

      /*
       * This seems to be difference of a product of sets. The formula for this
       * seems to be:
       *
       * (a0 x a1) - (b0 x b1) = (a0 - b0) x a1 + (a0 n b0) x (a1 - b1)
       *
       * Note, if a1 - b1 = a1, this becomes:
       * ((a0 - b0) + (a0 n b0)) x a1 = a0 x a1
       *
       * similarly: a0 - b0 = a0, implies a0 n b0 = 0
       * so, the difference is a0 x a1, or no difference...
       *
       * note that a0 - b0 <= a0, so if we have a0 - b0 >= a0, we know a0 - b0 = a0
       */
      def difference(a1: (A, B), a2: (A, B)): List[(A, B)] = {
        val inta = sa.intersection(a1._1, a2._1)
        if (inta.isEmpty) a1 :: Nil
        else {
          val da = sa.difference(a1._1, a2._1)
          given Eq[A] = Eq.instance(sa.equiv)
          // if a1._1 <= da, then a1._1 == da => no diff
          da match {
            case h :: Nil if h === a1._1 => a1 :: Nil
            case _ =>
              val db = sb.difference(a1._2, a2._2)
              given Eq[B] = Eq.instance(sb.equiv)
              // if a1._2 <= db, then a1._2 == db => no diff
              db match {
                case h :: Nil if h === a1._2 => a1 :: Nil
                case _ =>
                  val left = da.map((_, a1._2))
                  val right = for {
                    a <- inta
                    b <- db
                  } yield (a, b)

                  unifyUnion(left ::: right)
              }
          }
        }
      }

      def unifyUnion(u: List[(A, B)]): List[(A, B)] = {
        def step[X, Y](u: List[(X, Y)], sy: SetOps[Y]): Option[List[(X, Y)]] = {
          var change = false
          val u1 = u
            .groupBy(_._1)
            .iterator
            .flatMap { case (x, xys) =>
              val uy = sy.unifyUnion(xys.map(_._2))
              if (uy.size < xys.size) {
                change = true
                uy.map((x, _))
              } else xys
            }
            .toList

          if (change) Some(u1) else None
        }

        @annotation.tailrec
        def loop(u: List[(A, B)]): List[(A, B)] =
          step(u, sb) match {
            case None =>
              step(u.map(_.swap), sa) match {
                case None     => u
                case Some(u2) =>
                  // we got a change unifying a
                  loop(u2.map(_.swap))
              }
            case Some(u1) =>
              // we got a change unifying b
              loop(u1)
          }

        loop(u)
      }

      override def subset(left: (A, B), right: (A, B)): Boolean =
        sa.subset(left._1, right._1) && sb.subset(left._2, right._2)

      override def relate(left: (A, B), right: (A, B)): Rel =
        sa.relate(left._1, right._1).lazyCombine(sb.relate(left._2, right._2))
    }
}
