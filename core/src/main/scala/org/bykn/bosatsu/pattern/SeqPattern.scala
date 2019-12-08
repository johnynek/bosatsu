package org.bykn.bosatsu.pattern

object SeqPattern {
  sealed trait Pattern {
    def matchesAny: Boolean =
      this match {
        case Empty => false
        case Cat(Wildcard, t) => t.matchesAny
        case Cat(_, _) => false
      }

    def matchesEmpty: Boolean =
      this match {
        case Empty => true
        case Cat(Wildcard, t) => t.matchesEmpty
        case Cat(_, _) => false
      }

    def isEmpty: Boolean = this == Empty
    def nonEmpty: Boolean = !isEmpty

    def isLit: Boolean =
      this match {
        case Cat(Lit(_), Empty) => true
        case _ => false
      }

    /**
     * Concat that Pattern on the right
     */
    def +(that: Pattern): Pattern =
      Pattern.fromList(toList ::: that.toList)

    def reverse: Pattern =
      Pattern.fromList(toList.reverse)

    def prependWild: Pattern =
      this match {
        case Cat(Wildcard, _) => this
        case notAlreadyWild => Cat(Wildcard, notAlreadyWild)
      }

    def appendString(str: String): Pattern =
      if (str == "") this
      else {
        Pattern.fromList(toList ::: str.toList.map(Lit(_)))
      }

    def matches(str: String): Boolean =
      SeqPattern.matches(this, str)

    def toList: List[Part] =
      this match {
        case Empty => Nil
        case Cat(h, t) => h :: t.toList
      }

    /**
     * If two wilds are adjacent, the left one will always match empty string
     * this normalize just removes the left wild
     *
     * combine adjacent strings
     */
    def normalize: Pattern =
      this match {
        case Empty => Empty
        case Cat(Wildcard, tail@Cat(Wildcard, _)) =>
          tail.normalize
        case Cat(h, tail) =>
          Cat(h, tail.normalize)
      }

    def intersection(that: Pattern): List[Pattern] =
      SeqPattern.intersection(this, that)

    def difference(that: Pattern): List[Pattern] =
      SeqPattern.difference(this, that)
  }

  sealed trait Part {
    def notWild: Boolean = false
  }
  sealed trait Part1 extends Part {
    override def notWild: Boolean = true
  }

  case class Lit(item: Char) extends Part1
  case object AnyChar extends Part1
  // 0 or more characters
  case object Wildcard extends Part

  case object Empty extends Pattern
  case class Cat(head: Part, tail: Pattern) extends Pattern {
    // return the last non-empty
    @annotation.tailrec
    final def rightMost: Part =
      tail match {
        case Empty => head
        case Cat(h, Empty) => h
        case Cat(_, r@Cat(_, _)) => r.rightMost
      }
  }


  def ipart(p1: Part1, p2: Part1): List[Part1] =
    (p1, p2) match {
      case (Lit(c1), Lit(c2)) => if (c1 == c2) p1 :: Nil else Nil
      case (AnyChar, _) => p2 :: Nil
      case (_, AnyChar) => p1 :: Nil
    }

  def dpart(p1: Part1, p2: Part1): List[Part1] =
    (p1, p2) match {
      case (Lit(c1), Lit(c2)) => if (c1 == c2) Nil else p1 :: Nil
      case (_, AnyChar) => Nil
      case (AnyChar, _) => p1 :: Nil
    }

  /**
   * Compute a list of patterns that matches both patterns exactly
   */
  def intersection(p1: Pattern, p2: Pattern): List[Pattern] =
    (p1, p2) match {
      case (p1, p2) if p1 == p2 => p1 :: Nil
      case (Empty, _) =>
        if (p2.matchesEmpty) p1 :: Nil else Nil
      case (_, Empty) =>
        if (p1.matchesEmpty) p2 :: Nil else Nil
      case (Cat(Wildcard, t1@Cat(Wildcard, _)), _) =>
        // unnormalized
        intersection(t1, p2)
      case (_, Cat(Wildcard, t2@Cat(Wildcard, _))) =>
        // unnormalized
        intersection(p1, t2)
      case (Cat(Wildcard, t1), Cat(Wildcard, t2)) =>
        // *:t1 = (t1 + _:p1)
        // *:t2 = (t2 + _:p2)
        // p1 n p2 = t1 n t2 + (_:p1 n t2) + (t1 n _:p2) + _:(p1 n p2)
        //         = *:((t1 n t2) + (_:p1 n t2) + (t1 n _:p2))
        val i1 = intersection(t1, t2)
        val i2 = intersection(Cat(AnyChar, p1), t2)
        val i3 = intersection(t1, Cat(AnyChar, p2))
        (i1 ::: i2 ::: i3).distinct.map(_.prependWild)
      case (Cat(h1, t1), Cat(Wildcard, t2)) =>
        // h1 : t1 n *:t2 = h1:t1 n (t2 + _:p2) =
        // p1 n t2 + h1 : (t1 n p2)
        val i1 = intersection(p1, t2)
        val i2 = intersection(t1, p2).map(Cat(h1, _))
        (i1 ::: i2).distinct
      case (Cat(Wildcard, _), Cat(_, _)) =>
        // intersection is commutative
        intersection(p2, p1)
      case (Cat(h1: Part1, t1), Cat(h2: Part1, t2)) =>
        val intr = for {
          h <- ipart(h1, h2)
          t <- intersection(t1, t2)
        } yield Cat(h, t)

        intr.distinct
    }

  /**
   * return the patterns that match p1 but not p2
   *
   * For fixed sets A, B if we have (A1 x B1) - (A2 x B2) =
   * A1 = (A1 n A2) u (A1 - A2)
   * A2 = (A1 n A2) u (A2 - A1)
   * so we can decompose:
   *
   * A1 x B1 = (A1 n A2)xB1 u (A1 - A2)xB1
   * A2 x B2 = (A1 n A2)xB2 u (A2 - A1)xB2
   *
   * the difference is:
   * (A1 n A2)x(B1 - B2) u (A1 - A2)xB1
   *
   * The last challenge is we need to operate on
   * s ingle characters, so we need to expand
   * wild into [*] = [] | [_, *], since our pattern
   * language doesn't have a symbol for
   * a single character match we have to be a bit more careful
   *
   * also, we can't exactly represent Wildcard - Lit
   * so this is actually an upperbound on the difference
   * which is to say, all the returned patterns match p1,
   * but some of them also match p2
   */
  def difference(p1: Pattern, p2: Pattern): List[Pattern] =
    (p1, p2) match {
      case (Empty, _) => if (p2.matchesEmpty) Nil else p1 :: Nil
      case (Cat(_: Part1, _), Empty) =>
        // Cat(Part1, _) does not match Empty
        p1 :: Nil
      case (Cat(Wildcard, t1@Cat(Wildcard, _)), _) =>
        // unnormalized
        difference(t1, p2)
      case (_, Cat(Wildcard, t2@Cat(Wildcard, _))) =>
        // unnormalized
        difference(p1, t2)
      case (Cat(Wildcard, t1), Empty) =>
        if (!t1.matchesEmpty) p1 :: Nil
        else {
          // *:t1 = t1 + _:p1
          // _:p1 - [] = _:p1
          Cat(AnyChar, p1) :: difference(t1, Empty)
        }
      case (_, Cat(Wildcard, Empty)) =>
        // matches anything
        Nil
      case (Cat(h1: Part1, t1), Cat(Wildcard, t2)) =>
        // h1:t1 - (*:t2) = h1:t1 - t2 - _:p2
        val d1 =
          for {
            h <- ipart(h1, AnyChar)
            t <- difference(t1, p2)
          } yield Cat(h, t)

        val d2 = dpart(h1, AnyChar).map(Cat(_, t1))
        (d1 ::: d2).distinct.flatMap(difference(_, t2))
      case (Cat(Wildcard, t1), Cat(h2: Part1, t2)) =>
        // *:t1 - (h2:t2) = (t1 - p2) + (_:p1 - h2:t2)
        val d1 =
          for {
            h <- ipart(AnyChar, h2)
            t <- difference(p1, t2)
          } yield Cat(h, t)

        val d2 = dpart(AnyChar, h2).map(Cat(_, p1))
        val d3 = difference(t1, p2)
        (d1 ::: d2 ::: d3).distinct
      case (Cat(h1: Part1, t1), Cat(h2: Part1, t2)) =>
        // h1:t1 - h2:t2 = (h1 n h2):(t1 - t2) + (h1 - h2):t1
        val d1 =
          for {
            h <- ipart(h1, h2)
            t <- difference(t1, t2)
          } yield Cat(h, t)

        val d2 = dpart(h1, h2).map(Cat(_, t1))
        (d1 ::: d2).distinct
      case (c1@Cat(Wildcard, t1), c2@Cat(Wildcard, t2)) =>
        if (c1.rightMost.notWild || c2.rightMost.notWild) {
          // let's avoid the most complex case of both having
          // wild on the front if possible
          difference(c1.reverse, c2.reverse).map(_.reverse)
        }
        else {
          // both start and end with Wildcard
          val d1 = difference(t1, t2)
          if (d1.isEmpty) {
            // if t1 <= t2, then, *:t1 <= *:t2
            Nil
          }
          else {
            // *:t1 = t1 + _:p1
            // *:t2 = t2 + _:p2
            // p1 - p2 = t1 - t2 + _:p1 - t2 + t1 - _:p2 + _:(p1 - p2)
            // p1 - p2 = *:(d1 + d2 + d3)

            val d2 = difference(Cat(AnyChar, p1), t2)
            val d3 = difference(t1, Cat(AnyChar, p2))
            (d1 ::: d2 ::: d3).distinct.map(_.prependWild)
            //p1 :: Nil
          }
        }
    }

  object Pattern {

    def fromList(ps: List[Part]): Pattern =
      ps match {
        case h :: tail =>
          Cat(h, fromList(tail))
        case Nil => Empty
      }

    def apply(s: String): Pattern =
      if (s.isEmpty) Empty
      else Cat(Lit(s.head), apply(s.tail))

    val Wild: Pattern = Cat(Wildcard, Empty)
    val Any: Pattern = Cat(AnyChar, Empty)
  }

  def matches(p: Pattern, str: String): Boolean =
    p match {
      case Empty => str.isEmpty
      case Cat(Lit(c), t) =>
        str.nonEmpty &&
          (c == str.head) &&
          matches(t, str.tail)
      case Cat(AnyChar, t) =>
        str.nonEmpty &&
          matches(t, str.tail)
      case Cat(Wildcard, t) =>
        matchEnd(t, str).nonEmpty
    }

  // return all the places such that fst ++ c ++ snd == str
  def positions(c: Char, str: String): Stream[(String, String)] = {
    def loop(init: Int): Stream[(String, String)] =
      if (init >= str.length) Stream.Empty
      else if (str.charAt(init) == c) {
        (str.substring(0, init), str.substring(init + 1)) #:: loop(init + 1)
      }
      else loop(init + 1)

    loop(0)
  }

  // return all the prefixes where this pattern matches everything
  // after
  def matchEnd(p: Pattern, str: String): Stream[String] =
    p match {
      case Empty => str #:: Stream.Empty
      case Cat(Lit(c), t) =>
        positions(c, str).collect { case (pre, post) if matches(t, post) => pre }
      case Cat(AnyChar, t) =>
        (0 until str.length)
          .toStream
          .map { idx =>
            val prefix = str.substring(0, idx)
            val post = str.substring(idx + 1)
            (prefix, post)
          }
          .collect { case (pre, post) if matches(t, post) => pre }
      case Cat(Wildcard, t) =>
        matchEnd(t, str)
    }
}
