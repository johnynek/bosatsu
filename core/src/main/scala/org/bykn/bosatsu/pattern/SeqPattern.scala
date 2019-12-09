package org.bykn.bosatsu.pattern

object SeqPattern {
  sealed trait Pattern {
    def matchesAny: Boolean =
      this match {
        case Empty => false
        case Cat(Wildcard, Empty) => true
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
        case Cat(AnyChar, t) => Cat(AnyChar, Cat(Wildcard, t))
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
        case Cat(Wildcard, Cat(AnyChar, t)) =>
          // move AnyChar out
          val wtn = Cat(Wildcard, t).normalize
          Cat(AnyChar, wtn)
        case Cat(Wildcard, tail@Cat(Wildcard, _)) =>
          // remove duplicate Wildcard
          tail.normalize
        case Cat(h, tail) =>
          Cat(h, tail.normalize)
      }

    def intersection(that: Pattern): List[Pattern] =
      SeqPattern.intersection(this, that)

    def difference(that: Pattern): List[Pattern] =
      SeqPattern.difference(this, that)


    def show: String =
      toList
        .iterator
        .map {
          case Lit('.') => "\\."
          case Lit('*') => "\\*"
          case Lit(c) => c.toString
          case AnyChar => "."
          case Wildcard => "*"
        }
        .mkString
  }

  sealed trait Part {
    def notWild: Boolean = false

    def matches(str: String): Boolean =
      this match {
        case Wildcard => true
        case AnyChar => str.length == 1
        case Lit(c) => (str.length == 1) && (str.charAt(0) == c)
      }
    // return the prefix and the matched region
    def matchEnd(str: String): Stream[(String, String)] =
      this match {
        case Wildcard =>
          // we match all suffixes
          (0 to str.length).toStream.map { idx => (str.substring(0, idx), str.substring(idx, str.length)) }
        case AnyChar =>
          if (str.isEmpty) Stream.Empty
          else {
            (str.init, str.last.toString) #:: Stream.Empty
          }
        case Lit(c) =>
          if (str.isEmpty) Stream.Empty
          else {
            val lastC = str.last
            if (lastC == c) (str.init, c.toString) #:: Stream.Empty
            else Stream.Empty
          }
      }
  }
  sealed trait Part1 extends Part {
    override def notWild: Boolean = true
  }

  object Part {
    implicit val partOrdering: Ordering[Part] =
      new Ordering[Part] {
        def compare(a: Part, b: Part) =
          (a, b) match {
            case (Lit(i1), Lit(i2)) =>
              java.lang.Character.compare(i1, i2)
            case (Lit(_), _) => -1
            case (_, Lit(_)) => 1
            case (AnyChar, AnyChar) => 0
            case (AnyChar, Wildcard) => -1
            case (Wildcard, AnyChar) => 1
            case (Wildcard, Wildcard) => 0
          }
      }
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

  sealed trait Named {
    import Named._

    def unname: Pattern = {
      def loop(n: Named, right: List[Part]): List[Part] =
        n match {
          case Bind(_, n) => loop(n, right)
          case NEmpty => right
          case NCat(first, second) =>
            val r2 = loop(second, right)
            loop(first, r2)
          case NPart(p) => p :: right
        }

      Pattern.fromList(loop(this, Nil))
    }

    def matches(str: String): Option[Map[String, String]] =
      this match {
        case Bind(n, p) =>
          p.matches(str).map(_.updated(n, str))
        case NEmpty => if (str.isEmpty) emptyMatch else None
        case NPart(p) => if (p.matches(str)) emptyMatch else None
        case NCat(fst, snd) =>
          snd.matchEnd(str)
            .map { case (prefix, _, bindings) =>
              fst.matches(prefix).map(_ ++ bindings)
            }
            .collectFirst { case Some(m) => m }
      }

    // return all the matches of the suffix returning the unmatched
    // prefix, matched region and the bindings that have matched
    def matchEnd(str: String): Stream[(String, String, Map[String, String])] =
      this match {
        case Bind(n, p) =>
          p.matchEnd(str)
            .map { case (pre, m, b) => (pre, m, b.updated(n, m)) }
        case NEmpty => (str, "", Map.empty[String, String]) #:: Stream.Empty
        case NPart(p) =>
          p.matchEnd(str)
            .map { case (pre, m) => (pre, m, Map.empty[String, String]) }
        case NCat(fst, snd) =>
          snd.matchEnd(str)
            .flatMap { case (sprefix, msnd, bsnd) =>
              fst.matchEnd(sprefix)
                .map { case (fprefix, mfst, bfst) =>
                  (fprefix, mfst ++ msnd, bfst ++ bsnd)
                }
            }
      }
  }

  object Named {
    val emptyMatch: Option[Map[String, String]] = Some(Map.empty)

    case class Bind(name: String, p: Named) extends Named
    case object NEmpty extends Named
    case class NPart(part: Part) extends Named
    case class NCat(first: Named, second: Named) extends Named
  }

  // Try to unify lists according to the rules:
  // x u [_, *, x] = [*, x]
  // x u [*, _, x] = [*, x]
  // x u [x, *, _] = [x, *]
  // x u [x, _, *] = [x, *]
  //
  // this is an incomplete heuristic now, not a complete solution
  def unifyUnion(union: List[Pattern]): List[Pattern] =
    unifyUnionList {
        union
          .map(_.normalize)
          .distinct
          .map(_.toList)
      }
      .map(Pattern.fromList(_).normalize)
      .sorted

  private[this] val someWild = Some(Wildcard :: Nil)
  private[this] val someNil = Some(Nil)

  private def unifyUnionList(union: List[List[Part]]): List[List[Part]] = {

    def unifyPart(list: List[Part]): Option[List[Part]] =
      list match {
        case AnyChar :: Wildcard :: Nil => someWild
        case Wildcard :: AnyChar :: Nil => someWild
        case Wildcard :: Nil => someWild
        case Nil => someNil
        case _ => None
      }

    def unifyPair(left: List[Part], right: List[Part]): Option[List[Part]] = {
      def o1 =
        if (left.startsWith(right)) {
          unifyPart(left.drop(right.size)).map(right ::: _)
        }
        else None

      def o2 =
        if (right.startsWith(left)) {
          unifyPart(right.drop(left.size)).map(left ::: _)
        }
        else None

      def o3 =
        if (left.endsWith(right)) {
          unifyPart(left.dropRight(right.size)).map(_ ::: right)
        }
        else None

      def o4 =
        if (right.endsWith(left)) {
          unifyPart(right.dropRight(left.size)).map(_ ::: left)
        }
        else None

      def o5 = if (subsetList(left, right)) Some(right) else None
      def o6 = if (subsetList(right, left)) Some(left) else None

      o1
        .orElse(o2)
        .orElse(o3)
        .orElse(o4)
        .orElse(o5)
        .orElse(o6)
    }

    val items = union.toArray.map(_.toList)

    val pairs = for {
      i <- (0 until items.length).iterator
      j <- ((i + 1) until items.length).iterator
      pair <- unifyPair(items(i), items(j)).iterator
    } yield (pair, i, j)

    // stop after the first unification and loop
    if (pairs.hasNext) {
      val (pair, i, j) = pairs.next
      items(i) = null
      items(j) = null
      val rest = items.iterator.filterNot(_ == null).toList
      // let's look again
      unifyUnionList(pair :: rest)
    }
    else union
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
   * return true if p1 <= p2, can give false negatives
   */
  def subset(p1: Pattern, p2: Pattern): Boolean =
    p2.matchesAny || (p1 == p2) || subsetList(p1.toList, p2.toList)

  private def subsetList(p1: List[Part], p2: List[Part]): Boolean =
    (p1, p2) match {
      case (Nil, Nil) => true
      case (Nil, (_: Part1) :: t) => false
      case (Nil, Wildcard :: t) =>
        subsetList(Nil, t)
      case (_ :: _, Nil) => false
      case (Lit(s1) :: t1, Lit(s2) :: t2) =>
        (s1 == s2) && subsetList(t1, t2)
      case (AnyChar :: _, Lit(_) :: _) => false
      case ((_: Part1) :: t1, AnyChar :: t2) => subsetList(t1, t2)
      case (Wildcard :: _, (_: Part1) :: _) => false
      case ((_: Part1) :: t1, Wildcard :: t2) =>
        // p2 = t2 + _:p2
        subsetList(p1, t2) || subsetList(t1, p2)
      case (Wildcard :: t1, Wildcard :: t2) =>
        subsetList(t1, t2) || subsetList(p1, t2)
    }

  /**
   * Compute a list of patterns that matches both patterns exactly
   */
  def intersection(p1: Pattern, p2: Pattern): List[Pattern] =
    (p1, p2) match {
      case (Empty, _) =>
        if (p2.matchesEmpty) p1 :: Nil else Nil
      case (_, Empty) =>
        if (p1.matchesEmpty) p2 :: Nil else Nil
      case (_, Cat(Wildcard, _)) if p2.matchesAny =>
        // matches anything
        p1 :: Nil
      case (_, _) if subset(p1, p2) => p1 :: Nil
      case (_, _) if subset(p2, p1) => p2 :: Nil
      case (Cat(Wildcard, t1@Cat(Wildcard, _)), _) =>
        // unnormalized
        intersection(t1, p2)
      case (_, Cat(Wildcard, t2@Cat(Wildcard, _))) =>
        // unnormalized
        intersection(p1, t2)
      case (Cat(Wildcard, Cat(AnyChar, t1)), _) =>
        // *. == .*, push Wildcards to the end
        intersection(Cat(AnyChar, Cat(Wildcard, t1)), p2)
      case (_, Cat(Wildcard, Cat(AnyChar, t2))) =>
        // *. == .*, push Wildcards to the end
        intersection(p1, Cat(AnyChar, Cat(Wildcard, t2)))
      case (Cat(Wildcard, t1), Cat(Wildcard, t2)) =>
        // *:t1 = (t1 + _:p1)
        // *:t2 = (t2 + _:p2)
        // p1 n p2 = t1 n t2 + (_:p1 n t2) + (t1 n _:p2) + _:(p1 n p2)
        //         = *:((t1 n t2) + (_:p1 n t2) + (t1 n _:p2))
        val i1 = intersection(t1, t2)
        val i2 = intersection(Cat(AnyChar, p1), t2)
        val i3 = intersection(t1, Cat(AnyChar, p2))
        val union = (i1 ::: i2 ::: i3)

        unifyUnion(union.map(_.prependWild))
      case (Cat(h1, t1), Cat(Wildcard, t2)) =>
        // h1 : t1 n *:t2 = h1:t1 n (t2 + _:p2) =
        // p1 n t2 + h1 : (t1 n p2)
        val i1 = intersection(p1, t2)
        val i2 = intersection(t1, p2).map(Cat(h1, _))
        unifyUnion(i1 ::: i2)
      case (Cat(Wildcard, _), Cat(_, _)) =>
        // intersection is commutative
        intersection(p2, p1)
      case (Cat(h1: Part1, t1), Cat(h2: Part1, t2)) =>
        val intr = for {
          h <- ipart(h1, h2)
          t <- intersection(t1, t2)
        } yield Cat(h, t)

        unifyUnion(intr)
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
      case (_, Cat(Wildcard, _)) if p2.matchesAny =>
        // matches anything
        Nil
      case (_, _) if subset(p1, p2) => Nil
      case (Cat(Wildcard, t1@Cat(Wildcard, _)), _) =>
        // unnormalized
        difference(t1, p2)
      case (_, Cat(Wildcard, t2@Cat(Wildcard, _))) =>
        // unnormalized
        difference(p1, t2)
      case (Cat(Wildcard, Cat(AnyChar, t1)), _) =>
        // *. == .*, push Wildcards to the end
        difference(Cat(AnyChar, Cat(Wildcard, t1)), p2)
      case (_, Cat(Wildcard, Cat(AnyChar, t2))) =>
        // *. == .*, push Wildcards to the end
        difference(p1, Cat(AnyChar, Cat(Wildcard, t2)))
      case (_, _) if intersection(p1, p2).isEmpty => p1 :: Nil
      case (Cat(Wildcard, t1), Empty) =>
        if (!t1.matchesEmpty) p1 :: Nil
        else {
          // *:t1 = t1 + _:p1
          // _:p1 - [] = _:p1
          unifyUnion(Cat(AnyChar, p1) :: difference(t1, Empty))
        }
      case (Cat(h1: Part1, t1), Cat(Wildcard, t2)) =>
        // h1:t1 - (*:t2) = h1:t1 - t2 - _:p2
        val d12 = {
          val dtail = difference(t1, p2)
          if (dtail == (t1 :: Nil)) p1 :: Nil
          else {
            val d1 =
              for {
                h <- ipart(h1, AnyChar)
                t <- difference(t1, p2)
              } yield Cat(h, t)

            val d2 = dpart(h1, AnyChar).map(Cat(_, t1))
            d1 ::: d2
          }
        }

        val u = d12.flatMap(difference(_, t2))

        unifyUnion(u)
      case (Cat(Wildcard, t1), Cat(h2: Part1, t2)) =>
        // *:t1 - (h2:t2) = t1 + _:p1 - h2:t2
        //   = (t1 - p2) + (_:p1 - h2:t2)
        val d12 = {
          val dtail = difference(p1, t2)
          if (dtail == (p1 :: Nil)) Cat(AnyChar, p1) :: Nil
          else {
            val d1 =
              for {
                h <- ipart(AnyChar, h2)
                t <- dtail
              } yield Cat(h, t)

            val d2 = dpart(AnyChar, h2).map(Cat(_, p1))
            d1 ::: d2
          }
        }
        val d3 = difference(t1, p2)
        unifyUnion(d12 ::: d3)
      case (Cat(h1: Part1, t1), Cat(h2: Part1, t2)) =>
        // h1:t1 - h2:t2 = (h1 n h2):(t1 - t2) + (h1 - h2):t1
        //               = (t1 n t2):(h1 - h2) + (t1 - t2):h1
        // if t1 - t2 == t1, then t1 n t2 = 0
        val dt = difference(t1, t2)
        if (dt == (t1 :: Nil)) {
          p1 :: Nil
        }
        else {
          val d1 =
            for {
              h <- ipart(h1, h2)
              t <- dt
            } yield Cat(h, t)

          val d2 = dpart(h1, h2).map(Cat(_, t1))
          unifyUnion(d1 ::: d2)
        }
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
            val union = (d1 ::: d2 ::: d3).map(_.prependWild)
            unifyUnion(union)
          }
        }
    }

  def differenceAll(p1: List[Pattern], p2: List[Pattern]): List[Pattern] =
    unifyUnion(p2.foldLeft(p1) { (p1s, p) =>
      // remove p from all of p1s
      p1s.flatMap(_.difference(p))
    })

  object Pattern {

    implicit val ordSeqPattern: Ordering[Pattern] =
      new Ordering[Pattern] {
        def compare(a: Pattern, b: Pattern) =
          (a, b) match {
            case (Empty, Empty) => 0
            case (Empty, Cat(_, _)) => -1
            case (Cat(_, _), Empty) => 1
            case (Cat(h1, t1), Cat(h2, t2)) =>
              val c = Part.partOrdering.compare(h1, h2)
              if (c == 0) compare(t1, t2)
              else c
          }
      }

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
