package org.bykn.bosatsu.pattern

import cats.data.NonEmptyList

sealed trait SeqPattern[+A] {
  import SeqPattern._
  import SeqPart.{AnyElem, Lit, Wildcard}

  def matchesAny: Boolean =
    this match {
      case Empty            => false
      case Cat(Wildcard, t) => t.matchesEmpty
      case Cat(_, _)        => false
    }

  def matchesEmpty: Boolean =
    this match {
      case Empty            => true
      case Cat(Wildcard, t) => t.matchesEmpty
      case Cat(_, _)        => false
    }

  def isEmpty: Boolean = this == Empty

  /** Concat that SeqPattern on the right
    */
  def +[A1 >: A](that: SeqPattern[A1]): SeqPattern[A1] =
    SeqPattern.fromList(toList ::: that.toList)

  def reverse: SeqPattern[A] =
    SeqPattern.fromList(toList.reverse)

  def prependWild: SeqPattern[A] =
    this match {
      case Cat(AnyElem, t)  => Cat(AnyElem, Cat(Wildcard, t))
      case Cat(Wildcard, _) => this
      case notAlreadyWild   => Cat(Wildcard, notAlreadyWild)
    }

  def toList: List[SeqPart[A]] =
    this match {
      case Empty     => Nil
      case Cat(h, t) => h :: t.toList
    }

  // if this is a literal sequence return it
  def toLiteralSeq: Option[List[A]] =
    this match {
      case Empty => Some(Nil)
      case Cat(Lit(a), t) =>
        t.toLiteralSeq.map(a :: _)
      case Cat(_, _) => None
    }

  /** If two wilds are adjacent, the left one will always match empty string
    * this normalize just removes the left wild
    *
    * combine adjacent strings
    */
  def normalize: SeqPattern[A] =
    this match {
      case Empty                          => Empty
      case Cat(Wildcard, Cat(AnyElem, t)) =>
        // move AnyElem out
        val wtn = Cat(Wildcard, t).normalize
        Cat(AnyElem, wtn)
      case Cat(Wildcard, tail @ Cat(Wildcard, _)) =>
        // remove duplicate Wildcard
        tail.normalize
      case Cat(h, tail) =>
        Cat(h, tail.normalize)
    }

  def show: String =
    toList.iterator.map {
      case Lit('.') => "\\."
      case Lit('*') => "\\*"
      case Lit(c)   => c.toString
      case AnyElem  => "."
      case Wildcard => "*"
    }.mkString
}

object SeqPattern {
  case object Empty extends SeqPattern[Nothing]
  case class Cat[A](head: SeqPart[A], tail: SeqPattern[A])
      extends SeqPattern[A] {
    // return the last non-empty
    @annotation.tailrec
    final def rightMost: SeqPart[A] =
      tail match {
        case Empty                 => head
        case Cat(h, Empty)         => h
        case Cat(_, r @ Cat(_, _)) => r.rightMost
      }

    def reverseCat: Cat[A] = {
      val nel = NonEmptyList(head, tail.toList).reverse
      Cat(nel.head, SeqPattern.fromList(nel.tail))
    }
  }

  def fromList[A](ps: List[SeqPart[A]]): SeqPattern[A] =
    ps match {
      case h :: tail =>
        Cat(h, fromList(tail))
      case Nil => Empty
    }

  val Wild: SeqPattern[Nothing] = Cat(SeqPart.Wildcard, Empty)
  val Any: SeqPattern[Nothing] = Cat(SeqPart.AnyElem, Empty)

  implicit def ordSeqPattern[A: Ordering]: Ordering[SeqPattern[A]] =
    new Ordering[SeqPattern[A]] {
      val ordSeqPart: Ordering[SeqPart[A]] = implicitly[Ordering[SeqPart[A]]]
      def compare(a: SeqPattern[A], b: SeqPattern[A]) =
        (a, b) match {
          case (Empty, Empty)     => 0
          case (Empty, Cat(_, _)) => -1
          case (Cat(_, _), Empty) => 1
          case (Cat(h1, t1), Cat(h2, t2)) =>
            val c = ordSeqPart.compare(h1, h2)
            if (c == 0) compare(t1, t2)
            else c
        }
    }

  implicit def seqPatternSetOps[A](implicit
      part1SetOps: SetOps[SeqPart.SeqPart1[A]],
      ordA: Ordering[A]
  ): SetOps[SeqPattern[A]] =
    new SetOps[SeqPattern[A]] {
      import SeqPart.{SeqPart1, AnyElem, Wildcard}

      lazy val top = Some(Wild)
      def isTop(p: SeqPattern[A]) = p.matchesAny

      // Try to unify lists according to the rules:
      // x u [_, *, x] = [*, x]
      // x u [*, _, x] = [*, x]
      // x u [x, *, _] = [x, *]
      // x u [x, _, *] = [x, *]
      //
      // this is an incomplete heuristic now, not a complete solution
      def unifyUnion(union: List[SeqPattern[A]]): List[SeqPattern[A]] =
        unifyUnionList {
          union
            .map(_.normalize)
            .distinct
            .map(_.toList)
        }
          .map(SeqPattern.fromList(_).normalize)
          .sorted

      private[this] val someWild = Some(Wildcard :: Nil)
      private[this] val someNil = Some(Nil)

      private def unifyUnionList(
          union: List[List[SeqPart[A]]]
      ): List[List[SeqPart[A]]] = {

        // if a part of Sequences are the same except this part, can we merge by appending
        // something?
        def unifySeqPart(list: List[SeqPart[A]]): Option[List[SeqPart[A]]] =
          list match {
            case (a: SeqPart1[A]) :: Wildcard :: Nil if isAny(a) => someWild
            case Wildcard :: (a: SeqPart1[A]) :: Nil if isAny(a) => someWild
            case Wildcard :: Wildcard :: Nil                     => someWild
            case Wildcard :: Nil                                 => someWild
            case Nil                                             => someNil
            case _                                               => None
          }

        def unifyPair(
            left: List[SeqPart[A]],
            right: List[SeqPart[A]]
        ): Option[List[SeqPart[A]]] = {
          def o1 =
            if (left.startsWith(right)) {
              unifySeqPart(left.drop(right.size)).map(right ::: _)
            } else None

          def o2 =
            if (right.startsWith(left)) {
              unifySeqPart(right.drop(left.size)).map(left ::: _)
            } else None

          def o3 =
            if (left.endsWith(right)) {
              unifySeqPart(left.dropRight(right.size)).map(_ ::: right)
            } else None

          def o4 =
            if (right.endsWith(left)) {
              unifySeqPart(right.dropRight(left.size)).map(_ ::: left)
            } else None

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
          val (pair, i, j) = pairs.next()
          items(i) = null
          items(j) = null
          val rest = items.iterator.filterNot(_ == null).toList
          // let's look again
          unifyUnionList(pair :: rest)
        } else union
      }

      /** return true if p1 <= p2, can give false negatives
        */
      def subset(p1: SeqPattern[A], p2: SeqPattern[A]): Boolean =
        p2.matchesAny || {
          // if p2 doesn't matchEmpty but p1 does, we are done
          // check that case quickly
          if (!p2.matchesEmpty && p1.matchesEmpty) false
          else subsetList(p1.toList, p2.toList)
        }

      @inline
      final def isAny(p: SeqPart1[A]): Boolean =
        part1SetOps.isTop(p)

      private def subsetList(
          p1: List[SeqPart[A]],
          p2: List[SeqPart[A]]
      ): Boolean =
        (p1, p2) match {
          case (Nil, Nil)                   => true
          case (Nil, (_: SeqPart1[A]) :: _) => false
          case (Nil, Wildcard :: t) =>
            subsetList(Nil, t)
          case (_ :: _, Nil) => false
          case ((h1: SeqPart1[A]) :: t1, (h2: SeqPart1[A]) :: t2) =>
            part1SetOps.subset(h1, h2) && subsetList(t1, t2)
          case (Wildcard :: Wildcard :: t1, _) =>
            // normalize the left:
            subsetList(Wildcard :: t1, p2)
          case (_, Wildcard :: Wildcard :: t2) =>
            // normalize the right:
            subsetList(p1, Wildcard :: t2)
          case (_, Wildcard :: (a2: SeqPart1[A]) :: t2) if isAny(a2) =>
            // we know that right can't match empty,
            // let's see if that helps us rule out matches on the left
            subsetList(p1, AnyElem :: Wildcard :: t2)
          // either t1 or t2 also ends with Wildcard
          case (_ :: _, Wildcard :: _) if p2.last.notWild =>
            // wild on the right but not at the end
            // yields an approximation. avoid that
            // if possible
            subsetList(p1.reverse, p2.reverse)
          case (Wildcard :: _, _ :: _) if p1.last.notWild && p2.last.notWild =>
            // if we don't check both here we can loop
            subsetList(p1.reverse, p2.reverse)
          case (Wildcard :: t1, (h2: SeqPart1[A]) :: t2) =>
            // we know right head is not any, so something may
            // match on the left that won't match on the right
            // p1 = *t1 = t1 + _:p1
            // _:p1 <= h2:t2 => (_ <= h2) && (p1 <= t2)
            isAny(h2) &&
            subsetList(t1, p2) &&
            subsetList(p1, t2)
          case ((_: SeqPart1[A]) :: t1, Wildcard :: t2) =>
            // we could pop off one wildcard to match head
            // or we could match with nothing but the rest
            // p2 = t2 + _:p2
            // a <= (b + c), is a <= b or a <= c, it is true
            // if not, it may still be true,
            // this branch is what gives us the approximation
            subsetList(t1, p2) || subsetList(p1, t2)
          case (Wildcard :: t1, Wildcard :: _) =>
            // *t1 = t1 + _:*:t1 <= right
            //  t1 <= right && (_:*:t1 <= right)
            //  but right starts with *, so (_:*:t1 <= right) = (*:t1 <= *:t2)
            //  which is the current question
            subsetList(t1, p2)
        }

      /** Compute a list of patterns that matches both patterns exactly
        */
      def intersection(
          p1: SeqPattern[A],
          p2: SeqPattern[A]
      ): List[SeqPattern[A]] =
        (p1, p2) match {
          case (Empty, _) =>
            if (p2.matchesEmpty) p1 :: Nil else Nil
          case (_, Empty) =>
            if (p1.matchesEmpty) p2 :: Nil else Nil
          case (_, Cat(Wildcard, _)) if p2.matchesAny =>
            // matches anything
            p1 :: Nil
          case (_, _) if subset(p1, p2)                  => p1 :: Nil
          case (_, _) if subset(p2, p1)                  => p2 :: Nil
          case (Cat(Wildcard, t1 @ Cat(Wildcard, _)), _) =>
            // unnormalized
            intersection(t1, p2)
          case (_, Cat(Wildcard, t2 @ Cat(Wildcard, _))) =>
            // unnormalized
            intersection(p1, t2)
          case (Cat(Wildcard, Cat(a1: SeqPart1[A], t1)), _) if isAny(a1) =>
            // *. == .*, push Wildcards to the end
            intersection(Cat(AnyElem, Cat(Wildcard, t1)), p2)
          case (_, Cat(Wildcard, Cat(a2: SeqPart1[A], t2))) if isAny(a2) =>
            // *. == .*, push Wildcards to the end
            intersection(p1, Cat(AnyElem, Cat(Wildcard, t2)))
          case (c1 @ Cat(Wildcard, _), c2 @ Cat(Wildcard, _))
              if c1.rightMost.notWild || c2.rightMost.notWild =>
            // let's avoid the most complex case of both having
            // wild on the front if possible
            intersection(c1.reverse, c2.reverse).map(_.reverse)
          case (Cat(Wildcard, t1), Cat(Wildcard, t2)) =>
            // both start and end with wild
            //
            // *:t1 = (t1 + _:p1)
            // *:t2 = (t2 + _:p2)
            // p1 n p2 = t1 n t2 + (_:p1 n t2) + (t1 n _:p2) + _:(p1 n p2)
            //         = *:((t1 n t2) + (_:p1 n t2) + (t1 n _:p2))
            val i1 = intersection(t1, t2)
            val i2 = intersection(Cat(AnyElem, p1), t2)
            val i3 = intersection(t1, Cat(AnyElem, p2))
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
          case (Cat(h1: SeqPart1[A], t1), Cat(h2: SeqPart1[A], t2)) =>
            val intr = for {
              h <- part1SetOps.intersection(h1, h2)
              t <- intersection(t1, t2)
            } yield Cat(h, t)

            unifyUnion(intr)
        }

      /** return the patterns that match p1 but not p2
        *
        * For fixed sets A, B if we have (A1 x B1) - (A2 x B2) = A1 = (A1 n A2)
        * u (A1 - A2) A2 = (A1 n A2) u (A2 - A1) so we can decompose:
        *
        * A1 x B1 = (A1 n A2)xB1 u (A1 - A2)xB1 A2 x B2 = (A1 n A2)xB2 u (A2 -
        * A1)xB2
        *
        * the difference is: (A1 n A2)x(B1 - B2) u (A1 - A2)xB1
        *
        * A - (B1 u B2) = (A - B1) n (A - B2) A - (B1 u B2) <= ((A - B1) u (A -
        * B2)) - (B1 n B2) A - (B1 u B2) >= (A - B1) u (A - B2)
        *
        * so if (B1 n B2) = 0, then: A - (B1 u B2) = (A - B1) u (A - B2)
        *
        * (A1 u A2) - B = (A1 - B) u (A2 - B)
        *
        * The last challenge is we need to operate on s ingle characters, so we
        * need to expand wild into [*] = [] | [_, *], since our pattern language
        * doesn't have a symbol for a single character match we have to be a bit
        * more careful
        *
        * also, we can't exactly represent Wildcard - Lit so this is actually an
        * upperbound on the difference which is to say, all the returned
        * patterns match p1, but some of them also match p2
        */
      def difference(
          p1: SeqPattern[A],
          p2: SeqPattern[A]
      ): List[SeqPattern[A]] =
        (p1, p2) match {
          case (Empty, _) => if (p2.matchesEmpty) Nil else p1 :: Nil
          case (Cat(_: SeqPart1[A], _), Empty) =>
            // Cat(SeqPart1[A], _) does not match Empty
            p1 :: Nil
          case (Cat(Wildcard, t1 @ Cat(Wildcard, _)), _) =>
            // unnormalized
            difference(t1, p2)
          case (_, Cat(Wildcard, t2 @ Cat(Wildcard, _))) =>
            // unnormalized
            difference(p1, t2)
          case (Cat(Wildcard, Cat(a1: SeqPart1[A], t1)), _) if isAny(a1) =>
            // *. == .*, push Wildcards to the end
            difference(Cat(a1, Cat(Wildcard, t1)), p2)
          case (_, Cat(Wildcard, Cat(a2: SeqPart1[A], t2))) if isAny(a2) =>
            // *. == .*, push Wildcards to the end
            difference(p1, Cat(AnyElem, Cat(Wildcard, t2)))
          case (Cat(Wildcard, t1), Empty) =>
            if (!t1.matchesEmpty) p1 :: Nil
            else {
              // use (A + B) - C = (A - C) + (B - C)
              // *:t1 = t1 + _:p1
              // _:p1 - [] = _:p1
              unifyUnion(Cat(AnyElem, p1) :: difference(t1, Empty))
            }
          case (_, _) if subset(p1, p2) =>
            // p2 has to be bigger
            Nil
          case (Cat(h1: SeqPart1[A], t1), Cat(h2: SeqPart1[A], t2)) =>
            // h1:t1 - h2:t2 = (h1 n h2):(t1 - t2) + (h1 - h2):t1
            //               = (t1 n t2):(h1 - h2) + (t1 - t2):h1
            // if t1 n t2 = 0 then t1 - t2 == t1
            val intH = part1SetOps.intersection(h1, h2)
            if (intH.isEmpty) {
              // then h1 - h2 = h1
              p1 :: Nil
            } else if (disjoint(t1, t2)) {
              p1 :: Nil
            } else {
              val d1 =
                for {
                  h <- intH
                  t <- difference(t1, t2)
                } yield Cat(h, t)

              val d2 = part1SetOps.difference(h1, h2).map(Cat(_, t1))
              unifyUnion(d1 ::: d2)
            }
          case (Cat(h1: SeqPart1[A], t1), Cat(Wildcard, t2)) =>
            // h1:t1 - (*:t2) = ((h1:t1 - _:p2) - t2)
            //
            // h1:t1 - _:p2 = (h1 n _) : (t1 - p2) + (h1 - _):t1
            //              = h1 : (t1 - p2)
            val dtail = difference(t1, p2).map(Cat(h1, _))
            val u = differenceAll(dtail, t2 :: Nil)

            unifyUnion(u)
          case (Cat(Wildcard, t1), Cat(h2: SeqPart1[A], t2)) =>
            // *:t1 - (h2:t2) = t1 + _:p1 - h2:t2
            //   = (t1 - p2) + (_:p1 - h2:t2)
            val d12 = {
              // (_:p1 - h2:t2) =
              // (_ n h2):(p1 - t2) + (_ - h2):p1
              // h2:(p1 - t2) + (_ - h2):p1
              //
              // or:
              // (_ - h2):(p1 n t2) + _:(p1 - t2)
              if (disjoint(p1, t2)) {
                Cat(AnyElem, p1) :: Nil
              } else {
                val dtail = difference(p1, t2)
                val d1 = dtail.map(Cat(h2, _))
                val d2 = part1SetOps.difference(AnyElem, h2).map(Cat(_, p1))
                d1 ::: d2
              }
            }
            val d3 = difference(t1, p2)
            unifyUnion(d12 ::: d3)
          case (c1 @ Cat(Wildcard, t1), c2 @ Cat(Wildcard, t2)) =>
            if (c1.rightMost.notWild || c2.rightMost.notWild) {
              // let's avoid the most complex case of both having
              // wild on the front if possible
              difference(c1.reverse, c2.reverse).map(_.reverse)
            } else {
              // both start and end with wildcard
              //
              // p1 - (t2 + _:p2) =
              // if (t2 n (_:*:t2)).isEmpty, then
              // then we can use a simpler formula
              // but that is very uncommon (maybe
              // we can find a proof it can't
              // happen if t2 ends with Wildcard
              // which is the case we are in.
              //
              // otherwise
              // this branch is approximate:
              // (p1 - t2) n (p1 - _:p2) =
              // (p1 - t2) n ((t1 + _:p1) - _:p2)
              // (p1 - t2) n ((t1 - _:p2) + _:(p1 - p2))
              //
              // x = a n (b + _:x)
              //   = (a n b) + a n (_:x)
              //   <= (a n b) + (_:x)
              //   = *:(a n b)
              //
              // if t1 = [], then the above gives
              // either empty set or *.
              // this is a common case when we are
              // searching for missing branches, we
              // start at * - x
              //
              // (* - t2) n (([] - _:p2) + _:(p1 - p2))
              // a = * - t2
              // = (* - t2) n ([] + _:(p1 - p2))
              // p1 - p2 = a n ([] + _:(a n ([] + _:(a n ([] + _: ...
              //         <= a n ([] _ :(a n []) + _ _ :(a n []) +++
              //         = a n (*:(a n []))
              //
              //   since a <= *, in the right side we have
              //   a n * = a
              //  so p1 - p2 <= a
              //
              //  note, a is always an upper bound due
              //  to formula x = a n (...)
              val as = difference(p1, t2)
              if (t1.isEmpty) {
                as
              } else {
                // if x <= *:(a n b) and a then it is <= a n (*:(a n b))
                val bs = difference(t1, Cat(AnyElem, p2))
                // (a1 + a2) n (b1 + b2) =
                val intr =
                  for {
                    ai <- as
                    bi <- bs
                    c <- intersection(ai, bi)
                    // we know that everything
                    // in the result must be in a
                    a2 <- as
                    ca <- intersection(c.prependWild, a2)
                  } yield ca

                unifyUnion(intr)
              }
            }
        }
    }

  def matcher[A, I, S, R](
      split: Splitter[A, I, S, R]
  ): Matcher[SeqPattern[A], S, R] =
    new Matcher[SeqPattern[A], S, R] {
      import SeqPart.{AnyElem, Lit, SeqPart1, Wildcard}

      val someEmpty = Some(split.monoidResult.empty)

      def apply(p: SeqPattern[A]): S => Option[R] =
        p match {
          case Empty => { (s: S) => if (split.isEmpty(s)) someEmpty else None }
          case Cat(Lit(h), t) =>
            val mh = split.matcher(h)
            val mt = apply(t)

            { (s: S) =>
              for {
                ht <- split.uncons(s)
                (h, t) = ht
                rh <- mh(h)
                rt <- mt(t)
              } yield split.monoidResult.combine(rh, rt)
            }
          case Cat(AnyElem, t) =>
            val mt = apply(t)

            { (s: S) =>
              for {
                ht <- split.uncons(s)
                (_, t) = ht
                rt <- mt(t)
              } yield rt
            }
          case Cat(Wildcard, t) =>
            matchEnd(t).andThen(_.headOption.map(_._2))
        }

      def matchEnd(p: SeqPattern[A]): S => LazyList[(S, R)] =
        p match {
          case Empty => { (s: S) =>
            (s, split.monoidResult.empty) #:: LazyList.empty
          }
          case Cat(p: SeqPart1[A], t) =>
            val splitFn: S => LazyList[(S, I, R, S)] = p match {
              case Lit(c)  => split.positions(c)
              case AnyElem => split.anySplits(_: S)
            }
            val tailMatch = apply(t)

            { (s: S) =>
              splitFn(s)
                .map { case (pre, _, r, post) =>
                  tailMatch(post)
                    .map { rtail =>
                      (pre, split.monoidResult.combine(r, rtail))
                    }
                }
                .collect { case Some(res) => res }
            }
          case Cat(Wildcard, t) =>
            matchEnd(t)
        }
    }

  val stringUnitMatcher: Matcher[SeqPattern[Char], String, Unit] =
    matcher(Splitter.stringUnit)
}
