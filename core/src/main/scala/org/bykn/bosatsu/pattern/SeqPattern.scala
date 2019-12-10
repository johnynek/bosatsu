package org.bykn.bosatsu.pattern

abstract class SeqPattern { self =>
  // Elem is the individual pattern element
  type Elem
  // item is a single element of the values being matched
  type Item
  // Sequence is the sequence of Items
  type Sequence

  def elemMatch(m: Elem, i: Item): Boolean

  val elemOrdering: Ordering[Elem]
  val part1SetOps: SetOps[Part1]

  // return all the places such that fst ++ c ++ snd == str
  def positions(c: Elem, str: Sequence): Stream[(Sequence, Item, Sequence)]

  // splits skipping a single character to match AnyElem
  def anySplits(str: Sequence): Stream[(Sequence, Item, Sequence)]

  def seqToList(s: Sequence): List[Item]
  def listToSeq(s: List[Item]): Sequence

  def emptySeq: Sequence
  def catSeq(a: Sequence, b: Sequence): Sequence
  def isEmpty(s: Sequence): Boolean
  def nonEmpty(s: Sequence): Boolean = !isEmpty(s)

  // unsafe, only call after isEmpty
  protected def head(s: Sequence): Item
  // unsafe, only call after isEmpty
  protected def tail(s: Sequence): Sequence

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
        case Cat(AnyElem, t) => Cat(AnyElem, Cat(Wildcard, t))
        case Cat(Wildcard, _) => this
        case notAlreadyWild => Cat(Wildcard, notAlreadyWild)
      }

    def matches(str: Sequence): Boolean =
      self.matches(this, str)

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
        case Cat(Wildcard, Cat(AnyElem, t)) =>
          // move AnyElem out
          val wtn = Cat(Wildcard, t).normalize
          Cat(AnyElem, wtn)
        case Cat(Wildcard, tail@Cat(Wildcard, _)) =>
          // remove duplicate Wildcard
          tail.normalize
        case Cat(h, tail) =>
          Cat(h, tail.normalize)
      }

    def intersection(that: Pattern): List[Pattern] =
      Pattern.patternSetOps.intersection(this, that)

    def difference(that: Pattern): List[Pattern] =
      Pattern.patternSetOps.difference(this, that)


    def show: String =
      toList
        .iterator
        .map {
          case Lit('.') => "\\."
          case Lit('*') => "\\*"
          case Lit(c) => c.toString
          case AnyElem => "."
          case Wildcard => "*"
        }
        .mkString
  }

  sealed trait Part {
    def notWild: Boolean = false
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
              elemOrdering.compare(i1, i2)
            case (Lit(_), _) => -1
            case (_, Lit(_)) => 1
            case (AnyElem, AnyElem) => 0
            case (AnyElem, Wildcard) => -1
            case (Wildcard, AnyElem) => 1
            case (Wildcard, Wildcard) => 0
          }
      }
  }

  case class Lit(item: Elem) extends Part1
  case object AnyElem extends Part1
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

    def matches(str: Sequence): Option[Map[String, Sequence]] =
      Named.matches(Named.toMachine(this, Nil), str, Nil, Map.empty) match {
        case None => None
        case Some(m) => Some(m.map { case (k, es) => (k, listToSeq(es)) })
      }

    def name(n: String): Named = Bind(n, this)
    def +(that: Named): Named = NCat(this, that)

    // we are renderable if all Wild/AnyElem are named
    def isRenderable: Boolean =
      this match {
        case NEmpty => true
        case Bind(_, _) => true
        case NPart(Lit(_)) => true
        case NPart(_) => false
        case NCat(l, r) =>
          l.isRenderable && r.isRenderable
      }

    def render(names: Map[String, Sequence])(fn: Elem => Item): Option[Sequence] = {
      def loop(n: Named, right: List[Item]): Option[List[Item]] =
        n match {
          case NEmpty => Some(right)
          case Bind(nm, r) =>
            // since we have this name, we don't need to recurse
            names.get(nm)
              .map { seq => seqToList(seq) ::: right }
              .orElse(loop(r, right))
          case NPart(Lit(c)) => Some(fn(c) :: right)
          case NPart(_) => None
          case NCat(l, r) =>
            loop(r, right)
              .flatMap { right =>
                loop(l, right)
              }
        }

      loop(this, Nil).map(listToSeq(_))
    }
  }

  object Named {
    def fromPattern(p: Pattern): Named =
      p.toList.foldRight(NEmpty: Named) { (part, right) =>
        NCat(NPart(part), right)
      }

    val Wild: Named = NPart(Wildcard)
    val Any: Named = NPart(AnyElem)

    case class Bind(name: String, p: Named) extends Named
    case object NEmpty extends Named
    case class NPart(part: Part) extends Named
    case class NCat(first: Named, second: Named) extends Named

    def toMachine(n: Named, right: List[Machine]): List[Machine] =
      n match {
        case NEmpty => right
        case Bind(name, n) =>
          StartName(name) :: toMachine(n, EndName :: right)
        case NPart(p) => MPart(p) :: right
        case NCat(l, r) =>
          toMachine(l, toMachine(r, right))
      }

    sealed trait Machine
    case class StartName(name: String) extends Machine
    case object EndName extends Machine
    case class MPart(part: Part) extends Machine

    def hasWildLeft(m: List[Machine]): Boolean =
      m match {
        case Nil => false
        case MPart(Wildcard) :: _ => true
        case MPart(_) :: _ => false
        case _ :: tail => hasWildLeft(tail)
      }

    type CaptureState = Map[String, Either[List[Item], List[Item]]]

    private def appendState(c: Item, capturing: List[String], nameState: CaptureState): CaptureState =
      capturing.foldLeft(nameState) { (ns, n) =>
        val nextV = ns(n).left.map(c :: _)
        ns.updated(n, nextV)
      }

    private def matches(
      m: List[Machine],
      str: Sequence,
      capturing: List[String],
      nameState: CaptureState): Option[Map[String, List[Item]]] =

      m match {
        case Nil =>
          if (isEmpty(str)) {
            val bindings = nameState.map {
              case (k, Right(v)) => (k, v)
              case (k, Left(strings)) =>
                  sys.error(s"unclosed key: $k, $strings")
            }
            Some(bindings)
          } else None
        case StartName(n) :: tail =>
          val ns1 = nameState.get(n) match {
            case None => nameState.updated(n, Left(Nil))
            case Some(s) => sys.error(s"illegal shadow: $n")
          }
          matches(tail, str, n :: capturing, ns1)
        case EndName :: tail =>
          capturing match {
            case Nil => sys.error("illegal End with no capturing")
            case n :: cap =>
            val ns1 = nameState.get(n) match {
              case Some(Left(parts)) => nameState.updated(n, Right(parts.reverse))
              case res@(Some(Right(_)) | None) => sys.error(s"illegal end: $n, $res")
            }
            matches(tail, str, cap, ns1)
          }
        case MPart(Wildcard) :: tail =>
          if (hasWildLeft(tail)) {
            // two adjacent wilds means this one matches nothing
            matches(tail, str, capturing, nameState)
          }
          else {
            // match everything tail does not match on the right
            // the matchEnd is only going to capture
            // on AnyElem or Lit, so we need to reset the
            // capture state here, and append everything
            // we get
            val capSet = capturing.toSet
            val nameState1 = nameState.map {
              case (k, Left(_)) if capSet(k) => (k, Left(Nil))
              case notCap => notCap
            }
            matchEnd(tail, str, capturing, nameState1)
              .headOption
              .map { case (prefix, rightResult) =>
                // now merge the result
                capturing.foldLeft(rightResult) { (st, n) =>
                  nameState(n) match {
                    case Left(leftMatches) =>
                      // Left is accumulating in reverse order
                      // so we need to put the latest on the left
                      val res = leftMatches reverse_::: seqToList(prefix) ::: st(n)
                      st.updated(n, res)
                    case Right(r) =>
                      sys.error(s"both capturing and done: $n, $r")
                  }
                }
              }
          }
        case MPart(p1: Part1) :: tail =>
          if (nonEmpty(str)) {
            val h = head(str)

            // keep this lazy
            @inline def good = matches(tail,
              self.tail(str),
              capturing,
              appendState(h, capturing, nameState))

            p1 match {
              case AnyElem => good
              case Lit(c) => if (elemMatch(c, h)) good else None
            }
          }
          else None
      }

    /*
     * Return the bindings and unmatched prefix if we have to match the right of the string
     */
    private def matchEnd(
      m: List[Machine],
      str: Sequence,
      capturing: List[String],
      nameState: CaptureState): Stream[(Sequence, Map[String, List[Item]])] =
      m match {
        case Nil =>
          // we always match the end
          val bindings = nameState.map {
            case (k, Right(v)) => (k, v)
            case (k, Left(strings)) =>
                sys.error(s"unclosed key: $k, $strings")
          }
          (str, bindings) #:: Stream.Empty
        case StartName(n) :: tail =>
          val ns1 = nameState.get(n) match {
            case None => nameState.updated(n, Left(Nil))
            case Some(s) => sys.error(s"illegal shadow: $n")
          }
          matchEnd(tail, str, n :: capturing, ns1)
        case EndName :: tail =>
          capturing match {
            case Nil => sys.error("illegal End with no capturing")
            case n :: cap =>
            val ns1 = nameState.get(n) match {
              case Some(Left(parts)) => nameState.updated(n, Right(parts.reverse))
              case res@(Some(Right(_)) | None) => sys.error(s"illegal end: $n, $res")
            }
            matchEnd(tail, str, cap, ns1)
          }
        case MPart(Wildcard) :: tail =>
          // we can just go on matching the end, and sucking up
          // all current state
            matchEnd(tail, str, capturing, nameState)
        case MPart(p1: Part1) :: tail =>
          val splits = p1 match {
            case Lit(c) => positions(c, str)
            case AnyElem => anySplits(str)
          }
          splits.map { case (pre, c, post) =>
            val newState = appendState(c, capturing, nameState)
            matches(tail, post, capturing, newState)
              .map((pre, _))
          }
          .collect { case Some(res) => res }
      }

  }


  object Pattern {
    implicit val patternSetOps: SetOps[Pattern] = new SetOps[Pattern] {
      lazy val top = Some(Pattern.Wild)
      def isTop(p: Pattern) = p.matchesAny

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
            case AnyElem :: Wildcard :: Nil => someWild
            case Wildcard :: AnyElem :: Nil => someWild
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
          case (AnyElem :: _, Lit(_) :: _) => false
          case ((_: Part1) :: t1, AnyElem :: t2) => subsetList(t1, t2)
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
          case (Cat(Wildcard, Cat(AnyElem, t1)), _) =>
            // *. == .*, push Wildcards to the end
            intersection(Cat(AnyElem, Cat(Wildcard, t1)), p2)
          case (_, Cat(Wildcard, Cat(AnyElem, t2))) =>
            // *. == .*, push Wildcards to the end
            intersection(p1, Cat(AnyElem, Cat(Wildcard, t2)))
          case (Cat(Wildcard, t1), Cat(Wildcard, t2)) =>
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
          case (Cat(h1: Part1, t1), Cat(h2: Part1, t2)) =>
            val intr = for {
              h <- part1SetOps.intersection(h1, h2)
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
          case (Cat(Wildcard, Cat(AnyElem, t1)), _) =>
            // *. == .*, push Wildcards to the end
            difference(Cat(AnyElem, Cat(Wildcard, t1)), p2)
          case (_, Cat(Wildcard, Cat(AnyElem, t2))) =>
            // *. == .*, push Wildcards to the end
            difference(p1, Cat(AnyElem, Cat(Wildcard, t2)))
          case (_, _) if intersection(p1, p2).isEmpty => p1 :: Nil
          case (Cat(Wildcard, t1), Empty) =>
            if (!t1.matchesEmpty) p1 :: Nil
            else {
              // *:t1 = t1 + _:p1
              // _:p1 - [] = _:p1
              unifyUnion(Cat(AnyElem, p1) :: difference(t1, Empty))
            }
          case (Cat(h1: Part1, t1), Cat(Wildcard, t2)) =>
            // h1:t1 - (*:t2) = h1:t1 - t2 - _:p2
            val d12 = {
              val dtail = difference(t1, p2)
              if (dtail == (t1 :: Nil)) p1 :: Nil
              else {
                val d1 =
                  for {
                    h <- part1SetOps.intersection(h1, AnyElem)
                    t <- difference(t1, p2)
                  } yield Cat(h, t)

                val d2 = part1SetOps.difference(h1, AnyElem).map(Cat(_, t1))
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
              if (dtail == (p1 :: Nil)) Cat(AnyElem, p1) :: Nil
              else {
                val d1 =
                  for {
                    h <- part1SetOps.intersection(AnyElem, h2)
                    t <- dtail
                  } yield Cat(h, t)

                val d2 = part1SetOps.difference(AnyElem, h2).map(Cat(_, p1))
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
                  h <- part1SetOps.intersection(h1, h2)
                  t <- dt
                } yield Cat(h, t)

              val d2 = part1SetOps.difference(h1, h2).map(Cat(_, t1))
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

                val d2 = difference(Cat(AnyElem, p1), t2)
                val d3 = difference(t1, Cat(AnyElem, p2))
                val union = (d1 ::: d2 ::: d3).map(_.prependWild)
                unifyUnion(union)
              }
            }
        }
    }

    implicit val ordPattern: Ordering[Pattern] =
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

    val Wild: Pattern = Cat(Wildcard, Empty)
    val Any: Pattern = Cat(AnyElem, Empty)
  }

  def matches(p: Pattern, str: Sequence): Boolean =
    p match {
      case Empty => isEmpty(str)
      case Cat(Lit(c), t) =>
        nonEmpty(str) &&
          elemMatch(c, head(str)) &&
          matches(t, tail(str))
      case Cat(AnyElem, t) =>
        nonEmpty(str) &&
          matches(t, tail(str))
      case Cat(Wildcard, t) =>
        matchEnd(t, str).nonEmpty
    }

  // return all the prefixes where this pattern matches everything
  // after
  def matchEnd(p: Pattern, str: Sequence): Stream[Sequence] =
    p match {
      case Empty => str #:: Stream.Empty
      case Cat(p: Part1, t) =>
        val splits = p match {
          case Lit(c) => positions(c, str)
          case AnyElem => anySplits(str)
        }
        splits.collect { case (pre, _, post) if matches(t, post) => pre }
      case Cat(Wildcard, t) =>
        matchEnd(t, str)
    }
}

object SeqPattern {
  type Aux[E, S] = SeqPattern { type Elem = E; type Sequence = S }
}

object StringSeqPattern extends SeqPattern {
  type Elem = Char
  type Item = Char
  type Sequence = String

  def toPattern(s: String): Pattern =
    if (isEmpty(s)) Empty
    else Cat(Lit(head(s)), toPattern(tail(s)))

  val elemOrdering: Ordering[Char] = Ordering.Char

  val part1SetOps: SetOps[Part1] = new SetOps[Part1] {
    val top = Some(AnyElem)
    def isTop(c: Part1) = c == AnyElem

    def intersection(p1: Part1, p2: Part1): List[Part1] =
      (p1, p2) match {
        case (Lit(c1), Lit(c2)) => if (c1 == c2) p1 :: Nil else Nil
        case (AnyElem, _) => p2 :: Nil
        case (_, AnyElem) => p1 :: Nil
      }

    def difference(p1: Part1, p2: Part1): List[Part1] =
      (p1, p2) match {
        case (Lit(c1), Lit(c2)) => if (c1 == c2) Nil else p1 :: Nil
        case (_, AnyElem) => Nil
        case (AnyElem, _) => p1 :: Nil
      }

    def subset(p1: Part1, p2: Part1): Boolean =
      p2 match {
        case AnyElem => true
        case Lit(c2) =>
          p1 match {
            case Lit(c1) => c1 == c2
            case _ => false
          }
      }

    def unifyUnion(u: List[Part1]): List[Part1] =
      if (u.exists(_ == AnyElem)) AnyElem :: Nil
      else u.distinct.sorted(Part.partOrdering)
  }

  def elemMatch(c1: Char, c2: Char) = c1 == c2
  // return all the places such that fst ++ c ++ snd == str
  def positions(c: Char, str: String): Stream[(String, Char, String)] = {
    def loop(init: Int): Stream[(String, Char, String)] =
      if (init >= str.length) Stream.Empty
      else if (str.charAt(init) == c) {
        (str.substring(0, init), c, str.substring(init + 1)) #:: loop(init + 1)
      }
      else loop(init + 1)

    loop(0)
  }

  // splits skipping a single character to match AnyElem
  def anySplits(str: String): Stream[(String, Char, String)] =
    (0 until str.length)
      .toStream
      .map { idx =>
        val prefix = str.substring(0, idx)
        val post = str.substring(idx + 1)
        (prefix, str.charAt(idx), post)
      }

  def seqToList(s: String): List[Item] = s.toList
  def listToSeq(s: List[Item]): String = s.mkString

  def emptySeq = ""
  def catSeq(a: String, b: String) = a + b
  def isEmpty(s: String): Boolean = s.isEmpty
  def head(s: String) = s.head
  def tail(s: String) = s.tail
}
