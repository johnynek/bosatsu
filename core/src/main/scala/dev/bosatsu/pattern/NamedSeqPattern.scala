package dev.bosatsu.pattern

import cats.Monoid

sealed trait NamedSeqPattern[+A] derives CanEqual {
  import NamedSeqPattern._
  import SeqPart._

  def unname: SeqPattern[A] = {
    def loop(n: NamedSeqPattern[A], right: List[SeqPart[A]]): List[SeqPart[A]] =
      n match {
        case Bind(_, n)          => loop(n, right)
        case NEmpty              => right
        case NCat(first, second) =>
          val r2 = loop(second, right)
          loop(first, r2)
        case NSeqPart(p) => p :: right
      }

    SeqPattern.fromList(loop(this, Nil))
  }

  def name(n: String): NamedSeqPattern[A] =
    Bind(n, this)

  def +[A1 >: A](that: NamedSeqPattern[A1]): NamedSeqPattern[A1] =
    NCat(this, that)

  // we are renderable if all Wild/AnyElem are named
  def isRenderable: Boolean =
    this match {
      case NEmpty           => true
      case Bind(_, _)       => true
      case NSeqPart(Lit(_)) => true
      case NSeqPart(_)      => false
      case NCat(l, r)       =>
        l.isRenderable && r.isRenderable
    }

  def render[S: Monoid](names: Map[String, S])(fn: A => S): Option[S] = {
    val ms = Monoid[S]

    def loop(n: NamedSeqPattern[A], right: S): Option[S] =
      n match {
        case NEmpty      => Some(right)
        case Bind(nm, r) =>
          // since we have this name, we don't need to recurse
          names
            .get(nm)
            .map(seq => ms.combine(seq, right))
            .orElse(loop(r, right))
        case NSeqPart(SeqPart.Lit(c)) => Some(ms.combine(fn(c), right))
        case NSeqPart(_)              => None
        case NCat(l, r)               =>
          loop(r, right)
            .flatMap { right =>
              loop(l, right)
            }
      }

    loop(this, ms.empty)
  }

  def names: List[String] =
    this match {
      case Bind(name, nsp)      => name :: nsp.names
      case NEmpty | NSeqPart(_) => Nil
      case NCat(h, t)           => h.names ::: t.names
    }
}

object NamedSeqPattern {
  def fromPattern[A](p: SeqPattern[A]): NamedSeqPattern[A] =
    p.toList.foldRight(NEmpty: NamedSeqPattern[A]) { (part, right) =>
      NCat(NSeqPart(part), right)
    }

  val Wild: NamedSeqPattern[Nothing] = NSeqPart(SeqPart.Wildcard)
  val Any: NamedSeqPattern[Nothing] = NSeqPart(SeqPart.AnyElem)

  case class Bind[A](name: String, p: NamedSeqPattern[A])
      extends NamedSeqPattern[A]
  case object NEmpty extends NamedSeqPattern[Nothing]
  case class NSeqPart[A](part: SeqPart[A]) extends NamedSeqPattern[A]
  case class NCat[A](first: NamedSeqPattern[A], second: NamedSeqPattern[A])
      extends NamedSeqPattern[A]

  def fromLit[A](a: A): NamedSeqPattern[A] =
    NSeqPart(SeqPart.Lit(a))

  def matcher[E, I, S, R](
      split: Splitter[E, I, S, R]
  ): Matcher[NamedSeqPattern[E], S, (R, Map[String, S])] =
    new Matcher[NamedSeqPattern[E], S, (R, Map[String, S])] {
      def apply(nsp: NamedSeqPattern[E]): S => Option[(R, Map[String, S])] = {
        val machine = Impl.toMachine(nsp, Nil)

        Impl.matches(split, machine, Nil)
      }
    }

  private object Impl {
    def toMachine[A](
        n: NamedSeqPattern[A],
        right: List[Machine[A]]
    ): List[Machine[A]] =
      n match {
        case NEmpty        => right
        case Bind(name, n) =>
          StartName(name) :: toMachine(n, EndName :: right)
        case NSeqPart(p) => MSeqPart(p) :: right
        case NCat(l, r)  =>
          toMachine(l, toMachine(r, right))
      }

    sealed trait Machine[+A] derives CanEqual
    case class StartName(name: String) extends Machine[Nothing]
    case object EndName extends Machine[Nothing]
    case class MSeqPart[A](part: SeqPart[A]) extends Machine[A]

    def hasWildLeft(m: List[Machine[Any]]): Boolean =
      m match {
        case Nil                             => false
        case MSeqPart(SeqPart.Wildcard) :: _ => true
        case MSeqPart(_) :: _                => false
        case _ :: tail                       => hasWildLeft(tail)
      }

    import SeqPart.{AnyElem, Lit, SeqPart1, Wildcard}

    def capture[S](empty: S, capturing: List[String], res: Map[String, S])(
        fn: S => S
    ): Map[String, S] =
      capturing.foldLeft(res) { (mapB, n) =>
        val right = mapB.get(n) match {
          case None     => empty
          case Some(bv) => bv
        }
        mapB.updated(n, fn(right))
      }

    def matches[E, I, S, R](
        split: Splitter[E, I, S, R],
        m: List[Machine[E]],
        capturing: List[String]
    ): S => Option[(R, Map[String, S])] =
      m match {
        case Nil =>
          val res = Some((split.monoidResult.empty, Map.empty[String, S]))

          { (str: S) =>
            if (split.isEmpty(str)) res
            else None
          }
        case StartName(n) :: tail =>
          matches(split, tail, n :: capturing)
        case EndName :: tail =>
          capturing match {
            case Nil =>
              // $COVERAGE-OFF$
              sys.error("illegal End with no capturing")
            // $COVERAGE-ON$
            case n :: cap =>
              // if n captured nothing, we need
              // to add an empty list
              val e = split.emptySeq
              matches(split, tail, cap)
                .andThen {
                  case None         => None
                  case Some((r, m)) =>
                    Some {
                      val m1 = if (m.contains(n)) m else m.updated(n, e)
                      (r, m1)
                    }
                }
          }
        case MSeqPart(Wildcard) :: tail =>
          if (hasWildLeft(tail)) {
            // two adjacent wilds means this one matches nothing
            matches(split, tail, capturing)
          } else {
            val me = matchEnd(split, tail, capturing)

            me.andThen { stream =>
              stream.headOption
                .map { case (prefix, (rightR, rightBind)) =>
                  // now merge the prefix result
                  val resMatched = capturing.foldLeft(rightBind) { (st, n) =>
                    st.get(n) match {
                      case None        => st.updated(n, prefix)
                      case Some(right) =>
                        st.updated(n, split.catSeqs(prefix :: right :: Nil))
                    }
                  }
                  (rightR, resMatched)
                }
            }
          }
        case MSeqPart(p1: SeqPart1[E]) :: tail =>
          val someEmpty = Some(split.monoidResult.empty)

          val headm: I => Option[R] =
            p1 match {
              case AnyElem => { (_: I) => someEmpty }
              case Lit(c)  => split.matcher(c)
            }

          val tailm: S => Option[(R, Map[String, S])] =
            matches(split, tail, capturing)

          { (str: S) =>
            for {
              ht <- split.uncons(str)
              (h, t) = ht
              rh <- headm(h)
              rt <- tailm(t)
              (tailr, tailm) = rt
            } yield (
              split.monoidResult.combine(rh, tailr),
              capture(split.emptySeq, capturing, tailm)(split.cons(h, _))
            )
          }
      }

    def matchEnd[E, I, S, R](
        split: Splitter[E, I, S, R],
        m: List[Machine[E]],
        capturing: List[String]
    ): S => LazyList[(S, (R, Map[String, S]))] =
      m match {
        case Nil =>
          // we always match the end
          val res = (split.monoidResult.empty, Map.empty[String, S])

          { (str: S) =>
            (str, res) #:: LazyList.empty
          }
        case StartName(n) :: tail =>
          matchEnd(split, tail, n :: capturing)
        case EndName :: tail =>
          capturing match {
            case Nil =>
              // $COVERAGE-OFF$
              sys.error("illegal End with no capturing")
            // $COVERAGE-ON$
            case n :: cap =>
              // if n captured nothing, we need
              // to add an empty list
              val e = split.emptySeq
              matchEnd(split, tail, cap)
                .andThen { stream =>
                  stream.map { case (s, (r, m)) =>
                    val m1 = if (m.contains(n)) m else m.updated(n, e)
                    (s, (r, m1))
                  }
                }
          }
        case MSeqPart(Wildcard) :: tail =>
          // we can just go on matching the end, and sucking up
          // all current state
          matchEnd(split, tail, capturing)
        case MSeqPart(p1: SeqPart1[E]) :: tail =>
          val mtail = matches(split, tail, capturing)

          val splits = p1 match {
            case Lit(c)  => split.positions(c)
            case AnyElem => split.anySplits(_: S)
          }

          { (s: S) =>
            splits(s)
              .map { case (pre, i, r, post) =>
                mtail(post)
                  .map { case (rp, mapRes) =>
                    val res1 = split.monoidResult.combine(r, rp)
                    val res2 = capture(split.emptySeq, capturing, mapRes)(
                      split.cons(i, _)
                    )
                    (pre, (res1, res2))
                  }
              }
              .collect { case Some(res) => res }
          }
      }
  }
}
