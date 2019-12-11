package org.bykn.bosatsu.pattern

import cats.Monoid

sealed trait NamedSeqPattern[+A] {
  import NamedSeqPattern._
  import SeqPart._

  def unname: SeqPattern0[A] = {
    def loop(n: NamedSeqPattern[A], right: List[SeqPart[A]]): List[SeqPart[A]] =
      n match {
        case Bind(_, n) => loop(n, right)
        case NEmpty => right
        case NCat(first, second) =>
          val r2 = loop(second, right)
          loop(first, r2)
        case NSeqPart(p) => p :: right
      }

    SeqPattern0.fromList(loop(this, Nil))
  }

/*
  def matches(str: Sequence): Option[Map[String, Sequence]] =
    NamedSeqPattern
      .matches(NamedSeqPattern.toMachine(this, Nil), str, Nil, Map.empty) match {
        case None => None
        case Some(m) => Some(m.map { case (k, es) => (k, listToSeq(es)) })
      }
*/

  def name(n: String): NamedSeqPattern[A] =
    Bind(n, this)

  def +[A1 >: A](that: NamedSeqPattern[A1]): NamedSeqPattern[A1] =
    NCat(this, that)

  // we are renderable if all Wild/AnyElem are named
  def isRenderable: Boolean =
    this match {
      case NEmpty => true
      case Bind(_, _) => true
      case NSeqPart(Lit(_)) => true
      case NSeqPart(_) => false
      case NCat(l, r) =>
        l.isRenderable && r.isRenderable
    }


  def render[S: Monoid](names: Map[String, S])(fn: A => S): Option[S] = {
    val ms = Monoid[S]

    def loop(n: NamedSeqPattern[A], right: S): Option[S] =
      n match {
        case NEmpty => Some(right)
        case Bind(nm, r) =>
          // since we have this name, we don't need to recurse
          names.get(nm)
            .map { seq => ms.combine(seq, right) }
            .orElse(loop(r, right))
        case NSeqPart(SeqPart.Lit(c)) => Some(ms.combine(fn(c), right))
        case NSeqPart(_) => None
        case NCat(l, r) =>
          loop(r, right)
            .flatMap { right =>
              loop(l, right)
            }
      }

    loop(this, ms.empty)
  }
}

object NamedSeqPattern {
  def fromPattern[A](p: SeqPattern0[A]): NamedSeqPattern[A] =
    p.toList.foldRight(NEmpty: NamedSeqPattern[A]) { (part, right) =>
      NCat(NSeqPart(part), right)
    }

  val Wild: NamedSeqPattern[Nothing] = NSeqPart(SeqPart.Wildcard)
  val Any: NamedSeqPattern[Nothing] = NSeqPart(SeqPart.AnyElem)

  case class Bind[A](name: String, p: NamedSeqPattern[A]) extends NamedSeqPattern[A]
  case object NEmpty extends NamedSeqPattern[Nothing]
  case class NSeqPart[A](part: SeqPart[A]) extends NamedSeqPattern[A]
  case class NCat[A](first: NamedSeqPattern[A], second: NamedSeqPattern[A]) extends NamedSeqPattern[A]

  def toMachine[A](n: NamedSeqPattern[A], right: List[Machine[A]]): List[Machine[A]] =
    n match {
      case NEmpty => right
      case Bind(name, n) =>
        StartName(name) :: toMachine(n, EndName :: right)
      case NSeqPart(p) => MSeqPart(p) :: right
      case NCat(l, r) =>
        toMachine(l, toMachine(r, right))
    }

  sealed trait Machine[+A]
  case class StartName(name: String) extends Machine[Nothing]
  case object EndName extends Machine[Nothing]
  case class MSeqPart[A](part: SeqPart[A]) extends Machine[A]

  def hasWildLeft(m: List[Machine[Any]]): Boolean =
    m match {
      case Nil => false
      case MSeqPart(SeqPart.Wildcard) :: _ => true
      case MSeqPart(_) :: _ => false
      case _ :: tail => hasWildLeft(tail)
    }

/*
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
      case MSeqPart(Wildcard) :: tail =>
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
      case MSeqPart(p1: SeqPart1) :: tail =>
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
      case MSeqPart(Wildcard) :: tail =>
        // we can just go on matching the end, and sucking up
        // all current state
          matchEnd(tail, str, capturing, nameState)
      case MSeqPart(p1: SeqPart1) :: tail =>
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
*/
}


