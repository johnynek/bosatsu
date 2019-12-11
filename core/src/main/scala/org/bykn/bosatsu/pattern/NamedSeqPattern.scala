package org.bykn.bosatsu.pattern

import cats.Monoid
import cats.implicits._

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

  import SeqPart.{AnyElem, Lit, SeqPart1, Wildcard}

  private type CaptureState[I, R] = Map[String, Either[List[I], R]]

  private def appendState[I, R](c: I, capturing: List[String], nameState: CaptureState[I, R]): CaptureState[I, R] =
    capturing.foldLeft(nameState) { (ns, n) =>
      val nextV = ns(n).left.map(c :: _)
      ns.updated(n, nextV)
    }

  private def toMatch[I, R](nameState: CaptureState[I, R]): Map[String, R] =
    // we have the match, convert the state to a match
    nameState.map {
      case (k, Right(v)) => (k, v)
      case (k, Left(strings)) =>
          sys.error(s"unclosed key: $k, $strings")
    }

  private def liftResultLeft[R](resLeft: R, capturing: List[String], res: Map[String, R])(implicit monoid: Monoid[R]): Map[String, R] =
    capturing.foldLeft(res) { (mapB, n) =>
      val newV = mapB.get(n) match {
        case None => resLeft
        case Some(bv) =>
          monoid.combine(resLeft, bv)
      }
      mapB.updated(n, newV)
    }

  private def matches[E, I, S, R](
    split: Splitter[E, I, S, R],
    m: List[Machine[E]],
    str: S,
    capturing: List[String],
    nameState: CaptureState[I, R]): Option[Map[String, R]] =

    m match {
      case Nil =>
        if (split.isEmpty(str)) Some(toMatch(nameState))
        else None
      case StartName(n) :: tail =>
        val ns1 = nameState.get(n) match {
          case None => nameState.updated(n, Left(Nil))
          case Some(s) => sys.error(s"illegal shadow: $n")
        }
        matches(split, tail, str, n :: capturing, ns1)
      case EndName :: tail =>
        capturing match {
          case Nil => sys.error("illegal End with no capturing")
          case n :: cap =>
          val ns1 = nameState.get(n) match {
            case Some(Left(parts)) =>
              val rparts = parts.reverse
              implicit val mon = split.monoidResult
              val r = rparts.foldMap(split.toResult)
              nameState.updated(n, Right(r))
            case res@(Some(Right(_)) | None) => sys.error(s"illegal end: $n, $res")
          }
          matches(split, tail, str, cap, ns1)
        }
      case MSeqPart(Wildcard) :: tail =>
        if (hasWildLeft(tail)) {
          // two adjacent wilds means this one matches nothing
          matches(split, tail, str, capturing, nameState)
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
          matchEnd(split, tail, str, capturing, nameState1)
            .headOption
            .map { case (prefix, rightResult) =>
              // now merge the result
              capturing.foldLeft(rightResult) { (st, n) =>
                nameState(n) match {
                  case Left(leftMatches) =>
                    // Left is accumulating in reverse order
                    // so we need to put the latest on the left
                    implicit val mon = split.monoidResult
                    val leftRes = leftMatches.reverse.foldMap(split.toResult)
                    val preRes = split.foldMap(prefix)
                    val res = mon.combineAll(leftRes :: preRes :: st(n) :: Nil)
                    st.updated(n, res)
                  case Right(r) =>
                    sys.error(s"both capturing and done: $n, $r")
                }
              }
            }
        }
      case MSeqPart(p1: SeqPart1[E]) :: tail =>
        split.uncons(str)
          .flatMap { case (h, t) =>

            // keep this lazy
            @inline def good = matches(split,
              tail,
              t,
              capturing,
              appendState(h, capturing, nameState))

            p1 match {
              case AnyElem => good
              case Lit(c) =>
                split.matcher(c)(h)
                  .flatMap { rh =>
                    good.map((rh, _))
                  }
                  .map { case (a, b) =>
                    liftResultLeft(a, capturing, b)(split.monoidResult)
                  }
            }
          }
    }

  private def matchEnd[E, I, S, R](
    split: Splitter[E, I, S, R],
    m: List[Machine[E]],
    str: S,
    capturing: List[String],
    nameState: CaptureState[I, R]): Stream[(S, Map[String, R])] =
    m match {
      case Nil =>
        // we always match the end
        val bindings = toMatch(nameState)
        (str, bindings) #:: Stream.Empty
      case StartName(n) :: tail =>
        val ns1 = nameState.get(n) match {
          case None => nameState.updated(n, Left(Nil))
          case Some(s) => sys.error(s"illegal shadow: $n")
        }
        matchEnd(split, tail, str, n :: capturing, ns1)
      case EndName :: tail =>
        capturing match {
          case Nil => sys.error("illegal End with no capturing")
          case n :: cap =>
          val ns1 = nameState.get(n) match {
            case Some(Left(parts)) =>
              val rparts = parts.reverse
              implicit val mon = split.monoidResult
              val r = rparts.foldMap(split.toResult)
              nameState.updated(n, Right(r))
            case res@(Some(Right(_)) | None) => sys.error(s"illegal end: $n, $res")
          }
          matchEnd(split, tail, str, cap, ns1)
        }
      case MSeqPart(Wildcard) :: tail =>
        // we can just go on matching the end, and sucking up
        // all current state
          matchEnd(split, tail, str, capturing, nameState)
      case MSeqPart(p1: SeqPart1[E]) :: tail =>
        val splits = p1 match {
          case Lit(c) => split.positions(c, str)
          case AnyElem => split.anySplits(str)
        }
        splits.map { case (pre, r, post) =>
          matches(split, tail, post, capturing, nameState)
            .map { res =>
              (pre, liftResultLeft(r, capturing, res)(split.monoidResult))
            }
        }
        .collect { case Some(res) => res }
    }
}


