package dev.bosatsu.pattern

import cats.{Monoid, Order}
import dev.bosatsu.{Identifier, Lit, Nullable, Pattern}
import dev.bosatsu.Nullable.*

sealed abstract class StrPart derives CanEqual
object StrPart {
  sealed abstract class Glob(val capture: Boolean) extends StrPart
  sealed abstract class CharPart(val capture: Boolean) extends StrPart
  case object WildStr extends Glob(false)
  case object IndexStr extends Glob(true)
  case object WildChar extends CharPart(false)
  case object IndexChar extends CharPart(true)
  case class LitStr(asString: String) extends StrPart

  // Compact adjacent literal runs across the whole list.
  // Also normalize non-capturing `${_}$.{_}` shapes to avoid generating a
  // search in Matchless.
  def compact(parts: List[StrPart]): List[StrPart] =
    parts match {
      case Nil =>
        Nil
      case head :: tail =>
        val compactedTail = compact(tail)
        head match {
          case LitStr("") =>
            compactedTail
          case LitStr(s0) =>
            compactedTail match {
              case LitStr(s1) :: rest =>
                LitStr(s0 + s1) :: rest
              case _ =>
                LitStr(s0) :: compactedTail
            }
          case WildStr =>
            compactedTail match {
              case WildChar :: _ =>
                val (chars, rest) = compactedTail.span(_ == WildChar)
                rest match {
                  case (glob: Glob) :: tail =>
                    // `${_}` before non-capturing chars and then `${...}` is
                    // redundant. Keep a normalized non-searching form.
                    chars ::: (glob :: tail)
                  case _ =>
                    // `${_}$.{_}` (both non-capturing) can be normalized to
                    // `$.{_}${_}`.
                    chars ::: (WildStr :: rest)
                }
              case _ =>
                WildStr :: compactedTail
            }
          case _ =>
            head :: compactedTail
        }
    }

  implicit val strPartOrder: Order[StrPart] = new Order[StrPart] {
    private def tag(sp: StrPart): Int =
      sp match {
        case WildStr   => 0
        case IndexStr  => 1
        case WildChar  => 2
        case IndexChar => 3
        case LitStr(_) => 4
      }

    def compare(left: StrPart, right: StrPart): Int =
      (left, right) match {
        case (LitStr(left), LitStr(right)) =>
          left.compareTo(right)
        case _ =>
          java.lang.Integer.compare(tag(left), tag(right))
      }
  }

  sealed abstract class MatchSize(val isExact: Boolean) {
    def charCount: Int
    def canMatch(cp: Int): Boolean
    // we know chars/2 <= cpCount <= chars for utf16
    def canMatchUtf16Count(chars: Int): Boolean
  }
  object MatchSize {
    case class Exactly(charCount: Int) extends MatchSize(true) {
      def canMatch(cp: Int): Boolean = cp == charCount
      def canMatchUtf16Count(chars: Int): Boolean = {
        val cpmin = chars / 2
        val cpmax = chars
        (cpmin <= charCount) && (charCount <= cpmax)
      }
    }
    case class AtLeast(charCount: Int) extends MatchSize(false) {
      def canMatch(cp: Int): Boolean = charCount <= cp
      def canMatchUtf16Count(chars: Int): Boolean = {
        val cpmax = chars
        // we have any cp in [cpmin, cpmax]
        // but we require charCount <= cp
        (charCount <= cpmax)
      }
    }

    private val atLeast0 = AtLeast(0)
    private val exactly0 = Exactly(0)
    private val exactly1 = Exactly(1)

    def from(sp: StrPart): MatchSize =
      sp match {
        case _: Glob     => atLeast0
        case _: CharPart => exactly1
        case LitStr(str) =>
          Exactly(str.codePointCount(0, str.length))
      }

    def apply[F[_]: cats.Foldable](f: F[StrPart]): MatchSize =
      cats.Foldable[F].foldMap(f)(from)

    implicit val monoidMatchSize: Monoid[MatchSize] =
      new Monoid[MatchSize] {
        def empty: MatchSize = exactly0
        def combine(l: MatchSize, r: MatchSize) =
          if (l.isExact && r.isExact) Exactly(l.charCount + r.charCount)
          else AtLeast(l.charCount + r.charCount)
      }
  }

  private val emptyStringArray: Array[String] = new Array[String](0)

  /** This performs the matchstring algorithm on a literal string it returns
    * null if there is no match, or the array of binds in order if there is a
    * match
    */
  def matchString(
      str: String,
      pat: List[StrPart],
      binds: Int
  ): Array[String] | Null = {
    val strLen = str.length()
    val results =
      if (binds > 0) new Array[String](binds) else emptyStringArray

    def loop(offset: Int, pat: List[StrPart], next: Int): Boolean =
      pat match {
        case Nil                    => offset == strLen
        case LitStr(expect) :: tail =>
          val len = expect.length
          str.regionMatches(offset, expect, 0, len) && loop(
            offset + len,
            tail,
            next
          )
        case (c: CharPart) :: tail =>
          try {
            val nextOffset = str.offsetByCodePoints(offset, 1)
            val n =
              if (c.capture) {
                results(next) = str.substring(offset, nextOffset)
                next + 1
              } else next

            loop(nextOffset, tail, n)
          } catch {
            case _: IndexOutOfBoundsException => false
          }
        case (h: Glob) :: tail =>
          tail match {
            case Nil =>
              // we capture all the rest
              if (h.capture) {
                results(next) = str.substring(offset)
              }
              true
            case rest @ ((_: CharPart) :: _) =>
              val matchableSizes = MatchSize[List](rest)

              def canMatch(off: Int): Boolean =
                matchableSizes.canMatch(str.codePointCount(off, strLen))

              // (.*)(.)tail2
              // this is a naive algorithm that just
              // checks at all possible later offsets
              // a smarter algorithm could see if there
              // are Lit parts that can match or not
              var matched = false
              var off1 = offset
              val n1 = if (h.capture) (next + 1) else next
              while (!matched && (off1 < strLen)) {
                matched = canMatch(off1) && loop(off1, rest, n1)
                if (!matched) {
                  off1 = off1 + Character.charCount(str.codePointAt(off1))
                }
              }

              matched && {
                if (h.capture) {
                  results(next) = str.substring(offset, off1)
                }
                true
              }
            case LitStr(expect) :: tail2 =>
              val next1 = if (h.capture) next + 1 else next

              val matchableSizes = MatchSize(tail2)

              def canMatch(off: Int): Boolean =
                matchableSizes.canMatchUtf16Count(strLen - off)

              var start = offset
              var result = false
              while (start >= 0) {
                val candidate = str.indexOf(expect, start)
                if (candidate >= 0) {
                  // we have to skip the current expect string
                  val nextOff = candidate + expect.length
                  val check1 =
                    canMatch(nextOff) && loop(nextOff, tail2, next1)
                  if (check1) {
                    // this was a match, write into next if needed
                    if (h.capture) {
                      results(next) = str.substring(offset, candidate)
                    }
                    result = true
                    start = -1
                  } else {
                    // we couldn't match here, try just after candidate
                    start = candidate + Character.charCount(
                      str.codePointAt(candidate)
                    )
                  }
                } else {
                  // no more candidates
                  start = -1
                }
              }
              result
            // $COVERAGE-OFF$
            case (_: Glob) :: _ =>
              // this should be an error at compile time since it
              // is never meaningful to have two adjacent globs
              sys.error(s"invariant violation, adjacent globs: $pat")
            // $COVERAGE-ON$
          }
      }

    if (loop(0, pat, 0)) results else null
  }

  def matchPattern(
      str: String,
      pattern: Pattern.StrPat
  ): Option[List[(Identifier.Bindable, Lit.StringMatchResult)]] = {
    val partList = pattern.parts.toList

    val sbinds: List[String => (Identifier.Bindable, Lit.StringMatchResult)] =
      partList
        .collect {
          // that each name is distinct
          // should be checked in the SourceConverter/TotalityChecking code
          case Pattern.StrPart.NamedStr(n) => { (value: String) =>
            (n, Lit.Str(value))
          }
          case Pattern.StrPart.NamedChar(n) => { (value: String) =>
            (n, Lit.Chr(value))
          }
        }

    val pat = partList.map {
      case Pattern.StrPart.NamedStr(_)  => StrPart.IndexStr
      case Pattern.StrPart.NamedChar(_) => StrPart.IndexChar
      case Pattern.StrPart.WildStr      => StrPart.WildStr
      case Pattern.StrPart.WildChar     => StrPart.WildChar
      case Pattern.StrPart.LitStr(s)    => StrPart.LitStr(s)
    }

    Nullable(StrPart.matchString(str, pat, sbinds.length)).fold(None) { result =>
      // we match:
      Some(
        result.iterator
          .zip(sbinds.iterator)
          .map { case (m, fn) => fn(m) }
          .toList
      )
    }
  }
}
