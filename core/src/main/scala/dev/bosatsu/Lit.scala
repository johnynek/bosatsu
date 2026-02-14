package dev.bosatsu

import org.typelevel.paiges.{Document, Doc}
import java.math.BigInteger
import cats.parse.{Parser0 => P0, Parser => P}
import cats.Eq

import Parser.escape

sealed abstract class Lit {
  def repr: String =
    this match {
      case Lit.Integer(i) => i.toString
      case c @ Lit.Chr(_) =>
        ".'" + escape('\'', c.asStr) + "'"
      case Lit.Str(s)     => "\"" + escape('"', s) + "\""
      case f: Lit.Float64 =>
        Lit.Float64.toLiteralString(f)
    }

  def unboxToAny: Any
}
object Lit {
  implicit val eqLit: Eq[Lit] =
    Eq.instance {
      case (Integer(a), Integer(b)) => a == b
      case (Str(a), Str(b))         => a == b
      case (Chr(a), Chr(b))         => a == b
      case (a: Float64, b: Float64) => Float64.semanticEquals(a, b)
      case _                        => false
    }
  case class Integer(toBigInteger: BigInteger) extends Lit {
    def unboxToAny: Any = toBigInteger
  }

  object Integer {
    private val INT_MAX_CACHE = 1023
    private val INT_MIN_CACHE = -1024
    private val cache: Array[Integer] =
      (INT_MIN_CACHE to INT_MAX_CACHE).map { i =>
        new Integer(BigInteger.valueOf(i.toLong))
      }.toArray

    def apply(bi: BigInteger): Integer = {
      val i = bi.intValue
      if ((INT_MIN_CACHE <= i) && (i <= INT_MAX_CACHE)) {
        val int = cache(i - INT_MIN_CACHE)
        if (bi == int.toBigInteger) int
        else new Integer(bi)
      } else new Integer(bi)
    }

    def apply(l: Long): Integer =
      if ((INT_MIN_CACHE <= l) && (l <= INT_MAX_CACHE)) {
        cache(l.toInt - INT_MIN_CACHE)
      } else new Integer(BigInteger.valueOf(l))
  }

  // Means this lit could be the result of a string match
  sealed abstract class StringMatchResult extends Lit {
    def asStr: String
  }
  case class Str(toStr: String) extends StringMatchResult {
    def unboxToAny: Any = toStr
    def asStr = toStr
  }
  case class Chr(asStr: String) extends StringMatchResult {
    def toCodePoint: Int = asStr.codePointAt(0)
    def unboxToAny: Any = asStr
  }
  final case class Float64 private (toRawLongBits: Long) extends Lit {
    def toDouble: Double = java.lang.Double.longBitsToDouble(toRawLongBits)
    def unboxToAny: Any = java.lang.Double.valueOf(toDouble)

    override def equals(that: Any): Boolean =
      that match {
        case f: Float64 => Float64.semanticEquals(this, f)
        case _          => false
      }

    override val hashCode: Int =
      Float64.semanticHash(this)
  }
  object Float64 {
    private def canonicalDoubleForEq(d: Double): Double =
      if (java.lang.Double.isNaN(d)) java.lang.Double.NaN
      else if (d == 0.0d) 0.0d
      else d

    def semanticEquals(a: Float64, b: Float64): Boolean = {
      val ad = a.toDouble
      val bd = b.toDouble
      // Float literal equality follows runtime Float64 equality:
      // -0.0 == 0.0 and NaN == NaN.
      (ad == bd) || (java.lang.Double.isNaN(ad) && java.lang.Double.isNaN(bd))
    }

    def semanticHash(f: Float64): Int =
      java.lang.Double.hashCode(canonicalDoubleForEq(f.toDouble))

    private val NegZeroBits: Long =
      java.lang.Double.doubleToRawLongBits(-0.0d)

    private def finiteLiteralString(bits: Long, d: Double): String =
      if (bits == NegZeroBits) "-0.0"
      else {
        val raw = java.lang.Double.toString(d)
        if (
          raw.indexOf('.') >= 0 || raw
            .indexOf('e') >= 0 || raw.indexOf('E') >= 0
        ) raw
        else raw + ".0"
      }

    def fromDouble(d: Double): Float64 =
      Float64(java.lang.Double.doubleToRawLongBits(d))

    def fromRawLongBits(bits: Long): Float64 =
      Float64(bits)

    // This is the canonical Bosatsu source literal rendering used by repr/document.
    // Keep this aligned with float64Parser.
    def toLiteralString(f: Float64): String = {
      val bits = f.toRawLongBits
      val d = f.toDouble
      if (java.lang.Double.isNaN(d)) ".NaN"
      else if (d == java.lang.Double.POSITIVE_INFINITY) "\u221E"
      else if (d == java.lang.Double.NEGATIVE_INFINITY) "-\u221E"
      else finiteLiteralString(bits, d)
    }
  }
  object Chr {
    private def build(cp: Int): Chr =
      Chr((new java.lang.StringBuilder).appendCodePoint(cp).toString)

    private val cache: Array[Chr] =
      (0 until 256).map(build).toArray

    /** @throws IllegalArgumentException
      *   on a bad codepoint
      */
    def fromCodePoint(cp: Int): Chr =
      if ((0 <= cp) && (cp < 256)) cache(cp)
      else build(cp)
  }

  val EmptyStr: Str = Str("")

  def fromInt(i: Int): Lit = Integer(i.toLong)

  def fromChar(c: Char): Lit =
    if (0xd800 <= c && c < 0xe000)
      throw new IllegalArgumentException(
        s"utf-16 character int=${c.toInt} is not a valid single codepoint"
      )
    else Chr.fromCodePoint(c.toInt)

  def fromCodePoint(cp: Int): Lit = Chr.fromCodePoint(cp)

  def apply(i: Long): Lit = Integer(i)
  def apply(bi: BigInteger): Lit = Integer(bi)
  def apply(str: String): Lit = Str(str)

  val integerParser: P[Integer] =
    Parser.integerWithBase.map { case (bi, _) => Integer(bi) }

  private val float64TrailingDotEnd: P0[Unit] =
    P.not(P.charIn("abcdefghijklmnopqrstuvwxyz`(")).void

  private val digit: P[Char] = P.charIn('0' to '9')
  private val digits: P[String] = digit.rep.string
  private val digitPart: P[String] =
    (digits ~ (P.char('_') ~ digits).rep0).string
  private val exponentPart: P[String] =
    (P.charIn("eE") ~ P.charIn("+-").?.string ~ digitPart).string

  // This accepts python-style decimal float syntax plus optional leading sign.
  private val float64StringParser: P[String] = {
    val nanLiteral = P.string(".NaN").as("NaN")
    val leadingDot = (P.char('.') ~ digitPart ~ exponentPart.?).string
    val dotTail =
      (P.char('.') ~ (
        (digitPart ~ exponentPart.?).string
          .orElse(exponentPart)
          .orElse(P.pure("") <* float64TrailingDotEnd)
      )).string
    val digitStartBody = (digitPart ~ (dotTail.orElse(exponentPart))).string
    val body = leadingDot.orElse(digitStartBody)

    val infinity = P.char('\u221E').as("Infinity")
    val signed = (P.charIn("+-") ~ (infinity.orElse(body))).map {
      case ('-', "Infinity") => "-Infinity"
      case ('+', "Infinity") => "Infinity"
      case (sign, value)     => sign.toString + value
    }

    nanLiteral.orElse(signed).orElse(infinity).orElse(body)
  }

  private def parseFloat64(str: String): Option[Float64] = {
    val clean =
      if (str.indexOf('_') >= 0) str.filter(_ != '_')
      else str
    try Some(Float64.fromDouble(java.lang.Double.parseDouble(clean)))
    catch {
      case _: NumberFormatException => None
    }
  }

  val float64Parser: P[Float64] =
    float64StringParser.mapFilter(parseFloat64)

  val stringParser: P[Str] = {
    val q1 = '\''
    val q2 = '"'
    def str(q: Char): P[Str] =
      Parser.escapedString(q).map(Str(_))

    str(q1).orElse(str(q2))
  }

  val codePointParser: P[Chr] =
    (StringUtil.codepoint(P.string(".\""), P.char('"')) |
      StringUtil.codepoint(P.string(".'"), P.char('\'')))
      .map(Chr.fromCodePoint(_))

  implicit val litOrdering: Ordering[Lit] =
    new Ordering[Lit] {
      private def compareFloat64(a: Float64, b: Float64): Int = {
        val ad = a.toDouble
        val bd = b.toDouble
        val aNaN = java.lang.Double.isNaN(ad)
        val bNaN = java.lang.Double.isNaN(bd)
        if (aNaN) {
          if (bNaN) 0
          else -1
        } else if (bNaN) {
          1
        } else if (ad < bd) {
          -1
        } else if (ad > bd) {
          1
        } else {
          0
        }
      }

      def compare(a: Lit, b: Lit): Int =
        (a, b) match {
          case (Integer(a), Integer(b))                   => a.compareTo(b)
          case (Integer(_), _: Float64 | Str(_) | Chr(_)) => -1
          case (_: Float64, Integer(_))                   => 1
          case (a: Float64, b: Float64)            => compareFloat64(a, b)
          case (_: Float64, Str(_) | Chr(_))       => -1
          case (Chr(_), Integer(_) | (_: Float64)) => 1
          case (Chr(a), Chr(b))                    => a.compareTo(b)
          case (Chr(_), Str(_))                    => -1
          case (Str(_), Integer(_) | (_: Float64) | Chr(_)) => 1
          case (Str(a), Str(b))                             => a.compareTo(b)
        }
    }

  val parser: P[Lit] =
    float64Parser.backtrack | integerParser | stringParser | codePointParser

  implicit val document: Document[Lit] =
    Document.instance[Lit] {
      case Integer(i) =>
        Doc.text(i.toString)
      case Str(str) =>
        val q = if (str.contains('\'') && !str.contains('"')) '"' else '\''
        Doc.char(q) + Doc.text(escape(q, str)) + Doc.char(q)
      case c @ Chr(_) =>
        val str = c.asStr
        val (start, end) =
          if (str.contains('\'') && !str.contains('"')) (".\"", '"')
          else (".'", '\'')
        Doc.text(start) + Doc.text(escape(end, str)) + Doc.char(end)
      case f: Float64 =>
        Doc.text(Float64.toLiteralString(f))
    }
}
