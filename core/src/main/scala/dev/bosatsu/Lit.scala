package dev.bosatsu

import org.typelevel.paiges.{Document, Doc}
import java.math.BigInteger
import cats.parse.{Parser => P}
import cats.Eq

import Parser.escape

sealed abstract class Lit {
  def repr: String =
    this match {
      case Lit.Integer(i) => i.toString
      case c @ Lit.Chr(_) =>
        ".'" + escape('\'', c.asStr) + "'"
      case Lit.Str(s) => "\"" + escape('"', s) + "\""
    }

  def unboxToAny: Any
}
object Lit {
  implicit val eqLit: Eq[Lit] =
    Eq.fromUniversalEquals
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
      def compare(a: Lit, b: Lit): Int =
        (a, b) match {
          case (Integer(a), Integer(b))      => a.compareTo(b)
          case (Integer(_), Str(_) | Chr(_)) => -1
          case (Chr(_), Integer(_))          => 1
          case (Chr(a), Chr(b))              => a.compareTo(b)
          case (Chr(_), Str(_))              => -1
          case (Str(_), Integer(_) | Chr(_)) => 1
          case (Str(a), Str(b))              => a.compareTo(b)
        }
    }

  val parser: P[Lit] = integerParser | stringParser | codePointParser

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
    }
}
