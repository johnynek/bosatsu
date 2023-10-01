package org.bykn.bosatsu

import org.typelevel.paiges.{Document, Doc}
import java.math.BigInteger
import cats.parse.{Parser => P}

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
  case class Integer(toBigInteger: BigInteger) extends Lit {
    def unboxToAny: Any = toBigInteger
  }
  case class Str(toStr: String) extends Lit {
    def unboxToAny: Any = toStr
  }
  case class Chr(toCodePoint: Int) extends Lit {
    lazy val asStr: String =
      (new java.lang.StringBuilder).appendCodePoint(toCodePoint).toString

    def unboxToAny: Any = asStr
  }

  val EmptyStr: Str = Str("")

  def fromInt(i: Int): Lit = Integer(BigInteger.valueOf(i.toLong))
  def fromChar(c: Char): Lit =
    if (c >= 0xd800 && c < 0xdc00)
      throw new IllegalArgumentException(s"utf-16 character int=${c.toInt} is not a valid single codepoint")
    else Chr(c.toInt)
  def fromCodePoint(cp: Int): Lit = Chr(cp)

  def apply(i: Long): Lit = apply(BigInteger.valueOf(i))
  def apply(bi: BigInteger): Lit = Integer(bi)
  def apply(str: String): Lit = Str(str)

  val integerParser: P[Integer] =
    Parser.integerString.map { str => Integer(new BigInteger(str.filterNot(_ == '_'))) }

  val stringParser: P[Str] = {
    val q1 = '\''
    val q2 = '"'
    def str(q: Char): P[Str] =
      Parser.escapedString(q).map(Str(_))

    str(q1).orElse(str(q2))
  }

  val codePointParser: P[Chr] = {
    (StringUtil.codepoint(P.string(".\""), P.char('"')) |
      StringUtil.codepoint(P.string(".'"), P.char('\''))).map(Chr(_))
  }

  implicit val litOrdering: Ordering[Lit] =
    new Ordering[Lit] {
      def compare(a: Lit, b: Lit): Int =
        (a, b) match {
          case (Integer(a), Integer(b)) => a.compareTo(b)
          case (Integer(_), Str(_) | Chr(_)) => -1
          case (Chr(_), Integer(_)) => 1
          case (Chr(a), Chr(b)) => java.lang.Integer.compare(a, b)
          case (Chr(_), Str(_)) => -1
          case (Str(_), Integer(_)| Chr(_)) => 1
          case (Str(a), Str(b)) => a.compareTo(b)
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

