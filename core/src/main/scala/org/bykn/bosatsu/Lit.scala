package org.bykn.bosatsu

import org.typelevel.paiges.{Document, Doc}
import java.math.BigInteger
import cats.parse.{Parser => P}

import Parser.escape

sealed abstract class Lit {
  def repr: String =
    this match {
      case Lit.Integer(i) => i.toString
      case Lit.Str(s)     => "\"" + escape('"', s) + "\""
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

  val EmptyStr: Str = Str("")

  def fromInt(i: Int): Lit = Integer(BigInteger.valueOf(i.toLong))
  def apply(i: Long): Lit = apply(BigInteger.valueOf(i))
  def apply(bi: BigInteger): Lit = Integer(bi)
  def apply(str: String): Lit = Str(str)

  val integerParser: P[Integer] =
    Parser.integerString.map { str =>
      Integer(new BigInteger(str.filterNot(_ == '_')))
    }

  val stringParser: P[Str] = {
    val q1 = '\''
    val q2 = '"'
    def str(q: Char): P[Str] =
      Parser.escapedString(q).map(Str(_))

    str(q1).orElse(str(q2))
  }

  implicit val litOrdering: Ordering[Lit] =
    new Ordering[Lit] {
      def compare(a: Lit, b: Lit): Int =
        (a, b) match {
          case (Integer(a), Integer(b)) => a.compareTo(b)
          case (Integer(_), Str(_))     => -1
          case (Str(_), Integer(_))     => 1
          case (Str(a), Str(b))         => a.compareTo(b)
        }
    }

  val parser: P[Lit] = integerParser.orElse(stringParser)

  implicit val document: Document[Lit] =
    Document.instance[Lit] {
      case Integer(i) =>
        Doc.text(i.toString)
      case Str(str) =>
        val q = if (str.contains('\'') && !str.contains('"')) '"' else '\''
        Doc.char(q) + Doc.text(escape(q, str)) + Doc.char(q)
    }
}
