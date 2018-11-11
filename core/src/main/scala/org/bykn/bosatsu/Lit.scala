package org.bykn.bosatsu

import org.typelevel.paiges.{Document, Doc}
import java.math.BigInteger
import fastparse.all._

sealed abstract class Lit {
  def repr: String =
    this match {
      case Lit.Integer(i) => i.toString
      case Lit.Str(s) => "\"" + s + "\"" // TODO this should escape
    }
}
object Lit {
  case class Integer(toBigInteger: BigInteger) extends Lit
  case class Str(toStr: String) extends Lit

  def fromInt(i: Int): Lit = Integer(BigInteger.valueOf(i.toLong))
  def apply(i: Long): Lit = apply(BigInteger.valueOf(i))
  def apply(bi: BigInteger): Lit = Integer(bi)
  def apply(str: String): Lit = Str(str)

  val integerParser: P[Integer] =
    Parser.integerString.map { str => Integer(new BigInteger(str)) }

  val stringParser: P[Str] = {
    val q1 = '\''
    val q2 = '"'
    def str(q: Char): P[Str] =
      Parser.escapedString(q).map(Str(_))

    str(q1) | str(q2)
  }

  val parser: P[Lit] = integerParser | stringParser

  implicit val document: Document[Lit] =
    Document.instance[Lit] {
      case Integer(i) =>
        Doc.text(i.toString)
      case Str(str) =>
        val q = if (str.contains('\'') && !str.contains('"')) '"' else '\''
        Doc.char(q) + Doc.text(Parser.escape(q, str)) + Doc.char(q)
    }
}

