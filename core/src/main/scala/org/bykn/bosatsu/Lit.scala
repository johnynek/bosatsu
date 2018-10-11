package org.bykn.bosatsu

sealed abstract class Lit {
  def repr: String =
    this match {
      case Lit.Integer(i) => i.toString
      case Lit.Str(s) => "\"" + s + "\"" // TODO this should escape
    }
}
object Lit {
  case class Integer(toInt: Int) extends Lit
  case class Str(toStr: String) extends Lit

  def apply(i: Int): Lit = Integer(i)
  def apply(str: String): Lit = Str(str)
}

