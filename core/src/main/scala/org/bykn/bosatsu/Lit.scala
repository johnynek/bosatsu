package org.bykn.bosatsu

sealed abstract class Lit
object Lit {
  case class Integer(toInt: Int) extends Lit
  case class Str(toStr: String) extends Lit

  def apply(i: Int): Lit = Integer(i)
  def apply(str: String): Lit = Str(str)
}

