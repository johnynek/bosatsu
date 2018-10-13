package org.bykn.bosatsu

import java.math.BigInteger

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

  def apply(i: Long): Lit = apply(BigInteger.valueOf(i))
  def apply(bi: BigInteger): Lit = Integer(bi)
  def apply(str: String): Lit = Str(str)
}

