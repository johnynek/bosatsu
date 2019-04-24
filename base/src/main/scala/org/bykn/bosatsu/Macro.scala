package org.bykn.bosatsu

import reflect.macros.blackbox.Context
import scala.io._

class Macro(val c: Context) {
  import c._, universe._
  def smac(file: c.Expr[String]) = file.tree match {
    case Literal(Constant(s: String)) =>
      val res = Source.fromFile(s, "UTF-8").getLines.mkString("\n")
      q"$res"
  }
}

