package org.bykn.bosatsu

import reflect.macros.blackbox.Context
import java.io.File
import scala.io.Source
import scala.util.control.NonFatal

class Macro(val c: Context) {
  import c._, universe._
  def loadFileInCompileImpl(file: c.Expr[String]): c.Expr[String] = {

    def loadPath(s: String): c.Expr[String] =
      try {
        val res = Source.fromFile(s, "UTF-8").getLines().mkString("\n")
        c.Expr[String](q"$res")
      }
      catch {
        case NonFatal(err) =>
          c.abort(c.enclosingPosition, s"could not read existing file: $s. Exception: $err")
      }

    file.tree match {
      case Literal(Constant(s: String)) =>
        val normal = new File(s)
        if (normal.exists()) {
          loadPath(s)
        }
        else {
          val bazelPath = s"external/org_bykn_bosatsu/$s"
          val bazelFile = new File(bazelPath)
          if (bazelFile.exists()) {
            loadPath(bazelPath)
          }
          else {
            c.abort(c.enclosingPosition, s"no file found at: $s. working directory is ${System.getProperty("user.dir")}")
          }
        }
      case otherTree =>
        c.abort(c.enclosingPosition, s"expected string literal, found: $otherTree")
    }
  }
}

