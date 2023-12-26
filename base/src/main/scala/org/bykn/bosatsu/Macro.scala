package org.bykn.bosatsu
import scala.io.Source
import java.io.File
import scala.quoted.*

object Macro {
  /**
   * Loads a file *at compile time* as a means of embedding
   * external files into strings. This lets us avoid resources
   * which compilicate matters for scalajs.
   */
  private[bosatsu] inline def loadFileInCompile(inline file: String): String =
    ${loadFileInCompileImpl('file)}

  private def loadFileInCompileImpl(filename: Expr[String])(using Quotes): Expr[String] = {
    val fn = filename.valueOrAbort
    try {
      val f = new File(fn)
      if (f.exists()) {
        val res = Source.fromFile(f, "UTF-8").getLines().mkString("\n")
        Expr(res)
      }
      else {
        quotes.reflect.report.errorAndAbort(s"file $fn does not exist")
      }
    }
    catch {
      case scala.util.control.NonFatal(err) =>
        quotes.reflect.report.errorAndAbort(s"error reading file $fn: $err")
    }
  }

}

/*
package org.bykn.bosatsu
import reflect.macros.blackbox.Context
import java.io.File
import scala.io.Source
import scala.util.control.NonFatal

class Macro(val c: Context) {
  import c._, universe._
  def loadFileInCompileImpl(file: c.Expr[String]): c.Expr[String] = {

    def loadPath(s: String): Option[c.Expr[String]] =
      try {
        val f = new File(s)
        if (f.exists()) {
          val res = Source.fromFile(s, "UTF-8").getLines().mkString("\n")
          Some(c.Expr[String](q"$res"))
        }
        else {
          None
        }
      }
      catch {
        case NonFatal(err) =>
          c.abort(c.enclosingPosition, s"could not read existing file: $s. Exception: $err")
      }

    file.tree match {
      case Literal(Constant(s: String)) =>
        loadPath(s)
          .orElse {
            loadPath(s"external/org_bykn_bosatsu/$s")
          }
          .getOrElse {
            c.abort(
              c.enclosingPosition,
              s"no file found at: $s. working directory is ${System.getProperty("user.dir")}")
          }
      case otherTree =>
        c.abort(c.enclosingPosition, s"expected string literal, found: $otherTree")
    }
  }
}

*/