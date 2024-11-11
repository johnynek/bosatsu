package org.bykn.bosatsu

/*

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
        } else {
          None
        }
      } catch {
        case NonFatal(err) =>
          c.abort(
            c.enclosingPosition,
            s"could not read existing file: $s. Exception: $err"
          )
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
              s"no file found at: $s. working directory is ${System.getProperty("user.dir")}"
            )
          }
      case otherTree =>
        c.abort(
          c.enclosingPosition,
          s"expected string literal, found: $otherTree"
        )
    }
  }
}
*/
import scala.quoted.*
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

/** Loads a file *at compile time* as a means of embedding external files into
  * strings. This lets us avoid resources which compilicate matters for
  * scalajs.
  */
object Macro {
  inline def loadFile(path: String): String = ${ loadFileImpl('path) }

  def loadFileImpl(pathExpr: Expr[String])(using Quotes): Expr[String] = {
    import quotes.reflect.*

    // Extract the file path from the inline argument
    val path = pathExpr.valueOrAbort

    // Read the file and check if it exists
    val filePath = Paths.get(path)
    if (!Files.exists(filePath)) {
      report.error(s"File at path '$path' does not exist.")
      '{""} // This won't be used, but it's required for return type consistency
    } else {
      // Read the file as UTF-8 string
      val content = new String(Files.readAllBytes(filePath), StandardCharsets.UTF_8)
      Expr(content)
    }
  }
}