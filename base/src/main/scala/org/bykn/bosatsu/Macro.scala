package org.bykn.bosatsu

import java.io.File
import scala.io.Source
import scala.quoted.{Expr, Quotes}
import scala.util.control.NonFatal

object Macro {
  def loadFileInCompileImpl(
      fileExpr: Expr[String]
  )(using quotes: Quotes): Expr[String] = {
    import quotes.reflect.report

    def loadPath(path: String): Option[Expr[String]] =
      try {
        val f = new File(path)
        if (f.exists()) {
          val src = Source.fromFile(path, "UTF-8")
          try {
            val res = src.getLines().mkString("\n")
            Some(Expr(res))
          } finally {
            src.close()
          }
        } else {
          None
        }
      } catch {
        case NonFatal(err) =>
          report.errorAndAbort(
            s"could not read existing file: $path. Exception: $err"
          )
      }

    fileExpr.value match {
      case Some(path) =>
        loadPath(path)
          .orElse {
            loadPath(s"external/org_bykn_bosatsu/$path")
          }
          .getOrElse {
            report.errorAndAbort(
              s"no file found at: $path. working directory is ${System.getProperty("user.dir")}"
            )
          }
      case None =>
        report.errorAndAbort(
          s"expected string literal, found: ${fileExpr.show}"
        )
    }
  }
}
