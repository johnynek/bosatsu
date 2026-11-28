package dev.bosatsu

import java.io.File
import scala.io.Source
import scala.quoted.{Expr, Quotes}
import scala.util.control.NonFatal

object Macro {

  /** The file at `path`, relative to the repository root, which is the
    * compiler's working directory under sbt. Under bazel the repository is at
    * external/org_bykn_bosatsu instead.
    */
  private def repoFile(path: String): Option[File] =
    List(path, s"external/org_bykn_bosatsu/$path").iterator
      .map(new File(_))
      .find(_.exists())

  /** Reads `file`, joining its lines with `\n` */
  private def readFile(file: File)(using quotes: Quotes): String = {
    import quotes.reflect.report

    try {
      val src = Source.fromFile(file, "UTF-8")
      try src.getLines().mkString("\n")
      finally src.close()
    } catch {
      // $COVERAGE-OFF$ these error paths run only while the compiler compiles
      case NonFatal(err) =>
        report.errorAndAbort(
          s"could not read existing file: ${file.getPath}. Exception: $err"
        )
      // $COVERAGE-ON$
    }
  }

  /** Reads the repo file at `path`, or aborts compilation */
  def readRepoFile(path: String)(using quotes: Quotes): String =
    repoFile(path) match {
      case Some(file) => readFile(file)
      // $COVERAGE-OFF$ these error paths run only while the compiler compiles
      case None =>
        quotes.reflect.report.errorAndAbort(
          s"no file found at: $path. working directory is ${System.getProperty("user.dir")}"
        )
      // $COVERAGE-ON$
    }

  def loadFileInCompileImpl(
      fileExpr: Expr[String]
  )(using quotes: Quotes): Expr[String] = {
    import quotes.reflect.report

    fileExpr.value match {
      case Some(path) => Expr(readRepoFile(path))
      // $COVERAGE-OFF$ these error paths run only while the compiler compiles
      case None =>
        report.errorAndAbort(
          s"expected string literal, found: ${fileExpr.show}"
        )
      // $COVERAGE-ON$
    }
  }
}
