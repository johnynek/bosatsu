package org.bykn.bosatsu.tool

import org.bykn.bosatsu.PlatformIO
import org.typelevel.paiges.Doc

import cats.syntax.all._

trait CliException { self: Exception =>
  def errDoc: Doc
  def stdOutDoc: Doc
  def exitCode: ExitCode

  def report[F[_], P](platform: PlatformIO[F, P]): F[ExitCode] = {
    import platform.moduleIOMonad

    platform.writeStdout(stdOutDoc) *>
    platform.writeError(errDoc).as(exitCode)
  }
}

object CliException {
  case class Basic(summary: String, exitCode: ExitCode = ExitCode.Error) extends Exception(summary) with CliException {
    def stdOutDoc: Doc = Doc.empty
    lazy val errDoc: Doc = Doc.text(summary)
  }
}