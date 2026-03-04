package dev.bosatsu

import cats.effect.{ExitCode, IO, IOApp}

object Main extends IOApp {
  def fromToolExit(ec: tool.ExitCode): ExitCode =
    ExitCode(ec.toInt)
  def run(args: List[String]): IO[ExitCode] =
    PathModule.runAndReport(args) match {
      case Right(io)  =>
        IOPlatformIO.configureChicoryRuntimeCompilerCacheDir *>
          io.map(fromToolExit)
      case Left(help) =>
        IO.blocking {
          System.err.println(help.toString)
          ExitCode.Error
        }
    }
}
