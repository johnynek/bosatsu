package dev.bosatsu

import cats.effect.{ExitCode, IO, IOApp}

object Main extends IOApp {
  def fromToolExit(ec: tool.ExitCode): ExitCode =
    ec match {
      case tool.ExitCode.Success => ExitCode.Success
      case tool.ExitCode.Error   => ExitCode.Error
    }
  def run(args: List[String]): IO[ExitCode] =
    PathModule.runAndReport(args) match {
      case Right(io)  => io.map(fromToolExit)
      case Left(help) =>
        IO.blocking {
          System.err.println(help.toString)
          ExitCode.Error
        }
    }
}
