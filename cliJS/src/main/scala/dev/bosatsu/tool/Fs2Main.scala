package dev.bosatsu.tool

import cats.effect.{ExitCode => ceExitCode, IO, IOApp}

object Fs2Main extends IOApp {
  def fromToolExit(ec: ExitCode): ceExitCode =
    ec match {
      case ExitCode.Success => ceExitCode.Success
      case ExitCode.Error   => ceExitCode.Error
    }
  def run(args: List[String]): IO[ceExitCode] =
    Fs2Module.runAndReport(args) match {
      case Right(io)  => io.map(fromToolExit)
      case Left(help) =>
        IO.blocking {
          System.err.println(help.toString)
          ceExitCode.Error
        }
    }
}
