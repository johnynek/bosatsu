package org.bykn.bosatsu.tool

import cats.effect.{ExitCode, IO, IOApp}

object Fs2Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    Fs2Module.run(args) match {
      case Right(getOutput) =>
        Fs2Module.report(getOutput)
      case Left(help) =>
        IO.blocking {
          System.err.println(help.toString)
          ExitCode.Error
        }
    }
}