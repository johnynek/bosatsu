package org.bykn.bosatsu

import cats.effect.{ExitCode, IO, IOApp}

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    PathModule.run(args) match {
      case Right(getOutput) =>
        PathModule.report(getOutput)
      case Left(help) =>
        IO.blocking {
          System.err.println(help.toString)
          ExitCode.Error
        }
    }
}
