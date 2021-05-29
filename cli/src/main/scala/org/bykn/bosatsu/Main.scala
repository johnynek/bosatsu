package org.bykn.bosatsu

import scala.util.{ Failure, Success, Try }

import cats.effect.{ExitCode, IOApp}

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    PathModule.run(args) match {
      case Right(getOutput) =>
        for {
          out <- getOutput
          _ <- PathModule.reportOutput(out)
        } yield ExitCode.Success
      case Left(help) =>
        IO {
          System.err.println(help.toString)
          ExitCode.Error
        }
    }
}
