package dev.bosatsu

import cats.effect.{ExitCode, IO, IOApp}
import scala.concurrent.duration.Duration

object Main extends IOApp {
  override def runtimeConfig =
    super.runtimeConfig.copy(cpuStarvationCheckInitialDelay = Duration.Inf)

  def fromToolExit(ec: tool.ExitCode): ExitCode =
    ExitCode(ec.toInt)
  def run(args: List[String]): IO[ExitCode] =
    PathModule.runAndReport(args) match {
      case Right(io)  =>
        io.map(fromToolExit)
      case Left(help) =>
        IO.blocking {
          System.err.println(help.toString)
          ExitCode.Error
        }
    }
}
