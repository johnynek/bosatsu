package dev.bosatsu.tool

import cats.effect.{ExitCode => ceExitCode, IO, IOApp}
import scala.concurrent.duration.Duration

object Fs2Main extends IOApp {
  override def runtimeConfig =
    super.runtimeConfig.copy(cpuStarvationCheckInitialDelay = Duration.Inf)

  def fromToolExit(ec: ExitCode): ceExitCode =
    ceExitCode(ec.toInt)
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
