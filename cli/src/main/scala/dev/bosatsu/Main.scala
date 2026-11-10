package dev.bosatsu

import cats.effect.{ExitCode, IO, IOApp}
import scala.concurrent.duration.Duration

object Main extends IOApp {
  override def runtimeConfig =
    super.runtimeConfig.copy(cpuStarvationCheckInitialDelay = Duration.Inf)

  def fromToolExit(ec: tool.ExitCode): ExitCode =
    ExitCode(ec.toInt)

  def run(args: List[String]): IO[ExitCode] =
    PathModule.runCli(args).map(fromToolExit)
}
