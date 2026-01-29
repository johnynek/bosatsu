package dev.bosatsu

import dev.bosatsu.tool
import dev.bosatsu.daemon.DaemonCli
import cats.effect.{ExitCode, IO, IOApp}

object Main extends IOApp {
  def fromToolExit(ec: tool.ExitCode): ExitCode =
    ec match {
      case tool.ExitCode.Success => ExitCode.Success
      case tool.ExitCode.Error   => ExitCode.Error
    }

  def run(args: List[String]): IO[ExitCode] =
    args match {
      // Handle daemon subcommand separately
      case "daemon" :: rest =>
        DaemonCli.run(rest)

      case _ =>
        PathModule.runAndReport(args) match {
          case Right(io)  => io.map(fromToolExit)
          case Left(help) =>
            IO.blocking {
              System.err.println(help.toString)
              ExitCode.Error
            }
        }
    }
}
