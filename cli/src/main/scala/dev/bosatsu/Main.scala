package dev.bosatsu

import cats.effect.{ExitCode, IO, IOApp}
import dev.bosatsu.protobuf.ProtocPluginMain

object Main extends IOApp {
  private val ProtocPluginCommandPrefix = List("tool", "protoc-plugin")

  private def isProtocPluginCommand(args: List[String]): Boolean =
    args.startsWith(ProtocPluginCommandPrefix)

  def fromToolExit(ec: tool.ExitCode): ExitCode =
    ExitCode(ec.toInt)

  def run(args: List[String]): IO[ExitCode] =
    if (isProtocPluginCommand(args))
      IO.blocking {
        ProtocPluginMain.runFromStdio()
        ExitCode.Success
      }
    else
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
