package dev.bosatsu.tool

import cats.effect.{ExitCode => ceExitCode, IO, IOApp}
import dev.bosatsu.protobuf.ProtocPluginNodeMain

object Fs2Main extends IOApp {
  private val ProtocPluginCommandPrefix = List("tool", "protoc-plugin")

  private def isProtocPluginCommand(args: List[String]): Boolean =
    args.startsWith(ProtocPluginCommandPrefix)

  def fromToolExit(ec: ExitCode): ceExitCode =
    ceExitCode(ec.toInt)
  def run(args: List[String]): IO[ceExitCode] =
    if (isProtocPluginCommand(args))
      IO.blocking {
        ProtocPluginNodeMain.runFromStdio()
        ceExitCode.Success
      }
    else
      Fs2Module.runAndReport(args) match {
        case Right(io)  => io.map(fromToolExit)
        case Left(help) =>
          IO.blocking {
            System.err.println(help.toString)
            ceExitCode.Error
          }
      }
}
