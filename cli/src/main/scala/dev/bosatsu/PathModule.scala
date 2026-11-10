package dev.bosatsu

import cats.effect.IO
import dev.bosatsu.tool.ExitCode
import java.nio.file.{Path, Paths}

object PathModule extends MainModule[IO, Path](IOPlatformIO) {
  def runCli(args: List[String]): IO[ExitCode] =
    args match {
      case "tool" :: (fileArgs @ (operation :: _))
          if FileCommandRunner.supports(operation) =>
        FileCommandRunner.resource.use { runner =>
          runner.runTo(Paths.get("."), fileArgs, System.out, System.err)
        }
      case _ =>
        runAndReport(args) match {
          case Right(io)  => io
          case Left(help) =>
            IOPlatformIO.errorln(help.toString).as(ExitCode.Error)
        }
    }
}
