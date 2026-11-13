package dev.bosatsu

import cats.effect.IO
import dev.bosatsu.tool.{CliException, ExitCode}
import java.nio.file.{Path, Paths}

object PathModule extends MainModule[IO, Path](IOPlatformIO) {
  def runCli(args: List[String]): IO[ExitCode] =
    args match {
      case "tool" :: "bazel-worker" :: workerArgs =>
        runBazelWorker(workerArgs)
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

  private def runBazelWorker(args: List[String]): IO[ExitCode] = {
    val directory = Paths.get(".")
    val run = args match {
      case "--persistent_worker" :: Nil =>
        FileCommandRunner.resource.use { runner =>
          BazelWorker.runPersistent(
            directory,
            System.in,
            System.out,
            System.err,
            runner
          )
        }
      case parameterFile :: Nil
          if parameterFile.startsWith("@") && parameterFile.length > 1 =>
        BazelWorker.readParameterFile(directory, parameterFile).flatMap { parsed =>
          FileCommandRunner.resource.use { runner =>
            runner.runWorkerTo(
              directory,
              parsed.commandArguments,
              System.out,
              System.err
            )
          }
        }
      case _ =>
        IO.raiseError[ExitCode](
          CliException.Basic(
            "tool bazel-worker requires exactly one @parameter-file argument or --persistent_worker"
          )
        )
    }
    run.handleErrorWith(reportException)
  }
}
