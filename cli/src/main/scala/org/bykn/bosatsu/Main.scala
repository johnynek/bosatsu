package org.bykn.bosatsu

import cats.effect.{ExitCode, IO, IOApp}

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    PathModule.run(args) match {
      case Right(getOutput) =>
        getOutput
          .attempt
          .flatMap {
            case Right(out) =>
              PathModule.reportOutput(out).as(ExitCode.Success)
            case Left(PathModule.MainException.NoInputs(cmd)) =>
              val name = cmd.name
              IO.consoleForIO.errorln(s"no inputs given to $name")
                .as(ExitCode.Error)
            case Left(pe @ PathModule.MainException.ParseErrors(_, _, _)) =>
              IO.consoleForIO.errorln(pe.messages.mkString("\n"))
                .as(ExitCode.Error)
            case Left(pe @ PathModule.MainException.PackageErrors(_, _, _, _)) =>
              IO.consoleForIO.errorln(pe.messages.mkString("\n"))
                .as(ExitCode.Error)
            case Left(err) =>
              IO.consoleForIO.errorln("unknown error:\n") *>
                IO(err.printStackTrace(System.err)).as(ExitCode.Error)
          }
      case Left(help) =>
        IO {
          System.err.println(help.toString)
          ExitCode.Error
        }
    }
}
