package dev.bosatsu

import cats.syntax.all._
import com.monovore.decline.{Command, Help, Opts}
import dev.bosatsu.tool.{CliException, ExitCode, Output}
import org.typelevel.paiges.Doc

/** This is an implementation of the CLI tool where Path is abstracted. The idea
  * is to allow it to be testable and usable in scalajs where we don't have
  * file-IO
  */
class MainModule[IO[_], Path](val platformIO: PlatformIO[IO, Path]) {
  type F[A] = IO[A]

  import platformIO._

  final def run(args: List[String]): Either[Help, IO[Output[Path]]] =
    command.parse(args.toList)

  final def runAndReport(args: List[String]): Either[Help, IO[ExitCode]] =
    run(args).map(report)

  def mainExceptionToString(ex: Throwable): Option[String] =
    ex match {
      case _: CliException => Option(ex.getMessage)
      case _               => None
    }

  private val versionCommand: Opts[IO[Output[Path]]] = {
    val gitOpt =
      BuildInfo.gitHeadCommit match {
        case Some(gitVer) =>
          // We can see at build time if the git version is set, if it isn't set
          // don't create the option (in practice this will almost never happen)
          Opts
            .flag(
              "git",
              help = s"use the git-sha ($gitVer) the compiler was built at.",
              "g"
            )
            .orFalse
            .map(if (_) Some(gitVer) else None)
        case None =>
          Opts(None)
      }

    Opts.subcommand("version", "print to stdout the version of the tool")(
      (
        gitOpt,
        Opts
          .option[Path](
            "output",
            "file to write to, if not set, use stdout.",
            "o"
          )
          .orNone
      ).mapN { (useGit, outPath) =>
        val vStr = useGit match {
          case Some(v) => v
          case None    => BuildInfo.version
        }

        moduleIOMonad.pure(Output.Basic(Doc.text(vStr), outPath): Output[Path])
      }
    )
  }

  def opts: Opts[IO[Output[Path]]] =
    Opts
      .subcommand("lib", "tools for working with bosatsu libraries")(
        library.Command
          .opts(platformIO)
      )
      .orElse {
        Opts.subcommand(
          "tool",
          "lower-level file-based commands for build tool integration"
        )(
          tool_command.ToolCommand
            .opts(platformIO)
        )
      }
      .orElse(versionCommand)
      .orElse {
        Opts.subcommand(
          "c-runtime",
          "tools for installing the bosatsu c runtime"
        )(
          cruntime.Command
            .opts(platformIO)
        )
      }

  def command: Command[IO[Output[Path]]] = {
    val versionInfo =
      (s"version: ${BuildInfo.version}" ::
        s"scala-version: ${BuildInfo.scalaVersion}" ::
        (BuildInfo.gitHeadCommit.toList.map(sha => s"git-sha: ${sha}")))
        .mkString("\n")

    Command(
      "bosatsu",
      s"a total and functional programming language\n\n$versionInfo"
    )(opts)
  }

  final def reportOutput(out: Output[Path]): IO[ExitCode] =
    out.report(platformIO)

  private def stackTraceToString(t: Throwable): String = {
    val stringWriter = new java.io.StringWriter()
    val printWriter = new java.io.PrintWriter(stringWriter)
    t.printStackTrace(printWriter)
    stringWriter.toString
  }

  def reportException(ex: Throwable): IO[ExitCode] =
    ex match {
      case ce: CliException => ce.report(platformIO)
      case _                =>
        platformIO.errorln("unknown error:\n") *>
          platformIO
            .errorln(stackTraceToString(ex))
            .as(ExitCode.Error)
    }

  def report(io: IO[Output[Path]]): IO[ExitCode] =
    io.attempt.flatMap {
      case Right(out) => reportOutput(out)
      case Left(err)  => reportException(err)
    }
}
