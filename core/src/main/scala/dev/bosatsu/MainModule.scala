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

  private val argumentDelimiter = "--"

  private def isEvalCommandPath(args: List[String]): Boolean =
    args match {
      case "eval" :: _           => true
      case "tool" :: "eval" :: _ => true
      case _                     => false
    }

  private def splitEvalPassthroughArgs(
      args: List[String]
  ): (List[String], List[String]) =
    if (isEvalCommandPath(args)) {
      // Only eval commands support passthrough args, and only after an explicit `--`.
      args.span(_ != argumentDelimiter) match {
        case (prefix, Nil)         => (prefix, Nil)
        case (prefix, _ :: suffix) => (prefix, suffix)
      }
    } else (args, Nil)

  final def run(args: List[String]): Either[Help, IO[Output[Path]]] = {
    val (parseArgs, evalPassthroughArgs) = splitEvalPassthroughArgs(args)
    command(evalPassthroughArgs).parse(parseArgs)
  }

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

  def opts(evalPassthroughArgs: List[String]): Opts[IO[Output[Path]]] =
    // Root help follows the left-to-right parser construction order, so keep
    // library-mode commands first and leave file-based compiler flows under tool.
    library.Command
      .opts(platformIO, evalPassthroughArgs)
      .orElse {
        Opts.subcommand(
          "tool",
          "lower-level file-based compiler commands for build tool integration"
        )(
          tool_command.ToolCommand
            .opts(platformIO, evalPassthroughArgs)
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

  def command(evalPassthroughArgs: List[String]): Command[IO[Output[Path]]] = {
    val versionInfo =
      (s"version: ${BuildInfo.version}" ::
        s"scala-version: ${BuildInfo.scalaVersion}" ::
        (BuildInfo.gitHeadCommit.toList.map(sha => s"git-sha: ${sha}")))
        .mkString("\n")

    Command(
      "bosatsu",
      "a total and functional programming language\n\n" +
        "top-level commands are the default repo/library workflows.\n" +
        "use `tool` for lower-level file-based compiler commands.\n\n" +
        versionInfo
    )(opts(evalPassthroughArgs))
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
