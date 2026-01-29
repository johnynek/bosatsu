package dev.bosatsu.daemon

import cats.effect.{ExitCode, IO}
import cats.syntax.all._
import com.monovore.decline.{Command, Help, Opts}
import fs2.io.file.{Files, Path}

/**
 * CLI for daemon commands.
 *
 * Usage:
 *   bosatsu daemon generate <source.bosatsu> -o <trace.json>  - Generate trace from source
 *   bosatsu daemon start <trace.json>     - Start daemon with trace file
 *   bosatsu daemon status                 - Check if daemon is running
 *   bosatsu daemon stop                   - Stop the daemon
 *   bosatsu daemon <command>              - Send command to daemon
 */
object DaemonCli {

  sealed trait DaemonAction {
    def run: IO[ExitCode]
  }

  case class GenerateTraceAction(sourceFile: Path, outputFile: Path) extends DaemonAction {
    def run: IO[ExitCode] = {
      Files[IO].readAll(sourceFile)
        .through(fs2.text.utf8.decode)
        .compile
        .string
        .flatMap { source =>
          TraceGenerator.generateFromSource(source, sourceFile.toString) match {
            case Left(err) =>
              IO.println(s"Failed to generate trace: $err").as(ExitCode.Error)

            case Right(trace) =>
              val json = DaemonJson.renderTrace(trace)
              fs2.Stream.emit(json)
                .through(fs2.text.utf8.encode)
                .through(Files[IO].writeAll(outputFile))
                .compile
                .drain
                .flatMap { _ =>
                  IO.println(s"Generated trace with ${trace.nodeCount} nodes: $outputFile").as(ExitCode.Success)
                }
          }
        }
        .handleErrorWith { err =>
          IO.println(s"Error: ${err.getMessage}").as(ExitCode.Error)
        }
    }
  }

  case class StartAction(traceFile: Path, socketPath: Option[Path]) extends DaemonAction {
    def run: IO[ExitCode] = {
      val socket = socketPath.getOrElse(DaemonServer.defaultSocketPath)

      DaemonServer.loadTrace(traceFile).flatMap {
        case Left(err) =>
          IO.println(s"Failed to load trace: $err").as(ExitCode.Error)

        case Right(trace) =>
          val config = DaemonServer.Config(socket, trace, traceFile.toString)
          IO.println(s"Starting daemon with ${trace.nodeCount} nodes on ${socket}...") *>
            DaemonServer.run(config).as(ExitCode.Success)
              .handleErrorWith { err =>
                IO.println(s"Daemon error: ${err.getMessage}").as(ExitCode.Error)
              }
      }
    }
  }

  case class StopAction(socketPath: Option[Path]) extends DaemonAction {
    def run: IO[ExitCode] = {
      val socket = socketPath.getOrElse(DaemonServer.defaultSocketPath)

      DaemonClient.shutdown(socket).flatMap {
        case Right(_) =>
          IO.println("Daemon stopped.").as(ExitCode.Success)
        case Left(err) =>
          IO.println(s"Failed to stop daemon: $err").as(ExitCode.Error)
      }
    }
  }

  case class StatusAction(socketPath: Option[Path]) extends DaemonAction {
    def run: IO[ExitCode] = {
      val socket = socketPath.getOrElse(DaemonServer.defaultSocketPath)

      DaemonClient.ping(socket).flatMap {
        case true =>
          IO.println("Daemon is running.").as(ExitCode.Success)
        case false =>
          IO.println("Daemon is not running.").as(ExitCode.Error)
      }
    }
  }

  case class SendCommandAction(
    command: DaemonCommand,
    socketPath: Option[Path]
  ) extends DaemonAction {
    def run: IO[ExitCode] = {
      val socket = socketPath.getOrElse(DaemonServer.defaultSocketPath)

      DaemonClient.sendCommand(socket, command).flatMap {
        case Right(DaemonResponse.Success(data)) =>
          IO.println(formatResponseData(data)).as(ExitCode.Success)
        case Right(DaemonResponse.Error(msg)) =>
          IO.println(s"Error: $msg").as(ExitCode.Error)
        case Left(err) =>
          IO.println(s"Connection error: $err").as(ExitCode.Error)
      }
    }
  }

  private def formatResponseData(data: ResponseData): String = data match {
    case ResponseData.NodeList(nodes) =>
      nodes.map { n =>
        val value = n.value.map(v => s" = $v").getOrElse("")
        s"${n.id.value}: ${n.source}${value}"
      }.mkString("\n")

    case ResponseData.ExplainResult(_, tree, _, _) =>
      tree

    case ResponseData.FindResult(nodes) =>
      if (nodes.isEmpty) "No matching nodes found."
      else nodes.map(n => n.id.value).mkString(", ")

    case ResponseData.DepsResult(nodeId, deps) =>
      s"Dependencies of ${nodeId.value}:\n" +
        deps.map(d => s"  - ${d.id.value}").mkString("\n")

    case ResponseData.UsagesResult(nodeId, usages) =>
      s"Usages of ${nodeId.value}:\n" +
        usages.map(u => s"  - ${u.id.value}").mkString("\n")

    case ResponseData.FocusResult(msg, node) =>
      s"$msg\nFocused on: ${node.id.value}"

    case ResponseData.UnfocusResult(msg) =>
      msg

    case ResponseData.PathResult(path) =>
      path.map(_.id.value).mkString(" -> ")

    case ResponseData.ValueResult(_, value, _) =>
      value

    case ResponseData.SourceResult(nodeId, location) =>
      location.map(l => s"${l.file}:${l.line}:${l.column}")
        .getOrElse(s"No location for ${nodeId.value}")

    case ResponseData.SnippetResult(_, location, lines, scope, result) =>
      val code = lines.map { l =>
        val prefix = if (l.highlight) ">" else " "
        s"$prefix ${l.lineNum}: ${l.code}"
      }.mkString("\n")
      val scopeStr = scope.map(e => s"  ${e.name} = ${e.value}").mkString("\n")
      s"${location.file}:${location.line}\n$code\n\nScope:\n$scopeStr\n\nResult: ${result.name} = ${result.value}"

    case ResponseData.EvalResult(expr, result, _) =>
      s"$expr = $result"

    case ResponseData.StatusResult(running, traceFile, nodeCount, focusNodeId, resultNodeId, resultValue) =>
      s"""Status: ${if (running) "running" else "stopped"}
         |Trace file: $traceFile
         |Nodes: $nodeCount
         |Result: ${resultNodeId.value} = $resultValue
         |Focus: ${focusNodeId.map(_.value).getOrElse("none")}""".stripMargin

    case ResponseData.ShutdownResult(msg) =>
      msg
  }

  // CLI options
  val socketOpt: Opts[Option[Path]] =
    Opts.option[String]("socket", "Unix socket path", "s")
      .map(s => Path(s))
      .orNone

  val generateOpts: Opts[DaemonAction] =
    (
      Opts.argument[String]("source-file").map(s => Path(s)),
      Opts.option[String]("output", "Output trace file", "o").map(s => Path(s))
    ).mapN(GenerateTraceAction(_, _))

  val startOpts: Opts[DaemonAction] =
    (
      Opts.argument[String]("trace-file").map(s => Path(s)),
      socketOpt
    ).mapN(StartAction(_, _))

  val stopOpts: Opts[DaemonAction] =
    socketOpt.map(StopAction(_))

  val statusOpts: Opts[DaemonAction] =
    socketOpt.map(StatusAction(_))

  val listOpts: Opts[DaemonAction] = (
    Opts.flag("values", "Show values", "v").orFalse,
    socketOpt
  ).mapN { (showValues, socket) =>
    SendCommandAction(DaemonCommand.List(showValues), socket)
  }

  val explainOpts: Opts[DaemonAction] = (
    Opts.argument[String]("node-id").map(NodeId(_)).orNone,
    socketOpt
  ).mapN { (nodeId, socket) =>
    SendCommandAction(DaemonCommand.Explain(nodeId), socket)
  }

  val depsOpts: Opts[DaemonAction] = (
    Opts.argument[String]("node-id").map(NodeId(_)),
    socketOpt
  ).mapN { (nodeId, socket) =>
    SendCommandAction(DaemonCommand.Deps(nodeId), socket)
  }

  val usagesOpts: Opts[DaemonAction] = (
    Opts.argument[String]("node-id").map(NodeId(_)),
    socketOpt
  ).mapN { (nodeId, socket) =>
    SendCommandAction(DaemonCommand.Usages(nodeId), socket)
  }

  val focusOpts: Opts[DaemonAction] = (
    Opts.argument[String]("node-id").map(NodeId(_)),
    socketOpt
  ).mapN { (nodeId, socket) =>
    SendCommandAction(DaemonCommand.Focus(nodeId), socket)
  }

  val unfocusOpts: Opts[DaemonAction] =
    socketOpt.map(socket => SendCommandAction(DaemonCommand.Unfocus, socket))

  val valueOpts: Opts[DaemonAction] = (
    Opts.argument[String]("node-id").map(NodeId(_)),
    socketOpt
  ).mapN { (nodeId, socket) =>
    SendCommandAction(DaemonCommand.Value(nodeId), socket)
  }

  val sourceOpts: Opts[DaemonAction] = (
    Opts.argument[String]("node-id").map(NodeId(_)),
    socketOpt
  ).mapN { (nodeId, socket) =>
    SendCommandAction(DaemonCommand.Source(nodeId), socket)
  }

  val findOpts: Opts[DaemonAction] = (
    Opts.argument[String]("value"),
    socketOpt
  ).mapN { (value, socket) =>
    SendCommandAction(DaemonCommand.Find(value), socket)
  }

  val command: Command[DaemonAction] = {
    val subcommands = Opts
      .subcommand("generate", "Generate a trace file from Bosatsu source")(generateOpts)
      .orElse(Opts.subcommand("start", "Start the daemon with a trace file")(startOpts))
      .orElse(Opts.subcommand("stop", "Stop the daemon")(stopOpts))
      .orElse(Opts.subcommand("status", "Check daemon status")(statusOpts))
      .orElse(Opts.subcommand("list", "List all nodes")(listOpts))
      .orElse(Opts.subcommand("explain", "Explain a node's derivation")(explainOpts))
      .orElse(Opts.subcommand("deps", "Show dependencies of a node")(depsOpts))
      .orElse(Opts.subcommand("usages", "Show what uses a node")(usagesOpts))
      .orElse(Opts.subcommand("focus", "Set focus to a node")(focusOpts))
      .orElse(Opts.subcommand("unfocus", "Clear focus")(unfocusOpts))
      .orElse(Opts.subcommand("value", "Show full value of a node")(valueOpts))
      .orElse(Opts.subcommand("source", "Show source location")(sourceOpts))
      .orElse(Opts.subcommand("find", "Find nodes by value")(findOpts))

    Command("daemon", "Bosatsu debugger daemon commands")(subcommands)
  }

  def parse(args: List[String]): Either[Help, DaemonAction] =
    command.parse(args)

  def run(args: List[String]): IO[ExitCode] =
    parse(args) match {
      case Right(action) => action.run
      case Left(help) =>
        IO.println(help.toString).as(ExitCode.Error)
    }
}
