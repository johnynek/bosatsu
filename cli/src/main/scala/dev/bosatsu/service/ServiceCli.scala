package dev.bosatsu.service

import cats.effect.{ExitCode, IO}
import cats.syntax.all._
import com.monovore.decline.{Command, Help, Opts}
import fs2.io.file.{Files, Path}
import dev.bosatsu.Par

/**
 * CLI for service commands.
 *
 * Usage:
 *   bosatsu service analyze <file.bosatsu>   - Analyze for I/O operations
 *   bosatsu service validate <file.bosatsu>  - Validate syntax
 *   bosatsu service build <file.bosatsu>     - Compile to deployable JS
 *   bosatsu service serve <file.bosatsu>     - Start HTTP server
 *   bosatsu service mcp <file.bosatsu>       - Start MCP server
 */
object ServiceCli {

  sealed trait ServiceAction {
    def run: IO[ExitCode]
  }

  case class AnalyzeAction(
    sourceFile: Path,
    functionName: Option[String],
    outputJson: Boolean
  ) extends ServiceAction {
    def run: IO[ExitCode] = {
      given ec: Par.EC = Par.ecFromExecutionContext(
        using scala.concurrent.ExecutionContext.global
      )

      Files[IO].readAll(sourceFile)
        .through(fs2.text.utf8.decode)
        .compile
        .string
        .flatMap { source =>
          ServiceBuilder.compileHandlers(source, sourceFile.toString) match {
            case Left(err) =>
              IO.println(s"Analysis failed: $err").as(ExitCode.Error)

            case Right(handlers) =>
              // Filter by function name if specified
              val filteredHandlers = functionName match {
                case Some(fn) => handlers.filter(_.name == fn)
                case None => handlers
              }
              if (filteredHandlers.isEmpty && functionName.isDefined) {
                IO.println(s"Function '${functionName.get}' not found").as(ExitCode.Error)
              } else {
              val analyses = filteredHandlers.map(_.analysis)
              val output = if (outputJson) {
                ServiceJson.renderAnalyses(analyses)
              } else {
                formatAnalyses(analyses)
              }
              IO.println(output).as(ExitCode.Success)
              }
          }
        }
        .handleErrorWith { err =>
          IO.println(s"Error: ${err.getMessage}").as(ExitCode.Error)
        }
    }

    private def formatAnalyses(analyses: List[ServiceAnalysis]): String = {
      analyses.map { a =>
        val ops = a.operations.map { op =>
          val kind = op.kind match {
            case OperationKind.Read => "read"
            case OperationKind.Write => "write"
            case OperationKind.Unknown => "unknown"
          }
          val batch = if (op.batchable) s" [batchable -> ${op.batchMethod.getOrElse("?")}]" else ""
          s"  - ${op.interface}.${op.method} ($kind)$batch"
        }.mkString("\n")

        val stats = s"""Statistics:
  Total operations: ${a.totalQueries}
  Batching efficiency: ${a.batchingEfficiency}
  Queries saved: ${a.queriesSaved}"""

        s"""Handler: ${a.handlerName}
File: ${a.sourceFile}
Operations:
$ops

$stats
"""
      }.mkString("\n---\n")
    }
  }

  case class ValidateAction(sourceFile: Path) extends ServiceAction {
    def run: IO[ExitCode] = {
      given ec: Par.EC = Par.ecFromExecutionContext(
        using scala.concurrent.ExecutionContext.global
      )

      Files[IO].readAll(sourceFile)
        .through(fs2.text.utf8.decode)
        .compile
        .string
        .flatMap { source =>
          ServiceBuilder.compileHandlers(source, sourceFile.toString) match {
            case Left(err) =>
              IO.println(s"Validation failed: $err").as(ExitCode.Error)
            case Right(handlers) =>
              IO.println(s"Valid: ${sourceFile} (${handlers.size} handlers)").as(ExitCode.Success)
          }
        }
        .handleErrorWith { err =>
          IO.println(s"Error: ${err.getMessage}").as(ExitCode.Error)
        }
    }
  }

  case class BuildAction(
    sourceFile: Path,
    output: Path,
    target: BuildTarget
  ) extends ServiceAction {
    def run: IO[ExitCode] = {
      given ec: Par.EC = Par.ecFromExecutionContext(
        using scala.concurrent.ExecutionContext.global
      )

      Files[IO].readAll(sourceFile)
        .through(fs2.text.utf8.decode)
        .compile
        .string
        .flatMap { source =>
          ServiceBuilder.build(source, sourceFile.toString, target) match {
            case Left(err) =>
              IO.println(s"Build failed: $err").as(ExitCode.Error)

            case Right(result) =>
              // Write build output
              Files[IO].createDirectories(output) >>
              fs2.Stream.emit(result.jsCode)
                .through(fs2.text.utf8.encode)
                .through(Files[IO].writeAll(output / "index.js"))
                .compile
                .drain
                .flatMap { _ =>
                  IO.println(s"Built ${result.handlers.size} handlers to ${output}").as(ExitCode.Success)
                }
          }
        }
        .handleErrorWith { err =>
          IO.println(s"Error: ${err.getMessage}").as(ExitCode.Error)
        }
    }
  }

  case class ServeAction(
    sourceFile: Path,
    port: Int,
    configFile: Option[Path],  // TODO: Not yet implemented
    staticDir: Option[Path]    // TODO: Not yet implemented
  ) extends ServiceAction {
    def run: IO[ExitCode] = {
      given ec: Par.EC = Par.ecFromExecutionContext(
        using scala.concurrent.ExecutionContext.global
      )

      // Warn about unimplemented options
      val warnings = List(
        configFile.map(_ => "Warning: --config option is not yet implemented"),
        staticDir.map(_ => "Warning: --static option is not yet implemented")
      ).flatten

      warnings.traverse_(msg => IO(System.err.println(msg))) >>
      Files[IO].readAll(sourceFile)
        .through(fs2.text.utf8.decode)
        .compile
        .string
        .flatMap { source =>
          ServiceBuilder.compileHandlers(source, sourceFile.toString) match {
            case Left(err) =>
              IO.println(s"Compilation failed: $err").as(ExitCode.Error)

            case Right(handlers) =>
              IO.println(s"Starting HTTP server with ${handlers.size} handlers on port ${port}...") >>
              HttpServer.serve(handlers, port, staticDir).as(ExitCode.Success)
          }
        }
        .handleErrorWith { err =>
          IO.println(s"Error: ${err.getMessage}").as(ExitCode.Error)
        }
    }
  }

  case class McpAction(
    sourceFile: Path,
    configFile: Option[Path],  // TODO: Not yet implemented
    name: Option[String]
  ) extends ServiceAction {
    def run: IO[ExitCode] = {
      given ec: Par.EC = Par.ecFromExecutionContext(
        using scala.concurrent.ExecutionContext.global
      )

      // Warn about unimplemented options
      val warnings = configFile.map(_ => "Warning: --config option is not yet implemented").toList

      val serverName = name.getOrElse(sourceFile.fileName.toString.replace(".bosatsu", ""))

      warnings.traverse_(msg => IO(System.err.println(msg))) >>
      Files[IO].readAll(sourceFile)
        .through(fs2.text.utf8.decode)
        .compile
        .string
        .flatMap { source =>
          ServiceBuilder.compileHandlers(source, sourceFile.toString) match {
            case Left(err) =>
              IO.println(s"Compilation failed: $err").as(ExitCode.Error)

            case Right(handlers) =>
              // Write to stderr, not stdout - MCP uses stdout for JSON-RPC
              IO(System.err.println(s"Starting MCP server '$serverName' with ${handlers.size} tools...")) >>
              McpServer.serve(serverName, handlers).as(ExitCode.Success)
          }
        }
        .handleErrorWith { err =>
          IO.println(s"Error: ${err.getMessage}").as(ExitCode.Error)
        }
    }
  }

  // ==========================================================================
  // CLI Options
  // ==========================================================================

  val analyzeOpts: Opts[ServiceAction] = (
    Opts.argument[String]("source-file").map(s => Path(s)),
    Opts.option[String]("function", "Analyze specific function", "f").orNone,
    Opts.flag("json", "Output in JSON format", "j").orFalse
  ).mapN(AnalyzeAction(_, _, _))

  val validateOpts: Opts[ServiceAction] =
    Opts.argument[String]("source-file").map(s => Path(s)).map(ValidateAction(_))

  val buildOpts: Opts[ServiceAction] = (
    Opts.argument[String]("source-file").map(s => Path(s)),
    Opts.option[String]("output", "Output directory", "o").map(s => Path(s)).withDefault(Path("dist")),
    Opts.option[String]("target", "Build target: standalone, vercel, aws-lambda", "t")
      .map {
        case "vercel" => BuildTarget.Vercel
        case "aws-lambda" => BuildTarget.AwsLambda
        case _ => BuildTarget.Standalone
      }
      .withDefault(BuildTarget.Standalone)
  ).mapN(BuildAction(_, _, _))

  val serveOpts: Opts[ServiceAction] = (
    Opts.argument[String]("source-file").map(s => Path(s)),
    Opts.option[Int]("port", "Port to listen on", "p").withDefault(3000),
    Opts.option[String]("config", "Config file for service implementations", "c").map(s => Path(s)).orNone,
    Opts.option[String]("static", "Serve static files from directory", "s").map(s => Path(s)).orNone
  ).mapN(ServeAction(_, _, _, _))

  val mcpOpts: Opts[ServiceAction] = (
    Opts.argument[String]("source-file").map(s => Path(s)),
    Opts.option[String]("config", "Config file for service implementations", "c").map(s => Path(s)).orNone,
    Opts.option[String]("name", "MCP server name", "n").orNone
  ).mapN(McpAction(_, _, _))

  val command: Command[ServiceAction] = {
    val subcommands = Opts
      .subcommand("analyze", "Analyze handlers for I/O operations and batching")(analyzeOpts)
      .orElse(Opts.subcommand("validate", "Validate Bosatsu service handler syntax")(validateOpts))
      .orElse(Opts.subcommand("build", "Compile handlers to deployable JavaScript")(buildOpts))
      .orElse(Opts.subcommand("serve", "Start HTTP server for handlers")(serveOpts))
      .orElse(Opts.subcommand("mcp", "Start MCP server exposing handlers as AI tools")(mcpOpts))

    Command("service", "Bosatsu service commands")(subcommands)
  }

  def parse(args: List[String]): Either[Help, ServiceAction] =
    command.parse(args)

  def run(args: List[String]): IO[ExitCode] =
    parse(args) match {
      case Right(action) => action.run
      case Left(help) =>
        IO.println(help.toString).as(ExitCode.Error)
    }
}
