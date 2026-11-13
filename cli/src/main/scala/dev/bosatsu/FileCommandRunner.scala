package dev.bosatsu

import cats.effect.{IO, Resource}
import cats.effect.std.Supervisor
import dev.bosatsu.tool.{ExitCode, Output}
import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/** JVM entry point for file-based compiler invocations. A service owns a
  * compiler execution pool. Each call owns its path context, streams, pending
  * outputs and child fibers, even if the same IO is evaluated a second time.
  */
final class FileCommandRunner private (compilerEC: Par.EC) {
  import FileCommandRunner._

  def run(directory: Path, arguments: List[String]): IO[Result] =
    capture((stdout, stderr) => runTo(directory, arguments, stdout, stderr))

  /** Run a classified worker operation through the same request lifecycle as
    * standalone file commands.
    */
  def runWorker(directory: Path, arguments: List[String]): IO[Result] =
    capture { (stdout, stderr) =>
      runWorkerTo(directory, arguments, stdout, stderr)
    }

  private def capture(
      run: (PrintStream, PrintStream) => IO[ExitCode]
  ): IO[Result] =
    IO.defer {
      val out = new ByteArrayOutputStream
      val err = new ByteArrayOutputStream
      run(
        new PrintStream(out, false, UTF_8),
        new PrintStream(err, false, UTF_8)
      ).map(code => Result(code, out.toString(UTF_8), err.toString(UTF_8)))
    }

  /** The interactive CLI can stream large file-command reports without first
    * buffering a second copy. Stream ownership stays with the caller.
    */
  private[bosatsu] def runTo(
      directory: Path,
      arguments: List[String],
      stdout: PrintStream,
      stderr: PrintStream
  ): IO[ExitCode] =
    requestPlatform(directory, stdout, stderr).use { platform =>
      val module = new MainModule(platform)
      val execute = arguments match {
        case command :: _ if supports(command) =>
          IO.defer {
            module.run("tool" :: arguments) match {
              case Left(help) =>
                platform.errorln(help.toString).as(ExitCode.Error)
              case Right(result) => result.flatMap(platform.publish)
            }
          }
        case _ =>
          platform
            .errorln(
              s"expected a file command, got: ${arguments.headOption.getOrElse("<none>")}"
            )
            .as(ExitCode.Error)
      }
      execute.handleErrorWith(module.reportException)
    }

  /** Streaming counterpart used by PathModule and the later persistent
    * transport. Classification happens before any request is created.
    */
  private[bosatsu] def runWorkerTo(
      directory: Path,
      arguments: List[String],
      stdout: PrintStream,
      stderr: PrintStream
  ): IO[ExitCode] =
    arguments match {
      case operationName :: commandArguments =>
        BazelWorker.Operation.fromCommandName(operationName) match {
          case Some(operation) =>
            runTo(
              directory,
              operation.commandName :: commandArguments,
              stdout,
              stderr
            )
          case None =>
            IO.blocking(
              stderr.println(
                s"unsupported bazel worker operation: $operationName"
              )
            ).as(ExitCode.Error)
        }
      case Nil =>
        IO.blocking(stderr.println("expected a bazel worker operation"))
          .as(ExitCode.Error)
    }

  private[bosatsu] def requestPlatform(
      directory: Path,
      stdout: PrintStream,
      stderr: PrintStream
  ): Resource[IO, RequestPlatform] =
    Supervisor[IO].map { supervisor =>
      new RequestPlatform(
        directory.toAbsolutePath,
        stdout,
        stderr,
        compilerEC,
        supervisor
      )
    }
}

object FileCommandRunner {
  final case class Result(exitCode: ExitCode, stdout: String, stderr: String)

  // Commands which only read inputs and return reportable file/text outputs.
  // In particular, protoc-plugin owns binary stdin/stdout and transpile can
  // execute native processes. They retain their normal interactive CLI path.
  private[bosatsu] def supports(command: String): Boolean =
    command match {
      case "check" | "compile-packages" | "validate-graph" | "emit-c" |
          "headers" | "deps" | "show" | "assemble" | "extract-iface" =>
        true
      case _ => false
    }

  def resource: Resource[IO, FileCommandRunner] =
    IOPlatformIO.parResource.map(new FileCommandRunner(_))

  private[bosatsu] class RequestPlatform(
      directory: Path,
      override protected val stdout: PrintStream,
      override protected val stderr: PrintStream,
      compilerEC: Par.EC,
      supervisor: Supervisor[IO]
  ) extends IOPlatformIO {
    // Command computation may write an explicitly requested on-disk compiler
    // cache. Stage only declared artifacts, when the resulting Output is reported.
    def publish(out: Output[Path]): IO[ExitCode] =
      StagedOutputs
        .resource(err =>
          errorln(s"warning: cleaning staged outputs: ${err.getMessage}")
        )
        .use { staged =>
          val reporter = new RequestPlatform(
            directory,
            stdout,
            stderr,
            compilerEC,
            supervisor
          ) {
            override protected def outputPath(path: Path): IO[Path] =
              staged.prepare(diskPath(path))
          }
          out.report(reporter).flatMap {
            case ExitCode.Success => staged.publish.as(ExitCode.Success)
            case code             => IO.pure(code)
          }
        }

    override protected def diskPath(path: Path): Path = directory.resolve(path)

    override def withEC[A](fn: Par.EC ?=> IO[A]): IO[A] =
      fn(using compilerEC)

    override protected def startPromise(action: IO[Unit]): IO[Unit] =
      supervisor.supervise(action).void

    override protected def newTempDirectory(name: String): IO[Path] =
      IO.blocking(Files.createTempDirectory(directory, name))
  }
}
