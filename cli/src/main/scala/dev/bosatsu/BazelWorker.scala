package dev.bosatsu

import cats.effect.IO
import dev.bosatsu.tool.CliException
import java.nio.ByteBuffer
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, Path}

/** The file operations available through Bazel's worker entry point. */
object BazelWorker {
  enum Operation(val commandName: String) {
    case Headers extends Operation("headers")
    case CompilePackages extends Operation("compile-packages")
    case ValidateGraph extends Operation("validate-graph")
    case EmitC extends Operation("emit-c")
  }

  object Operation {
    val registered: List[Operation] = List(
      Headers,
      CompilePackages,
      ValidateGraph,
      EmitC
    )

    def fromCommandName(command: String): Option[Operation] =
      registered.find(_.commandName == command)
  }

  final case class Invocation(operation: String, arguments: List[String]) {
    def commandArguments: List[String] = operation :: arguments
  }

  /** Read one @file argument without shell parsing or recursive expansion. */
  def readParameterFile(
      directory: Path,
      parameterArgument: String
  ): IO[Invocation] =
    IO.blocking {
      parameterArgument match {
        case value if value.startsWith("@") && value.length > 1 =>
          val filename = value.substring(1)
          val path = directory.resolve(filename)
          val bytes = Files.readAllBytes(path)
          val decoder = StandardCharsets.UTF_8
            .newDecoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT)
          val content =
            try decoder.decode(ByteBuffer.wrap(bytes)).toString
            catch {
              case _: java.nio.charset.CharacterCodingException =>
                throw CliException.Basic(
                  s"parameter file '$filename' is not valid UTF-8"
                )
            }
          parse(content, filename)
        case _ =>
          throw CliException.Basic(
            "expected exactly one @parameter-file argument with a nonempty filename"
          )
      }
    }.handleErrorWith {
      case err: CliException => IO.raiseError(err)
      case err               =>
        val filename = parameterArgument.stripPrefix("@")
        IO.raiseError(
          CliException.Basic(
            s"cannot read parameter file '$filename': ${Option(err.getMessage).getOrElse(err.getClass.getSimpleName)}"
          )
        )
    }

  private def parse(content: String, filename: String): Invocation = {
    val lines = physicalLines(content)
    lines match {
      case Nil =>
        throw CliException.Basic(s"parameter file '$filename' is empty")
      case operation :: _ if operation.isEmpty =>
        throw CliException.Basic(
          s"parameter file '$filename' has an empty worker operation"
        )
      case operation :: arguments => Invocation(operation, arguments)
    }
  }

  // Split only on LF. A CR immediately before LF belongs to that CRLF
  // delimiter; other carriage returns remain part of their line.
  private def physicalLines(content: String): List[String] = {
    val lines = List.newBuilder[String]
    var start = 0
    var index = 0
    while (index < content.length) {
      if (content.charAt(index) == '\n') {
        val end =
          if (index > start && content.charAt(index - 1) == '\r') index - 1
          else index
        lines += content.substring(start, end)
        start = index + 1
      }
      index += 1
    }
    if (start < content.length) lines += content.substring(start)
    lines.result()
  }
}
