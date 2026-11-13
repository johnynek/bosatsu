package dev.bosatsu

import cats.effect.IO
import dev.bosatsu.tool.CliException
import dev.bosatsu.tool.ExitCode
import java.nio.ByteBuffer
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, Path}
import java.io.{ByteArrayOutputStream, IOException, InputStream, OutputStream, PrintStream}
import java.util.Base64

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

  /** A declared worker input. The digest is an opaque, immutable wire token. */
  final case class Input(path: String, digest: Vector[Byte])

  sealed trait Envelope

  final case class ActionRequest(
      arguments: List[String],
      inputs: List[Input],
      requestId: Int,
      verbosity: Int,
      sandboxDir: String
  ) extends Envelope

  /** V1 has no cancellation support, so controls cannot be dispatched as actions. */
  final case class CancelRequest(requestId: Int) extends Envelope

  final case class Response(
      requestId: Int,
      exitCode: Int,
      output: String,
      wasCancelled: Boolean = false
  )

  private final case class RequestFields(
      arguments: List[String] = Nil,
      inputs: List[Input] = Nil,
      requestId: Int = 0,
      verbosity: Int = 0,
      cancel: Boolean = false,
      sandboxDir: String = ""
  )

  private final case class InputFields(
      path: String = "",
      digest: Vector[Byte] = Vector.empty
  )

  /** Decode one complete protobuf-JSON WorkRequest object. */
  def decodeRequest(line: String): Either[String, Envelope] =
    Json.parserFile.parseAll(line) match {
      case Left(error) => Left(s"invalid worker JSON: $error")
      case Right(obj: Json.JObject) =>
        decodeRequestFields(obj.items).map { fields =>
          if (fields.cancel) CancelRequest(fields.requestId)
          else
            ActionRequest(
              fields.arguments,
              fields.inputs,
              fields.requestId,
              fields.verbosity,
              fields.sandboxDir
            )
        }
      case Right(_) => Left("worker request must be a JSON object")
    }

  private def decodeRequestFields(
      items: List[(String, Json)]
  ): Either[String, RequestFields] =
    items.foldLeft[Either[String, RequestFields]](Right(RequestFields())) {
      case (acc, (_, Json.JNull)) => acc
      case (acc, (name, value)) =>
        acc.flatMap { fields =>
          name match {
            case "arguments" =>
              decodeStrings(value, "arguments")
                .map(arguments => fields.copy(arguments = arguments))
            case "inputs" =>
              decodeInputs(value).map(inputs => fields.copy(inputs = inputs))
            case "requestId" | "request_id" =>
              decodeInt32(value, name).map(requestId => fields.copy(requestId = requestId))
            case "verbosity" =>
              decodeInt32(value, name).map(verbosity => fields.copy(verbosity = verbosity))
            case "cancel" =>
              decodeBoolean(value, name).map(cancel => fields.copy(cancel = cancel))
            case "sandboxDir" | "sandbox_dir" =>
              decodeString(value, name)
                .map(sandboxDir => fields.copy(sandboxDir = sandboxDir))
            case _ => Right(fields)
          }
        }
    }

  private def decodeStrings(value: Json, field: String): Either[String, List[String]] =
    value match {
      case Json.JArray(values) =>
        values.zipWithIndex.foldLeft[Either[String, Vector[String]]](Right(Vector.empty)) {
          case (acc, (Json.JString(value), _)) => acc.map(_ :+ value)
          case (acc, (_, index)) =>
            acc.flatMap(_ => Left(s"worker field '$field[$index]' must be a string"))
        }.map(_.toList)
      case _ => Left(s"worker field '$field' must be an array of strings")
    }

  private def decodeInputs(value: Json): Either[String, List[Input]] =
    value match {
      case Json.JArray(values) =>
        values.zipWithIndex.foldLeft[Either[String, Vector[Input]]](Right(Vector.empty)) {
          case (acc, (obj: Json.JObject, index)) =>
            acc.flatMap(inputs => decodeInput(obj.items, index).map(inputs :+ _))
          case (acc, (_, index)) =>
            acc.flatMap(_ => Left(s"worker field 'inputs[$index]' must be an object"))
        }.map(_.toList)
      case _ => Left("worker field 'inputs' must be an array of objects")
    }

  private def decodeInput(
      items: List[(String, Json)],
      index: Int
  ): Either[String, Input] =
    items.foldLeft[Either[String, InputFields]](Right(InputFields())) {
      case (acc, (_, Json.JNull)) => acc
      case (acc, ("path", value)) =>
        acc.flatMap(fields =>
          decodeString(value, s"inputs[$index].path").map(path => fields.copy(path = path))
        )
      case (acc, ("digest", value)) =>
        acc.flatMap(fields =>
          decodeDigest(value, s"inputs[$index].digest")
            .map(digest => fields.copy(digest = digest))
        )
      case (acc, _) => acc
    }.map(fields => Input(fields.path, fields.digest))

  private def decodeString(value: Json, field: String): Either[String, String] =
    value match {
      case Json.JString(str) => Right(str)
      case _                 => Left(s"worker field '$field' must be a string")
    }

  private def decodeBoolean(value: Json, field: String): Either[String, Boolean] =
    value match {
      case Json.JBool.True  => Right(true)
      case Json.JBool.False => Right(false)
      case _                => Left(s"worker field '$field' must be a boolean")
    }

  private def decodeInt32(value: Json, field: String): Either[String, Int] = {
    val asString = value match {
      case Json.JNumberStr(number) => Right(number)
      case Json.JString(number) => Right(number)
      case _ => Left(s"worker field '$field' must be an int32 number or quoted integer")
    }
    asString.flatMap { number =>
      try {
        val decimal = new java.math.BigDecimal(number)
        val min = java.math.BigDecimal.valueOf(Int.MinValue.toLong)
        val max = java.math.BigDecimal.valueOf(Int.MaxValue.toLong)
        if (decimal.stripTrailingZeros.scale > 0)
          Left(s"worker field '$field' must be an integer value")
        else if (decimal.compareTo(min) < 0 || decimal.compareTo(max) > 0)
          Left(s"worker field '$field' is outside the int32 range")
        else Right(decimal.intValue)
      }
      catch {
        case _: NumberFormatException =>
          Left(s"worker field '$field' is not a valid int32 value")
      }
    }
  }

  private def decodeDigest(value: Json, field: String): Either[String, Vector[Byte]] =
    value match {
      case Json.JString(encoded) =>
        try {
          val hasStandard = encoded.exists(c => c == '+' || c == '/')
          val hasUrlSafe = encoded.exists(c => c == '-' || c == '_')
          if (hasStandard && hasUrlSafe)
            Left(s"worker field '$field' is not valid base64")
          else {
            val decoder =
              if (hasUrlSafe) Base64.getUrlDecoder
              else Base64.getDecoder
            Right(decoder.decode(encoded).toVector)
          }
        } catch {
          case _: IllegalArgumentException =>
            Left(s"worker field '$field' is not valid base64")
        }
      case _ => Left(s"worker field '$field' must be a base64 string")
    }

  /** Encode the fixed WorkResponse shape without pretty-print line breaks. */
  def encodeResponse(response: Response): String =
    s"{\"requestId\":${response.requestId},\"exitCode\":${response.exitCode}," +
      s"\"output\":\"${JsonStringUtil.escape('"', response.output)}\"," +
      s"\"wasCancelled\":${response.wasCancelled}}"

  private enum Frame derives CanEqual {
    case End
    case Truncated
    case Line(bytes: Array[Byte])
  }

  /** Run the singleplex worker protocol without taking ownership of its streams. */
  def runPersistent(
      directory: Path,
      input: InputStream,
      output: OutputStream,
      diagnostics: PrintStream,
      runner: FileCommandRunner
  ): IO[ExitCode] = {
    def transportError(message: String): IO[ExitCode] =
      IO.blocking {
        diagnostics.println(s"bazel worker transport error: $message")
        diagnostics.flush()
        if (diagnostics.checkError())
          throw new IOException("failed to write bazel worker transport diagnostic")
      }.as(ExitCode.Error)

    def writeResponse(response: Response): IO[Unit] =
      IO.blocking {
        val bytes = (encodeResponse(response) + "\n").getBytes(StandardCharsets.UTF_8)
        output.write(bytes)
        output.flush()
        output match {
          case stream: PrintStream if stream.checkError() =>
            throw new IOException("failed to write bazel worker response")
          case _ => ()
        }
      }

    def respond(request: ActionRequest): IO[Unit] = {
      val response =
        if (request.sandboxDir.nonEmpty)
          IO.pure(
            Response(
              request.requestId,
              ExitCode.Error.toInt,
              s"unsupported nonempty sandboxDir: ${request.sandboxDir}\n"
            )
          )
        else
          runner.runWorker(directory, request.arguments).map { result =>
            Response(
              request.requestId,
              result.exitCode.toInt,
              result.stdout + result.stderr
            )
          }
      response.flatMap(writeResponse)
    }

    def decodeFrame(bytes: Array[Byte]): Either[String, Envelope] = {
      val decoder = StandardCharsets.UTF_8
        .newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
      try decodeRequest(decoder.decode(ByteBuffer.wrap(bytes)).toString)
      catch {
        case _: java.nio.charset.CharacterCodingException =>
          Left("worker request is not valid UTF-8")
      }
    }

    def readFrame: IO[Frame] = IO.blocking {
      val bytes = new ByteArrayOutputStream
      var finished = false
      var result: Frame = Frame.End
      var sawByte = false
      while (!finished) {
        val next = input.read()
        if (next < 0) {
          result = if (sawByte) Frame.Truncated else Frame.End
          finished = true
        } else if (next == '\n') {
          result = Frame.Line(bytes.toByteArray)
          finished = true
        } else {
          bytes.write(next)
          sawByte = true
        }
      }
      result
    }

    def loop: IO[ExitCode] = IO.defer {
      readFrame.attempt.flatMap {
        case Left(error) =>
          transportError(
            s"could not read worker request: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}"
          )
        case Right(Frame.End) => IO.pure(ExitCode.Success)
        case Right(Frame.Truncated) =>
          transportError("truncated worker request: EOF before newline")
        case Right(Frame.Line(bytes)) =>
          decodeFrame(bytes) match {
            case Left(error) => transportError(error)
            case Right(_: CancelRequest) => loop
            case Right(request: ActionRequest) => respond(request) *> loop
          }
      }
    }

    loop
  }

  /** Read one @file argument without shell parsing or recursive expansion. */
  def readParameterFile(directory: Path, parameterArgument: String): IO[Invocation] =
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
      case err =>
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
