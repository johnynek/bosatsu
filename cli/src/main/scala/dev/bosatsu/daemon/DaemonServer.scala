package dev.bosatsu.daemon

import cats.effect.{IO, Ref, Resource}
import cats.effect.std.Console
import cats.syntax.all._
import fs2.{Stream, Pipe}
import fs2.concurrent.SignallingRef
import fs2.io.net.unixsocket.{UnixSocketAddress, UnixSockets}
import fs2.io.file.{Files, Path}
import java.nio.file.{Path => JPath}

/**
 * Unix socket server for the Bosatsu debugger daemon.
 *
 * Listens on a Unix socket, receives JSON commands, and sends JSON responses.
 * State is maintained across connections using a Ref.
 *
 * Protocol:
 * - Each command is a single line of JSON
 * - Each response is a single line of JSON
 * - Connection stays open for multiple commands
 * - Server shuts down when it receives Shutdown command
 */
object DaemonServer {

  /** Default socket path */
  def defaultSocketPath: Path =
    Path(System.getProperty("java.io.tmpdir")) / "bosatsu-daemon.sock"

  /** Configuration for the daemon server */
  case class Config(
    socketPath: Path,
    trace: ProvenanceTrace,
    traceFile: String
  )

  /** Load a trace from a JSON file */
  def loadTrace(tracePath: Path): IO[Either[String, ProvenanceTrace]] =
    Files[IO].readAll(tracePath)
      .through(fs2.text.utf8.decode)
      .compile
      .string
      .map(DaemonJson.parseTrace)
      .handleErrorWith { err =>
        IO.pure(Left(s"Failed to read trace file: ${err.getMessage}"))
      }

  /** Run the daemon server */
  def run(config: Config): IO[Unit] = {
    val initialState = DaemonState.initial(config.trace, config.traceFile)

    Ref[IO].of(initialState).flatMap { stateRef =>
      SignallingRef[IO].of(false).flatMap { shutdownRef =>
        // Clean up old socket file if it exists
        val cleanup = Files[IO].deleteIfExists(config.socketPath).void

        val server = UnixSockets[IO].server(UnixSocketAddress(config.socketPath.toString))
          .evalTap(_ => Console[IO].println(s"[daemon] Listening on ${config.socketPath}"))
          .flatMap { socket =>
            // Handle each connection
            Stream.eval(Console[IO].println("[daemon] Client connected")) >>
            socket.reads
              .through(fs2.text.utf8.decode)
              .through(fs2.text.lines)
              .filter(_.nonEmpty)
              .evalMap { line =>
                handleLine(line, stateRef, shutdownRef)
              }
              .through(fs2.text.utf8.encode)
              .through(socket.writes)
              .handleErrorWith { err =>
                Stream.eval(Console[IO].println(s"[daemon] Connection error: ${err.getMessage}")).drain
              } ++
            Stream.eval(Console[IO].println("[daemon] Client disconnected")).drain
          }
          .interruptWhen(shutdownRef.discrete.takeWhile(!_).drain ++ Stream.emit(true))

        cleanup >> server.compile.drain >> cleanup
      }
    }
  }

  /** Handle a single line of input (JSON command) */
  private def handleLine(
    line: String,
    stateRef: Ref[IO, DaemonState],
    shutdownRef: SignallingRef[IO, Boolean]
  ): IO[String] = {
    DaemonJson.parseCommand(line) match {
      case Left(error) =>
        IO.pure(DaemonJson.renderResponse(DaemonResponse.Error(s"Parse error: $error")) + "\n")

      case Right(command) =>
        for {
          state <- stateRef.get
          result = CommandHandler.handle(command, state)
          _ <- stateRef.set(result.newState)
          _ <- if (result.shouldShutdown) shutdownRef.set(true) else IO.unit
          response = DaemonJson.renderResponse(result.response)
        } yield response + "\n"
    }
  }

  /** Start the daemon in the background */
  def startBackground(config: Config): IO[Unit] =
    run(config).start.void

  /** Resource that manages daemon lifecycle */
  def resource(config: Config): Resource[IO, Unit] =
    Resource.make(startBackground(config))(_ =>
      Files[IO].deleteIfExists(config.socketPath).void
    )
}

/**
 * Client for communicating with the daemon server.
 */
object DaemonClient {

  /** Send a command and get a response */
  def sendCommand(
    socketPath: Path,
    command: DaemonCommand
  ): IO[Either[String, DaemonResponse]] = {
    val commandJson = DaemonJson.renderCommand(command) + "\n"

    UnixSockets[IO].client(UnixSocketAddress(socketPath.toString)).use { socket =>
      for {
        // Send command
        _ <- Stream.emit(commandJson)
          .through(fs2.text.utf8.encode)
          .through(socket.writes)
          .compile
          .drain

        // Read response
        response <- socket.reads
          .through(fs2.text.utf8.decode)
          .through(fs2.text.lines)
          .filter(_.nonEmpty)
          .head
          .compile
          .lastOrError
      } yield {
        // Parse response
        dev.bosatsu.Json.parserFile.parseAll(response) match {
          case Left(err) =>
            Left(s"Failed to parse response: $err")
          case Right(json) =>
            parseResponse(json)
        }
      }
    }.handleErrorWith { err =>
      IO.pure(Left(s"Connection error: ${err.getMessage}"))
    }
  }

  /** Parse a JSON response */
  private def parseResponse(json: dev.bosatsu.Json): Either[String, DaemonResponse] = {
    json match {
      case obj: dev.bosatsu.Json.JObject =>
        obj.toMap.get("success") match {
          case Some(dev.bosatsu.Json.JBool.True) =>
            // Success response - we return a simplified success for now
            // Full response data parsing would be complex
            Right(DaemonResponse.Success(ResponseData.ShutdownResult("ok")))
          case Some(dev.bosatsu.Json.JBool.False) =>
            obj.toMap.get("error") match {
              case Some(dev.bosatsu.Json.JString(msg)) =>
                Right(DaemonResponse.Error(msg))
              case _ =>
                Right(DaemonResponse.Error("Unknown error"))
            }
          case _ =>
            Left("Missing 'success' field in response")
        }
      case _ =>
        Left("Response is not a JSON object")
    }
  }

  /** Check if daemon is running */
  def ping(socketPath: Path): IO[Boolean] =
    sendCommand(socketPath, DaemonCommand.Status).map(_.isRight)

  /** Request daemon shutdown */
  def shutdown(socketPath: Path): IO[Either[String, Unit]] =
    sendCommand(socketPath, DaemonCommand.Shutdown).map(_.map(_ => ()))
}
