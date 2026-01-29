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

  /** Start the daemon in the background, returning the fiber for cancellation */
  def startBackground(config: Config): IO[cats.effect.FiberIO[Unit]] =
    run(config).start

  /** Resource that manages daemon lifecycle */
  def resource(config: Config): Resource[IO, Unit] =
    Resource.make(startBackground(config))(fiber =>
      fiber.cancel >> Files[IO].deleteIfExists(config.socketPath).void
    ).void
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

  /** Parse a JSON response - package private for testing */
  private[daemon] def parseResponse(json: dev.bosatsu.Json): Either[String, DaemonResponse] = {
    json match {
      case obj: dev.bosatsu.Json.JObject =>
        obj.toMap.get("success") match {
          case Some(dev.bosatsu.Json.JBool.True) =>
            // Parse the data field to determine the actual response type
            obj.toMap.get("data") match {
              case Some(dataObj: dev.bosatsu.Json.JObject) =>
                parseResponseData(dataObj).map(DaemonResponse.Success(_))
              case Some(_) =>
                Left("'data' field is not a JSON object")
              case None =>
                Left("Missing 'data' field in success response")
            }
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

  /** Parse the data field of a success response */
  private def parseResponseData(obj: dev.bosatsu.Json.JObject): Either[String, ResponseData] = {
    import dev.bosatsu.Json._
    val map = obj.toMap

    map.get("type") match {
      case Some(JString("nodeList")) =>
        parseNodeList(map)
      case Some(JString("explain")) =>
        parseExplainResult(map)
      case Some(JString("find")) =>
        parseFindResult(map)
      case Some(JString("deps")) =>
        parseDepsResult(map)
      case Some(JString("usages")) =>
        parseUsagesResult(map)
      case Some(JString("focus")) =>
        parseFocusResult(map)
      case Some(JString("unfocus")) =>
        map.get("message") match {
          case Some(JString(msg)) => Right(ResponseData.UnfocusResult(msg))
          case _ => Right(ResponseData.UnfocusResult(""))
        }
      case Some(JString("path")) =>
        parsePathResult(map)
      case Some(JString("value")) =>
        parseValueResult(map)
      case Some(JString("source")) =>
        parseSourceResult(map)
      case Some(JString("snippet")) =>
        parseSnippetResult(map)
      case Some(JString("eval")) =>
        parseEvalResult(map)
      case Some(JString("status")) =>
        parseStatusResult(map)
      case Some(JString("shutdown")) =>
        map.get("message") match {
          case Some(JString(msg)) => Right(ResponseData.ShutdownResult(msg))
          case _ => Right(ResponseData.ShutdownResult("ok"))
        }
      case Some(JString(other)) =>
        Left(s"Unknown response type: $other")
      case _ =>
        Left("Missing 'type' field in response data")
    }
  }

  /** Parse a NodeSummary from JSON */
  private def parseNodeSummary(json: dev.bosatsu.Json): Either[String, ResponseData.NodeSummary] = {
    import dev.bosatsu.Json._
    json match {
      case obj: JObject =>
        val map = obj.toMap
        for {
          id <- map.get("id").collect { case JString(s) => NodeId(s) }.toRight("Missing 'id'")
          typeInfo <- map.get("typeInfo").collect { case JString(s) => s }.toRight("Missing 'typeInfo'")
          source <- map.get("source").collect { case JString(s) => s }.toRight("Missing 'source'")
        } yield ResponseData.NodeSummary(
          id = id,
          value = map.get("value").collect { case JString(s) => s },
          typeInfo = typeInfo,
          source = source,
          bindingName = map.get("bindingName").collect { case JString(s) => s },
          location = parseSourceLocation(map.get("location"))
        )
      case _ => Left("NodeSummary is not a JSON object")
    }
  }

  /** Parse a SourceLocation from JSON */
  private def parseSourceLocation(json: Option[dev.bosatsu.Json]): Option[SourceLocation] = {
    import dev.bosatsu.Json._
    json.flatMap {
      case obj: JObject =>
        val map = obj.toMap
        for {
          file <- map.get("file").collect { case JString(s) => s }
          line <- map.get("line").collect { case JNumberStr(s) => s.toInt }
          column <- map.get("column").collect { case JNumberStr(s) => s.toInt }
        } yield SourceLocation(file, line, column)
      case _ => None
    }
  }

  /** Parse NodeList response */
  private def parseNodeList(map: Map[String, dev.bosatsu.Json]): Either[String, ResponseData] = {
    import dev.bosatsu.Json._
    map.get("nodes") match {
      case Some(JArray(nodes)) =>
        nodes.toList.traverse(parseNodeSummary).map(ResponseData.NodeList(_))
      case _ => Left("Missing 'nodes' array in nodeList response")
    }
  }

  /** Parse Explain response */
  private def parseExplainResult(map: Map[String, dev.bosatsu.Json]): Either[String, ResponseData] = {
    import dev.bosatsu.Json._
    for {
      nodeId <- map.get("nodeId").collect { case JString(s) => NodeId(s) }.toRight("Missing 'nodeId'")
      tree <- map.get("tree").collect { case JString(s) => s }.toRight("Missing 'tree'")
      compact <- map.get("compact").collect { case JString(s) => s }.toRight("Missing 'compact'")
      node <- map.get("node").toRight("Missing 'node'").flatMap(parseNodeSummary)
    } yield ResponseData.ExplainResult(nodeId, tree, compact, node)
  }

  /** Parse Find response */
  private def parseFindResult(map: Map[String, dev.bosatsu.Json]): Either[String, ResponseData] = {
    import dev.bosatsu.Json._
    map.get("nodes") match {
      case Some(JArray(nodes)) =>
        nodes.toList.traverse(parseNodeSummary).map(ResponseData.FindResult(_))
      case _ => Left("Missing 'nodes' array in find response")
    }
  }

  /** Parse Deps response */
  private def parseDepsResult(map: Map[String, dev.bosatsu.Json]): Either[String, ResponseData] = {
    import dev.bosatsu.Json._
    for {
      nodeId <- map.get("nodeId").collect { case JString(s) => NodeId(s) }.toRight("Missing 'nodeId'")
      deps <- map.get("dependencies") match {
        case Some(JArray(nodes)) => nodes.toList.traverse(parseNodeSummary)
        case _ => Left("Missing 'dependencies' array")
      }
    } yield ResponseData.DepsResult(nodeId, deps)
  }

  /** Parse Usages response */
  private def parseUsagesResult(map: Map[String, dev.bosatsu.Json]): Either[String, ResponseData] = {
    import dev.bosatsu.Json._
    for {
      nodeId <- map.get("nodeId").collect { case JString(s) => NodeId(s) }.toRight("Missing 'nodeId'")
      usages <- map.get("usages") match {
        case Some(JArray(nodes)) => nodes.toList.traverse(parseNodeSummary)
        case _ => Left("Missing 'usages' array")
      }
    } yield ResponseData.UsagesResult(nodeId, usages)
  }

  /** Parse Focus response */
  private def parseFocusResult(map: Map[String, dev.bosatsu.Json]): Either[String, ResponseData] = {
    import dev.bosatsu.Json._
    for {
      message <- map.get("message").collect { case JString(s) => s }.toRight("Missing 'message'")
      node <- map.get("node").toRight("Missing 'node'").flatMap(parseNodeSummary)
    } yield ResponseData.FocusResult(message, node)
  }

  /** Parse Path response */
  private def parsePathResult(map: Map[String, dev.bosatsu.Json]): Either[String, ResponseData] = {
    import dev.bosatsu.Json._
    map.get("path") match {
      case Some(JArray(nodes)) =>
        nodes.toList.traverse(parseNodeSummary).map(ResponseData.PathResult(_))
      case _ => Left("Missing 'path' array")
    }
  }

  /** Parse Value response */
  private def parseValueResult(map: Map[String, dev.bosatsu.Json]): Either[String, ResponseData] = {
    import dev.bosatsu.Json._
    for {
      nodeId <- map.get("nodeId").collect { case JString(s) => NodeId(s) }.toRight("Missing 'nodeId'")
      value <- map.get("value").collect { case JString(s) => s }.toRight("Missing 'value'")
      valueJson <- map.get("valueJson").collect { case JString(s) => s }.toRight("Missing 'valueJson'")
    } yield ResponseData.ValueResult(nodeId, value, valueJson)
  }

  /** Parse Source response */
  private def parseSourceResult(map: Map[String, dev.bosatsu.Json]): Either[String, ResponseData] = {
    import dev.bosatsu.Json._
    for {
      nodeId <- map.get("nodeId").collect { case JString(s) => NodeId(s) }.toRight("Missing 'nodeId'")
    } yield ResponseData.SourceResult(nodeId, parseSourceLocation(map.get("location")))
  }

  /** Parse Snippet response */
  private def parseSnippetResult(map: Map[String, dev.bosatsu.Json]): Either[String, ResponseData] = {
    import dev.bosatsu.Json._
    for {
      nodeId <- map.get("nodeId").collect { case JString(s) => NodeId(s) }.toRight("Missing 'nodeId'")
      location <- parseSourceLocation(map.get("location")).toRight("Missing 'location'")
      lines <- map.get("lines") match {
        case Some(JArray(arr)) => arr.toList.traverse(parseSnippetLine)
        case _ => Left("Missing 'lines' array")
      }
      scope <- map.get("scope") match {
        case Some(JArray(arr)) => arr.toList.traverse(parseScopeEntry)
        case _ => Left("Missing 'scope' array")
      }
      result <- map.get("result").toRight("Missing 'result'").flatMap(parseScopeEntry)
    } yield ResponseData.SnippetResult(nodeId, location, lines, scope, result)
  }

  /** Parse SnippetLine from JSON */
  private def parseSnippetLine(json: dev.bosatsu.Json): Either[String, ResponseData.SnippetLine] = {
    import dev.bosatsu.Json._
    json match {
      case obj: JObject =>
        val map = obj.toMap
        for {
          lineNum <- map.get("lineNum").collect { case JNumberStr(s) => s.toInt }.toRight("Missing 'lineNum'")
          code <- map.get("code").collect { case JString(s) => s }.toRight("Missing 'code'")
          highlight <- Right(map.get("highlight").contains(JBool.True))
        } yield ResponseData.SnippetLine(lineNum, code, highlight)
      case _ => Left("SnippetLine is not a JSON object")
    }
  }

  /** Parse ScopeEntry from JSON */
  private def parseScopeEntry(json: dev.bosatsu.Json): Either[String, ResponseData.ScopeEntry] = {
    import dev.bosatsu.Json._
    json match {
      case obj: JObject =>
        val map = obj.toMap
        for {
          name <- map.get("name").collect { case JString(s) => s }.toRight("Missing 'name'")
          value <- map.get("value").collect { case JString(s) => s }.toRight("Missing 'value'")
        } yield ResponseData.ScopeEntry(
          name = name,
          value = value,
          nodeId = map.get("nodeId").collect { case JString(s) => NodeId(s) }
        )
      case _ => Left("ScopeEntry is not a JSON object")
    }
  }

  /** Parse Eval response */
  private def parseEvalResult(map: Map[String, dev.bosatsu.Json]): Either[String, ResponseData] = {
    import dev.bosatsu.Json._
    for {
      expression <- map.get("expression").collect { case JString(s) => s }.toRight("Missing 'expression'")
      result <- map.get("result").collect { case JString(s) => s }.toRight("Missing 'result'")
      scope <- map.get("scope") match {
        case Some(obj: JObject) =>
          Right(obj.toMap.collect { case (k, JString(v)) => k -> v })
        case _ => Right(Map.empty[String, String])
      }
    } yield ResponseData.EvalResult(expression, result, scope)
  }

  /** Parse Status response */
  private def parseStatusResult(map: Map[String, dev.bosatsu.Json]): Either[String, ResponseData] = {
    import dev.bosatsu.Json._
    for {
      running <- map.get("running").collect { case JBool(b) => b }.toRight("Missing 'running'")
      traceFile <- map.get("traceFile").collect { case JString(s) => s }.toRight("Missing 'traceFile'")
      nodeCount <- map.get("nodeCount").collect { case JNumberStr(s) => s.toInt }.toRight("Missing 'nodeCount'")
      resultNodeId <- map.get("resultNodeId").collect { case JString(s) => NodeId(s) }.toRight("Missing 'resultNodeId'")
      resultValue <- map.get("resultValue").collect { case JString(s) => s }.toRight("Missing 'resultValue'")
    } yield ResponseData.StatusResult(
      running = running,
      traceFile = traceFile,
      nodeCount = nodeCount,
      focusNodeId = map.get("focusNodeId").collect { case JString(s) => NodeId(s) },
      resultNodeId = resultNodeId,
      resultValue = resultValue
    )
  }

  /** Check if daemon is running */
  def ping(socketPath: Path): IO[Boolean] =
    sendCommand(socketPath, DaemonCommand.Status).map(_.isRight)

  /** Request daemon shutdown */
  def shutdown(socketPath: Path): IO[Either[String, Unit]] =
    sendCommand(socketPath, DaemonCommand.Shutdown).map(_.map(_ => ()))
}
