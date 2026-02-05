package dev.bosatsu.service

import cats.effect._
import cats.syntax.all._
import io.circe._
import io.circe.syntax._
import io.circe.parser
import fs2.io.stdinUtf8
import fs2.io.stdoutLines
import java.nio.charset.StandardCharsets

/**
 * MCP (Model Context Protocol) server for exposing Bosatsu handlers as AI tools.
 *
 * The MCP protocol uses JSON-RPC 2.0 over stdio:
 * - Server reads requests from stdin
 * - Server writes responses to stdout
 *
 * Methods:
 *   initialize       - Initialize the server
 *   tools/list       - List available tools (handlers)
 *   tools/call       - Call a tool (execute handler)
 *   shutdown         - Shutdown the server
 */
object McpServer {
  import ServiceJson.given

  // CanEqual for Json comparison
  given CanEqual[Json, Json] = CanEqual.derived

  /**
   * Build a JSON-RPC response.
   * Exposed for testing.
   */
  def jsonRpcResponse(id: Option[Json], result: Json): Json = {
    Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id" -> id.getOrElse(Json.Null),
      "result" -> result
    )
  }

  /**
   * Build a JSON-RPC error response.
   * Exposed for testing.
   */
  def jsonRpcError(id: Option[Json], code: Int, message: String): Json = {
    Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id" -> id.getOrElse(Json.Null),
      "error" -> Json.obj(
        "code" -> code.asJson,
        "message" -> message.asJson
      )
    )
  }

  /**
   * Process a single JSON-RPC request.
   * Exposed for testing.
   */
  def processRequest(
    serverName: String,
    handlers: List[CompiledHandler],
    request: Json
  ): IO[Json] = {
    val handlerMap = handlers.map(h => h.name -> h).toMap
    val method = request.hcursor.downField("method").as[String].getOrElse("")
    val id = request.hcursor.downField("id").focus
    val params = request.hcursor.downField("params").focus.getOrElse(Json.obj())

    method match {
      case "initialize" =>
        IO.pure(jsonRpcResponse(id, Json.obj(
          "protocolVersion" -> "2024-11-05".asJson,
          "capabilities" -> Json.obj(
            "tools" -> Json.obj()
          ),
          "serverInfo" -> Json.obj(
            "name" -> serverName.asJson,
            "version" -> "0.1.0".asJson
          )
        )))

      case "tools/list" =>
        val tools = handlers.map { h =>
          Json.obj(
            "name" -> h.name.asJson,
            "description" -> s"Bosatsu handler: ${h.name}".asJson,
            "inputSchema" -> Json.obj(
              "type" -> "object".asJson,
              "properties" -> Json.obj(),
              "required" -> Json.arr()
            )
          )
        }
        IO.pure(jsonRpcResponse(id, Json.obj(
          "tools" -> tools.asJson
        )))

      case "tools/call" =>
        val toolName = params.hcursor.downField("name").as[String].getOrElse("")
        val args = params.hcursor.downField("arguments").focus.getOrElse(Json.obj())

        handlerMap.get(toolName) match {
          case Some(handler) =>
            // For now, return a mock response
            // Real implementation would execute the JS code
            IO.pure(jsonRpcResponse(id, Json.obj(
              "content" -> Json.arr(
                Json.obj(
                  "type" -> "text".asJson,
                  "text" -> s"Executed ${handler.name} with args: ${args.noSpaces}".asJson
                )
              )
            )))
          case None =>
            IO.pure(jsonRpcError(id, -32601, s"Tool '$toolName' not found"))
        }

      case "notifications/initialized" =>
        // Notification, no response needed - return a sentinel value
        IO.pure(Json.Null)

      case "shutdown" =>
        IO.pure(jsonRpcResponse(id, Json.obj()))

      case _ =>
        IO.pure(jsonRpcError(id, -32601, s"Method not found: $method"))
    }
  }

  /**
   * Start the MCP server with the given handlers.
   */
  def serve(
    serverName: String,
    handlers: List[CompiledHandler]
  ): IO[Unit] = {
    // Read lines from stdin, process as JSON-RPC, write responses to stdout
    stdinUtf8[IO](4096)
      .through(fs2.text.lines)
      .filter(_.nonEmpty)
      .evalMap { line =>
        parser.parse(line) match {
          case Right(json) => processRequest(serverName, handlers, json)
          case Left(err) => IO.pure(jsonRpcError(None, -32700, s"Parse error: ${err.message}"))
        }
      }
      .filter(j => !j.isNull) // Filter out notifications (using isNull instead of ==)
      .map(_.noSpaces)
      .through(stdoutLines(StandardCharsets.UTF_8))
      .compile
      .drain
  }
}
