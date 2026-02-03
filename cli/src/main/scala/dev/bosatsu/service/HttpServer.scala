package dev.bosatsu.service

import cats.effect._
import cats.syntax.all._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.http4s.circe._
import org.http4s.circe.CirceEntityCodec._
import io.circe._
import io.circe.syntax._
import io.circe.parser
import com.comcast.ip4s._
import fs2.io.file.Path

/**
 * HTTP server for exposing Bosatsu service handlers as REST endpoints.
 *
 * Endpoints:
 *   GET  /                        - Server info
 *   GET  /handlers                - List available handlers
 *   POST /handlers/:name          - Execute a handler
 *   GET  /handlers/:name/analysis - Get handler analysis
 */
object HttpServer {
  import ServiceJson.given

  // CanEqual instances for http4s types (needed for Scala 3 strict equality)
  given CanEqual[Method, Method] = CanEqual.derived
  given CanEqual[Uri.Path, Uri.Path] = CanEqual.derived

  /**
   * Start the HTTP server with the given handlers.
   */
  def serve(
    handlers: List[CompiledHandler],
    port: Int,
    staticDir: Option[Path]
  ): IO[Unit] = {
    val handlerMap = handlers.map(h => h.name -> h).toMap

    val apiRoutes = HttpRoutes.of[IO] {
      // Server info
      case GET -> Root =>
        Ok(Json.obj(
          "name" -> "Bosatsu Service".asJson,
          "version" -> "0.1.0".asJson,
          "handlers" -> handlers.map(_.name).asJson
        ))

      // List handlers
      case GET -> Root / "handlers" =>
        val handlerInfos = handlers.map { h =>
          Json.obj(
            "name" -> h.name.asJson,
            "operations" -> h.analysis.totalQueries.asJson,
            "canBatch" -> h.analysis.canBatch.asJson
          )
        }
        Ok(handlerInfos.asJson)

      // Handler analysis
      case GET -> Root / "handlers" / name / "analysis" =>
        handlerMap.get(name) match {
          case Some(handler) =>
            Ok(handler.analysis.asJson)
          case None =>
            NotFound(Json.obj("error" -> s"Handler '$name' not found".asJson))
        }

      // Execute handler
      case req @ POST -> Root / "handlers" / name =>
        handlerMap.get(name) match {
          case Some(handler) =>
            req.as[Json].flatMap { body =>
              // For now, just return a mock response
              // Real implementation would execute the JS code
              val response: ServiceResponse = ServiceResponse.Success(
                handler = name,
                result = body.noSpaces,
                provenance = None
              )
              Ok(response.asJson)
            }
          case None =>
            NotFound(Json.obj("error" -> s"Handler '$name' not found".asJson))
        }
    }

    val app = Router(
      "/api" -> apiRoutes
    ).orNotFound

    val serverPort = Port.fromInt(port).getOrElse(port"3000")

    EmberServerBuilder
      .default[IO]
      .withHost(host"0.0.0.0")
      .withPort(serverPort)
      .withHttpApp(app)
      .build
      .useForever
  }
}
