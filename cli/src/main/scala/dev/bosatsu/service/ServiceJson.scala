package dev.bosatsu.service

import io.circe._
import io.circe.syntax._

/**
 * JSON serialization for service protocol types.
 */
object ServiceJson {

  // Encoders for OperationKind
  given Encoder[OperationKind] = Encoder.instance {
    case OperationKind.Read => Json.fromString("read")
    case OperationKind.Write => Json.fromString("write")
    case OperationKind.Unknown => Json.fromString("unknown")
  }

  given Decoder[OperationKind] = Decoder.decodeString.emap {
    case "read" => Right(OperationKind.Read)
    case "write" => Right(OperationKind.Write)
    case "unknown" => Right(OperationKind.Unknown)
    case other => Left(s"Unknown operation kind: $other")
  }

  // Encoders for ServiceOperation
  given Encoder[ServiceOperation] = Encoder.instance { op =>
    Json.obj(
      "interface" -> op.interface.asJson,
      "method" -> op.method.asJson,
      "kind" -> op.kind.asJson,
      "batchable" -> op.batchable.asJson,
      "batchMethod" -> op.batchMethod.asJson
    )
  }

  given Decoder[ServiceOperation] = Decoder.instance { c =>
    for {
      interface <- c.downField("interface").as[String]
      method <- c.downField("method").as[String]
      kind <- c.downField("kind").as[OperationKind]
      batchable <- c.downField("batchable").as[Boolean]
      batchMethod <- c.downField("batchMethod").as[Option[String]]
    } yield ServiceOperation(interface, method, kind, batchable, batchMethod)
  }

  // Encoders for BatchGroup
  given Encoder[BatchGroup] = Encoder.instance { bg =>
    Json.obj(
      "interface" -> bg.interface.asJson,
      "method" -> bg.method.asJson,
      "batchMethod" -> bg.batchMethod.asJson,
      "operations" -> bg.operations.asJson,
      "queriesSaved" -> bg.queriesSaved.asJson
    )
  }

  // Encoders for ServiceAnalysis
  given Encoder[ServiceAnalysis] = Encoder.instance { sa =>
    Json.obj(
      "handlerName" -> sa.handlerName.asJson,
      "sourceFile" -> sa.sourceFile.asJson,
      "operations" -> sa.operations.asJson,
      "batchGroups" -> sa.batchGroups.asJson,
      "canBatch" -> sa.canBatch.asJson,
      "totalQueries" -> sa.totalQueries.asJson,
      "batchedQueries" -> sa.batchedQueries.asJson,
      "queriesSaved" -> sa.queriesSaved.asJson,
      "batchingEfficiency" -> sa.batchingEfficiency.asJson
    )
  }

  // Encoders for BuildTarget
  given Encoder[BuildTarget] = Encoder.instance {
    case BuildTarget.Standalone => Json.fromString("standalone")
    case BuildTarget.Vercel => Json.fromString("vercel")
    case BuildTarget.AwsLambda => Json.fromString("aws-lambda")
  }

  // Encoders for ServiceResponse
  given Encoder[ServiceResponse] = Encoder.instance {
    case ServiceResponse.Success(handler, result, provenance) =>
      Json.obj(
        "success" -> true.asJson,
        "handler" -> handler.asJson,
        "result" -> parser.parse(result).getOrElse(Json.fromString(result)),
        "provenance" -> provenance.flatMap(p => parser.parse(p).toOption).asJson
      )
    case ServiceResponse.Error(handler, error, code) =>
      Json.obj(
        "success" -> false.asJson,
        "handler" -> handler.asJson,
        "error" -> error.asJson,
        "code" -> code.asJson
      )
  }

  // Encoders for HandlerResult
  given Encoder[HandlerResult] = Encoder.instance { hr =>
    Json.obj(
      "value" -> parser.parse(hr.value).getOrElse(Json.fromString(hr.value)),
      "provenance" -> hr.provenance.flatMap(p => parser.parse(p).toOption).asJson
    )
  }

  // Encoders for BuildResult
  given Encoder[BuildResult] = Encoder.instance { br =>
    Json.obj(
      "handlers" -> br.handlers.asJson,
      "target" -> br.target.asJson,
      "jsCodeLength" -> br.jsCode.length.asJson
    )
  }

  /**
   * Render a list of analyses to JSON string.
   */
  def renderAnalyses(analyses: List[ServiceAnalysis]): String = {
    analyses.asJson.spaces2
  }

  /**
   * Render a single analysis to JSON string.
   */
  def renderAnalysis(analysis: ServiceAnalysis): String = {
    analysis.asJson.spaces2
  }

  /**
   * Render a service response to JSON string.
   */
  def renderResponse(response: ServiceResponse): String = {
    response.asJson.noSpaces
  }

  /**
   * Render a build result to JSON string.
   */
  def renderBuildResult(result: BuildResult): String = {
    result.asJson.spaces2
  }
}
