package dev.bosatsu.daemon

import dev.bosatsu.Json
import dev.bosatsu.Json._
import cats.syntax.all._
import cats.parse.{Parser => P}

/**
 * JSON serialization for daemon protocol types.
 *
 * Uses the existing Json AST and Reader/Writer type classes for consistency
 * with the rest of the codebase.
 */
object DaemonJson {

  // ============================================================================
  // Writers (Scala -> JSON)
  // ============================================================================

  implicit val nodeIdWriter: Writer[NodeId] =
    Writer.from(id => JString(id.value))

  implicit val sourceLocationWriter: Writer[SourceLocation] =
    Writer.from { loc =>
      JObject(List(
        "file" -> JString(loc.file),
        "line" -> JNumberStr(loc.line.toString),
        "column" -> JNumberStr(loc.column.toString)
      ))
    }

  implicit val sourceTypeWriter: Writer[SourceType] =
    Writer.from {
      case SourceType.Pure(expr) =>
        JObject(List(
          "type" -> JString("pure"),
          "expression" -> expr.fold[Json](JNull)(JString(_))
        ))
      case SourceType.Operation(interface, method) =>
        JObject(List(
          "type" -> JString("operation"),
          "interface" -> JString(interface),
          "method" -> JString(method)
        ))
      case SourceType.Binding(name) =>
        JObject(List(
          "type" -> JString("binding"),
          "name" -> JString(name)
        ))
      case SourceType.Parameter =>
        JObject(List("type" -> JString("parameter")))
      case SourceType.Literal =>
        JObject(List("type" -> JString("literal")))
    }

  implicit val nodeSummaryWriter: Writer[ResponseData.NodeSummary] =
    Writer.from { ns =>
      JObject(List(
        "id" -> Writer.write(ns.id),
        "value" -> ns.value.fold[Json](JNull)(JString(_)),
        "typeInfo" -> JString(ns.typeInfo),
        "source" -> JString(ns.source),
        "bindingName" -> ns.bindingName.fold[Json](JNull)(JString(_)),
        "location" -> ns.location.fold[Json](JNull)(sourceLocationWriter.write)
      ))
    }

  implicit val snippetLineWriter: Writer[ResponseData.SnippetLine] =
    Writer.from { line =>
      JObject(List(
        "lineNum" -> JNumberStr(line.lineNum.toString),
        "code" -> JString(line.code),
        "highlight" -> JBool(line.highlight)
      ))
    }

  implicit val scopeEntryWriter: Writer[ResponseData.ScopeEntry] =
    Writer.from { entry =>
      JObject(List(
        "name" -> JString(entry.name),
        "value" -> JString(entry.value),
        "nodeId" -> entry.nodeId.fold[Json](JNull)(id => JString(id.value))
      ))
    }

  implicit val responseDataWriter: Writer[ResponseData] =
    Writer.from {
      case ResponseData.NodeList(nodes) =>
        JObject(List(
          "type" -> JString("nodeList"),
          "nodes" -> JArray(nodes.map(nodeSummaryWriter.write).toVector)
        ))
      case ResponseData.ExplainResult(nodeId, tree, compact, node) =>
        JObject(List(
          "type" -> JString("explain"),
          "nodeId" -> Writer.write(nodeId),
          "tree" -> JString(tree),
          "compact" -> JString(compact),
          "node" -> nodeSummaryWriter.write(node)
        ))
      case ResponseData.FindResult(nodes) =>
        JObject(List(
          "type" -> JString("find"),
          "nodes" -> JArray(nodes.map(nodeSummaryWriter.write).toVector)
        ))
      case ResponseData.DepsResult(nodeId, deps) =>
        JObject(List(
          "type" -> JString("deps"),
          "nodeId" -> Writer.write(nodeId),
          "dependencies" -> JArray(deps.map(nodeSummaryWriter.write).toVector)
        ))
      case ResponseData.UsagesResult(nodeId, usages) =>
        JObject(List(
          "type" -> JString("usages"),
          "nodeId" -> Writer.write(nodeId),
          "usages" -> JArray(usages.map(nodeSummaryWriter.write).toVector)
        ))
      case ResponseData.FocusResult(message, node) =>
        JObject(List(
          "type" -> JString("focus"),
          "message" -> JString(message),
          "node" -> nodeSummaryWriter.write(node)
        ))
      case ResponseData.UnfocusResult(message) =>
        JObject(List(
          "type" -> JString("unfocus"),
          "message" -> JString(message)
        ))
      case ResponseData.PathResult(path) =>
        JObject(List(
          "type" -> JString("path"),
          "path" -> JArray(path.map(nodeSummaryWriter.write).toVector)
        ))
      case ResponseData.ValueResult(nodeId, value, valueJson) =>
        JObject(List(
          "type" -> JString("value"),
          "nodeId" -> Writer.write(nodeId),
          "value" -> JString(value),
          "valueJson" -> JString(valueJson)
        ))
      case ResponseData.SourceResult(nodeId, location) =>
        JObject(List(
          "type" -> JString("source"),
          "nodeId" -> Writer.write(nodeId),
          "location" -> location.fold[Json](JNull)(sourceLocationWriter.write)
        ))
      case ResponseData.SnippetResult(nodeId, location, lines, scope, result) =>
        JObject(List(
          "type" -> JString("snippet"),
          "nodeId" -> Writer.write(nodeId),
          "location" -> sourceLocationWriter.write(location),
          "lines" -> JArray(lines.map(snippetLineWriter.write).toVector),
          "scope" -> JArray(scope.map(scopeEntryWriter.write).toVector),
          "result" -> scopeEntryWriter.write(result)
        ))
      case ResponseData.EvalResult(expression, result, scope) =>
        JObject(List(
          "type" -> JString("eval"),
          "expression" -> JString(expression),
          "result" -> JString(result),
          "scope" -> JObject(scope.toList.map { case (k, v) => k -> JString(v) })
        ))
      case ResponseData.StatusResult(running, traceFile, nodeCount, focusNodeId, resultNodeId, resultValue) =>
        JObject(List(
          "type" -> JString("status"),
          "running" -> JBool(running),
          "traceFile" -> JString(traceFile),
          "nodeCount" -> JNumberStr(nodeCount.toString),
          "focusNodeId" -> focusNodeId.fold[Json](JNull)(id => JString(id.value)),
          "resultNodeId" -> Writer.write(resultNodeId),
          "resultValue" -> JString(resultValue)
        ))
      case ResponseData.ShutdownResult(message) =>
        JObject(List(
          "type" -> JString("shutdown"),
          "message" -> JString(message)
        ))
    }

  implicit val daemonResponseWriter: Writer[DaemonResponse] =
    Writer.from {
      case DaemonResponse.Success(data) =>
        JObject(List(
          "success" -> JBool.True,
          "data" -> responseDataWriter.write(data)
        ))
      case DaemonResponse.Error(message) =>
        JObject(List(
          "success" -> JBool.False,
          "error" -> JString(message)
        ))
    }

  // ============================================================================
  // Readers (JSON -> Scala)
  // ============================================================================

  implicit val nodeIdReader: Reader[NodeId] =
    Reader.stringReader.mapEither("NodeId")(s => Right(NodeId(s)))

  implicit val sourceLocationReader: Reader[SourceLocation] =
    new Reader.Obj[SourceLocation] {
      def describe = "SourceLocation"
      def readObj(from: Reader.FromObj) =
        for {
          file <- from.field[String]("file")
          line <- from.field[String]("line").flatMap(s =>
            s.toIntOption.toRight(("expected int for line", JString(s), from.path)))
          column <- from.field[String]("column").flatMap(s =>
            s.toIntOption.toRight(("expected int for column", JString(s), from.path)))
        } yield SourceLocation(file, line, column)
    }

  implicit val daemonCommandReader: Reader[DaemonCommand] =
    new Reader.Obj[DaemonCommand] {
      def describe = "DaemonCommand"
      def readObj(from: Reader.FromObj) =
        from.field[String]("command").flatMap {
          case "list" =>
            from.optional[String]("showValues").map { sv =>
              DaemonCommand.List(sv.contains("true"))
            }
          case "explain" =>
            from.optional[String]("nodeId").map { nid =>
              DaemonCommand.Explain(nid.map(NodeId(_)))
            }
          case "find" =>
            from.field[String]("value").map(DaemonCommand.Find(_))
          case "deps" =>
            from.field[String]("nodeId").map(id => DaemonCommand.Deps(NodeId(id)))
          case "usages" =>
            from.field[String]("nodeId").map(id => DaemonCommand.Usages(NodeId(id)))
          case "focus" =>
            from.field[String]("nodeId").map(id => DaemonCommand.Focus(NodeId(id)))
          case "unfocus" =>
            Right(DaemonCommand.Unfocus)
          case "path" =>
            from.field[String]("nodeId").map(id => DaemonCommand.Path(NodeId(id)))
          case "value" =>
            from.field[String]("nodeId").map(id => DaemonCommand.Value(NodeId(id)))
          case "source" =>
            from.field[String]("nodeId").map(id => DaemonCommand.Source(NodeId(id)))
          case "snippet" =>
            for {
              nodeId <- from.field[String]("nodeId")
              context <- from.optional[String]("contextLines").map(_.flatMap(_.toIntOption).getOrElse(3))
            } yield DaemonCommand.Snippet(NodeId(nodeId), context)
          case "eval" =>
            for {
              nodeId <- from.field[String]("nodeId")
              expression <- from.field[String]("expression")
            } yield DaemonCommand.Eval(NodeId(nodeId), expression)
          case "status" =>
            Right(DaemonCommand.Status)
          case "shutdown" =>
            Right(DaemonCommand.Shutdown)
          case other =>
            Left((s"unknown command: $other", from.j, from.path))
        }
    }

  // ============================================================================
  // Convenience methods
  // ============================================================================

  def parseCommand(jsonStr: String): Either[String, DaemonCommand] =
    Json.parserFile.parseAll(jsonStr) match {
      case Left(err) => Left(s"JSON parse error: $err")
      case Right(json) =>
        daemonCommandReader.read(Json.Path.Root, json).left.map {
          case (msg, json, path) => s"$msg at ${path.show}"
        }
    }

  def renderResponse(response: DaemonResponse): String =
    daemonResponseWriter.write(response).render

  def renderCommand(command: DaemonCommand): String = {
    val json = command match {
      case DaemonCommand.List(showValues) =>
        JObject(List(
          "command" -> JString("list"),
          "showValues" -> JString(showValues.toString)
        ))
      case DaemonCommand.Explain(nodeId) =>
        JObject(List(
          "command" -> JString("explain")
        ) ++ nodeId.map(id => "nodeId" -> JString(id.value)).toList)
      case DaemonCommand.Find(value) =>
        JObject(List(
          "command" -> JString("find"),
          "value" -> JString(value)
        ))
      case DaemonCommand.Deps(nodeId) =>
        JObject(List(
          "command" -> JString("deps"),
          "nodeId" -> JString(nodeId.value)
        ))
      case DaemonCommand.Usages(nodeId) =>
        JObject(List(
          "command" -> JString("usages"),
          "nodeId" -> JString(nodeId.value)
        ))
      case DaemonCommand.Focus(nodeId) =>
        JObject(List(
          "command" -> JString("focus"),
          "nodeId" -> JString(nodeId.value)
        ))
      case DaemonCommand.Unfocus =>
        JObject(List("command" -> JString("unfocus")))
      case DaemonCommand.Path(nodeId) =>
        JObject(List(
          "command" -> JString("path"),
          "nodeId" -> JString(nodeId.value)
        ))
      case DaemonCommand.Value(nodeId) =>
        JObject(List(
          "command" -> JString("value"),
          "nodeId" -> JString(nodeId.value)
        ))
      case DaemonCommand.Source(nodeId) =>
        JObject(List(
          "command" -> JString("source"),
          "nodeId" -> JString(nodeId.value)
        ))
      case DaemonCommand.Snippet(nodeId, context) =>
        JObject(List(
          "command" -> JString("snippet"),
          "nodeId" -> JString(nodeId.value),
          "contextLines" -> JString(context.toString)
        ))
      case DaemonCommand.Eval(nodeId, expression) =>
        JObject(List(
          "command" -> JString("eval"),
          "nodeId" -> JString(nodeId.value),
          "expression" -> JString(expression)
        ))
      case DaemonCommand.Status =>
        JObject(List("command" -> JString("status")))
      case DaemonCommand.Shutdown =>
        JObject(List("command" -> JString("shutdown")))
    }
    json.render
  }
}
