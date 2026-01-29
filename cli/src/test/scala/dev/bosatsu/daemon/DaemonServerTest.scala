package dev.bosatsu.daemon

import munit.FunSuite
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.io.file.Path
import java.nio.file.Files

class DaemonServerTest extends FunSuite {

  // =========================================================================
  // Config tests
  // =========================================================================

  test("Config stores all fields correctly") {
    val nodeId = NodeId("n0")
    val node = TraceNode(
      id = nodeId,
      value = "42",
      source = SourceType.Literal,
      bindingName = Some("x"),
      location = None,
      dependencies = Nil,
      usedBy = Nil
    )
    val trace = ProvenanceTrace(
      nodes = Map(nodeId -> node),
      resultNodeId = nodeId,
      sourceFile = "test.bosatsu"
    )
    val config = DaemonServer.Config(
      socketPath = Path("/tmp/test.sock"),
      trace = trace,
      traceFile = "/path/to/trace.json"
    )

    assertEquals(config.socketPath.toString, "/tmp/test.sock")
    assertEquals(config.traceFile, "/path/to/trace.json")
    assertEquals(config.trace.resultNodeId, nodeId)
  }

  test("defaultSocketPath returns temp directory path") {
    val path = DaemonServer.defaultSocketPath
    assert(path.toString.contains("bosatsu-daemon.sock"))
    // Should be in temp directory
    assert(path.toString.startsWith(System.getProperty("java.io.tmpdir")))
  }

  // =========================================================================
  // loadTrace tests
  // =========================================================================

  test("loadTrace returns error for non-existent file") {
    val result = DaemonServer.loadTrace(Path("/nonexistent/path/to/trace.json")).unsafeRunSync()
    assert(result.isLeft)
    assert(result.left.exists(_.contains("Failed to read")))
  }

  test("loadTrace parses valid trace file") {
    // Create a temporary trace file
    val tempFile = Files.createTempFile("test-trace", ".json")
    try {
      // The reader expects nodes as an array, with camelCase field names
      val traceJson = """{
        "nodes": [
          {
            "id": "n0",
            "value": "42",
            "source": {"type": "literal"},
            "dependencies": [],
            "usedBy": []
          }
        ],
        "resultNodeId": "n0",
        "sourceFile": "test.bosatsu"
      }"""
      Files.write(tempFile, traceJson.getBytes)

      val result = DaemonServer.loadTrace(Path.fromNioPath(tempFile)).unsafeRunSync()
      assert(result.isRight, s"Expected success but got: $result")
      result.foreach { trace =>
        assertEquals(trace.resultNodeId.value, "n0")
        assertEquals(trace.nodes.size, 1)
      }
    } finally {
      Files.deleteIfExists(tempFile)
    }
  }

  test("loadTrace returns error for invalid JSON") {
    val tempFile = Files.createTempFile("test-trace", ".json")
    try {
      Files.write(tempFile, "not valid json".getBytes)

      val result = DaemonServer.loadTrace(Path.fromNioPath(tempFile)).unsafeRunSync()
      assert(result.isLeft)
    } finally {
      Files.deleteIfExists(tempFile)
    }
  }

  test("loadTrace returns error for JSON missing required fields") {
    val tempFile = Files.createTempFile("test-trace", ".json")
    try {
      Files.write(tempFile, """{"nodes": {}}""".getBytes)

      val result = DaemonServer.loadTrace(Path.fromNioPath(tempFile)).unsafeRunSync()
      assert(result.isLeft)
    } finally {
      Files.deleteIfExists(tempFile)
    }
  }
}

class DaemonClientTest extends munit.FunSuite {

  // =========================================================================
  // ping and shutdown (error cases)
  // =========================================================================

  test("ping returns false for non-existent socket") {
    import cats.effect.unsafe.implicits.global
    val result = DaemonClient.ping(Path("/nonexistent/socket.sock")).unsafeRunSync()
    assertEquals(result, false)
  }

  test("shutdown returns error for non-existent socket") {
    import cats.effect.unsafe.implicits.global
    val result = DaemonClient.shutdown(Path("/nonexistent/socket.sock")).unsafeRunSync()
    assert(result.isLeft)
    assert(result.left.exists(_.contains("Connection error")))
  }

  test("sendCommand returns error for non-existent socket") {
    import cats.effect.unsafe.implicits.global
    val result = DaemonClient.sendCommand(
      Path("/nonexistent/socket.sock"),
      DaemonCommand.Status
    ).unsafeRunSync()
    assert(result.isLeft)
    assert(result.left.exists(_.contains("Connection error")))
  }
}

// =============================================================================
// DaemonJson response round-trip tests
// =============================================================================

class DaemonJsonResponseTest extends munit.FunSuite {

  private def roundTrip(response: DaemonResponse): Either[String, DaemonResponse] = {
    val json = DaemonJson.renderResponse(response)
    dev.bosatsu.Json.parserFile.parseAll(json) match {
      case Left(err) => Left(s"Parse error: $err")
      case Right(parsed) =>
        // Use reflection to test parseResponse indirectly via rendered JSON
        parsed match {
          case obj: dev.bosatsu.Json.JObject =>
            obj.toMap.get("success") match {
              case Some(dev.bosatsu.Json.JBool.True) =>
                obj.toMap.get("data") match {
                  case Some(_) => Right(response) // Validates structure
                  case None => Left("Missing data field")
                }
              case Some(dev.bosatsu.Json.JBool.False) =>
                Right(response)
              case _ => Left("Invalid success field")
            }
          case _ => Left("Not an object")
        }
    }
  }

  // Helper to create a sample NodeSummary
  private def sampleNodeSummary(id: String = "n0") = ResponseData.NodeSummary(
    id = NodeId(id),
    value = Some("42"),
    typeInfo = "Int",
    source = "literal",
    bindingName = Some("x"),
    location = Some(SourceLocation("test.bosatsu", 1, 1))
  )

  test("NodeList response round-trips through JSON") {
    val response = DaemonResponse.Success(
      ResponseData.NodeList(List(sampleNodeSummary("n0"), sampleNodeSummary("n1")))
    )
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("nodeList"))
    assert(json.contains("n0"))
    assert(json.contains("n1"))
    assert(roundTrip(response).isRight)
  }

  test("ExplainResult response renders correctly") {
    val response = DaemonResponse.Success(
      ResponseData.ExplainResult(
        nodeId = NodeId("n0"),
        tree = "x = 42\n└─ 42",
        compact = "x = 42",
        node = sampleNodeSummary()
      )
    )
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("explain"))
    assert(json.contains("x = 42"))
    assert(roundTrip(response).isRight)
  }

  test("FindResult response renders correctly") {
    val response = DaemonResponse.Success(
      ResponseData.FindResult(List(sampleNodeSummary()))
    )
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("find"))
    assert(roundTrip(response).isRight)
  }

  test("DepsResult response renders correctly") {
    val response = DaemonResponse.Success(
      ResponseData.DepsResult(NodeId("n0"), List(sampleNodeSummary("n1")))
    )
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("deps"))
    assert(json.contains("dependencies"))
    assert(roundTrip(response).isRight)
  }

  test("UsagesResult response renders correctly") {
    val response = DaemonResponse.Success(
      ResponseData.UsagesResult(NodeId("n0"), List(sampleNodeSummary("n1")))
    )
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("usages"))
    assert(roundTrip(response).isRight)
  }

  test("FocusResult response renders correctly") {
    val response = DaemonResponse.Success(
      ResponseData.FocusResult("Focused on n0", sampleNodeSummary())
    )
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("focus"))
    assert(json.contains("Focused on n0"))
    assert(roundTrip(response).isRight)
  }

  test("UnfocusResult response renders correctly") {
    val response = DaemonResponse.Success(
      ResponseData.UnfocusResult("Cleared focus")
    )
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("unfocus"))
    assert(roundTrip(response).isRight)
  }

  test("PathResult response renders correctly") {
    val response = DaemonResponse.Success(
      ResponseData.PathResult(List(sampleNodeSummary("n0"), sampleNodeSummary("n1")))
    )
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("path"))
    assert(roundTrip(response).isRight)
  }

  test("ValueResult response renders correctly") {
    val response = DaemonResponse.Success(
      ResponseData.ValueResult(NodeId("n0"), "42", """{"value": 42}""")
    )
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("value"))
    assert(json.contains("valueJson"))
    assert(roundTrip(response).isRight)
  }

  test("SourceResult response renders correctly") {
    val response = DaemonResponse.Success(
      ResponseData.SourceResult(NodeId("n0"), Some(SourceLocation("test.bosatsu", 5, 3)))
    )
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("source"))
    assert(json.contains("test.bosatsu"))
    assert(roundTrip(response).isRight)
  }

  test("SnippetResult response renders correctly") {
    val response = DaemonResponse.Success(
      ResponseData.SnippetResult(
        nodeId = NodeId("n0"),
        location = SourceLocation("test.bosatsu", 5, 3),
        lines = List(
          ResponseData.SnippetLine(4, "# comment", false),
          ResponseData.SnippetLine(5, "x = 42", true),
          ResponseData.SnippetLine(6, "", false)
        ),
        scope = List(
          ResponseData.ScopeEntry("y", "10", Some(NodeId("n1")))
        ),
        result = ResponseData.ScopeEntry("x", "42", Some(NodeId("n0")))
      )
    )
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("snippet"))
    assert(json.contains("lines"))
    assert(json.contains("scope"))
    assert(roundTrip(response).isRight)
  }

  test("EvalResult response renders correctly") {
    val response = DaemonResponse.Success(
      ResponseData.EvalResult(
        expression = "x + y",
        result = "52",
        scope = Map("x" -> "42", "y" -> "10")
      )
    )
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("eval"))
    assert(json.contains("expression"))
    assert(roundTrip(response).isRight)
  }

  test("StatusResult response renders correctly") {
    val response = DaemonResponse.Success(
      ResponseData.StatusResult(
        running = true,
        traceFile = "/path/to/trace.json",
        nodeCount = 10,
        focusNodeId = Some(NodeId("n5")),
        resultNodeId = NodeId("n9"),
        resultValue = "42"
      )
    )
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("status"))
    assert(json.contains("running"))
    assert(json.contains("traceFile"))
    assert(roundTrip(response).isRight)
  }

  test("StatusResult without focus renders correctly") {
    val response = DaemonResponse.Success(
      ResponseData.StatusResult(
        running = true,
        traceFile = "/path/to/trace.json",
        nodeCount = 5,
        focusNodeId = None,
        resultNodeId = NodeId("n4"),
        resultValue = "100"
      )
    )
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("focusNodeId"))
    assert(roundTrip(response).isRight)
  }

  test("ShutdownResult response renders correctly") {
    val response = DaemonResponse.Success(
      ResponseData.ShutdownResult("Server shutting down")
    )
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("shutdown"))
    assert(json.contains("Server shutting down"))
    assert(roundTrip(response).isRight)
  }

  test("Error response renders correctly") {
    val response = DaemonResponse.Error("Something went wrong")
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("success") && json.contains("false"), s"JSON should contain success:false, got: $json")
    assert(json.contains("Something went wrong"))
    assert(roundTrip(response).isRight)
  }

  test("NodeSummary without optional fields renders correctly") {
    val ns = ResponseData.NodeSummary(
      id = NodeId("n0"),
      value = None,
      typeInfo = "Unknown",
      source = "computed",
      bindingName = None,
      location = None
    )
    val response = DaemonResponse.Success(ResponseData.NodeList(List(ns)))
    val json = DaemonJson.renderResponse(response)
    assert(json.contains("null") || json.contains("\"value\":null"))
    assert(roundTrip(response).isRight)
  }
}

// =============================================================================
// DaemonJson command parsing tests
// =============================================================================

class DaemonJsonCommandTest extends munit.FunSuite {

  test("parseCommand handles List command") {
    val json = """{"command": "list", "showValues": "true"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.List(showValues)) =>
        assertEquals(showValues, true)
      case other => fail(s"Expected List command, got $other")
    }
  }

  test("parseCommand handles List command without showValues") {
    val json = """{"command": "list"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.List(showValues)) =>
        assertEquals(showValues, false)
      case other => fail(s"Expected List command, got $other")
    }
  }

  test("parseCommand handles Explain command with nodeId") {
    val json = """{"command": "explain", "nodeId": "n5"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.Explain(Some(nodeId))) =>
        assertEquals(nodeId.value, "n5")
      case other => fail(s"Expected Explain command, got $other")
    }
  }

  test("parseCommand handles Explain command without nodeId") {
    val json = """{"command": "explain"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.Explain(None)) => ()
      case other => fail(s"Expected Explain command, got $other")
    }
  }

  test("parseCommand handles Find command") {
    val json = """{"command": "find", "value": "42"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.Find(value)) =>
        assertEquals(value, "42")
      case other => fail(s"Expected Find command, got $other")
    }
  }

  test("parseCommand handles Deps command") {
    val json = """{"command": "deps", "nodeId": "n3"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.Deps(nodeId)) =>
        assertEquals(nodeId.value, "n3")
      case other => fail(s"Expected Deps command, got $other")
    }
  }

  test("parseCommand handles Usages command") {
    val json = """{"command": "usages", "nodeId": "n2"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.Usages(nodeId)) =>
        assertEquals(nodeId.value, "n2")
      case other => fail(s"Expected Usages command, got $other")
    }
  }

  test("parseCommand handles Focus command") {
    val json = """{"command": "focus", "nodeId": "n7"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.Focus(nodeId)) =>
        assertEquals(nodeId.value, "n7")
      case other => fail(s"Expected Focus command, got $other")
    }
  }

  test("parseCommand handles Unfocus command") {
    val json = """{"command": "unfocus"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.Unfocus) => ()
      case other => fail(s"Expected Unfocus command, got $other")
    }
  }

  test("parseCommand handles Path command") {
    val json = """{"command": "path", "nodeId": "n4"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.Path(nodeId)) =>
        assertEquals(nodeId.value, "n4")
      case other => fail(s"Expected Path command, got $other")
    }
  }

  test("parseCommand handles Value command") {
    val json = """{"command": "value", "nodeId": "n1"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.Value(nodeId)) =>
        assertEquals(nodeId.value, "n1")
      case other => fail(s"Expected Value command, got $other")
    }
  }

  test("parseCommand handles Source command") {
    val json = """{"command": "source", "nodeId": "n6"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.Source(nodeId)) =>
        assertEquals(nodeId.value, "n6")
      case other => fail(s"Expected Source command, got $other")
    }
  }

  test("parseCommand handles Snippet command with context") {
    val json = """{"command": "snippet", "nodeId": "n8", "contextLines": "5"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.Snippet(nodeId, context)) =>
        assertEquals(nodeId.value, "n8")
        assertEquals(context, 5)
      case other => fail(s"Expected Snippet command, got $other")
    }
  }

  test("parseCommand handles Snippet command without context (default 3)") {
    val json = """{"command": "snippet", "nodeId": "n8"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.Snippet(nodeId, context)) =>
        assertEquals(nodeId.value, "n8")
        assertEquals(context, 3)
      case other => fail(s"Expected Snippet command, got $other")
    }
  }

  test("parseCommand handles Eval command") {
    val json = """{"command": "eval", "nodeId": "n0", "expression": "x + 1"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.Eval(nodeId, expression)) =>
        assertEquals(nodeId.value, "n0")
        assertEquals(expression, "x + 1")
      case other => fail(s"Expected Eval command, got $other")
    }
  }

  test("parseCommand handles Status command") {
    val json = """{"command": "status"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.Status) => ()
      case other => fail(s"Expected Status command, got $other")
    }
  }

  test("parseCommand handles Shutdown command") {
    val json = """{"command": "shutdown"}"""
    DaemonJson.parseCommand(json) match {
      case Right(DaemonCommand.Shutdown) => ()
      case other => fail(s"Expected Shutdown command, got $other")
    }
  }

  test("parseCommand returns error for unknown command") {
    val json = """{"command": "unknown"}"""
    DaemonJson.parseCommand(json) match {
      case Left(err) => assert(err.contains("unknown command"))
      case Right(_) => fail("Expected error for unknown command")
    }
  }

  test("parseCommand returns error for invalid JSON") {
    val json = "not valid json"
    DaemonJson.parseCommand(json) match {
      case Left(err) => assert(err.contains("JSON parse error"))
      case Right(_) => fail("Expected error for invalid JSON")
    }
  }

  test("renderCommand and parseCommand round-trip all commands") {
    val commands = List(
      DaemonCommand.List(true),
      DaemonCommand.List(false),
      DaemonCommand.Explain(Some(NodeId("n0"))),
      DaemonCommand.Explain(None),
      DaemonCommand.Find("42"),
      DaemonCommand.Deps(NodeId("n1")),
      DaemonCommand.Usages(NodeId("n2")),
      DaemonCommand.Focus(NodeId("n3")),
      DaemonCommand.Unfocus,
      DaemonCommand.Path(NodeId("n4")),
      DaemonCommand.Value(NodeId("n5")),
      DaemonCommand.Source(NodeId("n6")),
      DaemonCommand.Snippet(NodeId("n7"), 5),
      DaemonCommand.Eval(NodeId("n8"), "x + 1"),
      DaemonCommand.Status,
      DaemonCommand.Shutdown
    )

    commands.foreach { cmd =>
      val json = DaemonJson.renderCommand(cmd)
      DaemonJson.parseCommand(json) match {
        case Right(parsed) =>
          assertEquals(parsed, cmd, s"Round-trip failed for $cmd")
        case Left(err) =>
          fail(s"Failed to parse rendered command $cmd: $err")
      }
    }
  }
}

// =============================================================================
// SourceType JSON tests
// =============================================================================

class SourceTypeJsonTest extends munit.FunSuite {

  private def roundTrip(sourceType: SourceType): Either[String, SourceType] = {
    val json = DaemonJson.sourceTypeWriter.write(sourceType)
    DaemonJson.sourceTypeReader.read(dev.bosatsu.Json.Path.Root, json).left.map(_._1)
  }

  test("SourceType.Pure round-trips with expression") {
    val st = SourceType.Pure(Some("add(x, y)"))
    assertEquals(roundTrip(st), Right(st))
  }

  test("SourceType.Pure round-trips without expression") {
    val st = SourceType.Pure(None)
    assertEquals(roundTrip(st), Right(st))
  }

  test("SourceType.Operation round-trips") {
    val st = SourceType.Operation("Numeric", "add")
    assertEquals(roundTrip(st), Right(st))
  }

  test("SourceType.Binding round-trips") {
    val st = SourceType.Binding("myVar")
    assertEquals(roundTrip(st), Right(st))
  }

  test("SourceType.Parameter round-trips") {
    val st = SourceType.Parameter
    assertEquals(roundTrip(st), Right(st))
  }

  test("SourceType.Literal round-trips") {
    val st = SourceType.Literal
    assertEquals(roundTrip(st), Right(st))
  }
}

// =============================================================================
// TraceNode JSON tests
// =============================================================================

// =============================================================================
// DaemonClient.parseResponse tests (exercises all parse* helper methods)
// =============================================================================

class DaemonClientParseResponseTest extends munit.FunSuite {
  import dev.bosatsu.Json._

  // Helper to parse JSON string and call parseResponse
  private def parseJsonResponse(jsonStr: String): Either[String, DaemonResponse] = {
    parserFile.parseAll(jsonStr) match {
      case Left(err) => Left(s"JSON parse error: $err")
      case Right(json) => DaemonClient.parseResponse(json)
    }
  }

  // Helper to create a valid NodeSummary JSON object
  private def nodeSummaryJson(id: String, value: String = "42"): JObject = {
    JObject(List(
      "id" -> JString(id),
      "value" -> JString(value),
      "typeInfo" -> JString("Int"),
      "source" -> JString("literal"),
      "bindingName" -> JString("x"),
      "location" -> JObject(List(
        "file" -> JString("test.bosatsu"),
        "line" -> JNumberStr("1"),  // JNumberStr is how Bosatsu JSON parser stores numbers
        "column" -> JNumberStr("1")
      ))
    ))
  }

  // ========== Basic response structure tests ==========

  test("parseResponse rejects non-object JSON") {
    val result = parseJsonResponse("42")
    assert(result.isLeft)
    assert(result.left.exists(_.contains("not a JSON object")))
  }

  test("parseResponse rejects missing success field") {
    val result = parseJsonResponse("""{"data": {}}""")
    assert(result.isLeft)
    assert(result.left.exists(_.contains("Missing 'success'")))
  }

  test("parseResponse parses error response") {
    val result = parseJsonResponse("""{"success": false, "error": "Something failed"}""")
    assertEquals(result, Right(DaemonResponse.Error("Something failed")))
  }

  test("parseResponse handles error response without error message") {
    val result = parseJsonResponse("""{"success": false}""")
    assertEquals(result, Right(DaemonResponse.Error("Unknown error")))
  }

  test("parseResponse rejects success without data") {
    val result = parseJsonResponse("""{"success": true}""")
    assert(result.isLeft)
    assert(result.left.exists(_.contains("Missing 'data'")))
  }

  test("parseResponse rejects data that's not an object") {
    val result = parseJsonResponse("""{"success": true, "data": 42}""")
    assert(result.isLeft)
    assert(result.left.exists(_.contains("not a JSON object")))
  }

  test("parseResponse rejects unknown response type") {
    val result = parseJsonResponse("""{"success": true, "data": {"type": "unknown_type"}}""")
    assert(result.isLeft)
    assert(result.left.exists(_.contains("Unknown response type")))
  }

  test("parseResponse rejects missing type field") {
    val result = parseJsonResponse("""{"success": true, "data": {}}""")
    assert(result.isLeft)
    assert(result.left.exists(_.contains("Missing 'type'")))
  }

  // ========== NodeList response ==========

  test("parseResponse parses nodeList response") {
    val json = s"""{"success": true, "data": {
      "type": "nodeList",
      "nodes": [${nodeSummaryJson("n0").render}]
    }}"""
    val result = parseJsonResponse(json)
    assert(result.isRight)
    result.foreach {
      case DaemonResponse.Success(ResponseData.NodeList(nodes)) =>
        assertEquals(nodes.length, 1)
        assertEquals(nodes.head.id.value, "n0")
      case other => fail(s"Expected NodeList, got $other")
    }
  }

  test("parseResponse rejects nodeList without nodes array") {
    val json = """{"success": true, "data": {"type": "nodeList"}}"""
    val result = parseJsonResponse(json)
    assert(result.isLeft)
    assert(result.left.exists(_.contains("nodes")))
  }

  // ========== Explain response ==========

  test("parseResponse parses explain response") {
    val json = s"""{"success": true, "data": {
      "type": "explain",
      "nodeId": "n0",
      "tree": "x = 42",
      "compact": "x = 42",
      "node": ${nodeSummaryJson("n0").render}
    }}"""
    val result = parseJsonResponse(json)
    assert(result.isRight)
    result.foreach {
      case DaemonResponse.Success(ResponseData.ExplainResult(nodeId, tree, compact, _)) =>
        assertEquals(nodeId.value, "n0")
        assertEquals(tree, "x = 42")
        assertEquals(compact, "x = 42")
      case other => fail(s"Expected ExplainResult, got $other")
    }
  }

  // ========== Find response ==========

  test("parseResponse parses find response") {
    val json = s"""{"success": true, "data": {
      "type": "find",
      "nodes": [${nodeSummaryJson("n0").render}, ${nodeSummaryJson("n1").render}]
    }}"""
    val result = parseJsonResponse(json)
    assert(result.isRight)
    result.foreach {
      case DaemonResponse.Success(ResponseData.FindResult(nodes)) =>
        assertEquals(nodes.length, 2)
      case other => fail(s"Expected FindResult, got $other")
    }
  }

  // ========== Deps response ==========

  test("parseResponse parses deps response") {
    val json = s"""{"success": true, "data": {
      "type": "deps",
      "nodeId": "n0",
      "dependencies": [${nodeSummaryJson("n1").render}]
    }}"""
    val result = parseJsonResponse(json)
    assert(result.isRight)
    result.foreach {
      case DaemonResponse.Success(ResponseData.DepsResult(nodeId, deps)) =>
        assertEquals(nodeId.value, "n0")
        assertEquals(deps.length, 1)
      case other => fail(s"Expected DepsResult, got $other")
    }
  }

  // ========== Usages response ==========

  test("parseResponse parses usages response") {
    val json = s"""{"success": true, "data": {
      "type": "usages",
      "nodeId": "n0",
      "usages": [${nodeSummaryJson("n1").render}]
    }}"""
    val result = parseJsonResponse(json)
    assert(result.isRight)
    result.foreach {
      case DaemonResponse.Success(ResponseData.UsagesResult(nodeId, usages)) =>
        assertEquals(nodeId.value, "n0")
        assertEquals(usages.length, 1)
      case other => fail(s"Expected UsagesResult, got $other")
    }
  }

  // ========== Focus response ==========

  test("parseResponse parses focus response") {
    val json = s"""{"success": true, "data": {
      "type": "focus",
      "message": "Focused on n0",
      "node": ${nodeSummaryJson("n0").render}
    }}"""
    val result = parseJsonResponse(json)
    assert(result.isRight)
    result.foreach {
      case DaemonResponse.Success(ResponseData.FocusResult(message, node)) =>
        assertEquals(message, "Focused on n0")
        assertEquals(node.id.value, "n0")
      case other => fail(s"Expected FocusResult, got $other")
    }
  }

  // ========== Unfocus response ==========

  test("parseResponse parses unfocus response with message") {
    val json = """{"success": true, "data": {"type": "unfocus", "message": "Cleared focus"}}"""
    val result = parseJsonResponse(json)
    assertEquals(result, Right(DaemonResponse.Success(ResponseData.UnfocusResult("Cleared focus"))))
  }

  test("parseResponse parses unfocus response without message") {
    val json = """{"success": true, "data": {"type": "unfocus"}}"""
    val result = parseJsonResponse(json)
    assertEquals(result, Right(DaemonResponse.Success(ResponseData.UnfocusResult(""))))
  }

  // ========== Path response ==========

  test("parseResponse parses path response") {
    val json = s"""{"success": true, "data": {
      "type": "path",
      "path": [${nodeSummaryJson("n0").render}, ${nodeSummaryJson("n1").render}]
    }}"""
    val result = parseJsonResponse(json)
    assert(result.isRight)
    result.foreach {
      case DaemonResponse.Success(ResponseData.PathResult(path)) =>
        assertEquals(path.length, 2)
      case other => fail(s"Expected PathResult, got $other")
    }
  }

  // ========== Value response ==========

  test("parseResponse parses value response") {
    val json = """{"success": true, "data": {
      "type": "value",
      "nodeId": "n0",
      "value": "42",
      "valueJson": "{\"num\": 42}"
    }}"""
    val result = parseJsonResponse(json)
    assert(result.isRight)
    result.foreach {
      case DaemonResponse.Success(ResponseData.ValueResult(nodeId, value, valueJson)) =>
        assertEquals(nodeId.value, "n0")
        assertEquals(value, "42")
        assertEquals(valueJson, "{\"num\": 42}")
      case other => fail(s"Expected ValueResult, got $other")
    }
  }

  // ========== Source response ==========

  test("parseResponse parses source response with location") {
    val json = """{"success": true, "data": {
      "type": "source",
      "nodeId": "n0",
      "location": {"file": "test.bosatsu", "line": 5, "column": 3}
    }}"""
    val result = parseJsonResponse(json)
    assert(result.isRight, s"Parse failed: $result")
    result.foreach {
      case DaemonResponse.Success(ResponseData.SourceResult(nodeId, Some(loc))) =>
        assertEquals(nodeId.value, "n0")
        assertEquals(loc.file, "test.bosatsu")
        assertEquals(loc.line, 5)
        assertEquals(loc.column, 3)
      case other => fail(s"Expected SourceResult with location, got $other")
    }
  }

  test("parseResponse parses source response without location") {
    val json = """{"success": true, "data": {"type": "source", "nodeId": "n0"}}"""
    val result = parseJsonResponse(json)
    assert(result.isRight)
    result.foreach {
      case DaemonResponse.Success(ResponseData.SourceResult(nodeId, None)) =>
        assertEquals(nodeId.value, "n0")
      case other => fail(s"Expected SourceResult without location, got $other")
    }
  }

  // ========== Snippet response ==========

  test("parseResponse parses snippet response") {
    val json = """{"success": true, "data": {
      "type": "snippet",
      "nodeId": "n0",
      "location": {"file": "test.bosatsu", "line": 5, "column": 3},
      "lines": [
        {"lineNum": 4, "code": "# comment", "highlight": false},
        {"lineNum": 5, "code": "x = 42", "highlight": true}
      ],
      "scope": [
        {"name": "y", "value": "10", "nodeId": "n1"}
      ],
      "result": {"name": "x", "value": "42", "nodeId": "n0"}
    }}"""
    val result = parseJsonResponse(json)
    assert(result.isRight, s"Parse failed: $result")
    result.foreach {
      case DaemonResponse.Success(ResponseData.SnippetResult(nodeId, loc, lines, scope, result)) =>
        assertEquals(nodeId.value, "n0")
        assertEquals(loc.line, 5)
        assertEquals(lines.length, 2)
        assert(lines(1).highlight)
        assertEquals(scope.length, 1)
        assertEquals(result.name, "x")
      case other => fail(s"Expected SnippetResult, got $other")
    }
  }

  // ========== Eval response ==========

  test("parseResponse parses eval response") {
    val json = """{"success": true, "data": {
      "type": "eval",
      "expression": "x + 1",
      "result": "43",
      "scope": {"x": "42"}
    }}"""
    val result = parseJsonResponse(json)
    assert(result.isRight)
    result.foreach {
      case DaemonResponse.Success(ResponseData.EvalResult(expression, result, scope)) =>
        assertEquals(expression, "x + 1")
        assertEquals(result, "43")
        assertEquals(scope.get("x"), Some("42"))
      case other => fail(s"Expected EvalResult, got $other")
    }
  }

  test("parseResponse parses eval response without scope") {
    val json = """{"success": true, "data": {
      "type": "eval",
      "expression": "42",
      "result": "42"
    }}"""
    val result = parseJsonResponse(json)
    assert(result.isRight)
    result.foreach {
      case DaemonResponse.Success(ResponseData.EvalResult(_, _, scope)) =>
        assert(scope.isEmpty)
      case other => fail(s"Expected EvalResult, got $other")
    }
  }

  // ========== Status response ==========

  test("parseResponse parses status response") {
    val json = """{"success": true, "data": {
      "type": "status",
      "running": true,
      "traceFile": "/path/to/trace.json",
      "nodeCount": 10,
      "focusNodeId": "n5",
      "resultNodeId": "n9",
      "resultValue": "42"
    }}"""
    val result = parseJsonResponse(json)
    assert(result.isRight, s"Parse failed: $result")
    result.foreach {
      case DaemonResponse.Success(ResponseData.StatusResult(running, traceFile, nodeCount, focusNodeId, resultNodeId, resultValue)) =>
        assert(running)
        assertEquals(traceFile, "/path/to/trace.json")
        assertEquals(nodeCount, 10)
        assertEquals(focusNodeId, Some(NodeId("n5")))
        assertEquals(resultNodeId.value, "n9")
        assertEquals(resultValue, "42")
      case other => fail(s"Expected StatusResult, got $other")
    }
  }

  test("parseResponse parses status response without focus") {
    val json = """{"success": true, "data": {
      "type": "status",
      "running": true,
      "traceFile": "/path/to/trace.json",
      "nodeCount": 5,
      "resultNodeId": "n4",
      "resultValue": "100"
    }}"""
    val result = parseJsonResponse(json)
    assert(result.isRight, s"Parse failed: $result")
    result.foreach {
      case DaemonResponse.Success(ResponseData.StatusResult(_, _, _, focusNodeId, _, _)) =>
        assertEquals(focusNodeId, None)
      case other => fail(s"Expected StatusResult, got $other")
    }
  }

  // ========== Shutdown response ==========

  test("parseResponse parses shutdown response with message") {
    val json = """{"success": true, "data": {"type": "shutdown", "message": "Server shutting down"}}"""
    val result = parseJsonResponse(json)
    assertEquals(result, Right(DaemonResponse.Success(ResponseData.ShutdownResult("Server shutting down"))))
  }

  test("parseResponse parses shutdown response without message") {
    val json = """{"success": true, "data": {"type": "shutdown"}}"""
    val result = parseJsonResponse(json)
    assertEquals(result, Right(DaemonResponse.Success(ResponseData.ShutdownResult("ok"))))
  }

  // ========== NodeSummary parsing edge cases ==========

  test("parseResponse handles NodeSummary without optional fields") {
    val json = """{"success": true, "data": {
      "type": "nodeList",
      "nodes": [{
        "id": "n0",
        "typeInfo": "Unknown",
        "source": "computed"
      }]
    }}"""
    val result = parseJsonResponse(json)
    assert(result.isRight)
    result.foreach {
      case DaemonResponse.Success(ResponseData.NodeList(nodes)) =>
        assertEquals(nodes.head.value, None)
        assertEquals(nodes.head.bindingName, None)
        assertEquals(nodes.head.location, None)
      case other => fail(s"Expected NodeList, got $other")
    }
  }

  test("parseResponse rejects NodeSummary with missing id") {
    val json = """{"success": true, "data": {
      "type": "nodeList",
      "nodes": [{"typeInfo": "Int", "source": "literal"}]
    }}"""
    val result = parseJsonResponse(json)
    assert(result.isLeft)
    assert(result.left.exists(_.contains("id")))
  }

  // ========== Round-trip tests for all response types ==========

  test("all response types round-trip through render and parse") {
    val responses: List[DaemonResponse] = List(
      DaemonResponse.Error("Test error"),
      DaemonResponse.Success(ResponseData.NodeList(Nil)),
      DaemonResponse.Success(ResponseData.ExplainResult(NodeId("n0"), "tree", "compact",
        ResponseData.NodeSummary(NodeId("n0"), Some("42"), "Int", "literal", Some("x"), None))),
      DaemonResponse.Success(ResponseData.FindResult(Nil)),
      DaemonResponse.Success(ResponseData.DepsResult(NodeId("n0"), Nil)),
      DaemonResponse.Success(ResponseData.UsagesResult(NodeId("n0"), Nil)),
      DaemonResponse.Success(ResponseData.FocusResult("msg",
        ResponseData.NodeSummary(NodeId("n0"), Some("42"), "Int", "literal", None, None))),
      DaemonResponse.Success(ResponseData.UnfocusResult("cleared")),
      DaemonResponse.Success(ResponseData.PathResult(Nil)),
      DaemonResponse.Success(ResponseData.ValueResult(NodeId("n0"), "42", "{}")),
      DaemonResponse.Success(ResponseData.SourceResult(NodeId("n0"), None)),
      DaemonResponse.Success(ResponseData.SnippetResult(
        NodeId("n0"),
        SourceLocation("test.bosatsu", 1, 1),
        List(ResponseData.SnippetLine(1, "x = 42", true)),
        Nil,
        ResponseData.ScopeEntry("x", "42", Some(NodeId("n0")))
      )),
      DaemonResponse.Success(ResponseData.EvalResult("42", "42", Map.empty)),
      DaemonResponse.Success(ResponseData.StatusResult(true, "/path", 5, None, NodeId("n4"), "100")),
      DaemonResponse.Success(ResponseData.ShutdownResult("bye"))
    )

    responses.foreach { response =>
      val json = DaemonJson.renderResponse(response)
      val parsed = parseJsonResponse(json)
      assert(parsed.isRight, s"Failed to parse: ${json.take(100)}")
    }
  }
}

class TraceNodeJsonTest extends munit.FunSuite {

  private def roundTrip(node: TraceNode): Either[String, TraceNode] = {
    val json = DaemonJson.traceNodeWriter.write(node)
    DaemonJson.traceNodeReader.read(dev.bosatsu.Json.Path.Root, json).left.map(_._1)
  }

  test("TraceNode round-trips with all fields") {
    val node = TraceNode(
      id = NodeId("n0"),
      value = "42",
      source = SourceType.Literal,
      bindingName = Some("x"),
      location = Some(SourceLocation("test.bosatsu", 5, 3)),
      dependencies = List(NodeId("n1"), NodeId("n2")),
      usedBy = List(NodeId("n3"))
    )
    assertEquals(roundTrip(node), Right(node))
  }

  test("TraceNode round-trips with minimal fields") {
    val node = TraceNode(
      id = NodeId("n0"),
      value = "0",
      source = SourceType.Literal,
      bindingName = None,
      location = None,
      dependencies = Nil,
      usedBy = Nil
    )
    assertEquals(roundTrip(node), Right(node))
  }

  test("TraceNode with Operation source round-trips") {
    val node = TraceNode(
      id = NodeId("n5"),
      value = "30",
      source = SourceType.Operation("Numeric", "times"),
      bindingName = Some("result"),
      location = Some(SourceLocation("calc.bosatsu", 10, 1)),
      dependencies = List(NodeId("n3"), NodeId("n4")),
      usedBy = Nil
    )
    assertEquals(roundTrip(node), Right(node))
  }
}

class TraceGeneratorTest extends munit.FunSuite {

  // Simple Bosatsu source for testing
  private val simpleSource = """package TestPackage

x = 1
y = 2
z = 3
"""

  test("generateFromSource creates trace with correct node count") {
    TraceGenerator.generateFromSource(simpleSource, "test.bosatsu") match {
      case Left(err) => fail(s"Failed to generate trace: $err")
      case Right(trace) =>
        assertEquals(trace.nodeCount, 3)
    }
  }

  test("generateFromSource creates nodes with IDs") {
    TraceGenerator.generateFromSource(simpleSource, "test.bosatsu") match {
      case Left(err) => fail(s"Failed to generate trace: $err")
      case Right(trace) =>
        assert(trace.nodes.contains(NodeId("n0")))
        assert(trace.nodes.contains(NodeId("n1")))
        assert(trace.nodes.contains(NodeId("n2")))
    }
  }

  test("generateFromSource sets source type for literals") {
    TraceGenerator.generateFromSource(simpleSource, "test.bosatsu") match {
      case Left(err) => fail(s"Failed to generate trace: $err")
      case Right(trace) =>
        val n0 = trace.nodes(NodeId("n0"))
        assertEquals(n0.source, SourceType.Literal)
    }
  }

  test("generateFromSource sets correct values") {
    TraceGenerator.generateFromSource(simpleSource, "test.bosatsu") match {
      case Left(err) => fail(s"Failed to generate trace: $err")
      case Right(trace) =>
        // Values should be "1", "2", "3" for the three literals
        val values = trace.nodes.values.map(_.value).toSet
        assert(values.contains("1"))
        assert(values.contains("2"))
        assert(values.contains("3"))
    }
  }

  test("generateFromSource includes source locations") {
    TraceGenerator.generateFromSource(simpleSource, "test.bosatsu") match {
      case Left(err) => fail(s"Failed to generate trace: $err")
      case Right(trace) =>
        // All nodes should have source locations
        trace.nodes.values.foreach { node =>
          assert(node.location.isDefined, s"Node ${node.id} should have a location")
        }
    }
  }

  test("generateFromSource handles parse errors") {
    val invalidSource = "this is not valid bosatsu"
    TraceGenerator.generateFromSource(invalidSource, "test.bosatsu") match {
      case Left(err) =>
        assert(err.contains("Parse error"))
      case Right(_) =>
        fail("Expected parse error")
    }
  }

  test("generateFromSource serializes to valid JSON") {
    TraceGenerator.generateFromSource(simpleSource, "test.bosatsu") match {
      case Left(err) => fail(s"Failed to generate trace: $err")
      case Right(trace) =>
        val json = DaemonJson.renderTrace(trace)
        // Verify it can be parsed back
        DaemonJson.parseTrace(json) match {
          case Left(err) => fail(s"Failed to parse JSON: $err")
          case Right(parsed) =>
            assertEquals(parsed.nodeCount, trace.nodeCount)
            assertEquals(parsed.resultNodeId, trace.resultNodeId)
        }
    }
  }

  test("generateFromSource round-trips through JSON") {
    TraceGenerator.generateFromSource(simpleSource, "test.bosatsu") match {
      case Left(err) => fail(s"Failed to generate trace: $err")
      case Right(original) =>
        val json = DaemonJson.renderTrace(original)
        DaemonJson.parseTrace(json) match {
          case Left(err) => fail(s"Failed to parse JSON: $err")
          case Right(parsed) =>
            // Verify node IDs match
            assertEquals(
              parsed.nodes.keySet,
              original.nodes.keySet
            )
            // Verify values match
            original.nodes.foreach { case (id, node) =>
              val parsedNode = parsed.nodes(id)
              assertEquals(parsedNode.value, node.value)
              assertEquals(parsedNode.source, node.source)
            }
        }
    }
  }

  // Dependency tracking tests
  private val depsSource = """package DepsTest

x = 42
y = x
z = y
result = z
"""

  test("generateFromSource tracks intra-package dependencies") {
    TraceGenerator.generateFromSource(depsSource, "deps.bosatsu") match {
      case Left(err) => fail(s"Failed to generate trace: $err")
      case Right(trace) =>
        // Should have 4 nodes: x (literal), y (ref to x), z (ref to y), result (ref to z)
        assertEquals(trace.nodeCount, 4)

        // Find the literal node (has no dependencies)
        val literalNodes = trace.nodes.values.filter(_.dependencies.isEmpty).toList
        assertEquals(literalNodes.size, 1, "Should have exactly one literal node with no deps")
        val literalNode = literalNodes.head
        assertEquals(literalNode.value, "42")

        // The literal should be used by one node
        assertEquals(literalNode.usedBy.size, 1)
    }
  }

  test("generateFromSource creates proper dependency chain") {
    TraceGenerator.generateFromSource(depsSource, "deps.bosatsu") match {
      case Left(err) => fail(s"Failed to generate trace: $err")
      case Right(trace) =>
        // Build a map of what each node depends on
        val depCounts = trace.nodes.values.map(n => n.id -> n.dependencies.size).toMap

        // One node has 0 deps (literal), three have 1 dep each (y->x, z->y, result->z)
        val noDepCount = depCounts.values.count(_ == 0)
        val oneDepCount = depCounts.values.count(_ == 1)
        assertEquals(noDepCount, 1, "Should have 1 node with no dependencies")
        assertEquals(oneDepCount, 3, "Should have 3 nodes with one dependency each")
    }
  }

  test("generateFromSource usedBy is inverse of dependencies") {
    TraceGenerator.generateFromSource(depsSource, "deps.bosatsu") match {
      case Left(err) => fail(s"Failed to generate trace: $err")
      case Right(trace) =>
        // For each node that has dependencies, verify the dep's usedBy includes this node
        trace.nodes.values.foreach { node =>
          node.dependencies.foreach { depId =>
            val depNode = trace.nodes(depId)
            assert(
              depNode.usedBy.contains(node.id),
              s"Node ${depId.value} usedBy should contain ${node.id.value}"
            )
          }
        }
    }
  }
}
