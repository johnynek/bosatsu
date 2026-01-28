package dev.bosatsu.daemon

import org.scalacheck.{Prop, Gen}
import org.scalacheck.Prop.forAll
import dev.bosatsu.Json

class DaemonJsonTest extends munit.ScalaCheckSuite {

  import DaemonGen._
  import DaemonJson._

  // Round-trip property: command -> JSON -> command
  property("DaemonJson.parseCommand roundtrips for all commands") {
    forAll(genDaemonCommand) { cmd =>
      val json = renderCommand(cmd)
      val parsed = parseCommand(json)
      parsed == Right(cmd)
    }
  }

  // Response rendering produces valid JSON
  property("DaemonJson.renderResponse produces parseable JSON") {
    forAll(genDaemonResponse) { response =>
      val json = renderResponse(response)
      Json.parserFile.parseAll(json).isRight
    }
  }

  // Test specific command parsing
  test("DaemonJson.parseCommand parses list command") {
    val json = """{"command": "list", "showValues": "true"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.List(true)))
  }

  test("DaemonJson.parseCommand parses list command without showValues") {
    val json = """{"command": "list"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.List(false)))
  }

  test("DaemonJson.parseCommand parses explain with nodeId") {
    val json = """{"command": "explain", "nodeId": "n5"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.Explain(Some(NodeId("n5")))))
  }

  test("DaemonJson.parseCommand parses explain without nodeId") {
    val json = """{"command": "explain"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.Explain(None)))
  }

  test("DaemonJson.parseCommand parses deps command") {
    val json = """{"command": "deps", "nodeId": "n3"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.Deps(NodeId("n3"))))
  }

  test("DaemonJson.parseCommand parses focus command") {
    val json = """{"command": "focus", "nodeId": "n7"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.Focus(NodeId("n7"))))
  }

  test("DaemonJson.parseCommand parses unfocus command") {
    val json = """{"command": "unfocus"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.Unfocus))
  }

  test("DaemonJson.parseCommand parses snippet with context") {
    val json = """{"command": "snippet", "nodeId": "n2", "contextLines": "5"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.Snippet(NodeId("n2"), 5)))
  }

  test("DaemonJson.parseCommand parses eval command") {
    val json = """{"command": "eval", "nodeId": "n1", "expression": "x + 1"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.Eval(NodeId("n1"), "x + 1")))
  }

  test("DaemonJson.parseCommand parses status command") {
    val json = """{"command": "status"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.Status))
  }

  test("DaemonJson.parseCommand parses shutdown command") {
    val json = """{"command": "shutdown"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.Shutdown))
  }

  test("DaemonJson.parseCommand returns error for unknown command") {
    val json = """{"command": "unknown"}"""
    assert(parseCommand(json).isLeft)
  }

  test("DaemonJson.parseCommand returns error for invalid JSON") {
    val json = """not valid json"""
    assert(parseCommand(json).isLeft)
  }

  // Test response rendering
  test("DaemonJson.renderResponse renders success response") {
    val response = DaemonResponse.Success(ResponseData.ShutdownResult("goodbye"))
    val json = renderResponse(response)
    assert(json.contains("\"success\": true"))
    assert(json.contains("\"shutdown\""))
    assert(json.contains("goodbye"))
  }

  test("DaemonJson.renderResponse renders error response") {
    val response = DaemonResponse.Error("node not found")
    val json = renderResponse(response)
    assert(json.contains("\"success\": false"))
    assert(json.contains("node not found"))
  }

  test("DaemonJson.renderResponse renders NodeList") {
    val summary = ResponseData.NodeSummary(
      NodeId("n1"),
      Some("42"),
      "Int",
      "literal",
      Some("x"),
      Some(SourceLocation("test.bosatsu", 10, 5))
    )
    val response = DaemonResponse.Success(ResponseData.NodeList(List(summary)))
    val json = renderResponse(response)
    assert(json.contains("\"nodeList\""))
    assert(json.contains("\"n1\""))
    assert(json.contains("\"42\""))
    assert(json.contains("test.bosatsu"))
  }

  // Generator for DaemonResponse
  lazy val genDaemonResponse: Gen[DaemonResponse] = {
    val genNodeSummary = for {
      id <- genNodeId
      value <- Gen.option(Gen.alphaNumStr.suchThat(_.nonEmpty))
      typeInfo <- Gen.alphaStr.suchThat(_.nonEmpty)
      source <- Gen.oneOf("pure", "operation", "binding", "parameter", "literal")
      bindingName <- Gen.option(Gen.alphaStr.suchThat(_.nonEmpty))
      location <- Gen.option(genSourceLocation)
    } yield ResponseData.NodeSummary(id, value, typeInfo, source, bindingName, location)

    val genResponseData: Gen[ResponseData] = Gen.oneOf(
      Gen.listOf(genNodeSummary).map(ResponseData.NodeList(_)),
      Gen.alphaNumStr.map(ResponseData.ShutdownResult(_)),
      Gen.alphaNumStr.map(ResponseData.UnfocusResult(_)),
      for {
        nodeId <- genNodeId
        tree <- Gen.alphaNumStr
        compact <- Gen.alphaNumStr
        node <- genNodeSummary
      } yield ResponseData.ExplainResult(nodeId, tree, compact, node)
    )

    Gen.oneOf(
      genResponseData.map(DaemonResponse.Success(_)),
      Gen.alphaNumStr.map(DaemonResponse.Error(_))
    )
  }

  // =========================================================================
  // Additional writer tests for coverage
  // =========================================================================

  test("DaemonJson writes all SourceType variants") {
    // Pure
    val pure = SourceType.Pure(Some("x + y"))
    val pureJson = DaemonJson.sourceTypeWriter.write(pure).render
    assert(pureJson.contains("\"pure\""))
    assert(pureJson.contains("x + y"))

    // Pure with None
    val pureNone = SourceType.Pure(None)
    val pureNoneJson = DaemonJson.sourceTypeWriter.write(pureNone).render
    assert(pureNoneJson.contains("\"pure\""))
    assert(pureNoneJson.contains("null"))

    // Operation
    val op = SourceType.Operation("db", "query")
    val opJson = DaemonJson.sourceTypeWriter.write(op).render
    assert(opJson.contains("\"operation\""))
    assert(opJson.contains("db"))
    assert(opJson.contains("query"))

    // Binding
    val binding = SourceType.Binding("myVar")
    val bindingJson = DaemonJson.sourceTypeWriter.write(binding).render
    assert(bindingJson.contains("\"binding\""))
    assert(bindingJson.contains("myVar"))

    // Parameter
    val param = SourceType.Parameter
    val paramJson = DaemonJson.sourceTypeWriter.write(param).render
    assert(paramJson.contains("\"parameter\""))

    // Literal
    val literal = SourceType.Literal
    val literalJson = DaemonJson.sourceTypeWriter.write(literal).render
    assert(literalJson.contains("\"literal\""))
  }

  test("DaemonJson writes TraceNode correctly") {
    val node = TraceNode(
      id = NodeId("n1"),
      value = "42",
      source = SourceType.Literal,
      bindingName = Some("x"),
      location = Some(SourceLocation("test.bosatsu", 10, 5)),
      dependencies = List(NodeId("n0")),
      usedBy = List(NodeId("n2"))
    )
    val json = DaemonJson.traceNodeWriter.write(node).render
    assert(json.contains("\"n1\""))
    assert(json.contains("42"))
    assert(json.contains("literal"))
    assert(json.contains("\"x\""))
    assert(json.contains("test.bosatsu"))
    assert(json.contains("\"n0\""))
    assert(json.contains("\"n2\""))
  }

  test("DaemonJson writes TraceNode with null bindingName and location") {
    val node = TraceNode(
      id = NodeId("n1"),
      value = "42",
      source = SourceType.Literal,
      bindingName = None,
      location = None,
      dependencies = Nil,
      usedBy = Nil
    )
    val json = DaemonJson.traceNodeWriter.write(node).render
    assert(json.contains("\"n1\""))
    // bindingName and location should be null
    assert(json.contains("\"bindingName\": null"))
    assert(json.contains("\"location\": null"))
  }

  test("DaemonJson writes ProvenanceTrace correctly") {
    val node = TraceNode(
      id = NodeId("n0"),
      value = "100",
      source = SourceType.Literal,
      bindingName = Some("result"),
      location = None,
      dependencies = Nil,
      usedBy = Nil
    )
    val trace = ProvenanceTrace(
      nodes = Map(NodeId("n0") -> node),
      resultNodeId = NodeId("n0"),
      sourceFile = "main.bosatsu"
    )
    val json = DaemonJson.provenanceTraceWriter.write(trace).render
    assert(json.contains("\"n0\""))
    assert(json.contains("main.bosatsu"))
  }

  test("DaemonJson writes NodeSummary with null value") {
    val summary = ResponseData.NodeSummary(
      id = NodeId("n5"),
      value = None,
      typeInfo = "Int",
      source = "literal",
      bindingName = None,
      location = None
    )
    val json = DaemonJson.nodeSummaryWriter.write(summary).render
    assert(json.contains("\"n5\""))
    assert(json.contains("\"value\": null"))
    assert(json.contains("Int"))
  }

  test("DaemonJson writes all ResponseData variants") {
    // FindResult
    val findResult = ResponseData.FindResult(List())
    val findJson = DaemonJson.responseDataWriter.write(findResult).render
    assert(findJson.contains("\"find\""))

    // DepsResult
    val depsResult = ResponseData.DepsResult(NodeId("n1"), List())
    val depsJson = DaemonJson.responseDataWriter.write(depsResult).render
    assert(depsJson.contains("\"deps\""))
    assert(depsJson.contains("\"n1\""))

    // UsagesResult
    val usagesResult = ResponseData.UsagesResult(NodeId("n2"), List())
    val usagesJson = DaemonJson.responseDataWriter.write(usagesResult).render
    assert(usagesJson.contains("\"usages\""))
    assert(usagesJson.contains("\"n2\""))

    // FocusResult
    val nodeSummary = ResponseData.NodeSummary(NodeId("n3"), Some("10"), "Int", "literal", None, None)
    val focusResult = ResponseData.FocusResult("Focused", nodeSummary)
    val focusJson = DaemonJson.responseDataWriter.write(focusResult).render
    assert(focusJson.contains("\"focus\""))
    assert(focusJson.contains("Focused"))

    // PathResult
    val pathResult = ResponseData.PathResult(List(nodeSummary))
    val pathJson = DaemonJson.responseDataWriter.write(pathResult).render
    assert(pathJson.contains("\"path\""))

    // ValueResult
    val valueResult = ResponseData.ValueResult(NodeId("n4"), "42", "42")
    val valueJson = DaemonJson.responseDataWriter.write(valueResult).render
    assert(valueJson.contains("\"value\""))
    assert(valueJson.contains("\"n4\""))

    // SourceResult
    val srcResult = ResponseData.SourceResult(NodeId("n5"), Some(SourceLocation("f.bosatsu", 1, 1)))
    val srcJson = DaemonJson.responseDataWriter.write(srcResult).render
    assert(srcJson.contains("\"source\""))

    // SourceResult with no location
    val srcResultNone = ResponseData.SourceResult(NodeId("n6"), None)
    val srcNoneJson = DaemonJson.responseDataWriter.write(srcResultNone).render
    assert(srcNoneJson.contains("\"location\": null"))

    // SnippetResult
    val snippetLine = ResponseData.SnippetLine(10, "  x = 42", true)
    val scopeEntry = ResponseData.ScopeEntry("x", "42", Some(NodeId("n1")))
    val resultEntry = ResponseData.ScopeEntry("result", "42", None)
    val snippetResult = ResponseData.SnippetResult(
      NodeId("n7"),
      SourceLocation("f.bosatsu", 10, 1),
      List(snippetLine),
      List(scopeEntry),
      resultEntry
    )
    val snippetJson = DaemonJson.responseDataWriter.write(snippetResult).render
    assert(snippetJson.contains("\"snippet\""))
    assert(snippetJson.contains("x = 42"))

    // EvalResult
    val evalResult = ResponseData.EvalResult("x + 1", "43", Map("x" -> "42"))
    val evalJson = DaemonJson.responseDataWriter.write(evalResult).render
    assert(evalJson.contains("\"eval\""))
    assert(evalJson.contains("x + 1"))
    assert(evalJson.contains("43"))

    // StatusResult
    val statusResult = ResponseData.StatusResult(
      running = true,
      traceFile = "/tmp/trace.json",
      nodeCount = 10,
      focusNodeId = Some(NodeId("n1")),
      resultNodeId = NodeId("n9"),
      resultValue = "100"
    )
    val statusJson = DaemonJson.responseDataWriter.write(statusResult).render
    assert(statusJson.contains("\"status\""))
    assert(statusJson.contains("\"running\": true"))
    assert(statusJson.contains("/tmp/trace.json"))

    // StatusResult with no focus
    val statusResultNoFocus = ResponseData.StatusResult(
      running = false,
      traceFile = "/tmp/trace.json",
      nodeCount = 5,
      focusNodeId = None,
      resultNodeId = NodeId("n4"),
      resultValue = "50"
    )
    val statusNoFocusJson = DaemonJson.responseDataWriter.write(statusResultNoFocus).render
    assert(statusNoFocusJson.contains("\"focusNodeId\": null"))
  }

  test("DaemonJson writes SnippetLine correctly") {
    val line = ResponseData.SnippetLine(42, "let x = 1", false)
    val json = DaemonJson.snippetLineWriter.write(line).render
    assert(json.contains("42"))
    assert(json.contains("let x = 1"))
    assert(json.contains("false"))
  }

  test("DaemonJson writes ScopeEntry correctly") {
    val entry = ResponseData.ScopeEntry("myVar", "123", Some(NodeId("n10")))
    val json = DaemonJson.scopeEntryWriter.write(entry).render
    assert(json.contains("myVar"))
    assert(json.contains("123"))
    assert(json.contains("\"n10\""))

    val entryNoId = ResponseData.ScopeEntry("x", "5", None)
    val jsonNoId = DaemonJson.scopeEntryWriter.write(entryNoId).render
    assert(jsonNoId.contains("\"nodeId\": null"))
  }

  // =========================================================================
  // Additional reader tests for coverage
  // =========================================================================

  test("DaemonJson reads SourceType.Pure with expression") {
    val json = """{"type": "pure", "expression": "x + y"}"""
    val parsed = Json.parserFile.parseAll(json).flatMap { j =>
      DaemonJson.sourceTypeReader.read(Json.Path.Root, j)
    }
    assertEquals(parsed, Right(SourceType.Pure(Some("x + y"))))
  }

  test("DaemonJson reads SourceType.Pure without expression") {
    val json = """{"type": "pure"}"""
    val parsed = Json.parserFile.parseAll(json).flatMap { j =>
      DaemonJson.sourceTypeReader.read(Json.Path.Root, j)
    }
    assertEquals(parsed, Right(SourceType.Pure(None)))
  }

  test("DaemonJson reads SourceType.Operation") {
    val json = """{"type": "operation", "interface": "db", "method": "query"}"""
    val parsed = Json.parserFile.parseAll(json).flatMap { j =>
      DaemonJson.sourceTypeReader.read(Json.Path.Root, j)
    }
    assertEquals(parsed, Right(SourceType.Operation("db", "query")))
  }

  test("DaemonJson reads SourceType.Binding") {
    val json = """{"type": "binding", "name": "myVar"}"""
    val parsed = Json.parserFile.parseAll(json).flatMap { j =>
      DaemonJson.sourceTypeReader.read(Json.Path.Root, j)
    }
    assertEquals(parsed, Right(SourceType.Binding("myVar")))
  }

  test("DaemonJson reads SourceType.Parameter") {
    val json = """{"type": "parameter"}"""
    val parsed = Json.parserFile.parseAll(json).flatMap { j =>
      DaemonJson.sourceTypeReader.read(Json.Path.Root, j)
    }
    assertEquals(parsed, Right(SourceType.Parameter))
  }

  test("DaemonJson reads SourceType.Literal") {
    val json = """{"type": "literal"}"""
    val parsed = Json.parserFile.parseAll(json).flatMap { j =>
      DaemonJson.sourceTypeReader.read(Json.Path.Root, j)
    }
    assertEquals(parsed, Right(SourceType.Literal))
  }

  test("DaemonJson returns error for unknown source type") {
    val json = """{"type": "unknown"}"""
    val parsed = Json.parserFile.parseAll(json).flatMap { j =>
      DaemonJson.sourceTypeReader.read(Json.Path.Root, j)
    }
    assert(parsed.isLeft)
  }

  test("DaemonJson reads TraceNode correctly") {
    val json = """{
      "id": "n1",
      "value": "42",
      "source": {"type": "literal"},
      "bindingName": "x",
      "location": {"file": "test.bosatsu", "line": "10", "column": "5"},
      "dependencies": ["n0"],
      "usedBy": ["n2"]
    }"""
    val parsed = Json.parserFile.parseAll(json).flatMap { j =>
      DaemonJson.traceNodeReader.read(Json.Path.Root, j)
    }
    assert(parsed.isRight)
    val node = parsed.toOption.get
    assertEquals(node.id, NodeId("n1"))
    assertEquals(node.value, "42")
    assertEquals(node.bindingName, Some("x"))
    assertEquals(node.location, Some(SourceLocation("test.bosatsu", 10, 5)))
    assertEquals(node.dependencies, List(NodeId("n0")))
    assertEquals(node.usedBy, List(NodeId("n2")))
  }

  test("DaemonJson reads ProvenanceTrace correctly") {
    val json = """{
      "nodes": [{
        "id": "n0",
        "value": "100",
        "source": {"type": "literal"},
        "dependencies": [],
        "usedBy": []
      }],
      "resultNodeId": "n0",
      "sourceFile": "main.bosatsu"
    }"""
    val result = DaemonJson.parseTrace(json)
    assert(result.isRight)
    val trace = result.toOption.get
    assertEquals(trace.nodes.size, 1)
    assertEquals(trace.resultNodeId, NodeId("n0"))
    assertEquals(trace.sourceFile, "main.bosatsu")
  }

  test("DaemonJson.parseTrace returns error for invalid JSON") {
    val result = DaemonJson.parseTrace("not valid json")
    assert(result.isLeft)
  }

  test("DaemonJson.parseCommand handles path command") {
    val json = """{"command": "path", "nodeId": "n5"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.Path(NodeId("n5"))))
  }

  test("DaemonJson.parseCommand handles usages command") {
    val json = """{"command": "usages", "nodeId": "n3"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.Usages(NodeId("n3"))))
  }

  test("DaemonJson.parseCommand handles value command") {
    val json = """{"command": "value", "nodeId": "n4"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.Value(NodeId("n4"))))
  }

  test("DaemonJson.parseCommand handles source command") {
    val json = """{"command": "source", "nodeId": "n6"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.Source(NodeId("n6"))))
  }

  test("DaemonJson.parseCommand handles find command") {
    val json = """{"command": "find", "value": "hello"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.Find("hello")))
  }

  test("DaemonJson.parseCommand handles snippet with default context") {
    val json = """{"command": "snippet", "nodeId": "n1"}"""
    assertEquals(parseCommand(json), Right(DaemonCommand.Snippet(NodeId("n1"), 3)))
  }

  // =========================================================================
  // renderCommand tests for coverage
  // =========================================================================

  test("DaemonJson.renderCommand renders all command types") {
    // Test each command type produces valid JSON that can be parsed back
    val commands: List[DaemonCommand] = List(
      DaemonCommand.List(true),
      DaemonCommand.List(false),
      DaemonCommand.Explain(Some(NodeId("n1"))),
      DaemonCommand.Explain(None),
      DaemonCommand.Find("test"),
      DaemonCommand.Deps(NodeId("n2")),
      DaemonCommand.Usages(NodeId("n3")),
      DaemonCommand.Focus(NodeId("n4")),
      DaemonCommand.Unfocus,
      DaemonCommand.Path(NodeId("n5")),
      DaemonCommand.Value(NodeId("n6")),
      DaemonCommand.Source(NodeId("n7")),
      DaemonCommand.Snippet(NodeId("n8"), 5),
      DaemonCommand.Eval(NodeId("n9"), "x + 1"),
      DaemonCommand.Status,
      DaemonCommand.Shutdown
    )

    commands.foreach { cmd =>
      val json = renderCommand(cmd)
      val parsed = parseCommand(json)
      assertEquals(parsed, Right(cmd), s"Failed for command: $cmd")
    }
  }

  test("DaemonJson.renderTrace produces parseable JSON") {
    val node = TraceNode(
      id = NodeId("n0"),
      value = "42",
      source = SourceType.Literal,
      bindingName = None,
      location = None,
      dependencies = Nil,
      usedBy = Nil
    )
    val trace = ProvenanceTrace(
      nodes = Map(NodeId("n0") -> node),
      resultNodeId = NodeId("n0"),
      sourceFile = "test.bosatsu"
    )
    val json = DaemonJson.renderTrace(trace)
    val parsed = DaemonJson.parseTrace(json)
    assert(parsed.isRight)
  }
}
