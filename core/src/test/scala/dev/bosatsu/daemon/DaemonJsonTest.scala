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
}
