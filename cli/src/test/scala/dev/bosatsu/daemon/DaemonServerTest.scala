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
