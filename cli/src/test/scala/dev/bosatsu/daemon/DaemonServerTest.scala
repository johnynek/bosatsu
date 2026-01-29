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
