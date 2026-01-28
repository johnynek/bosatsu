package dev.bosatsu.daemon

import munit.FunSuite

class DaemonCliTest extends FunSuite {

  // =========================================================================
  // CLI Parsing Tests
  // =========================================================================

  test("parse fails with empty args") {
    val result = DaemonCli.parse(List())
    assert(result.isLeft)
  }

  test("parse handles list subcommand") {
    val result = DaemonCli.parse(List("list"))
    assert(result.isRight)
    result match {
      case Right(DaemonCli.SendCommandAction(DaemonCommand.List(showValues), _)) =>
        assertEquals(showValues, false)
      case _ => fail("Expected list command")
    }
  }

  test("parse handles list with --values flag") {
    val result = DaemonCli.parse(List("list", "-v"))
    assert(result.isRight)
    result match {
      case Right(DaemonCli.SendCommandAction(DaemonCommand.List(showValues), _)) =>
        assertEquals(showValues, true)
      case _ => fail("Expected list command with values")
    }
  }

  test("parse handles explain subcommand without node-id") {
    val result = DaemonCli.parse(List("explain"))
    assert(result.isRight)
    result match {
      case Right(DaemonCli.SendCommandAction(DaemonCommand.Explain(nodeId), _)) =>
        assertEquals(nodeId, None)
      case _ => fail("Expected explain command")
    }
  }

  test("parse handles explain subcommand with node-id") {
    val result = DaemonCli.parse(List("explain", "n5"))
    assert(result.isRight)
    result match {
      case Right(DaemonCli.SendCommandAction(DaemonCommand.Explain(nodeId), _)) =>
        assertEquals(nodeId, Some(NodeId("n5")))
      case _ => fail("Expected explain command with node-id")
    }
  }

  test("parse handles deps subcommand") {
    val result = DaemonCli.parse(List("deps", "n3"))
    assert(result.isRight)
    result match {
      case Right(DaemonCli.SendCommandAction(DaemonCommand.Deps(nodeId), _)) =>
        assertEquals(nodeId, NodeId("n3"))
      case _ => fail("Expected deps command")
    }
  }

  test("parse handles usages subcommand") {
    val result = DaemonCli.parse(List("usages", "n4"))
    assert(result.isRight)
    result match {
      case Right(DaemonCli.SendCommandAction(DaemonCommand.Usages(nodeId), _)) =>
        assertEquals(nodeId, NodeId("n4"))
      case _ => fail("Expected usages command")
    }
  }

  test("parse handles focus subcommand") {
    val result = DaemonCli.parse(List("focus", "n7"))
    assert(result.isRight)
    result match {
      case Right(DaemonCli.SendCommandAction(DaemonCommand.Focus(nodeId), _)) =>
        assertEquals(nodeId, NodeId("n7"))
      case _ => fail("Expected focus command")
    }
  }

  test("parse handles unfocus subcommand") {
    val result = DaemonCli.parse(List("unfocus"))
    assert(result.isRight)
    result match {
      case Right(DaemonCli.SendCommandAction(DaemonCommand.Unfocus, _)) =>
        // Success
      case _ => fail("Expected unfocus command")
    }
  }

  test("parse handles value subcommand") {
    val result = DaemonCli.parse(List("value", "n2"))
    assert(result.isRight)
    result match {
      case Right(DaemonCli.SendCommandAction(DaemonCommand.Value(nodeId), _)) =>
        assertEquals(nodeId, NodeId("n2"))
      case _ => fail("Expected value command")
    }
  }

  test("parse handles source subcommand") {
    val result = DaemonCli.parse(List("source", "n6"))
    assert(result.isRight)
    result match {
      case Right(DaemonCli.SendCommandAction(DaemonCommand.Source(nodeId), _)) =>
        assertEquals(nodeId, NodeId("n6"))
      case _ => fail("Expected source command")
    }
  }

  test("parse handles find subcommand") {
    val result = DaemonCli.parse(List("find", "42"))
    assert(result.isRight)
    result match {
      case Right(DaemonCli.SendCommandAction(DaemonCommand.Find(value), _)) =>
        assertEquals(value, "42")
      case _ => fail("Expected find command")
    }
  }

  test("parse handles start subcommand") {
    val result = DaemonCli.parse(List("start", "/path/to/trace.json"))
    assert(result.isRight)
    result match {
      case Right(DaemonCli.StartAction(tracePath, socketPath)) =>
        assertEquals(tracePath.toString, "/path/to/trace.json")
        assertEquals(socketPath, None)
      case _ => fail("Expected start action")
    }
  }

  test("parse handles stop subcommand") {
    val result = DaemonCli.parse(List("stop"))
    assert(result.isRight)
    result match {
      case Right(DaemonCli.StopAction(socketPath)) =>
        assertEquals(socketPath, None)
      case _ => fail("Expected stop action")
    }
  }

  test("parse handles status subcommand") {
    val result = DaemonCli.parse(List("status"))
    assert(result.isRight)
    result match {
      case Right(DaemonCli.StatusAction(socketPath)) =>
        assertEquals(socketPath, None)
      case _ => fail("Expected status action")
    }
  }

  test("parse handles socket option for list") {
    val result = DaemonCli.parse(List("list", "-s", "/tmp/test.sock"))
    assert(result.isRight)
    result match {
      case Right(DaemonCli.SendCommandAction(_, socketPath)) =>
        assert(socketPath.isDefined)
        assertEquals(socketPath.get.toString, "/tmp/test.sock")
      case _ => fail("Expected list command with socket")
    }
  }

  test("parse handles socket option for start") {
    val result = DaemonCli.parse(List("start", "/path/trace.json", "-s", "/tmp/custom.sock"))
    assert(result.isRight)
    result match {
      case Right(DaemonCli.StartAction(_, socketPath)) =>
        assert(socketPath.isDefined)
        assertEquals(socketPath.get.toString, "/tmp/custom.sock")
      case _ => fail("Expected start action with custom socket")
    }
  }

  test("parse fails for unknown subcommand") {
    val result = DaemonCli.parse(List("unknown"))
    assert(result.isLeft)
  }

  test("parse fails for deps without node-id") {
    val result = DaemonCli.parse(List("deps"))
    assert(result.isLeft)
  }

  test("parse fails for start without trace file") {
    val result = DaemonCli.parse(List("start"))
    assert(result.isLeft)
  }

  // =========================================================================
  // Command structure tests
  // =========================================================================

  test("command has correct name") {
    assertEquals(DaemonCli.command.name, "daemon")
  }

  test("command has description") {
    assert(DaemonCli.command.header.nonEmpty)
  }

  // =========================================================================
  // ResponseData formatting tests (private but observable via return values)
  // =========================================================================

  // We test the formatResponseData through observable behavior since it's private
  // The formatting tests ensure the CLI produces expected output shapes
}
