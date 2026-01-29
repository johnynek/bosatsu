package dev.bosatsu

import munit.FunSuite
import cats.effect.unsafe.implicits.global

class MainTest extends FunSuite {

  // =========================================================================
  // fromToolExit tests
  // =========================================================================

  test("fromToolExit converts Success correctly") {
    assertEquals(Main.fromToolExit(tool.ExitCode.Success), cats.effect.ExitCode.Success)
  }

  test("fromToolExit converts Error correctly") {
    assertEquals(Main.fromToolExit(tool.ExitCode.Error), cats.effect.ExitCode.Error)
  }

  // =========================================================================
  // run tests - error cases
  // =========================================================================

  test("run with daemon subcommand and no args returns error") {
    val result = Main.run(List("daemon")).unsafeRunSync()
    assertEquals(result, cats.effect.ExitCode.Error)
  }

  test("run with invalid command returns error") {
    val result = Main.run(List("--invalid-option")).unsafeRunSync()
    assertEquals(result, cats.effect.ExitCode.Error)
  }

  test("run with no args shows help (error code)") {
    // Running with no args typically shows help and returns error
    val result = Main.run(List()).unsafeRunSync()
    assertEquals(result, cats.effect.ExitCode.Error)
  }

  // =========================================================================
  // run tests - daemon routing
  // =========================================================================

  test("run routes daemon subcommand correctly") {
    // The daemon subcommand should be handled by DaemonCli
    // Without proper setup, it will return an error, but the routing should work
    val result = Main.run(List("daemon", "status")).unsafeRunSync()
    // Status without a running daemon returns error
    assertEquals(result, cats.effect.ExitCode.Error)
  }

  test("run routes daemon list subcommand") {
    val result = Main.run(List("daemon", "list")).unsafeRunSync()
    // List without a running daemon returns error
    assertEquals(result, cats.effect.ExitCode.Error)
  }
}
