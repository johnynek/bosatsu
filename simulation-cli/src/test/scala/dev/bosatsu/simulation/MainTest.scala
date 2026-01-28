package dev.bosatsu.simulation

import munit.FunSuite
import cats.effect.unsafe.implicits.global
import java.nio.file.Files

class MainTest extends FunSuite {

  // Helper to create both input and config files for a test
  private def withTempFiles(
      inputContent: String,
      configContent: String
  )(test: (java.nio.file.Path, java.nio.file.Path, java.nio.file.Path) => Unit): Unit = {
    val tmpInput = Files.createTempFile("test_input", ".bosatsu")
    val tmpConfig = Files.createTempFile("test_config", ".sim.bosatsu")
    val tmpOutput = Files.createTempFile("test_output", ".html")

    try {
      Files.writeString(tmpInput, inputContent)
      Files.writeString(tmpConfig, configContent)
      test(tmpInput, tmpConfig, tmpOutput)
    } finally {
      Files.deleteIfExists(tmpInput)
      Files.deleteIfExists(tmpConfig)
      Files.deleteIfExists(tmpOutput)
    }
  }

  // Standard test config that matches the expected SimConfig structure
  // struct SimConfig(name, description, package_name, function_name, inputs, outputs)
  // struct InputConfig(label, default_value, min_value, max_value, step, widget)
  // struct OutputConfig(label, format, primary)
  private val simpleConfig = """package TestConfig

struct InputConfig(label: String, default_value: Int, min_value: Int, max_value: Int, step: Int, widget: String)
struct OutputConfig(label: String, format: String, primary: Bool)
struct SimConfig(
  name: String,
  description: String,
  package_name: String,
  function_name: String,
  inputs: List[(String, InputConfig)],
  outputs: List[(String, OutputConfig)]
)

config = SimConfig(
  "Test Simulation",
  "A simple test simulation",
  "TestSim",
  "compute",
  [("x", InputConfig("Input X", 1, 0, 10, 1, "slider"))],
  [("result", OutputConfig("Result", "number", True))]
)
"""

  // Simple simulation function
  private val simpleSimulation = """package TestSim
from Bosatsu/Predef import add

def compute(x: Int) -> Int:
  add(x, 1)
"""

  test("Main.run returns error code for missing file") {
    val result = Main.run(List("nonexistent.bosatsu", "nonexistent_config.bosatsu")).unsafeRunSync()
    assertEquals(result.code, 1)
  }

  test("Main.run returns error code for no arguments") {
    val result = Main.run(List()).unsafeRunSync()
    assertEquals(result.code, 1)
  }

  test("Main.run returns error code for only one argument") {
    val result = Main.run(List("input.bosatsu")).unsafeRunSync()
    assertEquals(result.code, 1)
  }

  test("Main.run returns error code for invalid option") {
    val result = Main.run(List("--invalid-option")).unsafeRunSync()
    assertEquals(result.code, 1)
  }

  test("Main.run processes valid bosatsu file with config") {
    withTempFiles(simpleSimulation, simpleConfig) { (tmpInput, tmpConfig, tmpOutput) =>
      val result = Main.run(List(
        tmpInput.toString,
        tmpConfig.toString,
        "-o", tmpOutput.toString
      )).unsafeRunSync()

      assertEquals(result.code, 0)
      assert(Files.exists(tmpOutput))
      val content = Files.readString(tmpOutput)
      assert(content.contains("<!DOCTYPE html>"))
    }
  }

  test("Main.run handles title option") {
    withTempFiles(simpleSimulation, simpleConfig) { (tmpInput, tmpConfig, tmpOutput) =>
      val result = Main.run(List(
        tmpInput.toString,
        tmpConfig.toString,
        "-o", tmpOutput.toString,
        "--title", "Custom Title"
      )).unsafeRunSync()

      assertEquals(result.code, 0)
      val content = Files.readString(tmpOutput)
      assert(content.contains("<title>Custom Title</title>"))
    }
  }

  test("Main.run handles dark theme option") {
    withTempFiles(simpleSimulation, simpleConfig) { (tmpInput, tmpConfig, tmpOutput) =>
      val result = Main.run(List(
        tmpInput.toString,
        tmpConfig.toString,
        "-o", tmpOutput.toString,
        "--theme", "dark"
      )).unsafeRunSync()

      assertEquals(result.code, 0)
      val content = Files.readString(tmpOutput)
      assert(content.contains("data-theme=\"dark\""))
    }
  }

  test("Main.run handles --no-why flag") {
    withTempFiles(simpleSimulation, simpleConfig) { (tmpInput, tmpConfig, tmpOutput) =>
      val result = Main.run(List(
        tmpInput.toString,
        tmpConfig.toString,
        "-o", tmpOutput.toString,
        "--no-why"
      )).unsafeRunSync()

      assertEquals(result.code, 0)
    }
  }

  test("Main.run handles --sweeps flag") {
    withTempFiles(simpleSimulation, simpleConfig) { (tmpInput, tmpConfig, tmpOutput) =>
      val result = Main.run(List(
        tmpInput.toString,
        tmpConfig.toString,
        "-o", tmpOutput.toString,
        "--sweeps"
      )).unsafeRunSync()

      assertEquals(result.code, 0)
    }
  }

  test("Main.run handles --canvas flag") {
    withTempFiles(simpleSimulation, simpleConfig) { (tmpInput, tmpConfig, tmpOutput) =>
      val result = Main.run(List(
        tmpInput.toString,
        tmpConfig.toString,
        "-o", tmpOutput.toString,
        "--canvas"
      )).unsafeRunSync()

      assertEquals(result.code, 0)
    }
  }
}
