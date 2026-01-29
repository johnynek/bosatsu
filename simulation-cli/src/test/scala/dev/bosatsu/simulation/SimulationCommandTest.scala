package dev.bosatsu.simulation

import munit.FunSuite
import java.nio.file.{Files, Paths}

class SimulationCommandTest extends FunSuite {

  test("Theme.fromString parses light") {
    assertEquals(SimulationCommand.Theme.fromString("light"), SimulationCommand.Theme.Light)
  }

  test("Theme.fromString parses dark") {
    assertEquals(SimulationCommand.Theme.fromString("dark"), SimulationCommand.Theme.Dark)
  }

  test("Theme.fromString parses Dark (case insensitive)") {
    assertEquals(SimulationCommand.Theme.fromString("Dark"), SimulationCommand.Theme.Dark)
    assertEquals(SimulationCommand.Theme.fromString("DARK"), SimulationCommand.Theme.Dark)
  }

  test("Theme.fromString defaults to light for unknown") {
    assertEquals(SimulationCommand.Theme.fromString("unknown"), SimulationCommand.Theme.Light)
    assertEquals(SimulationCommand.Theme.fromString(""), SimulationCommand.Theme.Light)
  }

  test("parse succeeds with minimal args") {
    // Requires both input and config arguments
    val result = SimulationCommand.parse(List("input.bosatsu", "config.sim.bosatsu"))
    assert(result.isRight)
    val cmd = result.toOption.get
    assertEquals(cmd.input.toString, "input.bosatsu")
    assertEquals(cmd.config.toString, "config.sim.bosatsu")
    assertEquals(cmd.output.toString, "output.html")
    assertEquals(cmd.title, None)
    assertEquals(cmd.theme, SimulationCommand.Theme.Light)
    assertEquals(cmd.showWhy, true)
    assertEquals(cmd.showWhatIf, true)
    assertEquals(cmd.showSweeps, false)
    assertEquals(cmd.showCanvas, false)
  }

  test("parse handles --output option") {
    val result = SimulationCommand.parse(List("input.bosatsu", "config.sim.bosatsu", "-o", "custom.html"))
    assert(result.isRight)
    assertEquals(result.toOption.get.output.toString, "custom.html")
  }

  test("parse handles --title option") {
    val result = SimulationCommand.parse(List("input.bosatsu", "config.sim.bosatsu", "--title", "My Title"))
    assert(result.isRight)
    assertEquals(result.toOption.get.title, Some("My Title"))
  }

  test("parse handles --theme option") {
    val result = SimulationCommand.parse(List("input.bosatsu", "config.sim.bosatsu", "--theme", "dark"))
    assert(result.isRight)
    assertEquals(result.toOption.get.theme, SimulationCommand.Theme.Dark)
  }

  test("parse handles --no-why flag") {
    val result = SimulationCommand.parse(List("input.bosatsu", "config.sim.bosatsu", "--no-why"))
    assert(result.isRight)
    assertEquals(result.toOption.get.showWhy, false)
  }

  test("parse handles --no-what-if flag") {
    val result = SimulationCommand.parse(List("input.bosatsu", "config.sim.bosatsu", "--no-what-if"))
    assert(result.isRight)
    assertEquals(result.toOption.get.showWhatIf, false)
  }

  test("parse handles --sweeps flag") {
    val result = SimulationCommand.parse(List("input.bosatsu", "config.sim.bosatsu", "--sweeps"))
    assert(result.isRight)
    assertEquals(result.toOption.get.showSweeps, true)
  }

  test("parse handles --canvas flag") {
    val result = SimulationCommand.parse(List("input.bosatsu", "config.sim.bosatsu", "--canvas"))
    assert(result.isRight)
    assertEquals(result.toOption.get.showCanvas, true)
  }

  test("parse handles multiple options") {
    val result = SimulationCommand.parse(List(
      "input.bosatsu",
      "config.sim.bosatsu",
      "-o", "out.html",
      "-t", "Title",
      "--theme", "dark",
      "--no-why",
      "--no-what-if",
      "--sweeps",
      "--canvas"
    ))
    assert(result.isRight)
    val cmd = result.toOption.get
    assertEquals(cmd.input.toString, "input.bosatsu")
    assertEquals(cmd.config.toString, "config.sim.bosatsu")
    assertEquals(cmd.output.toString, "out.html")
    assertEquals(cmd.title, Some("Title"))
    assertEquals(cmd.theme, SimulationCommand.Theme.Dark)
    assertEquals(cmd.showWhy, false)
    assertEquals(cmd.showWhatIf, false)
    assertEquals(cmd.showSweeps, true)
    assertEquals(cmd.showCanvas, true)
  }

  test("parse fails without input") {
    val result = SimulationCommand.parse(List())
    assert(result.isLeft)
  }

  test("parse fails without config") {
    // Must have both input and config
    val result = SimulationCommand.parse(List("input.bosatsu"))
    assert(result.isLeft)
  }

  test("pathArgument reads valid path") {
    val arg = SimulationCommand.pathArgument
    val result = arg.read("/some/path/file.txt")
    assert(result.isValid)
    assertEquals(result.toOption.get.toString, "/some/path/file.txt")
  }

  test("pathArgument has correct metavar") {
    assertEquals(SimulationCommand.pathArgument.defaultMetavar, "path")
  }

  test("command has correct name and description") {
    assertEquals(SimulationCommand.command.name, "bosatsu-sim")
    assert(SimulationCommand.command.header.contains("interactive simulation HTML"))
  }
}
