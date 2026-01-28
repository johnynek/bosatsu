package dev.bosatsu.simulation

import munit.FunSuite
import cats.effect.unsafe.implicits.global
import java.nio.file.Files

class MainTest extends FunSuite {

  test("Main.run returns error code for missing file") {
    val result = Main.run(List("nonexistent.bosatsu")).unsafeRunSync()
    assertEquals(result.code, 1)
  }

  test("Main.run returns error code for no arguments") {
    val result = Main.run(List()).unsafeRunSync()
    assertEquals(result.code, 1)
  }

  test("Main.run returns error code for invalid option") {
    val result = Main.run(List("--invalid-option")).unsafeRunSync()
    assertEquals(result.code, 1)
  }

  test("Main.run processes valid bosatsu file") {
    // Create a temporary bosatsu file
    val tmpInput = Files.createTempFile("test", ".bosatsu")
    val tmpOutput = Files.createTempFile("test", ".html")

    try {
      Files.writeString(tmpInput, """package Test

x = 42
""")

      val result = Main.run(List(tmpInput.toString, "-o", tmpOutput.toString)).unsafeRunSync()

      assertEquals(result.code, 0)
      assert(Files.exists(tmpOutput))
      val content = Files.readString(tmpOutput)
      assert(content.contains("<!DOCTYPE html>"))
    } finally {
      Files.deleteIfExists(tmpInput)
      Files.deleteIfExists(tmpOutput)
    }
  }

  test("Main.run with computation expressions") {
    val tmpInput = Files.createTempFile("test_comp", ".bosatsu")
    val tmpOutput = Files.createTempFile("test_comp", ".html")

    try {
      Files.writeString(tmpInput, """package TestComp

a = 100
b = 20
c = a.add(b)
""")

      val result = Main.run(List(tmpInput.toString, "-o", tmpOutput.toString)).unsafeRunSync()

      assertEquals(result.code, 0)
      val content = Files.readString(tmpOutput)
      assert(content.contains("<!DOCTYPE html>"))
      assert(content.contains("_derivations"))
    } finally {
      Files.deleteIfExists(tmpInput)
      Files.deleteIfExists(tmpOutput)
    }
  }

  test("Main.run handles title option") {
    val tmpInput = Files.createTempFile("test_title", ".bosatsu")
    val tmpOutput = Files.createTempFile("test_title", ".html")

    try {
      Files.writeString(tmpInput, """package TestTitle

x = 1
""")

      val result = Main.run(List(
        tmpInput.toString,
        "-o", tmpOutput.toString,
        "--title", "Custom Title"
      )).unsafeRunSync()

      assertEquals(result.code, 0)
      val content = Files.readString(tmpOutput)
      assert(content.contains("<title>Custom Title</title>"))
    } finally {
      Files.deleteIfExists(tmpInput)
      Files.deleteIfExists(tmpOutput)
    }
  }

  test("Main.run handles dark theme option") {
    val tmpInput = Files.createTempFile("test_theme", ".bosatsu")
    val tmpOutput = Files.createTempFile("test_theme", ".html")

    try {
      Files.writeString(tmpInput, """package TestTheme

x = 1
""")

      val result = Main.run(List(
        tmpInput.toString,
        "-o", tmpOutput.toString,
        "--theme", "dark"
      )).unsafeRunSync()

      assertEquals(result.code, 0)
      val content = Files.readString(tmpOutput)
      assert(content.contains("data-theme=\"dark\""))
    } finally {
      Files.deleteIfExists(tmpInput)
      Files.deleteIfExists(tmpOutput)
    }
  }

  test("Main.run handles --no-why flag") {
    val tmpInput = Files.createTempFile("test_nowhy", ".bosatsu")
    val tmpOutput = Files.createTempFile("test_nowhy", ".html")

    try {
      Files.writeString(tmpInput, """package TestNoWhy

x = 1
""")

      val result = Main.run(List(
        tmpInput.toString,
        "-o", tmpOutput.toString,
        "--no-why"
      )).unsafeRunSync()

      assertEquals(result.code, 0)
    } finally {
      Files.deleteIfExists(tmpInput)
      Files.deleteIfExists(tmpOutput)
    }
  }

  test("Main.run handles --sweeps flag") {
    val tmpInput = Files.createTempFile("test_sweeps", ".bosatsu")
    val tmpOutput = Files.createTempFile("test_sweeps", ".html")

    try {
      Files.writeString(tmpInput, """package TestSweeps

x = 1
""")

      val result = Main.run(List(
        tmpInput.toString,
        "-o", tmpOutput.toString,
        "--sweeps"
      )).unsafeRunSync()

      assertEquals(result.code, 0)
      val content = Files.readString(tmpOutput)
      assert(content.contains("sweep-slider"))
    } finally {
      Files.deleteIfExists(tmpInput)
      Files.deleteIfExists(tmpOutput)
    }
  }
}
