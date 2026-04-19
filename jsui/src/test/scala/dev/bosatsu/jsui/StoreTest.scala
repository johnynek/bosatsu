package dev.bosatsu.jsui

import cats.data.Chain
import dev.bosatsu.tool.Output

class StoreTest extends munit.FunSuite {
  private val webDemoPath = Chain("root", "WebDemo")
  private val webDemoSource =
    """package WebDemo
      |
      |main = add(40, 2)
      |
      |test = Assertion(main.eq_Int(42), "main computes 42")
      |""".stripMargin

  private val webDemoSourceNoPackage =
    """main = add(40, 2)
      |
      |test = Assertion(main.eq_Int(42), "main computes 42")
      |""".stripMargin

  private val webDemoFiles = Map(webDemoPath -> webDemoSource)

  private def runWith(args: List[String]): Either[Throwable, Output[Chain[String]]] =
    Store.memoryMain.runWith(files = webDemoFiles)(args)

  test("legacy root test argv fails in library-mode parser") {
    val legacyArgs = List(
      "test",
      "--input",
      "root/WebDemo",
      "--test_file",
      "root/WebDemo",
      "--color",
      "html"
    )

    runWith(legacyArgs) match {
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("Unexpected option: --input"), msg)
        assert(msg.contains("Usage: bosatsu test"), msg)
        assert(msg.contains("test packages in a library"), msg)
      case Right(output) =>
        fail(s"expected parse/help failure, got output: $output")
    }
  }

  private def assertCommandOutput(cmd: Action.Cmd)(
      check: Output[Chain[String]] => Unit
  ): Unit = {
    val (args, _) = Store.cmdHandler(cmd)
    assertEquals(args.headOption, Some("tool"))

    runWith(args) match {
      case Right(output) =>
        check(output)
      case Left(err) =>
        fail(Option(err.getMessage).getOrElse(err.toString))
    }
  }

  test("cmdHandler eval args return EvaluationResult") {
    assertCommandOutput(Action.Cmd.Eval) {
      case Output.EvaluationResult(_, _, _) => ()
      case other =>
        fail(s"expected Output.EvaluationResult, got: $other")
    }
  }

  test("cmdHandler test args return TestOutput") {
    assertCommandOutput(Action.Cmd.Test) {
      case _: Output.TestOutput => ()
      case other =>
        fail(s"expected Output.TestOutput, got: $other")
    }
  }

  test("cmdHandler show args return ShowOutput") {
    assertCommandOutput(Action.Cmd.Show) {
      case Output.ShowOutput(_, _) => ()
      case other =>
        fail(s"expected Output.ShowOutput, got: $other")
    }
  }

  test("withWebDemoPackage injects package header when absent") {
    val parsed = Store.withWebDemoPackage(webDemoSourceNoPackage)
    assert(parsed.startsWith("package WebDemo\n\n"), parsed)
    assert(parsed.endsWith(webDemoSourceNoPackage), parsed)
  }

  test("cmdHandler eval args accept package-less web source") {
    val source = Store.withWebDemoPackage(webDemoSourceNoPackage)
    val (args, _) = Store.cmdHandler(Action.Cmd.Eval)
    Store.memoryMain.runWith(files = Map(webDemoPath -> source))(args) match {
      case Right(Output.EvaluationResult(_, _, _)) => ()
      case Right(other) =>
        fail(s"expected Output.EvaluationResult, got: $other")
      case Left(err) =>
        fail(Option(err.getMessage).getOrElse(err.toString))
    }
  }

  test("evaluate error renders source paths with slash form") {
    val reproSource =
      """package Repro/Issue1
        |
        |test = Assertion(int_to_String(42) matches str, "msg")
        |""".stripMargin

    val (args, _) = Store.cmdHandler(Action.Cmd.Eval)
    Store.memoryMain.runWith(files = Map(webDemoPath -> reproSource))(args) match {
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("in file: root/WebDemo"), msg)
        assert(!msg.contains("Chain(root, WebDemo)"), msg)
      case Right(output) =>
        fail(s"expected evaluate to fail with type errors, got: $output")
    }
  }
}
