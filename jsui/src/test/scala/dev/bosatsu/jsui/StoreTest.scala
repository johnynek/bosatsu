package dev.bosatsu.jsui

import cats.data.Chain
import dev.bosatsu.tool.Output

class StoreTest extends munit.FunSuite {
  private val webDemoPath = Chain("root", "WebDemo")
  private val webDemoSource =
    """main = add(40, 2)
      |
      |test = Assertion(main.eq_Int(42), "main computes 42")
      |""".stripMargin

  private val webDemoFiles = Map(webDemoPath -> webDemoSource)

  private def runWith(args: List[String]): Either[Throwable, Output[Chain[String]]] =
    Store.memoryMain.runWith(files = webDemoFiles)(args)

  test("legacy root test argv fails with parse/help output") {
    val legacyArgs = List(
      "test",
      "--input",
      "root/WebDemo",
      "--package_root",
      "root",
      "--test_file",
      "root/WebDemo",
      "--color",
      "html"
    )

    runWith(legacyArgs) match {
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("Unexpected argument: test"), msg)
        assert(msg.contains("Subcommands:"), msg)
        assert(msg.contains("tool"), msg)
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
      case Output.TestOutput(_, _) => ()
      case other =>
        fail(s"expected Output.TestOutput, got: $other")
    }
  }

  test("cmdHandler show args return ShowOutput") {
    assertCommandOutput(Action.Cmd.Show) {
      case Output.ShowOutput(_, _, _) => ()
      case other =>
        fail(s"expected Output.ShowOutput, got: $other")
    }
  }
}
