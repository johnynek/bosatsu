package dev.bosatsu

import java.nio.file.{Files, Path}
import java.util.Comparator
import dev.bosatsu.tool.{ExitCode => ToolExitCode, Output}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

// allow us to unsafeRunSync
import cats.effect.unsafe.implicits.global

class PathModuleTest extends munit.FunSuite {
  override def munitTimeout: Duration = 6.minutes

  private def writeFile(path: Path, content: String): Unit = {
    Files.createDirectories(path.getParent)
    Files.writeString(path, content): Unit
  }

  private def deleteRecursively(path: Path): Unit = {
    val walk = Files.walk(path)
    try {
      walk.sorted(Comparator.reverseOrder()).forEach { p =>
        Files.deleteIfExists(p)
        ()
      }
    } finally walk.close()
  }

  private def withLibraryRepo[A](fn: Path => A): A = {
    val repo = Files.createTempDirectory("bosatsu-path-module-")
    try {
      Files.createDirectories(repo.resolve(".git"))
      val initExit =
        runAndReport(
          "init",
          "--repo_root",
          repo.toString,
        "--name",
        "mylib",
        "--repo_uri",
        "https://example.com/mylib",
          "--src_root",
          "src",
          "--version",
          "0.0.1"
        )
      assertEquals(initExit, ToolExitCode.Success)

      writeFile(
        repo.resolve("src/Bosatsu/Prog.bosatsu"),
        """package Bosatsu/Prog
          |
          |export Main(), Prog(), pure
          |
          |enum Prog[e, a]:
          |  Pure(value: a)
          |  Raise(err: e)
          |
          |struct Main(run: List[String] -> Prog[String, Int])
          |
          |def pure(a):
          |  Pure(a)
          |""".stripMargin
      )
      writeFile(
        repo.resolve("src/MyLib/Foo.bosatsu"),
        """package MyLib/Foo
          |
          |from Bosatsu/Prog import Main, pure
          |
          |export main, runner
          |exposes Bosatsu/Prog
          |
          |main = 42
          |
          |runner = Main(args -> match args:
          |  case [_, "--compact"]:
          |    pure(0)
          |  case _:
          |    pure(1)
          |)
          |""".stripMargin
      )

      fn(repo)
    } finally deleteRecursively(repo)
  }

  def run(args: String*): Output[Path] =
    PathModule.run(args.toList) match {
      case Left(h)   => fail(s"got help: $h on command: ${args.toList}")
      case Right(io) =>
        io.attempt.unsafeRunSync() match {
          case Right(out) =>
            out
          case Left(err)  =>
            fail(s"${err.getMessage}\ncommand: ${args.toList.mkString(" ")}")
        }
    }

  def runAndReport(args: String*): ToolExitCode =
    PathModule.runAndReport(args.toList) match {
      case Left(h)   => fail(s"got help: $h on command: ${args.toList}")
      case Right(io) =>
        io.attempt.unsafeRunSync() match {
          case Right(code) =>
            code
          case Left(err)   =>
            fail(s"${err.getMessage}\ncommand: ${args.toList.mkString(" ")}")
        }
    }

  private def helpText(args: String*): String =
    PathModule.run(args.toList) match {
      case Left(help) => help.toString
      case Right(_)   =>
        fail(s"expected help output for command: ${args.toList.mkString(" ")}")
    }

  private def subcommands(helpText: String): List[String] =
    helpText.linesIterator
      .dropWhile(_ != "Subcommands:")
      .drop(1)
      .collect { case line if line.startsWith("    ") && !line.startsWith("        ") =>
        line.trim
      }
      .toList

  test("root help lists top-level commands in the intended order") {
    val msg = helpText("--help")
    assertEquals(
      subcommands(msg),
      List(
        "check",
        "test",
        "build",
        "json",
        "doc",
        "publish",
        "eval",
        "fetch",
        "deps",
        "list",
        "show",
        "assemble",
        "init",
        "tool",
        "version",
        "c-runtime"
      )
    )
    assert(!msg.contains("\n  lib"), msg)
  }

  test("tool help lists subcommands in the intended order") {
    assertEquals(
      subcommands(helpText("tool", "--help")),
      List(
        "check",
        "test",
        "transpile",
        "json",
        "doc",
        "eval",
        "deps",
        "show",
        "assemble",
        "extract-iface"
      )
    )
  }

  test("top-level library commands use repo mode") {
    withLibraryRepo { repo =>
      assertEquals(
        runAndReport("check", "--repo_root", repo.toString),
        ToolExitCode.Success
      )

      run(
        "json",
        "write",
        "--repo_root",
        repo.toString,
        "--main",
        "MyLib/Foo::main"
      ) match {
        case Output.JsonOutput(Json.JNumberStr("42"), _) => ()
        case other                                       =>
          fail(s"expected json output, got: $other")
      }

      run(
        "show",
        "--repo_root",
        repo.toString,
        "--package",
        "MyLib/Foo"
      ) match {
        case Output.ShowOutput(Output.ShowValue.Typed(packs, _, _, _), _) =>
          assertEquals(packs.map(_.name.asString), List("MyLib/Foo"))
        case other =>
          fail(s"expected show output, got: $other")
      }
    }
  }

  test("top-level eval --run accepts delimiter-separated args") {
    withLibraryRepo { repo =>
      assertEquals(
        runAndReport(
          "eval",
          "--repo_root",
          repo.toString,
          "--main",
          "MyLib/Foo::runner",
          "--run",
          "--",
          "--compact"
        ),
        ToolExitCode.Success
      )
    }
  }

  test("tool subcommands reject removed package-root options") {
    val badCommands = List(
      "tool check --package_root test_workspace --input test_workspace/Foo.bosatsu",
      "tool test --search --test_file test_workspace/Bar.bosatsu"
    )

    badCommands.foreach { command =>
      PathModule.run(command.split("\\s+").toList) match {
        case Left(help) =>
          val msg = help.toString
          assert(
            msg.contains("Unexpected option") || msg.contains("Unexpected argument"),
            msg
          )
        case Right(_)   =>
          fail(s"expected parse failure for command: $command")
      }
    }
  }

  test("tool test direct run of a file") {
    val deps = List("Nat", "List", "Bool", "Int64", "Rand", "Properties", "BinNat")
    val inputs =
      deps.map(n => s"--input test_workspace/${n}.bosatsu").mkString(" ")
    val out = run(
      s"tool test $inputs --test_file test_workspace/Queue.bosatsu"
        .split("\\s+")
        .toSeq*
    )
    out match {
      case Output.TestOutput(results, _, _) =>
        val res = results.collect {
          case (pn, Some(t)) if pn.asString == "Bosatsu/Collection/Queue" =>
            t.value
        }
        assertEquals(res.length, 1)
      case other => fail(s"expected test output: $other")
    }
  }

  test("tool test search run of a file") {
    val out = run(
      "tool test --input test_workspace/Foo.bosatsu --test_file test_workspace/Bar.bosatsu"
        .split("\\s+")
        .toSeq*
    )
    out match {
      case Output.TestOutput(results, _, _) =>
        val res = results.collect {
          case (pn, Some(t)) if pn.asString == "Bar" => t.value
        }
        assertEquals(res.length, 1)
        assertEquals(res.head.assertions, 1)
        assertEquals(res.head.failureCount, 0)
      case other => fail(s"expected test output: $other")
    }
  }

  test("tool eval --run executes Bosatsu/FibBench::main") {
    val cmd =
      "tool eval --run --main Bosatsu/FibBench::main --input_dir test_workspace --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu 20"
        .split("\\s+")
        .toSeq
    val exitCode = runAndReport(cmd*)
    assertEquals(exitCode, ToolExitCode.Success)
  }

  test("tool eval --run accepts delimiter-separated args") {
    val cmd =
      "tool eval --run --main Bosatsu/FibBench::main --input_dir test_workspace --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu -- 20"
        .split("\\s+")
        .toSeq
    val exitCode = runAndReport(cmd*)
    assertEquals(exitCode, ToolExitCode.Success)
  }

  test("tool eval delimiter args without --run return trailing args error") {
    val cmd =
      "tool eval --main Bosatsu/FibBench::main --input_dir test_workspace --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu -- --compact"
        .split("\\s+")
        .toList

    PathModule.run(cmd) match {
      case Left(help) =>
        fail(s"got help: $help on command: ${cmd.mkString(" ")}")
      case Right(io)  =>
        io.attempt.unsafeRunSync() match {
          case Left(err) =>
            val msg = Option(err.getMessage).getOrElse(err.toString)
            assert(msg.contains("trailing args require --run"), msg)
          case Right(out) =>
            fail(s"expected trailing-args failure, got output: $out")
        }
    }
  }

  test("tool test python transpile on the entire test_workspace") {
    val out = run(
      "tool transpile --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu python --outdir pyout --externals test_workspace/Prog.bosatsu_externals --evaluators test_workspace/Prog.bosatsu_eval"
        .split("\\s+")
        .toSeq*
    )
    out match {
      case Output.TranspileOut(_) =>
        assert(true)
      case other => fail(s"expected transpile output: $other")
    }
  }

  test("tool test search with json write") {

    val out = run(
      "tool json write --input test_workspace/Foo.bosatsu --main_file test_workspace/Bar.bosatsu"
        .split("\\s+")
        .toSeq*
    )
    out match {
      case Output.JsonOutput(j @ Json.JObject(_), _) =>
        assertEquals(
          j.toMap,
          Map(
            "value" -> Json.JBool(true),
            "message" -> Json.JString("got the right string")
          )
        )
        assertEquals(j.items.length, 2)
      case other => fail(s"expected json object output: $other")
    }
  }

  test("tool test search with json write --yaml") {
    val out = run(
      "tool json write --input test_workspace/Foo.bosatsu --main_file test_workspace/Bar.bosatsu --yaml"
        .split("\\s+")
        .toSeq*
    )
    out match {
      case Output.Basic(doc, _) =>
        val rendered = doc.render(120)
        assert(rendered.contains("value: true"), rendered)
        assert(
          rendered.contains("message: \"got the right string\""),
          rendered
        )
      case other => fail(s"expected yaml output: $other")
    }
  }

  test("tool test search json apply") {
    val cmd =
      "tool json apply --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu --main Bosatsu/Num/Nat::mult --json_string"
        .split("\\s+")
        .toList :+ "[2, 4]"

    run(cmd*) match {
      case Output.JsonOutput(Json.JNumberStr("8"), _) => ()
      case other => fail(s"expected json object output: $other")
    }
  }

  test("tool test search json traverse") {
    val cmd =
      "tool json traverse --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu --main Bosatsu/Num/Nat::mult --json_string"
        .split("\\s+")
        .toList :+ "[[2, 4], [3, 5]]"

    run(cmd*) match {
      case Output.JsonOutput(
            Json.JArray(Vector(Json.JNumberStr("8"), Json.JNumberStr("15"))),
            _
          ) =>
        ()
      case other => fail(s"expected json object output: $other")
    }
  }

  test("error coverage on json command") {
    def fails(str: String, suffix: String*) =
      PathModule.run(str.split("\\s+").toList ::: suffix.toList) match {
        case Left(h)   => fail(s"got help: $h, expected a non-help command")
        case Right(io) =>
          Try(io.unsafeRunSync()) match {
            case Success(s) => fail(s"got Success($s) expected to fail")
            case Failure(_) => ()
          }
      }

    // ill-typed json fails
    val cmd =
      "tool json apply --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu --main Bosatsu/Num/Nat::mult --json_string"
    fails(cmd, "[\"2\", 4]")
    fails(cmd, "[2, \"4\"]")
    // wrong arity
    fails(cmd, "[2, 4, 3]")
    fails(cmd, "[2]")
    fails(cmd, "[]")
    // unknown command fails
    val badName =
      "tool json apply --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu --main Bosatsu/Num/Nat::foooooo --json_string 23"
    fails(badName)
    val badPack =
      "tool json apply --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu --main Bosatsu/DoesNotExist --json_string 23"
    fails(badPack)
    // bad json fails
    fails(cmd, "[\"2\", foo, bla]")
    fails(cmd, "[42, 31] and some junk")
    // exercise unsupported, we cannot write mult, it is a function
    fails(
      "tool json write --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu --main Bosatsu/Num/Nat::mult"
    )
    // a bad main name triggers help
    PathModule.run(
      "tool json write --input_dir test_workspace --main Bo//".split(' ').toList
    ) match {
      case Left(_)  => ()
      case Right(_) => fail("expected invalid main name to fail")
    }
    PathModule.run(
      "tool json write --input_dir test_workspace --main Bo:::boop"
        .split(' ')
        .toList
    ) match {
      case Left(_)  => ()
      case Right(_) => fail("expected invalid main name to fail")
    }
  }

  test("tool test running all test in test_workspace") {

    val out = run(
      "tool test --input_dir test_workspace --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu"
        .split("\\s+")
        .toSeq*
    )
    out match {
      case Output.TestOutput(res, _, _) =>
        val noTests = res.collect { case (pn, None) => pn }.toList
        assertEquals(noTests, Nil)
        val failures = res.collect {
          case (pn, Some(t)) if t.value.failureCount > 0 => pn
        }
        assertEquals(failures, Nil)
      case other => fail(s"expected test output: $other")
    }
  }

  test("evaluation by name with shadowing") {
    run(
      "tool json write --input test_workspace/Foo.bosatsu --main Foo::x"
        .split("\\s+")
        .toSeq*
    ) match {
      case Output.JsonOutput(Json.JString("this is Foo"), _) =>
        ()
      case other => fail(s"unexpeced: $other")
    }
  }
}
