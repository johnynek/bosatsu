package dev.bosatsu

import java.nio.file.Path
import dev.bosatsu.tool.{ExitCode => ToolExitCode, Output}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

// allow us to unsafeRunSync
import cats.effect.unsafe.implicits.global

class PathModuleTest extends munit.FunSuite {
  override def munitTimeout: Duration = 6.minutes

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
    val deps = List("Nat", "List", "Bool", "Rand", "Properties", "BinNat")
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
