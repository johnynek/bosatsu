package dev.bosatsu

import cats.data.Chain
import cats.implicits._
import dev.bosatsu.library.{LibConfig, Libraries, Name, Version}
import dev.bosatsu.tool.Output
import munit.FunSuite
import scala.collection.immutable.SortedMap

class ToolAndLibCommandTest extends FunSuite {
  private type ErrorOr[A] = Either[Throwable, A]
  private val module = MemoryMain[ErrorOr]

  private def renderJson[A: Json.Writer](value: A): String =
    Json.Writer.write(value).render

  private def runWithState(
      cmd: List[String],
      state: MemoryMain.State
  ): ErrorOr[(MemoryMain.State, Output[Chain[String]])] =
    module.run(cmd) match {
      case Left(help) =>
        Left(new Exception(s"got help: $help on command: $cmd"))
      case Right(io)  =>
        for {
          stateOut <- io.run(state)
          (nextState, _) <- module.reportOutput(stateOut._2).run(stateOut._1)
        } yield (nextState, stateOut._2)
    }

  private def baseLibFiles(mainSrc: String): List[(Chain[String], String)] = {
    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    List(
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      Chain("repo", "src", "MyLib", "Foo.bosatsu") -> mainSrc
    )
  }

  test("root eval command moved under tool") {
    module.run(List("eval", "--main", "MyLib/Foo")) match {
      case Left(_)  => ()
      case Right(_) => fail("expected parse failure without `tool` prefix")
    }
  }

  test("tool assemble and tool extract-iface") {
    val src =
      """main = 1
"""
    val files = List(Chain("src", "Tool", "Foo.bosatsu") -> src)

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
          "--package_root",
          "src",
          "--input",
          "src/Tool/Foo.bosatsu",
          "--output",
          "out/Tool.Foo.bosatsu_package"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runWithState(
        List(
          "tool",
          "assemble",
          "--name",
          "tooltest",
          "--version",
          "0.0.1",
          "--package",
          "out/Tool.Foo.bosatsu_package",
          "--output",
          "out/tooltest.bosatsu_lib"
        ),
        state1
      )
      (state2, out2) = s2
      _ = out2 match {
        case Output.Library(_, _) => ()
        case other                => fail(s"unexpected output: $other")
      }
      s3 <- runWithState(
        List(
          "tool",
          "extract-iface",
          "--input",
          "out/tooltest.bosatsu_lib",
          "--output",
          "out/tooltest.bosatsu_ifacelib"
        ),
        state2
      )
    } yield s3

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, out3)) =>
        out3 match {
          case Output.Library(lib, outPath) =>
            assertEquals(outPath, Chain("out", "tooltest.bosatsu_ifacelib"))
            assertEquals(lib.internalPackages.toList, Nil)
          case other =>
            fail(s"unexpected output: $other")
        }
    }
  }

  test("lib eval/json/show use library context") {
    val src =
      """main = 42
"""
    val files = baseLibFiles(src)

    module.runWith(files)(List("lib", "eval", "--repo_root", "repo", "--main",
      "MyLib/Foo")) match {
      case Right(Output.EvaluationResult(_, _, _)) => ()
      case Right(other)                            => fail(s"unexpected output: $other")
      case Left(err)                               => fail(err.getMessage)
    }

    module.runWith(files)(
      List("lib", "json", "write", "--repo_root", "repo", "--main",
        "MyLib/Foo")
    ) match {
      case Right(Output.JsonOutput(Json.JNumberStr("42"), _)) => ()
      case Right(other)                                        => fail(s"unexpected output: $other")
      case Left(err)                                           => fail(err.getMessage)
    }

    module.runWith(files)(
      List("lib", "show", "--repo_root", "repo", "--package", "MyLib/Foo")
    ) match {
      case Right(Output.ShowOutput(packs, _, _)) =>
        assertEquals(packs.map(_.name.asString), List("MyLib/Foo"))
      case Right(other)                          => fail(s"unexpected output: $other")
      case Left(err)                             => fail(err.getMessage)
    }
  }

  test("tool commands can evaluate and show packages from --pub_dep libraries") {
    val depSrc =
      """main = 9
"""
    val files = List(Chain("dep", "Dep", "Foo.bosatsu") -> depSrc)

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
          "--package_root",
          "dep",
          "--input",
          "dep/Dep/Foo.bosatsu",
          "--output",
          "out/Dep.Foo.bosatsu_package"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runWithState(
        List(
          "tool",
          "assemble",
          "--name",
          "dep",
          "--version",
          "0.0.1",
          "--package",
          "out/Dep.Foo.bosatsu_package",
          "--output",
          "out/dep.bosatsu_lib"
        ),
        state1
      )
      (state2, _) = s2
      s3 <- runWithState(
        List(
          "tool",
          "json",
          "write",
          "--pub_dep",
          "out/dep.bosatsu_lib",
          "--main",
          "Dep/Foo"
        ),
        state2
      )
      (state3, outJson) = s3
      s4 <- runWithState(
        List("tool", "show", "--pub_dep", "out/dep.bosatsu_lib"),
        state3
      )
      (_, outShow) = s4
    } yield (outJson, outShow)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((jsonOut, showOut)) =>
        jsonOut match {
          case Output.JsonOutput(Json.JNumberStr("9"), _) => ()
          case other                                      => fail(s"unexpected json output: $other")
        }
        showOut match {
          case Output.ShowOutput(packs, _, _) =>
            assertEquals(packs.map(_.name.asString), List("Dep/Foo"))
          case other                         =>
            fail(s"unexpected show output: $other")
        }
    }
  }
}
