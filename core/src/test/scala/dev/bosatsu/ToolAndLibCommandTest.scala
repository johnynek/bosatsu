package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.Eval
import cats.data.Chain
import cats.implicits._
import dev.bosatsu.hashing.{Algo, Hashed}
import dev.bosatsu.library.{LibConfig, Libraries, Name, Version}
import dev.bosatsu.LocationMap.Colorize
import dev.bosatsu.tool.{ExitCode, GraphOutput, Output}
import munit.FunSuite
import org.typelevel.paiges.Doc
import scala.collection.immutable.SortedMap

class ToolAndLibCommandTest extends FunSuite {
  private type ErrorOr[A] = Either[Throwable, A]
  private val module = MemoryMain[ErrorOr]
  private val validHash1 = "blake3:" + ("1" * 64)
  private val validHash2 = "blake3:" + ("2" * 64)
  private val validHash3 = "blake3:" + ("3" * 64)

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

  private def readStringFile(
      state: MemoryMain.State,
      path: Chain[String]
  ): String =
    state.get(path) match {
      case Some(Right(MemoryMain.FileContent.Str(s))) => s
      case other =>
        fail(s"expected string file at ${path.mkString_("/")}, found: $other")
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

  private def baseLibFilesWithConf(
      mainSrc: String,
      conf: LibConfig
  ): List[(Chain[String], String)] = {
    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    List(
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      Chain("repo", "src", "MyLib", "Foo.bosatsu") -> mainSrc
    )
  }

  private def withInjectedPublicDep(
      state: MemoryMain.State,
      previousLibPath: Chain[String],
      dependencyLibPath: Chain[String],
      outPath: Chain[String]
  ): ErrorOr[MemoryMain.State] =
    (state.get(previousLibPath), state.get(dependencyLibPath)) match {
      case (
            Some(Right(MemoryMain.FileContent.Lib(previousLib))),
            Some(Right(MemoryMain.FileContent.Lib(depLib)))
          ) =>
        val depRef =
          proto.LibDependency(name = depLib.arg.name, desc = depLib.arg.descriptor)
        val rewritten = previousLib.arg.copy(
          publicDependencies = depRef :: previousLib.arg.publicDependencies.toList
        )
        val rehashed = Hashed(Algo.hashBytes(rewritten.toByteArray), rewritten)
        state.withFile(outPath, MemoryMain.FileContent.Lib(rehashed)) match {
          case Some(next) => Right(next)
          case None       =>
            Left(
              new Exception(show"failed to write rewritten library at $outPath")
            )
        }
      case (prev, dep) =>
        Left(
          new Exception(
            show"expected library files at $previousLibPath and $dependencyLibPath, found: ${prev.toString} and ${dep.toString}"
          )
        )
    }

  test("root eval command moved under tool") {
    module.run(List("eval", "--main", "MyLib/Foo")) match {
      case Left(_)  => ()
      case Right(_) => fail("expected parse failure without `tool` prefix")
    }
  }

  test("mainExceptionToString returns None for non-cli exceptions") {
    assertEquals(module.mainExceptionToString(new Exception("boom")), None)
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

  test("tool assemble rejects interface-only dependency libraries") {
    val depSrc =
      """depValue = 9
main = depValue
"""
    val appSrc =
      """main = 1
"""
    val files = List(
      Chain("dep", "Dep", "Foo.bosatsu") -> depSrc,
      Chain("app", "App", "Main.bosatsu") -> appSrc
    )

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
          "extract-iface",
          "--input",
          "out/dep.bosatsu_lib",
          "--output",
          "out/dep.bosatsu_ifacelib"
        ),
        state2
      )
      (state3, _) = s3
      s4 <- runWithState(
        List(
          "tool",
          "check",
          "--package_root",
          "app",
          "--input",
          "app/App/Main.bosatsu",
          "--output",
          "out/App.Main.bosatsu_package"
        ),
        state3
      )
      (state4, _) = s4
    } yield state4

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right(state4) =>
        val privateDepResult = runWithState(
          List(
            "tool",
            "assemble",
            "--name",
            "app",
            "--version",
            "0.0.1",
            "--package",
            "out/App.Main.bosatsu_package",
            "--priv_dep",
            "out/dep.bosatsu_ifacelib",
            "--output",
            "out/app_priv.bosatsu_lib"
          ),
          state4
        )
        privateDepResult match {
          case Right((_, out)) =>
            fail(
              s"expected assemble failure when using .bosatsu_ifacelib private dependency, got: $out"
            )
          case Left(err)       =>
            val msg = Option(err.getMessage).getOrElse(err.toString)
            assert(
              msg.contains("invalid private dependency libraries"),
              s"unexpected private-dep error: $msg"
            )
        }

        val publicDepResult = runWithState(
          List(
            "tool",
            "assemble",
            "--name",
            "app",
            "--version",
            "0.0.1",
            "--package",
            "out/App.Main.bosatsu_package",
            "--pub_dep",
            "out/dep.bosatsu_ifacelib",
            "--output",
            "out/app_pub.bosatsu_lib"
          ),
          state4
        )
        publicDepResult match {
          case Right((_, out)) =>
            fail(
              s"expected assemble failure when using .bosatsu_ifacelib public dependency, got: $out"
            )
          case Left(err)       =>
            val msg = Option(err.getMessage).getOrElse(err.toString)
            assert(
              msg.contains("invalid public dependency libraries"),
              s"unexpected public-dep error: $msg"
            )
        }
    }
  }

  test("tool deps emits sorted json and dot output") {
    val alphaSrc =
      """from Missing/Dep import x
"""
    val zedSrc =
      ""
    val files = List(
      Chain("src", "Zed", "Main.bosatsu") -> zedSrc,
      Chain("src", "Alpha", "Main.bosatsu") -> alphaSrc
    )

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "tool",
          "deps",
          "--package_root",
          "src",
          "--input",
          "src/Zed/Main.bosatsu",
          "--input",
          "src/Alpha/Main.bosatsu",
          "--graph_format",
          "json",
          "--output",
          "out/deps.json"
        ),
        s0
      )
      (state1, jsonOut) = s1
      _ = jsonOut match {
        case Output.DepsOutput(_, Some(path), GraphOutput.Json) =>
          assertEquals(path, Chain("out", "deps.json"))
        case other =>
          fail(s"unexpected json deps output: $other")
      }
      s2 <- runWithState(
        List(
          "tool",
          "deps",
          "--package_root",
          "src",
          "--input",
          "src/Zed/Main.bosatsu",
          "--input",
          "src/Alpha/Main.bosatsu",
          "--graph_format",
          "dot",
          "--output",
          "out/deps.dot"
        ),
        state1
      )
      (state2, dotOut) = s2
      _ = dotOut match {
        case Output.DepsOutput(_, Some(path), GraphOutput.Dot) =>
          assertEquals(path, Chain("out", "deps.dot"))
        case other =>
          fail(s"unexpected dot deps output: $other")
      }
    } yield state2

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right(state) =>
        val jsonStr = readStringFile(state, Chain("out", "deps.json"))
        Json.parserFile.parseAll(jsonStr) match {
          case Right(Json.JArray(items)) =>
            val itemList = items.toList
            assertEquals(itemList.size, 2)

            val paths = itemList.map {
              case Json.JObject(fields) =>
                fields.collectFirst { case ("path", Json.JString(p)) => p } match {
                  case Some(p) => p
                  case None    => fail(s"missing path field in: ${fields}")
                }
              case other =>
                fail(s"expected json object item, found: $other")
            }
            assertEquals(
              paths,
              List("src/Alpha/Main.bosatsu", "src/Zed/Main.bosatsu")
            )

            val alphaDependsOn: Option[List[String]] = itemList.collectFirst {
              case Json.JObject(fields)
                  if fields.exists {
                    case ("package", Json.JString("Alpha/Main")) => true
                    case _                                        => false
                  } =>
                fields.collectFirst {
                  case ("dependsOn", Json.JArray(values)) =>
                    values.toList.collect { case Json.JString(s) => s }
                }.getOrElse(Nil)
            }
            assertEquals(alphaDependsOn, Some(List("Missing/Dep")))

          case Right(other) =>
            fail(s"expected json array output, found: $other")
          case Left(err)    =>
            fail(show"failed to parse deps json output: $err")
        }

        val dotStr = readStringFile(state, Chain("out", "deps.dot"))
        assert(dotStr.contains("digraph G {"), dotStr)
        assert(dotStr.contains("Alpha/Main"), dotStr)
        assert(dotStr.contains("Zed/Main"), dotStr)
        assert(dotStr.contains("Missing/Dep"), dotStr)
    }
  }

  test("tool assemble fails when previous public dependencies are not provided") {
    val depSrc =
      """export depValue,
depValue = 9
main = depValue
"""
    val appV1Src =
      """main = 1
"""
    val appV2Src =
      """main = 2
"""
    val files = List(
      Chain("dep", "Dep", "Foo.bosatsu") -> depSrc,
      Chain("app_v1", "App", "Main.bosatsu") -> appV1Src,
      Chain("app_v2", "App", "Main.bosatsu") -> appV2Src
    )

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
          "check",
          "--package_root",
          "app_v1",
          "--input",
          "app_v1/App/Main.bosatsu",
          "--pub_dep",
          "out/dep.bosatsu_lib",
          "--output",
          "out/App.Main.v1.bosatsu_package"
        ),
        state2
      )
      (state3, _) = s3
      s4 <- runWithState(
        List(
          "tool",
          "assemble",
          "--name",
          "app",
          "--version",
          "0.0.1",
          "--package",
          "out/App.Main.v1.bosatsu_package",
          "--output",
          "out/app_prev_base.bosatsu_lib"
        ),
        state3
      )
      (state4, _) = s4
      state5 <- withInjectedPublicDep(
        state4,
        Chain("out", "app_prev_base.bosatsu_lib"),
        Chain("out", "dep.bosatsu_lib"),
        Chain("out", "app_prev.bosatsu_lib")
      )
      s5 <- runWithState(
        List(
          "tool",
          "check",
          "--package_root",
          "app_v2",
          "--input",
          "app_v2/App/Main.bosatsu",
          "--output",
          "out/App.Main.v2.bosatsu_package"
        ),
        state5
      )
      (state6, _) = s5
      s6 <- runWithState(
        List(
          "tool",
          "assemble",
          "--name",
          "app",
          "--version",
          "0.0.2",
          "--package",
          "out/App.Main.v2.bosatsu_package",
          "--previous_lib",
          "out/app_prev.bosatsu_lib",
          "--output",
          "out/app_new.bosatsu_lib"
        ),
        state6
      )
    } yield s6

    result match {
      case Right((_, out)) =>
        fail(
          s"expected missing previous public dependency failure, got: $out"
        )
      case Left(err)       =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(
          msg.contains("missing previous public dependency"),
          s"unexpected error: $msg"
        )
    }
  }

  test("tool deps includes interface/package info from includes and dependency libraries") {
    val depSrc =
      """export DepBox(), depBox,

struct DepBox(v: Int)

depBox = DepBox(1)
"""
    val appSrc =
      """from Dep/Foo import depBox

export main,

main = depBox
"""
    val files = List(
      Chain("dep", "Dep", "Foo.bosatsu") -> depSrc,
      Chain("app", "App", "Main.bosatsu") -> appSrc
    )

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
          "check",
          "--package_root",
          "app",
          "--input",
          "app/App/Main.bosatsu",
          "--pub_dep",
          "out/dep.bosatsu_lib",
          "--output",
          "out/App.Main.bosatsu_package",
          "--interface_out",
          "out/App.Main.bosatsig"
        ),
        state2
      )
      (state3, _) = s3
      s4 <- runWithState(
        List(
          "tool",
          "deps",
          "--include",
          "out/App.Main.bosatsu_package",
          "--interface",
          "out/App.Main.bosatsig",
          "--pub_dep",
          "out/dep.bosatsu_lib",
          "--graph_format",
          "json",
          "--output",
          "out/deps_mixed.json"
        ),
        state3
      )
      (state4, out4) = s4
      _ = out4 match {
        case Output.DepsOutput(_, Some(path), GraphOutput.Json) =>
          assertEquals(path, Chain("out", "deps_mixed.json"))
        case other =>
          fail(s"unexpected deps output: $other")
      }
    } yield state4

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right(state) =>
        val jsonStr = readStringFile(state, Chain("out", "deps_mixed.json"))
        Json.parserFile.parseAll(jsonStr) match {
          case Right(Json.JArray(items)) =>
            val rows = items.toList.collect { case Json.JObject(fields) => fields }
            val kinds = rows.flatMap(_.collectFirst { case ("kind", Json.JString(k)) => k })
            assert(kinds.contains("interface"), jsonStr)
            assert(kinds.contains("package"), jsonStr)

            val packages =
              rows.flatMap(_.collectFirst { case ("package", Json.JString(p)) => p })
            assert(packages.contains("Dep/Foo"), jsonStr)
            assert(packages.contains("App/Main"), jsonStr)

            val appPackDeps: Option[List[String]] = rows.collectFirst {
              case fields
                  if fields.contains(("package", Json.JString("App/Main"))) &&
                    fields.contains(("kind", Json.JString("package"))) =>
                fields.collectFirst {
                  case ("dependsOn", Json.JArray(values)) =>
                    values.toList.collect { case Json.JString(s) => s }
                }.getOrElse(Nil)
            }
            assertEquals(appPackDeps, Some(List("Dep/Foo")))
          case Right(other) =>
            fail(show"expected json array output, found: $other")
          case Left(err) =>
            fail(show"failed to parse deps json output: $err")
        }
    }
  }

  test("tool json apply reads --json_input and json errors are CliExceptions") {
    val applySrc =
      """main = (x) -> x.add(1)
"""
    val applyFiles = List(
      Chain("src", "Json", "Foo.bosatsu") -> applySrc,
      Chain("in.json") -> "[123]"
    )

    val applyFromPath = module.runWith(applyFiles)(
      List(
        "tool",
        "json",
        "apply",
        "--package_root",
        "src",
        "--input",
        "src/Json/Foo.bosatsu",
        "--main",
        "Json/Foo",
        "--json_input",
        "in.json"
      )
    )
    applyFromPath match {
      case Right(Output.JsonOutput(Json.JNumberStr("124"), _)) => ()
      case Right(other)                                         => fail(s"unexpected output: $other")
      case Left(err)                                            => fail(err.getMessage)
    }

    val wrongAritySrc =
      """main = (x, y) -> x.add(y)
"""
    val wrongArity = module.runWith(
      List(Chain("src", "Json", "Foo.bosatsu") -> wrongAritySrc)
    )(
      List(
        "tool",
        "json",
        "apply",
        "--package_root",
        "src",
        "--input",
        "src/Json/Foo.bosatsu",
        "--main",
        "Json/Foo",
        "--json_string",
        "[1]"
      )
    )
    wrongArity match {
      case Right(out) =>
        fail(s"expected apply error, got output: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("required a json array of size 2"), msg)
        assert(module.mainExceptionToString(err).nonEmpty, show"expected CliException: $msg")
      }
  }

  test("tool json traverse rejects non-array inputs") {
    val src =
      """main = (x) -> x.add(1)
"""
    val files = List(Chain("src", "Json", "Foo.bosatsu") -> src)

    module.runWith(files)(
      List(
        "tool",
        "json",
        "traverse",
        "--package_root",
        "src",
        "--input",
        "src/Json/Foo.bosatsu",
        "--main",
        "Json/Foo",
        "--json_string",
        "{}"
      )
    ) match {
      case Right(out) =>
        fail(s"expected traverse error, got output: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("require an array or arrays for traverse"), msg)
        assert(module.mainExceptionToString(err).nonEmpty, show"expected CliException: $msg")
    }
  }

  test("tool check without inputs reports a cli error") {
    module.runWith(Nil)(List("tool", "check")) match {
      case Right(out) =>
        fail(s"expected no-input check failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("no inputs given to check"), msg)
        assert(module.mainExceptionToString(err).nonEmpty, show"expected CliException: $msg")
    }
  }

  test("lib deps list text output includes public and private sections") {
    val files = baseLibFiles("main = 1\n")

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "lib",
          "deps",
          "add",
          "--repo_root",
          "repo",
          "--dep",
          "zed",
          "--version",
          "0.0.1",
          "--hash",
          validHash1,
          "--uri",
          "https://example.com/zed.bosatsu_lib",
          "--public",
          "--no-fetch"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runWithState(
        List(
          "lib",
          "deps",
          "add",
          "--repo_root",
          "repo",
          "--dep",
          "alpha",
          "--version",
          "0.0.2",
          "--hash",
          validHash2,
          "--uri",
          "https://example.com/alpha.bosatsu_lib",
          "--private",
          "--no-fetch"
        ),
        state1
      )
      (state2, _) = s2
      s3 <- runWithState(
        List("lib", "deps", "list", "--repo_root", "repo"),
        state2
      )
      (_, out3) = s3
    } yield out3

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right(Output.Basic(doc, None)) =>
        val rendered = doc.render(120)
        assert(rendered.contains("public deps:"), rendered)
        assert(rendered.contains("private deps:"), rendered)
        assert(rendered.contains("zed"), rendered)
        assert(rendered.contains("alpha"), rendered)
      case Right(other) =>
        fail(s"unexpected output: $other")
    }
  }

  test("lib json apply and traverse use CliException paths") {
    val src =
      """main = (x) -> x.add(1)
"""
    val files = baseLibFiles(src) :+ (Chain("repo", "in.json") -> "[41]")

    module.runWith(files)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_input",
        "repo/in.json"
      )
    ) match {
      case Right(Output.JsonOutput(Json.JNumberStr("42"), _)) => ()
      case Right(other)                                        => fail(s"unexpected output: $other")
      case Left(err)                                           => fail(err.getMessage)
    }

    val applyWrongArity = module.runWith(files)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "[1,2]"
      )
    )
    applyWrongArity match {
      case Right(out) =>
        fail(s"expected apply arity error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("required a json array of size 1"), msg)
    }

    val applyInvalidData = module.runWith(files)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "[\"x\"]"
      )
    )
    applyInvalidData match {
      case Right(out) =>
        fail(s"expected apply json conversion error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("invalid input json"), msg)
    }

    val applyParseError = module.runWith(files)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "}123456789012345678901234567890"
      )
    )
    applyParseError match {
      case Right(out) =>
        fail(s"expected apply parse error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("could not parse a JSON record at 1"), msg)
    }

    val traverseInvalidData = module.runWith(files)(
      List(
        "lib",
        "json",
        "traverse",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "[[\"x\"]]"
      )
    )
    traverseInvalidData match {
      case Right(out) =>
        fail(s"expected traverse invalid-data error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("invalid input json"), msg)
    }

    val traverseWrongArity = module.runWith(files)(
      List(
        "lib",
        "json",
        "traverse",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "[[1,2]]"
      )
    )
    traverseWrongArity match {
      case Right(out) =>
        fail(s"expected traverse arity error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("required a json array of size 1"), msg)
    }

    val traverseNonArray = module.runWith(files)(
      List(
        "lib",
        "json",
        "traverse",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "{}"
      )
    )
    traverseNonArray match {
      case Right(out) =>
        fail(s"expected traverse non-array error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("require an array for traverse"), msg)
    }
  }

  test("lib json write rejects unsupported function output") {
    val src =
      """main = (x) -> x
"""
    val files = baseLibFiles(src)

    module.runWith(files)(
      List(
        "lib",
        "json",
        "write",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo"
      )
    ) match {
      case Right(out) =>
        fail(s"expected unsupported-type error, got: $out")
      case Left(err)  =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("cannot convert type to Json"), msg)
    }
  }

  test("lib check renders previous descriptor details when previous is missing") {
    val previousDesc = proto.LibDescriptor(
      version = Some(Version(0, 0, 0).toProto),
      hashes = validHash1 :: Nil,
      uris = "https://example.com/mylib-0.0.0.bosatsu_lib" :: Nil
    )
    val conf = LibConfig
      .init(Name("mylib"), "https://example.com", Version(0, 0, 1))
      .copy(previous = Some(previousDesc))
    val files = baseLibFilesWithConf("main = 1\n", conf)

    module.runWith(files)(List("lib", "check", "--repo_root", "repo")) match {
      case Right(out) =>
        fail(s"expected missing-previous failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("previous not in cas"), msg)
    }
  }

  test("lib fetch fails on dependency download issues and reports details") {
    val files = baseLibFiles("main = 1\n")

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "lib",
          "deps",
          "add",
          "--repo_root",
          "repo",
          "--dep",
          "dep_with_uri",
          "--version",
          "0.0.1",
          "--hash",
          validHash2,
          "--uri",
          "https://example.com/dep_with_uri.bosatsu_lib",
          "--public",
          "--no-fetch"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runWithState(List("lib", "fetch", "--repo_root", "repo"), state1)
    } yield s2

    result match {
      case Right((_, out)) =>
        fail(s"expected lib fetch failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("failed to fetch"), msg)
    }
  }

  test("lib fetch reports previous fetch failures") {
    val previousDesc = proto.LibDescriptor(
      version = Some(Version(0, 0, 0).toProto),
      hashes = validHash3 :: Nil,
      uris = Nil
    )
    val conf = LibConfig
      .init(Name("mylib"), "https://example.com", Version(0, 0, 1))
      .copy(previous = Some(previousDesc))
    val files = baseLibFilesWithConf("main = 1\n", conf)

    module.runWith(files)(List("lib", "fetch", "--repo_root", "repo")) match {
      case Right(out) =>
        fail(s"expected previous fetch failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("failed to fetch previous"), msg)
    }
  }

  test("lib build without --main_pack reports missing main") {
    val files = baseLibFiles("main = 1\n")

    module.runWith(files)(
      List("lib", "build", "--repo_root", "repo", "--outdir", "out")
    ) match {
      case Right(out) =>
        fail(s"expected no-main build failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("no main defined"), msg)
    }
  }

  test("Output.Many stops at first non-success exit code") {
    val first = Output.TestOutput(
      List(
        (PackageName.parts("Pkg"), Some(Eval.now(Test.Assertion(false, "boom"))))
      ),
      Colorize.None
    )
    val second =
      Output.Basic(Doc.text("should not be written"), Some(Chain("out", "later.txt")))
    val out = Output.Many(Chain(first, second))

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](Nil)
      s1 <- module.reportOutput(out).run(s0)
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, exitCode)) =>
        assertEquals(exitCode, ExitCode.Error)
        assertEquals(state.get(Chain("out", "later.txt")), None)
    }
  }
}
