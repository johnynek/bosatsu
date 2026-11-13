package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.effect.IO
import cats.syntax.all._
import java.nio.file.{Files, Path, Paths}
import java.nio.file.attribute.FileTime

class EmitCCommandTest extends FileCommandFixtures {
  private val helper =
    """package Demo/Helper
      |export answer
      |answer = 7
      |checks = Assertion(False, "dependency assertion")
      |""".stripMargin
  private val suite =
    """package Demo/Suite
      |from Demo/Helper import answer
      |def private_check(x): x matches 7
      |checks = Assertion(private_check(answer), "root assertion")
      |""".stripMargin
  private val other = "package Demo/Other\nchecks = Assertion(True, \"another root\")\n"
  private val noTests = "package Demo/NoTests\nexport value\nvalue = 1\n"
  private val native =
    """package Demo/Native
      |export active, dormant
      |external def active(x: Int) -> Int
      |external def dormant(x: Int) -> Int
      |""".stripMargin
  private val nativeSuite =
    """package Demo/Foreign
      |from Demo/Native import active
      |checks = Assertion(active(9) matches 9, "native binding")
      |""".stripMargin
  private val nativeHeader = "bosatsu_ext_Demo_l_Native.h"
  private val progHeader = "bosatsu_ext_Bosatsu_l_Prog.h"
  private val progApi =
    """package Bosatsu/Prog
      |export Prog, Main(), pure
      |external type Prog[err: +*, res: +*]
      |external def pure[err, res](value: res) -> Prog[err, res]
      |struct Main(run: List[String] -> forall err. Prog[err, Int])
      |""".stripMargin
  private val programNative =
    """package Demo/Native
      |export active
      |external def active(x: Int) -> Int
      |""".stripMargin
  private val programMain =
    """package Demo/Program
      |from Bosatsu/Prog import Main, Prog, pure
      |from Demo/Native import active
      |export main, marker
      |exposes Bosatsu/Prog
      |def run(args: List[String]) -> forall err. Prog[err, Int]:
      |  _ = args
      |  pure(active(7))
      |main = Main(run)
      |marker = 1
      |""".stripMargin

  private def compile(
      runner: FileCommandRunner,
      root: Path,
      output: String,
      srcs: List[(String, String)],
      deps: List[String]
  ): IO[Unit] =
    srcs.traverse_ { case (name, body) => write(root, name, body) } *>
      runner.run(root,
        List("compile-packages", "--error-unused-interfaces", "--packages-out", output + ".pkg",
          "--sig-out", output + ".sig") :::
          srcs.flatMap { case (name, _) => List("--src", name) } :::
          deps.flatMap(name => List("--direct-interface", name + ".sig"))
      ).map(success)

  private def manifest(
      root: Path, files: List[String], bindings: List[(String, List[String])]
  ): IO[Unit] = {
    val external = bindings.map { case (pkg, names) =>
      Json.JObject(List("package" -> Json.JString(pkg),
        "symbols" -> Json.JArray(names.map(Json.JString(_)).toVector)))
    }
    write(root, "runtime.json", Json.JObject(List(
      "schema_version" -> Json.JNumberStr("1"), "kind" -> Json.JString("local"),
      "packages" -> Json.JArray(files.map(Json.JString(_)).toVector),
      "externals" -> Json.JArray(external.toVector)
    )).render)
  }

  private def emit(
      runner: FileCommandRunner, root: Path, selected: String, output: String, headers: String*
  ): IO[FileCommandRunner.Result] =
    runner.run(root, List("emit-c", "--runtime-manifest", "runtime.json",
      "--test-packages", selected, "--out", output) ::: headers.toList.flatMap(p => List("--extern-header-out", p)))

  private def emitProgram(
      runner: FileCommandRunner,
      root: Path,
      selected: String,
      mainPackage: String,
      output: String,
      headers: String*
  ): IO[FileCommandRunner.Result] =
    runner.run(root, List("emit-c", "--runtime-manifest", "runtime.json",
      "--main-packages", selected, "--main-package", mainPackage, "--out", output) :::
      headers.toList.flatMap(p => List("--extern-header-out", p)))

  private def programGraph(root: Path, runner: FileCommandRunner): IO[Unit] =
    for {
      _ <- compile(runner, root, "prog", List("Prog.bosatsu" -> progApi), Nil)
      _ <- compile(runner, root, "native", List("ProgramNative.bosatsu" -> programNative), Nil)
      _ <- compile(runner, root, "program", List("Program.bosatsu" -> programMain), List("prog", "native"))
      _ <- manifest(root, List("program.pkg", "native.pkg", "prog.pkg"),
        List("Bosatsu/Prog" -> List("pure"), "Demo/Native" -> List("active")))
    } yield ()

  private def simpleGraph(root: Path, runner: FileCommandRunner): IO[Unit] =
    for {
      _ <- compile(runner, root, "helper", List("Helper.bosatsu" -> helper), Nil)
      _ <- compile(runner, root, "suite", List("Suite.bosatsu" -> suite, "Other.bosatsu" -> other,
        "NoTests.bosatsu" -> noTests), List("helper"))
      _ <- manifest(root, List("suite.pkg", "helper.pkg"), Nil)
    } yield ()

  test("emit only suites from the selected multi-package file and tree-shake dependency assertions") {
    withRunner { (root, runner) =>
      for {
        _ <- simpleGraph(root, runner)
        result <- emit(runner, root, "suite.pkg", "out/test.c")
        _ = success(result)
        _ = assertEquals(result.stdout, "")
        c <- read(root, "out/test.c")
        _ = assert(c.contains("int main("), c)
        _ = assert(c.contains("bsts_test_run("), c)
        _ = assert(c.contains("\"Demo/Other\""), c)
        _ = assert(c.indexOf("\"Demo/Suite\"") > c.indexOf("\"Demo/Other\""), c)
        _ = assert(!c.contains("dependency assertion") && !c.contains("\"Demo/NoTests\""), c)
        _ = assert(c.contains("root assertion"), c)

        // Switching the root changes discovery even with the same graph.
        second <- emit(runner, root, "helper.pkg", "out/helper.c")
        _ = success(second)
        changed <- read(root, "out/helper.c")
        _ = assert(changed.contains("dependency assertion"), changed)
        _ = assert(!changed.contains("root assertion") && !changed.contains("\"Demo/Other\""), changed)
      } yield ()
    }
  }

  test("emit a selected program main with deterministic C and declared native headers") {
    withRunner { (root, runner) =>
      for {
        _ <- programGraph(root, runner)
        first <- emitProgram(runner, root, "program.pkg", "Demo/Program", "program.c",
          progHeader, nativeHeader)
        _ = success(first)
        c <- read(root, "program.c")
        prog <- read(root, progHeader)
        native <- read(root, nativeHeader)
        _ = assert(c.contains("int main("), c)
        _ = assert(c.contains("bsts_Bosatsu_Prog_run_main"), c)
        _ = assert(c.contains("___bsts_g_Demo_l_Program_l_main"), c)
        _ = assert(c.contains("bosatsu_ext_Bosatsu_l_Prog.h") && c.contains(nativeHeader), c)
        _ = assert(prog.contains("___bsts_g_Bosatsu_l_Prog_l_pure("), prog)
        _ = assert(native.contains("___bsts_g_Demo_l_Native_l_active("), native)
        again <- emitProgram(runner, root, "program.pkg", "Demo/Program", "program.c",
          progHeader, nativeHeader)
        _ = success(again)
        stable <- read(root, "program.c")
        _ = assertEquals(stable, c)
      } yield ()
    }
  }

  test("program selectors must be complete and mutually exclusive before inputs are read") {
    val module = new MainModule(IOPlatformIO)
    val base = List("tool", "emit-c", "--runtime-manifest", "missing-runtime.json", "--out", "program.c")
    val selectors = List(
      List("--test-packages", "program.pkg"),
      List("--main-packages", "program.pkg"),
      List("--main-package", "Demo/Program")
    )
    // Check every subset and ordering at the parser boundary without running I/O.
    selectors.toSet.subsets().foreach { subset =>
      val valid = subset == Set(selectors.head) || subset == selectors.tail.toSet
      subset.toList.permutations.foreach { ordered =>
        val args = base ::: ordered.flatten
        assertEquals(module.run(args).isRight, valid, args.mkString(" "))
      }
    }
  }

  test("emit-c help describes the required alternative root selectors") {
    val help = new MainModule(IOPlatformIO).run(List("tool", "emit-c", "--help"))
      .swap.toOption.getOrElse(fail("expected emit-c help")).toString
    val usage = help.linesIterator.drop(1).takeWhile(_.nonEmpty).toList
    assertEquals(usage.length, 2, help)
    assert(usage.exists(_.contains("--test-packages <path> --out")), help)
    assert(usage.exists(_.contains("--main-packages <path> --main-package <packageName> --out")), help)
  }

  test("program selection rejects absent bundles, absent root packages, and dependency-only mains") {
    withRunner { (root, runner) =>
      val rootSource =
        "package Demo/ProgramRoot\nfrom Demo/Program import marker\nvalue = marker\n"
      for {
        _ <- programGraph(root, runner)
        absentBundle <- emitProgram(runner, root, "absent.pkg", "Demo/Program", "program.c")
        _ = failure(absentBundle)
        _ = assert(absentBundle.stderr.contains("program bundle absent.pkg is absent"), absentBundle.stderr)
        absentPackage <- emitProgram(runner, root, "program.pkg", "Demo/Missing", "program.c")
        _ = failure(absentPackage)
        _ = assert(absentPackage.stderr.contains("Demo/Missing") && absentPackage.stderr.contains("selected program bundle"), absentPackage.stderr)
        _ <- compile(runner, root, "root", List("ProgramRoot.bosatsu" -> rootSource), List("program", "prog"))
        _ <- manifest(root, List("root.pkg", "program.pkg", "native.pkg", "prog.pkg"),
          List("Bosatsu/Prog" -> List("pure"), "Demo/Native" -> List("active")))
        dependency <- emitProgram(runner, root, "root.pkg", "Demo/Program", "program.c")
        _ = failure(dependency)
        _ = assert(dependency.stderr.contains("Demo/Program") && dependency.stderr.contains("selected program bundle"), dependency.stderr)
        _ = assert(!Files.exists(root.resolve("program.c")))
      } yield ()
    }
  }

  test("program selection rejects invalid mains and incompatible native Prog layouts") {
    withRunner { (root, runner) =>
      val invalidMain = "package Demo/InvalidMain\nmain = 7\n"
      val badMainLayout =
        "package Bosatsu/Prog\nexport Prog, Main()\n" +
          "external type Prog[err: +*, res: +*]\nstruct Main(run: Int)\n"
      val badProgLayout =
        "package Bosatsu/Prog\nexport Prog, Main(), pure\n" +
          "enum Prog[err: +*, res: +*]: Finished(value: res)\n" +
          "external def pure[err, res](value: res) -> Prog[err, res]\n" +
          "struct Main(run: List[String] -> forall err. Prog[err, Int])\n"
      val layoutCases = List(
        ("wrong-main", badMainLayout, "from Bosatsu/Prog import Main\nmain = Main(1)\n"),
        ("wrong-prog", badProgLayout,
          "from Bosatsu/Prog import Main, Prog, pure\n" +
            "def run(args: List[String]) -> forall err. Prog[err, Int]:\n  _ = args\n  pure(7)\n" +
            "main = Main(run)\n")
      )
      for {
        _ <- compile(runner, root, "invalid", List("InvalidMain.bosatsu" -> invalidMain), Nil)
        _ <- manifest(root, List("invalid.pkg"), Nil)
        invalid <- emitProgram(runner, root, "invalid.pkg", "Demo/InvalidMain", "program.c")
        _ = failure(invalid)
        _ = assert(invalid.stderr.contains("no valid Bosatsu/Prog::Main value found"), invalid.stderr)
        _ <- layoutCases.traverse_ { case (name, prog, mainBody) =>
          val mainSource = s"package Demo/LayoutMain\n$mainBody"
          for {
            _ <- compile(runner, root, "prog", List("Prog.bosatsu" -> prog), Nil)
            _ <- compile(runner, root, "layout", List("LayoutMain.bosatsu" -> mainSource), List("prog"))
            _ <- manifest(root, List("layout.pkg", "prog.pkg"), Nil)
            result <- emitProgram(runner, root, "layout.pkg", "Demo/LayoutMain", s"$name.c")
            _ = failure(result)
            _ = assert(result.stderr.contains("incompatible Bosatsu/Prog::Main"), result.stderr)
            _ = assert(!Files.exists(root.resolve(s"$name.c")))
          } yield ()
        }
      } yield ()
    }
  }

  test("program rendering checks reachable externals and program outputs cannot alias inputs") {
    withRunner { (root, runner) =>
      for {
        _ <- programGraph(root, runner)
        _ <- manifest(root, List("program.pkg", "native.pkg", "prog.pkg"),
          List("Bosatsu/Prog" -> List("pure")))
        missing <- emitProgram(runner, root, "program.pkg", "Demo/Program", "program.c")
        _ = failure(missing)
        _ = assert(missing.stderr.contains("missing native binding for Demo/Native::active"), missing.stderr)
        _ = assert(!Files.exists(root.resolve("program.c")))
        alias <- emitProgram(runner, root, "program.pkg", "Demo/Program", "program.pkg")
        _ = failure(alias)
        _ = assert(alias.stderr.contains("also a runtime input"), alias.stderr)
        manifestAlias <- emitProgram(runner, root, "program.pkg", "Demo/Program", "runtime.json")
        _ = failure(manifestAlias)
        _ = assert(manifestAlias.stderr.contains("also a runtime input"), manifestAlias.stderr)
        _ <- manifest(root, List("program.pkg", "native.pkg", "prog.pkg"),
          List("Bosatsu/Prog" -> List("pure"), "Demo/Native" -> List("active")))
        bound <- emitProgram(runner, root, "program.pkg", "Demo/Program", "program.c")
        _ = success(bound)
      } yield ()
    }
  }

  test("a program header publication failure preserves prior siblings and can be retried") {
    withRunner { (root, runner) =>
      val generated = "generated"
      val oldC = "previous C"
      val oldProgHeader = "previous Prog header"
      val generatedProgHeader = s"$generated/$progHeader"
      val generatedNativeHeader = s"$generated/$nativeHeader"
      for {
        _ <- programGraph(root, runner)
        _ <- write(root, "program.c", oldC)
        _ <- write(root, generatedProgHeader, oldProgHeader)
        _ <- IO.blocking(Files.createDirectories(root.resolve(generatedNativeHeader)))
        failed <- emitProgram(runner, root, "program.pkg", "Demo/Program", "program.c",
          generatedProgHeader, generatedNativeHeader)
        _ = failure(failed)
        _ = assert(failed.stderr.contains("expected a regular output file"), failed.stderr)
        c <- read(root, "program.c")
        prog <- read(root, generatedProgHeader)
        _ = assertEquals(c, oldC)
        _ = assertEquals(prog, oldProgHeader)
        _ <- IO.blocking(Files.delete(root.resolve(generatedNativeHeader)))
        retry <- emitProgram(runner, root, "program.pkg", "Demo/Program", "program.c",
          generatedProgHeader, generatedNativeHeader)
        _ = success(retry)
        updated <- read(root, "program.c")
        native <- read(root, generatedNativeHeader)
        _ = assertNotEquals(updated, oldC)
        _ = assert(native.contains("___bsts_g_Demo_l_Native_l_active("), native)
      } yield ()
    }
  }

  test("a clean action can emit from the manifest and bundles alone") {
    withRunner { (producer, runner) =>
      directory.use { action =>
        val selected = "inputs/generated 雪/suite.pkg"
        val dep = "external/lib/helper.pkg"
        for {
          _ <- simpleGraph(producer, runner)
          _ <- IO.blocking {
            List("suite.pkg" -> selected, "helper.pkg" -> dep).foreach { case (source, destination) =>
              val target = action.resolve(destination)
              Files.createDirectories(target.getParent)
              Files.copy(producer.resolve(source), target)
              ()
            }
          }
          _ <- manifest(action, List(dep, selected), Nil)
          res <- emit(runner, action, selected, "result.c")
          _ = success(res)
          c <- read(action, "result.c")
          _ = assert(c.contains("root assertion"))
          _ = assert(!Files.exists(action.resolve(".bosatsuc")))
          _ = assert(!listPaths(action).exists(_.endsWith(".sig")))
        } yield ()
      }
    }
  }

  test("manifest order and a diamond's repeated input leave generated bytes and mtime stable") {
    withRunner { (root, runner) =>
      for {
        _ <- simpleGraph(root, runner)
        result <- emit(runner, root, "suite.pkg", "test.c")
        _ = success(result)
        original <- read(root, "test.c")
        path = root.resolve("test.c")
        _ <- IO.blocking(Files.setLastModifiedTime(path, FileTime.fromMillis(1000000000000L)))
        before = Files.getLastModifiedTime(path)
        _ <- manifest(root, List("helper.pkg", "suite.pkg", "helper.pkg"), Nil)
        cold <- FileCommandRunner.resource.use(emit(_, root, "suite.pkg", "test.c"))
        _ = success(cold)
        after <- read(root, "test.c")
        _ = assertEquals(after, original)
        _ = assertEquals(Files.getLastModifiedTime(path), before)
      } yield ()
    }
  }

  test("a dependency suite cannot satisfy an empty selected group and undeclared selectors are rejected") {
    withRunner { (root, runner) =>
      for {
        _ <- simpleGraph(root, runner)
        _ <- compile(runner, root, "empty", List("NoTests.bosatsu" -> noTests), Nil)
        _ <- manifest(root, List("empty.pkg", "helper.pkg"), Nil)
        _ <- write(root, "test.c", "previous")
        res <- emit(runner, root, "empty.pkg", "test.c")
        _ = failure(res)
        _ = assert(res.stderr.contains("no tests found in empty.pkg") && res.stderr.contains("Demo/NoTests"), res.stderr)
        _ = assert(!res.stderr.contains("Demo/Helper"), res.stderr)
        missing <- emit(runner, root, "suite.pkg", "test.c")
        _ = failure(missing)
        _ = assert(missing.stderr.contains("suite.pkg is absent"), missing.stderr)
        old <- read(root, "test.c")
        _ = assertEquals(old, "previous")
      } yield ()
    }
  }

  test("invalid ProgTest ordering is diagnosed only for the selected packages") {
    withRunner { (root, runner) =>
      val prog = "package Bosatsu/Prog\nexport ProgTest()\nstruct ProgTest(check: Test)\n"
      val invalid =
        "package Demo/Invalid\nfrom Bosatsu/Prog import ProgTest\n" +
          "first = ProgTest(Assertion(True, \"first\"))\nlate = Assertion(False, \"late\")\n"
      for {
        _ <- compile(runner, root, "prog", List("Prog.bosatsu" -> prog), Nil)
        _ <- compile(runner, root, "mixed", List("Invalid.bosatsu" -> invalid, "Other.bosatsu" -> other), List("prog"))
        _ <- compile(runner, root, "helper", List("Helper.bosatsu" -> helper), Nil)
        _ <- manifest(root, List("helper.pkg", "mixed.pkg", "prog.pkg"), Nil)
        allowed <- emit(runner, root, "helper.pkg", "helper.c")
        _ = success(allowed)
        bad <- emit(runner, root, "mixed.pkg", "mixed.c")
        _ = failure(bad)
        _ = assert(bad.stderr.contains("Demo/Invalid") && bad.stderr.contains("first") && bad.stderr.contains("late"),
          bad.stderr)
        _ = assert(!Files.exists(root.resolve("mixed.c")))
      } yield ()
    }
  }

  test("selected ProgTest values must match the native runner calling convention") {
    withRunner { (root, runner) =>
      val runtimeType = "export Prog\nexternal type Prog[e: +*, r: +*]\n"
      val cases = List(
        (runtimeType + "struct ProgTest(check: Test)", "Assertion(True, \"value instead of callback\")"),
        (runtimeType + "struct ProgTest(check: List[String] -> Int)", "_ -> 7"),
        ("enum Prog[e, r]: Done(value: r), Failure(error: e)\n" +
          "struct ProgTest(check: List[String] -> forall e. Prog[e, Test])",
          "_ -> Done(Assertion(True, \"local representation\"))")
      )
      for {
        _ <- manifest(root, List("prog.pkg"), Nil)
        _ <- write(root, "test.c", "previous output")
        _ <- cases.traverse_ { case (declaration, value) =>
          val src = s"package Bosatsu/Prog\n$declaration\nchecks = ProgTest($value)\n"
          for {
            _ <- compile(runner, root, "prog", List("Prog.bosatsu" -> src), Nil)
            result <- emit(runner, root, "prog.pkg", "test.c")
            _ = failure(result)
            _ = assert(result.stderr.contains("incompatible Bosatsu/Prog::ProgTest"), result.stderr)
            previous <- read(root, "test.c")
            _ = assertEquals(previous, "previous output")
          } yield ()
        }
      } yield ()
    }
  }

  test("a reachable native symbol needs a binding, a discarded symbol does not") {
    withRunner { (root, runner) =>
      val deps = List("native.pkg", "suite.pkg")
      for {
        _ <- compile(runner, root, "native", List("Native.bosatsu" -> native), Nil)
        _ <- compile(runner, root, "suite", List("Suite.bosatsu" -> nativeSuite), List("native"))
        _ <- manifest(root, deps, List("Demo/Native" -> List("dormant")))
        bad <- emit(runner, root, "suite.pkg", "test.c")
        _ = failure(bad)
        _ = assert(bad.stderr.contains("missing native binding for Demo/Native::active"), bad.stderr)
        _ = assert(!Files.exists(root.resolve("test.c")))
        _ <- manifest(root, deps, List("Demo/Native" -> List("active")))
        ok <- emit(runner, root, "suite.pkg", "test.c", s"generated/$nativeHeader")
        _ = success(ok)
        code <- read(root, "test.c")
        header <- read(root, s"generated/$nativeHeader")
        _ = assert(code.contains("#include \"" + nativeHeader + "\""), code)
        _ = assert(header.contains("___bsts_g_Demo_l_Native_l_active("), header)
        _ = assert(!header.contains("dormant"), header)
        removed <- manifest(root, deps, Nil) *> emit(runner, root, "suite.pkg", "test.c", s"generated/$nativeHeader")
        _ = failure(removed)
        oldCode <- read(root, "test.c")
        oldHeader <- read(root, s"generated/$nativeHeader")
        _ = assertEquals(oldCode, code)
        _ = assertEquals(oldHeader, header)
      } yield ()
    }
  }

  test("only requested headers are written and a bad header prevents publishing the whole output set") {
    withRunner { (root, runner) =>
      for {
        _ <- compile(runner, root, "native", List("Native.bosatsu" -> native), Nil)
        _ <- compile(runner, root, "suite", List("Suite.bosatsu" -> nativeSuite), List("native"))
        _ <- manifest(root, List("suite.pkg", "native.pkg"), List("Demo/Native" -> List("active")))
        _ <- write(root, "out.c", "old C")
        _ <- write(root, nativeHeader, "old header")
        bad <- emit(runner, root, "suite.pkg", "out.c", nativeHeader, "invented.h")
        _ = failure(bad)
        _ = assert(bad.stderr.contains("no declared external bindings generate header invented.h"), bad.stderr)
        old <- read(root, "out.c")
        h <- read(root, nativeHeader)
        _ = assertEquals(old, "old C")
        _ = assertEquals(h, "old header")
        _ = assert(!Files.exists(root.resolve("invented.h")))
        ok <- emit(runner, root, "suite.pkg", "out.c")
        _ = success(ok)
        retained <- read(root, nativeHeader)
        _ = assertEquals(retained, "old header")
        dup <- emit(runner, root, "suite.pkg", "out.c", "out.c")
        _ = failure(dup)
        _ = assert(dup.stderr.contains("distinct"), dup.stderr)
      } yield ()
    }
  }

  test("fresh C and header paths follow the destination filesystem's case rules") {
    withRunner { (root, runner) =>
      val upper = nativeHeader.toUpperCase(java.util.Locale.ROOT)
      for {
        _ <- compile(runner, root, "native", List("Native.bosatsu" -> native), Nil)
        _ <- compile(runner, root, "suite", List("Suite.bosatsu" -> nativeSuite), List("native"))
        _ <- manifest(root, List("suite.pkg", "native.pkg"), List("Demo/Native" -> List("active")))
        insensitive = Files.exists(root.resolve("NATIVE.PKG"))
        result <- emit(runner, root, "suite.pkg", s"new/$upper", s"new/$nativeHeader")
        _ = if (insensitive) {
          failure(result)
          assert(!Files.exists(root.resolve(s"new/$nativeHeader")))
        } else {
          success(result)
          assert(!Files.isSameFile(root.resolve(s"new/$upper"), root.resolve(s"new/$nativeHeader")))
        }
        _ <- IO.blocking {
          Files.createSymbolicLink(root.resolve("runner.c"), Path.of("PENDING"))
          Files.createSymbolicLink(root.resolve(nativeHeader), Path.of("pending"))
        }
        linked <- emit(runner, root, "suite.pkg", "runner.c", nativeHeader)
        _ = if (insensitive) {
          failure(linked)
          assert(!Files.exists(root.resolve("pending")))
        } else success(linked)
      } yield ()
    }
  }

  test("an in-place dependency edit is observed by a reused emission request") {
    withRunner { (root, runner) =>
      for {
        _ <- simpleGraph(root, runner)
        good <- emit(runner, root, "suite.pkg", "out.c")
        _ = success(good)
        original <- read(root, "out.c")
        api <- IO.blocking(Files.readAllBytes(root.resolve("helper.sig")).toList)
        _ <- compile(runner, root, "helper", List("Helper.bosatsu" -> helper.replace("answer = 7", "answer = 9")), Nil)
        nextApi <- IO.blocking(Files.readAllBytes(root.resolve("helper.sig")).toList)
        _ = assertEquals(nextApi, api)
        updated <- emit(runner, root, "suite.pkg", "out.c")
        _ = success(updated)
        newCode <- read(root, "out.c")
        _ = assertNotEquals(newCode, original)
        // A disappeared implementation is an input error, even after success.
        _ <- manifest(root, List("suite.pkg"), Nil)
        missing <- emit(runner, root, "suite.pkg", "out.c")
        _ = failure(missing)
        _ = assert(missing.stderr.contains("missing package Demo/Helper required by Demo/Suite"), missing.stderr)
        preserved <- read(root, "out.c")
        _ = assertEquals(preserved, newCode)
      } yield ()
    }
  }

  test("a replacement bundle cannot change the type of an already compiled value import") {
    withRunner { (root, runner) =>
      def provider(arg: String) =
        s"package Provider\nexport id_value\ndef id_value(x: $arg) -> $arg: x\n"
      val consumer = "package Client\nfrom Provider import id_value as old_id\n" +
        "checks = Assertion(old_id(7) matches 7, \"stored import\")\n"
      for {
        _ <- compile(runner, root, "provider", List("Provider.bosatsu" -> provider("Int")), Nil)
        _ <- compile(runner, root, "root", List("Client.bosatsu" -> consumer), List("provider"))
        _ <- manifest(root, List("root.pkg", "provider.pkg"), Nil)
        valid <- emit(runner, root, "root.pkg", "runner.c")
        _ = success(valid)
        c <- read(root, "runner.c")
        _ <- compile(runner, root, "provider", List("Provider.bosatsu" -> provider("String")), Nil)
        stale <- emit(runner, root, "root.pkg", "runner.c")
        _ = failure(stale)
        _ = assert(stale.stderr.contains("Client") && stale.stderr.contains("Provider::id_value"), stale.stderr)
        previous <- read(root, "runner.c")
        _ = assertEquals(previous, c)
      } yield ()
    }
  }

  test("a new public enum case invalidates a caller compiled with an exhaustive match") {
    withRunner { (root, runner) =>
      def api(cases: String, value: String) =
        s"package Provider\nexport Event(), current\nenum Event: $cases\ncurrent = $value\n"
      val consumer = """package Client
                       |from Provider import A, B, current
                       |def old_match(x):
                       |  match x:
                       |    case A: True
                       |    case B: False
                       |checks = Assertion(old_match(current), "exhaustive")
                       |""".stripMargin
      for {
        _ <- compile(runner, root, "provider", List("Provider.bosatsu" -> api("A, B", "A")), Nil)
        _ <- compile(runner, root, "root", List("Client.bosatsu" -> consumer), List("provider"))
        _ <- manifest(root, List("root.pkg", "provider.pkg"), Nil)
        first <- emit(runner, root, "root.pkg", "out.c")
        _ = success(first)
        before <- read(root, "out.c")
        _ <- compile(runner, root, "provider", List("Provider.bosatsu" -> api("A, B, C", "C")), Nil)
        stale <- emit(runner, root, "root.pkg", "out.c")
        _ = failure(stale)
        _ = assert(stale.stderr.contains("root.pkg") && stale.stderr.contains("Provider"), stale.stderr)
        after <- read(root, "out.c")
        _ = assertEquals(after, before)
      } yield ()
    }
  }

  // The intrinsic for int_low_bits_to_Int64 would treat this Int result as a
  // packed Int64, so the package must be rejected rather than lowered.
  private val fakeInt64 =
    "package Bosatsu/Num/Int64\nexport int_low_bits_to_Int64\n" +
      "external def int_low_bits_to_Int64(x: Int) -> Int\n"
  private val fakeInt64Errors = List(
    "Bosatsu/Num/Int64 must match the core library",
    "Int64 must be declared `external type Int64`",
    "int_low_bits_to_Int64 must have type Int -> Int64, found Int -> Int"
  )

  test("compiling a Bosatsu/Num/Int64 that differs from core fails") {
    withRunner { (root, runner) =>
      for {
        _ <- write(root, "Native.bosatsu", fakeInt64)
        result <- runner.run(root, List("compile-packages", "--packages-out", "native.pkg",
          "--sig-out", "native.sig", "--src", "Native.bosatsu"))
        _ = failure(result)
        _ = fakeInt64Errors.foreach(msg => assert(result.stderr.contains(msg), result.stderr))
      } yield ()
    }
  }

  test("emitting from a bundle with a Bosatsu/Num/Int64 that differs from core fails") {
    withRunner { (root, runner) =>
      // Today's compiler cannot produce this bundle, so rename a compiled one.
      val renamed = fakeInt64.replace("package Bosatsu/Num/Int64", "package Fake/Num/Int64")
      for {
        _ <- compile(runner, root, "native", List("Native.bosatsu" -> renamed), Nil)
        _ <- IO.blocking {
          val path = root.resolve("native.pkg")
          val packs = proto.Packages.parseFrom(Files.readAllBytes(path)).packages.map { p =>
            p.withStrings(p.strings.map(str => if (str == "Fake/Num/Int64") "Bosatsu/Num/Int64" else str))
          }
          Files.write(path, proto.Packages(packs).toByteArray)
        }
        _ <- manifest(root, List("native.pkg"), List("Bosatsu/Num/Int64" -> List("int_low_bits_to_Int64")))
        result <- emit(runner, root, "native.pkg", "out.c")
        _ = failure(result)
        _ = assert(result.stderr.contains("invalid local implementation graph"), result.stderr)
        _ = fakeInt64Errors.foreach(msg => assert(result.stderr.contains(msg), result.stderr))
        _ <- IO.blocking(assert(!Files.exists(root.resolve("out.c"))))
      } yield ()
    }
  }

  test("literal Int64 conversions from the core Int64 source fold to constants") {
    withRunner { (root, runner) =>
      val use = "package Client\nfrom Bosatsu/Num/Int64 import int_low_bits_to_Int64, int64_to_Int\n" +
        "checks = Assertion(int64_to_Int(int_low_bits_to_Int64(4)) matches 4, \"folded literal\")\n"
      for {
        int64 <- IO.blocking(Files.readString(Paths.get("test_workspace", "Int64.bosatsu")))
        _ <- compile(runner, root, "int64", List("Int64.bosatsu" -> int64), Nil)
        _ <- compile(runner, root, "root", List("Client.bosatsu" -> use), List("int64"))
        _ <- manifest(root, List("root.pkg", "int64.pkg"), List("Bosatsu/Num/Int64" -> List("int64_to_Int")))
        result <- emit(runner, root, "root.pkg", "out.c")
        _ = success(result)
        c <- read(root, "out.c")
        _ = assert(c.contains("bsts_int64_from_int64(4)"), c)
        _ = assert(!c.contains("___bsts_g_Bosatsu_l_Num_l_Int64_l_int__low__bits__to__Int64"), c)
      } yield ()
    }
  }

  test("legacy callers cannot use a constructor their provider has stopped exporting") {
    withRunner { (root, runner) =>
      val api = "package Provider\nexport Flag(), current\nenum Flag: On, Off\ncurrent = On\n"
      val client = "package Client\nfrom Provider import current, On\nchecks = Assertion(current matches On, \"visible\")\n"
      for {
        _ <- compile(runner, root, "provider", List("Provider.bosatsu" -> api), Nil)
        _ <- compile(runner, root, "root", List("Client.bosatsu" -> client), List("provider"))
        _ <- manifest(root, List("provider.pkg", "root.pkg"), Nil)
        good <- emit(runner, root, "root.pkg", "out.c")
        _ = success(good)
        _ <- IO.blocking {
          val path = root.resolve("root.pkg")
          val old = proto.Packages.parseFrom(Files.readAllBytes(path)).packages.map { p =>
            p.withImports(p.imports.map(_.withCanonicalInterface(false)))
          }
          Files.write(path, proto.Packages(old).toByteArray)
        }
        _ <- compile(runner, root, "provider", List("Provider.bosatsu" -> api.replace("Flag()", "Flag")), Nil)
        hidden <- emit(runner, root, "root.pkg", "out.c")
        _ = failure(hidden)
        _ = assert(hidden.stderr.contains("Provider::On"), hidden.stderr)
      } yield ()
    }
  }

  test("legacy external nominal types with no saved layout cannot certify an old exhaustive match") {
    withRunner { (root, runner) =>
      val api = "package Provider\nexport Flag(), current\nenum Flag: On, Off\ncurrent = On\n"
      val client = "package Client\nfrom Provider import current, On, Off\n" +
        "def flags(x):\n  match x:\n    case On: True\n    case Off: False\n" +
        "checks = Assertion(flags(current), \"exhaustive\")\n"
      for {
        _ <- compile(runner, root, "provider", List("Provider.bosatsu" -> api), Nil)
        _ <- compile(runner, root, "root", List("Client.bosatsu" -> client), List("provider"))
        _ <- IO.blocking {
          val p = root.resolve("root.pkg")
          Files.write(p, proto.Packages(proto.Packages.parseFrom(Files.readAllBytes(p)).packages).toByteArray)
        }
        _ <- compile(runner, root, "provider", List("Provider.bosatsu" ->
          api.replace("On, Off", "On, Off, Unknown").replace("current = On", "current = Unknown")), Nil)
        _ <- manifest(root, List("root.pkg", "provider.pkg"), Nil)
        stale <- emit(runner, root, "root.pkg", "out.c")
        _ = failure(stale)
        _ = assert(stale.stderr.contains("recompile") && stale.stderr.contains("root.pkg") && stale.stderr.contains("Provider"), stale.stderr)
        _ = assert(!Files.exists(root.resolve("out.c")))
      } yield ()
    }
  }

  test("an exposure-only owner's public layout is checked even after rebuilding the facade") {
    withRunner { (root, runner) =>
      def owner(field: String, n: String) =
        s"package Owner\nexport Box(), box\nstruct Box(n: $field)\nbox = Box($n)\n"
      val front = "package Front\nfrom Owner import box\nexport value\nexposes Owner\nvalue = box\n"
      val client = "package Client\nfrom Front import value\nexport values\nexposes Owner\n" +
        "values = [value]\nchecks = Assertion(True, \"ok\")\n"
      def rebuild(body: String) =
        compile(runner, root, "owner", List("Owner.bosatsu" -> body), Nil) *>
          compile(runner, root, "front", List("Front.bosatsu" -> front), List("owner"))
      for {
        _ <- rebuild(owner("Int", "1"))
        _ <- compile(runner, root, "root", List("Client.bosatsu" -> client), List("front", "owner"))
        _ <- manifest(root, List("owner.pkg", "front.pkg", "root.pkg"), Nil)
        ok <- emit(runner, root, "root.pkg", "out.c")
        _ = success(ok)
        before = Files.readAllBytes(root.resolve("front.sig")).toList
        _ <- rebuild(owner("String", "\"changed\""))
        _ = assertEquals(Files.readAllBytes(root.resolve("front.sig")).toList, before)
        stale <- emit(runner, root, "root.pkg", "out.c")
        _ = failure(stale)
        _ = assert(stale.stderr.contains("root.pkg") && stale.stderr.contains("Owner"), stale.stderr)
        _ <- compile(runner, root, "root", List("Client.bosatsu" -> client), List("front", "owner"))
        rebuilt <- emit(runner, root, "root.pkg", "out.c")
        _ = success(rebuilt)
      } yield ()
    }
  }

  test("opaque fields and unrelated sibling exports do not change a caller's interface requirements") {
    withRunner { (root, runner) =>
      val client = "package Client\nfrom Provider import box, is_ok\nchecks = Assertion(is_ok(box), \"opaque\")\n"
      def provider(field: String, initial: String) =
        s"package Provider\nexport Box, box, is_ok\nstruct Box(value: $field)\nbox = Box($initial)\n" +
          "def is_ok(_: Box) -> Bool: True\n"
      def deps(api: String, siblingValue: String) =
        compile(runner, root, "wide", List("Provider.bosatsu" -> api, "Sibling.bosatsu" ->
          s"package Sibling\nexport extra\nextra = $siblingValue\n"), Nil)
      def caller() = compile(runner, root, "root", List("Client.bosatsu" -> client), List("wide"))
      def bytes(file: String) = Files.readAllBytes(root.resolve(file)).toList
      for {
        _ <- deps(provider("Int", "1"), "1")
        _ <- caller()
        original = bytes("root.pkg")
        wideSig = bytes("wide.sig")
        _ <- manifest(root, List("root.pkg", "wide.pkg"), Nil)
        _ <- deps(provider("String", "\"private\""), "True")
        _ = assertNotEquals(bytes("wide.sig"), wideSig)
        valid <- emit(runner, root, "root.pkg", "out.c")
        _ = success(valid)
        _ <- caller()
        _ = assertEquals(bytes("root.pkg"), original)
      } yield ()
    }
  }

  test("modern bundles cannot omit, duplicate or substitute their recorded interface requirements") {
    withRunner { (root, runner) =>
      for {
        _ <- simpleGraph(root, runner)
        path = root.resolve("suite.pkg")
        bundle = proto.Packages.parseFrom(Files.readAllBytes(path))
        reqs = bundle.getBuildRequirements
        _ = assertEquals(reqs.interfaces.map(_.packageName).toList, List("Demo/Helper"))
        req = reqs.interfaces.head
        variants = List(
          Nil -> "missing interface requirements",
          List(req, req) -> "repeated interface requirement",
          List(req.withInterfaceBlake3("0" * 64)) -> "stale interface",
          List(req.withPackageName("Unknown")) -> "missing implementation"
        )
        _ <- write(root, "out.c", "last successful output")
        _ <- variants.traverse_ { case (changed, message) =>
          for {
            _ <- IO.blocking(Files.write(path, bundle.withBuildRequirements(reqs.withInterfaces(changed)).toByteArray))
            result <- emit(runner, root, "suite.pkg", "out.c")
            _ = failure(result)
            _ = assert(result.stderr.contains(message), result.stderr)
            previous <- read(root, "out.c")
            _ = assertEquals(previous, "last successful output")
          } yield ()
        }
      } yield ()
    }
  }

  test("output aliases cannot overwrite runtime inputs") {
    withRunner { (root, runner) =>
      for {
        _ <- simpleGraph(root, runner)
        _ <- IO.blocking {
          Files.createSymbolicLink(root.resolve("linked.c"), Path.of("suite.pkg"))
          Files.createLink(root.resolve("hard.c"), root.resolve("suite.pkg"))
        }
        before <- IO.blocking(Files.readAllBytes(root.resolve("suite.pkg")).toList)
        _ <- List("./suite.pkg", "linked.c", "hard.c", "missing/../suite.pkg").traverse_ { path =>
          for {
            result <- emit(runner, root, "suite.pkg", path)
            _ = failure(result)
            after <- IO.blocking(Files.readAllBytes(root.resolve("suite.pkg")).toList)
            _ = assertEquals(after, before)
          } yield ()
        }
      } yield ()
    }
  }

  test("legacy implementation import views can still drive test C emission") {
    withRunner { (root, runner) =>
      for {
        _ <- simpleGraph(root, runner)
        _ <- IO.blocking {
          val path = root.resolve("suite.pkg")
          val bundle = proto.Packages.parseFrom(Files.readAllBytes(path))
          val old = bundle.withPackages(bundle.packages.map { p =>
            p.withImports(p.imports.map(_.withCanonicalInterface(false)))
          })
          Files.write(path, proto.Packages(old.packages).toByteArray)
        }
        legacy <- emit(runner, root, "suite.pkg", "legacy.c")
        _ = success(legacy)
        c <- read(root, "legacy.c")
        _ = assert(c.contains("root assertion"))
        _ = assert(!c.contains("dependency assertion"))
      } yield ()
    }
  }

  test("a declared unused external can change its header without changing the test runner") {
    withRunner { (root, runner) =>
      val header = s"inc/$nativeHeader"
      for {
        _ <- compile(runner, root, "native", List("Native.bosatsu" -> native), Nil)
        _ <- compile(runner, root, "root", List("Other.bosatsu" -> other), Nil)
        _ <- manifest(root, List("root.pkg", "native.pkg"), List("Demo/Native" -> List("dormant")))
        first <- emit(runner, root, "root.pkg", "runner.c", header)
        _ = success(first)
        c1 <- read(root, "runner.c")
        h1 <- read(root, header)
        _ = assert(!c1.contains(nativeHeader), c1)
        _ = assert(h1.contains("___bsts_g_Demo_l_Native_l_dormant(") && !h1.contains("active"), h1)
        _ <- compile(runner, root, "native", List("Native.bosatsu" -> native.replace(
          "dormant(x: Int)", "dormant(x: Int, y: Int)")), Nil)
        second <- emit(runner, root, "root.pkg", "runner.c", header)
        _ = success(second)
        c2 <- read(root, "runner.c")
        h2 <- read(root, header)
        _ = assertEquals(c2, c1)
        _ = assertNotEquals(h2, h1)
      } yield ()
    }
  }

  test("a C or header destination cannot replace a declared runtime input") {
    withRunner { (root, runner) =>
      for {
        _ <- simpleGraph(root, runner)
        _ <- List("runtime.json", "suite.pkg", "helper.pkg").traverse_ { input =>
          for {
            before <- IO.blocking(Files.readAllBytes(root.resolve(input)).toList)
            asC <- emit(runner, root, "suite.pkg", input)
            _ = failure(asC)
            _ = assert(asC.stderr.contains(s"output $input is also a runtime input"), asC.stderr)
            asHeader <- emit(runner, root, "suite.pkg", "runner.c", input)
            _ = failure(asHeader)
            _ = assert(asHeader.stderr.contains(s"output $input is also a runtime input"), asHeader.stderr)
            after <- IO.blocking(Files.readAllBytes(root.resolve(input)).toList)
            _ = assertEquals(after, before)
            _ = assert(!Files.exists(root.resolve("runner.c")))
          } yield ()
        }
      } yield ()
    }
  }

  test("first-class external functions need a provider at their point of use") {
    withRunner { (root, runner) =>
      val callbacks =
        "package Demo/Callback\nexport invoke\nexternal def invoke(fn: Int -> Int, x: Int) -> Int\n"
      val client =
        "package Demo/TestCallback\nfrom Demo/Callback import invoke\nfrom Demo/Native import active\n" +
          "checks = Assertion(invoke(active, 1) matches 1, \"callback\")\n"
      val files = List("suite.pkg", "callback.pkg", "native.pkg")
      val callbackProvider = "Demo/Callback" -> List("invoke")
      val callbackHeader = "bosatsu_ext_Demo_l_Callback.h"
      for {
        _ <- compile(runner, root, "native", List("Native.bosatsu" -> native), Nil)
        _ <- compile(runner, root, "callback", List("Callback.bosatsu" -> callbacks), Nil)
        _ <- compile(runner, root, "suite", List("Suite.bosatsu" -> client), List("native", "callback"))
        _ <- manifest(root, files, List(callbackProvider))
        missing <- emit(runner, root, "suite.pkg", "out.c")
        _ = failure(missing)
        _ = assert(missing.stderr.contains("missing native binding for Demo/Native::active"), missing.stderr)
        _ <- manifest(root, files, List(callbackProvider, "Demo/Native" -> List("active")))
        ok <- emit(runner, root, "suite.pkg", "out.c", nativeHeader, callbackHeader)
        _ = success(ok)
        c <- read(root, "out.c")
        _ = assert(c.contains(nativeHeader) && c.contains(callbackHeader), c)
        h <- read(root, callbackHeader)
        _ = assert(h.contains("___bsts_g_Demo_l_Callback_l_invoke("), h)
      } yield ()
    }
  }

  test("independent file requests can select different test entries at the same relative paths") {
    withRunner { (left, runner) =>
      directory.use { right =>
        def setup(root: Path, name: String) =
          compile(runner, root, "suite", List("Tests.bosatsu" ->
            s"package Demo/$name\nchecks = Assertion(True, \"suite $name\")\n"), Nil) *>
            manifest(root, List("suite.pkg"), Nil)
        for {
          _ <- (setup(left, "First"), setup(right, "Second")).parTupled
          results <- (emit(runner, left, "suite.pkg", "out.c"), emit(runner, right, "suite.pkg", "out.c")).parTupled
          _ = success(results._1)
          _ = success(results._2)
          one <- read(left, "out.c")
          two <- read(right, "out.c")
          _ = assert(one.contains("suite First") && !one.contains("suite Second"), one)
          _ = assert(two.contains("suite Second") && !two.contains("suite First"), two)
        } yield ()
      }
    }
  }
}
