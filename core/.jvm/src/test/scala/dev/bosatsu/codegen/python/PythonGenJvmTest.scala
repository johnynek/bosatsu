package dev.bosatsu.codegen.python

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.Comparator
import scala.sys.process.{Process, ProcessLogger}
import dev.bosatsu.{Identifier, PackageName, Par, Parser, TestUtils}
import scala.concurrent.duration.DurationInt

class PythonGenJvmTest extends munit.FunSuite {
  override val munitTimeout = 2.minutes

  private def modulePath(root: Path, module: PythonGen.Module): Path =
    module.toList.foldLeft(root) { (curr, ident) =>
      curr.resolve(ident.name)
    }

  private def writeModules(
      rendered: Map[PackageName, (PythonGen.Module, org.typelevel.paiges.Doc)],
      root: Path
  ): Unit =
    rendered.values.foreach { case (module, doc) =>
      val path = modulePath(root, module)
      val parent = path.getParent
      if (parent != null) {
        Files.createDirectories(parent)
        val parts = module.toList.map(_.name)
        (1 until parts.size).foreach { idx =>
          val dir = parts.take(idx).foldLeft(root) { (curr, part) =>
            curr.resolve(part)
          }
          Files.createDirectories(dir)
          val initFile = dir.resolve("__init__.py")
          if (!Files.exists(initFile))
            Files.writeString(initFile, "", StandardCharsets.UTF_8): Unit
        }
      }
      Files.writeString(path, doc.renderTrim(80), StandardCharsets.UTF_8)
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

  private def readProgExternals
      : Map[(PackageName, Identifier.Bindable), (PythonGen.Module, Code.Ident)] = {
    val externalsFile = Path.of("test_workspace/Prog.bosatsu_externals")
    Parser
      .unsafeParse(
        PythonGen.externalParser,
        Files.readString(externalsFile, StandardCharsets.UTF_8)
      )
      .iterator
      .map { case (pack, bindable, module, ident) =>
        ((pack, bindable), (module, ident))
      }
      .toMap
  }

  private def pyStringLiteral(str: String): String =
    s"'${str.replace("\\", "\\\\").replace("'", "\\'")}'"

  private def runnerScript(
      root: Path,
      moduleFile: Path,
      testName: String,
      prefix: String
  ): String =
    s"""import runpy
       |import sys
       |
       |sys.path.insert(0, ${pyStringLiteral(root.toString)})
       |scope = runpy.run_path(${pyStringLiteral(moduleFile.toString)})
       |test_value = scope[${pyStringLiteral(testName)}]
       |
       |def check_test(test, prefix):
       |    tag = test[0]
       |    if tag == 0:
       |        if not test[1]:
       |            raise AssertionError(prefix + "/" + str(test[2]))
       |        return
       |    if tag != 1:
       |        raise AssertionError("unexpected Test encoding: " + repr(test))
       |
       |    suite = str(test[1])
       |    current = test[2]
       |    while current[0] == 1:
       |        check_test(current[1], prefix + "/" + suite)
       |        current = current[2]
       |    if current[0] != 0:
       |        raise AssertionError("unexpected List encoding: " + repr(current))
       |
       |check_test(test_value, ${pyStringLiteral(prefix)})
       |""".stripMargin

  test("array intrinsic representation uses tuple-backed storage") {
    Par.withEC {
      val pm = TestUtils.compileFile(
        "test_workspace/Bosatsu/Collection/Array.bosatsu",
        "test_workspace/Float64.bosatsu",
        "test_workspace/List.bosatsu",
        "test_workspace/Option.bosatsu",
        "test_workspace/Char.bosatsu",
        "test_workspace/Bool.bosatsu",
        "test_workspace/Nat.bosatsu",
        "test_workspace/BinNat.bosatsu",
        "test_workspace/Int64.bosatsu",
        "test_workspace/Rand.bosatsu",
        "test_workspace/Properties.bosatsu"
      )
      val rendered = PythonGen.renderSource(pm, Map.empty, Map.empty)
      val doc =
        rendered(())(PackageName.parts("Bosatsu", "Collection", "Array"))._2
      val code = doc.render(120)

      assert(
        code.contains("return (tuple("),
        s"expected tuple-backed array payloads in generated code:\n$code"
      )
      assert(
        code.contains(" = list("),
        s"expected mutable list copy for in-place updates/sort:\n$code"
      )
      assert(
        !code.contains("return ([], 0, 0)"),
        s"unexpected list-backed empty array representation:\n$code"
      )
    }
  }

  test("Array Int64 helpers work through the shipped ProgExt runtime") {
    val root = Files.createTempDirectory("bosatsu_python_array_runtime_test_")
    try {
      Par.withEC {
        val pm = TestUtils.compileFile(
          "test_workspace/Bosatsu/Collection/Array.bosatsu",
          "test_workspace/Float64.bosatsu",
          "test_workspace/List.bosatsu",
          "test_workspace/Option.bosatsu",
          "test_workspace/Char.bosatsu",
          "test_workspace/Bool.bosatsu",
          "test_workspace/Nat.bosatsu",
          "test_workspace/BinNat.bosatsu",
          "test_workspace/Int64.bosatsu",
          "test_workspace/Rand.bosatsu",
          "test_workspace/Properties.bosatsu"
        )
        val rendered = PythonGen.renderSource(pm, readProgExternals, Map.empty)
        val packages = rendered(())
        val moduleFile =
          modulePath(
            root,
            packages(PackageName.parts("Bosatsu", "Collection", "Array"))._1
          )

        writeModules(packages, root)
        Files.copy(Path.of("test_workspace/ProgExt.py"), root.resolve("ProgExt.py"))

        val runner = root.resolve("run_array_tests.py")
        Files.writeString(
          runner,
          runnerScript(
            root.toAbsolutePath,
            moduleFile.toAbsolutePath,
            "tests",
            "Bosatsu/Collection/Array"
          ),
          StandardCharsets.UTF_8
        )

        val stdout = new StringBuilder
        val stderr = new StringBuilder
        val exit =
          Process(Seq("python3", runner.toString), root.toFile)
            .!(ProcessLogger(
              line => stdout.append(line).append('\n'): Unit,
              line => stderr.append(line).append('\n'): Unit
            ))

        assertEquals(
          exit,
          0,
          s"python3 runner failed\nstdout:\n$stdout\nstderr:\n$stderr"
        )
      }
    } finally deleteRecursively(root)
  }

  test("Int64 wrapper smoke works through the shipped ProgExt runtime") {
    val root = Files.createTempDirectory("bosatsu_python_runtime_test_")
    try {
      Par.withEC {
        val pm = TestUtils.compileFile(
          "test_workspace/Int64WrapperSmoke.bosatsu",
          "test_workspace/Int64.bosatsu"
        )
        val rendered = PythonGen.renderSource(pm, readProgExternals, Map.empty)
        val packages = rendered(())
        val moduleFile =
          modulePath(root, packages(PackageName.parts("Int64WrapperSmoke"))._1)

        writeModules(packages, root)
        Files.copy(Path.of("test_workspace/ProgExt.py"), root.resolve("ProgExt.py"))

        val runner = root.resolve("run_predef_tests.py")
        Files.writeString(
          runner,
          runnerScript(
            root.toAbsolutePath,
            moduleFile.toAbsolutePath,
            "test",
            "Int64WrapperSmoke"
          ),
          StandardCharsets.UTF_8
        )

        val stdout = new StringBuilder
        val stderr = new StringBuilder
        val exit =
          Process(Seq("python3", runner.toString), root.toFile)
            .!(ProcessLogger(
              line => stdout.append(line).append('\n'): Unit,
              line => stderr.append(line).append('\n'): Unit
            ))

        assertEquals(
          exit,
          0,
          s"python3 runner failed\nstdout:\n$stdout\nstderr:\n$stderr"
        )
      }
    } finally deleteRecursively(root)
  }
}
