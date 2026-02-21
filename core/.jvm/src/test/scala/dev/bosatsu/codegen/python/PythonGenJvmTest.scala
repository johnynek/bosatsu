package dev.bosatsu.codegen.python

import dev.bosatsu.{PackageName, Par, TestUtils}

class PythonGenJvmTest extends munit.FunSuite {
  test("array intrinsic representation uses tuple-backed storage") {
    Par.withEC {
      val pm = TestUtils.compileFile(
        "test_workspace/Bosatsu/Collection/Array.bosatsu",
        "test_workspace/List.bosatsu",
        "test_workspace/Option.bosatsu",
        "test_workspace/Char.bosatsu",
        "test_workspace/Bool.bosatsu",
        "test_workspace/Nat.bosatsu"
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
}
