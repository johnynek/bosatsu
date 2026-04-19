package dev.bosatsu

class BosatsuIntRuntimeTest extends munit.FunSuite {
  import TestUtils.runBosatsuTest

  private val runtimeCasesPack =
    Predef.loadFileInCompile("test_workspace/IntRuntimeCases.bosatsu")

  test("Bosatsu Int tests exercise optimized runtime special cases") {
    runBosatsuTest(List(runtimeCasesPack), "IntRuntimeCases", 41)
  }
}
