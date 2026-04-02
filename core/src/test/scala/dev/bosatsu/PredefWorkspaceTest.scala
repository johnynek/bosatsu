package dev.bosatsu

class PredefWorkspaceTest extends munit.FunSuite with ParTest {
  import TestUtils.*

  private val loopsPack = Predef.loadFileInCompile("test_workspace/Loops.bosatsu")
  private val float64Pack = Predef.loadFileInCompile("test_workspace/Float64.bosatsu")
  private val predefTestsPack =
    Predef.loadFileInCompile("test_workspace/PredefTests.bosatsu")

  test("predef workspace tests run") {
    runBosatsuTest(
      List(loopsPack, float64Pack, predefTestsPack),
      "PredefTests",
      178
    )
  }
}
