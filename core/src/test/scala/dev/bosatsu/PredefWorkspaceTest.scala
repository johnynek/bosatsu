package dev.bosatsu

class PredefWorkspaceTest extends munit.FunSuite with ParTest {
  import TestUtils.*

  private val loopsPack = Predef.loadFileInCompile("test_workspace/Loops.bosatsu")
  private val float64Pack = Predef.loadFileInCompile("test_workspace/Float64.bosatsu")
  private val int64Pack = Predef.loadFileInCompile("test_workspace/Int64.bosatsu")
  // Keep the Int64 wrapper and function smoke in its own package so `/` can
  // use the Int64 wrapper without colliding with the package-wide Int `/`
  // alias in PredefTests.bosatsu.
  private val int64WrapperSmokePack =
    Predef.loadFileInCompile("test_workspace/Int64WrapperSmoke.bosatsu")
  private val predefTestsPack =
    Predef.loadFileInCompile("test_workspace/PredefTests.bosatsu")

  test("predef workspace tests run") {
    runBosatsuTest(
      List(loopsPack, float64Pack, int64Pack, predefTestsPack),
      "PredefTests",
      223
    )
  }

  test("Int64 wrapper smoke tests run") {
    runBosatsuTest(
      List(int64Pack, int64WrapperSmokePack),
      "Int64WrapperSmoke",
      13
    )
  }
}
