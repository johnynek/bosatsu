package dev.bosatsu.cruntime

import dev.bosatsu.Json
import dev.bosatsu.hashing.{Algo, HashValue}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class VendoredDepsTest extends munit.ScalaCheckSuite {
  private val hash: Algo.WithAlgo[HashValue] =
    Algo.parseIdent
      .parseAll(
        "blake3:0ec0780a271850fa1640a4d97c4dd8185cddcfd1bfb2563dd8501382fb85f6a4"
      )
      .fold(err => fail(err.toString), identity)

  private val noDescCatchExceptionRaise = "-DNO_DESC_CATCH_EXCEPTION_RAISE"

  private def dependency(
      name: String,
      recipe: String,
      options: Option[Json.JObject] = None
  ) =
    CDeps.Dependency(
      name = name,
      version = "1.0.0",
      uris = "https://example.com/src.tar.gz" :: Nil,
      hash = hash,
      source_subdir = "src",
      recipe = recipe,
      options = options
    )

  private def cFlagsArg(args: List[String]): Option[String] =
    args.collectFirst { case arg if arg.startsWith("-DCMAKE_C_FLAGS=") =>
      arg.stripPrefix("-DCMAKE_C_FLAGS=")
    }

  test("staticLibFileName follows vendored recipe naming") {
    assertEquals(
      VendoredDeps.staticLibFileName(
        dependency("bdwgc", CDeps.BdwgcCmakeStatic)
      ),
      "libgc.a"
    )
    assertEquals(
      VendoredDeps.staticLibFileName(
        dependency("libuv", CDeps.LibuvCmakeStatic)
      ),
      "libuv.a"
    )
    assertEquals(
      VendoredDeps.staticLibFileName(
        dependency("zstd", "custom-recipe")
      ),
      "libzstd.a"
    )
  }

  test("bdwgc runtime requirements carry GC_THREADS for runtime and generated code") {
    val dep =
      dependency(
        "bdwgc",
        CDeps.BdwgcCmakeStatic,
        Some(Json.JObject(("threadsafe" -> Json.JBool(true)) :: Nil))
      )

    assertEquals(
      VendoredDeps.runtimeRequirementsFor(dep),
      CDeps.RuntimeRequirements(
        "-DGC_THREADS" :: Nil,
        "-DGC_THREADS" :: Nil
      )
    )
  }

  test("bdwgc configure args add the Darwin-only define and keep common switches") {
    val macosArgs =
      VendoredDeps.bdwgcConfigureArgs(
        normalizedHostOs = "macos",
        profile = "release",
        sourceRoot = "/tmp/src",
        buildDir = "/tmp/build",
        prefix = "/tmp/prefix",
        inheritedCFlags = Some("-O2 -g")
      )
    val linuxArgs =
      VendoredDeps.bdwgcConfigureArgs(
        normalizedHostOs = "linux",
        profile = "release",
        sourceRoot = "/tmp/src",
        buildDir = "/tmp/build",
        prefix = "/tmp/prefix",
        inheritedCFlags = Some("-O2 -g")
      )

    val commonArgs =
      List(
        "-S",
        "/tmp/src",
        "-B",
        "/tmp/build",
        "-DCMAKE_BUILD_TYPE=Release",
        "-DCMAKE_INSTALL_PREFIX=/tmp/prefix",
        "-DBUILD_SHARED_LIBS=OFF",
        "-DBUILD_TESTING=OFF",
        "-Denable_threads=ON"
      )

    assert(commonArgs.forall(macosArgs.contains(_)))
    assert(commonArgs.forall(linuxArgs.contains(_)))
    assertEquals(
      cFlagsArg(macosArgs),
      Some(s"-O2 -g $noDescCatchExceptionRaise")
    )
    assertEquals(cFlagsArg(linuxArgs), None)
  }

  test("bdwgc configure args use the expected CMake build type for debug and release") {
    val debugArgs =
      VendoredDeps.bdwgcConfigureArgs(
        normalizedHostOs = "macos",
        profile = "debug",
        sourceRoot = "/tmp/src",
        buildDir = "/tmp/build",
        prefix = "/tmp/prefix",
        inheritedCFlags = None
      )
    val releaseArgs =
      VendoredDeps.bdwgcConfigureArgs(
        normalizedHostOs = "macos",
        profile = "release",
        sourceRoot = "/tmp/src",
        buildDir = "/tmp/build",
        prefix = "/tmp/prefix",
        inheritedCFlags = None
      )

    assert(debugArgs.contains("-DCMAKE_BUILD_TYPE=Debug"))
    assert(releaseArgs.contains("-DCMAKE_BUILD_TYPE=Release"))
  }

  test("bdwgc configure args emit a valid Darwin CFLAGS entry when inherited flags are empty") {
    val args =
      VendoredDeps.bdwgcConfigureArgs(
        normalizedHostOs = "macos",
        profile = "release",
        sourceRoot = "/tmp/src",
        buildDir = "/tmp/build",
        prefix = "/tmp/prefix",
        inheritedCFlags = None
      )

    assertEquals(cFlagsArg(args), Some(noDescCatchExceptionRaise))
  }

  test("bdwgc configure args do not duplicate the Darwin define when it is already inherited") {
    val args =
      VendoredDeps.bdwgcConfigureArgs(
        normalizedHostOs = "macos",
        profile = "release",
        sourceRoot = "/tmp/src",
        buildDir = "/tmp/build",
        prefix = "/tmp/prefix",
        inheritedCFlags = Some(s"-O2 $noDescCatchExceptionRaise")
      )

    assertEquals(cFlagsArg(args), Some(s"-O2 $noDescCatchExceptionRaise"))
  }

  private val safeCFlagTokenGen =
    Gen.oneOf(
      "-O2",
      "-g",
      "-fPIC",
      "-Winvalid-offsetof",
      "-Werror",
      "-DMY_FLAG=1"
    )

  property("Darwin bdwgc configure args retain inherited CFLAGS tokens and inject the define once") {
    forAll(Gen.listOf(safeCFlagTokenGen)) { tokens =>
      val inherited =
        Option.when(tokens.nonEmpty)(tokens.mkString(" "))
      val args =
        VendoredDeps.bdwgcConfigureArgs(
          normalizedHostOs = "macos",
          profile = "release",
          sourceRoot = "/tmp/src",
          buildDir = "/tmp/build",
          prefix = "/tmp/prefix",
          inheritedCFlags = inherited
        )
      val actualTokens =
        cFlagsArg(args).toList.flatMap(_.split("\\s+")).filter(_.nonEmpty)

      assertEquals(actualTokens, tokens :+ noDescCatchExceptionRaise)
    }
  }
}
