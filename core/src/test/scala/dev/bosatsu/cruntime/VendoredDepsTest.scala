package dev.bosatsu.cruntime

import dev.bosatsu.Json
import dev.bosatsu.hashing.{Algo, HashValue}

class VendoredDepsTest extends munit.FunSuite {
  private val hash: Algo.WithAlgo[HashValue] =
    Algo.parseIdent
      .parseAll(
        "blake3:0ec0780a271850fa1640a4d97c4dd8185cddcfd1bfb2563dd8501382fb85f6a4"
      )
      .fold(err => fail(err.toString), identity)

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

  test(
    "bdwgc runtime requirements carry GC_THREADS for runtime and generated code"
  ) {
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
}
