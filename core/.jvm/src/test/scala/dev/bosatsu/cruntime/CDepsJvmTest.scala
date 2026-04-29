package dev.bosatsu.cruntime

import dev.bosatsu.Json
import dev.bosatsu.hashing.{Algo, HashValue}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

class CDepsJvmTest extends munit.FunSuite {
  private def parseHash(value: String): Algo.WithAlgo[HashValue] =
    Algo.parseIdent
      .parseAll(value)
      .fold(err => fail(err.toString), identity)

  private val bdwgcHash: Algo.WithAlgo[HashValue] =
    parseHash(
      "blake3:0ec0780a271850fa1640a4d97c4dd8185cddcfd1bfb2563dd8501382fb85f6a4"
    )

  private val libuvHash: Algo.WithAlgo[HashValue] =
    parseHash(
      "blake3:433979d1027ec72d546e1e4440e193a9d587f1378a8405299d6f219d23c215b7"
    )

  private val bdwgcDependency =
    CDeps.Dependency(
      name = "bdwgc",
      version = "8.2.8",
      uris =
        "https://github.com/bdwgc/bdwgc/releases/download/v8.2.8/gc-8.2.8.tar.gz" ::
          Nil,
      hash = bdwgcHash,
      source_subdir = "gc-8.2.8",
      recipe = CDeps.BdwgcCmakeStatic,
      options = Some(Json.JObject(
        ("threadsafe" -> Json.JBool(true)) ::
          Nil
      ))
    )

  private val libuvDependency =
    CDeps.Dependency(
      name = "libuv",
      version = "1.52.1",
      uris = "https://dist.libuv.org/dist/v1.52.1/libuv-v1.52.1.tar.gz" :: Nil,
      hash = libuvHash,
      source_subdir = "libuv-v1.52.1",
      recipe = CDeps.LibuvCmakeStatic
    )

  test("checked-in manifest pins bdwgc and libuv source contracts") {
    val content =
      Files.readString(Paths.get("c_runtime", "deps.json"), StandardCharsets.UTF_8)

    val parsed = CDeps.parseManifestString(content)

    assertEquals(parsed.map(_.schema_version), Right(1))
    assertEquals(parsed.map(_.recipe_version), Right(1))
    assertEquals(
      parsed.map(_.dependencies),
      Right(bdwgcDependency :: libuvDependency :: Nil)
    )
    assertEquals(
      parsed.map(_.dependencies.flatMap(_.dependencies).flatten),
      Right(Nil)
    )
  }
}
