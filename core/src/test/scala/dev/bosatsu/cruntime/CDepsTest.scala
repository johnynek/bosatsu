package dev.bosatsu.cruntime

import dev.bosatsu.Json
import dev.bosatsu.hashing.{Algo, HashValue}

class CDepsTest extends munit.FunSuite {
  private val hash: Algo.WithAlgo[HashValue] =
    Algo.parseIdent
      .parseAll(
        "blake3:0ec0780a271850fa1640a4d97c4dd8185cddcfd1bfb2563dd8501382fb85f6a4"
      )
      .fold(err => fail(err.toString), identity)

  private val dependency =
    CDeps.Dependency(
      name = "bdwgc",
      version = "8.2.8",
      uris =
        "https://github.com/bdwgc/bdwgc/releases/download/v8.2.8/gc-8.2.8.tar.gz" ::
          Nil,
      hash = hash,
      sourceSubdir = "gc-8.2.8",
      recipe = CDeps.BdwgcCmakeStatic,
      options = Json.JObject(
        ("threadsafe" -> Json.JBool(true)) ::
          Nil
      )
    )

  test("manifest parses vendored dependency entries") {
    val content =
      """{
        |  "schema_version": 1,
        |  "recipe_version": 1,
        |  "dependencies": [
        |    {
        |      "name": "bdwgc",
        |      "version": "8.2.8",
        |      "uris": [
        |        "https://github.com/bdwgc/bdwgc/releases/download/v8.2.8/gc-8.2.8.tar.gz"
        |      ],
        |      "hash": "blake3:0ec0780a271850fa1640a4d97c4dd8185cddcfd1bfb2563dd8501382fb85f6a4",
        |      "source_subdir": "gc-8.2.8",
        |      "recipe": "bdwgc-cmake-static",
        |      "options": {
        |        "threadsafe": true
        |      }
        |    }
        |  ]
        |}""".stripMargin

    val parsed = CDeps.parseManifestString(content)
    assertEquals(parsed.map(_.schemaVersion), Right(1))
    assertEquals(parsed.map(_.recipeVersion), Right(1))
    assertEquals(parsed.map(_.dependencies), Right(dependency :: Nil))
  }

  test("manifest round trips through the json writer") {
    val manifest = CDeps.Manifest(1, 3, dependency :: Nil)
    val rendered = CDeps.renderManifest(manifest)
    assertEquals(CDeps.parseManifestString(rendered), Right(manifest))
  }

  test("build key is stable across env insertion order and changes with recipe inputs") {
    val baseCtx =
      CDeps.BuildContext(
        os = "Darwin",
        arch = "aarch64",
        toolchainFamily = "Clang",
        compilerPath = "/usr/bin/cc",
        compilerVersion = "Apple clang version 17.0.0",
        archiverPath = Some("/usr/bin/ar"),
        archiverVersion = Some("Apple ar"),
        profile = "release",
        recipeVersion = 1,
        relevantEnv = Map("CFLAGS" -> "-O2", "CC" -> "/usr/bin/cc")
      )
    val sameCtx =
      baseCtx.copy(relevantEnv = Map("CC" -> "/usr/bin/cc", "CFLAGS" -> "-O2"))
    val differentCtx = baseCtx.copy(recipeVersion = 2)

    val first = CDeps.buildKey(dependency, baseCtx)
    val second = CDeps.buildKey(dependency, sameCtx)
    val third = CDeps.buildKey(dependency, differentCtx)

    assertEquals(first, second)
    assertNotEquals(first, third)
  }

  test("build key changes when transitive dependency identities change") {
    val ctx =
      CDeps.BuildContext(
        os = "macos",
        arch = "arm64",
        toolchainFamily = "clang",
        compilerPath = "/usr/bin/cc",
        compilerVersion = "Apple clang version 17.0.0",
        archiverPath = Some("/usr/bin/ar"),
        archiverVersion = Some("Apple ar"),
        profile = "release",
        recipeVersion = 1,
        relevantEnv = Map.empty
      )

    val dep =
      dependency.copy(
        name = "libuv",
        version = "1.0.0",
        recipe = CDeps.LibuvCmakeStatic,
        dependencies = "bdwgc" :: Nil
      )

    val first = CDeps.buildKey(dep, ctx, "key-a" :: Nil)
    val second = CDeps.buildKey(dep, ctx, "key-b" :: Nil)

    assertNotEquals(first, second)
  }

  test("orderedDependencies sorts dependencies before dependents") {
    val atomic =
      dependency.copy(name = "libatomic_ops", version = "7.8.2")
    val gc =
      dependency.copy(
        dependencies = "libatomic_ops" :: Nil
      )
    val uv =
      dependency.copy(
        name = "libuv",
        version = "1.52.1",
        recipe = CDeps.LibuvCmakeStatic,
        dependencies = "bdwgc" :: Nil
      )
    val manifest = CDeps.Manifest(1, 1, uv :: gc :: atomic :: Nil)

    val ordered = CDeps.orderedDependencies(manifest)
    assertEquals(
      ordered.map(_.map(_.name)),
      Right("libatomic_ops" :: "bdwgc" :: "libuv" :: Nil)
    )
  }

  test("orderedDependencies rejects missing dependencies and cycles") {
    val missing =
      CDeps.Manifest(
        1,
        1,
        dependency.copy(dependencies = "missing" :: Nil) :: Nil
      )
    val cycle =
      CDeps.Manifest(
        1,
        1,
        dependency.copy(name = "a", dependencies = "b" :: Nil) ::
          dependency.copy(name = "b", dependencies = "a" :: Nil) ::
          Nil
      )

    assertEquals(
      CDeps.orderedDependencies(missing),
      Left("bdwgc depends on missing vendored dependency: missing")
    )
    assert(
      CDeps.orderedDependencies(cycle).left.exists(
        _.contains("dependency cycle detected")
      )
    )
  }

  test("metadata round trips with runtime requirements") {
    val metadata =
      CDeps.Metadata(
        schemaVersion = 1,
        name = "bdwgc",
        version = "8.2.8",
        recipe = CDeps.BdwgcCmakeStatic,
        sourceHash = hash.toIdent,
        buildKey = "abc123",
        dependencies = Nil,
        target =
          CDeps.Target("macos", "arm64", "clang", "Apple clang version 17.0.0"),
        prefix = "/tmp/prefix",
        includeDirs = "/tmp/prefix/include" :: Nil,
        staticLibs = "/tmp/prefix/lib/libgc.a" :: Nil,
        systemLinkFlags = "-lm" :: Nil,
        runtimeRequirements =
          CDeps.RuntimeRequirements("-DGC_THREADS" :: Nil, Nil)
      )

    val rendered = CDeps.renderMetadata(metadata)
    assertEquals(CDeps.parseMetadataString(rendered), Right(metadata))
  }

  test("pkg-config parsing keeps only system flags") {
    val content =
      """prefix=/tmp/prefix
        |libdir=${prefix}/lib
        |includedir=${prefix}/include
        |Libs: -L${libdir} -lgc
        |Libs.private: -pthread -ldl
        |Cflags: -I${includedir}
        |""".stripMargin

    assertEquals(
      VendoredDeps.parsePkgConfigSystemFlags(content),
      "-pthread" :: "-ldl" :: Nil
    )
  }
}
