package dev.bosatsu.tool

import cats.data.Chain
import dev.bosatsu.{
  CompileOptions,
  LocationMap,
  MemoryMain,
  Package,
  PackageMap,
  PackageName,
  Par
}
import munit.FunSuite

class CompileCacheTest extends FunSuite {
  private type ErrorOr[A] = Either[Throwable, A]
  private type F[A] = MemoryMain.StateF[ErrorOr][A]

  private val platform = MemoryMain.memoryPlatformIO[ErrorOr]
  private val cacheDir = Chain("cache")
  private val cache = CompileCache.filesystem(cacheDir, platform)

  private def parsePackage(source: String): Package.Parsed =
    Package.parser(None).parseAll(source) match {
      case Right(pack) => pack
      case Left(err)   => fail(s"parse failure: $err")
    }

  private def compilePackage(
      source: String,
      compileOptions: CompileOptions = CompileOptions.Default
  ): Package.Inferred = {
    val parsed = parsePackage(source)
    val checked = Par.noParallelism {
      PackageMap.typeCheckParsed(
        cats.data.NonEmptyList.one((("test", LocationMap(source)), parsed)),
        Nil,
        "predef",
        compileOptions
      )
    }

    checked.toOption match {
      case Some(packMap) =>
        packMap.toMap.get(parsed.name).getOrElse {
          fail(s"missing compiled package ${parsed.name}")
        }
      case None          =>
        fail(s"typecheck failed: $checked")
    }
  }

  private def runF[A](
      fa: F[A],
      state: MemoryMain.State = MemoryMain.State.empty
  ): (MemoryMain.State, A) =
    fa.run(state) match {
      case Right(value) => value
      case Left(err)    => fail(Option(err.getMessage).getOrElse(err.toString))
    }

  private def compileKey(
      pack: Package.Parsed,
      deps: List[(PackageName, Package.Interface)] = Nil,
      compileOptions: CompileOptions = CompileOptions.Default
  ): InferCache.FsKey =
    runF(
      cache.generateKey(
        pack,
        deps,
        compileOptions,
        "compiler-id",
        "phase-id"
      )
    )._2

  test("sourceExprHash ignores statement regions") {
    val sourceA =
      """package Cache/Foo
        |
        |main = 1
        |""".stripMargin
    val sourceB =
      """
        |
        |package Cache/Foo
        |
        |
        |main = 1
        |""".stripMargin

    val packA = parsePackage(sourceA)
    val packB = parsePackage(sourceB)

    assertEquals(
      CompileCache.sourceExprHash(packA),
      CompileCache.sourceExprHash(packB)
    )
  }

  test("sourceExprHash ignores comments and padding statements") {
    val sourceA =
      """package Cache/Foo
        |main = 1
        |""".stripMargin
    val sourceB =
      """package Cache/Foo
        |
        |# this should not affect the hash
        |
        |main = 1
        |""".stripMargin

    val packA = parsePackage(sourceA)
    val packB = parsePackage(sourceB)

    assertEquals(
      CompileCache.sourceExprHash(packA),
      CompileCache.sourceExprHash(packB)
    )
  }

  test("dependency interface changes invalidate the compile key") {
    val consumerSource =
      """package Cache/App
        |from Cache/Dep import dep
        |main = dep
        |""".stripMargin
    val depSourceV1 =
      """package Cache/Dep
        |export dep
        |dep = 1
        |""".stripMargin
    val depSourceV2 =
      """package Cache/Dep
        |export dep, other
        |dep = 1
        |other = True
        |""".stripMargin

    val consumer = parsePackage(consumerSource)
    val depIfaceV1 = Package.interfaceOf(compilePackage(depSourceV1))
    val depIfaceV2 = Package.interfaceOf(compilePackage(depSourceV2))

    val keyV1 = compileKey(consumer, List(depIfaceV1.name -> depIfaceV1))
    val keyV2 = compileKey(consumer, List(depIfaceV2.name -> depIfaceV2))

    assertNotEquals(CompileCache.keyHashHex(keyV1), CompileCache.keyHashHex(keyV2))
  }

  test("compile mode and optimize flag invalidate the compile key") {
    val source =
      """package Cache/Foo
        |main = 1
        |""".stripMargin
    val parsed = parsePackage(source)

    val emitKey = compileKey(parsed, compileOptions = CompileOptions.Default)
    val typecheckKey =
      compileKey(parsed, compileOptions = CompileOptions.TypeCheckOnly)
    val noOptimizeKey =
      compileKey(parsed, compileOptions = CompileOptions.NoOptimize)

    assertNotEquals(
      CompileCache.keyHashHex(emitKey),
      CompileCache.keyHashHex(typecheckKey)
    )
    assertNotEquals(
      CompileCache.keyHashHex(emitKey),
      CompileCache.keyHashHex(noOptimizeKey)
    )
  }

  test("corrupt links and missing cas entries are treated as cache misses") {
    val source =
      """package Cache/Foo
        |main = 1
        |""".stripMargin
    val parsed = parsePackage(source)
    val compiled = compilePackage(source)
    val key = compileKey(parsed)

    val (stateWithCache, _) = runF(cache.put(key, compiled))
    val (_, initialHit) = runF(cache.get(key), stateWithCache)
    assert(initialHit.nonEmpty)

    val keyHex = CompileCache.keyHashHex(key)
    val keyPath = Chain(
      "cache",
      "keys",
      "blake3",
      keyHex.take(2),
      keyHex.drop(2)
    )
    val corruptState = stateWithCache
      .withFile(keyPath, MemoryMain.FileContent.Str("invalid-hash-ident"))
      .getOrElse(fail(s"failed to write corrupt link at $keyPath"))

    val (_, missFromCorruptLink) = runF(cache.get(key), corruptState)
    assertEquals(missFromCorruptLink, None)

    val outputHex = CompileCache.outputHashHex(compiled).getOrElse {
      fail("expected compiled package output hash")
    }
    val casPath = Chain(
      "cache",
      "cas",
      "blake3",
      outputHex.take(2),
      s"${outputHex.drop(2)}.bosatsu_package"
    )
    val missingCasState = stateWithCache.remove(casPath)

    val (_, missFromMissingCas) = runF(cache.get(key), missingCasState)
    assertEquals(missFromMissingCas, None)
  }
}
