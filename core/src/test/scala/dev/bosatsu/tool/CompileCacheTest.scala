package dev.bosatsu.tool

import cats.data.{Chain, NonEmptyList}
import dev.bosatsu.cache.{CompileCache, FsKey}
import dev.bosatsu.{
  CompileOptions,
  HasRegion,
  Identifier,
  LocationMap,
  MemoryMain,
  Package,
  PackageMap,
  PackageName,
  Par,
  Region,
  TypedExpr
}
import munit.FunSuite
import scala.collection.immutable.SortedMap

class CompileCacheTest extends FunSuite {
  private type ErrorOr[A] = Either[Throwable, A]
  private type F[A] = MemoryMain.StateF[ErrorOr][A]

  private val platform = MemoryMain.memoryPlatformIO[ErrorOr]
  private val cacheDir = Chain("cache")
  private val cache = CompileCache.filesystem(cacheDir, platform)

  private def parsePackage(source: String): Package.Parsed =
    Package.parser.parseAll(source) match {
      case Right(pack) => pack
      case Left(err)   => fail(s"parse failure: $err")
    }

  private def compilePackage(
      source: String,
      depIfaces: List[Package.Interface] = Nil,
      compileOptions: CompileOptions = CompileOptions.Default
  ): Package.Compiled = {
    val parsed = parsePackage(source)
    val checked = Par.noParallelism {
      PackageMap.typeCheckParsed(
        cats.data.NonEmptyList.one((("test", LocationMap(source)), parsed)),
        depIfaces,
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
      deps: SortedMap[PackageName, Package.Interface] = SortedMap.empty,
      compileOptions: CompileOptions = CompileOptions.Default
  ): FsKey = {
    val (stateAfterDepHashes, depHashes) =
      deps.iterator.foldLeft(
        (
          MemoryMain.State.empty,
          SortedMap.empty[PackageName, cache.DepHash]
        )
      ) { case ((state, hashes), (depName, depIface)) =>
        val (nextState, depHash) = runF(cache.dependencyHash(depIface), state)
        (nextState, hashes.updated(depName, depHash))
      }

    runF(
      cache.generateKey(
        pack.name,
        CompileCache.sourceExprHash(pack),
        depHashes,
        compileOptions,
        "compiler-id",
        "phase-id"
      ),
      stateAfterDepHashes
    )._2
  }

  private def keyPathFor(key: FsKey): Chain[String] = {
    val keyHex = CompileCache.keyHashHex(key)
    Chain("cache", "keys", "blake3", keyHex.take(2), keyHex.drop(2))
  }

  private def casPathFor(pack: Package.Compiled): Chain[String] = {
    val outputHex = CompileCache.outputHashHex(pack).getOrElse {
      fail("expected compiled package output hash")
    }
    Chain(
      "cache",
      "cas",
      "blake3",
      outputHex.take(2),
      s"${outputHex.drop(2)}.bosatsu_package"
    )
  }

  private def mainExpr(pack: Package.Compiled): TypedExpr[Region] =
    pack.lets.collectFirst {
      case (Identifier.Name("main"), _, expr) => expr
    }.getOrElse(fail(s"missing main in ${pack.name}"))

  @annotation.tailrec
  private def stripWrappers(
      expr: TypedExpr[Region]
  ): TypedExpr[Region] =
    expr match {
      case TypedExpr.Generic(_, in)       => stripWrappers(in)
      case TypedExpr.Annotation(in, _, _) => stripWrappers(in)
      case _                              => expr
    }

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

    val keyV1 =
      compileKey(consumer, SortedMap(depIfaceV1.name -> depIfaceV1))
    val keyV2 =
      compileKey(consumer, SortedMap(depIfaceV2.name -> depIfaceV2))

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

  test("generateKey requires dependencyHash lookup for dependency interfaces") {
    val consumerSource =
      """package Cache/App
        |from Cache/Dep import dep
        |main = dep
        |""".stripMargin
    val depSource =
      """package Cache/Dep
        |export dep
        |dep = 1
        |""".stripMargin

    val consumer = parsePackage(consumerSource)
    val depIface = Package.interfaceOf(compilePackage(depSource))
    val isolatedCache = CompileCache.filesystem(cacheDir, platform)
    val depHashes = SortedMap(
      depIface.name -> CompileCache.interfaceHash(depIface).getOrElse {
        fail("failed to compute dependency interface hash")
      }
    )

    val missingLookup =
      isolatedCache
        .generateKey(
          consumer.name,
          CompileCache.sourceExprHash(consumer),
          depHashes,
          CompileOptions.Default,
          "compiler-id",
          "phase-id"
        )
        .run(MemoryMain.State.empty)
    assert(missingLookup.isLeft)

    val (stateWithLookup, _) = runF(isolatedCache.dependencyHash(depIface))
    val seededKey =
      runF(
        isolatedCache.generateKey(
          consumer.name,
          CompileCache.sourceExprHash(consumer),
          depHashes,
          CompileOptions.Default,
          "compiler-id",
          "phase-id"
        ),
        stateWithLookup
      )._2

    val helperKey =
      compileKey(consumer, SortedMap(depIface.name -> depIface))

    assertEquals(
      CompileCache.keyHashHex(seededKey),
      CompileCache.keyHashHex(helperKey)
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

    val keyPath = keyPathFor(key)
    val corruptState = stateWithCache
      .withFile(keyPath, MemoryMain.FileContent.Str("invalid-hash-ident"))
      .getOrElse(fail(s"failed to write corrupt link at $keyPath"))

    val (_, missFromCorruptLink) = runF(cache.get(key), corruptState)
    assertEquals(missFromCorruptLink, None)

    val casPath = casPathFor(compiled)
    val missingCasState = stateWithCache.remove(casPath)

    val (_, missFromMissingCas) = runF(cache.get(key), missingCasState)
    assertEquals(missFromMissingCas, None)
  }

  test("cached packages with imports decode on warm hits") {
    val depSource =
      """package Cache/Dep
        |export dep
        |dep = 1
        |""".stripMargin
    val appSource =
      """package Cache/App
        |from Cache/Dep import dep
        |main = dep.add(1)
        |""".stripMargin

    val depCompiled = compilePackage(depSource)
    val depIface = Package.interfaceOf(depCompiled)
    val appParsed = parsePackage(appSource)
    val appCompiled = compilePackage(appSource, depIfaces = depIface :: Nil)
    val key = compileKey(appParsed, SortedMap(depIface.name -> depIface))

    val (stateWithCache, _) = runF(cache.put(key, appCompiled))
    val (_, warmHit) = runF(cache.get(key), stateWithCache)

    assert(warmHit.nonEmpty, "expected warm cache hit for package with imports")
    assertEquals(warmHit, Some(appCompiled))
    assertEquals(
      warmHit.map(pack => HasRegion.region(mainExpr(pack))),
      Some(HasRegion.region(mainExpr(appCompiled)))
    )
  }

  test("warm hits preserve branch pattern regions") {
    val source =
      """package Cache/Match
        |export select
        |def select(opt: Option[Int]) -> Int:
        |  match opt:
        |    case Some(v): v
        |    case None: 0
        |""".stripMargin

    val parsed = parsePackage(source)
    val compiled = compilePackage(source)
    val key = compileKey(parsed)
    val (stateWithCache, _) = runF(cache.put(key, compiled))
    val (_, warmHit) = runF(cache.get(key), stateWithCache)
    val warmPack = warmHit.getOrElse(fail("expected warm cache hit"))

    def patternRegions(
        pack: Package.Compiled
    ): NonEmptyList[Region] =
      pack.lets.collectFirst {
        case (Identifier.Name("select"), _, expr) => expr
      }.map(stripWrappers).getOrElse(fail(s"missing select in ${pack.name}")) match {
        case TypedExpr.AnnotatedLambda(_, body, _) =>
          stripWrappers(body) match {
            case TypedExpr.Match(_, branches, _) => branches.map(_.patternRegion)
            case other                           =>
              fail(s"expected select body to be a match expression, got $other")
          }
        case other =>
          fail(s"expected select to be a lambda, got $other")
      }

    val originalRegions = patternRegions(compiled)
    assert(originalRegions.exists(r => !r.eqv(Region.empty)))
    assertEquals(patternRegions(warmPack), originalRegions)
  }

  test("old schema entries are ignored after the cache schema bump") {
    val source =
      """package Cache/Foo
        |main = 1
        |""".stripMargin
    val parsed = parsePackage(source)
    val compiled = compilePackage(source)
    val currentKey = compileKey(parsed)
    val oldKey = currentKey.copy(schemaVersion = 1)
    val outputHex = CompileCache.outputHashHex(compiled).getOrElse {
      fail("expected compiled package output hash")
    }
    val oldState = MemoryMain.State.empty
      .withFile(
        casPathFor(compiled),
        MemoryMain.FileContent.Packages(List(compiled))
      )
      .getOrElse(fail("failed to seed legacy cas entry"))
      .withFile(
        keyPathFor(oldKey),
        MemoryMain.FileContent.Str(outputHex)
      )
      .getOrElse(fail("failed to seed legacy key entry"))

    val (_, miss) = runF(cache.get(currentKey), oldState)
    assertEquals(miss, None)
  }
}
