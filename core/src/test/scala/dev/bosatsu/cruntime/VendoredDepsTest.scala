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

  private def metadata(
      name: String,
      staticLibs: List[String],
      systemLinkFlags: List[String]
  ): CDeps.Metadata =
    CDeps.Metadata(
      schema_version = CDeps.SchemaVersion,
      name = name,
      version = "1.0.0",
      recipe = s"$name-recipe",
      source_hash = hash.toIdent,
      build_key = s"$name-build-key",
      dependencies = None,
      target = CDeps.Target("linux", "x86_64", "clang", "clang 17"),
      prefix = s"/deps/$name",
      include_dirs = s"/deps/$name/include" :: Nil,
      static_libs = staticLibs,
      system_link_flags = systemLinkFlags,
      runtime_requirements = CDeps.RuntimeRequirements(Nil, Nil)
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

  test("libuv runtime requirements are empty") {
    assertEquals(
      VendoredDeps.runtimeRequirementsFor(
        dependency("libuv", CDeps.LibuvCmakeStatic)
      ),
      CDeps.RuntimeRequirements(Nil, Nil)
    )
  }

  test("pkg-config parsing filters bdwgc self library and keeps system flags") {
    val content =
      """prefix=/tmp/prefix
        |libdir=${prefix}/lib
        |includedir=${prefix}/include
        |Libs: -L${libdir} -lgc
        |Libs.private: -pthread -ldl
        |Cflags: -I${includedir}
        |""".stripMargin

    assertEquals(
      VendoredDeps.parsePkgConfigSystemFlags(content, "libgc.a"),
      "-pthread" :: "-ldl" :: Nil
    )
  }

  test("pkg-config parsing filters libuv self library spellings") {
    val content =
      """prefix=/tmp/prefix
        |libdir=${prefix}/lib
        |Libs: -L${libdir} -luv -l:libuv.a ${libdir}/libuv.a
        |Libs.private: -pthread -ldl -lrt -lsocket
        |""".stripMargin

    assertEquals(
      VendoredDeps.parsePkgConfigSystemFlags(content, "libuv.a"),
      "-pthread" :: "-ldl" :: "-lrt" :: "-lsocket" :: Nil
    )
  }

  test("BuildInputs link flags place dependent archives before dependency archives") {
    val bdwgc = dependency("bdwgc", CDeps.BdwgcCmakeStatic)
    val libuv = dependency("libuv", CDeps.LibuvCmakeStatic)
    val bdwgcStaticLib = "/cache/bdwgc/prefix/lib/libgc.a"
    val libuvStaticLib = "/cache/libuv/prefix/lib/libuv.a"
    val inputs =
      VendoredDeps.BuildInputs(
        CDeps.Manifest(1, 1, bdwgc :: libuv :: Nil),
        VendoredDeps.ResolvedDependency(
          bdwgc,
          "bdwgc-key",
          "/cache/bdwgc",
          metadata("bdwgc", bdwgcStaticLib :: Nil, "-pthread" :: Nil)
        ) ::
          VendoredDeps.ResolvedDependency(
            libuv,
            "libuv-key",
            "/cache/libuv",
            metadata("libuv", libuvStaticLib :: Nil, "-ldl" :: Nil)
          ) ::
          Nil
      )

    assertEquals(
      inputs.linkFlags,
      libuvStaticLib :: bdwgcStaticLib :: "-ldl" :: "-pthread" :: Nil
    )
  }

  property("BuildInputs link flags reverse topological dependency order within each flag class") {
    val nameGen =
      Gen
        .choose(2, 6)
        .map(count => List.tabulate(count)(idx => s"dep$idx"))

    forAll(nameGen) { names =>
      val resolved =
        names.map { name =>
          VendoredDeps.ResolvedDependency(
            dependency(name, "custom-recipe"),
            s"$name-key",
            s"/cache/$name",
            metadata(
              name,
              s"/cache/$name/prefix/lib/lib$name.a" :: Nil,
              s"-l${name}sys" :: Nil
            )
          )
        }
      val inputs =
        VendoredDeps.BuildInputs(
          CDeps.Manifest(1, 1, resolved.map(_.dependency)),
          resolved
        )

      assertEquals(
        inputs.linkFlags,
        names.reverse.map(name => s"/cache/$name/prefix/lib/lib$name.a") :::
          names.reverse.map(name => s"-l${name}sys")
      )
    }
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

  test("libuv configure args keep common static CMake switches") {
    val args =
      VendoredDeps.libuvConfigureArgs(
        profile = "release",
        sourceRoot = "/tmp/src",
        buildDir = "/tmp/build",
        prefix = "/tmp/prefix",
        inheritedCFlags = Some("-O2 -g")
      )

    assertEquals(
      args,
      List(
        "-S",
        "/tmp/src",
        "-B",
        "/tmp/build",
        "-DCMAKE_BUILD_TYPE=Release",
        "-DCMAKE_INSTALL_PREFIX=/tmp/prefix",
        "-DCMAKE_C_FLAGS=-O2 -g",
        "-DLIBUV_BUILD_SHARED=OFF",
        "-DBUILD_TESTING=OFF",
        "-DLIBUV_BUILD_TESTS=OFF",
        "-DLIBUV_BUILD_BENCH=OFF"
      )
    )
  }

  test("libuv configure args use the expected CMake build type for debug and release") {
    val debugArgs =
      VendoredDeps.libuvConfigureArgs(
        profile = "debug",
        sourceRoot = "/tmp/src",
        buildDir = "/tmp/build",
        prefix = "/tmp/prefix",
        inheritedCFlags = None
      )
    val releaseArgs =
      VendoredDeps.libuvConfigureArgs(
        profile = "release",
        sourceRoot = "/tmp/src",
        buildDir = "/tmp/build",
        prefix = "/tmp/prefix",
        inheritedCFlags = None
      )

    assert(debugArgs.contains("-DCMAKE_BUILD_TYPE=Debug"))
    assert(releaseArgs.contains("-DCMAKE_BUILD_TYPE=Release"))
    assertEquals(cFlagsArg(debugArgs), None)
    assertEquals(cFlagsArg(releaseArgs), None)
  }

  test("libuv configure args do not inject GC-specific flags") {
    val args =
      VendoredDeps.libuvConfigureArgs(
        profile = "release",
        sourceRoot = "/tmp/src",
        buildDir = "/tmp/build",
        prefix = "/tmp/prefix",
        inheritedCFlags = Some("-O2")
      )

    assert(!args.exists(_.contains("-DGC_THREADS")))
    assert(!args.exists(_.contains(noDescCatchExceptionRaise)))
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

  private val systemLinkFlagGen =
    Gen.oneOf(
      "-pthread",
      "-ldl",
      "-lrt",
      "-lsocket",
      "-lkstat",
      "-lsendfile",
      "-lws2_32",
      "-lpsapi",
      "-liphlpapi",
      "-luserenv"
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

  property("libuv configure args preserve inherited CFLAGS once without GC flags") {
    forAll(Gen.nonEmptyListOf(safeCFlagTokenGen)) { tokens =>
      val inherited = tokens.mkString(" ")
      val args =
        VendoredDeps.libuvConfigureArgs(
          profile = "release",
          sourceRoot = "/tmp/src",
          buildDir = "/tmp/build",
          prefix = "/tmp/prefix",
          inheritedCFlags = Some(inherited)
        )
      val cFlagsArgs =
        args.filter(_.startsWith("-DCMAKE_C_FLAGS="))
      val actualTokens =
        cFlagsArgs.flatMap(_.stripPrefix("-DCMAKE_C_FLAGS=").split("\\s+"))

      assertEquals(cFlagsArgs.size, 1)
      assertEquals(actualTokens, tokens)
      assert(!actualTokens.contains("-DGC_THREADS"))
      assert(!actualTokens.contains(noDescCatchExceptionRaise))
    }
  }

  property("pkg-config parsing preserves distinct system flags and filters self libraries") {
    val flagGen =
      for {
        staticLib <- Gen.oneOf("libgc.a", "libuv.a")
        systemFlags <- Gen.listOf(systemLinkFlagGen)
        selfFlags = {
          val stem = staticLib.stripPrefix("lib").stripSuffix(".a")
          List(s"-l$stem", s"-l:$staticLib", s"/tmp/prefix/lib/$staticLib")
        }
      } yield (staticLib, systemFlags, selfFlags)

    forAll(flagGen) { case (staticLib, systemFlags, selfFlags) =>
      val content =
        s"""prefix=/tmp/prefix
           |libdir=$${prefix}/lib
           |Libs: -L$${libdir} ${selfFlags.mkString(" ")}
           |Libs.private: ${systemFlags.mkString(" ")}
           |""".stripMargin

      assertEquals(
        VendoredDeps.parsePkgConfigSystemFlags(content, staticLib),
        systemFlags.distinct
      )
    }
  }
}
