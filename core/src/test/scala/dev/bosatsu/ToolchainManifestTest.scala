package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.data.{Chain, NonEmptyList}
import dev.bosatsu.hashing.Algo
import dev.bosatsu.library.Version
import dev.bosatsu.tool.ToolchainManifest
import java.nio.charset.StandardCharsets

/** Run the detached manifest verifier on both the JVM and Scala.js through
  * the same in-memory file platform used by file-based commands.
  */
class ToolchainManifestTest extends munit.FunSuite {
  private type Result[A] = Either[Throwable, A]
  private type Path = Chain[String]

  private val platform = MemoryMain.memoryPlatformIO[Result]
  private val manifestPath: Path = Chain.one("manifest.json")
  private val compilerPath: Path = Chain.one("compiler.jar")
  private val corePath: Path = Chain.one("core.bosatsu_lib")
  private val runtimePath: Path = Chain.one("runtime.tar.gz")
  private val compilerBytes = bytes("candidate compiler bytes")
  private val runtimeBytes = bytes("compressed runtime bytes")
  private val version = Version.parser.parseAll("1.2.3").toOption.get
  private val compilerHash = sha(compilerBytes)
  private val runtimeHash = sha(runtimeBytes)

  private def bytes(value: String): Array[Byte] = value.getBytes(StandardCharsets.UTF_8)
  private def sha(value: Array[Byte]): String = Algo.hashBytes[Algo.Sha256](value).hex

  private def library(name: String = "core_alpha", ver: Version = version, repo: String = "repo"):
      Array[Byte] =
    proto.Library(
      name = name,
      repoUri = repo,
      descriptor = Some(proto.LibDescriptor(version = Some(ver.toProto)))
    ).toByteArray

  private def manifest(
      compiler: Array[Byte] = compilerBytes,
      core: Array[Byte] = bytes("core pin"),
      coreVersion: Version = version,
      runtime: Array[Byte] = runtimeBytes,
      sources: List[(String, Array[Byte])] = Nil
  ): String = {
    val sourceField =
      if (sources.isEmpty) ""
      else {
        val entries = sources.map { case (name, source) => s"\"$name\":\"${sha(source)}\"" }.mkString(",")
        s",\"core_sources\":{$entries}"
      }
    s"""{"schema_version":1,
       |"compiler":{"uri":"https://unused.invalid/compiler.jar","sha256":"${sha(compiler)}"},
       |"core":{"library_name":"core_alpha","version":"${coreVersion.render}","uri":"https://unused.invalid/core.lib","sha256":"${sha(core)}"},
       |"c_runtime":{"uri":"https://unused.invalid/runtime.tar.gz","sha256":"${sha(runtime)}"}$sourceField}""".stripMargin
  }

  private def run(
      manifestJson: String,
      files: List[(Path, Array[Byte])],
      core: ToolchainManifest.CoreSelection[Path] = ToolchainManifest.CoreSelection.None,
      runtime: Option[Path] = None,
      advertisedRuntimeHash: String = ""
  ): Result[Unit] =
    runBytes(bytes(manifestJson), files, core, runtime, advertisedRuntimeHash)

  private def runBytes(
      manifestBytes: Array[Byte],
      files: List[(Path, Array[Byte])],
      core: ToolchainManifest.CoreSelection[Path] = ToolchainManifest.CoreSelection.None,
      runtime: Option[Path] = None,
      advertisedRuntimeHash: String = ""
  ): Result[Unit] = {
    val initial = MemoryMain.State.empty
      .withFile(manifestPath, MemoryMain.FileContent.Bytes(manifestBytes)).get
    val state = files.foldLeft(initial) { case (next, (path, content)) =>
      next.withFile(path, MemoryMain.FileContent.Bytes(content)).get
    }
    (for {
      selected <- ToolchainManifest.read(platform, manifestPath)
      _ <- ToolchainManifest.verify(
        platform,
        selected,
        compilerPath,
        core,
        runtime,
        advertisedRuntimeHash
      )
    } yield ()).runA(state)
  }

  private def commonFiles: List[(Path, Array[Byte])] =
    List(compilerPath -> compilerBytes)

  private def checked(result: Result[Unit]): Unit =
    result.fold(err => fail(Option(err.getMessage).getOrElse(err.toString)), identity)

  private def rejects(result: Result[Unit], expected: String*): Unit = {
    val message = result.swap.toOption.flatMap(err => Option(err.getMessage))
      .getOrElse(fail("accepted invalid toolchain input"))
    expected.foreach(part => assert(message.contains(part), message))
  }

  test("compiler-only verification does not open unused core or runtime inputs") {
    checked(run(
      manifest(),
      commonFiles,
      advertisedRuntimeHash = "not a runtime hash"
    ))
  }

  test("each requested component is checked against the selected manifest bytes") {
    val core = library()
    checked(run(
      manifest(core = core),
      commonFiles :+ (corePath -> core),
      ToolchainManifest.CoreSelection.Binary(corePath)
    ))
    checked(run(
      manifest(),
      commonFiles :+ (runtimePath -> runtimeBytes),
      runtime = Some(runtimePath)
    ))

    val changedCompiler = bytes("another independently hashed compiler")
    rejects(run(manifest(), List(compilerPath -> changedCompiler)), "compiler jar SHA-256 mismatch")

    val selectedCore = library(repo = "selected")
    val substituteCore = library(repo = "substitute")
    rejects(run(
      manifest(core = selectedCore),
      commonFiles :+ (corePath -> substituteCore),
      ToolchainManifest.CoreSelection.Binary(corePath)
    ), "core library SHA-256 mismatch")

    val pinnedSource = bytes("package Bosatsu/Prog\n\nmain = 1\n")
    val replacementSource = bytes("package Bosatsu/Prog\n\nmain = 2\n")
    rejects(run(
      manifest(sources = List("Bosatsu/Prog" -> pinnedSource)),
      commonFiles :+ (Chain.one("Prog.bosatsu") -> replacementSource),
      ToolchainManifest.CoreSelection.Sources(NonEmptyList.one(Chain.one("Prog.bosatsu")))
    ), "core source SHA-256 mismatch")

    val changedRuntime = bytes("different compressed archive")
    rejects(run(
      manifest(),
      commonFiles :+ (runtimePath -> changedRuntime),
      runtime = Some(runtimePath)
    ), "runtime archive SHA-256 mismatch")
  }

  test("binary core identity is checked after its exact bytes match") {
    val sameIdentityChangedBytes = library(repo = "other-repo")
    val expected = library(repo = "expected-repo")
    rejects(run(
      manifest(core = expected),
      commonFiles :+ (corePath -> sameIdentityChangedBytes),
      ToolchainManifest.CoreSelection.Binary(corePath)
    ), "core library SHA-256 mismatch")

    val wrongName = library(name = "another_library")
    rejects(run(
      manifest(core = wrongName),
      commonFiles :+ (corePath -> wrongName),
      ToolchainManifest.CoreSelection.Binary(corePath)
    ), "core library identity mismatch")

    val wrongVersion = library(ver = Version.parser.parseAll("1.2.4").toOption.get)
    rejects(run(
      manifest(core = wrongVersion),
      commonFiles :+ (corePath -> wrongVersion),
      ToolchainManifest.CoreSelection.Binary(corePath)
    ), "core library version mismatch")

    val missingVersion = proto.Library(name = "core_alpha").toByteArray
    rejects(run(
      manifest(core = missingVersion),
      commonFiles :+ (corePath -> missingVersion),
      ToolchainManifest.CoreSelection.Binary(corePath)
    ), "has no version")

    val malformedLibrary = Array(0x0a.toByte, 0x80.toByte)
    rejects(run(
      manifest(core = malformedLibrary),
      commonFiles :+ (corePath -> malformedLibrary),
      ToolchainManifest.CoreSelection.Binary(corePath)
    ), "cannot parse core library")
  }

  test("binary core version identity preserves literal build identifiers") {
    val buildIdentifierPairs = List(("01", "1"), ("0002", "2"), ("09", "9"))

    buildIdentifierPairs.foreach { case (withLeadingZeros, withoutLeadingZeros) =>
      val leadingZeroVersion = Version.parser.parseAll(s"1.2.3+$withLeadingZeros").toOption.get
      val plainVersion = Version.parser.parseAll(s"1.2.3+$withoutLeadingZeros").toOption.get

      assertEquals(leadingZeroVersion.render, s"1.2.3+$withLeadingZeros")
      assertEquals(plainVersion.render, s"1.2.3+$withoutLeadingZeros")

      val leadingZeroLibrary = library(ver = leadingZeroVersion)
      val plainLibrary = library(ver = plainVersion)

      checked(run(
        manifest(core = leadingZeroLibrary, coreVersion = leadingZeroVersion),
        commonFiles :+ (corePath -> leadingZeroLibrary),
        ToolchainManifest.CoreSelection.Binary(corePath)
      ))
      checked(run(
        manifest(core = plainLibrary, coreVersion = plainVersion),
        commonFiles :+ (corePath -> plainLibrary),
        ToolchainManifest.CoreSelection.Binary(corePath)
      ))
      rejects(run(
        manifest(core = plainLibrary, coreVersion = leadingZeroVersion),
        commonFiles :+ (corePath -> plainLibrary),
        ToolchainManifest.CoreSelection.Binary(corePath)
      ), "core library version mismatch")
      rejects(run(
        manifest(core = leadingZeroLibrary, coreVersion = plainVersion),
        commonFiles :+ (corePath -> leadingZeroLibrary),
        ToolchainManifest.CoreSelection.Binary(corePath)
      ), "core library version mismatch")
    }
  }

  test("source mode validates package headers and hashes from each requested source") {
    val source = bytes("package Bosatsu/Prog\n\nmain = 1\n")
    val sourcePath = Chain.one("Prog.bosatsu")
    val sources = ToolchainManifest.CoreSelection.Sources(NonEmptyList.one(sourcePath))
    checked(run(
      manifest(sources = List("Bosatsu/Prog" -> source)),
      commonFiles :+ (sourcePath -> source),
      sources
    ))

    val changedPackage = bytes("package Bosatsu/Changed\n\nmain = 1\n")
    rejects(run(
      manifest(sources = List("Bosatsu/Prog" -> source)),
      commonFiles :+ (sourcePath -> changedPackage),
      sources
    ), "unlisted package Bosatsu/Changed")

    rejects(run(
      manifest(sources = List("Bosatsu/Prog" -> source)),
      commonFiles :+ (sourcePath -> Array(0xc3.toByte, 0x28.toByte)),
      sources
    ), "invalid UTF-8 in core source")

    val otherPath = Chain.one("Prog-copy.bosatsu")
    rejects(run(
      manifest(sources = List("Bosatsu/Prog" -> source)),
      commonFiles ++ List(sourcePath -> source, otherPath -> source),
      ToolchainManifest.CoreSelection.Sources(NonEmptyList.of(sourcePath, otherPath))
    ), "duplicate core source providers", "Bosatsu/Prog")
  }

  test("runtime archive enforces both the manifest SHA-256 and any advertised hash") {
    val files = commonFiles :+ (runtimePath -> runtimeBytes)
    val selected = manifest()
    checked(run(selected, files, runtime = Some(runtimePath)))
    checked(run(
      selected,
      files,
      runtime = Some(runtimePath),
      advertisedRuntimeHash = s"sha256:$runtimeHash"
    ))
    checked(run(
      selected,
      files,
      runtime = Some(runtimePath),
      advertisedRuntimeHash = Algo.hashBytes[Algo.Blake3](runtimeBytes).toIdent(using Algo.blake3Algo)
    ))
    checked(run(selected, commonFiles, advertisedRuntimeHash = "malformed but unused"))

    rejects(run(
      selected,
      files,
      runtime = Some(runtimePath),
      advertisedRuntimeHash = "blake3:bad"
    ), "invalid advertised runtime hash")

    val other = bytes("independently valid but conflicting archive pin")
    val blake3 = Algo.hashBytes[Algo.Blake3](other).toIdent(using Algo.blake3Algo)
    rejects(run(
      selected,
      files,
      runtime = Some(runtimePath),
      advertisedRuntimeHash = blake3
    ), "runtime archive hash mismatch")
  }

  test("missing requested components fail while absent unrequested components are tolerated") {
    rejects(run(
      manifest(),
      commonFiles,
      ToolchainManifest.CoreSelection.Binary(corePath)
    ), "cannot read core library", "core.bosatsu_lib")
    rejects(run(manifest(), commonFiles, runtime = Some(runtimePath)), "cannot read runtime archive")
  }

  test("manifest parsing rejects malformed UTF-8, duplicate fields, unknown fields, and bad pins") {
    rejects(runBytes(Array(0xc3.toByte, 0x28.toByte), commonFiles), "invalid UTF-8 in manifest")

    val valid = manifest()
    rejects(run(valid.replace("\"schema_version\":1", "\"schema_version\":1,\"schema_version\":1"), commonFiles),
      "repeated JSON fields")
    rejects(run(valid.replace("\"compiler\":{", "\"extra\":true,\"compiler\":{"), commonFiles),
      "unknown toolchain manifest fields")
    rejects(run(valid.replace("\"uri\":\"https://unused.invalid/compiler.jar\",", ""), commonFiles),
      "compiler", "uri")
    rejects(run(valid.replace(compilerHash, "x" * 64), commonFiles), "compiler", "string parser error")
    rejects(run(valid.replace("\"version\":\"1.2.3\"", "\"version\":\"not-semver\""), commonFiles),
      "version", "semver")

    val source = bytes("package Bosatsu/Prog\n")
    val sourceHash = sha(source)
    val duplicateSources = valid.stripSuffix("}") +
      s",\"core_sources\":{\"Bosatsu/Prog\":\"$sourceHash\",\"Bosatsu/Prog\":\"$sourceHash\"}}"
    rejects(run(duplicateSources, commonFiles), "repeated core source packages", "Bosatsu/Prog")
  }
}
