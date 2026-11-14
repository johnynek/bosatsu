package dev.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.data.Chain
import dev.bosatsu.{MemoryMain, PlatformIO}
import dev.bosatsu.hashing.Algo
import org.scalacheck.{Gen, Prop}
import java.nio.charset.StandardCharsets

class VerifiedLibrarySnapshotTest extends munit.ScalaCheckSuite {
  private type Result[A] = Either[Throwable, A]
  private type Path = Chain[String]

  private val platform = MemoryMain.memoryPlatformIO[Result]
  private val path: Path = Chain.one("pinned/lib.bosatsu_lib")
  private val version = Version(1, 2, 3)
  private var pinnedReads = 0

  private val countingPlatform = new PlatformIO[MemoryMain.StateF[Result], Path] {
    type FileKey = platform.FileKey
    export platform.{
      moduleIOMonad,
      parallelF,
      canPromiseF,
      pathArg,
      pathOrdering,
      withEC,
      pathToString,
      readUtf8,
      readPackages,
      readLibrary,
      fetchHash,
      fsDataType,
      pendingOutputPath,
      withTempDirectoryIn,
      resolve,
      relativize,
      parent,
      unfoldDir,
      hasExtension,
      writeDoc,
      writeDocAtomic,
      writeStdout,
      writeError,
      readStdinBytes,
      writeStdoutBytes,
      system,
      systemStdout,
      env,
      hostOs,
      hostArch,
      gitShaHead,
      println,
      errorln,
      writeInterfaces,
      writeFileSetAtomic,
      writeBytes,
      writeLibrary,
      writePackages,
      withTempPrefix
    }

    override def fileKeyHash = platform.fileKeyHash
    override def existingFileKey(path: Path) = platform.existingFileKey(path)

    override def readBytes(path: Path): MemoryMain.StateF[Result][Array[Byte]] = {
      pinnedReads += 1
      platform.readBytes(path)
    }
  }

  private def library(
      repoUri: String = "https://example.invalid/repo",
      libraryVersion: Version = version
  ): Array[Byte] =
    proto.Library(
      name = "sample",
      repoUri = repoUri,
      descriptor = Some(proto.LibDescriptor(version = Some(libraryVersion.toProto)))
    ).toByteArray

  private def pin(name: String, bytes: Array[Byte]): String =
    if (name == "blake3") s"blake3:${Algo.hashBytes[Algo.Blake3](bytes).hex}"
    else s"sha256:${Algo.hashBytes[Algo.Sha256](bytes).hex}"

  private def verify(
      bytes: Array[Byte],
      hashes: List[String],
      expectedName: Name = Name("sample"),
      expectedVersion: Version = version
  ): Result[VerifiedLibrarySnapshot[Path]] = {
    val state = MemoryMain.State.empty
      .withFile(path, MemoryMain.FileContent.Bytes(bytes)).get
    VerifiedLibrarySnapshot
      .verify(platform, path, expectedName, expectedVersion, hashes)
      .runA(state)
  }

  private def rejects(result: Result[?], messageParts: String*): Unit = {
    val message = result.swap.toOption.flatMap(error => Option(error.getMessage))
      .getOrElse(fail("accepted unauthenticated library input"))
    messageParts.foreach(part => assert(message.contains(part), message))
  }

  property("supported pins authenticate one exact identity and altered bytes fail") {
    Prop.forAll(Gen.alphaStr) { repoUri =>
      val bytes = library(repoUri)
      val altered = library(repoUri + "/changed")
      val pins = List(pin("sha256", bytes), pin("blake3", bytes))

      val snapshot = verify(bytes, pins).toOption.get
      assertEquals(snapshot.name, Name("sample"))
      assertEquals(snapshot.version, version)
      assertEquals(snapshot.sha256.hex, Algo.hashBytes[Algo.Sha256](bytes).hex)
      assertEquals(snapshot.blake3.hex, Algo.hashBytes[Algo.Blake3](bytes).hex)
      assertEquals(snapshot.protoLib.repoUri, repoUri)
      assert(verify(bytes, pins.reverse :+ pins.head).isRight)
      assert(verify(bytes, pins :+ "future:unrecognized-digest").isRight)
      assert(verify(altered, pins).isLeft)
      val alteredSnapshot = verify(altered, List(pin("sha256", altered))).toOption.get
      assert(!snapshot.hasSameBytes(alteredSnapshot))
      assert(snapshot.hasSameBytes(verify(bytes, pins).toOption.get))
    }
  }

  test("BLAKE3-only and SHA-256-only pins are supported") {
    val bytes = library()
    assert(verify(bytes, List(pin("blake3", bytes))).isRight)
    assert(verify(bytes, List(pin("sha256", bytes))).isRight)
  }

  test("empty, unsupported-only, malformed, and conflicting supported pins fail") {
    val bytes = library()
    rejects(verify(bytes, Nil), "no expected hashes", "pinned/lib.bosatsu_lib")
    rejects(
      verify(bytes, List("future:1234")),
      "no supported expected hashes",
      "pinned/lib.bosatsu_lib"
    )
    rejects(
      verify(bytes, List("sha256:" + ("g" * 64))),
      "malformed sha256 hash",
      "pinned/lib.bosatsu_lib"
    )
    rejects(
      verify(bytes, List(pin("sha256", bytes), "blake3:" + ("0" * 64))),
      "blake3 pin mismatch",
      "expected",
      "actual"
    )
  }

  test("embedded identity must match the caller's exact name and version") {
    val bytes = library()
    val hashes = List(pin("sha256", bytes))
    rejects(
      verify(bytes, hashes, expectedName = Name("other")),
      "name 'sample'",
      "expected 'other'"
    )
    rejects(
      verify(bytes, hashes, expectedVersion = Version(1, 2, 4)),
      "version 1.2.3",
      "expected 1.2.4"
    )
    val missingVersion = proto.Library(name = "sample").toByteArray
    rejects(
      verify(missingVersion, List(pin("sha256", missingVersion))),
      "has no version",
      "expected 1.2.3"
    )
  }

  test("correct pins do not authorize a colliding prerelease identity") {
    val actualVersion = Version.unsafe("1.2.3--0")
    val collidingExpectedVersion = Version.unsafe("1.2.3-0")
    val bytes = library(libraryVersion = actualVersion)
    val hashes = List(pin("sha256", bytes))

    rejects(
      verify(bytes, hashes, expectedVersion = collidingExpectedVersion),
      "version 1.2.3--0",
      "expected 1.2.3-0"
    )

    val snapshot = verify(bytes, hashes, expectedVersion = actualVersion).toOption.get
    val cachedReauthentication = snapshot.authenticateAs(
      "pinned/lib.bosatsu_lib",
      Name("sample"),
      collidingExpectedVersion,
      hashes
    )
    val failure = cachedReauthentication.swap.toOption.getOrElse(
      fail("accepted cached snapshot under a colliding prerelease identity")
    )
    assert(failure.contains("version 1.2.3--0"), failure)
    assert(failure.contains("expected 1.2.3-0"), failure)
  }

  test("a correctly pinned root with a malformed serialized version is rejected") {
    val malformedVersion = proto.Version(
      major = 1L,
      minor = 2L,
      patch = 3L,
      preRelease = "alpha."
    )
    val malformedLibrary = proto.Library(
      name = "sample",
      descriptor = Some(proto.LibDescriptor(version = Some(malformedVersion)))
    )
    val bytes = malformedLibrary.toByteArray
    rejects(
      verify(
        bytes,
        List(pin("sha256", bytes)),
        expectedVersion = Version.unsafe("1.2.3-alpha")
      ),
      "invalid version",
      "pinned/lib.bosatsu_lib"
    )
  }

  test("malformed protobuf and missing files fail with path provenance") {
    val malformed = "not a protobuf".getBytes(StandardCharsets.UTF_8)
    rejects(
      verify(malformed, List(pin("sha256", malformed))),
      "pinned/lib.bosatsu_lib",
      "cannot parse"
    )

    val missing = VerifiedLibrarySnapshot
      .verify(platform, path, Name("sample"), version, List("blake3:" + ("0" * 64)))
      .runA(MemoryMain.State.empty)
    rejects(missing, "pinned/lib.bosatsu_lib", "cannot read")
  }

  test("pinned library verification reads the artifact once") {
    val bytes = library("single-read")
    val state = MemoryMain.State.empty
      .withFile(path, MemoryMain.FileContent.Bytes(bytes)).get
    pinnedReads = 0

    val result = VerifiedLibrarySnapshot
      .verify(countingPlatform, path, Name("sample"), version, List(pin("sha256", bytes)))
      .runA(state)

    assert(result.isRight)
    assertEquals(pinnedReads, 1)
    assertEquals(result.toOption.get.protoLib.repoUri, "single-read")
  }
}
