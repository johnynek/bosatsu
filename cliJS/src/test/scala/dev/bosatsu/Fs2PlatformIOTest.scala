package dev.bosatsu

import cats.effect.IO
import cats.effect.Ref
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import dev.bosatsu.hashing.Algo
import dev.bosatsu.library.Version
import dev.bosatsu.tool.{ExitCode, Output}
import fs2.io.file.{Files, Path}
import munit.FunSuite
import org.typelevel.paiges.Doc
import java.io.IOException
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import _root_.bosatsu.{TypedAst => proto}

@js.native
private trait DocumentStat extends js.Object {
  val mode: Int = js.native
  val uid: Int = js.native
  val gid: Int = js.native
  val ino: Double = js.native
}

@js.native
@JSImport("fs/promises", "stat")
private object DocumentStat extends js.Function1[String, js.Promise[DocumentStat]] {
  def apply(path: String): js.Promise[DocumentStat] = js.native
}

@js.native
@JSImport("fs/promises", "chmod")
private object DocumentChmod extends js.Function2[String, Int, js.Promise[Unit]] {
  def apply(path: String, mode: Int): js.Promise[Unit] = js.native
}

class Fs2PlatformIOTest extends FunSuite {
  test("deps rewrites changed output while retaining document permissions and inode") {
    val fs = Files.forIO
    val module = new MainModule(Fs2PlatformIO)
    fs.tempDirectory.use { dir =>
      val source = dir / "source.bosatsu"
      val output = dir / "deps.json"
      def run = IO.fromEither(module.run(List(
        "tool", "deps", "--input", source.toString, "--output", output.toString
      )).leftMap(h => new IllegalArgumentException(h.toString)))
        .flatten.flatMap(_.report(Fs2PlatformIO))
      def stat = IO.fromPromise(IO(DocumentStat(output.toString)))
        .map(s => (s.mode, s.uid, s.gid, s.ino))
      for {
        _ <- Fs2PlatformIO.writeDoc(source, Doc.text("package Example/One\nvalue = 1\n"))
        first <- run
        _ = assertEquals(first, ExitCode.Success)
        oldOutput <- Fs2PlatformIO.readUtf8(output)
        _ <- IO.fromPromise(IO(DocumentChmod(output.toString, 0x180)))
        before <- stat
        _ <- fs.writeUtf8(source)(fs2.Stream.emit("package Example/Two\nvalue = 2\n")).compile.drain
        second <- run
        _ = assertEquals(second, ExitCode.Success)
        newOutput <- Fs2PlatformIO.readUtf8(output)
        _ = assert(newOutput != oldOutput)
        _ = assert(newOutput.contains("Example/Two"), newOutput)
        after <- stat
        _ = assertEquals(after, before)
        paths <- fs.walk(dir).compile.toList
        _ = assertEquals(paths.toSet, Set(dir, source, output))
      } yield ()
    }.unsafeToFuture()
  }

  test("ordinary transpilation outputs overwrite writable documents in place") {
    val fs = Files.forIO
    List("600", "640", "644").traverse_ { permissions =>
      fs.tempDirectory.use { dir =>
        val target = dir / "generated.txt"
        def stat = IO.fromPromise(IO(DocumentStat(target.toString)))
          .map(s => (s.mode, s.uid, s.gid, s.ino))
        for {
          _ <- Fs2PlatformIO.writeDoc(target, Doc.text("old"))
          _ <- IO.fromPromise(IO(DocumentChmod(target.toString, Integer.parseInt(permissions, 8))))
          before <- stat
          result <- Output.TranspileOut(List(target -> Doc.text("changed"))).report(Fs2PlatformIO)
          _ = assertEquals(result, ExitCode.Success)
          content <- Fs2PlatformIO.readUtf8(target)
          _ = assertEquals(content, "changed")
          after <- stat
          _ = assertEquals(after, before)
          _ <- Fs2PlatformIO.writeDoc(target, Doc.text("retry"))
          retried <- Fs2PlatformIO.readUtf8(target)
          _ = assertEquals(retried, "retry")
          afterRetry <- stat
          _ = assertEquals(afterRetry, before)
          paths <- fs.walk(dir).compile.toList
          _ = assertEquals(paths.toSet, Set(dir, target))
        } yield ()
      }
    }.unsafeToFuture()
  }

  test("atomic document replacement preserves access controls or fails without changing the inode") {
    val fs = Files.forIO
    List("600", "640", "644", "400").traverse_ { permissions =>
      val mode = Integer.parseInt(permissions, 8)
      fs.tempDirectory.use { dir =>
        val target = dir / "document"
        def stat = IO.fromPromise(IO(DocumentStat(target.toString)))
          .map(s => (s.mode, s.uid, s.gid, s.ino))
        def assertTree = fs.walk(dir).compile.toList.map { paths =>
          assertEquals(paths.toSet, Set(dir, target))
        }
        for {
          _ <- fs.writeUtf8(target)(fs2.Stream.emit("sentinel")).compile.drain
          _ <- IO.fromPromise(IO(DocumentChmod(target.toString, mode)))
          before <- stat
          result <- Fs2PlatformIO.writeDocAtomic(target, Doc.text("replacement")).attempt
          _ = assert(result.swap.toOption.exists(_.getMessage.contains("cannot preserve output access controls")), result)
          after <- stat
          _ = assertEquals(after, before)
          content <- Fs2PlatformIO.readUtf8(target)
          _ = assertEquals(content, "sentinel")
          _ <- assertTree
          // An identical retry needs no inode replacement or ACL copying.
          _ <- Fs2PlatformIO.writeDocAtomic(target, Doc.text("sentinel"))
          afterRetry <- stat
          _ = assertEquals(afterRetry, before)
          _ <- assertTree
        } yield ()
      }
    }.unsafeToFuture()
  }

  test("atomic document publication handles nested and symlink destinations") {
    val fs = Files.forIO
    fs.tempDirectory.use { dir =>
      val nested = dir / "nested" / "stamp"
      val target = dir / "stamp-target"
      val link = dir / "stamp-link"
      val danglingTarget = dir / "new-target"
      val danglingLink = dir / "dangling-link"
      for {
        _ <- Fs2PlatformIO.writeDocAtomic(nested, Doc.text("nested"))
        nestedContent <- Fs2PlatformIO.readUtf8(nested)
        _ = assertEquals(nestedContent, "nested")
        _ <- Fs2PlatformIO.writeDocAtomic(target, Doc.text("old"))
        _ <- fs.createSymbolicLink(link, target)
        _ <- Fs2PlatformIO.writeDocAtomic(link, Doc.text("old"))
        rejected <- Fs2PlatformIO.writeDocAtomic(link, Doc.text("through link")).attempt
        _ = assert(rejected.isLeft, rejected)
        targetContent <- Fs2PlatformIO.readUtf8(target)
        linkIsSymlink <- fs.isSymbolicLink(link)
        _ = assertEquals(targetContent, "old")
        _ = assert(linkIsSymlink)
        _ <- fs.createSymbolicLink(danglingLink, danglingTarget)
        _ <- Fs2PlatformIO.writeDocAtomic(danglingLink, Doc.text("created target"))
        danglingContent <- Fs2PlatformIO.readUtf8(danglingTarget)
        danglingLinkIsSymlink <- fs.isSymbolicLink(danglingLink)
        _ = assertEquals(danglingContent, "created target")
        _ = assert(danglingLinkIsSymlink)
      } yield ()
    }.unsafeToFuture()
  }

  test("atomic document publication preserves destinations and cleans staging after writer or rename failure") {
    val fs = Files.forIO
    fs.tempDirectory.use { dir =>
      val existing = dir / "existing-stamp"
      val absent = dir / "absent-stamp"
      val normalOps = Fs2PlatformIO.documentWriteOps
      def stat = IO.fromPromise(IO(DocumentStat(existing.toString)))
        .map(s => (s.mode, s.uid, s.gid, s.ino))
      def assertPreserved(before: (Int, Int, Int, Double)) =
        fs.walk(dir).compile.toList.flatMap { paths =>
          assertEquals(paths.toSet, Set(dir, existing))
          stat.map(after => assertEquals(after, before))
        }

      val partialWriteOps = normalOps.copy(
        writeStaged = (staged, _) => {
          val partialThenFail: fs2.Stream[IO, String] =
            fs2.Stream.emit("partial").covary[IO] ++
              fs2.Stream.eval(IO.raiseError[String](new IOException("staged write failed")))
          fs.writeUtf8(staged)(partialThenFail).compile.drain
        }
      )

      for {
        _ <- Fs2PlatformIO.writeDocAtomic(existing, Doc.text("sentinel"))
        _ <- IO.fromPromise(IO(DocumentChmod(existing.toString, 0x180)))
        before <- stat
        failedWrite <- Fs2PlatformIO
          .writeDocWith(existing, Doc.text("replacement"), partialWriteOps)
          .attempt
        _ = assert(failedWrite.swap.toOption.exists(_.getMessage == "staged write failed"), failedWrite)
        existingAfterWriteFailure <- Fs2PlatformIO.readUtf8(existing)
        _ = assertEquals(existingAfterWriteFailure, "sentinel")
        _ <- assertPreserved(before)
        failedAbsentWrite <- Fs2PlatformIO
          .writeDocWith(absent, Doc.text("replacement"), partialWriteOps)
          .attempt
        _ = assert(failedAbsentWrite.isLeft, failedAbsentWrite)
        absentAfterWriteFailure <- fs.exists(absent)
        _ = assert(!absentAfterWriteFailure)
        _ <- assertPreserved(before)
        events <- Ref.of[IO, List[String]](Nil)
        closeFailureOps = normalOps.copy(
          writeStaged = (staged, doc) =>
            normalOps.writeStaged(staged, doc) *>
              events.update(_ :+ "closed").flatMap(_ =>
                IO.raiseError(new IOException("staged writer close failed"))
              ),
          replace = (staged, destination) =>
            events.update(_ :+ "replace") *> normalOps.replace(staged, destination)
        )
        failedClose <- Fs2PlatformIO
          .writeDocWith(existing, Doc.text("replacement"), closeFailureOps)
          .attempt
        _ = assert(failedClose.swap.toOption.exists(_.getMessage == "staged writer close failed"), failedClose)
        closeEvents <- events.get
        _ = assertEquals(closeEvents, List("closed"))
        existingAfterCloseFailure <- Fs2PlatformIO.readUtf8(existing)
        _ = assertEquals(existingAfterCloseFailure, "sentinel")
        _ <- assertPreserved(before)
        failedAbsentClose <- Fs2PlatformIO
          .writeDocWith(absent, Doc.text("replacement"), closeFailureOps)
          .attempt
        _ = assert(
          failedAbsentClose.swap.toOption.exists(_.getMessage == "staged writer close failed"),
          failedAbsentClose
        )
        absentAfterCloseFailure <- fs.exists(absent)
        _ = assert(!absentAfterCloseFailure)
        _ <- assertPreserved(before)
        replacementFailureOps = normalOps.copy(
          replace = (_, _) => IO.raiseError(new IOException("rename failed"))
        )
        failedReplace <- Fs2PlatformIO
          .writeDocWith(existing, Doc.text("replacement"), replacementFailureOps)
          .attempt
        _ = assert(failedReplace.swap.toOption.exists(_.getMessage == "rename failed"), failedReplace)
        existingAfterReplaceFailure <- Fs2PlatformIO.readUtf8(existing)
        _ = assertEquals(existingAfterReplaceFailure, "sentinel")
        _ <- assertPreserved(before)
        failedAbsentReplace <- Fs2PlatformIO
          .writeDocWith(absent, Doc.text("replacement"), replacementFailureOps)
          .attempt
        _ = assert(failedAbsentReplace.swap.toOption.exists(_.getMessage == "rename failed"), failedAbsentReplace)
        absentAfterReplaceFailure <- fs.exists(absent)
        _ = assert(!absentAfterReplaceFailure)
        _ <- assertPreserved(before)
        _ <- Fs2PlatformIO.writeDocAtomic(existing, Doc.text("sentinel"))
        _ <- assertPreserved(before)
        _ <- Fs2PlatformIO.writeDocAtomic(absent, Doc.text("retry succeeded"))
        afterRetry <- Fs2PlatformIO.readUtf8(absent)
        _ = assertEquals(afterRetry, "retry succeeded")
        paths <- fs.walk(dir).compile.toList
        _ = assertEquals(paths.toSet, Set(dir, existing, absent))
      } yield ()
    }.unsafeToFuture()
  }

  test("verify-toolchain runs portably and preserves a stamp after validation failure") {
    val fs = Files.forIO
    val module = new MainModule(Fs2PlatformIO)
    def run(args: String*): IO[ExitCode] =
      IO.fromEither(
        module.run("tool" :: args.toList).leftMap(h => new IllegalArgumentException(h.toString))
      )
        .flatten.flatMap(_.report(Fs2PlatformIO))
    fs.tempDirectory.use { dir =>
      val manifest = dir / "manifest.json"
      val compiler = dir / "compiler.jar"
      val stamp = dir / "stamp"
      val compilerBytes = "node candidate jar".getBytes("UTF-8")
      def sha(bytes: Array[Byte]): String = Algo.hashBytes[Algo.Sha256](bytes).hex
      val emptyHash = sha(Array.emptyByteArray)
      val manifestBytes =
        s"""{
           |"schema_version":1,
           |"compiler":{"uri":"unused","sha256":"${sha(compilerBytes)}"},
           |"core":{"library_name":"bosatsu","version":"1.2.3","uri":"unused","sha256":"$emptyHash"},
           |"c_runtime":{"uri":"unused","sha256":"$emptyHash"}
           |}""".stripMargin.getBytes("UTF-8")
      def verify: IO[ExitCode] = run(
        "verify-toolchain",
        "--manifest", manifest.toString,
        "--compiler-jar", compiler.toString,
        "--out", stamp.toString
      )
      for {
        _ <- Fs2PlatformIO.writeBytes(manifest, manifestBytes)
        _ <- Fs2PlatformIO.writeBytes(compiler, compilerBytes)
        valid <- verify.attempt
        _ = assertEquals(valid.toOption, Some(ExitCode.Success))
        stampContent <- Fs2PlatformIO.readUtf8(stamp)
        _ = assertEquals(stampContent, "ok")
        again <- verify
        _ = assertEquals(again, ExitCode.Success)
        _ <- Fs2PlatformIO.writeDoc(stamp, Doc.text("sentinel"))
        blocked <- verify.attempt
        _ = assert(
          blocked.swap.toOption.exists(_.getMessage.contains("cannot preserve output access controls")),
          blocked
        )
        afterBlocked <- Fs2PlatformIO.readUtf8(stamp)
        _ = assertEquals(afterBlocked, "sentinel")
        paths <- fs.walk(dir).compile.toList
        _ = assertEquals(paths.toSet, Set(dir, manifest, compiler, stamp))
        _ <- Fs2PlatformIO.writeBytes(compiler, "altered candidate".getBytes("UTF-8"))
        invalid <- verify.attempt
        _ = assert(
          invalid.swap.toOption.exists(_.getMessage.contains("compiler jar SHA-256 mismatch")),
          invalid
        )
        previous <- Fs2PlatformIO.readUtf8(stamp)
        _ = assertEquals(previous, "sentinel")
      } yield ()
    }.unsafeToFuture()
  }

  test("verify-toolchain rejects stamp aliases for every requested component") {
    val fs = Files.forIO
    val module = new MainModule(Fs2PlatformIO)
    def run(args: List[String]): IO[ExitCode] =
      IO.fromEither(module.run("tool" :: args).leftMap(h => new IllegalArgumentException(h.toString)))
        .flatten.flatMap(_.report(Fs2PlatformIO))
    fs.tempDirectory.use { dir =>
      val manifest = dir / "manifest.json"
      val compiler = dir / "compiler.jar"
      val firstSource = dir / "One.bosatsu"
      val secondSource = dir / "Two.bosatsu"
      val core = dir / "core.lib"
      val runtime = dir / "runtime.tar.gz"
      val compilerBytes = "node candidate jar".getBytes("UTF-8")
      val firstBytes = "package Example/One\n\nvalue = 1\n".getBytes("UTF-8")
      val secondBytes = "package Example/Two\n\nvalue = 2\n".getBytes("UTF-8")
      val coreBytes = proto.Library(
        name = "bosatsu",
        repoUri = "unused",
        descriptor = Some(proto.LibDescriptor(
          version = Some(Version.parser.parseAll("1.2.3").toOption.get.toProto)
        ))
      ).toByteArray
      val runtimeBytes = "node runtime archive".getBytes("UTF-8")
      def sha(bytes: Array[Byte]): String = Algo.hashBytes[Algo.Sha256](bytes).hex
      def manifestBytes(coreBytes: Array[Byte]): Array[Byte] = {
        val sourcePins =
          s"\"Example/One\":\"${sha(firstBytes)}\",\"Example/Two\":\"${sha(secondBytes)}\""
        s"""{
           |"schema_version":1,
           |"compiler":{"uri":"unused","sha256":"${sha(compilerBytes)}"},
           |"core":{"library_name":"bosatsu","version":"1.2.3","uri":"unused","sha256":"${sha(coreBytes)}"},
           |"c_runtime":{"uri":"unused","sha256":"${sha(runtimeBytes)}"},
           |"core_sources":{$sourcePins}
           |}""".stripMargin.getBytes("UTF-8")
      }
      def verifyArgs(
          out: Path,
          binaryCore: Option[Path] = None,
          sources: List[Path] = Nil,
          archive: Option[Path] = None
      ): List[String] =
        List(
          "verify-toolchain",
          "--manifest", manifest.toString,
          "--compiler-jar", compiler.toString,
          "--out", out.toString
        ) ++ binaryCore.toList.flatMap(path => List("--core-library", path.toString)) ++
          sources.flatMap(path => List("--core-source", path.toString)) ++
          archive.toList.flatMap(path => List("--runtime-archive", path.toString))
      def rejectedAlias(out: Path, requested: List[Path], command: List[String]): IO[Unit] = {
        val paths = (requested :+ out).distinct
        for {
          before <- paths.traverse(path => Fs2PlatformIO.readBytes(path).map(path.toString -> _.toList)).map(_.toMap)
          result <- run(command).attempt
          _ = assert(
            result.swap.toOption.exists(_.getMessage.contains("refers to requested input")),
            result
          )
          after <- paths.traverse(path => Fs2PlatformIO.readBytes(path).map(path.toString -> _.toList)).map(_.toMap)
          _ = assertEquals(after, before)
        } yield ()
      }
      val requestedSources = List(firstSource, secondSource)
      val sourceAndRuntimeInputs = List(manifest, compiler, firstSource, secondSource, runtime)
      for {
        _ <- Fs2PlatformIO.writeBytes(manifest, manifestBytes(coreBytes))
        _ <- Fs2PlatformIO.writeBytes(compiler, compilerBytes)
        _ <- Fs2PlatformIO.writeBytes(firstSource, firstBytes)
        _ <- Fs2PlatformIO.writeBytes(secondSource, secondBytes)
        _ <- Fs2PlatformIO.writeBytes(runtime, runtimeBytes)
        _ <- sourceAndRuntimeInputs.traverse_(path =>
          rejectedAlias(path, sourceAndRuntimeInputs, verifyArgs(
            path,
            sources = requestedSources,
            archive = Some(runtime)
          ))
        )
        link = dir / "compiler-link.jar"
        _ <- fs.createSymbolicLink(link, compiler)
        _ <- rejectedAlias(
          link,
          sourceAndRuntimeInputs,
          verifyArgs(link, sources = requestedSources, archive = Some(runtime))
        )
        _ <- Fs2PlatformIO.writeBytes(core, coreBytes)
        _ <- Fs2PlatformIO.writeBytes(manifest, manifestBytes(coreBytes))
        _ <- rejectedAlias(core, List(manifest, compiler, core), verifyArgs(core, binaryCore = Some(core)))
      } yield ()
    }.unsafeToFuture()
  }

  test("inspect-library authenticates descriptors without opening children on Scala.js") {
    val fs = Files.forIO
    val module = new MainModule(Fs2PlatformIO)
    val version = Version.parser.parseAll("1.2.3").toOption.get
    val missingChildHash = "blake3:" + ("a" * 64)
    val libraryBytes = proto.Library(
      name = "root",
      repoUri = "unused",
      descriptor = Some(proto.LibDescriptor(version = Some(version.toProto))),
      publicDependencies = List(proto.LibDependency(
        name = "missing_child",
        desc = Some(proto.LibDescriptor(
          version = Some(version.toProto),
          hashes = List(missingChildHash),
          uris = List("https://unused.invalid/missing-child")
        ))
      ))
    ).toByteArray
    val libraryHash = Algo.hashBytes[Algo.Blake3](libraryBytes).hex
    val librarySha = Algo.hashBytes[Algo.Sha256](libraryBytes).hex
    def run(args: List[String]): IO[ExitCode] =
      IO.fromEither(
        module.run("tool" :: args).leftMap(h => new IllegalArgumentException(h.toString))
      ).flatten.flatMap(_.report(Fs2PlatformIO))

    fs.tempDirectory.use { dir =>
      val input = dir / "root.lib"
      val context = dir / "context.json"
      val descriptors = dir / "descriptors.json"
      val signature = dir / "signature.bundle"
      val pinned = List(
        "inspect-library",
        "--input", input.toString,
        "--expected-name", "root",
        "--expected-version", version.render,
        "--expected-hash", s"blake3:$libraryHash",
        "--expected-hash", s"sha256:$librarySha"
      )
      val descriptorArgs = pinned ::: List(
        "--descriptors-only", "--descriptor-out", descriptors.toString
      )
      val fullArgs = pinned ::: List(
        "--dependency-context", context.toString,
        "--sig-out", signature.toString,
        "--descriptor-out", descriptors.toString
      )
      for {
        _ <- Fs2PlatformIO.writeBytes(input, libraryBytes)
        _ <- Fs2PlatformIO.writeDoc(
          context,
          Doc.text("{\"schema_version\":1,\"libraries\":[]}")
        )
        inspected <- run(descriptorArgs).attempt
        _ = assertEquals(inspected.toOption, Some(ExitCode.Success))
        descriptorJson <- Fs2PlatformIO.readUtf8(descriptors)
        _ = assert(descriptorJson.contains("missing_child"), descriptorJson)
        fullFailure <- run(fullArgs).attempt
        _ = assert(
          fullFailure.swap.toOption.exists(_.getMessage.contains("missing declared file")),
          fullFailure
        )
        after <- Fs2PlatformIO.readUtf8(descriptors)
        _ = assertEquals(after, descriptorJson)
        signatureExists <- fs.exists(signature)
        _ = assert(!signatureExists)
      } yield ()
    }.unsafeToFuture()
  }

  test("emit-c resolves pending output aliases and creates the declared header directory") {
    val fs = Files.forIO
    val module = new MainModule(Fs2PlatformIO)
    def run(args: String*): IO[ExitCode] =
      IO.fromEither(module.run("tool" :: args.toList).leftMap(h => new IllegalArgumentException(h.toString)))
        .flatten.flatMap(_.report(Fs2PlatformIO))
    fs.tempDirectory.use { dir =>
      val source = dir / "Native.bosatsu"
      val bundle = dir / "native.pkg"
      val manifest = dir / "runtime.json"
      val c = dir / "runner.c"
      val header = dir / "bosatsu_ext_Native.h"
      val target = dir / "pending"
      def emit(out: Path, h: Path): IO[ExitCode] = run("emit-c", "--runtime-manifest", manifest.toString,
        "--test-packages", bundle.toString, "--out", out.toString, "--extern-header-out", h.toString)
      val graph = Json.JObject(List(
        "schema_version" -> Json.JNumberStr("1"), "kind" -> Json.JString("local"),
        "packages" -> Json.JArray(Vector(Json.JString(bundle.toString))),
        "externals" -> Json.JArray(Vector(Json.JObject(List(
          "package" -> Json.JString("Native"), "symbols" -> Json.JArray(Vector(Json.JString("native_value")))
        ))))
      ))
      for {
        _ <- Fs2PlatformIO.writeDoc(source, Doc.text("package Native\nexternal def native_value(x: Int) -> Int\n" +
          "checks = Assertion(native_value(1) matches 1, \"ok\")\n"))
        _ <- run("compile-packages", "--src", source.toString, "--packages-out", bundle.toString,
          "--sig-out", (dir / "native.sig").toString)
        _ <- Fs2PlatformIO.writeDoc(manifest, graph.toDoc)
        _ <- fs.createSymbolicLink(c, target)
        _ <- fs.createSymbolicLink(header, target)
        bad <- emit(c, header).attempt
        _ = assert(bad.swap.exists(_.getMessage.contains("same file")), bad)
        created <- fs.exists(target)
        _ = assert(!created)
        caseDir = dir / "new"
        caseHeader = caseDir / "bosatsu_ext_Native.h"
        upper = caseDir / "BOSATSU_EXT_NATIVE.H"
        insensitive <- fs.exists(dir / "NATIVE.PKG")
        collision <- emit(upper, caseHeader).attempt
        _ = if (insensitive) assert(collision.isLeft, collision)
            else assertEquals(collision.toOption, Some(ExitCode.Success))
        present <- fs.exists(caseHeader)
        _ = assertEquals(present, !insensitive)
        nested = dir / "inc" / "bosatsu_ext_Native.h"
        good <- emit(dir / "good.c", nested)
        _ = assertEquals(good, ExitCode.Success)
        content <- Fs2PlatformIO.readUtf8(nested)
        _ = assert(content.contains("___bsts_g_Native_l_native__value("), content)
      } yield ()
    }.unsafeToFuture()
  }

  test("relativize handles src and ./src under an absolute root") {
    val root = Path(js.Dynamic.global.process.cwd().asInstanceOf[String])
    val srcAbs = Fs2PlatformIO.resolve(root, Path("src"))
    val srcDotAbs = Fs2PlatformIO.resolve(root, Path("./src"))

    val relSrc = Fs2PlatformIO.relativize(root, srcAbs)
    assert(relSrc.nonEmpty)
    assertEquals(Fs2PlatformIO.resolve(root, relSrc.get), srcAbs)

    val relDotSrc = Fs2PlatformIO.relativize(root, srcDotAbs)
    assert(relDotSrc.nonEmpty)
    assertEquals(Fs2PlatformIO.resolve(root, relDotSrc.get), srcDotAbs)
  }
}
