package dev.bosatsu

import cats.effect.{Deferred, Fiber, IO, Outcome, Ref}
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import dev.bosatsu.hashing.Algo
import dev.bosatsu.library.Version
import dev.bosatsu.tool.{ExitCode, Output}
import fs2.io.file.{Files, Path}
import munit.FunSuite
import org.typelevel.paiges.Doc
import java.io.IOException
import scala.concurrent.duration.DurationInt
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
          normalOps.writeStaged(staged, "partial".getBytes("UTF-8")) *>
            IO.raiseError(new IOException("staged write failed"))
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

  test("atomic file sets roll back every install position across output orders") {
    val fs = Files.forIO
    val normalOps = Fs2PlatformIO.documentWriteOps
    fs.tempDirectory.use { dir =>
      val installationFailures = List(1, 2, 3).traverse_ { failureAt =>
        val existing = dir / s"existing-$failureAt"
        val absent = List(
          dir / s"first-$failureAt",
          dir / s"second-$failureAt",
          dir / s"third-$failureAt"
        )
        val files = (existing -> "same".getBytes("UTF-8").toVector) ::
          absent.zipWithIndex.map { case (path, index) =>
            path -> s"new-$index".getBytes("UTF-8").toVector
          }
        val ordered = if (failureAt % 2 == 0) files.reverse else files
        for {
          _ <- Fs2PlatformIO.writeBytes(existing, "same".getBytes("UTF-8"))
          existingBefore <- IO.fromPromise(IO(DocumentStat(existing.toString)))
            .map(s => (s.mode, s.uid, s.gid, s.ino))
          installs <- Ref.of[IO, Int](0)
          ops = normalOps.copy(
            replace = (staged, destination) =>
              installs.updateAndGet(_ + 1).flatMap {
                case n if n == failureAt =>
                  IO.raiseError(new IOException(s"failed install $failureAt"))
                case _ => normalOps.replace(staged, destination)
              }
          )
          failed <- Output.AtomicFileSet(ordered).report(
            Fs2PlatformIO.withDocumentWriteOps(ops)
          ).attempt
          _ = assert(
            failed.swap.toOption.exists(_.getMessage == s"failed install $failureAt"),
            failed
          )
          present <- absent.traverse(fs.exists)
          _ = assertEquals(present, List(false, false, false))
          existingContent <- Fs2PlatformIO.readBytes(existing)
          _ = assertEquals(existingContent.toList, "same".getBytes("UTF-8").toList)
          existingAfter <- IO.fromPromise(IO(DocumentStat(existing.toString)))
            .map(s => (s.mode, s.uid, s.gid, s.ino))
          _ = assertEquals(existingAfter, existingBefore)
          paths <- fs.walk(dir).compile.toList
          _ = assert(!paths.exists(_.fileName.toString.startsWith(".bosatsu-write-")))
        } yield ()
      }
      val stagingFailures = List(1, 2, 3).traverse_ { failureAt =>
        val existing = dir / s"staged-existing-$failureAt"
        val absent = List(
          dir / s"staged-first-$failureAt",
          dir / s"staged-second-$failureAt",
          dir / s"staged-third-$failureAt"
        )
        val files = (existing -> "same".getBytes("UTF-8").toVector) ::
          absent.zipWithIndex.map { case (path, index) =>
            path -> s"new-$index".getBytes("UTF-8").toVector
          }
        for {
          _ <- Fs2PlatformIO.writeBytes(existing, "same".getBytes("UTF-8"))
          writes <- Ref.of[IO, Int](0)
          ops = normalOps.copy(
            writeStaged = (staged, bytes) =>
              writes.updateAndGet(_ + 1).flatMap {
                case n if n == failureAt =>
                  normalOps.writeStaged(staged, bytes) *>
                    IO.raiseError(new IOException(s"failed stage $failureAt"))
                case _ => normalOps.writeStaged(staged, bytes)
              }
          )
          failed <- Output.AtomicFileSet(files).report(
            Fs2PlatformIO.withDocumentWriteOps(ops)
          ).attempt
          _ = assert(
            failed.swap.toOption.exists(_.getMessage == s"failed stage $failureAt"),
            failed
          )
          present <- absent.traverse(fs.exists)
          _ = assertEquals(present, List(false, false, false))
          paths <- fs.walk(dir).compile.toList
          _ = assert(!paths.exists(_.fileName.toString.startsWith(".bosatsu-write-")))
        } yield ()
      }
      installationFailures *> stagingFailures
    }.unsafeToFuture()
  }

  test("atomic file set cancellation is deferred through commit and rollback") {
    val fs = Files.forIO
    val normalOps = Fs2PlatformIO.documentWriteOps
    val sameBytes = "same".getBytes("UTF-8")
    val newBytes = List("first", "second", "third").map(_.getBytes("UTF-8"))

    def stat(path: Path): IO[(Int, Int, Int, Double)] =
      IO.fromPromise(IO(DocumentStat(path.toString)))
        .map(s => (s.mode, s.uid, s.gid, s.ino))

    def setup(dir: Path, name: String, reverse: Boolean) = {
      val caseDir = dir / name
      val existing = caseDir / "existing"
      val absent = List(caseDir / "first", caseDir / "second", caseDir / "third")
      val outputs = (existing -> sameBytes.toVector) ::
        absent.zip(newBytes).map { case (path, bytes) => path -> bytes.toVector }
      val ordered = if (reverse) outputs.reverse else outputs
      for {
        _ <- fs.createDirectories(caseDir)
        _ <- Fs2PlatformIO.writeBytes(existing, sameBytes)
        _ <- IO.fromPromise(IO(DocumentChmod(existing.toString, 0x180)))
        existingBefore <- stat(existing)
      } yield (caseDir, existing, absent, ordered, existingBefore)
    }

    def assertNoStaging(caseDir: Path): IO[Unit] =
      fs.walk(caseDir).compile.toList.map { paths =>
        assert(!paths.exists(_.fileName.toString.startsWith(".bosatsu-write-")), paths)
      }

    def assertCanceled[A](label: String, outcome: Outcome[IO, Throwable, A]): Unit =
      assert(outcome match {
        case Outcome.Canceled() => true
        case _                  => false
      }, s"$label: $outcome")

    def cancelAndRelease[A](fiber: Fiber[IO, Throwable, A], release: Deferred[IO, Unit]): IO[Unit] =
      for {
        releaser <- (IO.sleep(10.millis) *> release.complete(())).start
        // Fiber.cancel sends the request before it waits for termination. The
        // independent releaser lets the masked commit finish without waiting on it.
        _ <- fiber.cancel
        _ <- releaser.joinWithNever
      } yield ()

    fs.tempDirectory.use { dir =>
      val canceledDuringStage = for {
        (_, _, absent, outputs, _) <- setup(dir, "stage-cancel", reverse = false)
        entered <- Deferred[IO, Unit]
        blocked <- Deferred[IO, Unit]
        ops = normalOps.copy(
          writeStaged = (staged, bytes) => entered.complete(()) *> blocked.get *>
            normalOps.writeStaged(staged, bytes)
        )
        fiber <- Output.AtomicFileSet(outputs).report(
          Fs2PlatformIO.withDocumentWriteOps(ops)
        ).start
        _ <- entered.get
        _ <- fiber.cancel
        outcome <- fiber.join
        _ = assertCanceled("staging", outcome)
        present <- absent.traverse(fs.exists)
        _ = assertEquals(present, List(false, false, false))
        _ <- assertNoStaging(dir / "stage-cancel")
        retried <- Output.AtomicFileSet(outputs).report(Fs2PlatformIO)
        _ = assertEquals(retried, ExitCode.Success)
      } yield ()

      val canceledAfterRename = List(1, 2, 3).traverse_ { installPosition =>
        for {
          (caseDir, existing, absent, outputs, existingBefore) <- setup(
            dir,
            s"after-rename-$installPosition",
            reverse = installPosition % 2 == 0
          )
          renamed <- Deferred[IO, Unit]
          release <- Deferred[IO, Unit]
          installs <- Ref.of[IO, Int](0)
          ops = normalOps.copy(
            replace = (staged, destination) =>
              normalOps.replace(staged, destination).flatMap { installed =>
                installs.updateAndGet(_ + 1).flatMap {
                  case n if n == installPosition =>
                    renamed.complete(()) *> release.get.as(installed)
                  case _                         => IO.pure(installed)
                }
              }
          )
          fiber <- Output.AtomicFileSet(outputs).report(
            Fs2PlatformIO.withDocumentWriteOps(ops)
          ).start
          _ <- renamed.get
          // Cancel after the real rename while replace is still held, then release
          // it from an independent fiber rather than waiting for cancellation.
          _ <- cancelAndRelease(fiber, release)
          outcome <- fiber.join
          _ = assertCanceled(s"install $installPosition", outcome)
          contents <- absent.traverse(Fs2PlatformIO.readBytes)
          _ = assertEquals(contents.map(_.toList), newBytes.map(_.toList))
          existingContent <- Fs2PlatformIO.readBytes(existing)
          _ = assertEquals(existingContent.toList, sameBytes.toList)
          existingAfter <- stat(existing)
          _ = assertEquals(existingAfter, existingBefore)
          _ <- assertNoStaging(caseDir)
          retried <- Output.AtomicFileSet(outputs).report(Fs2PlatformIO)
          _ = assertEquals(retried, ExitCode.Success)
        } yield ()
      }

      val canceledBeforeLaterInstallFailure = for {
        (caseDir, existing, absent, outputs, existingBefore) <- setup(
          dir,
          "cancel-later-error",
          reverse = true
        )
        entered <- Deferred[IO, Unit]
        release <- Deferred[IO, Unit]
        installs <- Ref.of[IO, Int](0)
        ops = normalOps.copy(
          replace = (staged, destination) =>
            installs.updateAndGet(_ + 1).flatMap {
              case 1 => normalOps.replace(staged, destination)
              case 2 => entered.complete(()) *> release.get *>
                IO.raiseError(new IOException("later installation failed"))
              case n => IO.raiseError(new IOException(s"unexpected installation $n"))
            }
        )
        fiber <- Output.AtomicFileSet(outputs).report(
          Fs2PlatformIO.withDocumentWriteOps(ops)
        ).start
        _ <- entered.get
        _ <- cancelAndRelease(fiber, release)
        outcome <- fiber.join
        present <- absent.traverse(fs.exists)
        _ = assertEquals(present, List(false, false, false))
        existingContent <- Fs2PlatformIO.readBytes(existing)
        _ = assertEquals(existingContent.toList, sameBytes.toList)
        existingAfter <- stat(existing)
        _ = assertEquals(existingAfter, existingBefore)
        _ <- assertNoStaging(caseDir)
        retried <- Output.AtomicFileSet(outputs).report(Fs2PlatformIO)
        _ = assertEquals(retried, ExitCode.Success)
        _ = assertCanceled("later installation error", outcome)
      } yield ()

      canceledDuringStage *> canceledAfterRename *> canceledBeforeLaterInstallFailure
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

  test("inspect-library publishes the complete output set and rolls back a later install failure") {
    val fs = Files.forIO
    val fixtureNames = List(
      "parser-v1.0.0.bosatsu_lib",
      "common-v1.0.0.bosatsu_lib",
      "legacy-v1.0.0.bosatsu_lib",
      "public_child-v1.0.0.bosatsu_lib",
      "private_child-v1.0.0.bosatsu_lib",
      "shared-v1.0.0.bosatsu_lib",
      "shared-v2.0.0.bosatsu_lib",
      "left-v1.0.0.bosatsu_lib",
      "right-v1.0.0.bosatsu_lib"
    )
    val fixturePrefix = "test_workspace/inspect_library/"
    val sourceContext = Path(fixturePrefix + "parser-context.json")

    def report(platform: PlatformIO[IO, Path], args: List[String]): IO[ExitCode] =
      IO.fromEither(
        new MainModule(platform).run("tool" :: args).leftMap(h => new IllegalArgumentException(h.toString))
      ).flatten.flatMap(_.report(platform))

    def pinned(input: Path, context: Path, bytes: Array[Byte]): List[String] = {
      val version = "1.0.0"
      List(
        "inspect-library",
        "--input", input.toString,
        "--expected-name", "parser",
        "--expected-version", version,
        "--expected-hash", s"blake3:${Algo.hashBytes[Algo.Blake3](bytes).hex}",
        "--expected-hash", s"sha256:${Algo.hashBytes[Algo.Sha256](bytes).hex}",
        "--dependency-context", context.toString
      )
    }

    def withOutputs(command: List[String], outputs: List[(String, Path)]): List[String] =
      command ::: outputs.flatMap { case (option, path) => List(option, path.toString) }

    def stat(path: Path): IO[(Int, Int, Int, Double)] =
      IO.fromPromise(IO(DocumentStat(path.toString)))
        .map(s => (s.mode, s.uid, s.gid, s.ino))

    fs.tempDirectory.use { dir =>
      val fixtureDir = dir / "fixtures"
      val root = fixtureDir / fixtureNames.head
      val context = dir / "parser-context.json"
      val signature = dir / "parser.sig"
      val exposures = dir / "exposures.json"
      val membership = dir / "membership.json"
      val implementation = dir / "implementation.json"
      val descriptor = dir / "descriptor.json"
      val baselineOutputs = List(
        signature,
        exposures,
        membership,
        implementation,
        descriptor
      )
      val fullOptions = List(
        "--sig-out" -> signature,
        "--exposures-out" -> exposures,
        "--membership-out" -> membership,
        "--implementation-out" -> implementation,
        "--descriptor-out" -> descriptor
      )
      for {
        _ <- fs.createDirectories(fixtureDir)
        _ <- fixtureNames.traverse_ { name =>
          Fs2PlatformIO.readBytes(Path(fixturePrefix + name)).flatMap { bytes =>
            Fs2PlatformIO.writeBytes(fixtureDir / name, bytes)
          }
        }
        contextText <- Fs2PlatformIO.readUtf8(sourceContext)
        contextBytes = contextText.replace(fixturePrefix, s"${fixtureDir.toString}/")
        _ <- Fs2PlatformIO.writeDoc(context, Doc.text(contextBytes))
        originalRoot <- Fs2PlatformIO.readBytes(root)
        fullCommand = withOutputs(pinned(root, context, originalRoot), fullOptions)
        first <- report(Fs2PlatformIO, fullCommand)
        _ = assertEquals(first, ExitCode.Success)
        originalOutputs <- baselineOutputs.traverse(path =>
          Fs2PlatformIO.readBytes(path).map(_.toList)
        )
        originalSignatureStat <- stat(signature)

        normalOps = Fs2PlatformIO.documentWriteOps
        installs <- Ref.of[IO, Int](0)
        faultOps = normalOps.copy(
          replace = (staged, destination) =>
            installs.updateAndGet(_ + 1).flatMap {
              case 2 => IO.raiseError(new IOException("injected later install failure"))
              case _ => normalOps.replace(staged, destination)
            }
        )
        retryExposures = dir / "retry-exposures.json"
        retryMembership = dir / "retry-membership.json"
        rollbackCommand = withOutputs(
          pinned(root, context, originalRoot),
          List(
            "--sig-out" -> signature,
            "--exposures-out" -> retryExposures,
            "--membership-out" -> retryMembership
          )
        )
        failedInstall <- report(
          Fs2PlatformIO.withDocumentWriteOps(faultOps),
          rollbackCommand
        ).attempt
        _ = assert(
          failedInstall.swap.toOption.exists(_.getMessage.contains("injected later install failure")),
          failedInstall
        )
        afterFailedInstall <- baselineOutputs.traverse(path =>
          Fs2PlatformIO.readBytes(path).map(_.toList)
        )
        _ = assertEquals(afterFailedInstall, originalOutputs)
        signatureStatAfterFailure <- stat(signature)
        _ = assertEquals(signatureStatAfterFailure, originalSignatureStat)
        retryExposureExists <- fs.exists(retryExposures)
        retryMembershipExists <- fs.exists(retryMembership)
        _ = assert(!retryExposureExists && !retryMembershipExists)
        pathsAfterFailure <- fs.walk(dir).compile.toList
        _ = assert(!pathsAfterFailure.exists(_.fileName.toString.startsWith(".bosatsu-write-")))
        retried <- report(Fs2PlatformIO, rollbackCommand)
        _ = assertEquals(retried, ExitCode.Success)
        signatureStatAfterRetry <- stat(signature)
        _ = assertEquals(signatureStatAfterRetry, originalSignatureStat)
        retryExposure <- Fs2PlatformIO.readUtf8(retryExposures)
        retryMember <- Fs2PlatformIO.readUtf8(retryMembership)
        _ = assert(retryExposure.contains("witness_path"), retryExposure)
        _ = assert(retryMember.contains("Parser"), retryMember)

        library = proto.Library.parseFrom(originalRoot)
        packageIndex = library.internalPackages.indexWhere(_.exports.nonEmpty)
        _ = assert(packageIndex >= 0, "fixture has no exported package")
        packageProto = library.internalPackages(packageIndex)
        packageName = packageProto.strings(packageProto.packageName - 1)
        interfaceIndex = library.exportedIfaces.indexWhere(interface =>
          interface.strings(interface.packageName - 1) == packageName
        )
        _ = assert(interfaceIndex >= 0, s"fixture has no exported interface for $packageName")
        changedRoot = library.copy(
          internalPackages = library.internalPackages.updated(
            packageIndex,
            packageProto.copy(exports = packageProto.exports.dropRight(1))
          ),
          exportedIfaces = library.exportedIfaces.updated(
            interfaceIndex,
            library.exportedIfaces(interfaceIndex).copy(
              exports = library.exportedIfaces(interfaceIndex).exports.dropRight(1)
            )
          )
        ).toByteArray
        _ <- Fs2PlatformIO.writeBytes(root, changedRoot)
        changedSignature = dir / "changed.sig"
        changedSigResult <- report(
          Fs2PlatformIO,
          withOutputs(pinned(root, context, changedRoot), List("--sig-out" -> changedSignature))
        )
        _ = assertEquals(changedSigResult, ExitCode.Success)
        changedSignatureBytes <- Fs2PlatformIO.readBytes(changedSignature)
        baselineSignature <- Fs2PlatformIO.readBytes(signature).map(_.toList)
        _ = assertNotEquals(changedSignatureBytes.toList, baselineSignature)
        beforeChangedPublication <- baselineOutputs.traverse(path =>
          Fs2PlatformIO.readBytes(path).map(_.toList)
        )
        beforeChangedStats <- baselineOutputs.traverse(stat)
        failedChangedPublication <- report(
          Fs2PlatformIO,
          withOutputs(pinned(root, context, changedRoot), fullOptions)
        ).attempt
        _ = assert(
          failedChangedPublication.swap.toOption.exists(
            _.getMessage.contains("cannot preserve output access controls")
          ),
          failedChangedPublication
        )
        afterChangedPublication <- baselineOutputs.traverse(path =>
          Fs2PlatformIO.readBytes(path).map(_.toList)
        )
        afterChangedStats <- baselineOutputs.traverse(stat)
        _ = assertEquals(afterChangedPublication, beforeChangedPublication)
        _ = assertEquals(afterChangedStats, beforeChangedStats)
        pathsAfterChangedFailure <- fs.walk(dir).compile.toList
        _ = assert(!pathsAfterChangedFailure.exists(_.fileName.toString.startsWith(".bosatsu-write-")))
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
