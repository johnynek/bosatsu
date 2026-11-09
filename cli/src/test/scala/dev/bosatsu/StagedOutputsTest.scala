package dev.bosatsu

import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import java.io.IOException
import java.nio.file.{Files, Path, StandardCopyOption}
import java.nio.file.attribute.PosixFilePermissions

class StagedOutputsTest extends FileCommandFixtures {
  private def stage(
      staged: StagedOutputs,
      path: Path,
      contents: String
  ): IO[Unit] =
    staged
      .prepare(path)
      .flatMap(tmp => IO.blocking(Files.writeString(tmp, contents)).void)

  test("no visible output before successful publication") {
    directory
      .use { root =>
        val path = root.resolve("already.txt")
        val fresh = root.resolve("fresh.txt")
        for {
          _ <- write(root, "already.txt", "original")
          _ <- StagedOutputs.resource().use { staged =>
            for {
              _ <- stage(staged, path, "updated")
              _ <- stage(staged, fresh, "new")
              _ = assertEquals(Files.readString(path), "original")
              _ = assert(!Files.exists(fresh))
              _ <- staged.publish
            } yield ()
          }
          _ = assertEquals(Files.readString(path), "updated")
          _ = assertEquals(Files.readString(fresh), "new")
          _ = assertEquals(
            listPaths(root),
            List("", "already.txt", "fresh.txt")
          )
        } yield ()
      }
      .unsafeRunSync()
  }

  test("serializer failure drops partial bytes and preserves existing files") {
    directory
      .use { root =>
        for {
          _ <- write(root, "out", "previous bytes")
          attempt <- StagedOutputs
            .resource()
            .use { staged =>
              for {
                _ <- stage(staged, root.resolve("out"), "partial new bytes")
                _ <- IO.raiseError[Unit](
                  new IOException("serialization failed")
                )
              } yield ()
            }
            .attempt
          _ = assert(attempt.isLeft)
          _ = assertEquals(
            Files.readString(root.resolve("out")),
            "previous bytes"
          )
          _ = assertEquals(listPaths(root), List("", "out"))
        } yield ()
      }
      .unsafeRunSync()
  }

  test(
    "failure on the last rename restores overwritten files and removes new ones"
  ) {
    directory
      .use { root =>
        val existing = root.resolve("existing")
        val fresh = root.resolve("fresh")
        val last = root.resolve("last")
        // Deterministic filesystem fault after two successful renames.
        val failingInstall = (from: Path, to: Path) => {
          if (to.endsWith("last")) throw new IOException("rename failed")
          Files.move(
            from,
            to,
            StandardCopyOption.ATOMIC_MOVE,
            StandardCopyOption.REPLACE_EXISTING
          )
          ()
        }
        for {
          _ <- write(root, "existing", "old")
          _ <- write(root, "last", "untouched")
          attempt <- StagedOutputs
            .resource(install = failingInstall)
            .use { staged =>
              for {
                _ <- stage(staged, existing, "changed")
                _ <- stage(staged, fresh, "created")
                _ <- stage(staged, last, "changed too")
                _ <- staged.publish
              } yield ()
            }
            .attempt
          _ = assertEquals(
            attempt.swap.toOption.map(_.getMessage),
            Some("rename failed")
          )
          _ = assertEquals(Files.readString(existing), "old")
          _ = assertEquals(Files.readString(last), "untouched")
          _ = assertEquals(listPaths(root), List("", "existing", "last"))
        } yield ()
      }
      .unsafeRunSync()
  }

  test("cancellation before publication removes staged files") {
    directory
      .use { root =>
        for {
          ready <- Deferred[IO, Unit]
          waiting <- StagedOutputs
            .resource()
            .use { staged =>
              stage(staged, root.resolve("new/nested/out"), "partial") *>
                ready.complete(()) *> IO.never[Unit]
            }
            .start
          _ <- ready.get
          _ <- waiting.cancel
          _ = assertEquals(listPaths(root), List(""))
        } yield ()
      }
      .unsafeRunSync()
  }

  test("live output symlinks continue to refer to the updated target") {
    directory
      .use { root =>
        assume(Files.getFileStore(root).supportsFileAttributeView("posix"))
        val target = root.resolve("original")
        val link = root.resolve("alias")
        for {
          _ <- write(root, "original", "old")
          _ <- IO.blocking(Files.createSymbolicLink(link, target.getFileName))
          _ <- StagedOutputs.resource().use { staged =>
            stage(staged, link, "new") *> staged.publish
          }
          _ = assert(Files.isSymbolicLink(link))
          _ = assertEquals(Files.readString(target), "new")
        } yield ()
      }
      .unsafeRunSync()
  }

  test("replacing a file preserves its POSIX mode") {
    directory
      .use { root =>
        val target = root.resolve("artifact")
        val expected = PosixFilePermissions.fromString("rw-r-----")
        assume(Files.getFileStore(root).supportsFileAttributeView("posix"))
        for {
          _ <- write(root, "artifact", "old")
          _ <- IO.blocking(Files.setPosixFilePermissions(target, expected))
          _ <- StagedOutputs.resource().use { staged =>
            stage(staged, target, "replacement") *> staged.publish
          }
          _ = assertEquals(Files.getPosixFilePermissions(target), expected)
        } yield ()
      }
      .unsafeRunSync()
  }
}
