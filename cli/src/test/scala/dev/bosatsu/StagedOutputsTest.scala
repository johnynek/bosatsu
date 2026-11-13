package dev.bosatsu

import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import com.sun.jna.{Library, Native, NativeLong}
import com.sun.security.auth.module.UnixSystem
import java.io.IOException
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path, StandardCopyOption}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.attribute.{
  PosixFileAttributeView,
  PosixFilePermissions,
  UserDefinedFileAttributeView
}

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
          _ <- StagedOutputs.resource(IO.raiseError(_)).use { staged =>
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
            .resource(IO.raiseError(_))
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
            .withInstaller(IO.raiseError(_), failingInstall)
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
            .resource(IO.raiseError(_))
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

  test("first publication and rebuild follow a dangling chain of output links") {
    directory
      .use { root =>
        assume(Files.getFileStore(root).supportsFileAttributeView("posix"))
        val alias = root.resolve("alias")
        val link = root.resolve("links/relative")
        val target = root.resolve("artifacts/out")
        def publish(text: String): IO[Unit] =
          StagedOutputs.resource(IO.raiseError(_)).use { staged =>
            stage(staged, alias, text) *> staged.publish
          }
        for {
          _ <- IO.blocking(Files.createDirectory(link.getParent))
          _ <- IO.blocking(Files.createSymbolicLink(alias, link))
          _ <- IO.blocking(
            Files.createSymbolicLink(link, Path.of("../artifacts/out"))
          )
          _ <- publish("first")
          _ = assertEquals(Files.readString(target), "first")
          _ <- publish("rebuilt")
          _ = assertEquals(Files.readString(target), "rebuilt")
          _ = assert(Files.isSymbolicLink(alias))
          _ = assert(Files.isSymbolicLink(link))
        } yield ()
      }
      .unsafeRunSync()
  }

  test("an output symlink cycle fails before publishing its siblings") {
    directory
      .use { root =>
        assume(Files.getFileStore(root).supportsFileAttributeView("posix"))
        val link = root.resolve("link")
        for {
          _ <- IO.blocking(Files.createSymbolicLink(link, link.getFileName))
          result <- StagedOutputs
            .resource(IO.raiseError(_))
            .use { staged =>
              stage(staged, root.resolve("other"), "new") *>
                stage(staged, link, "invalid") *> staged.publish
            }
            .attempt
          _ = assert(result.isLeft)
          _ = assertEquals(listPaths(root), List("", "link"))
        } yield ()
      }
      .unsafeRunSync()
  }

  test(
    "replacing an output retains its owning group when it differs from the parent"
  ) {
    directory
      .use { root =>
        assume(Files.getFileStore(root).supportsFileAttributeView("posix"))
        val target = root.resolve("out")
        val lookup = root.getFileSystem.getUserPrincipalLookupService
        val parentGroup = Files
          .getFileAttributeView(root, classOf[PosixFileAttributeView])
          .readAttributes()
          .group()
        val alternate = new UnixSystem().getGroups.iterator
          .map(id => lookup.lookupPrincipalByGroupName(id.toString))
          .find(group => !group.equals(parentGroup))
        assume(
          alternate.nonEmpty,
          "requires a second Unix group for the test user"
        )
        val view =
          Files.getFileAttributeView(target, classOf[PosixFileAttributeView])
        for {
          _ <- write(root, "out", "old")
          _ <- IO.blocking {
            view.setGroup(alternate.get)
            view.setPermissions(PosixFilePermissions.fromString("rw-r-----"))
          }
          expected <- IO.blocking(view.readAttributes())
          _ <- StagedOutputs.resource(IO.raiseError(_)).use { staged =>
            stage(staged, target, "new") *> staged.publish
          }
          actual <- IO.blocking(view.readAttributes())
          _ = assertEquals(actual.owner(), expected.owner())
          _ = assertEquals(actual.group(), expected.group())
          _ = assertEquals(actual.permissions(), expected.permissions())
          _ = assertEquals(Files.readString(target), "new")
        } yield ()
      }
      .unsafeRunSync()
  }

  test("a Linux output retains the bytes of its user attributes") {
    directory
      .use { root =>
        assume(System.getProperty("os.name") == "Linux")
        assume(Files.getFileStore(root).supportsFileAttributeView("user"))
        val target = root.resolve("out")
        val name = "bosatsu.fixture"
        val value = "retained"
        val view = Files.getFileAttributeView(
          target,
          classOf[UserDefinedFileAttributeView]
        )
        for {
          _ <- write(root, "out", "old")
          _ <- IO.blocking(view.write(name, UTF_8.encode(value)))
          _ <- StagedOutputs.resource(IO.raiseError(_)).use { staged =>
            stage(staged, target, "new") *> staged.publish
          }
          actual <- IO.blocking {
            val bytes = ByteBuffer.allocate(view.size(name))
            assertEquals(view.read(name, bytes), bytes.capacity())
            UTF_8.decode(bytes.flip()).toString
          }
          _ = assertEquals(actual, value)
          _ = assertEquals(Files.readString(target), "new")
        } yield ()
      }
      .unsafeRunSync()
  }

  test("an extended ACL fails preflight before any output is replaced") {
    directory
      .use { root =>
        val os = System.getProperty("os.name")
        assume(os == "Mac OS X" || os == "Linux")
        val name = "shared +% café"
        val target = root.resolve(name)
        val other = root.resolve("first")
        for {
          _ <- write(root, name, "restricted")
          _ <- write(root, "first", "previous")
          _ <- IO.blocking {
            if (os == "Mac OS X") {
              val chmod = new ProcessBuilder(
                "chmod",
                "+a",
                "everyone deny execute",
                target.toString
              )
              assertEquals(chmod.start().waitFor(), 0)
            } else {
              // Linux's posix_acl_xattr_header and five posix_acl_xattr_entry
              // records. Include a named user so the kernel keeps a real ACL.
              val acl =
                ByteBuffer.allocate(4 + 5 * 8).order(ByteOrder.LITTLE_ENDIAN)
              acl.putInt(2)
              def entry(tag: Int, perms: Int, id: Int): Unit = {
                acl.putShort(tag.toShort).putShort(perms.toShort).putInt(id)
                ()
              }
              entry(1, 6, -1) // owner
              entry(2, 4, (new UnixSystem().getUid + 1L).toInt) // named user
              entry(4, 0, -1) // group
              entry(16, 4, -1) // mask
              entry(32, 0, -1) // other
              val linux = Native.load("c", classOf[LinuxAclFixture])
              val result = linux.setxattr(
                target.toString,
                "system.posix_acl_access",
                acl.array(),
                new NativeLong(acl.capacity().toLong),
                0
              )
              assume(
                result == 0 || Native.getLastError != 95,
                "the temporary filesystem does not support POSIX ACLs"
              )
              assertEquals(result, 0)
            }
          }
          failed <- StagedOutputs
            .resource(IO.raiseError(_))
            .use { staged =>
              stage(staged, other, "new") *> stage(
                staged,
                target,
                "updated"
              ) *> staged.publish
            }
            .attempt
          _ = assert(
            failed.swap.toOption
              .exists(_.getMessage.contains("access controls"))
          )
          _ = assertEquals(Files.readString(target), "restricted")
          _ = assertEquals(Files.readString(other), "previous")
          _ = assertEquals(listPaths(root), List("", "first", name))
        } yield ()
      }
      .unsafeRunSync()
  }

  test(
    "a write-only output can be replaced and rolled back without reading its bytes"
  ) {
    directory
      .use { root =>
        assume(Files.getFileStore(root).supportsFileAttributeView("posix"))
        val target = root.resolve("private")
        val later = root.resolve("second")
        val mode = PosixFilePermissions.fromString("-w-------")
        val readable = PosixFilePermissions.fromString("rw-------")
        val original = root.resolve("original-inode")
        val fault = new IOException("last rename failed")
        def install(from: Path, to: Path): Unit =
          if (to.endsWith("second")) throw fault
          else {
            Files.move(
              from,
              to,
              StandardCopyOption.ATOMIC_MOVE,
              StandardCopyOption.REPLACE_EXISTING
            )
            ()
          }

        def publish(staged: StagedOutputs): IO[Unit] =
          stage(staged, target, "new") *> stage(
            staged,
            later,
            "updated second"
          ) *> staged.publish

        for {
          _ <- write(root, "private", "previous")
          _ <- write(root, "second", "original second")
          _ <- IO.blocking(Files.createLink(original, target))
          _ <- IO.blocking(Files.setPosixFilePermissions(target, mode))
          failed <- StagedOutputs
            .withInstaller(IO.raiseError(_), install)
            .use(publish)
            .attempt
          _ = assertEquals(failed.swap.toOption, Some(fault))
          _ = assert(Files.isSameFile(original, target))
          _ = assertEquals(Files.getPosixFilePermissions(target), mode)
          _ = assertEquals(Files.readString(later), "original second")
          _ <- StagedOutputs.resource(IO.raiseError(_)).use(publish)
          _ = assertEquals(Files.getPosixFilePermissions(target), mode)
          _ = assertEquals(Files.readString(later), "updated second")
          _ <- IO.blocking(Files.setPosixFilePermissions(target, readable))
          _ <- IO.blocking(Files.setPosixFilePermissions(original, readable))
          _ = assertEquals(Files.readString(target), "new")
          _ = assertEquals(Files.readString(original), "previous")
        } yield ()
      }
      .unsafeRunSync()
  }

}

private[bosatsu] trait LinuxAclFixture extends Library {
  def setxattr(
      path: String,
      name: String,
      value: Array[Byte],
      length: NativeLong,
      flags: Int
  ): Int
}
