package dev.bosatsu

import cats.effect.{IO, Resource}
import java.io.IOException
import java.nio.file.{
  AccessDeniedException,
  DirectoryNotEmptyException,
  FileAlreadyExistsException,
  Files,
  FileSystemLoopException,
  LinkOption,
  NoSuchFileException,
  NotDirectoryException,
  Path,
  StandardCopyOption
}
import java.nio.file.attribute.{
  AclFileAttributeView,
  BasicFileAttributes,
  PosixFileAttributes,
  PosixFileAttributeView
}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.control.NonFatal

/** The output set of one JVM file command. Writers finish and close their files
  * before publication starts. Each rename stays on the destination's
  * filesystem. A failed commit restores earlier members of this output set.
  */
final private[bosatsu] class StagedOutputs private (
    install: (Path, Path) => Unit
) {
  import StagedOutputs._

  private val entries = mutable.LinkedHashMap.empty[Path, Entry]

  private val createdDirectories = mutable.LinkedHashSet.empty[Path]

  // Probe with readAttributes: Files.isDirectory also returns false on I/O
  // errors, which would walk every prefix of a path the OS already rejected.
  private def ensureDirectory(path: Path): Unit = {
    var missing = List.empty[Path]
    var cursor = path
    var found = false
    while (!found) {
      try {
        if (!Files.readAttributes(cursor, classOf[BasicFileAttributes]).isDirectory)
          throw new NotDirectoryException(cursor.toString)
        found = true
      } catch {
        case err: NoSuchFileException =>
          missing = cursor :: missing
          cursor = Option(cursor.getParent).getOrElse(throw err)
      }
    }
    missing.foreach { dir =>
      try {
        Files.createDirectory(dir)
        val _ = createdDirectories.add(dir)
      } catch {
        case _: FileAlreadyExistsException if Files.isDirectory(dir) => ()
      }
    }
  }

  private def destination(path: Path): Path = {
    val links = mutable.Set.empty[Path]
    @tailrec def follow(current: Path): Path = {
      val parent = Option(current.getParent).getOrElse {
        throw new IOException(s"expected an output file: $path")
      }
      ensureDirectory(parent)
      // Resolve the parent before looking at the final link. Its target
      // may be missing on the first build, and may itself be relative.
      val leaf = parent.toRealPath().resolve(current.getFileName)
      if (!Files.isSymbolicLink(leaf)) leaf
      else if (links.add(leaf))
        follow(leaf.resolveSibling(Files.readSymbolicLink(leaf)))
      else throw new FileSystemLoopException(leaf.toString)
    }
    follow(path.toAbsolutePath)
  }

  def prepare(path: Path): IO[Path] = IO.blocking {
    commitLock.synchronized {
      val target = destination(path)
      if (entries.contains(target))
        throw new IOException(s"multiple outputs refer to the same file: $path")

      val stage = Files.createTempDirectory(target.getParent, ".bosatsu-stage-")
      val entry = Entry(target, stage)
      entries.update(target, entry)
      entry.next
    }
  }

  def publish: IO[Unit] = IO.blocking {
    // Compilations can run in parallel. Compare, back up and install under
    // one lock so a failed request cannot undo a later writer.
    commitLock.synchronized {
      val changed = entries.valuesIterator.filter(_.needsInstall).toList
      changed.foreach(_.preserveAccess())
      // Only files installed before a later rename need a rollback copy.
      changed.dropRight(1).foreach(_.saveOriginal())

      var installed = List.empty[Entry]
      try
        changed.foreach { entry =>
          install(entry.next, entry.target)
          installed = entry :: installed
        }
      catch {
        case NonFatal(failure) =>
          installed.foreach { entry =>
            try entry.restore()
            catch {
              case NonFatal(rollback) =>
                entry.keepBackup = Files.exists(entry.backup)
                val recovery =
                  if (entry.keepBackup) s"; original saved in ${entry.backup}"
                  else ""
                failure.addSuppressed(
                  new IOException(
                    s"failed restoring ${entry.target}$recovery",
                    rollback
                  )
                )
            }
          }
          throw failure
      }
    }
  }

  private def cleanup: IO[Unit] = IO.blocking {
    val errors = List.newBuilder[Throwable]
    entries.valuesIterator.foreach { entry =>
      try {
        Files.deleteIfExists(entry.next)
        if (!entry.keepBackup) {
          Files.deleteIfExists(entry.backup)
          Files.deleteIfExists(entry.directory)
          ()
        }
      } catch {
        case NonFatal(err) => errors += err; ()
      }
    }
    // A failed request can have created a parent for an output that was
    // never published. Only remove directories this request created and
    // which are still empty.
    commitLock.synchronized {
      createdDirectories.toList.reverse.foreach { path =>
        try {
          Files.deleteIfExists(path)
          ()
        } catch {
          case _: DirectoryNotEmptyException => ()
          case NonFatal(err)                 =>
            errors += err
            ()
        }
      }
    }
    errors.result() match {
      case first :: rest =>
        rest.foreach(first.addSuppressed)
        throw first
      case Nil => ()
    }
  }
}

private[bosatsu] object StagedOutputs {
  private val commitLock = new Object

  def resource(
      onCleanupError: Throwable => IO[Unit]
  ): Resource[IO, StagedOutputs] =
    withInstaller(onCleanupError, replace)

  def withInstaller(
      onCleanupError: Throwable => IO[Unit],
      install: (Path, Path) => Unit
  ): Resource[IO, StagedOutputs] =
    Resource.make(IO(new StagedOutputs(install)))(
      _.cleanup.handleErrorWith(onCleanupError)
    )

  private def replace(from: Path, to: Path): Unit = {
    Files.move(
      from,
      to,
      StandardCopyOption.ATOMIC_MOVE,
      StandardCopyOption.REPLACE_EXISTING
    )
    ()
  }

  final private class Entry(val target: Path, val directory: Path) {
    val next: Path = directory.resolve("output")
    val backup: Path = directory.resolve("original")
    var keepBackup: Boolean = false

    // Files.mismatch streams the contents. Identical signatures keep their
    // timestamp for callers which use make's timestamp-based build graph.
    def needsInstall: Boolean = {
      if (!Files.isRegularFile(next))
        throw new IOException(s"command did not produce its output: $target")
      if (Files.notExists(target, LinkOption.NOFOLLOW_LINKS)) true
      else if (!Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS))
        throw new IOException(s"expected a regular output file: $target")
      else if (Files.isReadable(target) && Files.mismatch(next, target) == -1L)
        false
      else if (!Files.isWritable(target))
        throw new AccessDeniedException(target.toString)
      else true
    }

    def saveOriginal(): Unit =
      if (Files.exists(target)) {
        // Keep the original inode, including access controls. Linking it does
        // not require opening its contents for reading.
        Files.createLink(backup, target)
        ()
      }

    def preserveAccess(): Unit =
      if (Files.exists(target)) {
        val posix =
          Files.getFileAttributeView(target, classOf[PosixFileAttributeView])
        val acl = Files.getFileAttributeView(target, classOf[AclFileAttributeView])
        if (posix == null && acl == null)
          throw new IOException(s"cannot preserve output access controls: $target")

        val priorPosix = Option(posix).map { view =>
          if (acl == null) PosixOutputAccess.preserve(target, next)
          val expected = view.readAttributes()
          val staged = Files.getFileAttributeView(next, classOf[PosixFileAttributeView])
          val original = staged.readAttributes()
          // Setting ownership can clear permission bits. Set the mode last.
          if (!original.owner().equals(expected.owner())) staged.setOwner(expected.owner())
          if (!original.group().equals(expected.group())) staged.setGroup(expected.group())
          staged.setPermissions(expected.permissions())
          expected
        }
        if (acl != null) {
          val staged = Files.getFileAttributeView(next, classOf[AclFileAttributeView])
          val owner = acl.getOwner
          val expected = acl.getAcl
          if (!staged.getOwner.equals(owner)) staged.setOwner(owner)
          staged.setAcl(expected)
          if (!staged.getAcl.equals(expected) || !staged.getOwner.equals(owner))
            throw new IOException(s"cannot preserve output ACL: $target")
        }
        priorPosix.foreach { before =>
          val after = Files.readAttributes(next, classOf[PosixFileAttributes])
          if (!after.owner().equals(before.owner()) ||
              !after.group().equals(before.group()) ||
              !after.permissions().equals(before.permissions()))
            throw new IOException(s"cannot preserve output permissions: $target")
        }
      }

    def restore(): Unit =
      if (Files.exists(backup)) replace(backup, target)
      else Files.delete(target)
  }
}
