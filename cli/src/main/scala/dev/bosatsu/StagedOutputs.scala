package dev.bosatsu

import cats.effect.{IO, Resource}
import java.io.IOException
import java.nio.file.{
  AccessDeniedException,
  DirectoryNotEmptyException,
  FileAlreadyExistsException,
  Files,
  LinkOption,
  Path,
  StandardCopyOption
}
import java.nio.file.attribute.PosixFileAttributeView
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

  private def ensureDirectory(path: Path): Unit =
    if (!Files.isDirectory(path)) {
      Option(path.getParent).foreach(ensureDirectory)
      try {
        Files.createDirectory(path)
        val _ = createdDirectories.add(path)
        ()
      } catch {
        case _: FileAlreadyExistsException if Files.isDirectory(path) => ()
      }
    }

  def prepare(path: Path): IO[Path] = IO.blocking {
    commitLock.synchronized {
      val absolute = path.toAbsolutePath
      val parent = Option(absolute.getParent).getOrElse {
        throw new IOException(s"expected an output file: $path")
      }
      ensureDirectory(parent)
      // Canonicalize existing parents and live output symlinks so relative
      // aliases cannot cause two outputs to overwrite each other.
      val leaf = parent.toRealPath().resolve(absolute.getFileName)
      val target =
        if (Files.isSymbolicLink(leaf)) leaf.toRealPath()
        else leaf

      if (entries.contains(target))
        throw new IOException(s"multiple outputs refer to the same file: $path")

      val stage = Files.createTempDirectory(target.getParent, ".bosatsu-stage-")
      val entry = Entry(target, stage)
      entries.update(target, entry)
      entry.next
    }
  }

  def publish: IO[Unit] = IO.blocking {
    // Compilations can run in parallel. Only make/restore backups and install
    // results under the lock so a failed request cannot undo a later writer.
    commitLock.synchronized {
      val changed = entries.valuesIterator.filter(_.needsInstall).toList
      changed.foreach(_.preserveMode())
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
      onCleanupError: Throwable => IO[Unit] = IO.raiseError(_),
      install: (Path, Path) => Unit = replace
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
      else if (Files.mismatch(next, target) == -1L) false
      else if (!Files.isWritable(target))
        throw new AccessDeniedException(target.toString)
      else true
    }

    def saveOriginal(): Unit =
      if (Files.exists(target)) {
        Files.copy(target, backup, StandardCopyOption.COPY_ATTRIBUTES)
        ()
      }

    def preserveMode(): Unit =
      if (Files.exists(target)) {
        val posix =
          Files.getFileAttributeView(target, classOf[PosixFileAttributeView])
        if (posix != null) {
          Files.setPosixFilePermissions(
            next,
            posix.readAttributes().permissions()
          )
          ()
        }
      }

    def restore(): Unit =
      if (Files.exists(backup)) replace(backup, target)
      else Files.delete(target)
  }
}
