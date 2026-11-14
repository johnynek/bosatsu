package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.{Hash, MonadError}
import cats.data.Validated
import cats.effect.{IO, Resource}
import cats.effect.kernel.Deferred
import fs2.io.file.{FileKey as FsKey, Files, NoSuchFileException, Path}
import com.monovore.decline.Argument
import org.typelevel.paiges.Doc
import dev.bosatsu.graph.CanPromise
import dev.bosatsu.hashing.{Algo, Hashed, HashValue}
import scala.util.{Failure, Success, Try}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.typedarray.Uint8Array
import java.io.IOException

import cats.syntax.all._
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}

@js.native
@JSImport("fs/promises", "readlink")
private object ReadLink extends js.Function1[String, js.Promise[String]] {
  def apply(path: String): js.Promise[String] = js.native
}

@js.native
@JSImport("fs/promises", "rename")
private object Rename extends js.Function2[String, String, js.Promise[Unit]] {
  def apply(source: String, target: String): js.Promise[Unit] = js.native
}

@js.native
@JSImport("fs/promises", "unlink")
private object Unlink extends js.Function1[String, js.Promise[Unit]] {
  def apply(path: String): js.Promise[Unit] = js.native
}

class Fs2PlatformIO private[dev] (
    injectedDocumentWriteOps: Option[Fs2PlatformIO.DocumentWriteOps]
) extends PlatformIO[IO, Path] {
  private lazy val documentWriteOps =
    injectedDocumentWriteOps.getOrElse(Fs2PlatformIO.documentWriteOps)
  type FileKey = Either[Path, FsKey]
  val fileKeyHash: Hash[FileKey] = Hash[FileKey]

  def withTempDirectoryIn[A](parent: Path)(fn: Path => IO[A]): IO[A] =
    Resource.make(FilesIO.createTempDirectory(Some(parent), ".bosatsu-probe-", None))(
      FilesIO.deleteRecursively(_)
    ).use(fn)

  def existingFileKey(path: Path): IO[Option[FileKey]] =
    FilesIO.getBasicFileAttributes(path, followLinks = true).flatMap { attrs =>
      attrs.fileKey match {
        case Some(key) => IO.pure(Some(Right(key)): Option[FileKey])
        case None => FilesIO.realPath(path).map(p => Some(Left(p)): Option[FileKey])
      }
    }.recover { case _: NoSuchFileException => None }

  def pendingOutputPath(path: Path): IO[Path] = {
    // realpath requires the leaf to exist. Resolve its parent or, for a
    // dangling symlink, the name where writing through the link would land.
    def target(p: Path, links: Set[Path]): IO[Path] =
      FilesIO.realPath(p).recoverWith { case _: NoSuchFileException =>
        target(p.parent.getOrElse(Path(".")), links).flatMap { dir =>
          val leaf = dir.resolve(p.fileName).normalize
          FilesIO.isSymbolicLink(leaf).recover { case _: NoSuchFileException => false }.flatMap {
            case true if links(leaf) => IO.raiseError(new java.io.IOException(s"output symlink cycle at $leaf"))
            case true => IO.fromPromise(IO(ReadLink(leaf.toString)))
              .flatMap(s => target(leaf.resolveSibling(Path(s)), links + leaf))
            case false => IO.pure(leaf)
          }
        }
      }
    target(path, Set.empty)
  }

  def moduleIOMonad: MonadError[IO, Throwable] =
    IO.asyncForIO

  override val parallelF: cats.Parallel[IO] = IO.parallelForIO
  override val canPromiseF: CanPromise[IO] =
    new CanPromise[IO] {
      type Promise[A] = Deferred[IO, Either[Throwable, A]]

      def delay[A](a: => A): IO[A] =
        IO(a)

      def compute[A](a: => A): IO[A] =
        IO.blocking(a)

      def unsafeNewPromise[A]: Promise[A] =
        Deferred.unsafe[IO, Either[Throwable, A]]

      def completeWith[A](p: Promise[A], fa: IO[A]): IO[Unit] =
        fa.attempt.flatMap(p.complete).void.start.void

      def wait[A](p: Promise[A]): IO[A] =
        p.get.rethrow
    }

  val pathArg: Argument[Path] =
    new Argument[Path] {
      def read(string: String) =
        Try(Path(string)) match {
          case Success(value) =>
            Validated.valid(value)
          case Failure(exception) =>
            Validated.invalidNel(
              s"could not parse $string as path: ${exception.getMessage()}"
            )
        }

      def defaultMetavar: String = "path"
    }

  def pathToString(p: Path): String = p.toString

  override def gitTopLevel: IO[Option[Path]] = {
    val start = Path(js.Dynamic.global.process.cwd().asInstanceOf[String])
    gitTopLevelFrom(start)
  }

  def withTempPrefix[A](name: String)(fn: Path => IO[A]): IO[A] = {
    val baseDirF =
      gitTopLevel.map(_.map(resolve(_, ".bosatsuc" :: "tmp" :: Nil)))

    val tempDirF = baseDirF.flatMap {
      case Some(base) =>
        FilesIO.createDirectories(base) *>
          FilesIO.createTempDirectory(
            dir = Some(base),
            prefix = name,
            permissions = None
          )
      case None =>
        FilesIO.createTempDirectory(
          dir = None,
          prefix = name,
          permissions = None
        )
    }

    def cleanup(path: Path): IO[Unit] =
      FilesIO.deleteRecursively(path).handleErrorWith { err =>
        errorln(
          s"warning: failed to delete temp dir ${path.toString}: ${err.getMessage}"
        )
      }

    tempDirF.flatMap { tempDir =>
      fn(tempDir).attempt.flatMap {
        case Right(a) => cleanup(tempDir).as(a)
        case Left(e)  => cleanup(tempDir) *> IO.raiseError(e)
      }
    }
  }

  def system(cmd: String, args: List[String]): IO[Unit] = {
    import fs2.io.process.ProcessBuilder
    ProcessBuilder(cmd, args).withCurrentWorkingDirectory
      .spawn[IO]
      .use { process =>
        val drainAll = (
          fs2.Stream.empty.through(process.stdin).compile.drain,
          fs2.text.utf8
            .decode(process.stderr)
            .evalMapChunk(IO.consoleForIO.error)
            .compile
            .drain,
          fs2.text.utf8
            .decode(process.stdout)
            .evalMapChunk(IO.consoleForIO.print)
            .compile
            .drain
        ).parTupled

        for {
          _ <- drainAll
          result <- process.exitValue
          _ <- IO
            .raiseError(
              new Exception(s"$cmd ${args.mkString(" ")} returned $result")
            )
            .whenA(result != 0)
        } yield ()
      }
  }

  def systemStdout(cmd: String, args: List[String]): IO[String] = {
    import fs2.io.process.ProcessBuilder
    ProcessBuilder(cmd, args).withCurrentWorkingDirectory
      .spawn[IO]
      .use { process =>
        val drainAll = (
          fs2.Stream.empty.through(process.stdin).compile.drain,
          fs2.text.utf8
            .decode(process.stderr)
            .evalMapChunk(IO.consoleForIO.error)
            .compile
            .drain,
          fs2.text.utf8.decode(process.stdout).compile.string
        ).parTupled

        for {
          res <- drainAll
          stdOut = res._3
          result <- process.exitValue
          _ <- IO
            .raiseError(
              new Exception(s"$cmd ${args.mkString(" ")} returned $result")
            )
            .whenA(result != 0)
        } yield stdOut
      }
  }

  def env(name: String): IO[Option[String]] =
    IO {
      val value = js.Dynamic.global.process.env.selectDynamic(name)
      if (js.isUndefined(value) || value == null) None
      else Some(value.asInstanceOf[String])
    }

  def hostOs: IO[String] =
    IO(js.Dynamic.global.process.platform.asInstanceOf[String])

  def hostArch: IO[String] =
    IO(js.Dynamic.global.process.arch.asInstanceOf[String])

  val gitShaHead: IO[String] =
    systemStdout("git", "rev-parse" :: "HEAD" :: Nil).map(_.trim)

  val pathOrdering: Ordering[Path] = Path.instances.toOrdering

  private val FilesIO = Files.forIO

  private val parResource: Resource[IO, Par.EC] =
    Resource
      .make(IO(Par.newService()))(es => IO(Par.shutdownService(es)))
      .map(Par.ecFromService(_))

  def withEC[A](fn: Par.EC ?=> IO[A]): IO[A] =
    parResource.use(ec => fn(using ec))

  def readUtf8(p: Path): IO[String] =
    FilesIO.readUtf8(p).compile.string

  def readBytes(p: Path): IO[Array[Byte]] =
    FilesIO.readAll(p).compile.to(Array)

  def fsDataType(p: Path): IO[Option[PlatformIO.FSDataType]] =
    FilesIO
      .exists(p, followLinks = true)
      .flatMap {
        case false => IO.pure(None)
        case true  =>
          FilesIO
            .isDirectory(p, followLinks = true)
            .map {
              case true  => Some(PlatformIO.FSDataType.Dir)
              case false => Some(PlatformIO.FSDataType.File)
            }
      }

  def parent(p: Path): Option[Path] =
    p.parent

  private def read[A <: GeneratedMessage](
      path: Path
  )(implicit A: GeneratedMessageCompanion[A]): IO[A] =
    FilesIO
      .readAll(path)
      .compile
      .to(Array)
      .flatMap { bytes =>
        IO(A.parseFrom(bytes))
      }

  def readHashed[A <: GeneratedMessage, H](
      path: Path
  )(implicit
      gmc: GeneratedMessageCompanion[A],
      algo: Algo[H]
  ): IO[Hashed[H, A]] =
    FilesIO
      .readAll(path)
      .compile
      .to(Array)
      .flatMap { bytes =>
        IO {
          val a = gmc.parseFrom(bytes)
          Hashed(Algo.hashBytes[H](bytes), a)
        }
      }

  def readPackages(paths: List[Path]): IO[List[Package.Compiled]] =
    paths
      .parTraverse { path =>
        for {
          ppack <- read[proto.Packages](path)
          packs <- IO.fromTry(
            ProtoConverter.packagesFromProto(Nil, ppack.packages)
          )
        } yield packs._2
      }
      .map(_.flatten)

  override def readInterfaceBundle(path: Path): IO[proto.Interfaces] =
    read[proto.Interfaces](path)

  override def writeInterfaceBundle(bundle: proto.Interfaces, path: Path): IO[Unit] =
    write(bundle, path)

  def readLibrary(path: Path): IO[Hashed[Algo.Blake3, proto.Library]] =
    readHashed[proto.Library, Algo.Blake3](path)

  def fetchHash[A](
      algo: Algo[A],
      hash: HashValue[A],
      path: Path,
      uri: String
  ): IO[Either[PlatformIO.FetchHashFailure, Unit]] = {
    // Create a Blaze client resource
    import org.http4s._
    import fs2.io.file.{Files, CopyFlags, CopyFlag}
    import org.http4s.client.middleware.FollowRedirect
    import org.http4s.headers.`User-Agent`
    import PlatformIO.FetchHashFailure

    val clientResource: Resource[IO, org.http4s.client.Client[IO]] =
      org.http4s.ember.client.EmberClientBuilder
        .default[IO]
        .withMaxResponseHeaderSize(64 * 1024)
        .build
        .map(FollowRedirect(maxRedirects = 10))

    val hashFile: fs2.Pipe[IO, Byte, HashValue[A]] =
      in =>
        fs2.Stream.suspend {
          in.chunks
            .fold(algo.newHasher()) { (h, c) =>
              val bytes = c.toArraySlice
              algo.hashBytes(h, bytes.values, bytes.offset, bytes.size)
            }
            .flatMap(h => fs2.Stream.emit(algo.finishHash(h)))
        }

    val tempFileRes = FilesIO.tempFile(
      dir = path.parent,
      prefix = s"${algo.name}_${hash.hex.take(12)}",
      suffix = "temp",
      permissions = None
    )

    def networkFailure(err: Throwable): FetchHashFailure = {
      val message = Option(err.getMessage).getOrElse(err.toString)
      FetchHashFailure.Network(uri, message)
    }

    path.parent.traverse_(FilesIO.createDirectories(_)) *>
      (Uri.fromString(uri) match {
        case Left(err) =>
          IO.pure(Left(FetchHashFailure.Network(uri, err.sanitized)))
        case Right(parsedUri) =>
          (clientResource, tempFileRes).tupled.use { case (client, tempPath) =>
            // Create an HTTP GET request
            val request =
              Request[IO](method = Method.GET, uri = parsedUri)
                .putHeaders(`User-Agent`(ProductId("bosatsu", None)))

            client
              .run(request)
              .use { response =>
                if (response.status.isSuccess) {
                  response.body
                    .broadcastThrough(
                      Files[IO].writeAll(tempPath),
                      hashFile
                    )
                    .compile
                    .lastOrError
                    .flatMap { computedHash =>
                      if (computedHash === hash) {
                        // move it atomically to output
                        FilesIO
                          .move(
                            source = tempPath,
                            target = path,
                            // Reflink tries to do an atomic copy, and falls back to non-atomic if not
                            CopyFlags(CopyFlag.Reflink, CopyFlag.ReplaceExisting)
                          )
                          .as(Right(()))
                      } else {
                        IO.pure(
                          Left(
                            FetchHashFailure.HashMismatch(
                              uri,
                              hash.toIdent(using algo),
                              computedHash.toIdent(using algo)
                            )
                          )
                        )
                      }
                    }
                } else {
                  IO.pure(
                    Left(FetchHashFailure.HttpStatus(uri, response.status.toString))
                  )
                }
              }
              .handleError(err => Left(networkFailure(err)))
          }
      })
  }

  def resolve(p: Path, c: String): Path = p.resolve(c)
  def resolve(p: Path, c: Path): Path = p.resolve(c)
  def relativize(root: Path, pf: Path): Option[Path] =
    if (pathOrdering.equiv(root, Path(".")) && !pf.isAbsolute) Some(pf)
    else if (pf.startsWith(root)) Some(root.relativize(pf))
    else None

  def unfoldDir(path: Path): IO[Option[IO[List[Path]]]] =
    FilesIO
      .isDirectory(path, followLinks = true)
      .map {
        case true =>
          Some {
            // create a list of children
            FilesIO.list(path).compile.toList
          }
        case false => None
      }

  def hasExtension(str: String): Path => Boolean = { (path: Path) =>
    path.extName == str
  }

  private def docStream(doc: Doc): fs2.Stream[IO, String] =
    fs2.Stream.fromIterator[IO](doc.renderStream(100).iterator, chunkSize = 128)

  def writeDoc(p: Path, d: Doc): IO[Unit] =
    p.parent.traverse_(FilesIO.createDirectories(_)) *>
      FilesIO.writeUtf8(p)(docStream(d)).compile.drain

  def writeDocAtomic(p: Path, d: Doc): IO[Unit] =
    writeFileSetAtomic(List(p -> d.render(100).getBytes("UTF-8").toVector))

  def writeFileSetAtomic(files: List[(Path, Vector[Byte])]): IO[Unit] =
    Fs2PlatformIO.writeFilesWith(files, documentWriteOps)

  private def onParts(doc: Doc)(fn: String => IO[Unit]): IO[Unit] =
    docStream(doc)
      .evalMapChunk(fn)
      .compile
      .drain

  private lazy val nodeFs = global.require("fs")

  private def nodeBufferToBytes(buffer: js.Dynamic): Array[Byte] = {
    val length = buffer.length.asInstanceOf[Int]
    val out = new Array[Byte](length)
    var idx = 0
    while (idx < length) {
      out(idx) = (buffer(idx).asInstanceOf[Int] & 0xff).toByte
      idx += 1
    }
    out
  }

  private def bytesToUint8Array(bytes: Array[Byte]): Uint8Array = {
    val out = new Uint8Array(bytes.length)
    var idx = 0
    while (idx < bytes.length) {
      out(idx) = (bytes(idx) & 0xff).toShort
      idx += 1
    }
    out
  }

  def writeStdout(doc: Doc): IO[Unit] =
    onParts(doc)(part => IO.print(part))

  def writeError(doc: Doc): IO[Unit] =
    onParts(doc)(part => IO.consoleForIO.error(part))

  def readStdinBytes: IO[Array[Byte]] =
    IO.blocking(nodeBufferToBytes(nodeFs.readFileSync(0)))

  def writeStdoutBytes(bytes: Array[Byte]): IO[Unit] =
    IO.blocking {
      val _ = nodeFs.writeFileSync(1, bytesToUint8Array(bytes))
      ()
    }

  def println(str: String): IO[Unit] =
    IO.println(str)

  def errorln(str: String): IO[Unit] =
    IO.consoleForIO.errorln(str)

  private def write[A <: GeneratedMessage](a: A, path: Path): IO[Unit] =
    FilesIO
      .writeAll(path)(fs2.Stream.chunk(fs2.Chunk.array(a.toByteArray)))
      .compile
      .drain

  def writeInterfaces(
      interfaces: List[Package.Interface],
      path: Path
  ): IO[Unit] =
    IO.fromTry(ProtoConverter.interfacesToProto(interfaces))
      .flatMap(write(_, path))

  def writePackages(packages: List[Package.Compiled], path: Path): IO[Unit] =
    IO.fromTry(ProtoConverter.packagesToProto(packages))
      .flatMap(write(_, path))

  def writeBytes(path: Path, bytes: Array[Byte]): IO[Unit] =
    FilesIO
      .writeAll(path)(fs2.Stream.chunk(fs2.Chunk.array(bytes)))
      .compile
      .drain

  def writeLibrary(lib: proto.Library, path: Path): IO[Unit] =
    write(lib, path)
}

object Fs2PlatformIO extends Fs2PlatformIO(None) {
  private[dev] final case class DocumentWriteOps(
      createParentDirectories: Path => IO[Unit],
      resolveDestination: Path => IO[Path],
      stagedFile: Path => Resource[IO, Path],
      writeStaged: (Path, Array[Byte]) => IO[Unit],
      needsInstall: (Path, Path) => IO[Boolean],
      replace: (Path, Path) => IO[Boolean],
      remove: Path => IO[Unit]
  )

  private def shouldInstall(source: Path, target: Path): IO[Boolean] =
    Files[IO].getBasicFileAttributes(target, followLinks = false).map(Some(_))
      .recover { case _: NoSuchFileException => None }
      .flatMap {
        case None => IO.pure(true)
        case Some(attributes) if !attributes.isRegularFile =>
          IO.raiseError(new IOException(s"expected a regular output file: $target"))
        case Some(_) =>
          // Node exposes mode/owner/group, but no ACL reader or copier. Keep
          // identical outputs in place; fail closed for changed ones.
          Files[IO].readAll(source).map(Option(_))
            .zipAll(Files[IO].readAll(target).map(Option(_)))(None, None)
            .forall { case (left, right) => left == right }
            .compile.lastOrError.flatMap {
              case true => IO.pure(false)
              case false => IO.raiseError(new IOException(
                s"cannot preserve output access controls with Node's filesystem API: $target"
              ))
            }
      }

  private[dev] val documentWriteOps: DocumentWriteOps = DocumentWriteOps(
    createParentDirectories = p => p.parent.traverse_(Files[IO].createDirectories(_)),
    resolveDestination = path => Fs2PlatformIO.pendingOutputPath(path),
    stagedFile = destination => {
      val directory = destination.parent.getOrElse(Path("."))
      Resource
        .make(Files[IO].createTempDirectory(Some(directory), ".bosatsu-write-", None)) { stage =>
          // Own the directory, not fs2's JS tempFile, whose release removes only
          // the file. Cleanup must not report failure after a committed rename.
          Files[IO].deleteRecursively(stage).handleErrorWith(_ => IO.unit)
        }
        .map(_ / "document")
    },
    writeStaged = (path, bytes) =>
      Files[IO].writeAll(path)(fs2.Stream.chunk(fs2.Chunk.array(bytes))).compile.drain,
    needsInstall = shouldInstall,
    replace = (source, target) =>
      shouldInstall(source, target).flatMap {
        case false => IO.pure(false)
        case true =>
          IO.fromPromise(IO(Rename(source.toString, target.toString))).as(true)
        },
    remove = path =>
      IO.fromPromise(IO(Unlink(path.toString))).handleErrorWith {
        case _: NoSuchFileException => IO.unit
        case error => IO.raiseError(error)
      }
  )

  private[dev] def withDocumentWriteOps(ops: DocumentWriteOps): Fs2PlatformIO =
    new Fs2PlatformIO(Some(ops))

  private final case class PendingFile(
      destination: Path,
      staged: Path,
      bytes: Vector[Byte]
  )

  private[dev] def writeDocWith(
      path: Path,
      doc: Doc,
      ops: DocumentWriteOps
  ): IO[Unit] =
    writeFilesWith(List(path -> doc.render(100).getBytes("UTF-8").toVector), ops)

  private[dev] def writeFilesWith(
      files: List[(Path, Vector[Byte])],
      ops: DocumentWriteOps
  ): IO[Unit] = {
    val destinations = files.traverse { case (path, bytes) =>
      ops.createParentDirectories(path) *>
        ops.resolveDestination(path).map(destination => (destination, bytes))
    }

    destinations.flatMap { resolved =>
      val uniqueDestinations = resolved.map(_._1).distinct
      if (uniqueDestinations.size != resolved.size)
        IO.raiseError(new IOException("multiple outputs refer to the same file"))
      else {
        val staged = resolved.traverse { case (destination, bytes) =>
          ops.stagedFile(destination).map(path => PendingFile(destination, path, bytes))
        }

        staged.use { pending =>
          pending.traverse_ { file =>
            ops.writeStaged(file.staged, file.bytes.toArray)
          } *> pending.traverse(file =>
            ops.needsInstall(file.staged, file.destination)
          ).flatMap { shouldInstall =>
            // Once installation begins, keep each rename adjacent to its
            // bookkeeping and complete rollback before honoring cancellation.
            IO.uncancelable { poll =>
              IO.defer {
                var installed = List.empty[Path]
                val installation = pending.zip(shouldInstall).traverse_ {
                  case (file, true) =>
                    ops.replace(file.staged, file.destination).flatMap {
                      case true => IO { installed = file.destination :: installed }
                      case false => IO.unit
                    }
                  case (_, false) => IO.unit
                }.handleErrorWith { failure =>
                  installed.traverse(path => ops.remove(path).attempt).flatMap { rollback =>
                    rollback.collect { case Left(error) => error }
                      .foreach(failure.addSuppressed)
                    IO.raiseError(failure)
                  }
                }
                installation.attempt.flatMap { result =>
                  // Let queued cancellation take effect only after publication or
                  // the full rollback, while retaining ordinary install failures.
                  poll(IO.cede) *> IO.fromEither(result)
                }
              }
            }
          }
        }
      }
    }
  }
}
