package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.MonadError
import cats.data.Validated
import cats.effect.{IO, Resource}
import fs2.io.file.{Files, Path}
import com.monovore.decline.Argument
import org.typelevel.paiges.Doc
import dev.bosatsu.hashing.{Algo, Hashed, HashValue}
import scala.util.{Failure, Success, Try}
import scala.scalajs.js

import cats.syntax.all._
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}

object Fs2PlatformIO extends PlatformIO[IO, Path] {
  def moduleIOMonad: MonadError[IO, Throwable] =
    IO.asyncForIO

  override val parallelF: cats.Parallel[IO] = IO.parallelForIO

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
    def searchStep(current: Path): IO[Either[Path, Option[Path]]] =
      fsDataType(current).flatMap {
        case Some(PlatformIO.FSDataType.Dir) =>
          fsDataType(resolve(current, ".git"))
            .map {
              case Some(PlatformIO.FSDataType.Dir) => Right(Some(current))
              case _ => Left(resolve(current, ".."))
            }
        case _ => moduleIOMonad.pure(Right(None))
      }

    val start = Path(js.Dynamic.global.process.cwd().asInstanceOf[String])
    moduleIOMonad.tailRecM(start)(searchStep)
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

  def readPackages(paths: List[Path]): IO[List[Package.Typed[Unit]]] =
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

  def readInterfaces(paths: List[Path]): IO[List[Package.Interface]] =
    paths
      .parTraverse { path =>
        for {
          pifaces <- read[proto.Interfaces](path)
          ifaces <- IO.fromTry(
            ProtoConverter.packagesFromProto(pifaces.interfaces, Nil)
          )
        } yield ifaces._1
      }
      .map(_.flatten)

  def readLibrary(path: Path): IO[Hashed[Algo.Blake3, proto.Library]] =
    readHashed[proto.Library, Algo.Blake3](path)

  def fetchHash[A](
      algo: Algo[A],
      hash: HashValue[A],
      path: Path,
      uri: String
  ): IO[Unit] = {
    // Create a Blaze client resource
    import org.http4s._
    import fs2.io.file.{Files, CopyFlags, CopyFlag}
    import org.http4s.client.middleware.FollowRedirect
    import org.http4s.headers.`User-Agent`

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

    path.parent.traverse_(FilesIO.createDirectories(_)) *>
      (
        clientResource,
        tempFileRes,
        Resource.eval(IO(Uri.unsafeFromString(uri)))
      ).tupled.use { case (client, tempPath, uri) =>
        // Create an HTTP GET request
        val request =
          Request[IO](method = Method.GET, uri = uri)
            .putHeaders(`User-Agent`(ProductId("bosatsu", None)))

        // Stream the response body and write it to the specified file path
        client
          .stream(request)
          .flatMap { response =>
            if (response.status.isSuccess) {
              response.body
            } else {
              fs2.Stream.raiseError[IO](
                new Exception(
                  s"Failed to download from $uri: ${response.status}"
                )
              )
            }
          }
          .broadcastThrough(
            Files[IO].writeAll(tempPath),
            hashFile
          )
          .compile
          .lastOrError
          .flatMap { computedHash =>
            if (computedHash == hash) {
              // move it atomically to output
              FilesIO.move(
                source = tempPath,
                target = path,
                // Reflink tries to do an atomic copy, and falls back to non-atomic if not
                CopyFlags(CopyFlag.Reflink, CopyFlag.ReplaceExisting)
              )
            } else {
              IO.raiseError(
                new Exception(
                  s"from $uri expected hash to be ${hash.toIdent(using algo)} but found ${computedHash.toIdent(using algo)}"
                )
              )
            }
          }
      }
  }

  /** given an ordered list of prefered roots, if a packFile starts with one of
    * these roots, return a PackageName based on the rest
    */
  def pathPackage(roots: List[Path], packFile: Path): Option[PackageName] =
    PlatformIO.pathPackage(roots, packFile) { (root, pf) =>
      relativize(root, pf).map(_.names.map(_.toString))
    }

  def resolve(p: Path, c: String): Path = p.resolve(c)
  def resolve(p: Path, c: Path): Path = p.resolve(c)
  def relativize(root: Path, pf: Path): Option[Path] =
    if (root == Path(".") && !pf.isAbsolute) Some(pf)
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

  def writeDoc(p: Path, d: Doc): IO[Unit] = {
    val pipe = FilesIO.writeUtf8(p)
    pipe(docStream(d)).compile.drain
  }

  private def onParts(doc: Doc)(fn: String => IO[Unit]): IO[Unit] =
    docStream(doc)
      .evalMapChunk(fn)
      .compile
      .drain

  def writeStdout(doc: Doc): IO[Unit] =
    onParts(doc)(part => IO.print(part))

  def writeError(doc: Doc): IO[Unit] =
    onParts(doc)(part => IO.consoleForIO.error(part))

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

  def writePackages[A](packages: List[Package.Typed[A]], path: Path): IO[Unit] =
    IO.fromTry(ProtoConverter.packagesToProto(packages))
      .flatMap(write(_, path))

  def writeLibrary(lib: proto.Library, path: Path): IO[Unit] =
    write(lib, path)
}
