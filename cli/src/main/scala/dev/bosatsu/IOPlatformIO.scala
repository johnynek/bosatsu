package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.MonadError
import cats.effect.{IO, Resource}
import cats.effect.kernel.Deferred
import com.monovore.decline.Argument
import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Path => JPath, Files}
import java.io.{
  FileInputStream,
  FileOutputStream,
  BufferedInputStream,
  BufferedOutputStream,
  PrintStream
}
import org.typelevel.paiges.Doc
import dev.bosatsu.graph.CanPromise
import dev.bosatsu.hashing.{Hashed, HashValue, Algo}
import dev.bosatsu.tool.CliException
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}

import cats.syntax.all._

object IOPlatformIO extends IOPlatformIO

class IOPlatformIO extends PlatformIO[IO, JPath] {
  type F[A] = IO[A]
  type Path = JPath

  // Keep compiler-visible paths independent of the machine running a request.
  // Only translate them when crossing the filesystem boundary.
  protected def diskPath(path: Path): Path = path
  protected def outputPath(path: Path): IO[Path] = IO.pure(diskPath(path))

  protected def stdout: PrintStream = System.out
  protected def stderr: PrintStream = System.err

  def pathOrdering = Ordering.ordered[JPath]

  override def pathArg: Argument[Path] =
    Argument.readPath

  def pathToString(p: Path): String = p.toString

  private def systemCmd[A](
      cmd: String,
      args: List[String],
      bldr: java.lang.ProcessBuilder
  )(fn: java.lang.Process => A): IO[A] = IO.blocking {
    // Start the process
    val process = bldr.start()
    val result = fn(process)
    // Wait for the process to complete and check the exit value
    val exitCode = process.waitFor()
    if (exitCode != 0) {
      val fullCmd =
        if (args.isEmpty) cmd else s"$cmd ${args.mkString(" ")}"
      throw CliException.Basic(
        s"command $fullCmd failed with exit code: $exitCode"
      )
    }

    result
  }

  private def processBldr(
      cmd: String,
      args: List[String]
  ): IO[java.lang.ProcessBuilder] = IO {
    val processBuilder = new java.lang.ProcessBuilder()
    val command = new java.util.ArrayList[String]()
    (cmd :: args).foreach(command.add(_))

    processBuilder.command(command)
  }

  def system(cmd: String, args: List[String]): IO[Unit] =
    processBldr(cmd, args).flatMap { processBuilder =>
      // Redirect output and error streams
      processBuilder.redirectOutput(ProcessBuilder.Redirect.INHERIT)
      processBuilder.redirectError(ProcessBuilder.Redirect.INHERIT)
      systemCmd(cmd, args, processBuilder)(_ => ())
    }

  def systemStdout(cmd: String, args: List[String]): IO[String] =
    processBldr(cmd, args).flatMap { processBuilder =>
      processBuilder.redirectErrorStream(true)
      processBuilder.redirectOutput(ProcessBuilder.Redirect.PIPE)
      systemCmd(cmd, args, processBuilder) { process =>
        val reader = new java.io.BufferedReader(
          new java.io.InputStreamReader(process.getInputStream)
        )
        val output = new StringBuilder
        var line: String = null
        while ({ line = reader.readLine(); line != null }) {
          output.append(line).append("\n"): Unit
        }
        reader.close()
        output.toString
      }
    }

  val gitShaHead: IO[String] = {
    systemStdout("git", "rev-parse" :: "HEAD" :: Nil).map(_.trim)
  }

  def env(name: String): IO[Option[String]] =
    IO(Option(System.getenv(name)))

  def hostOs: IO[String] =
    IO(Option(System.getProperty("os.name")).getOrElse("unknown"))

  def hostArch: IO[String] =
    IO(Option(System.getProperty("os.arch")).getOrElse("unknown"))

  override def moduleIOMonad: MonadError[IO, Throwable] =
    cats.effect.IO.asyncForIO

  override def gitTopLevel: IO[Option[Path]] = {
    val start = diskPath(Paths.get(".")).toAbsolutePath.normalize
    gitTopLevelFrom(start)
  }

  private[bosatsu] def deleteRecursively(path: Path): IO[Unit] =
    IO.blocking {
      if (Files.exists(path)) {
        val stream = Files.walk(path)
        try {
          stream.sorted(java.util.Comparator.reverseOrder()).forEach { p =>
            Files.deleteIfExists(p): Unit
          }
        } finally {
          stream.close()
        }
      }
    }

  protected def newTempDirectory(name: String): IO[Path] =
    gitTopLevel.flatMap {
      case Some(root) =>
        IO.blocking {
          val base =
            Files.createDirectories(resolve(root, ".bosatsuc" :: "tmp" :: Nil))
          Files.createTempDirectory(base, name)
        }
      case None => IO.blocking(Files.createTempDirectory(name))
    }

  def withTempPrefix[A](name: String)(fn: Path => IO[A]): IO[A] = {
    def cleanup(path: Path): IO[Unit] =
      deleteRecursively(path).handleErrorWith { err =>
        errorln(
          s"warning: failed to delete temp dir ${path.toString}: ${err.getMessage}"
        )
      }

    Resource.make(newTempDirectory(name))(cleanup).use(fn)
  }

  protected def startPromise(action: IO[Unit]): IO[Unit] = action.start.void

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
        startPromise(fa.attempt.flatMap(p.complete).void)

      def wait[A](p: Promise[A]): IO[A] =
        p.get.rethrow
    }

  private[bosatsu] val parResource: Resource[IO, Par.EC] =
    Resource
      .make(IO(Par.newService()))(es => IO(Par.shutdownService(es)))
      .map(Par.ecFromService(_))

  def withEC[A](fn: Par.EC ?=> IO[A]): IO[A] =
    parResource.use(ec => fn(using ec))

  def readUtf8(path: Path): IO[String] =
    IO.blocking(
      new String(Files.readAllBytes(diskPath(path)), StandardCharsets.UTF_8)
    )

  def readBytes(path: Path): IO[Array[Byte]] =
    IO.blocking(Files.readAllBytes(diskPath(path)))

  def fsDataType(p: Path): IO[Option[PlatformIO.FSDataType]] =
    IO.blocking {
      val f = diskPath(p).toFile()
      if (f.exists()) Some {
        if (f.isDirectory()) PlatformIO.FSDataType.Dir
        else PlatformIO.FSDataType.File
      }
      else None
    }

  def resolve(p: Path, c: String): Path = p.resolve(c)
  def resolve(p: Path, c: Path): Path = p.resolve(c)
  def relativize(root: Path, pf: Path): Option[Path] =
    if (pathOrdering.equiv(root, Paths.get(".")) && !pf.isAbsolute) Some(pf)
    else if (pf.startsWith(root)) Some {
      root.relativize(pf)
    }
    else None
  def parent(p: Path): Option[Path] =
    Option(p.getParent)

  def read[A <: GeneratedMessage](
      path: Path
  )(implicit gmc: GeneratedMessageCompanion[A]): IO[A] =
    IO.blocking {
      val f = diskPath(path).toFile
      val ios = new BufferedInputStream(new FileInputStream(f))
      try gmc.parseFrom(ios)
      finally {
        ios.close
      }
    }

  def readHashed[A <: GeneratedMessage, H](
      path: Path
  )(implicit
      gmc: GeneratedMessageCompanion[A],
      algo: Algo[H]
  ): IO[Hashed[H, A]] =
    IO.blocking {
      val bytes = Files.readAllBytes(diskPath(path))
      val a = gmc.parseFrom(bytes)
      Hashed(Algo.hashBytes[H](bytes), a)
    }

  private def writeStream(
      path: Path
  )(fn: BufferedOutputStream => Unit): IO[Unit] =
    outputPath(path).flatMap { target =>
      IO.blocking {
        Option(target.getParent).foreach(Files.createDirectories(_))
        val os = new BufferedOutputStream(new FileOutputStream(target.toFile))
        try fn(os)
        finally os.close()
      }
    }

  def write(a: GeneratedMessage, path: Path): IO[Unit] =
    writeStream(path)(a.writeTo(_))

  def writeBytes(path: Path, bytes: Array[Byte]): IO[Unit] =
    writeStream(path)(_.write(bytes))

  def readInterfacesAndPackages(
      ifacePaths: List[Path],
      packagePaths: List[Path]
  ): IO[(List[Package.Interface], List[Package.Compiled])] =
    (
      ifacePaths.traverse(read[proto.Interfaces](_)),
      packagePaths.traverse(read[proto.Packages](_))
    ).flatMapN { (ifs, packs) =>
      IO.fromTry(
        ProtoConverter.packagesFromProto(
          ifs.flatMap(_.interfaces),
          packs.flatMap(_.packages)
        )
      )
    }

  def readInterfaces(paths: List[Path]): IO[List[Package.Interface]] =
    readInterfacesAndPackages(paths, Nil).map(_._1)

  def readPackages(paths: List[Path]): IO[List[Package.Compiled]] =
    readInterfacesAndPackages(Nil, paths).map(_._2)

  def readLibrary(path: Path): IO[Hashed[Algo.Blake3, proto.Library]] =
    readHashed[proto.Library, Algo.Blake3](path)

  def fetchHash[A](
      algo: Algo[A],
      hash: HashValue[A],
      path: Path,
      uri: String
  ): F[Either[PlatformIO.FetchHashFailure, Unit]] = {
    // Create a Blaze client resource
    import org.http4s._
    import fs2.io.file.{Files, Path => Fs2Path, CopyFlags, CopyFlag}
    import org.http4s.client.middleware.FollowRedirect
    import org.http4s.headers.`User-Agent`
    import PlatformIO.FetchHashFailure

    val filesIO = Files[IO]

    import org.http4s.ember.client.EmberClientBuilder

    val clientResource: Resource[IO, org.http4s.client.Client[IO]] =
      EmberClientBuilder
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

    val parent = Option(path.getParent)
    val fs2Parent = parent.map(Fs2Path.fromNioPath)
    val tempFileRes = filesIO.tempFile(
      dir = fs2Parent,
      prefix = s"${algo.name}_${hash.hex.take(12)}",
      suffix = "temp",
      permissions = None
    )

    def networkFailure(err: Throwable): FetchHashFailure = {
      val message = Option(err.getMessage).getOrElse(err.toString)
      FetchHashFailure.Network(uri, message)
    }

    fs2Parent.traverse_(filesIO.createDirectories(_)) *>
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
                        filesIO
                          .move(
                            source = tempPath,
                            target = Fs2Path.fromNioPath(path),
                            CopyFlags(CopyFlag.AtomicMove, CopyFlag.ReplaceExisting)
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

  def writeInterfaces(
      interfaces: List[Package.Interface],
      path: Path
  ): IO[Unit] =
    IO.fromTry(ProtoConverter.interfacesToProto(interfaces))
      .flatMap(write(_, path))

  def writePackages(packages: List[Package.Compiled], path: Path): IO[Unit] =
    IO.fromTry(ProtoConverter.packagesToProto(packages))
      .flatMap(write(_, path))

  def writeLibrary(lib: proto.Library, path: Path): IO[Unit] =
    write(lib, path)

  def unfoldDir(path: Path): IO[Option[IO[List[Path]]]] =
    IO.blocking {
      val f = diskPath(path).toFile

      if (f.isDirectory()) {
        Some(IO.blocking {
          f.listFiles.iterator.map(child => path.resolve(child.getName)).toList
        })
      } else None
    }

  def hasExtension(str: String): Path => Boolean = { (path: Path) =>
    path.toString.endsWith(str)
  }

  def writeDoc(p: Path, d: Doc): IO[Unit] =
    outputPath(p).flatMap { target =>
      IO.blocking {
        Option(target.getParent).foreach(Files.createDirectories(_))
        val writer = Files.newBufferedWriter(target, StandardCharsets.UTF_8)
        try d.renderStream(100).foreach(writer.write(_))
        finally writer.close()
      }
    }

  def println(str: String): IO[Unit] = IO.blocking(stdout.println(str))
  def errorln(str: String): IO[Unit] = IO.blocking(stderr.println(str))

  private def printDoc(doc: Doc, stream: PrintStream): IO[Unit] =
    IO.blocking {
      doc.renderStreamTrim(80).iterator.foreach(stream.print)
      stream.println()
    }

  def writeStdout(doc: Doc): IO[Unit] = printDoc(doc, stdout)
  def writeError(doc: Doc): IO[Unit] = printDoc(doc, stderr)

  def readStdinBytes: IO[Array[Byte]] = IO.blocking(System.in.readAllBytes())

  def writeStdoutBytes(bytes: Array[Byte]): IO[Unit] =
    IO.blocking {
      stdout.write(bytes)
      stdout.flush()
    }
}
