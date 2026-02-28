package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.MonadError
import cats.effect.{IO, Resource}
import com.monovore.decline.Argument
import java.nio.file.{Paths, Path => JPath, Files}
import java.io.{
  FileInputStream,
  FileOutputStream,
  BufferedInputStream,
  BufferedOutputStream,
  PrintWriter
}
import org.typelevel.paiges.Doc
import dev.bosatsu.hashing.{Hashed, HashValue, Algo}
import dev.bosatsu.tool.CliException
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}

import cats.syntax.all._

object IOPlatformIO extends PlatformIO[IO, JPath] {
  type F[A] = IO[A]
  type Path = JPath

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

  val gitShaHead: IO[String] = {
    val args = "rev-parse" :: "HEAD" :: Nil
    processBldr("git", args).flatMap { processBuilder =>
      // Combine stdout and stderr, and pipe the combined output for capturing
      processBuilder.redirectErrorStream(true)
      processBuilder.redirectOutput(ProcessBuilder.Redirect.PIPE)
      systemCmd("git", args, processBuilder) { process =>
        // Prepare to read the combined output
        val reader = new java.io.BufferedReader(
          new java.io.InputStreamReader(process.getInputStream)
        )
        val output = new StringBuilder
        var line: String = null

        // Read all lines from the process's output
        while ({ line = reader.readLine(); line != null }) {
          output.append(line).append("\n"): Unit
        }
        reader.close()
        output.toString.trim
      }
    }
  }

  override def moduleIOMonad: MonadError[IO, Throwable] =
    cats.effect.IO.asyncForIO

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

    val start = Paths.get(".").toAbsolutePath.normalize
    moduleIOMonad.tailRecM(start)(searchStep)
  }

  private def deleteRecursively(path: Path): IO[Unit] =
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

  def withTempPrefix[A](name: String)(fn: Path => IO[A]): IO[A] = {
    val baseDirF =
      gitTopLevel.map(_.map(resolve(_, ".bosatsuc" :: "tmp" :: Nil)))

    val tempDirF = baseDirF.flatMap {
      case Some(base) =>
        IO.blocking {
          Files.createDirectories(base)
          Files.createTempDirectory(base, name)
        }
      case None =>
        IO.blocking(Files.createTempDirectory(name))
    }

    def cleanup(path: Path): IO[Unit] =
      deleteRecursively(path).handleErrorWith { err =>
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

  override val parallelF: cats.Parallel[IO] = IO.parallelForIO

  private val parResource: Resource[IO, Par.EC] =
    Resource
      .make(IO(Par.newService()))(es => IO(Par.shutdownService(es)))
      .map(Par.ecFromService(_))

  def withEC[A](fn: Par.EC ?=> IO[A]): IO[A] =
    parResource.use(ec => fn(using ec))

  def readUtf8(path: Path): IO[String] =
    IO.blocking(new String(Files.readAllBytes(path), "utf-8"))

  def fsDataType(p: Path): IO[Option[PlatformIO.FSDataType]] =
    IO.blocking {
      val f = p.toFile()
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

  def read[A <: GeneratedMessage](
      path: Path
  )(implicit gmc: GeneratedMessageCompanion[A]): IO[A] =
    IO.blocking {
      val f = path.toFile
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
      val bytes = Files.readAllBytes(path)
      val a = gmc.parseFrom(bytes)
      Hashed(Algo.hashBytes[H](bytes), a)
    }

  def write(a: GeneratedMessage, path: Path): IO[Unit] =
    IO.blocking {
      val f = path.toFile
      Option(f.getParentFile).foreach(_.mkdirs())
      val os = new BufferedOutputStream(new FileOutputStream(f))
      try a.writeTo(os)
      finally {
        os.close
      }
    }

  def writeBytes(path: Path, bytes: Array[Byte]): IO[Unit] =
    IO.blocking {
      val f = path.toFile
      Option(f.getParentFile).foreach(_.mkdirs())
      val os = new BufferedOutputStream(new FileOutputStream(f))
      try os.write(bytes)
      finally {
        os.close
      }
    }

  def readInterfacesAndPackages(
      ifacePaths: List[Path],
      packagePaths: List[Path]
  ): IO[(List[Package.Interface], List[Package.Typed[Unit]])] =
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

  def readPackages(paths: List[Path]): IO[List[Package.Typed[Unit]]] =
    readInterfacesAndPackages(Nil, paths).map(_._2)

  def readLibrary(path: Path): IO[Hashed[Algo.Blake3, proto.Library]] =
    readHashed[proto.Library, Algo.Blake3](path)

  def fetchHash[A](
      algo: Algo[A],
      hash: HashValue[A],
      path: Path,
      uri: String
  ): F[Unit] = {
    // Create a Blaze client resource
    import org.http4s._
    import fs2.io.file.{Files, Path => Fs2Path, CopyFlags, CopyFlag}
    import org.http4s.client.middleware.FollowRedirect
    import org.http4s.headers.`User-Agent`

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

    fs2Parent.traverse_(filesIO.createDirectories(_)) *>
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
            if (computedHash === hash) {
              // move it atomically to output
              filesIO.move(
                source = tempPath,
                target = Fs2Path.fromNioPath(path),
                CopyFlags(CopyFlag.AtomicMove, CopyFlag.ReplaceExisting)
              )
            } else {
              IO.raiseError(
                new Exception(
                  s"from $uri expected hash to be ${hash.toIdent(using algo)} but found ${computedHash
                      .toIdent(using algo)}"
                )
              )
            }
          }
      }
  }

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

  def unfoldDir(path: Path): IO[Option[IO[List[Path]]]] =
    IO.blocking {
      val f = path.toFile

      if (f.isDirectory()) {
        Some(IO.blocking {
          f.listFiles.iterator.map(_.toPath).toList
        })
      } else None
    }

  def hasExtension(str: String): Path => Boolean = { (path: Path) =>
    path.toString.endsWith(str)
  }

  def pathPackage(roots: List[Path], packFile: Path): Option[PackageName] =
    PlatformIO.pathPackage(roots, packFile) { (root, pf) =>
      if (pf.startsWith(root)) Some {
        import scala.jdk.CollectionConverters._
        root.relativize(pf).asScala.map(_.toString)
      }
      else None
    }

  def writeDoc(p: Path, d: Doc): IO[Unit] =
    IO.blocking {
      Option(p.getParent).foreach(_.toFile.mkdirs)
      val pw = new PrintWriter(p.toFile, "UTF-8")
      try d.renderStream(100).foreach(pw.print(_))
      finally {
        pw.close
      }
    }

  def println(str: String): IO[Unit] =
    IO.println(str)

  def errorln(str: String): IO[Unit] =
    IO.consoleForIO.errorln(str)

  def writeStdout(doc: Doc): IO[Unit] =
    IO.blocking {
      doc
        .renderStreamTrim(80)
        .iterator
        .foreach(System.out.print)

      System.out.println("")
    }

  def writeError(doc: Doc): IO[Unit] =
    IO.blocking {
      doc
        .renderStreamTrim(80)
        .iterator
        .foreach(System.err.print)

      System.out.println("")
    }

}
