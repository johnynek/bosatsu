package org.bykn.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.MonadError
import cats.data.Validated
import cats.effect.{IO, Resource}
import fs2.io.file.{Files, Path}
import com.monovore.decline.Argument
import org.typelevel.paiges.Doc
import scala.util.{Failure, Success, Try}

import cats.syntax.all._

object Fs2PlatformIO extends PlatformIO[IO, Path] {
  def moduleIOMonad: MonadError[IO, Throwable] =
    IO.asyncForIO

  val pathArg: Argument[Path] =
    new Argument[Path] {
      def read(string: String) =
        Try(Path(string)) match {
          case Success(value) =>
            Validated.valid(value)
          case Failure(exception) =>
            Validated.invalidNel(s"could not parse $string as path: ${exception.getMessage()}") 
        }
      
      def defaultMetavar: String = "path"
    }

  def pathToString(p: Path): String = p.toString
  def system(cmd: String, args: List[String]): IO[Unit] = {
    import fs2.io.process.ProcessBuilder
    ProcessBuilder(cmd, args)
      .withCurrentWorkingDirectory
      .spawn[IO]
      .use { process =>
        for {
          _ <- fs2.Stream.empty.through(process.stdin).compile.drain
          _ <- fs2.text.utf8.decode(process.stderr).evalMapChunk(IO.consoleForIO.error).compile.drain
          _ <- fs2.text.utf8.decode(process.stdout).evalMapChunk(IO.consoleForIO.print).compile.drain
          result <- process.exitValue
          _ <- if (result == 0) IO.pure(0) else IO.raiseError(new Exception(s"$cmd ${args.mkString(" ")} returned $result"))
        } yield ()
      }
  }

  val pathOrdering: Ordering[Path] = Path.instances.toOrdering

  private val FilesIO = Files.forIO

  private val parResource: Resource[IO, Par.EC] =
    Resource.make(IO(Par.newService()))(es => IO(Par.shutdownService(es)))
      .map(Par.ecFromService(_))

  def withEC[A](fn: Par.EC => IO[A]): IO[A] =
    parResource.use(fn)

  def readUtf8(p: Path): IO[String] =
    FilesIO.readUtf8(p).compile.string

  def fsDataType(p: Path): IO[Option[PlatformIO.FSDataType]] =
    FilesIO.exists(p, followLinks = true)
      .flatMap {
        case false => IO.pure(None)
        case true =>
          FilesIO.isDirectory(p, followLinks = true)
            .map {
              case true => Some(PlatformIO.FSDataType.Dir)
              case false => Some(PlatformIO.FSDataType.File)
            }
      }

  def readPackages(paths: List[Path]): IO[List[Package.Typed[Unit]]] =
    paths.parTraverse { path =>
      for {
        bytes <- FilesIO.readAll(path).compile.to(Array)
        ppack <- IO(proto.Packages.parseFrom(bytes))
        packs <- IO.fromTry(ProtoConverter.packagesFromProto(Nil, ppack.packages))
      } yield packs._2
    }
    .map(_.flatten)

  def readInterfaces(paths: List[Path]): IO[List[Package.Interface]] =
    paths.parTraverse { path =>
      for {
        bytes <- FilesIO.readAll(path).compile.to(Array)
        pifaces <- IO(proto.Interfaces.parseFrom(bytes))
        ifaces <- IO.fromTry(ProtoConverter.packagesFromProto(pifaces.interfaces, Nil))
      } yield ifaces._1
    }
    .map(_.flatten)

  /** given an ordered list of prefered roots, if a packFile starts with one of
    * these roots, return a PackageName based on the rest
    */
  def pathPackage(roots: List[Path], packFile: Path): Option[PackageName] =
    PlatformIO.pathPackage(roots, packFile) { (root, pf) =>
      if (pf.startsWith(root)) Some {
        root.relativize(pf).names.map(_.toString)
      }
      else None
    }

  def resolve(p: Path, c: String): Path = p.resolve(c)
  def resolve(p: Path, c: Path): Path = p.resolve(c)

  def unfoldDir(path: Path): IO[Option[IO[List[Path]]]] =
    FilesIO.isDirectory(path, followLinks = true)
      .map {
        case true => Some {
          // create a list of children
          FilesIO.list(path).compile.toList
        }
        case false => None
      }

  def hasExtension(str: String): Path => Boolean =
    { (path: Path) => path.extName == str }

  private def docStream(doc: Doc): fs2.Stream[IO, String] =
    fs2.Stream.fromIterator[IO](doc.renderStream(100).iterator, chunkSize = 128)

  def writeDoc(p: Path, d: Doc): IO[Unit] = {
    val pipe = Files.forIO.writeUtf8(p)
    pipe(docStream(d)).compile.drain
  }

  def writeStdout(doc: Doc): IO[Unit] =
    docStream(doc)
      .evalMapChunk(part => IO.print(part))
      .compile
      .drain

  def println(str: String): IO[Unit] =
    IO.println(str)

  def errorln(str: String): IO[Unit] =
    IO.consoleForIO.errorln(str)

  def writeInterfaces(
      interfaces: List[Package.Interface],
      path: Path
  ): IO[Unit] =
    for {
      protoIfaces <- IO.fromTry(ProtoConverter.interfacesToProto(interfaces))
      bytes = protoIfaces.toByteArray
      pipe = Files.forIO.writeAll(path)
      _ <- pipe(fs2.Stream.chunk(fs2.Chunk.array(bytes))).compile.drain
    } yield ()

  def writePackages[A](packages: List[Package.Typed[A]], path: Path): IO[Unit] =
    for {
      protoPacks <- IO.fromTry(ProtoConverter.packagesToProto(packages))
      bytes = protoPacks.toByteArray
      pipe = Files.forIO.writeAll(path)
      _ <- pipe(fs2.Stream.chunk(fs2.Chunk.array(bytes))).compile.drain
    } yield ()
}