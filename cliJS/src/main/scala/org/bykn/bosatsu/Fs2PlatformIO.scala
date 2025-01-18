package org.bykn.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.MonadError
import cats.data.Validated
import cats.effect.{IO, Resource}
import fs2.io.file.{Files, Path}
import com.monovore.decline.Argument
import org.typelevel.paiges.Doc
import org.bykn.bosatsu.hashing.{Hashed, Algo}
import scala.util.{Failure, Success, Try}

import cats.syntax.all._
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}

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
        val drainAll = (
            fs2.Stream.empty.through(process.stdin).compile.drain,
            fs2.text.utf8.decode(process.stderr).evalMapChunk(IO.consoleForIO.error).compile.drain,
            fs2.text.utf8.decode(process.stdout).evalMapChunk(IO.consoleForIO.print).compile.drain
          ).parTupled

        for {
          _ <- drainAll
          result <- process.exitValue
          _ <- IO.raiseError(new Exception(s"$cmd ${args.mkString(" ")} returned $result")).whenA(result != 0)
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

  private def read[A <: GeneratedMessage](path: Path)(implicit A: GeneratedMessageCompanion[A]): IO[A] =
    FilesIO
      .readAll(path)
      .compile.to(Array)
      .flatMap { bytes =>
        IO(A.parseFrom(bytes))  
      }

  def readHashed[A <: GeneratedMessage, H](
    path: Path
  )(implicit gmc: GeneratedMessageCompanion[A], algo: Algo[H]): IO[Hashed[H, A]] =
    FilesIO
      .readAll(path)
      .compile.to(Array)
      .flatMap { bytes =>
        IO {
          val a = gmc.parseFrom(bytes)
          Hashed(Algo.hashBytes[H](bytes), a)
        }
      }

  def readPackages(paths: List[Path]): IO[List[Package.Typed[Unit]]] =
    paths.parTraverse { path =>
      for {
        ppack <- read[proto.Packages](path)
        packs <- IO.fromTry(ProtoConverter.packagesFromProto(Nil, ppack.packages))
      } yield packs._2
    }
    .map(_.flatten)

  def readInterfaces(paths: List[Path]): IO[List[Package.Interface]] =
    paths.parTraverse { path =>
      for {
        pifaces <- read[proto.Interfaces](path)
        ifaces <- IO.fromTry(ProtoConverter.packagesFromProto(pifaces.interfaces, Nil))
      } yield ifaces._1
    }
    .map(_.flatten)

  def readLibrary(path: Path): IO[Hashed[Algo.Blake3, proto.Library]] =
    readHashed[proto.Library, Algo.Blake3](path)

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
    FilesIO.writeAll(path)(fs2.Stream.chunk(fs2.Chunk.array(a.toByteArray)))
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