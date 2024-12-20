package org.bykn.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.MonadError
import cats.data.Validated
import cats.effect.IO
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

  val pathOrdering: Ordering[Path] = Path.instances.toOrdering

  private val FilesIO = Files.forIO

  def readPath(p: Path): IO[String] =
    FilesIO.readUtf8(p).compile.string

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

  /** Modules optionally have the capability to combine paths into a tree
    */
  val resolvePath: Option[(Path, PackageName) => IO[Option[Path]]] = Some {
    (root: Path, pack: PackageName) => {
      val dir = pack.parts.init.foldLeft(root)(_.resolve(_))
      val filePath = dir.resolve(pack.parts.last + ".bosatsu")
      FilesIO.exists(filePath, true)
        .map {
          case true => Some(filePath)
          case false => None
        }
    }
  }

  /** some modules have paths that form directory trees
    *
    * if the given path is a directory, return Some and all the first children.
    */
  def unfoldDir: Option[Path => IO[Option[IO[List[Path]]]]] = Some {
    (path: Path) => {
      FilesIO.isDirectory(path, followLinks = true)
        .map {
          case true => Some {
            // create a list of children
            FilesIO.list(path).compile.toList
          }
          case false => None
        }
    }
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

  def resolve(base: Path, p: List[String]): Path =
    p.foldLeft(base)(_.resolve(_))

  // this is println actually
  def print(str: String): IO[Unit] =
    IO.println(str)

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