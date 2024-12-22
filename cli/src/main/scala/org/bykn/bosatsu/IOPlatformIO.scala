package org.bykn.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.MonadError
import cats.effect.IO
import com.monovore.decline.Argument
import java.nio.file.{Path => JPath}
import java.io.{
  FileInputStream,
  FileOutputStream,
  BufferedInputStream,
  BufferedOutputStream,
  PrintWriter
}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import org.typelevel.paiges.Doc

import cats.syntax.all._

object IOPlatformIO extends PlatformIO[IO, JPath] {
  type F[A] = IO[A]
  type Path = JPath

  def pathOrdering = Ordering.ordered[JPath]

  override def pathArg: Argument[Path] =
    Argument.readPath

  override def moduleIOMonad: MonadError[IO, Throwable] =
    cats.effect.IO.asyncForIO

  def readUtf8(path: Path): IO[String] =
    IO.blocking(new String(java.nio.file.Files.readAllBytes(path), "utf-8"))

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

  def write(a: GeneratedMessage, path: Path): IO[Unit] =
    IO.blocking {
      val f = path.toFile
      val os = new BufferedOutputStream(new FileOutputStream(f))
      try a.writeTo(os)
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
    ).tupled
      .flatMap { case (ifs, packs) =>
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

  def writeInterfaces(
      interfaces: List[Package.Interface],
      path: Path
  ): IO[Unit] =
    IO.fromTry(ProtoConverter.interfacesToProto(interfaces))
      .flatMap(write(_, path))

  def writePackages[A](packages: List[Package.Typed[A]], path: Path): IO[Unit] =
    IO.fromTry(ProtoConverter.packagesToProto(packages))
      .flatMap(write(_, path))

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

  def print(str: String): IO[Unit] =
    IO.println(str)

  def writeStdout(doc: Doc): IO[Unit] =
    IO.blocking {
      doc
        .renderStreamTrim(80)
        .iterator
        .foreach(System.out.print)

      System.out.println("")
    }

}