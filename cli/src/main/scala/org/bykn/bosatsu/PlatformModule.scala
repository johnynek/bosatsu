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
  BufferedOutputStream
}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}

import cats.syntax.all._

object IOPlatformIO extends PlatformIO[IO, JPath] {
  type F[A] = IO[A]
  type Path = JPath

  override def pathArg: Argument[Path] =
    Argument.readPath

  override def moduleIOMonad: MonadError[IO, Throwable] =
    cats.effect.IO.asyncForIO

  def readPath(path: Path): IO[String] =
    IO.blocking(new String(java.nio.file.Files.readAllBytes(path), "utf-8"))

  val resolvePath: Some[(Path, PackageName) => IO[Option[Path]]] =
    Some(
      { (root: Path, pack: PackageName) =>
        val dir = pack.parts.init.foldLeft(root)(_.resolve(_))
        val filePath = dir.resolve(pack.parts.last + ".bosatsu")
        IO.blocking {
          // this is a side-effect since file is mutable
          // and talks to the file system
          val file = filePath.toFile
          if (file.exists()) Some(filePath)
          else None
        }
      }
    )

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
    IO.fromTry {
      packages
        .traverse(ProtoConverter.packageToProto(_))
        .map(proto.Packages(_))
    }.flatMap(write(_, path))

  def unfoldDir: Option[Path => IO[Option[IO[List[Path]]]]] = Some {
    (path: Path) =>
      IO.blocking {
        val f = path.toFile

        if (f.isDirectory()) {
          Some(IO.blocking {
            f.listFiles.iterator.map(_.toPath).toList
          })
        } else None
      }
  }

  def hasExtension(str: String): Path => Boolean = { (path: Path) =>
    path.toString.endsWith(str)
  }

  def pathPackage(roots: List[Path], packFile: Path): Option[PackageName] = {
    import scala.jdk.CollectionConverters._

    def getP(p: Path): Option[PackageName] = {
      val subPath = p
        .relativize(packFile)
        .asScala
        .map { part =>
          part.toString.toLowerCase.capitalize
        }
        .mkString("/")

      val dropExtension = """(.*)\.[^.]*$""".r
      val toParse = subPath match {
        case dropExtension(prefix) => prefix
        case _                     => subPath
      }
      PackageName.parse(toParse)
    }

    @annotation.tailrec
    def loop(roots: List[Path]): Option[PackageName] =
      roots match {
        case Nil                              => None
        case h :: _ if packFile.startsWith(h) => getP(h)
        case _ :: t                           => loop(t)
      }

    if (packFile.toString.isEmpty) None
    else loop(roots)
  }
}