package org.bykn.bosatsu

import _root_.bosatsu.{TypedAst => proto}

import cats.{MonadError, Show}
import cats.data.ValidatedNel
import cats.parse.{Parser0 => P0}
import com.monovore.decline.Argument
import org.typelevel.paiges.Doc

import cats.syntax.all._
import cats.data.Validated.Invalid
import cats.data.Validated.Valid

trait PlatformIO[F[_], Path] {
  implicit def moduleIOMonad: MonadError[F, Throwable]
  implicit def pathArg: Argument[Path]
  implicit def pathOrdering: Ordering[Path]

  def withEC[A](fn: Par.EC => F[A]): F[A]

  final def path(str: String): ValidatedNel[String, Path] =
    pathArg.read(str)

  def pathF(str: String): F[Path] =
    path(str) match {
      case Valid(a) => moduleIOMonad.pure(a)
      case Invalid(e) => moduleIOMonad.raiseError(new Exception(s"invalid path $str: $e"))
    }

  // this must return a parseable String
  def pathToString(path: Path): String
  implicit val showPath: Show[Path] =
    new Show[Path] {
      def show(p: Path) = pathToString(p)
    }

  def readUtf8(p: Path): F[String]
  def parseUtf8[A](path: Path, p0: P0[A]): F[A] =
    readUtf8(path)
      .flatMap { str =>
        p0.parseAll(str) match {
          case Right(a) => moduleIOMonad.pure(a)
          case Left(e) =>
            moduleIOMonad.raiseError(
              new Exception(show"could not parse ${path}.\n\n${e}")
            )
        }
      }

  def getOrError[A](oa: Option[A], msg: => String): F[A] =
    oa match {
      case Some(a) => moduleIOMonad.pure(a)
      case None => moduleIOMonad.raiseError(new Exception(msg))
    }

  def readPackages(paths: List[Path]): F[List[Package.Typed[Unit]]]
  def readInterfaces(paths: List[Path]): F[List[Package.Interface]]
  def readLibrary(path: Path): F[proto.Library]

  /** given an ordered list of prefered roots, if a packFile starts with one of
    * these roots, return a PackageName based on the rest
    */
  def pathPackage(roots: List[Path], packFile: Path): Option[PackageName]

  def fsDataType(p: Path): F[Option[PlatformIO.FSDataType]]

  def resolve(p: Path, child: String): Path
  def resolve(p: Path, child: Path): Path
  def relativize(prefix: Path, deeper: Path): Option[Path]

  def resolveFile(root: Path, pack: PackageName): F[Option[Path]] = {
    val dir = resolve(root, pack.parts.init)
    val filePath = resolve(dir, pack.parts.last + ".bosatsu")
    fsDataType(filePath).map {
      case Some(PlatformIO.FSDataType.File) => Some(filePath)
      case _ => None
    }
  }

  /** some modules have paths that form directory trees
    *
    * if the given path is a directory, return Some and all the first children.
    */
  def unfoldDir(path: Path): F[Option[F[List[Path]]]]

  def hasExtension(str: String): Path => Boolean

  def writeDoc(p: Path, d: Doc): F[Unit]
  def writeStdout(doc: Doc): F[Unit]
  def writeError(doc: Doc): F[Unit]

  def system(command: String, args: List[String]): F[Unit]

  def gitTopLevel: F[Option[Path]] = {
    def searchStep(current: Path): F[Either[Path, Option[Path]]] =
      fsDataType(current).flatMap {
        case Some(PlatformIO.FSDataType.Dir) => 
          fsDataType(resolve(current, ".git"))
            .map {
              case Some(PlatformIO.FSDataType.Dir) => Right(Some(current))
              case _ => Left(resolve(current, ".."))
            }
        case _ => moduleIOMonad.pure(Right(None))
      }

    path(".") match {
      case Valid(a) => moduleIOMonad.tailRecM(a)(searchStep)
      case Invalid(e) => moduleIOMonad.raiseError(new Exception(s"could not find current directory: $e"))
    }
  }

  final def writeOut(doc: Doc, out: Option[Path]): F[Unit] =
    out match {
      case None => writeStdout(doc)
      case Some(p) => writeDoc(p, doc)
    }


  def resolve(base: Path, p: List[String]): Path =
    p.foldLeft(base)(resolve(_, _))

  def println(str: String): F[Unit]
  def errorln(str: String): F[Unit]

  def writeInterfaces(
      interfaces: List[Package.Interface],
      path: Path
  ): F[Unit]

  def writeLibrary(lib: proto.Library, path: Path): F[Unit]
  def writePackages[A](packages: List[Package.Typed[A]], path: Path): F[Unit]
}

object PlatformIO {
  def pathPackage[Path](roots: List[Path], packFile: Path)(relativeParts: (Path, Path) => Option[Iterable[String]]): Option[PackageName] = {
    def getP(p: Path): Option[PackageName] =
      relativeParts(p, packFile).flatMap { parts =>
        val subPath = parts
          .iterator
          .map { part =>
            part.toLowerCase.capitalize
          }
          .mkString("/")

        val dropExtension = """(.*)\.[^.]*$""".r
        val toParse = subPath match {
          case dropExtension(prefix) => prefix
          case _                     => subPath
        }
        PackageName.parse(toParse)
      }

    if (packFile.toString.isEmpty) None
    else roots.collectFirstSome(getP)
  }

  sealed abstract class FSDataType
  object FSDataType {
    case object Dir extends FSDataType
    case object File extends FSDataType
  }
}