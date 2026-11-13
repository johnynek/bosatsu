package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}

import cats.{Hash, MonadError, Parallel, Show}
import cats.data.ValidatedNel
import cats.parse.{Parser0 => P0}
import com.monovore.decline.Argument
import org.typelevel.paiges.Doc
import scala.util.Try

import cats.syntax.all._
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import dev.bosatsu.graph.CanPromise
import dev.bosatsu.hashing.{Algo, Hashed, HashValue}

trait PlatformIO[F[_], Path] {
  implicit def moduleIOMonad: MonadError[F, Throwable]
  implicit def parallelF: Parallel[F]
  implicit def canPromiseF: CanPromise[F]

  implicit def pathArg: Argument[Path]
  implicit def pathOrdering: Ordering[Path]

  def withEC[A](fn: Par.EC ?=> F[A]): F[A]

  final def path(str: String): ValidatedNel[String, Path] =
    pathArg.read(str)

  def pathF(str: String): F[Path] =
    path(str) match {
      case Valid(a)   => moduleIOMonad.pure(a)
      case Invalid(e) =>
        moduleIOMonad.raiseError(new Exception(s"invalid path $str: $e"))
    }

  // this must return a parseable String
  def pathToString(path: Path): String
  implicit val showPath: Show[Path] =
    new Show[Path] {
      def show(p: Path) = pathToString(p)
    }

  def readUtf8(p: Path): F[String]
  def readBytes(p: Path): F[Array[Byte]]
  def parseUtf8[A](path: Path, p0: P0[A]): F[A] =
    readUtf8(path)
      .flatMap { str =>
        p0.parseAll(str) match {
          case Right(a) => moduleIOMonad.pure(a)
          case Left(e)  =>
            moduleIOMonad.raiseError(
              new Exception(show"could not parse ${path}.\n\n${e}")
            )
        }
      }

  final inline def getOrError[A](
      oa: Option[A],
      inline msg: => Throwable
  ): F[A] =
    oa match {
      case Some(a) => moduleIOMonad.pure(a)
      case None    => moduleIOMonad.raiseError(msg)
    }

  def readPackages(paths: List[Path]): F[List[Package.Compiled]]
  def readPackageBundle(path: Path): F[proto.Packages] =
    readBytes(path).flatMap(bytes =>
      canPromiseF.compute(proto.Packages.parseFrom(bytes))
    )

  def writePackageBundle(bundle: proto.Packages, path: Path): F[Unit] =
    writeBytes(path, bundle.toByteArray)
  def readInterfaceBundle(path: Path): F[proto.Interfaces] =
    readBytes(path).flatMap(bytes =>
      moduleIOMonad.fromTry(Try(proto.Interfaces.parseFrom(bytes)))
    )

  def readInterfaces(paths: List[Path]): F[List[Package.Interface]] =
    paths.parTraverse(readInterfaceBundle).flatMap { bundles =>
      moduleIOMonad
        .fromTry(
          ProtoConverter.packagesFromProto(bundles.flatMap(_.interfaces), Nil)
        )
        .map(_._1)
    }

  def writeInterfaceBundle(bundle: proto.Interfaces, path: Path): F[Unit] =
    writeBytes(path, bundle.toByteArray)

  def readLibrary(path: Path): F[Hashed[Algo.Blake3, proto.Library]]

  /** Download an object to a given path with a given HashValue
    */
  def fetchHash[A](
      algo: Algo[A],
      hash: HashValue[A],
      path: Path,
      uri: String
  ): F[Either[PlatformIO.FetchHashFailure, Unit]]

  def fsDataType(p: Path): F[Option[PlatformIO.FSDataType]]

  /** Filesystem identity after following links. Equal keys identify the same
    * existing file, including hard links when the filesystem supplies native
    * identities. Otherwise implementations fall back to the resolved path.
    * Equality and hashing apply only within one operation on a static
    * filesystem. Never use a key in an artifact or a compiler cache key.
    */
  type FileKey
  def fileKeyHash: Hash[FileKey]
  def existingFileKey(path: Path): F[Option[FileKey]]

  /** Resolve a prospective output name without creating it. */
  def pendingOutputPath(path: Path): F[Path]

  /** Use the destination filesystem's naming rules when probing output aliases.
    */
  def withTempDirectoryIn[A](parent: Path)(fn: Path => F[A]): F[A]

  def fileExists(p: Path): F[Boolean] =
    fsDataType(p).map {
      case Some(PlatformIO.FSDataType.File) => true
      case _                                => false
    }

  def resolve(p: Path, child: String): Path
  def resolve(p: Path, child: Path): Path
  def relativize(prefix: Path, deeper: Path): Option[Path]
  def parent(p: Path): Option[Path]

  final def hasGitMetadataEntry(dir: Path): F[Boolean] =
    fsDataType(resolve(dir, ".git")).map {
      case Some(_) => true
      case None    => false
    }

  def resolveFile(root: Path, pack: PackageName): F[Option[Path]] = {
    val dir = resolve(root, pack.parts.init)
    val filePath = resolve(dir, pack.parts.last + ".bosatsu")
    fsDataType(filePath).map {
      case Some(PlatformIO.FSDataType.File) => Some(filePath)
      case _                                => None
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
  def readStdinBytes: F[Array[Byte]]
  def writeStdoutBytes(bytes: Array[Byte]): F[Unit]

  def system(command: String, args: List[String]): F[Unit]
  def systemStdout(command: String, args: List[String]): F[String]

  def env(name: String): F[Option[String]]
  def hostOs: F[String]
  def hostArch: F[String]

  def gitShaHead: F[String]

  final def gitTopLevelFrom(start: Path): F[Option[Path]] = {
    def searchStep(current: Path): F[Either[Path, Option[Path]]] =
      fsDataType(current).flatMap {
        case Some(PlatformIO.FSDataType.Dir) =>
          hasGitMetadataEntry(current).map {
            case true  => Right(Some(current))
            case false =>
              parent(current) match {
                case Some(next) if !pathOrdering.equiv(next, current) =>
                  Left(next)
                case _ =>
                  Right(None)
              }
          }
        case _ => moduleIOMonad.pure(Right(None))
      }

    moduleIOMonad.tailRecM(start)(searchStep)
  }

  def gitTopLevel: F[Option[Path]] =
    path(".") match {
      case Valid(a)   => gitTopLevelFrom(a)
      case Invalid(e) =>
        moduleIOMonad.raiseError(
          new Exception(s"could not find current directory: $e")
        )
    }

  final def writeOut(doc: Doc, out: Option[Path]): F[Unit] =
    out match {
      case None    => writeStdout(doc)
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

  def writeBytes(path: Path, bytes: Array[Byte]): F[Unit]

  def writeLibrary(lib: proto.Library, path: Path): F[Unit]
  def writePackages(packages: List[Package.Compiled], path: Path): F[Unit]

  /** Create a temporary directory with the given prefix, run the function, then
    * delete the directory (best-effort) before returning the original result.
    */
  def withTempPrefix[A](name: String)(fn: Path => F[A]): F[A]
}

object PlatformIO {
  enum FetchHashFailure derives CanEqual {
    case HttpStatus(uri: String, status: String)
    case HashMismatch(uri: String, expected: String, found: String)
    case Network(uri: String, message: String)
  }

  object FetchHashFailure {
    def toDoc(failure: FetchHashFailure): Doc =
      failure match {
        case FetchHashFailure.HttpStatus(uri, status) =>
          Doc.intercalate(
            Doc.line,
            List(
              Doc.text(show"uri=$uri"),
              Doc.text("reason=http status"),
              Doc.text(show"status=$status")
            )
          )
        case FetchHashFailure.HashMismatch(uri, expected, found) =>
          Doc.intercalate(
            Doc.line,
            List(
              Doc.text(show"uri=$uri"),
              Doc.text("reason=hash mismatch"),
              Doc.text(show"expected=$expected"),
              Doc.text(show"found=$found")
            )
          )
        case FetchHashFailure.Network(uri, message) =>
          Doc.intercalate(
            Doc.line,
            List(
              Doc.text(show"uri=$uri"),
              Doc.text("reason=network error"),
              Doc.text(show"message=$message")
            )
          )
      }
  }

  sealed abstract class FSDataType derives CanEqual
  object FSDataType {
    case object Dir extends FSDataType
    case object File extends FSDataType
  }
}
