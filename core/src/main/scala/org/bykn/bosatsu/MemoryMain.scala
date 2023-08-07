package org.bykn.bosatsu

import cats.MonadError
import cats.data.Kleisli
import com.monovore.decline.Argument
import scala.collection.immutable.SortedMap

import cats.implicits._

class MemoryMain[F[_], K: Ordering](split: K => List[String])(implicit
    val pathArg: Argument[K],
    val innerMonad: MonadError[F, Throwable]
) extends MainModule[Kleisli[F, MemoryMain.State[K], *]] {

  type IO[A] = Kleisli[F, MemoryMain.State[K], A]

  type Path = K

  def readPath(p: Path): IO[String] =
    Kleisli
      .ask[F, MemoryMain.State[K]]
      .flatMap { files =>
        files.get(p) match {
          case Some(MemoryMain.FileContent.Str(res)) => moduleIOMonad.pure(res)
          case other =>
            moduleIOMonad.raiseError(
              new Exception(s"expect String content, found: $other")
            )
        }
      }

  def resolvePath: Option[(Path, PackageName) => IO[Option[Path]]] = None

  def readPackages(paths: List[Path]): IO[List[Package.Typed[Unit]]] =
    Kleisli
      .ask[F, MemoryMain.State[K]]
      .flatMap { files =>
        paths
          .traverse { path =>
            files.get(path) match {
              case Some(MemoryMain.FileContent.Packages(res)) =>
                moduleIOMonad.pure(res)
              case other =>
                moduleIOMonad.raiseError[List[Package.Typed[Unit]]](
                  new Exception(s"expect Packages content, found: $other")
                )
            }
          }
          .map(_.flatten)
      }

  def readInterfaces(paths: List[Path]): IO[List[Package.Interface]] =
    Kleisli
      .ask[F, MemoryMain.State[K]]
      .flatMap { files =>
        paths
          .traverse { path =>
            files.get(path) match {
              case Some(MemoryMain.FileContent.Interfaces(res)) =>
                moduleIOMonad.pure(res)
              case other =>
                moduleIOMonad.raiseError[List[Package.Interface]](
                  new Exception(s"expect Packages content, found: $other")
                )
            }
          }
          .map(_.flatten)
      }

  def unfoldDir: Option[Path => IO[Option[IO[List[Path]]]]] = None

  def hasExtension(str: String): (Path => Boolean) =
    Function.const(false)(_)

  def runWith(
      files: Iterable[(K, String)],
      packages: Iterable[(K, List[Package.Typed[Unit]])] = Nil,
      interfaces: Iterable[(K, List[Package.Interface])] = Nil
  )(cmd: List[String]): F[Output] =
    run(cmd) match {
      case Left(_) =>
        innerMonad.raiseError[Output](
          new Exception(s"got the help message for: $cmd")
        )
      case Right(io) =>
        val state0 =
          files.foldLeft(SortedMap.empty[K, MemoryMain.FileContent]) {
            case (st, (k, str)) =>
              st.updated(k, MemoryMain.FileContent.Str(str))
          }
        val state1 = packages.foldLeft(state0) { case (st, (k, packs)) =>
          st.updated(k, MemoryMain.FileContent.Packages(packs))
        }
        val state2 = interfaces.foldLeft(state1) { case (st, (k, ifs)) =>
          st.updated(k, MemoryMain.FileContent.Interfaces(ifs))
        }
        io.run(state2)
    }

  def pathPackage(roots: List[Path], packFile: Path): Option[PackageName] = {
    val fparts = split(packFile)

    def getP(p: Path): Option[PackageName] = {
      val splitP = split(p)
      if (fparts.startsWith(splitP)) {
        val parts = fparts.drop(splitP.length)
        PackageName.parse(parts.mkString("/"))
      } else None
    }

    @annotation.tailrec
    def loop(roots: List[Path]): Option[PackageName] =
      roots match {
        case Nil => None
        case h :: t =>
          getP(h) match {
            case None => loop(t)
            case some => some
          }
      }

    loop(roots)
  }

  def delay[A](a: => A): IO[A] =
    Kleisli(_ => innerMonad.pure(a))
}

object MemoryMain {
  sealed abstract class FileContent
  object FileContent {
    case class Str(str: String) extends FileContent
    case class Packages(ps: List[Package.Typed[Unit]]) extends FileContent
    case class Interfaces(ifs: List[Package.Interface]) extends FileContent
  }

  type State[K] = SortedMap[K, FileContent]
}
