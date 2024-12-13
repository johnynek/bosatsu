package org.bykn.bosatsu

import cats.MonadError
import cats.data.Kleisli
import com.monovore.decline.Argument
import scala.collection.immutable.SortedMap

import cats.syntax.all._

class MemoryMain[G[_], K: Ordering](
  platform: PlatformIO[Kleisli[G, MemoryMain.State[K], *], K]) extends
  MainModule[Kleisli[G, MemoryMain.State[K], *], K](platform) {

  import platformIO._

  def withEC[A](fn: Par.EC => F[A]): F[A] =
    moduleIOMonad.flatMap(moduleIOMonad.unit) { _ =>
      val es = Par.newService()
      moduleIOMonad.map(fn(Par.ecFromService(es))) { a =>
        Par.shutdownService(es)
        a
      }
      .recoverWith { case e =>
        Par.shutdownService(es)
        moduleIOMonad.raiseError[A](e)
      }
    }

  def runWith(
      files: Iterable[(K, String)],
      packages: Iterable[(K, List[Package.Typed[Unit]])] = Nil,
      interfaces: Iterable[(K, List[Package.Interface])] = Nil
  )(cmd: List[String]): G[Output] =
    run(cmd) match {
      case Left(msg) =>
        moduleIOMonad.raiseError[Output](
          new Exception(s"got the help message for: $cmd: $msg")
        )
        .run(SortedMap.empty[K, MemoryMain.FileContent])
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

}

object MemoryMain {
  sealed abstract class FileContent
  object FileContent {
    case class Str(str: String) extends FileContent
    case class Packages(ps: List[Package.Typed[Unit]]) extends FileContent
    case class Interfaces(ifs: List[Package.Interface]) extends FileContent
  }

  type State[K] = SortedMap[K, FileContent]

  def apply[G[_], K: Argument: Ordering](split: K => List[String])(implicit innerMonad: MonadError[G, Throwable]): MemoryMain[G, K] =
    new MemoryMain(memoryPlatformIO[G, K](split))

  def memoryPlatformIO[G[_], K](split: K => List[String])(implicit
    pathArg0: Argument[K],
    innerMonad: MonadError[G, Throwable]): PlatformIO[Kleisli[G, State[K], *], K] = {

      val catsDefaultME = implicitly[MonadError[Kleisli[G, State[K], *], Throwable]]

      new PlatformIO[Kleisli[G, State[K], *], K] {
        type F[A] = Kleisli[G, State[K], A]
        type Path = K
        def moduleIOMonad: MonadError[F, Throwable] = catsDefaultME
        def pathArg: Argument[K] = pathArg0

        def readPath(p: Path): F[String] =
          Kleisli
            .ask[G, State[K]]
            .flatMap { files =>
              files.get(p) match {
                case Some(MemoryMain.FileContent.Str(res)) => moduleIOMonad.pure(res)
                case other =>
                  moduleIOMonad.raiseError(
                    new Exception(s"expect String content, found: $other")
                  )
              }
            }

        def resolvePath: Option[(Path, PackageName) => F[Option[Path]]] = None

        def readPackages(paths: List[Path]): F[List[Package.Typed[Unit]]] =
          Kleisli
            .ask[G, MemoryMain.State[K]]
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

        def readInterfaces(paths: List[Path]): F[List[Package.Interface]] =
          Kleisli
            .ask[G, MemoryMain.State[K]]
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

        def unfoldDir: Option[Path => F[Option[F[List[Path]]]]] = None

        def hasExtension(str: String): (Path => Boolean) =
          Function.const(false)(_)

        def pathPackage(roots: List[K], packFile: K): Option[PackageName] = {
          val fparts = split(packFile)

          def getP(p: Path): Option[PackageName] = {
            val splitP = split(p)
            if (fparts.startsWith(splitP)) {
              val parts = fparts.drop(splitP.length)
              PackageName.parse(parts.mkString("/"))
            } else None
          }

          roots.collectFirstSome(getP)
        }
      }
    }
}
