package org.bykn.bosatsu

import cats.effect.IO
import com.monovore.decline.Argument
import scala.collection.immutable.SortedMap
import org.bykn.bosatsu.tool.Output

import cats.syntax.all._

object MemoryMain {
  sealed abstract class FileContent
  object FileContent {
    case class Str(str: String) extends FileContent
    case class Packages(ps: List[Package.Typed[Unit]]) extends FileContent
    case class Interfaces(ifs: List[Package.Interface]) extends FileContent
  }

  type State[K] = SortedMap[K, FileContent]

  def stateFrom[K: Ordering](
    files: Iterable[(K, String)] = Nil,
    packages: Iterable[(K, List[Package.Typed[Unit]])] = Nil,
    interfaces: Iterable[(K, List[Package.Interface])] = Nil
  ): State[K] = {
    val state0 =
      files.foldLeft(SortedMap.empty[K, MemoryMain.FileContent]) {
        case (st, (k, str)) =>
          st.updated(k, MemoryMain.FileContent.Str(str))
      }
    val state1 = packages.foldLeft(state0) { case (st, (k, packs)) =>
      st.updated(k, MemoryMain.FileContent.Packages(packs))
    }
    
    interfaces.foldLeft(state1) { case (st, (k, ifs)) =>
      st.updated(k, MemoryMain.FileContent.Interfaces(ifs))
    }
  }

  def apply[K: Argument](state: State[K])(split: K => List[String]): MainModule[K] =
    new MainModule(memoryPlatformIO[K](state)(split))

  def runWith[K: Argument: Ordering](
      files: Iterable[(K, String)],
      packages: Iterable[(K, List[Package.Typed[Unit]])] = Nil,
      interfaces: Iterable[(K, List[Package.Interface])] = Nil
  )(split: K => List[String])(cmd: List[String]): IO[Output[K]] = {
    val main = apply(stateFrom(files, packages, interfaces))(split)
    main.run(cmd) match {
      case Right(io) => io
      case Left(msg) =>
        IO.raiseError[Output[K]](
          new Exception(s"got the help message for: $cmd: $msg")
        )
    }
  }

  def memoryPlatformIO[K](state: State[K])(split: K => List[String])(implicit
    pathArg0: Argument[K]): PlatformIO[K] = {

      new PlatformIO[K] {
        type Path = K
        def pathArg: Argument[K] = pathArg0

        def readPath(p: Path): IO[String] =
          state.get(p) match {
            case Some(MemoryMain.FileContent.Str(res)) => IO.pure(res)
            case other =>
              IO.raiseError(
                new Exception(s"expect String content, found: $other")
              )
          }

        def resolvePath: Option[(Path, PackageName) => IO[Option[Path]]] = None

        def readPackages(paths: List[Path]): IO[List[Package.Typed[Unit]]] =
          paths
            .traverse { path =>
              state.get(path) match {
                case Some(MemoryMain.FileContent.Packages(res)) =>
                  IO.pure(res)
                case other =>
                  IO.raiseError[List[Package.Typed[Unit]]](
                    new Exception(s"expect Packages content, found: $other")
                  )
              }
            }
            .map(_.flatten)

        def readInterfaces(paths: List[Path]): IO[List[Package.Interface]] =
          paths
            .traverse { path =>
              state.get(path) match {
                case Some(MemoryMain.FileContent.Interfaces(res)) =>
                  IO.pure(res)
                case other =>
                  IO.raiseError[List[Package.Interface]](
                    new Exception(s"expect Packages content, found: $other")
                  )
              }
            }
            .map(_.flatten)

        def unfoldDir: Option[Path => IO[Option[IO[List[Path]]]]] = None

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
