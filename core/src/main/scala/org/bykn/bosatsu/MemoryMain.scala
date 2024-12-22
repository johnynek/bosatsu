package org.bykn.bosatsu

import cats.MonadError
import cats.data.{Chain, Kleisli, Validated}
import com.monovore.decline.Argument
import scala.collection.immutable.SortedMap
import org.bykn.bosatsu.tool.Output
import org.typelevel.paiges.Doc

import cats.syntax.all._
import cats.data.ValidatedNel

class MemoryMain[G[_]](
  platform: PlatformIO[Kleisli[G, MemoryMain.State, *], Chain[String]]) extends
  MainModule[Kleisli[G, MemoryMain.State, *], Chain[String]](platform) {

  def withEC[A](fn: Par.EC => F[A]): F[A] =
    Kleisli { state =>
      // this is safe to use the side-effects
      // of Par here because they are local to this method
      // and can't escape or be deferred
      val es = Par.newService()
      try {
        val ec = Par.ecFromService(es)
        val fa = fn(ec)
        fa.run(state)
      }
      finally {
        Par.shutdownService(es)
      }
    }

  def runWith(
      files: Iterable[(Chain[String], String)],
      packages: Iterable[(Chain[String], List[Package.Typed[Unit]])] = Nil,
      interfaces: Iterable[(Chain[String], List[Package.Interface])] = Nil
  )(cmd: List[String])(implicit G: MonadError[G, Throwable]): G[Output[Chain[String]]] =
    run(cmd) match {
      case Right(io) =>
        for {
          state <- MemoryMain.State.from(files, packages, interfaces)
          res <- io.run(state)
        } yield res
      case Left(msg) =>
        G.raiseError[Output[Chain[String]]](
          new Exception(s"got the help message for: $cmd: $msg")
        )
    }
}

object MemoryMain {
  sealed abstract class FileContent
  object FileContent {
    case class Str(str: String) extends FileContent
    case class Packages(ps: List[Package.Typed[Unit]]) extends FileContent
    case class Interfaces(ifs: List[Package.Interface]) extends FileContent
  }

  case class State(children: SortedMap[String, Either[State, FileContent]]) {
    def get(path: Chain[String]): Option[Either[State, FileContent]] =
      path.uncons match {
        case None => Some(Left(this))
        case Some((head, tail)) =>
          children.get(head).flatMap {
            case Left(s) => s.get(tail)
            case Right(fc) =>
              if (tail.isEmpty) Some(Right(fc))
              else None
          }
      }

      def withDir(path: Chain[String]): Option[State] =
        path.uncons match {
          case None => Some(this)
          case Some((h, t)) =>
            children.get(h) match {
              case Some(Right(_)) =>
                // this is a file
                None
              case Some(Left(s)) =>
                s.withDir(t).map { s1 =>
                  copy(children = children.updated(h, Left(s1)))  
                }
              case None =>
                State.empty.withDir(t).map { s1 =>
                  copy(children = children.updated(h, Left(s1)))  
                }
            }
        }

      def withFile(path: Chain[String], fc: FileContent): Option[State] =
        path.uncons.flatMap { case (h, t) =>
          if (t.isEmpty) {
            children.get(h) match {
              case Some(Right(_)) | None =>
                // a file, or no file
                Some(copy(children = children.updated(h, Right(fc))))
              case Some(Left(_)) =>
                // already a directory here
                None
            }
          }
          else children.get(h) match {
            // we need to find a directory
            case Some(Right(_)) =>
              // this is a file
              None
            case Some(Left(s)) =>
              s.withFile(t, fc).map { s1 =>
                copy(children = children.updated(h, Left(s1)))  
              }
            case None =>
              State.empty.withFile(t, fc).map { s1 =>
                copy(children = children.updated(h, Left(s1)))  
              }
          }
        }
  }

  object State {
    val empty: State = State(SortedMap.empty)
    def from[G[_]](
        files: Iterable[(Chain[String], String)],
        packages: Iterable[(Chain[String], List[Package.Typed[Unit]])] = Nil,
        interfaces: Iterable[(Chain[String], List[Package.Interface])] = Nil
    )(implicit G: MonadError[G, Throwable]): G[State] =
      for {
        state0 <-
          files.toList.foldM(MemoryMain.State.empty) {
            case (st, (k, str)) =>
              st.withFile(k, MemoryMain.FileContent.Str(str)) match {
                case Some(s1) => G.pure(s1)
                case None =>
                  G.raiseError[MemoryMain.State](
                    new Exception(s"couldn't add file: $k to state = $st")
                  )
              }
          }
        state1 <- packages.toList.foldM(state0) { case (st, (k, packs)) =>
          st.withFile(k, MemoryMain.FileContent.Packages(packs)) match {
            case Some(s1) => G.pure(s1)
            case None =>
              G.raiseError[MemoryMain.State](
                new Exception(s"couldn't add file: $k to state = $st")
              )
          }
        }
        state2 <- interfaces.toList.foldM(state1) { case (st, (k, ifs)) =>
          st.withFile(k, MemoryMain.FileContent.Interfaces(ifs)) match {
            case Some(s1) => G.pure(s1)
            case None =>
              G.raiseError[MemoryMain.State](
                new Exception(s"couldn't add file: $k to state = $st")
              )
          }
        }
      } yield state2
  }

  def apply[G[_]](implicit innerMonad: MonadError[G, Throwable]): MemoryMain[G] =
    new MemoryMain(memoryPlatformIO[G])

  def memoryPlatformIO[G[_]](implicit
    innerMonad: MonadError[G, Throwable]): PlatformIO[Kleisli[G, State, *], Chain[String]] = {

      val catsDefaultME = implicitly[MonadError[Kleisli[G, State, *], Throwable]]

      new PlatformIO[Kleisli[G, State, *], Chain[String]] {
        type F[A] = Kleisli[G, State, A]
        type Path = Chain[String]
        def moduleIOMonad: MonadError[F, Throwable] = catsDefaultME
        def pathOrdering = Chain.catsDataOrderForChain[String].toOrdering
        val pathArg: Argument[Path] =
          new Argument[Path] {
            def defaultMetavar: String = "path"
            def read(string: String): ValidatedNel[String,Path] =
              if (string == "") Validated.valid(Chain.empty)
              else Validated.valid(Chain.fromSeq(string.split("/", -1).toIndexedSeq))
          }

        def readUtf8(p: Path): F[String] =
          Kleisli
            .ask[G, State]
            .flatMap { files =>
              files.get(p) match {
                case Some(Right(MemoryMain.FileContent.Str(res))) => moduleIOMonad.pure(res)
                case other =>
                  moduleIOMonad.raiseError(
                    new Exception(s"expect String content, found: $other")
                  )
              }
            }

        def fsDataType(p: Path): Kleisli[G,State,Option[PlatformIO.FSDataType]] =
          Kleisli
            .ask[G, State]
            .map { files =>
              files.get(p) match {
                case Some(Right(_)) => Some(PlatformIO.FSDataType.File)
                case Some(Left(_)) => Some(PlatformIO.FSDataType.Dir)
                case None => None
              }
            }

        def resolve(p: Chain[String], child: String): Chain[String] =
          p :+ child

        def readPackages(paths: List[Path]): F[List[Package.Typed[Unit]]] =
          Kleisli
            .ask[G, MemoryMain.State]
            .flatMap { files =>
              paths
                .traverse { path =>
                  files.get(path) match {
                    case Some(Right(MemoryMain.FileContent.Packages(res))) =>
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
            .ask[G, MemoryMain.State]
            .flatMap { files =>
              paths
                .traverse { path =>
                  files.get(path) match {
                    case Some(Right(MemoryMain.FileContent.Interfaces(res))) =>
                      moduleIOMonad.pure(res)
                    case other =>
                      moduleIOMonad.raiseError[List[Package.Interface]](
                        new Exception(s"expect Packages content, found: $other")
                      )
                  }
                }
                .map(_.flatten)
            }

        def unfoldDir(path: Path): F[Option[F[List[Path]]]] =
          Kleisli
            .ask[G, MemoryMain.State]
            .map { files =>
              files.get(path).flatMap {
                case Left(state) =>
                  Some(moduleIOMonad.pure(state.children.iterator.collect {
                    case (k, Right(_)) =>
                      path :+ k
                  }
                  .toList))
                case Right(_) => None
              }
            }

        def hasExtension(str: String): (Path => Boolean) =
          Function.const(false)(_)

        def pathPackage(roots: List[Path], packFile: Path): Option[PackageName] = {
          val parts = packFile.toList
          def getP(p: Path): Option[PackageName] = {
            if (parts.startsWith(p.toList)) {
              val parts1 = parts.drop(p.length.toInt)
              PackageName.parse(parts1.mkString("/"))
            } else None
          }

          roots.collectFirstSome(getP)
        }

        def writeDoc(p: Path, d: Doc): F[Unit] =
          catsDefaultME.raiseError(new Exception(s"writeDoc($p, $d) is unimplemented on a read only platform"))

        def writeInterfaces(ifaces: List[Package.Interface], path: Path): F[Unit] =
          catsDefaultME.raiseError(new Exception(s"writeInterfaces($ifaces, $path) is unimplemented on a read only platform"))

        def writePackages[A](packs: List[Package.Typed[A]], path: Path): F[Unit] =
          catsDefaultME.raiseError(new Exception(s"writePackages($packs, $path) is unimplemented on a read only platform"))

        def writeStdout(doc: Doc): F[Unit] =
          catsDefaultME.raiseError(new Exception(s"writeStdout($doc) is unimplemented on a read only platform"))

        def println(str: String): F[Unit] =
          catsDefaultME.raiseError(new Exception(s"println($str) is unimplemented on a read only platform"))

        def errorln(str: String): F[Unit] =
          catsDefaultME.raiseError(new Exception(s"errorln($str) is unimplemented on a read only platform"))

        override def resolve(base: Path, parts: List[String]): Path =
          base ++ Chain.fromSeq(parts)
      }
    }
}
