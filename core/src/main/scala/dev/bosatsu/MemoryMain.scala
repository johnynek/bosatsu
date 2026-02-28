package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.MonadError
import cats.data.{Chain, StateT, Validated}
import com.monovore.decline.Argument
import scala.collection.immutable.SortedMap
import dev.bosatsu.tool.Output
import dev.bosatsu.hashing.{Algo, Hashed, HashValue}
import org.typelevel.paiges.Doc

import cats.syntax.all._
import cats.data.ValidatedNel

class MemoryMain[G[_]](
    platform: PlatformIO[MemoryMain.StateF[G], Chain[String]]
) extends MainModule[MemoryMain.StateF[G], Chain[String]](platform) {

  def runWith(
      files: Iterable[(Chain[String], String)],
      packages: Iterable[(Chain[String], List[Package.Typed[Unit]])] = Nil,
      interfaces: Iterable[(Chain[String], List[Package.Interface])] = Nil
  )(
      cmd: List[String]
  )(implicit G: MonadError[G, Throwable]): G[Output[Chain[String]]] =
    run(cmd) match {
      case Right(io) =>
        for {
          state <- MemoryMain.State.from(files, packages, interfaces)
          res <- io.run(state)
        } yield res._2
      case Left(msg) =>
        G.raiseError[Output[Chain[String]]](
          new Exception(s"got the help message for: $cmd: $msg")
        )
    }
}

object MemoryMain {
  type StateF[G[_]] = [A] =>> StateT[G, State, A]

  sealed abstract class FileContent
  object FileContent {
    case class Str(str: String) extends FileContent
    case class Packages(ps: List[Package.Typed[Unit]]) extends FileContent
    case class Interfaces(ifs: List[Package.Interface]) extends FileContent
    case class Lib(lib: Hashed[Algo.Blake3, proto.Library]) extends FileContent
  }

  case class State(
      children: SortedMap[String, Either[State, FileContent]],
      stdOut: Doc,
      stdErr: Doc
  ) {
    def get(path: Chain[String]): Option[Either[State, FileContent]] =
      path.uncons match {
        case None               => Some(Left(this))
        case Some((head, tail)) =>
          children.get(head).flatMap {
            case Left(s)   => s.get(tail)
            case Right(fc) =>
              if (tail.isEmpty) Some(Right(fc))
              else None
          }
      }

    def withDir(path: Chain[String]): Option[State] =
      path.uncons match {
        case None         => Some(this)
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
        } else
          children.get(h) match {
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

    def remove(path: Chain[String]): State =
      path.uncons match {
        case None         => this
        case Some((h, t)) =>
          children.get(h) match {
            case None           => this
            case Some(Right(_)) =>
              if (t.isEmpty) copy(children = children - h) else this
            case Some(Left(s)) =>
              if (t.isEmpty) copy(children = children - h)
              else {
                val s1 = s.remove(t)
                val nextChildren =
                  if (s1.children.isEmpty) children - h
                  else children.updated(h, Left(s1))
                copy(children = nextChildren)
              }
          }
      }
  }

  object State {
    val empty: State = State(SortedMap.empty, Doc.empty, Doc.empty)

    def from[G[_]](
        files: Iterable[(Chain[String], String)],
        packages: Iterable[(Chain[String], List[Package.Typed[Unit]])] = Nil,
        interfaces: Iterable[(Chain[String], List[Package.Interface])] = Nil
    )(implicit G: MonadError[G, Throwable]): G[State] =
      for {
        state0 <-
          files.toList.foldM(MemoryMain.State.empty) { case (st, (k, str)) =>
            st.withFile(k, MemoryMain.FileContent.Str(str)) match {
              case Some(s1) => G.pure(s1)
              case None     =>
                G.raiseError[MemoryMain.State](
                  new Exception(s"couldn't add file: $k to state = $st")
                )
            }
          }
        state1 <- packages.toList.foldM(state0) { case (st, (k, packs)) =>
          st.withFile(k, MemoryMain.FileContent.Packages(packs)) match {
            case Some(s1) => G.pure(s1)
            case None     =>
              G.raiseError[MemoryMain.State](
                new Exception(s"couldn't add file: $k to state = $st")
              )
          }
        }
        state2 <- interfaces.toList.foldM(state1) { case (st, (k, ifs)) =>
          st.withFile(k, MemoryMain.FileContent.Interfaces(ifs)) match {
            case Some(s1) => G.pure(s1)
            case None     =>
              G.raiseError[MemoryMain.State](
                new Exception(s"couldn't add file: $k to state = $st")
              )
          }
        }
      } yield state2
  }

  def apply[G[_]](implicit
      innerMonad: MonadError[G, Throwable]
  ): MemoryMain[G] =
    new MemoryMain(memoryPlatformIO[G])

  def memoryPlatformIO[G[_]](implicit
      innerMonad: MonadError[G, Throwable]
  ): PlatformIO[StateF[G], Chain[String]] = {

    val catsDefaultME = implicitly[MonadError[StateF[G], Throwable]]

    new PlatformIO[StateF[G], Chain[String]] {
      type F[A] = StateT[G, State, A]
      type Path = Chain[String]
      def moduleIOMonad: MonadError[F, Throwable] = catsDefaultME
      val parallelF: cats.Parallel[F] = cats.Parallel.identity[F]
      def pathOrdering = Chain.catsDataOrderForChain[String].toOrdering
      val pathArg: Argument[Path] =
        new Argument[Path] {
          def defaultMetavar: String = "path"
          def read(string: String): ValidatedNel[String, Path] =
            if (string == "") Validated.valid(Chain.empty)
            else
              Validated.valid(Chain.fromSeq(string.split("/", -1).toIndexedSeq))
        }
      def pathToString(path: Chain[String]): String = path.mkString_("/")
      def system(command: String, args: List[String]) =
        moduleIOMonad.raiseError(
          new Exception(
            s"system not supported in memory mode: system($command, $args)"
          )
        )

      def gitShaHead = moduleIOMonad.raiseError(new Exception("no git sha"))

      def withTempPrefix[A](name: String)(fn: Path => F[A]): F[A] = {
        def candidateName(idx: Int): String =
          if (idx == 0) name else s"${name}_$idx"

        def pickCandidate(state: State, base: Path, idx: Int): Path = {
          val candidate = base :+ candidateName(idx)
          state.get(candidate) match {
            case None    => candidate
            case Some(_) => pickCandidate(state, base, idx + 1)
          }
        }

        def allocate(base: Path): F[Path] =
          StateT { state =>
            state.withDir(base) match {
              case None =>
                innerMonad.raiseError(
                  new Exception(
                    s"could not create temp base directory: ${pathToString(base)}"
                  )
                )
              case Some(withBase) =>
                val candidate = pickCandidate(withBase, base, 0)
                withBase.withDir(candidate) match {
                  case Some(s2) => innerMonad.pure((s2, candidate))
                  case None     =>
                    innerMonad.raiseError(
                      new Exception(
                        s"could not create temp directory: ${pathToString(candidate)}"
                      )
                    )
                }
            }
          }

        def cleanup(path: Path): F[Unit] =
          StateT.modify { state =>
            state.remove(path)
          }

        val baseDirF: F[Path] =
          gitTopLevel.map {
            case Some(root) => resolve(root, ".bosatsuc" :: "tmp" :: Nil)
            case None       => Chain(".bosatsuc", "tmp")
          }

        baseDirF.flatMap(allocate).flatMap { tempDir =>
          fn(tempDir).attempt.flatMap {
            case Right(a) => cleanup(tempDir).as(a)
            case Left(e)  => cleanup(tempDir) *> moduleIOMonad.raiseError(e)
          }
        }
      }

      def withEC[A](fn: Par.EC ?=> F[A]): F[A] =
        StateT { state =>
          // this is safe to use the side-effects
          // of Par here because they are local to this method
          // and can't escape or be deferred as long as F is not
          // lazy. If F is lazy, such as Eval, this will not work
          // because we need ec to be active while F is evaluated
          val es = Par.newService()
          try {
            val ec = Par.ecFromService(es)
            val fa = fn(using ec)
            fa.run(state)
          } finally {
            Par.shutdownService(es)
          }
        }

      def readUtf8(p: Path): F[String] =
        StateT
          .get[G, State]
          .flatMap { files =>
            files.get(p) match {
              case Some(Right(MemoryMain.FileContent.Str(res))) =>
                moduleIOMonad.pure(res)
              case other =>
                moduleIOMonad.raiseError(
                  new Exception(s"expect String content, found: $other")
                )
            }
          }

      def fsDataType(p: Path): StateT[G, State, Option[PlatformIO.FSDataType]] =
        StateT
          .get[G, State]
          .map { files =>
            files.get(p) match {
              case Some(Right(_)) => Some(PlatformIO.FSDataType.File)
              case Some(Left(_))  => Some(PlatformIO.FSDataType.Dir)
              case None           => None
            }
          }

      def resolve(p: Chain[String], child: String): Chain[String] =
        p :+ child

      def resolve(p: Chain[String], child: Chain[String]): Chain[String] =
        p ++ child
      def relativize(prefix: Path, deeper: Path): Option[Path] = {
        val agreeLen = prefix.iterator
          .zip(deeper.iterator)
          .takeWhile { case (a, b) => a == b }
          .length

        if (agreeLen == prefix.length)
          Some(Chain.fromIterableOnce(deeper.iterator.drop(agreeLen)))
        else None
      }

      def readPackages(paths: List[Path]): F[List[Package.Typed[Unit]]] =
        StateT
          .get[G, MemoryMain.State]
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
        StateT
          .get[G, MemoryMain.State]
          .flatMap { files =>
            paths
              .traverse { path =>
                files.get(path) match {
                  case Some(Right(MemoryMain.FileContent.Interfaces(res))) =>
                    moduleIOMonad.pure(res)
                  case other =>
                    moduleIOMonad.raiseError[List[Package.Interface]](
                      new Exception(s"expect Interfaces content, found: $other")
                    )
                }
              }
              .map(_.flatten)
          }

      def readLibrary(path: Path): F[Hashed[Algo.Blake3, proto.Library]] =
        StateT
          .get[G, MemoryMain.State]
          .flatMap { files =>
            files.get(path) match {
              case Some(Right(MemoryMain.FileContent.Lib(lib))) =>
                moduleIOMonad.pure(lib)
              case other =>
                moduleIOMonad.raiseError[Hashed[Algo.Blake3, proto.Library]](
                  new Exception(s"expect Library content, found: $other")
                )
            }
          }

      def fetchHash[A](
          algo: Algo[A],
          hash: HashValue[A],
          path: Path,
          uri: String
      ): F[Either[PlatformIO.FetchHashFailure, Unit]] = {
        val expected = hash.toIdent(using algo)
        val failure =
          if (uri.contains("hash-mismatch")) {
            PlatformIO.FetchHashFailure.HashMismatch(
              uri,
              expected,
              s"${algo.name}:${"0" * hash.hex.length}"
            )
          } else if (uri.contains("404")) {
            PlatformIO.FetchHashFailure.HttpStatus(uri, "404 Not Found")
          } else if (uri.contains("network")) {
            PlatformIO.FetchHashFailure.Network(uri, "connection reset by peer")
          } else {
            PlatformIO.FetchHashFailure.Network(
              uri,
              s"fetchHash($algo, $hash, $path, $uri) not implemented yet."
            )
          }
        moduleIOMonad.pure(Left(failure))
      }

      def unfoldDir(path: Path): F[Option[F[List[Path]]]] =
        StateT
          .get[G, MemoryMain.State]
          .map { files =>
            files.get(path).flatMap {
              case Left(state) =>
                Some(moduleIOMonad.pure(state.children.iterator.map {
                  case (k, _) =>
                    path :+ k
                }.toList))
              case Right(_) => None
            }
          }

      def hasExtension(str: String): (Path => Boolean) = { (path: Path) =>
        path.toList.lastOption.exists(_.endsWith(str))
      }

      def pathPackage(
          roots: List[Path],
          packFile: Path
      ): Option[PackageName] =
        PlatformIO.pathPackage(roots, packFile) { (root, pf) =>
          relativize(root, pf).map(_.iterator.toList)
        }

      def writeFC(p: Path, fc: FileContent): F[Unit] =
        StateT.modifyF { state =>
          state.withFile(p, fc) match {
            case Some(newState) => innerMonad.pure(newState)
            case None           =>
              innerMonad.raiseError(
                new Exception(
                  s"couldn't write to $p because it is already a directory."
                )
              )
          }
        }

      def writeDoc(p: Path, d: Doc): F[Unit] =
        writeFC(p, FileContent.Str(d.renderTrim(100)))

      def writeInterfaces(
          ifaces: List[Package.Interface],
          path: Path
      ): F[Unit] =
        writeFC(path, FileContent.Interfaces(ifaces))

      def writePackages[A](packs: List[Package.Typed[A]], path: Path): F[Unit] =
        writeFC(path, FileContent.Packages(packs.map(_.void)))

      def writeLibrary(lib: proto.Library, path: Path): F[Unit] = {
        val hash = Algo.hashBytes(lib.toByteArray)
        val hashed = Hashed(hash, lib)
        writeFC(path, FileContent.Lib(hashed))
      }

      def writeStdout(doc: Doc): F[Unit] =
        StateT.modify { state =>
          state.copy(stdOut = state.stdOut + (doc + Doc.hardLine))
        }

      def writeError(doc: Doc): F[Unit] =
        StateT.modify { state =>
          state.copy(stdErr = state.stdErr + (doc + Doc.hardLine))
        }

      def println(str: String): F[Unit] =
        writeStdout(Doc.text(str))

      def errorln(str: String): F[Unit] =
        writeError(Doc.text(str))

      override def resolve(base: Path, parts: List[String]): Path =
        base ++ Chain.fromSeq(parts)
    }
  }
}
