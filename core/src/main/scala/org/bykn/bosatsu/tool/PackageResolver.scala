package org.bykn.bosatsu.tool

import cats.data.NonEmptyList
import org.bykn.bosatsu.{PackageName, PlatformIO}
import cats.syntax.all._
import com.monovore.decline.Opts

/** This is a class that names packages based on path and finds packages
  * based on imports
  */
sealed abstract class PackageResolver[IO[_], Path] {
  def pathFor(name: PackageName)(io: PlatformIO[IO, Path]): IO[Option[Path]]
  def packageNameFor(path: Path)(io: PlatformIO[IO, Path]): Option[PackageName]
}
object PackageResolver {
  case class ExplicitOnly[IO[_], Path]() extends PackageResolver[IO, Path] {
    def pathFor(name: PackageName)(io: PlatformIO[IO, Path]): IO[Option[Path]] =
      io.moduleIOMonad.pure(Option.empty[Path])
    def packageNameFor(path: Path)(io: PlatformIO[IO, Path]): Option[PackageName] = None
  }

  case class LocalRoots[IO[_], Path](
      roots: NonEmptyList[Path],
      optResolvePath: Option[(Path, PackageName) => IO[Option[Path]]]
  ) extends PackageResolver[IO, Path] {
    def pathFor(name: PackageName)(io: PlatformIO[IO, Path]): IO[Option[Path]] =
      optResolvePath match {
        case None => io.moduleIOMonad.pure(Option.empty[Path])
        case Some(resolvePath) =>
          import io.moduleIOMonad

          def step(p: List[Path]): IO[Either[List[Path], Option[Path]]] =
            p match {
              case Nil =>
                io.moduleIOMonad.pure(Right[List[Path], Option[Path]](None))
              case phead :: ptail =>
                resolvePath(phead, name).map {
                  case None => Left[List[Path], Option[Path]](ptail)
                  case some @ Some(_) =>
                    Right[List[Path], Option[Path]](some)
                }
            }

          moduleIOMonad.tailRecM(roots.toList)(step)
      }

    def packageNameFor(path: Path)(io: PlatformIO[IO, Path]): Option[PackageName] =
      io.pathPackage(roots.toList, path)
  }


  private def packRoot[IO[_], Path](platformIO: PlatformIO[IO, Path]): Opts[NonEmptyList[Path]] = {
    import platformIO.pathArg
    Opts.options[Path](
      "package_root",
      help = "for implicit package names, consider these paths as roots"
    )
  }

  private def packSearch[IO[_], Path](
    platformIO: PlatformIO[IO, Path]
  ): Opts[Option[(Path, PackageName) => IO[Option[Path]]]] =
    Opts
      .flag(
        "search",
        help =
          "if set, we search the package_roots for imports not explicitly given"
      )
      .orFalse
      .map {
        case true  => Some((p, pn) => platformIO.resolveFile(p, pn))
        case false => None
      }

  def opts[IO[_], Path](platformIO: PlatformIO[IO, Path]): Opts[PackageResolver[IO, Path]] =
    (packRoot(platformIO)
      .product(packSearch(platformIO)))
      .orNone
      .map {
        case None => PackageResolver.ExplicitOnly()
        case Some((paths, search)) =>
          PackageResolver.LocalRoots(paths, search)
      }

  // type-checking and writing protos should be explicit. search option isn't supported
  def noSearchOpts[IO[_], Path](platformIO: PlatformIO[IO, Path]): Opts[PackageResolver[IO, Path]] =
    packRoot(platformIO)
      .orNone
      .map {
        case None        => PackageResolver.ExplicitOnly()
        case Some(paths) => PackageResolver.LocalRoots(paths, None)
      }
}