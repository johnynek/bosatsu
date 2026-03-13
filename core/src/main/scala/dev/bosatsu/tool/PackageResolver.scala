package dev.bosatsu.tool

import cats.data.{NonEmptyList, ValidatedNel}
import cats.parse.{Parser => P}
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{LocationMap, Package, PackageName, PlatformIO}

/** This parses packages from explicit inputs.
  */
sealed abstract class PackageResolver[IO[_], Path] {

  final def parseAllInputs(
      paths: NonEmptyList[Path],
      _included: Set[PackageName]
  )(platformIO: PlatformIO[IO, Path]): IO[
    ValidatedNel[PathParseError[Path], NonEmptyList[
      ((Path, LocationMap), Package.Parsed)
    ]]
  ] = {
    import platformIO.moduleIOMonad
    if (_included.nonEmpty) ()
    paths
      .traverse { path =>
        PathParseError
          .parseFile(Package.parser, path, platformIO)
          .map(_.map { case (lm, parsed) =>
            ((path, lm), parsed)
          })
      }
      .map(_.sequence)
  }

  def parseHeaders(
      paths: List[Path]
  )(
      platformIO: PlatformIO[IO, Path]
  ): IO[ValidatedNel[PathParseError[Path], List[(Path, Package.Header)]]] = {
    import platformIO.moduleIOMonad
    // we use IO(traverse) so we can accumulate all the errors in parallel easily
    // if do this with parseFile returning an IO, we need to do IO.Par[Validated[...]]
    // and use the composed applicative... too much work for the same result
    val headerParser = Package.headerParser <* P.anyChar.rep0
    paths
      .traverse { path =>
        platformIO.readUtf8(path).map { str =>
          PathParseError
            .parseString(headerParser, path, str)
            .map { case (_, pp) => (path, pp) }
        }
      }
      .map(_.sequence)
  }
}

object PackageResolver {
  case class ExplicitOnly[IO[_], Path]() extends PackageResolver[IO, Path]

  def opts[IO[_], Path](
      platformIO: PlatformIO[IO, Path]
  ): Opts[PackageResolver[IO, Path]] = {
    val _ = platformIO
    Opts(ExplicitOnly())
  }

  // type-checking and writing protos should be explicit.
  def noSearchOpts[IO[_], Path](
      platformIO: PlatformIO[IO, Path]
  ): Opts[PackageResolver[IO, Path]] =
    opts(platformIO)
}
