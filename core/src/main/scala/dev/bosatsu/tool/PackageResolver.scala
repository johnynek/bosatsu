package dev.bosatsu.tool

import cats.data.{NonEmptyList, ValidatedNel}
import cats.parse.{Parser => P}
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.hashing.{Algo, HashValue}
import dev.bosatsu.{
  ExportedName,
  Import,
  LocationMap,
  Package,
  PackageMap,
  PackageName,
  PlatformIO
}

import java.nio.charset.StandardCharsets

/** This parses packages from explicit inputs.
  */
sealed abstract class PackageResolver[IO[_], Path] {

  final def loadSourceFiles(
      paths: NonEmptyList[Path]
  )(platformIO: PlatformIO[IO, Path]): IO[
    ValidatedNel[PathParseError[Path], NonEmptyList[
      PackageResolver.SourceFile[IO, Path]
    ]]
  ] = {
    import platformIO.{canPromiseF, moduleIOMonad}
    paths
      .traverse { path =>
        moduleIOMonad.attempt(platformIO.readBytes(path)).map {
          case Left(err) =>
            cats.data.Validated.invalidNel(PathParseError.FileError(path, err))
          case Right(bytes) =>
            val source = new String(bytes, StandardCharsets.UTF_8)
            val rawHash = Algo.hashBytes[Algo.Blake3](bytes)
            PathParseError.parseString(
              PackageResolver.headerParserIgnoreRest,
              path,
              source
            ).map {
              case (lm, (packageName, imports, exports)) =>
                PackageResolver.SourceFile(
                  path = path,
                  locationMap = lm,
                  packageName = packageName,
                  imports = imports,
                  exports = exports,
                  sourceHash = rawHash,
                  loadParsed =
                    canPromiseF.compute {
                      PathParseError
                        .parseString(Package.parser, path, source)
                        .map(_._2)
                    }
                )
            }
        }
      }
      .map(_.sequence)
  }

  def parseHeaders(
      paths: List[Path]
  )(
      platformIO: PlatformIO[IO, Path]
  ): IO[ValidatedNel[PathParseError[Path], List[(Path, Package.Header)]]] = {
    import platformIO.{canPromiseF, moduleIOMonad}
    // we use IO(traverse) so we can accumulate all the errors in parallel easily
    // if do this with parseFile returning an IO, we need to do IO.Par[Validated[...]]
    // and use the composed applicative... too much work for the same result
    paths
      .traverse { path =>
        platformIO.readUtf8(path).flatMap { str =>
          canPromiseF.compute {
            PathParseError
              .parseString(PackageResolver.headerParserIgnoreRest, path, str)
              .map { case (_, pp) => (path, pp) }
          }
        }
      }
      .map(_.sequence)
  }
}

object PackageResolver {
  val headerParserIgnoreRest = Package.headerParser <* P.anyChar.rep0

  final case class SourceFile[IO[_], Path](
      path: Path,
      locationMap: LocationMap,
      packageName: PackageName,
      imports: List[Import[PackageName, Unit]],
      exports: List[ExportedName[Unit]],
      sourceHash: HashValue[Algo.Blake3],
      loadParsed: IO[ValidatedNel[PathParseError[Path], Package.Parsed]]
  ) {
    def toSourceUnit[A](
        sourceKey: A,
        loadParsedF: IO[Package.Parsed]
    ): PackageMap.SourceUnit[IO, A] =
      PackageMap.SourceUnit(
        sourceKey = sourceKey,
        locationMap = locationMap,
        packageName = packageName,
        imports = imports,
        exports = exports,
        sourceHash = sourceHash,
        loadParsed = loadParsedF
      )
  }

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
