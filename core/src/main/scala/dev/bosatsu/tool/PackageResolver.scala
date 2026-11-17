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
  Padding,
  Parser,
  PlatformIO,
  Statement
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
        moduleIOMonad.attempt(platformIO.readBytes(path)).flatMap {
          case Left(err) =>
            moduleIOMonad.pure(
              cats.data.Validated
                .invalidNel(PathParseError.FileError(path, err))
            )
          case Right(bytes) =>
            // Decoding, raw hashing, and header parsing can add up across many
            // files, so keep that pure CPU work off the caller thread.
            canPromiseF.compute {
              val source = new String(bytes, StandardCharsets.UTF_8)
              val rawHash = Algo.hashBytes[Algo.Blake3](bytes)
              PathParseError
                .parseString(
                  PackageResolver.headerParserIgnoreRest,
                  path,
                  source
                )
                .map { case (lm, (packageName, imports, exports, exposes)) =>
                  PackageResolver.SourceFile(
                    path = path,
                    locationMap = lm,
                    packageName = packageName,
                    imports = imports,
                    exports = exports,
                    exposes = exposes,
                    sourceHash = rawHash,
                    loadParsed =
                      // On a cache miss we need the full parse; defer that
                      // heavier pure work to the compute pool.
                      canPromiseF.compute {
                        PathParseError
                          .parseString(Package.parser, path, source)
                          .map(_._2)
                      }
                  )
                }
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
    import platformIO.moduleIOMonad
    paths
      .traverse { path =>
        PathParseError
          .parseFile(PackageResolver.headerParserIgnoreRest, path, platformIO)
          .map(_.map { case (_, header) => (path, header) })
      }
      .map(_.sequence)
  }
}

object PackageResolver {
  // At the first body position a header keyword can still be a binding name.
  // Check just its binding head. Parsing an entire header clause here would
  // accept an incomplete or misplaced import as opaque body text.
  private val headerKeyword =
    Parser.lowerIdent.filter(Set("package", "from", "export", "exposes"))
  private val unexpectedHeader = Padding.parser(
    (headerKeyword.peek *> !Statement.bindingHead).with1 *> headerKeyword,
    Package.headerSpaceCommentParser
  )
  val headerParserIgnoreRest =
    Package.headerParser <* !unexpectedHeader <* P.anyChar.rep0.void

  final case class SourceFile[IO[_], Path](
      path: Path,
      locationMap: LocationMap,
      packageName: PackageName,
      imports: List[Import[PackageName, Unit]],
      exports: List[ExportedName[Unit]],
      exposes: List[List[PackageName]],
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
        exposes = exposes,
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
