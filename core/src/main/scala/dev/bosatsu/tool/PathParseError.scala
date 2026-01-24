package dev.bosatsu.tool

import cats.data.{Validated, ValidatedNel}
import cats.parse.{Parser0 => P0}
import cats.syntax.all._
import dev.bosatsu.{LocationMap, Parser, PlatformIO}

sealed trait PathParseError[Path]
object PathParseError {
  case class ParseFailure[Path](error: Parser.Error.ParseFailure, path: Path)
      extends PathParseError[Path]
  case class FileError[Path](readPath: Path, error: Throwable)
      extends PathParseError[Path]

  def parseString[Path, A](
      p: P0[A],
      path: Path,
      str: String
  ): ValidatedNel[PathParseError[Path], (LocationMap, A)] =
    Parser.parse(p, str).leftMap { nel =>
      nel.map { case pf @ Parser.Error.ParseFailure(_, _, _) =>
        PathParseError.ParseFailure(pf, path)
      }
    }

  def parseFile[IO[_], Path, A](
      p: P0[A],
      path: Path,
      platformIO: PlatformIO[IO, Path]
  ): IO[ValidatedNel[PathParseError[Path], (LocationMap, A)]] = {
    import platformIO.moduleIOMonad

    platformIO
      .readUtf8(path)
      .attempt
      .map {
        case Right(str) => PathParseError.parseString(p, path, str)
        case Left(err)  =>
          Validated.invalidNel(PathParseError.FileError(path, err))
      }
  }
}
