package dev.bosatsu.tool

import cats.syntax.all._
import dev.bosatsu.{Json, PlatformIO}
import dev.bosatsu.library.{Name, Version}

/** Caller-declared files available to a full library inspection. The same
  * identity may have multiple paths so the verifier can authenticate aliases
  * and reject conflicting bytes instead of silently selecting one.
  */
private[bosatsu] object LibraryInspection {
  final case class DeclaredLibraryFile[Path](
      name: Name,
      version: Version,
      path: Path,
      hashes: List[String]
  )

  final case class DependencyContext[Path](libraries: List[DeclaredLibraryFile[Path]])

  private final case class RawLibraryFile(
      name: Name,
      version: Version,
      path: String,
      hashes: List[String]
  )

  private final case class RawDependencyContext(libraries: List[RawLibraryFile])

  private type Result[A] = BuildManifest.Result[A]

  private val libraryNameReader: Json.Reader[Name] = new Json.Reader[Name] {
    val describe = "nonempty library name"
    def read(path: Json.Path, value: Json): Result[Name] =
      BuildManifest.nonEmpty.read(path, value).map(Name(_))
  }

  private val rawLibraryFileReader: Json.Reader[RawLibraryFile] =
    BuildManifest.obj("declared library file") { from =>
      for {
        _ <- BuildManifest.onlyFields(
          from,
          "declared library file",
          Set("name", "version", "path", "hashes")
        )
        name <- from.field[Name]("name")(using libraryNameReader)
        version <- from.field[Version]("version")
        path <- from.field[String]("path")(using BuildManifest.nonEmpty)
        hashes <- from.field[List[String]]("hashes")(
          using Json.Reader.listReader(using BuildManifest.nonEmpty)
        )
        _ <- Either.cond(hashes.nonEmpty, (),
          ("expected at least one recorded hash", from.j, from.path.key("hashes")))
      } yield RawLibraryFile(name, version, path, hashes)
    }

  private val rawContextReader: Json.Reader[RawDependencyContext] =
    BuildManifest.obj("library dependency context") { from =>
      for {
        _ <- BuildManifest.version1(from)
        _ <- BuildManifest.onlyFields(
          from,
          "library dependency context",
          Set("schema_version", "libraries")
        )
        libraries <- from.field[List[RawLibraryFile]]("libraries")(
          using Json.Reader.listReader(using rawLibraryFileReader)
        )
      } yield RawDependencyContext(libraries)
    }

  /** Read only the context JSON. Paths are interpreted by the platform's
    * argument parser; the declarations do not trigger file reads or fetching.
    */
  def readDependencyContext[F[_], Path](
      platform: PlatformIO[F, Path],
      file: Path
  ): F[DependencyContext[Path]] = {
    import platform.{moduleIOMonad, showPath}

    BuildManifest.read(platform, file, rawContextReader).flatMap { raw =>
      raw.libraries.zipWithIndex.traverse { case (library, index) =>
        moduleIOMonad.fromEither(
          platform.path(library.path).toEither.leftMap { errors =>
            CliException.Basic(
              show"invalid dependency path '${library.path}' in $file at " +
                s"/libraries[$index].path: ${errors.toList.mkString(", ")}"
            )
          }.map { path =>
            DeclaredLibraryFile(library.name, library.version, path, library.hashes)
          }
        )
      }.map(DependencyContext(_))
    }
  }
}
