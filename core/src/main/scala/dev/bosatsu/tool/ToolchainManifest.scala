package dev.bosatsu.tool

import _root_.bosatsu.{TypedAst => proto}
import cats.data.NonEmptyList
import cats.syntax.all._
import dev.bosatsu.{Json, PackageName, PlatformIO}
import dev.bosatsu.hashing.{Algo, HashValue}
import dev.bosatsu.library.{Library, Name, Version}
import scala.util.Try

/** The selected detached manifest must be authenticated by its caller, along
  * with the jar that is about to run. Inspecting the running jar cannot
  * authenticate an otherwise untrusted manifest or jar.
  */
private[bosatsu] final case class ToolchainManifest(
    compiler: ToolchainManifest.Artifact,
    core: ToolchainManifest.Core,
    cRuntime: ToolchainManifest.Artifact,
    coreSources: Option[Map[PackageName, HashValue[Algo.Sha256]]]
)

private[bosatsu] object ToolchainManifest {
  final case class Artifact(uri: String, sha256: HashValue[Algo.Sha256])
  final case class Core(
      libraryName: Name,
      version: Version,
      uri: String,
      sha256: HashValue[Algo.Sha256]
  )

  sealed trait CoreSelection[+P] derives CanEqual
  object CoreSelection {
    case object None extends CoreSelection[Nothing]
    final case class Binary[P](path: P) extends CoreSelection[P]
    final case class Sources[P](paths: NonEmptyList[P]) extends CoreSelection[P]
  }

  private type Result[A] = BuildManifest.Result[A]

  private val sha256Reader: Json.Reader[HashValue[Algo.Sha256]] =
    Json.Reader.fromParser("SHA-256 hash", Algo.parseHashHex(Algo.sha256Algo))

  private val libraryNameReader: Json.Reader[Name] = new Json.Reader[Name] {
    val describe = "nonempty library name"
    def read(path: Json.Path, value: Json): Result[Name] =
      BuildManifest.nonEmpty.read(path, value).map(Name(_))
  }

  private def onlyFields(
      from: Json.Reader.FromObj,
      kind: String,
      allowed: Set[String]
  ): Result[Unit] = {
    val unknown = from.j.toMap.keySet -- allowed
    Either.cond(unknown.isEmpty, (),
      (s"unknown $kind fields: ${unknown.toList.sorted.mkString(", ")}", from.j, from.path))
  }

  private given Json.Reader[Artifact] = BuildManifest.obj("toolchain artifact") { from =>
    for {
      _ <- onlyFields(from, "toolchain artifact", Set("uri", "sha256"))
      uri <- from.field[String]("uri")(using BuildManifest.nonEmpty)
      sha256 <- from.field[HashValue[Algo.Sha256]]("sha256")(using sha256Reader)
    } yield Artifact(uri, sha256)
  }

  private given Json.Reader[Core] = BuildManifest.obj("toolchain core") { from =>
    for {
      _ <- onlyFields(from, "toolchain core", Set("library_name", "version", "uri", "sha256"))
      libraryName <- from.field[Name]("library_name")(using libraryNameReader)
      version <- from.field[Version]("version")
      uri <- from.field[String]("uri")(using BuildManifest.nonEmpty)
      sha256 <- from.field[HashValue[Algo.Sha256]]("sha256")(using sha256Reader)
    } yield Core(libraryName, version, uri, sha256)
  }

  private def readCoreSources(from: Json.Reader.FromObj): Result[Option[Map[PackageName, HashValue[Algo.Sha256]]]] =
    from.j.getOrNull("core_sources") match {
      case Json.JNull => Right(None)
      case sources: Json.JObject =>
        val sourcePath = from.path.key("core_sources")
        val repeated = sources.items.groupMapReduce(_._1)(_ => 1)(_ + _)
          .collect { case (key, count) if count > 1 => key }.toList.sorted
        if (repeated.nonEmpty)
          Left((s"repeated core source packages: ${repeated.mkString(", ")}", sources, sourcePath))
        else
          sources.items.traverse { case (rawName, jsonHash) =>
            val path = sourcePath.key(rawName)
            for {
              name <- PackageName.jsonReader.read(path, Json.JString(rawName))
              hash <- sha256Reader.read(path, jsonHash)
            } yield name -> hash
          }.map(_.toMap.some)
      case other =>
        Left(("expected object of package names to SHA-256 hashes", other, from.path.key("core_sources")))
    }

  private val reader: Json.Reader[ToolchainManifest] =
    BuildManifest.obj("toolchain manifest") { from =>
      for {
        _ <- BuildManifest.version1(from)
        _ <- onlyFields(from, "toolchain manifest", Set("schema_version", "compiler", "core", "c_runtime", "core_sources"))
        compiler <- from.field[Artifact]("compiler")
        core <- from.field[Core]("core")
        cRuntime <- from.field[Artifact]("c_runtime")
        coreSources <- readCoreSources(from)
      } yield ToolchainManifest(compiler, core, cRuntime, coreSources)
    }

  def read[F[_], P](platform: PlatformIO[F, P], file: P): F[ToolchainManifest] =
    BuildManifest.read(platform, file, reader)

  private def readBytes[F[_], P](
      platform: PlatformIO[F, P],
      path: P,
      component: String
  ): F[Array[Byte]] = {
    import platform.moduleIOMonad
    import platform.showPath
    platform.readBytes(path).adaptError { case err =>
      CliException.Basic(show"cannot read $component $path: ${err.getMessage}")
    }
  }

  private def checkSha256[F[_], P](
      platform: PlatformIO[F, P],
      path: P,
      component: String,
      bytes: Array[Byte],
      expected: HashValue[Algo.Sha256]
  ): F[Unit] = {
    import platform.{moduleIOMonad, showPath}
    val actual = Algo.hashBytes[Algo.Sha256](bytes)
    if (actual.hex == expected.hex) moduleIOMonad.pure(())
    else moduleIOMonad.raiseError(CliException.Basic(
      show"$component SHA-256 mismatch for $path: expected ${expected.hex}, found ${actual.hex}"
    ))
  }

  private def decodeSource[F[_], P](
      platform: PlatformIO[F, P],
      path: P,
      bytes: Array[Byte],
      manifest: ToolchainManifest
  ): F[PackageName] = {
    import platform.{moduleIOMonad, showPath}
    val parsed = for {
      source <- BuildManifest.decodeUtf8(bytes).leftMap(_ => show"invalid UTF-8 in core source $path")
      packageName <- PackageResolver.headerParserIgnoreRest.parseAll(source).leftMap(err =>
        show"cannot parse package header in core source $path: $err"
      )
      name = packageName._1
    } yield name
    moduleIOMonad.fromEither(parsed.leftMap(CliException.Basic(_))).flatMap { name =>
      val expected = manifest.coreSources.flatMap(_.get(name)).toRight(
        CliException.Basic(show"core source $path declares unlisted package ${name.asString}")
      )
      moduleIOMonad.fromEither(expected)
        .flatMap(hash => checkSha256(platform, path, "core source", bytes, hash).as(name))
    }
  }

  private def sameVersionIdentity(left: Version, right: Version): Boolean =
    left.major == right.major &&
      left.minor == right.minor &&
      left.patch == right.patch &&
      left.preRelease.map(_.asString) == right.preRelease.map(_.asString) &&
      left.build.map(_.asString) == right.build.map(_.asString)

  private def verifyCore[F[_], P](
      platform: PlatformIO[F, P],
      manifest: ToolchainManifest,
      selection: CoreSelection[P]
  ): F[Unit] = {
    import platform.moduleIOMonad
    import platform.showPath
    selection match {
      case CoreSelection.None => moduleIOMonad.pure(())
      case CoreSelection.Binary(path) =>
        readBytes(platform, path, "core library").flatMap { bytes =>
          checkSha256(platform, path, "core library", bytes, manifest.core.sha256).flatMap { _ =>
            moduleIOMonad.fromTry(Try(proto.Library.parseFrom(bytes))).adaptError { case err =>
              CliException.Basic(show"cannot parse core library $path: ${err.getMessage}")
            }.flatMap { library =>
              Library.getVersion(library) match {
                case None => moduleIOMonad.raiseError(CliException.Basic(show"core library $path has no version"))
                case Some(_) if library.name != manifest.core.libraryName.name =>
                  moduleIOMonad.raiseError(CliException.Basic(
                    show"core library identity mismatch for $path: expected ${manifest.core.libraryName.name}, found ${library.name}"
                  ))
                case Some(version) if !sameVersionIdentity(version, manifest.core.version) =>
                  moduleIOMonad.raiseError(CliException.Basic(
                    show"core library version mismatch for $path: expected ${manifest.core.version.render}, found ${version.render}"
                  ))
                case Some(_) => moduleIOMonad.pure(())
              }
            }
          }
        }
      case CoreSelection.Sources(paths) =>
        paths.toList.traverse { path =>
          readBytes(platform, path, "core source").flatMap(bytes => decodeSource(platform, path, bytes, manifest))
        }.flatMap { packageNames =>
          val duplicates = packageNames.groupMapReduce(identity)(_ => 1)(_ + _)
            .collect { case (name, count) if count > 1 => name }
            .toList
            .sorted
          if (duplicates.nonEmpty)
            moduleIOMonad.raiseError(CliException.Basic(
              show"duplicate core source providers: ${duplicates.map(_.asString).mkString(", ")}"
            ))
          else moduleIOMonad.pure(())
        }
    }
  }

  private def verifyRuntime[F[_], P](
      platform: PlatformIO[F, P],
      manifest: ToolchainManifest,
      path: P,
      advertisedRuntimeHash: String
  ): F[Unit] = {
    import platform.moduleIOMonad
    import platform.showPath
    readBytes(platform, path, "runtime archive").flatMap { bytes =>
      checkSha256(platform, path, "runtime archive", bytes, manifest.cRuntime.sha256).flatMap { _ =>
        if (advertisedRuntimeHash.isEmpty) moduleIOMonad.pure(())
        else
          Algo.parseIdent.parseAll(advertisedRuntimeHash) match {
            case Left(error) =>
              moduleIOMonad.raiseError(CliException.Basic(
                show"invalid advertised runtime hash '$advertisedRuntimeHash': $error"
              ))
            case Right(advertised) =>
              val actual = advertised.algo.hashBytes(bytes)
              if (actual.hex == advertised.value.hex) moduleIOMonad.pure(())
              else moduleIOMonad.raiseError(CliException.Basic(
                show"runtime archive hash mismatch for $path: advertised ${advertised.algo.name}:${advertised.value.hex}, found ${advertised.algo.name}:${actual.hex}"
              ))
          }
      }
    }
  }

  /** Verify the compiler and only the components selected by the requested
    * action. The manifest's URI values are descriptive metadata and are never
    * fetched. The caller must already trust the manifest and running jar.
    */
  def verify[F[_], P](
      platform: PlatformIO[F, P],
      manifest: ToolchainManifest,
      compilerJar: P,
      core: CoreSelection[P],
      runtimeArchive: Option[P],
      advertisedRuntimeHash: String
  ): F[Unit] = {
    import platform.moduleIOMonad
    for {
      compilerBytes <- readBytes(platform, compilerJar, "compiler jar")
      _ <- checkSha256(platform, compilerJar, "compiler jar", compilerBytes, manifest.compiler.sha256)
      _ <- verifyCore(platform, manifest, core)
      _ <- runtimeArchive.traverse_(path => verifyRuntime(platform, manifest, path, advertisedRuntimeHash))
    } yield ()
  }
}
