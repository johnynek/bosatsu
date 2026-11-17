package dev.bosatsu.tool

import _root_.bosatsu.{TypedAst => proto}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.syntax.all._
import dev.bosatsu.LocationMap.Colorize
import dev.bosatsu.hashing.{Algo, Hashed}
import dev.bosatsu.library.{DecodedLibrary, LibConfig, Name, Version}
import dev.bosatsu.{Package, PackageName, PlatformIO}
import org.typelevel.paiges.Doc

import java.util.regex.Pattern

object CommandSupport {
  type DepDecode[Path] = (Path, DecodedLibrary[Algo.Blake3])

  def dependencyPackageDocBaseUrls(
      dependencies: Iterable[DecodedLibrary[Algo.Blake3]]
  ): Either[CliException.Basic, Map[PackageName, String]] = {
    val packageBaseEntries =
      for {
        dep <- dependencies.iterator.toList.sortBy(dep =>
          (dep.name.name, dep.version.render)
        )
        // A library-level base URL applies to each package it exports.
        baseUrl <- dep.docBaseUrl.toList
        iface <- dep.interfaces.iterator
      } yield (iface.name, baseUrl, dep.name.name, dep.version.render)

    val groupedByPackage =
      packageBaseEntries.groupBy(_._1).toList.sortBy(_._1.asString)

    val conflicts =
      groupedByPackage.flatMap { case (pn, entries) =>
        val byBaseUrl = entries.groupBy(_._2).toList.sortBy(_._1)
        if (byBaseUrl.size <= 1) Nil
        else {
          // Same package should resolve to one docs site; mixed bases are ambiguous.
          val details = byBaseUrl.map { case (baseUrl, baseEntries) =>
            val providers = baseEntries
              .map { case (_, _, depName, depVersion) =>
                show"$depName $depVersion"
              }
              .distinct
              .sorted
              .mkString(", ")
            show"$baseUrl from $providers"
          }
          show"${pn.asString}: ${details.mkString("; ")}" :: Nil
        }
      }

    if (conflicts.isEmpty) {
      Right(groupedByPackage.iterator.map { case (pn, entries) =>
        pn -> entries.head._2
      }.toMap)
    } else {
      Left(
        CliException.Basic(
          show"conflicting doc_base_url values for dependency packages: ${conflicts.mkString(" | ")}"
        )
      )
    }
  }

  def noInputs[F[_], Path, A](
      platformIO: PlatformIO[F, Path],
      commandName: String
  ): F[A] = {
    import platformIO.moduleIOMonad

    moduleIOMonad.raiseError(
      CliException.Basic(show"no inputs given to $commandName")
    )
  }

  type DepArtifact[Path] = (Path, Hashed[Algo.Blake3, proto.Library])

  // A type-check action needs library descriptors, file hashes and raw
  // exported entries. Symbolic re-exports can only bind after their owners
  // have been selected from all source, signature and library inputs.
  def readDepArtifacts[F[_], Path](
      platformIO: PlatformIO[F, Path],
      publicPaths: List[Path],
      privatePaths: List[Path]
  ): F[(List[DepArtifact[Path]], List[DepArtifact[Path]])] = {
    import platformIO.moduleIOMonad

    def read(paths: List[Path]) =
      paths.traverse(path => platformIO.readLibrary(path).map(path -> _))
    def descriptors(artifacts: List[DepArtifact[Path]]) =
      artifacts.traverse { case (_, lib) =>
        DecodedLibrary.versionOf(lib).map(DecodedLibrary.toDep(lib, _))
      }

    for {
      pub <- read(publicPaths)
      priv <- read(privatePaths)
      publicDeps <- moduleIOMonad.fromEither(descriptors(pub))
      privateDeps <- moduleIOMonad.fromEither(descriptors(priv))
      conf = LibConfig(
        name = Name("_tool"),
        repo_uri = "",
        next_version = Version.zero,
        previous = None,
        exported_packages = Nil,
        all_packages =
          LibConfig.PackageFilter.Regex(Pattern.compile(".*")) :: Nil,
        public_deps = publicDeps,
        private_deps = privateDeps,
        default_main = None
      )
      validated = conf.validateDepArtifacts((pub ::: priv).map(_._2))
      _ <- moduleIOMonad.fromTry(LibConfig.Error.toTry(validated))
    } yield (pub, priv)
  }

  def readDepLibraries[F[_], Path](
      platformIO: PlatformIO[F, Path],
      publicDependencyPaths: List[Path],
      privateDependencyPaths: List[Path]
  ): F[(List[DepDecode[Path]], List[DepDecode[Path]])] = {
    import platformIO.{canPromiseF, moduleIOMonad}
    def decode(artifacts: List[DepArtifact[Path]]) =
      artifacts.traverse { case (path, lib) =>
        DecodedLibrary.decode(lib).map(path -> _)
      }

    readDepArtifacts(platformIO, publicDependencyPaths, privateDependencyPaths)
      .flatMap { case (pub, priv) =>
        (decode(pub), decode(priv)).tupled
      }
  }

  def dependencyInterfaces[Path](
      dependencies: (List[DepDecode[Path]], List[DepDecode[Path]])
  ): List[Package.Interface] =
    (dependencies._1 ::: dependencies._2).flatMap(_._2.interfaces)

  def dependencyPackages[Path](
      dependencies: (List[DepDecode[Path]], List[DepDecode[Path]])
  ): List[Package.Typed[Any]] =
    (dependencies._1 ::: dependencies._2).flatMap { case (_, decoded) =>
      decoded.implementations.toMap.values.toList
    }

  private def duplicatePackNames(
      packs: List[Package.Typed[Any]]
  ): List[PackageName] =
    packs
      .groupBy(_.name)
      .collect { case (pn, _ :: _ :: _) => pn }
      .toList
      .sorted

  def ensureDistinctPackages[F[_], Path](
      platformIO: PlatformIO[F, Path],
      packs: List[Package.Typed[Any]],
      note: String
  ): F[Unit] = {
    import platformIO.moduleIOMonad

    val dups = duplicatePackNames(packs)
    if (dups.isEmpty) moduleIOMonad.unit
    else
      moduleIOMonad.raiseError(
        CliException.Basic(
          show"duplicate package names in $note: ${dups.map(_.asString).mkString(", ")}"
        )
      )
  }

  def liftParseErrors[F[_], Path, A](
      platformIO: PlatformIO[F, Path],
      v: ValidatedNel[PathParseError[Path], A],
      color: Colorize
  ): F[A] = {
    import platformIO.{moduleIOMonad, showPath}

    v match {
      case Validated.Valid(a)      => moduleIOMonad.pure(a)
      case Validated.Invalid(errs) =>
        moduleIOMonad.raiseError(parseErrors(errs, color))
    }
  }

  private def parseErrors[Path](
      errors: NonEmptyList[PathParseError[Path]],
      color: Colorize
  )(using cats.Show[Path]): Exception & CliException = {
    val messages: List[String] =
      errors.toList.flatMap {
        case PathParseError.ParseFailure(pf, path) =>
          val (r, c) = pf.locations.toLineCol(pf.position).get
          val ctx = pf.showContext(color)
          List(
            show"failed to parse $path:${r + 1}:${c + 1}",
            ctx.render(80)
          )
        case PathParseError.FileError(path, err) =>
          err match {
            // This looks weird, but it is for scalajs which doesn't have the same
            // class.
            case e
                if e.getClass.getName == "java.nio.file.NoSuchFileException" =>
              List(show"file not found: $path")
            case _ =>
              List(
                show"failed to parse $path",
                err.getMessage,
                err.getClass.toString
              )
          }
      }

    val messageString = messages.mkString("\n")
    val errDoc = Doc.intercalate(Doc.hardLine, messages.map(Doc.text(_)))

    CliException(messageString, errDoc)
  }
}
