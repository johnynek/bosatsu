package dev.bosatsu.tool

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.syntax.all._
import dev.bosatsu.LocationMap.Colorize
import dev.bosatsu.hashing.Algo
import dev.bosatsu.library.{DecodedLibrary, LibConfig, Name, Version}
import dev.bosatsu.{Package, PackageName, PlatformIO}
import org.typelevel.paiges.Doc

import java.util.regex.Pattern

object CommandSupport {
  type DepDecode[Path] = (Path, DecodedLibrary[Algo.Blake3])

  def noInputs[F[_], Path, A](
      platformIO: PlatformIO[F, Path],
      commandName: String
  ): F[A] = {
    import platformIO.moduleIOMonad

    moduleIOMonad.raiseError(
      CliException.Basic(show"no inputs given to $commandName")
    )
  }

  private def readDep[F[_], Path](
      platformIO: PlatformIO[F, Path],
      path: Path
  ): F[DecodedLibrary[Algo.Blake3]] = {
    import platformIO.moduleIOMonad

    for {
      lib <- platformIO.readLibrary(path)
      decLib <- DecodedLibrary.decode(lib)
    } yield decLib
  }

  private def validateDepSet[F[_], Path](
      platformIO: PlatformIO[F, Path],
      pub: List[DecodedLibrary[Algo.Blake3]],
      priv: List[DecodedLibrary[Algo.Blake3]]
  ): F[Unit] = {
    import platformIO.moduleIOMonad

    val conf = LibConfig(
      name = Name("_tool"),
      repoUri = "",
      nextVersion = Version.zero,
      previous = None,
      exportedPackages = Nil,
      allPackages = LibConfig.PackageFilter.Regex(Pattern.compile(".*")) :: Nil,
      publicDeps = pub.map(_.toDep),
      privateDeps = priv.map(_.toDep),
      defaultMain = None
    )
    val validated = conf.validateDeps(pub ::: priv)
    moduleIOMonad.fromTry(LibConfig.Error.toTry(validated)).void
  }

  def readDepLibraries[F[_], Path](
      platformIO: PlatformIO[F, Path],
      publicDependencyPaths: List[Path],
      privateDependencyPaths: List[Path]
  ): F[(List[DepDecode[Path]], List[DepDecode[Path]])] = {
    import platformIO.moduleIOMonad

    val publicReads =
      publicDependencyPaths.traverse { path =>
        for {
          dep <- readDep(platformIO, path)
        } yield (path, dep)
      }
    val privateReads =
      privateDependencyPaths.traverse { path =>
        for {
          dep <- readDep(platformIO, path)
        } yield (path, dep)
      }

    (publicReads, privateReads).flatMapN { (pub, priv) =>
      validateDepSet(platformIO, pub.map(_._2), priv.map(_._2)).as((pub, priv))
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
