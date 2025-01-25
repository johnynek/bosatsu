package org.bykn.bosatsu.tool

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import org.bykn.bosatsu.{PlatformIO, Package, PackageError, PackageMap, PackageName, Par}
import org.bykn.bosatsu.LocationMap.Colorize
import org.typelevel.paiges.Doc
import scala.util.{Success, Failure, Try}

import org.bykn.bosatsu.IorMethods.IorExtension

import cats.syntax.all._

object CompilerApi {
  private def toTry[Path, A](
      v: ValidatedNel[PathParseError[Path], A],
      color: Colorize
  ): Try[A] =
    v match {
      case Validated.Valid(a) => Success(a)
      case Validated.Invalid(errs) =>
        Failure(ParseErrors(errs, color))
    }

  /** like typecheck, but a no-op for empty lists
    */
  private def typeCheck0[IO[_], Path](
      platformIO: PlatformIO[IO, Path],
      inputs: List[Path],
      ifs: List[Package.Interface],
      errColor: Colorize,
      packRes: PackageResolver[IO, Path]
  )(implicit
      ec: Par.EC
  ): IO[(PackageMap.Inferred, List[(Path, PackageName)])] =
    NonEmptyList.fromList(inputs) match {
      case None =>
        // we should still return the predef
        // if it is not in ifs
        val useInternalPredef =
          !ifs.exists { (p: Package.Interface) =>
            p.name == PackageName.PredefName
          }

        if (useInternalPredef) {
          platformIO.moduleIOMonad.pure(
            (PackageMap.fromIterable(PackageMap.predefCompiled :: Nil), Nil)
          )
        } else {
          platformIO.moduleIOMonad.pure((PackageMap.empty, Nil))
        }
      case Some(nel) => typeCheck(platformIO, nel, ifs, errColor, packRes)
    }

  def typeCheck[IO[_], Path](
      platformIO: PlatformIO[IO, Path],
      inputs: NonEmptyList[Path],
      ifs: List[Package.Interface],
      errColor: Colorize,
      packRes: PackageResolver[IO, Path]
  )(implicit
      ec: Par.EC,
  ): IO[(PackageMap.Inferred, List[(Path, PackageName)])] = {
    import platformIO.moduleIOMonad

    packRes.parseAllInputs(inputs.toList, ifs.map(_.name).toSet)(platformIO)
      .flatMap { ins =>
        moduleIOMonad.fromTry {
          // Now we have completed all IO, here we do all the checks we need for correctness
          toTry(ins, errColor)
            .flatMap { packs =>
              // TODO, we could use applicative, to report both duplicate packages and the other
              // errors
              NonEmptyList.fromList(packs) match {
                case Some(packs) =>
                  val packsString = packs.map { case ((path, lm), parsed) =>
                    ((path.toString, lm), parsed)
                  }
                  PackageMap
                    .typeCheckParsed[String](packsString, ifs, "predef")
                    .strictToValidated match {
                    case Validated.Valid(p) =>
                      val pathToName: List[(Path, PackageName)] =
                        packs.map { case ((path, _), p) =>
                          (path, p.name)
                        }.toList
                      Success((p, pathToName))
                    case Validated.Invalid(errs) =>
                      val sourceMap = PackageMap.buildSourceMap(packs)
                      Failure(
                        PackageErrors(
                          sourceMap,
                          errs,
                          errColor
                        )
                      )
                  }
                case None =>
                  Success((PackageMap.empty, Nil))
              }
            }
        }
      }
    }

  def buildPackMap[IO[_], Path](
      platformIO: PlatformIO[IO, Path],
      srcs: List[Path],
      deps: List[Path],
      errColor: Colorize,
      packRes: PackageResolver[IO, Path]
  )(implicit
      ec: Par.EC
  ): IO[(PackageMap.Typed[Any], List[(Path, PackageName)])] = {
    import platformIO.moduleIOMonad

    for {
      packs <- platformIO.readPackages(deps)
      ifaces = packs.map(Package.interfaceOf(_))
      packsList <- typeCheck0(platformIO, srcs, ifaces, errColor, packRes)
      (thesePacks, lst) = packsList
      packMap = packs.foldLeft(PackageMap.toAnyTyped(thesePacks))(_ + _)
    } yield (packMap, lst)
  }

  private def PackageErrors(
    sourceMap: PackageMap.SourceMap,
    errors: NonEmptyList[PackageError],
    color: Colorize
  ): CliException with Exception = {
      val messages: List[String] =
        errors.toList.distinct
          .map(_.message(sourceMap, color))

      val messageString: String = messages.mkString("\n")
      val errDoc = Doc.intercalate(Doc.hardLine, messages.map(Doc.text(_)))

    CliException(messageString, errDoc)
  }

  private def ParseErrors[Path](
    errors: NonEmptyList[PathParseError[Path]],
    color: Colorize
  ): CliException with Exception = {

    val messages: List[String] =
      errors.toList.flatMap {
        case PathParseError.ParseFailure(pf, path) =>
          // we should never be partial here
          val (r, c) = pf.locations.toLineCol(pf.position).get
          val ctx = pf.showContext(color)
          List(
            s"failed to parse $path:${r + 1}:${c + 1}",
            ctx.render(80)
          )
        case PathParseError.FileError(path, err) =>
          err match {
            case e
                if e.getClass.getName == "java.nio.file.NoSuchFileException" =>
              // This class isn't present in scalajs, use the String
              List(s"file not found: $path")
            case _ =>
              List(
                s"failed to parse $path",
                err.getMessage,
                err.getClass.toString
              )
          }
      }

    val messageString: String = messages.mkString("\n")
    val errDoc = Doc.intercalate(Doc.hardLine, messages.map(Doc.text(_)))

    CliException(messageString, errDoc)
  }
}