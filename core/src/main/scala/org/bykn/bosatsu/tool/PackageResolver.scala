package org.bykn.bosatsu.tool

import cats.data.{Chain, NonEmptyList, Validated, ValidatedNel}
import cats.syntax.all._
import com.monovore.decline.Opts
import org.bykn.bosatsu.{LocationMap, Package, PackageName, PlatformIO}

/** This is a class that names packages based on path and finds packages based
  * on imports
  */
sealed abstract class PackageResolver[IO[_], Path] {
  def pathFor(name: PackageName)(io: PlatformIO[IO, Path]): IO[Option[Path]]
  def packageNameFor(path: Path)(io: PlatformIO[IO, Path]): Option[PackageName]

  /** This parses all the given paths and returns them first, and if the
    * PackageResolver supports it, we look for any missing dependencies that are
    * not already included
    */
  final def parseAllInputs(
      paths: List[Path],
      included: Set[PackageName]
  )(platformIO: PlatformIO[IO, Path]): IO[
    ValidatedNel[PathParseError[Path], List[
      ((Path, LocationMap), Package.Parsed)
    ]]
  ] = {
    import platformIO.moduleIOMonad

    type ParseTransResult = ValidatedNel[
      PathParseError[Path],
      (Chain[((Path, LocationMap), Package.Parsed)], Set[PackageName])
    ]

    def parseInputs(
        paths: List[Path]
    ): IO[ValidatedNel[PathParseError[Path], List[
      ((Path, LocationMap), Package.Parsed)
    ]]] =
      // we use IO(traverse) so we can accumulate all the errors in parallel easily
      // if do this with parseFile returning an IO, we need to do IO.Par[Validated[...]]
      // and use the composed applicative... too much work for the same result
      paths
        .traverse { path =>
          val defaultPack = packageNameFor(path)(platformIO)
          PathParseError
            .parseFile(Package.parser(defaultPack), path, platformIO)
            .map(_.map { case (lm, parsed) =>
              ((path, lm), parsed)
            })
        }
        .map(_.sequence)

    def flatTrav[A, B, C](va: Validated[A, B])(
        fn: B => IO[Validated[A, C]]
    ): IO[Validated[A, C]] =
      va.traverse(fn).map(_.andThen(identity _))

    def parseTransitive(
        search: PackageName,
        done: Set[PackageName]
    ): IO[ParseTransResult] = {

      val maybeReadPack: IO[Option[(Path, String)]] =
        if (done(search)) {
          moduleIOMonad.pure(Option.empty[(Path, String)])
        } else {
          pathFor(search)(platformIO)
            .flatMap(_.traverse { path =>
              platformIO.readUtf8(path).map((path, _))
            })
        }

      val optParsed: IO[
        ValidatedNel[PathParseError[Path], Option[
          ((Path, LocationMap), Package.Parsed)
        ]]
      ] =
        maybeReadPack.map { opt =>
          opt.traverse { case (path, str) =>
            val defaultPack = packageNameFor(path)(platformIO)
            PathParseError
              .parseString(Package.parser(defaultPack), path, str)
              .map { case (lm, parsed) =>
                ((path, lm), parsed)
              }
          }
        }

      def imports(p: Package.Parsed): List[PackageName] =
        p.imports.map(_.pack)

      val newDone = done + search

      optParsed.flatMap {
        flatTrav(_) {
          case None =>
            moduleIOMonad.pure(
              Validated.valid(
                (Chain.empty[((Path, LocationMap), Package.Parsed)], newDone)
              ): ParseTransResult
            )
          case Some(item @ (_, pack)) =>
            val imps = imports(pack).filterNot(done)
            parseTransitivePacks(imps, newDone)
              .map(_.map { case (newPacks, newDone) =>
                (item +: newPacks, newDone)
              })
        }
      }
    }

    def parseTransitivePacks(
        search: List[PackageName],
        done: Set[PackageName]
    ): IO[ParseTransResult] =
      search.foldM(Validated.valid((Chain.empty, done)): ParseTransResult) {
        (prev, impPack) =>
          flatTrav(prev) { case (acc, prevDone) =>
            parseTransitive(impPack, prevDone)
              .map(_.map { case (newPacks, newDone) =>
                (acc ++ newPacks, newDone)
              })
          }
      }

    parseInputs(paths)
      .flatMap {
        flatTrav(_) { parsed =>
          val done = included ++ parsed.toList.map(_._2.name)
          val allImports = parsed.toList.flatMap(_._2.imports.map(_.pack))
          val missing: List[PackageName] = allImports.filterNot(done)
          parseTransitivePacks(missing, done)
            .map(_.map { case (searched, _) => parsed ::: searched.toList })
        }
      }
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
    paths
      .traverse { path =>
        val defaultPack = packageNameFor(path)(platformIO)
        platformIO.readUtf8(path).map { str =>
          PathParseError
            .parseString(Package.headerParser(defaultPack), path, str)
            .map { case (_, pp) => (path, pp) }
        }
      }
      .map(_.sequence)
  }
}

object PackageResolver {
  case class ExplicitOnly[IO[_], Path]() extends PackageResolver[IO, Path] {
    def pathFor(name: PackageName)(io: PlatformIO[IO, Path]): IO[Option[Path]] =
      io.moduleIOMonad.pure(Option.empty[Path])
    def packageNameFor(path: Path)(
        io: PlatformIO[IO, Path]
    ): Option[PackageName] = None
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

    def packageNameFor(path: Path)(
        io: PlatformIO[IO, Path]
    ): Option[PackageName] =
      io.pathPackage(roots.toList, path)
  }

  private def packRoot[IO[_], Path](
      platformIO: PlatformIO[IO, Path]
  ): Opts[NonEmptyList[Path]] = {
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

  def opts[IO[_], Path](
      platformIO: PlatformIO[IO, Path]
  ): Opts[PackageResolver[IO, Path]] =
    (packRoot(platformIO)
      .product(packSearch(platformIO)))
      .orNone
      .map {
        case None => PackageResolver.ExplicitOnly()
        case Some((paths, search)) =>
          PackageResolver.LocalRoots(paths, search)
      }

  // type-checking and writing protos should be explicit. search option isn't supported
  def noSearchOpts[IO[_], Path](
      platformIO: PlatformIO[IO, Path]
  ): Opts[PackageResolver[IO, Path]] =
    packRoot(platformIO).orNone
      .map {
        case None        => PackageResolver.ExplicitOnly()
        case Some(paths) => PackageResolver.LocalRoots(paths, None)
      }
}
