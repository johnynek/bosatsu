package dev.bosatsu.tool

import cats.parse.{Parser => P}
import cats.syntax.all._
import com.monovore.decline.{Argument, Opts}
import dev.bosatsu.{Identifier, PackageName, Parser, PlatformIO}
import dev.bosatsu.Identifier.Bindable

sealed trait MainIdentifier[Path] {
  def path: Option[Path]

  def getMain[F[_]](
      ps: List[(Path, PackageName)]
  )(platformIO: PlatformIO[F, Path]): F[(PackageName, Option[Bindable])]
}

object MainIdentifier {
  final case class FromPackage[Path](
      mainPackage: PackageName,
      value: Option[Bindable]
  ) extends MainIdentifier[Path] {
    def path: Option[Path] = None

    def getMain[F[_]](
        ps: List[(Path, PackageName)]
    )(platformIO: PlatformIO[F, Path]): F[(PackageName, Option[Bindable])] = {
      import platformIO.moduleIOMonad

      moduleIOMonad.pure((mainPackage, value))
    }
  }

  final case class FromFile[Path](mainFile: Path) extends MainIdentifier[Path] {
    def path: Option[Path] = Some(mainFile)

    def getMain[F[_]](
        ps: List[(Path, PackageName)]
    )(platformIO: PlatformIO[F, Path]): F[(PackageName, Option[Bindable])] = {
      import platformIO.{moduleIOMonad, pathOrdering, showPath}

      ps.collectFirst {
        case (path, pn) if pathOrdering.equiv(path, mainFile) => pn
      } match {
        case Some(p) => moduleIOMonad.pure((p, None))
        case None    =>
          moduleIOMonad.raiseError(
            CliException.Basic(
              show"could not find file $mainFile in parsed sources"
            )
          )
      }
    }
  }

  implicit val argument: Argument[(PackageName, Option[Bindable])] =
    Parser.argFromParser(
      (PackageName.parser ~ (P.string("::") *> Identifier.bindableParser).?),
      "valueIdent",
      "package or package::name",
      "Must be a package name with an optional :: value, e.g. Foo/Bar or Foo/Bar::baz."
    )

  def opts[Path](
      pnOpts: Opts[(PackageName, Option[Bindable])],
      fileOpts: Opts[Path]
  ): Opts[MainIdentifier[Path]] =
    pnOpts
      .map { case (p, i) => FromPackage[Path](p, i) }
      .orElse(fileOpts.map(FromFile(_)))

  def list[Path](
      packs: Opts[List[(PackageName, Option[Bindable])]],
      files: Opts[List[Path]]
  ): Opts[List[MainIdentifier[Path]]] =
    (packs, files).mapN { (ps, fs) =>
      ps.map { case (p, i) => FromPackage[Path](p, i) } ::: fs.map(FromFile(_))
    }

  def addAnyAbsent[Path](
      ms: List[MainIdentifier[Path]],
      paths: List[Path]
  )(using ordering: Ordering[Path]): List[Path] = {
    val toAdd =
      ms.iterator
        .flatMap(_.path)
        .filterNot(p => paths.exists(ordering.equiv(_, p)))
        .toList

    toAdd ::: paths
  }
}
