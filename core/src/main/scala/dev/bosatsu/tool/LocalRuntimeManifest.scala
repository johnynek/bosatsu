package dev.bosatsu.tool

import cats.data.NonEmptyList
import cats.Order
import cats.syntax.all._
import dev.bosatsu.{Identifier, Json, PackageName, PlatformIO}
import scala.collection.immutable.{SortedMap, SortedSet}

private[bosatsu] final case class LocalRuntimeManifest[P](
    packages: NonEmptyList[P],
    externals: SortedMap[PackageName, SortedSet[Identifier.Bindable]]
)

private[bosatsu] object LocalRuntimeManifest {
  private final case class Binding(pack: PackageName, symbols: List[Identifier.Bindable])
  private final case class Manifest(packages: NonEmptyList[String], bindings: List[Binding])

  private given Json.Reader[Identifier.Bindable] =
    Json.Reader.fromParser("Bosatsu binding name", Identifier.bindableParser)

  private def onlyFields(from: Json.Reader.FromObj, kind: String, allowed: Set[String]): BuildManifest.Result[Unit] = {
    val unknown = from.j.toMap.keySet -- allowed
    Either.cond(unknown.isEmpty, (),
      (s"unknown $kind fields: ${unknown.toList.sorted.mkString(", ")}", from.j, from.path))
  }

  private given Json.Reader[Binding] = BuildManifest.obj("native binding provider") { from =>
    for {
      _ <- onlyFields(from, "native binding", Set("package", "symbols"))
      pack <- from.field[PackageName]("package")
      symbols <- from.field[List[Identifier.Bindable]]("symbols")
      _ <- Either.cond(symbols.nonEmpty, (),
        ("expected at least one external symbol", from.j, from.path.key("symbols")))
    } yield Binding(pack, symbols)
  }

  private val reader = BuildManifest.obj("local runtime manifest") { from =>
    for {
      _ <- BuildManifest.version1(from)
      kind <- from.field[String]("kind")
      _ <- Either.cond(kind == "local", (),
        (s"unsupported runtime scope '$kind'", from.j, from.path.key("kind")))
      // Do not silently flatten an imported scope or ignore its bindings.
      _ <- onlyFields(from, "local runtime", Set("schema_version", "kind", "packages", "externals"))
      files <- from.field[List[String]]("packages")(using Json.Reader.listReader(using BuildManifest.nonEmpty))
      packages <- NonEmptyList.fromList(files).toRight(
        ("expected at least one implementation bundle", from.j, from.path.key("packages")))
      bindings <- from.field[List[Binding]]("externals")
    } yield Manifest(packages, bindings)
  }

  def read[F[_], P](platform: PlatformIO[F, P], file: P): F[LocalRuntimeManifest[P]] = {
    import platform.{moduleIOMonad, showPath}
    given Order[P] = Order.fromOrdering(using platform.pathOrdering)
    BuildManifest.read(platform, file, reader).flatMap { raw =>
      val result = for {
        packages <- raw.packages.traverse { name =>
          platform.path(name).toEither.leftMap(errs => s"invalid package path '$name': ${errs.toList.mkString(", ")}")
        }
        bindings <- raw.bindings.foldM(SortedMap.empty[PackageName, SortedSet[Identifier.Bindable]]) {
          case (prev, Binding(pack, names)) =>
            names.foldM(prev.getOrElse(pack, SortedSet.empty[Identifier.Bindable])) { (symbols, name) =>
              Either.cond(!symbols(name), symbols + name,
                show"duplicate native binding for $pack::${name.sourceCodeRepr}")
            }.map(prev.updated(pack, _))
        }
      } yield LocalRuntimeManifest(packages.distinct.sorted, bindings)
      moduleIOMonad.fromEither(result.leftMap(reason => CliException.Basic(show"invalid runtime manifest $file: $reason")))
    }
  }
}
