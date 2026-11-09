package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{Json, LocationMap, Package, PlatformIO}
import dev.bosatsu.tool.{CommandSupport, Output, PackageResolver}

/** Source metadata for build tools. Version 1 uses only lexical paths and
  * package headers. Bodies, timestamps and dependency contents do not enter its
  * output. Imported symbol names are retained for interface selection.
  */
object HeadersCommand {
  private def headerJson(path: String, header: Package.Header): Json = {
    val (pkg, imports, _, exposes) = header
    val importEntries = imports.groupBy(_.pack).toList.sortBy(_._1).map {
      case (from, statements) =>
        val names = statements
          .flatMap(_.items.toList)
          .map { item =>
            (item.originalName.sourceCodeRepr, item.localName.sourceCodeRepr)
          }
          .distinct
          .sorted
        Json.JObject(
          ("package" -> Json.JString(from.asString)) ::
            ("names" -> Json.JArray(names.toVector.map {
              case (original, local) =>
                Json.JObject(
                  ("original" -> Json.JString(original)) ::
                    ("local" -> Json.JString(local)) :: Nil
                )
            })) :: Nil
        )
    }

    Json.JObject(
      ("path" -> Json.JString(path)) ::
        ("package" -> Json.JString(pkg.asString)) ::
        ("imports" -> Json.JArray(importEntries.toVector)) ::
        ("exposes" -> Json.JArray(
          exposes.flatten.distinct.sorted.toVector
            .map(p => Json.JString(p.asString))
        )) :: Nil
    )
  }

  def opts[F[_], Path](platform: PlatformIO[F, Path]): Opts[F[Output[Path]]] = {
    import platform.{moduleIOMonad, pathArg}
    val sources =
      Opts.options[Path]("src", "explicit Bosatsu source file (repeatable)")
    val output =
      Opts.option[Path]("out", "versioned package-header JSON output")
    Opts.subcommand(
      "headers",
      "read package headers from explicit source files"
    ) {
      (sources, output).mapN { (srcs, dest) =>
        val paths = srcs.toList.distinct.sortBy(platform.pathToString)
        for {
          parsed <- PackageResolver
            .ExplicitOnly[F, Path]()
            .parseHeaders(paths)(platform)
          headers <- CommandSupport.liftParseErrors(
            platform,
            parsed,
            LocationMap.Colorize.None
          )
        } yield {
          val packages = headers.map { case (path, header) =>
            headerJson(platform.pathToString(path), header)
          }
          val json = Json.JObject(
            ("schema_version" -> Json.JNumberStr("1")) ::
              ("packages" -> Json.JArray(packages.toVector)) :: Nil
          )
          Output.JsonOutput(json, Some(dest)): Output[Path]
        }
      }
    }
  }
}
