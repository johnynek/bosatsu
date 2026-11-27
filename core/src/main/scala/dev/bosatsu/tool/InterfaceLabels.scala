package dev.bosatsu.tool

import cats.syntax.all._
import dev.bosatsu.{Json, PlatformIO}

/** A label map annotates the declared input files. Paths in JSON never
  * introduce new file reads and labels never participate in inference keys.
  */
private[bosatsu] object InterfaceLabels {
  final private case class Entry(path: String, label: String)
  private given Json.Reader[Entry] =
    BuildManifest.obj("interface attribution") { from =>
      (
        from.field[String]("path")(using BuildManifest.nonEmpty),
        from.field[String]("label")(using BuildManifest.nonEmpty)
      ).mapN(Entry(_, _))
    }
  private val reader = BuildManifest.obj("interface label map") { from =>
    BuildManifest.version1(from) *> from.field[List[Entry]]("interfaces")
  }

  def read[F[_], P](
      platform: PlatformIO[F, P],
      file: P,
      declared: List[P]
  ): F[Map[P, String]] = {
    import platform.{moduleIOMonad, showPath}
    val inputs = declared.toSet
    BuildManifest.read(platform, file, reader).flatMap { entries =>
      val result = entries
        .foldM(Map.empty[P, String]) { (labels, entry) =>
          platform
            .path(entry.path)
            .toEither
            .leftMap(_.toList.mkString(", "))
            .flatMap { path =>
              if (!inputs(path))
                Left(show"undeclared interface $path (${entry.label})")
              else if (labels.contains(path))
                Left(show"duplicate entry for interface $path")
              else Right(labels.updated(path, entry.label))
            }
        }
        .flatMap { labels =>
          val missing = declared.filterNot(labels.contains)
          Either.cond(
            missing.isEmpty,
            labels,
            show"missing labels for: ${missing.map(_.show).sorted.mkString(", ")}"
          )
        }
      moduleIOMonad.fromEither(
        result.leftMap(msg =>
          CliException.Basic(show"invalid interface label map $file: $msg")
        )
      )
    }
  }
}
