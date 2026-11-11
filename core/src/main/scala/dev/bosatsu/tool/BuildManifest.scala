package dev.bosatsu.tool

import cats.syntax.all._
import dev.bosatsu.{Json, PlatformIO}
import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, StandardCharsets}

/** JSON used by file-based build actions. Decode a manifest inside the
  * request and retain both its filename and the JSON field on failure.
  */
private[bosatsu] object BuildManifest {
  type Result[A] = Either[(String, Json, Json.Path), A]

  val nonEmpty: Json.Reader[String] =
    Json.Reader[String].mapEither("nonempty string") { text =>
      Either.cond(text.nonEmpty, text, "expected a nonempty string")
    }

  def obj[A](name: String)(
      decode: Json.Reader.FromObj => Result[A]
  ): Json.Reader.Obj[A] =
    new Json.Reader.Obj[A] {
      val describe = name
      def readObj(from: Json.Reader.FromObj): Result[A] =
        if (from.j.items.lengthCompare(from.j.toMap.size) == 0) decode(from)
        else {
          val repeated = from.j.items.groupMapReduce(_._1)(_ => 1)(_ + _)
            .collect { case (key, count) if count > 1 => key }.toList.sorted
          Left((s"repeated JSON fields: ${repeated.mkString(", ")}", from.j, from.path))
        }
    }

  def version1(from: Json.Reader.FromObj): Result[Unit] =
    from.field[Int]("schema_version").flatMap { version =>
      Either.cond(version == 1, (), (
        s"unsupported schema version $version, expected 1", from.j, from.path.key("schema_version")
      ))
    }

  def read[F[_], P, A](platform: PlatformIO[F, P], file: P, reader: Json.Reader[A]): F[A] = {
    import platform.{moduleIOMonad, showPath}
    platform.readBytes(file).adaptError { case e =>
      CliException.Basic(show"cannot read manifest $file: ${e.getMessage}")
    }.flatMap { bytes =>
      // Replacing a malformed byte can make distinct file paths or group
      // labels compare equal. A fresh decoder reports malformed UTF-8.
      val decoded =
        try Right(StandardCharsets.UTF_8.newDecoder().decode(ByteBuffer.wrap(bytes)).toString)
        catch { case _: CharacterCodingException => Left(show"invalid UTF-8 in manifest $file") }
      val result = for {
        content <- decoded
        json <- Json.parserFile.parseAll(content).leftMap(e => show"invalid JSON in $file: $e")
        value <- reader.read(Json.Path.Root, json).leftMap { case (reason, _, location) =>
          show"invalid manifest $file at $location: $reason"
        }
      } yield value
      moduleIOMonad.fromEither(result.leftMap(CliException.Basic(_)))
    }
  }
}
