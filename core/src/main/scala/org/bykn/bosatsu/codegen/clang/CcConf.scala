package org.bykn.bosatsu.codegen.clang

import org.bykn.bosatsu.{Json, PlatformIO}

case class CcConf(
    ccPath: String,
    flags: List[String],
    iFlags: List[String],
    libs: List[String],
    os: String
) {
  def compile[F[_], Path](src: Path, to: Path)(platformIO: PlatformIO[F, Path]): F[Unit] = {
    val args = List.newBuilder[String]

    args ++= flags
    args ++= iFlags
    args += "-o"
    args += platformIO.pathToString(to)
    args += platformIO.pathToString(src)
    args ++= libs

    platformIO.system(ccPath, args.result())
  }
}

object CcConf {
  def parse(j: Json): Either[(String, Json), CcConf] = {
    import Json._

    def parseStringField(jObj: JObject, fieldName: String): Either[(String, Json), String] = {
      jObj.toMap.get(fieldName) match {
        case Some(JString(value)) => Right(value)
        case Some(other)          => Left((s"Field '$fieldName' must be a string", other))
        case None                 => Left((s"Missing field: '$fieldName'", j))
      }
    }

    def parseStringArrayField(jObj: JObject, fieldName: String): Either[(String, Json), List[String]] = {
      jObj.toMap.get(fieldName) match {
        case Some(JArray(elements)) =>
          elements.foldRight[Either[(String, Json), List[String]]](Right(Nil)) {
            case (JString(value), Right(acc)) => Right(value :: acc)
            case (other, _)                   => Left((s"Elements of '$fieldName' must be strings", other))
          }
        case Some(other) => Left((s"Field '$fieldName' must be an array of strings", other))
        case None        => Left((s"Missing field: '$fieldName'", j))
      }
    }

    j match {
      case obj: JObject =>
        for {
          ccPath <- parseStringField(obj, "cc_path")
          flags <- parseStringArrayField(obj, "flags")
          iFlags <- parseStringArrayField(obj, "iflags")
          libs <- parseStringArrayField(obj, "libs")
          os <- parseStringField(obj, "os")
        } yield CcConf(ccPath, flags, iFlags, libs, os)

      case _ => Left(("Expected a JSON object", j))
    }
  }
}
