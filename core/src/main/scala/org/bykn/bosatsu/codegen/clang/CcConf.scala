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

  implicit val ccConfJsonReader: Json.Reader[CcConf] =
    new Json.Reader[CcConf] {
      def describe = "CcConf"
      def read(path: Json.Path, json: Json): Either[(String, Json, Json.Path), CcConf] =
        json match {
          case obj: Json.JObject =>
            for {
              ccPath <- Json.Reader.readField[String](path, obj, "cc_path")
              flags <- Json.Reader.readField[List[String]](path, obj, "flags")
              iFlags <- Json.Reader.readField[List[String]](path, obj, "iflags")
              libs <- Json.Reader.readField[List[String]](path, obj, "libs")
              os <- Json.Reader.readField[String](path, obj, "os")
            } yield CcConf(ccPath, flags, iFlags, libs, os)
          case _ => Left((s"expected $describe", json, path))
        }
      }

  def parse(j: Json): Either[(String, Json, Json.Path), CcConf] =
    ccConfJsonReader.read(Json.Path.Root, j)
}
