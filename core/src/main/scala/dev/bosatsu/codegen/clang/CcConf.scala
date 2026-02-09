package dev.bosatsu.codegen.clang

import dev.bosatsu.{Json, PlatformIO}

case class CcConf(
    ccPath: String,
    flags: List[String],
    iFlags: List[String],
    libs: List[String],
    os: String
) {
  def compile[F[_], Path](
      src: Path,
      to: Path,
      extraFlags: List[String] = Nil,
      extraLibs: List[String] = Nil
  )(
      platformIO: PlatformIO[F, Path]
  ): F[Unit] = {
    val args = List.newBuilder[String]

    args ++= flags
    args ++= extraFlags
    args ++= iFlags
    args += "-o"
    args += platformIO.pathToString(to)
    args += platformIO.pathToString(src)
    args ++= libs
    args ++= extraLibs

    platformIO.system(ccPath, args.result())
  }
}

object CcConf {

  implicit val ccConfJsonReader: Json.Reader[CcConf] =
    new Json.Reader.Obj[CcConf] {
      def describe = "CcConf"
      def readObj(
          from: Json.Reader.FromObj
      ): Either[(String, Json, Json.Path), CcConf] = {
        import from.field
        for {
          ccPath <- field[String]("cc_path")
          flags <- field[List[String]]("flags")
          iFlags <- field[List[String]]("iflags")
          libs <- field[List[String]]("libs")
          os <- field[String]("os")
        } yield CcConf(ccPath, flags, iFlags, libs, os)
      }
    }

  def parse(j: Json): Either[(String, Json, Json.Path), CcConf] =
    ccConfJsonReader.read(Json.Path.Root, j)
}
