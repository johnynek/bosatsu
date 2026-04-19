package dev.bosatsu.codegen.clang

import dev.bosatsu.{Json, PlatformIO}

case class CcConf(
    cc_path: String,
    flags: List[String],
    iflags: List[String],
    libs: List[String],
    os: String
) derives Json.Reader, Json.Writer {
  def compile[F[_], Path](
      src: Path,
      to: Path,
      extraFlags: List[String] = Nil,
      extraLibs: List[String] = Nil
  )(
      platformIO: PlatformIO[F, Path]
  ): F[Unit] = {
    val linkArgs = (libs ::: extraLibs).distinct
    val args = List.newBuilder[String]

    args ++= flags
    args ++= extraFlags
    args ++= iflags
    args += "-o"
    args += platformIO.pathToString(to)
    args += platformIO.pathToString(src)
    args ++= linkArgs

    platformIO.system(cc_path, args.result())
  }
}

object CcConf {
  def parse(j: Json): Either[(String, Json, Json.Path), CcConf] =
    Json.Reader[CcConf].read(Json.Path.Root, j)
}
