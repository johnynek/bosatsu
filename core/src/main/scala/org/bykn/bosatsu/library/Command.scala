package org.bykn.bosatsu.library

import cats.MonoidK
import org.bykn.bosatsu.{Json, PlatformIO}
import org.bykn.bosatsu.tool.{CliException, Output}
import com.monovore.decline.Opts
import cats.syntax.all._

object Command {
  def opts[F[_], P](platformIO: PlatformIO[F, P]): Opts[F[Output[P]]] = {
    import platformIO.{pathArg, moduleIOMonad, showPath}

    val libsPath: F[(P, P)] =
      platformIO.gitTopLevel
        .flatMap {
          case Some(p) =>
            moduleIOMonad.pure((p, platformIO.resolve(p, "bosatsu_libs.json")))
          case None =>
            (
              platformIO.pathF("."),
              platformIO.pathF("bosatsu_libs.json")
            ).tupled
        }

    def readLibs(path: P): F[Libraries] =
      platformIO.fsDataType(path).flatMap {
        case None => moduleIOMonad.pure(Libraries.empty)
        case Some(PlatformIO.FSDataType.File) =>
          platformIO
            .parseUtf8(path, Json.parserFile)
            .flatMap { json =>
              Json.Reader[Libraries].read(Json.Path.Root, json) match {
                case Right(lib) => moduleIOMonad.pure(lib)
                case Left((msg, j, p)) =>
                  moduleIOMonad.raiseError[Libraries](
                    CliException.Basic(show"$msg: from json = $j at $p")
                  )
              }
            }
        case Some(PlatformIO.FSDataType.Dir) =>
          moduleIOMonad.raiseError[Libraries](
            CliException
              .Basic(show"expected $path to be a file, not directory.")
          )
      }

    val initCommand =
      Opts.subcommand("init", "initialize a config") {
        (
          Opts.option[String]("name", "name of the library"),
          Opts.option[String](
            "repo_uri",
            "uri for the version control of this library"
          ),
          Opts.option[P](
            "src_root",
            "path to the src root for the bosatsu packages in this library"
          ),
          Opts.option[Version]("version", "the initial version to use")
        ).mapN { (name, repoUri, rootDir, ver) =>
          val conf = LibConfig.init(name, repoUri, ver)
          val confJson = Json.Writer.write(conf)
          val confPath = platformIO.resolve(rootDir, s"${name}_conf.json")
          val writeJson = Output.JsonOutput(confJson, Some(confPath))

          libsPath
            .flatMap { case (gitRoot, path) =>
              for {
                lib0 <- readLibs(path)
                relDir <- platformIO.relativize(gitRoot, rootDir) match {
                  case Some(value) => moduleIOMonad.pure(value)
                  case None =>
                    moduleIOMonad.raiseError(
                      CliException.Basic(
                        show"$rootDir is not a subdir of $gitRoot"
                      )
                    )
                }
                lib1 = lib0.updated(name, show"$relDir")
                out1 = Output.JsonOutput(Json.Writer.write(lib1), Some(path))
              } yield Output.many(out1, writeJson)
            }
        }
      }

    val listCommand =
      Opts.subcommand("list", "print all the known libraries") {
        Opts(
          libsPath
            .flatMap { case (_, path) =>
              for {
                lib0 <- readLibs(path)
                json = Json.Writer.write(lib0)
              } yield (Output.JsonOutput(json, None): Output[P])
            }
        )
      }

    MonoidK[Opts].combineAllK(initCommand :: listCommand :: Nil)
  }
}
