package org.bykn.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}

import cats.MonoidK
import org.bykn.bosatsu.{Json, PlatformIO}
import org.bykn.bosatsu.tool.{CliException, Output}
import com.monovore.decline.Opts
import cats.syntax.all._

object Command {
  def opts[F[_], P](platformIO: PlatformIO[F, P]): Opts[F[Output[P]]] = {
    import platformIO.{pathArg, moduleIOMonad, showPath}

    val topLevelOpt: Opts[F[P]] =
      Opts.option[P]("repo_root", "the path to the root of the repo, if not set, search for .git directory")
        .orNone
        .map {
          case Some(value) => moduleIOMonad.pure(value)
          case None =>
            platformIO.gitTopLevel
              .flatMap {
                case Some(value) => moduleIOMonad.pure(value)
                case None => moduleIOMonad.raiseError(CliException.Basic("could not find .git directory in parents."))
              }
        }

    def libsPath(root: P): P = platformIO.resolve(root, "bosatsu_libs.json")

    def readJson[A: Json.Reader](path: P, onEmpty: => F[A]): F[A] =
      platformIO.fsDataType(path).flatMap {
        case None => onEmpty
        case Some(PlatformIO.FSDataType.File) =>
          platformIO.parseUtf8(path, Json.parserFile)
            .flatMap { json =>
              Json.Reader[A].read(Json.Path.Root, json) match {
                case Right(a) => moduleIOMonad.pure(a)
                case Left((msg, j, p)) =>
                  moduleIOMonad.raiseError[A](
                    CliException.Basic(show"$msg: from json = $j at $p")
                  )
              }
            }
        case Some(PlatformIO.FSDataType.Dir) =>
          moduleIOMonad.raiseError[A](CliException.Basic(show"expected $path to be a file, not directory."))
      }  

    def readLibs(path: P): F[Libraries] =
      readJson[Libraries](path, moduleIOMonad.pure(Libraries.empty))

    def readLibConf(name: Name, path: P): F[LibConfig] =
      readJson[LibConfig](path, moduleIOMonad.raiseError(CliException.Basic(show"expected $path to exist to read $name")))

    def confPath(root: P, name: Name): P =
      platformIO.resolve(root, s"${name.name}_conf.json")

    val initCommand = 
      Opts.subcommand("init", "initialize a config") {
        (Opts.option[Name]("name", "name of the library"),
          Opts.option[String]("repo_uri", "uri for the version control of this library"),
          Opts.option[P]("src_root", "path to the src root for the bosatsu packages in this library"),
          Opts.option[Version]("version", "the initial version to use"),
          topLevelOpt
        ).mapN { (name, repoUri, rootDir, ver, repoRootF) =>
          val conf = LibConfig.init(name, repoUri, ver)  
          val confJson = Json.Writer.write(conf)
          val writeJson = Output.JsonOutput(confJson, Some(confPath(rootDir, name)))

          repoRootF
            .flatMap { gitRoot =>
              val path = libsPath(gitRoot)
              for {
                lib0 <- readLibs(path)
                relDir <- platformIO.relativize(gitRoot, rootDir) match {
                  case Some(value) => moduleIOMonad.pure(value)
                  case None =>
                    moduleIOMonad.raiseError(CliException.Basic(show"$rootDir is not a subdir of $gitRoot"))
                }
                lib1 = lib0.updated(name, show"$relDir")
                out1 = Output.JsonOutput(Json.Writer.write(lib1), Some(path))
              } yield Output.many(out1, writeJson)
            }
        }
      }

    val listCommand =
      Opts.subcommand("list", "print all the known libraries") {
        topLevelOpt
          .map { gitRootF =>
            for {
              gitRoot <- gitRootF
              path = libsPath(gitRoot)
              lib0 <- readLibs(path)
              json = Json.Writer.write(lib0)
            } yield (Output.JsonOutput(json, None): Output[P])
          }
      }

    val optName: Opts[Name] = Opts.option[Name]("name", help = "the name of the library to consider, if not set and there is" +
      "only one, use that.", "n")

    /*
     Git the path to the repo root, the name of the library we are working with and the path to that library 
     */
    val rootAndName: Opts[F[(P, Name, P)]] =
      // either there is only one path, or we need to have both
      (topLevelOpt, optName.orNone)
        .mapN { (gitRootF, optName) =>
          for {
            gitRoot <- gitRootF
            path = libsPath(gitRoot)
            libs <- readLibs(path)
            namePath <- optName match {
              case None =>
                // there must be only one library
                if (libs.toMap.size == 1) {
                  val (name, pathStr) = libs.toMap.head
                  platformIO.pathF(pathStr).map((name, _))
                }
                else {
                  moduleIOMonad.raiseError(
                    CliException.Basic(show"more than one library, select one: ${libs.toMap.keys.toList.sorted}")
                  )
                }
              case Some(value) =>
                // make sure this name exists
                libs.get(value) match {
                  case Some(path) =>
                    platformIO.pathF(path).map((value, _))
                  case None =>
                    moduleIOMonad.raiseError(
                      CliException.Basic(show"library $value not found. Select one of: ${libs.toMap.keys.toList.sorted}")
                    )
                }
            }
            (name, path) = namePath
          } yield (gitRoot, name, platformIO.resolve(gitRoot, path))
        }

    val assembleCommand =
      Opts.subcommand("assemble", "construct a .bosatsu_lib from the configuration and .bosatsu_package files") {
        (rootAndName,
        Opts.options[P]("packages", help = "all the packages to include in this library.", "p").orEmpty,
        Opts.option[P]("output", help = "path to write the libary to, or default name_{version}.bosatsu_lib", "o").orNone
        )
          .mapN { (fpnp, packs, optOut) =>
            for {
              pnp <- fpnp
              (gitRoot, name, confPath) = pnp
              conf <- readLibConf(name, confPath)
              packages <- platformIO.readPackages(packs)
              lib <- moduleIOMonad.fromTry(conf.assemble(packages))
            } yield (null: Output[P])
          }
      }

    MonoidK[Opts].combineAllK(initCommand :: listCommand :: assembleCommand :: Nil)
  }
}