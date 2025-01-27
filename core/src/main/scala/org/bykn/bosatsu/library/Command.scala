package org.bykn.bosatsu.library

import cats.{Monad, MonoidK}
import com.monovore.decline.Opts
import org.bykn.bosatsu.tool.{CliException, Output}
import org.bykn.bosatsu.hashing.{Algo, HashValue}
import org.bykn.bosatsu.{Json, PlatformIO}
import org.typelevel.paiges.Doc
import scala.collection.immutable.SortedMap

import _root_.bosatsu.{TypedAst => proto}

import cats.syntax.all._

object Command {
  def opts[F[_], P](platformIO: PlatformIO[F, P]): Opts[F[Output[P]]] = {
    import platformIO.{pathArg, moduleIOMonad, showPath}

    val topLevelOpt: Opts[F[P]] =
      Opts
        .option[P](
          "repo_root",
          "the path to the root of the repo, if not set, search for .git directory"
        )
        .orNone
        .map {
          case Some(value) => moduleIOMonad.pure(value)
          case None =>
            platformIO.gitTopLevel
              .flatMap {
                case Some(value) => moduleIOMonad.pure(value)
                case None =>
                  moduleIOMonad.raiseError(
                    CliException
                      .Basic("could not find .git directory in parents.")
                  )
              }
        }

    def libsPath(root: P): P = platformIO.resolve(root, "bosatsu_libs.json")

    def readJson[A: Json.Reader](path: P, onEmpty: => F[A]): F[A] =
      platformIO.fsDataType(path).flatMap {
        case None => onEmpty
        case Some(PlatformIO.FSDataType.File) =>
          platformIO
            .parseUtf8(path, Json.parserFile)
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
          moduleIOMonad.raiseError[A](
            CliException
              .Basic(show"expected $path to be a file, not directory.")
          )
      }

    def readLibs(path: P): F[Libraries] =
      readJson[Libraries](path, moduleIOMonad.pure(Libraries.empty))

    def readLibConf(name: Name, path: P): F[LibConfig] =
      readJson[LibConfig](
        path,
        moduleIOMonad.raiseError(
          CliException.Basic(show"expected $path to exist to read $name")
        )
      )

    def confPath(root: P, name: Name): P =
      platformIO.resolve(root, s"${name.name}_conf.json")

    def fetchAllDeps(casDir: P, deps: List[proto.LibDependency]): F[Doc] = {
      import platformIO.parallelF

      // Right(true): succeeded to download
      // Right(false): succeeded with cached
      // Left: failed to download or add to cache
      type DownloadRes = Either[Throwable, Boolean]
      type FetchState = SortedMap[
        (String, Version),
        SortedMap[Algo.WithAlgo[HashValue], DownloadRes]
      ]

      def hashes(dep: proto.LibDependency): List[Algo.WithAlgo[HashValue]] =
        for {
          desc <- dep.desc.toList
          hash <- desc.hashes
          hashValue <- Algo.parseIdent.parseAll(hash).toOption.toList
        } yield hashValue

      def depUris(dep: proto.LibDependency): List[String] =
        dep.desc.toList.flatMap(_.uris)

      def casPaths(
          dep: proto.LibDependency
      ): SortedMap[Algo.WithAlgo[HashValue], P] =
        hashes(dep)
          .map { withAlgo =>
            val algoName = withAlgo.algo.name
            val hex1 = withAlgo.value.hex.take(2)
            val hex2 = withAlgo.value.hex.drop(2)

            val path =
              platformIO.resolve(casDir, algoName :: hex1 :: hex2 :: Nil)

            (withAlgo, path)
          }
          .to(SortedMap)

      def libFromCas(dep: proto.LibDependency): F[Option[proto.Library]] =
        casPaths(dep).values.toList.collectFirstSomeM { path =>
          platformIO.fileExists(path).flatMap {
            case true  => platformIO.readLibrary(path).map(h => Some(h.arg))
            case false => Monad[F].pure(None)
          }
        }

      def fetchIfNeeded(
          dep: proto.LibDependency
      ): F[SortedMap[Algo.WithAlgo[HashValue], DownloadRes]] = {
        val paths = casPaths(dep)
        val uris = depUris(dep)

        paths.transform { (hashValue, path) =>
          platformIO
            .fileExists(path)
            .flatMap {
              case true => Monad[F].pure(Right(false)).widen[DownloadRes]
              case false =>
                {
                  // We need to download
                  uris
                    .foldM((List.empty[(String, Throwable)], false)) {
                      case ((fails, false), uri) =>
                        platformIO
                          .fetchHash(hashValue.algo, hashValue.value, path, uri)
                          .attempt
                          .map {
                            case Right(_) => ((fails, true))
                            case Left(e)  => (((uri, e) :: fails, false))
                          }
                      case (done, _) => Monad[F].pure(done)
                    }
                    .map {
                      case (_, true) =>
                        // we were able to download
                        Right(true)
                      case (fails, false) =>
                        // couldn't download
                        Left(
                          CliException(
                            s"download failure: ${dep.name} with ${fails.size} fails.",
                            if (fails.isEmpty)
                              Doc.text(
                                s"failed to fetch ${dep.name} with no uris."
                              )
                            else {
                              Doc.text(
                                s"failed to fetch ${dep.name} with ${fails.size} fails:"
                              ) +
                                (Doc.line + Doc.intercalate(
                                  Doc.line + Doc.line,
                                  fails.map { case (uri, f) =>
                                    Doc.text(
                                      s"uri=$uri failed with ${f.getMessage}"
                                    )
                                  }
                                )).nested(4)
                            }
                          )
                        )
                    }
                }
                  .widen[DownloadRes]
            }
        }.parSequence
      }

      def versionOf(dep: proto.LibDependency): Version =
        dep.desc.flatMap(_.version) match {
          case None    => Version.zero
          case Some(v) => Version.fromProto(v)
        }

      def step(
          fetched: FetchState,
          batch: List[proto.LibDependency]
      ): F[(FetchState, List[proto.LibDependency])] =
        batch
          .parTraverse { dep =>
            fetchIfNeeded(dep).map(fetchMap => (dep, fetchMap))
          }
          .flatMap { thisFetched =>
            val nextFetched = fetched ++ thisFetched.map { case (dep, fm) =>
              (dep.name, versionOf(dep)) -> fm
            }

            val nextBatchF: F[List[proto.LibDependency]] =
              thisFetched
                .parTraverse { case (dep, _) =>
                  libFromCas(dep)
                }
                .map { fetchedLibsOpt =>
                  val fetchedDeps = fetchedLibsOpt.flatMap {
                    case None      => Nil
                    case Some(dep) =>
                      // we will find the transitivies by walking them
                      dep.publicDependencies.toList ::: dep.privateDependencies.toList
                  }

                  fetchedDeps.filterNot { dep =>
                    nextFetched.contains((dep.name, versionOf(dep)))
                  }
                }

            nextBatchF.map((nextFetched, _))
          }

      moduleIOMonad
        .tailRecM((SortedMap.empty: FetchState, deps)) { case (fetched, deps) =>
          step(fetched, deps).map {
            case (state, Nil) => Right(state)
            case next         => Left(next)
          }
        }
        .flatMap { fs =>
          val depStr = if (fs.size == 1) "dependency" else "dependencies"
          val header = Doc.text(s"fetched ${fs.size} transitive ${depStr}.")

          val resultDoc = header + Doc.line + Doc.intercalate(
            Doc.hardLine,
            fs.toList.map { case ((n, v), hashes) =>
              val sortedHashes = hashes.toList.sortBy(_._1.toIdent)
              val hashDoc = Doc.intercalate(
                Doc.comma + Doc.line,
                sortedHashes.map { case (wh, msg) =>
                  val ident = wh.toIdent
                  msg match {
                    case Right(true)  => Doc.text(show"fetched $ident")
                    case Right(false) => Doc.text(show"cached $ident")
                    case Left(err) =>
                      Doc.text(show"failed: $ident ${err.getMessage}")
                  }
                }
              )

              Doc.text(show"$n $v:") + (Doc.line + hashDoc).nested(4).grouped
            }
          )

          val success = fs.forall { case (_, dl) =>
            dl.forall { case (_, res) => res.isRight }
          }
          if (success) moduleIOMonad.pure(resultDoc)
          else
            moduleIOMonad.raiseError(
              CliException("failed to fetch", err = resultDoc)
            )
        }
    }

    val initCommand =
      Opts.subcommand("init", "initialize a config") {
        (
          Opts.option[Name]("name", "name of the library"),
          Opts.option[String](
            "repo_uri",
            "uri for the version control of this library"
          ),
          Opts.option[P](
            "src_root",
            "path to the src root for the bosatsu packages in this library"
          ),
          Opts.option[Version]("version", "the initial version to use"),
          topLevelOpt
        ).mapN { (name, repoUri, rootDir, ver, repoRootF) =>
          val conf = LibConfig.init(name, repoUri, ver)
          val confJson = Json.Writer.write(conf)
          val writeJson =
            Output.JsonOutput(confJson, Some(confPath(rootDir, name)))

          repoRootF
            .flatMap { gitRoot =>
              val path = libsPath(gitRoot)
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

    val optName: Opts[Name] = Opts.option[Name](
      "name",
      help = "the name of the library to consider, if not set and there is" +
        "only one, use that.",
      "n"
    )

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
                } else {
                  moduleIOMonad.raiseError(
                    CliException.Basic(
                      show"more than one library, select one: ${libs.toMap.keys.toList.sorted}"
                    )
                  )
                }
              case Some(value) =>
                // make sure this name exists
                libs.get(value) match {
                  case Some(path) =>
                    platformIO.pathF(path).map((value, _))
                  case None =>
                    moduleIOMonad.raiseError(
                      CliException.Basic(
                        show"library $value not found. Select one of: ${libs.toMap.keys.toList.sorted}"
                      )
                    )
                }
            }
            (name, path) = namePath
          } yield (
            gitRoot,
            name,
            platformIO.resolve(
              platformIO.resolve(gitRoot, path),
              show"${name}_conf.json"
            )
          )
        }

    val gitShaOpt: Opts[F[String]] =
      Opts
        .option[String](
          "git_sha",
          help =
            "the git-sha to use for the library (default is `git rev-parse HEAD`)"
        )
        .map(moduleIOMonad.pure(_))
        .orElse(Opts(platformIO.gitShaHead))

    val assembleCommand =
      Opts.subcommand(
        "assemble",
        "construct a .bosatsu_lib from the configuration and .bosatsu_package files"
      ) {
        (
          rootAndName,
          Opts
            .options[P](
              "packages",
              help = "all the packages to include in this library.",
              "p"
            )
            .orEmpty,
          Opts
            .options[P]("dep", help = "dependency library", short = "d")
            .orEmpty,
          Opts
            .option[P](
              "output",
              help =
                "path to write the library to, or default name_{version}.bosatsu_lib",
              "o"
            )
            .orNone,
          Opts
            .option[P](
              "previous_lib",
              help =
                "if this is not the first version of the library this is the previous version."
            )
            .orNone,
          gitShaOpt
        )
          .mapN { (fpnp, packs, deps, optOut, prevLibPath, readGitSha) =>
            for {
              gitSha <- readGitSha
              pnp <- fpnp
              (gitRoot, name, confPath) = pnp
              conf <- readLibConf(name, confPath)
              outPath <- optOut match {
                case Some(p) => moduleIOMonad.pure(p)
                case None =>
                  platformIO.pathF(
                    show"${name}_${conf.nextVersion}.bosatsu_lib"
                  )
              }
              prevLib <- prevLibPath.traverse(platformIO.readLibrary(_))
              packages <- platformIO.readPackages(packs)
              depLibs <- deps.traverse(platformIO.readLibrary(_))
              maybeNewLib = conf.assemble(
                vcsIdent = gitSha,
                prevLib,
                packages,
                depLibs
              )
              lib <- moduleIOMonad.fromTry(LibConfig.Error.toTry(maybeNewLib))
            } yield (Output.Library(lib, outPath): Output[P])
          }
      }

    val casDirOpts: Opts[P => P] =
      Opts
        .option[P](
          "cas_dir",
          "the path to the cas/ directory, by default .bosatsuc/cas/ in git root"
        )
        .orNone
        .map {
          case None => { root =>
            platformIO.resolve(root, ".bosatsuc" :: "cas" :: Nil)
          }
          case Some(d) => { _ => d }
        }

    val fetchCommand =
      Opts.subcommand(
        "fetch",
        "download all transitive deps into the content storage."
      ) {
        (rootAndName, casDirOpts).mapN { (fpnp, casDirFn) =>
          for {
            pnp <- fpnp
            (gitRoot, name, confPath) = pnp
            conf <- readLibConf(name, confPath)
            casDir = casDirFn(gitRoot)
            msg <- fetchAllDeps(casDir, conf.publicDeps ::: conf.privateDeps)
          } yield (Output.Basic(msg, None): Output[P])
        }
      }

    MonoidK[Opts].combineAllK(
      initCommand ::
        listCommand ::
        assembleCommand ::
        fetchCommand ::
        Nil
    )
  }
}
