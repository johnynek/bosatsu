package org.bykn.bosatsu.library

import cats.{Monad, MonoidK}
import cats.arrow.FunctionK
import cats.data.{Chain, NonEmptyList}
import com.monovore.decline.Opts
import org.bykn.bosatsu.tool.{
  CliException,
  CompilerApi,
  Output,
  PathGen,
  PackageResolver
}
import org.bykn.bosatsu.codegen.Transpiler
import org.bykn.bosatsu.codegen.clang.ClangTranspiler
import org.bykn.bosatsu.hashing.{Algo, Hashed, HashValue}
import org.bykn.bosatsu.LocationMap.Colorize
import org.bykn.bosatsu.{Json, PackageName, PackageMap, PlatformIO}
import org.typelevel.paiges.Doc
import scala.collection.immutable.SortedMap

import _root_.bosatsu.{TypedAst => proto}

import cats.syntax.all._

object Command {
  def opts[F[_], P](platformIO: PlatformIO[F, P]): Opts[F[Output[P]]] = {
    import platformIO.{pathArg, moduleIOMonad, showPath, parallelF}

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

    def confOutput(rootDir: P, conf: LibConfig): Output[P] = {
      val confJson = Json.Writer.write(conf)
      Output.JsonOutput(confJson, Some(confPath(rootDir, conf.name)))
    }

    val initCommand =
      Opts.subcommand("init", "initialize a config") {
        (
          Opts.option[Name]("name", help = "name of the library"),
          Opts.option[String](
            "repo_uri",
            help = "uri for the version control of this library"
          ),
          Opts.option[P](
            "src_root",
            help =
              "path to the src root for the bosatsu packages in this library"
          ),
          Opts.option[Version]("version", "the initial version to use"),
          topLevelOpt
        ).mapN { (name, repoUri, rootDir, ver, repoRootF) =>
          val conf = LibConfig.init(name, repoUri, ver)
          val writeJson = confOutput(rootDir, conf)

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
    val rootNameRoot: Opts[F[(P, Name, P)]] =
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
                val libSize = libs.toMap.size
                if (libSize == 1) {
                  val (name, pathStr) = libs.toMap.head
                  platformIO.pathF(pathStr).map((name, _))
                } else {
                  val msg =
                    if (libSize == 0) {
                      "no libraries configured. Run `lib init`"
                    } else {
                      show"more than one library, select one: ${libs.toMap.keys.toList.sorted.mkString(", ")}"
                    }

                  moduleIOMonad.raiseError(CliException.Basic(msg))
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
            platformIO.resolve(gitRoot, path)
          )
        }

    def readAllLibs(gitRoot: P): F[SortedMap[Name, (LibConfig, P)]] =
      for {
        libs <- readLibs(libsPath(gitRoot))
        configs <- libs.toMap.transform { (name, path) =>
          val confDir = platformIO.resolve(gitRoot, path)
          readLibConf(name, confPath(confDir, name)).map { conf =>
            (conf, confDir)
          }
        }.sequence
      } yield configs

    val allLibs: Opts[F[(P, SortedMap[Name, (LibConfig, P)])]] =
      topLevelOpt
        .map { gitRootF =>
          for {
            gitRoot <- gitRootF
            configs <- readAllLibs(gitRoot)
          } yield (gitRoot, configs)
        }

    val rootAndName: Opts[F[(P, Name, P)]] =
      cats.Functor[Opts].compose[F].map(rootNameRoot) {
        case (gitRoot, name, path) =>
          (gitRoot, name, confPath(path, name))
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

    case class ConfigConf(conf: LibConfig, cas: Cas[F, P], confDir: P) {

      def pubPrivDeps: F[
        (List[DecodedLibrary[Algo.Blake3]], List[DecodedLibrary[Algo.Blake3]])
      ] =
        for {
          pubPriv <- cas.depsFromCas(conf.publicDeps, conf.privateDeps)
          (pubLibs, privLibs) = pubPriv
          pubDecodes <- pubLibs.traverse(DecodedLibrary.decode(_))
          privDecodes <- privLibs.traverse(DecodedLibrary.decode(_))
        } yield (pubDecodes, privDecodes)

      def previousThis: F[Option[DecodedLibrary[Algo.Blake3]]] =
        conf.previous.traverse { desc =>
          cas
            .libFromCas(
              proto.LibDependency(name = conf.name.name, desc = Some(desc))
            )
            .flatMap {
              case Some(a) => DecodedLibrary.decode(a)
              case None =>
                moduleIOMonad.raiseError[DecodedLibrary[Algo.Blake3]](
                  CliException(
                    "previous not in cas",
                    Doc.text(
                      s"could not find previous version ($desc) in CAS, run `lib fetch`."
                    )
                  )
                )
            }
        }

      private val inputRes =
        PackageResolver.LocalRoots[F, P](NonEmptyList.one(confDir), None)

      case class CheckState(
          prevThis: Option[DecodedLibrary[Algo.Blake3]],
          pubDecodes: List[DecodedLibrary[Algo.Blake3]],
          privDecodes: List[DecodedLibrary[Algo.Blake3]]
      ) {

        def packageMap(colorize: Colorize): F[PackageMap.Inferred] =
          PathGen
            .recursiveChildren(confDir, ".bosatsu")(platformIO)
            .read
            .flatMap { inputSrcs =>
              NonEmptyList.fromList(inputSrcs) match {
                case Some(inputNel) =>
                  platformIO
                    .withEC { ec =>
                      CompilerApi.typeCheck(
                        platformIO,
                        inputNel,
                        pubDecodes.flatMap(_.interfaces) ::: privDecodes
                          .flatMap(
                            _.interfaces
                          ),
                        colorize,
                        inputRes
                      )(ec)
                    }
                    .map(_._1)
                case None =>
                  moduleIOMonad.pure(PackageMap.empty)
              }
            }
      }

      def checkState: F[CheckState] =
        for {
          prevThis <- previousThis
          pubPriv <- pubPrivDeps
          (pubDecodes, privDecodes) = pubPriv
        } yield CheckState(
          prevThis = prevThis,
          pubDecodes = pubDecodes,
          privDecodes = privDecodes
        )

      def check(colorize: Colorize): F[LibConfig.ValidationResult] =
        for {
          cs <- checkState
          allPacks <- cs.packageMap(colorize)
          validated = conf.validate(
            cs.prevThis,
            allPacks.toMap.values.toList,
            cs.pubDecodes ::: cs.privDecodes
          )
          res <- moduleIOMonad.fromTry(LibConfig.Error.toTry(validated))
        } yield res

      def build(
          colorize: Colorize,
          trans: Transpiler.Optioned[F, P]
      ): F[Doc] =
        for {
          pubPriv <- pubPrivDeps
          (pubDecodes, privDecodes) = pubPriv
          cs = CheckState(
            None,
            pubDecodes = pubDecodes,
            privDecodes = privDecodes
          )
          allPacks <- cs.packageMap(colorize)
          // we don't need a valid protoLib which will be published, just for resolving names/versions
          protoLib <- moduleIOMonad.fromEither(
            conf.unvalidatedAssemble(
              None,
              "",
              allPacks.toMap.values.toList,
              Nil
            )
          )
          hashedLib = Hashed.viaBytes[Algo.Blake3, proto.Library](protoLib)(
            _.toByteArray
          )
          // TODO: it's wasteful to serialize, just do
          decLib = DecodedLibrary(
            conf.name,
            conf.nextVersion,
            hashedLib.hash,
            protoLib,
            Nil, // we can ignore interfaces when generating binaries
            allPacks
          )
          allDeps = (pubDecodes.iterator ++ privDecodes.iterator).map { dec =>
            (dec.name.name, dec.version) -> dec.toHashed
          }.toMap

          loadFn = { (dep: proto.LibDependency) =>
            val version = dep.desc
              .flatMap(_.version)
              .fold(Version.zero)(Version.fromProto(_))
            allDeps.get((dep.name, version)) match {
              case Some(lib) => moduleIOMonad.pure(lib)
              case None =>
                cas.libFromCas(dep).flatMap {
                  case Some(lib) => moduleIOMonad.pure(lib)
                  case None =>
                    moduleIOMonad
                      .raiseError[Hashed[Algo.Blake3, proto.Library]](
                        CliException(
                          "missing dep from cas",
                          Doc.text(
                            s"missing ${dep.name} $version"
                          ) + Doc.line + Doc.text(
                            "run `lib fetch` to insert these libraries into the cas."
                          )
                        )
                      )
                }
            }
          }
          decWithLibs <- DecodedLibraryWithDeps.decodeAll(decLib)(loadFn)
          outputs <- platformIO.withEC { implicit ec =>
            trans.renderAll(decWithLibs)
          }
          _ <- outputs.traverse_ { case (path, doc) =>
            platformIO.writeDoc(path, doc)
          }
        } yield Doc.empty

      def buildLibrary(vcsIdent: String, colorize: Colorize): F[proto.Library] =
        for {
          cs <- checkState
          allPacks <- cs.packageMap(colorize)
          validated = conf.assemble(
            vcsIdent = vcsIdent,
            previous = cs.prevThis,
            packs = allPacks.toMap.values.toList,
            deps = cs.pubDecodes ::: cs.privDecodes
          )
          res <- moduleIOMonad.fromTry(LibConfig.Error.toTry(validated))
        } yield res
    }

    object ConfigConf {
      val opts: Opts[F[ConfigConf]] =
        (rootNameRoot, casDirOpts).mapN { (fpnp, casDirFn) =>
          for {
            pnp <- fpnp
            (gitRoot, name, confDir) = pnp
            conf <- readLibConf(name, confPath(confDir, name))
            casDir = casDirFn(gitRoot)
            cas = new Cas(casDir, platformIO)
          } yield ConfigConf(conf, cas, confDir)
        }
    }

    def libraryFileName(name: Name, version: Version): String =
      show"${name}-v${version}.bosatsu_lib"

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
                "path to write the library to, or default {name}-v{version}.bosatsu_lib",
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
                  platformIO.pathF(libraryFileName(name, conf.nextVersion))
              }
              prevLib <- prevLibPath.traverse(platformIO.readLibrary(_))
              prevLibDec <- prevLib.traverse(DecodedLibrary.decode(_))
              packages <- platformIO.readPackages(packs)
              depLibs <- deps.traverse(platformIO.readLibrary(_))
              decLibs <- depLibs.traverse(DecodedLibrary.decode(_))
              maybeNewLib = conf.assemble(
                vcsIdent = gitSha,
                prevLibDec,
                packages,
                decLibs
              )
              lib <- moduleIOMonad.fromTry(LibConfig.Error.toTry(maybeNewLib))
            } yield (Output.Library(lib, outPath): Output[P])
          }
      }

    val fetchCommand =
      Opts.subcommand(
        "fetch",
        "download all transitive deps into the content storage."
      ) {
        ConfigConf.opts.map { fcc =>
          for {
            cc <- fcc
            _ <- cc.conf.previous.traverse_ { desc =>
              cc.cas.fetchIfNeeded(
                proto.LibDependency(name = cc.conf.name.name, desc = Some(desc))
              )
            }
            msg <- cc.cas.fetchAllDeps(
              cc.conf.publicDeps ::: cc.conf.privateDeps
            )
          } yield (Output.Basic(msg, None): Output[P])
        }
      }

    val checkCommand =
      Opts.subcommand(
        "check",
        "check all the code, but do not build the final output library (faster than build)."
      ) {
        (ConfigConf.opts, Colorize.optsConsoleDefault).mapN { (fcc, colorize) =>
          for {
            cc <- fcc
            _ <- cc.check(colorize)
            msg = Doc.text("")
          } yield (Output.Basic(msg, None): Output[P])
        }
      }

    val buildCommand =
      Opts.subcommand(
        "build",
        "build an executable for a library"
      ) {
        val mainPack = Opts
          .option[PackageName](
            "main_pack",
            help = "package to use to define the main.",
            "m"
          )
          .orNone

        (
          ClangTranspiler.justOptsGivenMode(
            ConfigConf.opts.product(mainPack),
            platformIO
          ) {
            case (_, Some(m)) =>
              ClangTranspiler.Mode.Main(moduleIOMonad.pure(m))
            case (fcc, None) =>
              ClangTranspiler.Mode.Main(
                fcc.flatMap { cc =>
                  cc.conf.defaultMain match {
                    case Some(m) => moduleIOMonad.pure(m)
                    case None =>
                      moduleIOMonad.raiseError(
                        CliException(
                          "no main defined",
                          Doc.text(
                            s"no argument (--main_pack, -m) given to define main package and none found in ${cc.conf.name.name}"
                          )
                        )
                      )
                  }
                }
              )
          },
          Colorize.optsConsoleDefault
        ).mapN { case (((fcc, _), trans), colorize) =>
          for {
            cc <- fcc
            msg <- cc.build(colorize, trans)
          } yield (Output.Basic(msg, None): Output[P])
        }
      }

    val testCommand =
      Opts.subcommand(
        "test",
        "test packages in a library"
      ) {
        val clangOut: Opts[ClangTranspiler.Output[F, P]] =
          (
            Opts("test.c").mapValidated(platformIO.path(_)),
            Opts("test").mapValidated(platformIO.path(_)),
            ClangTranspiler.Output.ccConfOpt(platformIO)
          ).mapN { (o, e, conf) =>
            ClangTranspiler.Output(o, Some((e, conf)))
          }

        (
          ClangTranspiler.justOptsGivenModeOutput(
            (
              ConfigConf.opts,
              // we want to run the test after generating it
              ClangTranspiler.Mode.testOpts[F](executeOpts = Opts(true)),
              clangOut
            ).tupled,
            platformIO
          ) { case (_, test, out) => (test, out) },
          Colorize.optsConsoleDefault
        ).mapN { case (((fcc, _, _), trans), colorize) =>
          for {
            cc <- fcc
            // build is the same as test, Transpiler controls the difference
            msg <- cc.build(colorize, trans)
          } yield (Output.Basic(msg, None): Output[P])
        }
      }

    def libraryPath(outDir: P, name: Name, version: Version): P =
      platformIO.resolve(outDir, libraryFileName(name, version))

    def toDesc(
        hashedLib: Hashed[Algo.Blake3, proto.Library],
        uris: List[String]
    ): proto.LibDescriptor =
      proto.LibDescriptor(
        version = hashedLib.arg.descriptor.flatMap(_.version),
        hashes = hashedLib.hash.toIdent :: Nil,
        uris = uris
      )

    val publishCommand =
      Opts.subcommand(
        "publish",
        "publish all the libraries into binary files"
      ) {
        (
          allLibs,
          casDirOpts,
          Colorize.optsConsoleDefault,
          gitShaOpt,
          Transpiler.outDir[P],
          Opts
            .option[String](
              long = "uri-base",
              short = "u",
              help = "uri prefix where all the libraries will be accessible."
            )
            .orNone
        ).mapN { (readGitLibs, casDirFn, colorize, gitShaF, outDir, uriBaseOpt) =>
          for {
            gitRootlibs <- readGitLibs
            gitSha <- gitShaF
            (gitRoot, libs) = gitRootlibs
            casDir = casDirFn(gitRoot)
            cas = new Cas(casDir, platformIO)
            allLibs <- libs.transform { case (name, (conf, path)) =>
              val cc = ConfigConf(conf, cas, path)
              val libOut: P = libraryPath(outDir, name, conf.nextVersion)
              for {
                protoLib <- cc.buildLibrary(vcsIdent = gitSha, colorize)
                hashedLib = Hashed(
                  Algo[Algo.Blake3].hashBytes(protoLib.toByteArray),
                  protoLib
                )
                _ <- cas.putIfAbsent(hashedLib)
              } yield (hashedLib, libOut, cc)
            }.parSequence
            // if we get here, we have successfully built all the libraries, now update the libconfig
            // and mutate those
            confOuts = allLibs.values.iterator.map { case (hashedLib, _, cc) =>
              val uris = uriBaseOpt match {
                case None => Nil
                case Some(uriBase) =>
                  val uriBase1 =
                    if (uriBase.endsWith("/")) uriBase else s"${uriBase}/"
                  val uri = uriBase1 + libraryFileName(
                    cc.conf.name,
                    cc.conf.nextVersion
                  )

                  uri :: Nil
              }
              val conf1 = cc.conf.copy(
                previous = Some(toDesc(hashedLib, uris)),
                nextVersion = cc.conf.nextVersion.nextPatch
              )

              confOutput(cc.confDir, conf1)
            }
            out = Output.Many(
              Chain.fromIterableOnce(
                allLibs.iterator.map { case (_, (lib, path, _)) =>
                  Output.Library(lib.arg, path)
                } ++
                  confOuts
              )
            )
          } yield (out: Output[P])
        }
      }

    MonoidK[Opts].combineAllK(
      initCommand ::
        listCommand ::
        assembleCommand ::
        fetchCommand ::
        checkCommand ::
        buildCommand ::
        testCommand ::
        publishCommand ::
        Nil
    )
  }

  class Cas[F[_], P](casDir: P, platformIO: PlatformIO[F, P]) {
    import platformIO.moduleIOMonad
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

    def hashPath(withAlgo: Algo.WithAlgo[HashValue]): P = {
      val algoName = withAlgo.algo.name
      val hex1 = withAlgo.value.hex.take(2)
      val hex2 = withAlgo.value.hex.drop(2)

      platformIO.resolve(casDir, algoName :: hex1 :: hex2 :: Nil)
    }

    def casPaths(
        dep: proto.LibDependency
    ): SortedMap[Algo.WithAlgo[HashValue], Algo.WithAlgo[Lambda[
      A => Hashed[A, P]
    ]]] =
      hashes(dep)
        .map { withAlgo =>
          val path = hashPath(withAlgo)
          (
            withAlgo,
            withAlgo.mapK(new FunctionK[HashValue, Lambda[A => Hashed[A, P]]] {
              def apply[A](fn: HashValue[A]) = Hashed(fn, path)
            })
          )
        }
        .to(SortedMap)

    def libFromCas(
        dep: proto.LibDependency
    ): F[Option[Hashed[Algo.Blake3, proto.Library]]] =
      casPaths(dep).values.toList.collectFirstSomeM { hashed =>
        val path = hashed.value.arg
        platformIO.fileExists(path).flatMap {
          case true  => platformIO.readLibrary(path).map(h => Some(h))
          case false => Monad[F].pure(None)
        }
      }

    def depsFromCas(
        pubDeps: List[proto.LibDependency],
        privDeps: List[proto.LibDependency]
    ): F[
      (
          List[Hashed[Algo.Blake3, proto.Library]],
          List[Hashed[Algo.Blake3, proto.Library]]
      )
    ] =
      (
        pubDeps.parTraverse(dep => libFromCas(dep).map(dep -> _)),
        privDeps.parTraverse(dep => libFromCas(dep).map(dep -> _))
      ).parTupled
        .flatMap { case (pubLibs, privLibs) =>
          val missingPubs = pubLibs.collect { case (dep, None) => dep }
          val missingPrivs = privLibs.collect { case (dep, None) => dep }

          if (missingPubs.isEmpty && missingPrivs.isEmpty) {
            moduleIOMonad.pure(
              (
                pubLibs.collect { case (_, Some(lib)) => lib },
                privLibs.collect { case (_, Some(lib)) => lib }
              )
            )
          } else {
            // report the missing libraries and suggest running fetch
            val pubDoc = Doc.text("public dependencies:") + (
              Doc.line + Doc.intercalate(
                Doc.comma + Doc.line,
                missingPubs.map(dep => Doc.text(dep.name))
              )
            ).nested(4).grouped

            val privDoc = Doc.text("private dependencies:") + (
              Doc.line + Doc.intercalate(
                Doc.comma + Doc.line,
                missingPrivs.map(dep => Doc.text(dep.name))
              )
            ).nested(4).grouped

            moduleIOMonad
              .raiseError(
                CliException(
                  "missing deps from cas",
                  Doc.text(
                    "missing "
                  ) + pubDoc + Doc.line + privDoc + Doc.line + Doc.text(
                    "run `lib fetch` to insert these libraries into the cas."
                  )
                )
              )
          }
        }

    def depUris(dep: proto.LibDependency): List[String] =
      dep.desc.toList.flatMap(_.uris)

    def versionOf(dep: proto.LibDependency): Version =
      dep.desc.flatMap(_.version) match {
        case None    => Version.zero
        case Some(v) => Version.fromProto(v)
      }

    def putIfAbsent(lib: Hashed[Algo.Blake3, proto.Library]): F[Unit] = {
      val path = hashPath(Algo.WithAlgo(lib.hash))

      platformIO
        .fileExists(path)
        .flatMap {
          case true  => moduleIOMonad.unit
          case false => platformIO.writeLibrary(lib.arg, path)
        }
    }

    def fetchIfNeeded(
        dep: proto.LibDependency
    ): F[SortedMap[Algo.WithAlgo[HashValue], DownloadRes]] = {
      val paths = casPaths(dep)
      val uris = depUris(dep)

      paths.transform { (hashValue, hashed) =>
        val path = hashed.value.arg
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

    def fetchAllDeps(deps: List[proto.LibDependency]): F[Doc] = {

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
                  fetchedLibsOpt.flatMap {
                    case None      => Nil
                    case Some(dep) =>
                      // we will find the transitivies by walking them
                      (dep.arg.publicDependencies.toList ::: dep.arg.privateDependencies.toList)
                        .filterNot { dep =>
                          nextFetched.contains((dep.name, versionOf(dep)))
                        }
                  }

                }

            nextBatchF.map((nextFetched, _))
          }

      def showFetchState(fs: FetchState): F[Doc] = {
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

      moduleIOMonad
        .tailRecM((SortedMap.empty: FetchState, deps)) { case (fetched, deps) =>
          step(fetched, deps).map {
            case (state, Nil) => Right(state)
            case next         => Left(next)
          }
        }
        .flatMap(showFetchState(_))
    }
  }
}
