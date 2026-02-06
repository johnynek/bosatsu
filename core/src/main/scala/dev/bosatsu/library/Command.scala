package dev.bosatsu.library

import cats.{Monad, MonoidK}
import cats.arrow.FunctionK
import cats.data.{Chain, Ior, NonEmptyList}
import com.monovore.decline.{Argument, Opts}
import dev.bosatsu.tool.{
  CliException,
  CompilerApi,
  Output,
  PathGen,
  PackageResolver
}
import dev.bosatsu.codegen.Transpiler
import dev.bosatsu.codegen.clang.ClangTranspiler
import dev.bosatsu.Parser
import dev.bosatsu.hashing.Algo.WithAlgo.WithAlgoHashValue
import dev.bosatsu.hashing.{Algo, Hashed, HashValue}
import dev.bosatsu.LocationMap.Colorize
import dev.bosatsu.{Json, PackageName, PackageMap, PlatformIO}
import org.typelevel.paiges.Doc
import scala.collection.immutable.SortedMap

import _root_.bosatsu.{TypedAst => proto}

import cats.syntax.all._

object Command {
  sealed abstract class DepVisibility(val label: String) derives CanEqual
  object DepVisibility {
    case object Public extends DepVisibility("public")
    case object Private extends DepVisibility("private")
  }

  final case class DepInfo(
      name: String,
      version: Option[Version],
      hashes: List[String],
      uris: List[String],
      visibility: DepVisibility
  )

  private def descriptorDoc(desc: proto.LibDescriptor): Doc = {
    val versionStr = desc.version match {
      case Some(v) => Version.fromProto(v).render
      case None    => "unknown"
    }
    val versionDoc = Doc.text(s"version: $versionStr")
    val hashDoc =
      desc.hashes.toList match {
        case Nil =>
          Doc.text("hash: (none)")
        case h :: Nil =>
          Doc.text(s"hash: $h")
        case hs =>
          Doc.text("hashes:") + (Doc.line + Doc.intercalate(
            Doc.line,
            hs.map(Doc.text(_))
          )).nested(2)
      }
    val uriDoc =
      desc.uris.toList match {
        case Nil =>
          Doc.text("uri: (none)")
        case u :: Nil =>
          Doc.text(s"uri: $u")
        case us =>
          Doc.text("uris:") + (Doc.line + Doc.intercalate(
            Doc.line,
            us.map(Doc.text(_))
          )).nested(2)
      }

    Doc.intercalate(Doc.line, List(versionDoc, hashDoc, uriDoc))
  }

  private def depInfo(
      dep: proto.LibDependency,
      visibility: DepVisibility
  ): DepInfo = {
    val desc = dep.desc
    DepInfo(
      name = dep.name,
      version = desc.flatMap(_.version).map(Version.fromProto(_)),
      hashes = desc.toList.flatMap(_.hashes).toList,
      uris = desc.toList.flatMap(_.uris).toList,
      visibility = visibility
    )
  }

  private def depInfos(conf: LibConfig): List[DepInfo] =
    conf.publicDeps.map(depInfo(_, DepVisibility.Public)) :::
      conf.privateDeps.map(depInfo(_, DepVisibility.Private))

  private def updateDeps(
      conf: LibConfig,
      dep: proto.LibDependency,
      visibility: DepVisibility
  ): LibConfig = {
    val depName = dep.name
    val pubWithout = conf.publicDeps.filterNot(_.name == depName)
    val privWithout = conf.privateDeps.filterNot(_.name == depName)
    visibility match {
      case DepVisibility.Public =>
        conf.copy(publicDeps = pubWithout :+ dep, privateDeps = privWithout)
      case DepVisibility.Private =>
        conf.copy(publicDeps = pubWithout, privateDeps = privWithout :+ dep)
    }
  }

  def opts[F[_], P](platformIO: PlatformIO[F, P]): Opts[F[Output[P]]] = {
    import platformIO.{pathArg, moduleIOMonad, showPath, parallelF}

    implicit val hashArg: Argument[Algo.WithAlgo[HashValue]] =
      Parser.argFromParser(
        Algo.parseIdent,
        "hash",
        "hash",
        "Use format blake3:<hex>."
      )

    val topLevelOpt: Opts[F[P]] =
      Opts
        .option[P](
          "repo_root",
          "the path to the root of the repo, if not set, search for .git directory"
        )
        .orNone
        .map {
          case Some(value) => moduleIOMonad.pure(value)
          case None        =>
            platformIO.gitTopLevel
              .flatMap {
                case Some(value) => moduleIOMonad.pure(value)
                case None        =>
                  moduleIOMonad.raiseError(
                    CliException
                      .Basic("could not find .git directory in parents.")
                  )
              }
        }

    def libsPath(root: P): P = platformIO.resolve(root, "bosatsu_libs.json")

    def readJson[A: Json.Reader](path: P, onEmpty: => F[A]): F[A] =
      platformIO.fsDataType(path).flatMap {
        case None                             => onEmpty
        case Some(PlatformIO.FSDataType.File) =>
          platformIO
            .parseUtf8(path, Json.parserFile)
            .flatMap { json =>
              Json.Reader[A].read(Json.Path.Root, json) match {
                case Right(a)          => moduleIOMonad.pure(a)
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
              "path to the src root for the bosatsu packages in this library " +
                "(relative paths are interpreted relative to the repo root)"
          ),
          Opts.option[Version]("version", "the initial version to use"),
          topLevelOpt
        ).mapN { (name, repoUri, rootDir, ver, repoRootF) =>
          repoRootF
            .flatMap { gitRoot =>
              for {
                rootDirAbs = platformIO.resolve(gitRoot, rootDir)
                relDir <- platformIO.relativize(gitRoot, rootDirAbs) match {
                  case Some(value) => moduleIOMonad.pure(value)
                  case None        =>
                    moduleIOMonad.raiseError(
                      CliException.Basic(
                        show"$rootDirAbs is not a subdir of $gitRoot"
                      )
                    )
                }
                path = libsPath(gitRoot)
                lib0 <- readLibs(path)
                lib1 = lib0.updated(name, show"$relDir")
                out1 = Output.JsonOutput(Json.Writer.write(lib1), Some(path))
                conf = LibConfig.init(name, repoUri, ver)
                writeJson = confOutput(rootDirAbs, conf)
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
              case None    =>
                moduleIOMonad.raiseError[DecodedLibrary[Algo.Blake3]](
                  CliException(
                    "previous not in cas",
                    Doc.text("could not find previous version in CAS.") +
                      Doc.line +
                      (Doc.text("descriptor:") + (Doc.line + descriptorDoc(
                        desc
                      )).nested(2)).grouped +
                      Doc.line +
                      Doc.text("run `lib fetch` to download it.")
                  )
                )
            }
        }

      def previousPublicDeps(
          prevThis: Option[DecodedLibrary[Algo.Blake3]]
      ): F[List[DecodedLibrary[Algo.Blake3]]] =
        prevThis match {
          case None      => moduleIOMonad.pure(Nil)
          case Some(dec) =>
            val prevDeps = dec.protoLib.publicDependencies.toList
            if (prevDeps.isEmpty) moduleIOMonad.pure(Nil)
            else
              cas
                .depsFromCas(prevDeps, Nil)
                .map(_._1)
                .flatMap(_.traverse(DecodedLibrary.decode(_)))
        }

      def publicDepClosure(
          startLibs: List[DecodedLibrary[Algo.Blake3]]
      ): F[List[DecodedLibrary[Algo.Blake3]]] =
        publicDepClosureFromCas(cas, startLibs)

      private val inputRes =
        PackageResolver.LocalRoots[F, P](NonEmptyList.one(confDir), None)

      case class CheckState(
          prevThis: Option[DecodedLibrary[Algo.Blake3]],
          pubDecodes: List[DecodedLibrary[Algo.Blake3]],
          privDecodes: List[DecodedLibrary[Algo.Blake3]],
          publicDepClosureDecodes: List[DecodedLibrary[Algo.Blake3]],
          prevPublicDepDecodes: List[DecodedLibrary[Algo.Blake3]]
      ) {

        def packageMap(colorize: Colorize): F[PackageMap.Inferred] =
          PathGen
            .recursiveChildren(confDir, ".bosatsu")(platformIO)
            .read
            .flatMap { inputSrcs =>
              NonEmptyList.fromList(inputSrcs) match {
                case Some(inputNel) =>
                  platformIO
                    .withEC {
                      CompilerApi.typeCheck(
                        platformIO,
                        inputNel,
                        pubDecodes.flatMap(_.interfaces) ::: privDecodes
                          .flatMap(
                            _.interfaces
                          ),
                        colorize,
                        inputRes
                      )
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
          prevPublicDepDecodes <- previousPublicDeps(prevThis)
          pubPriv <- pubPrivDeps
          (pubDecodes, privDecodes) = pubPriv
          publicDepClosureDecodes <- publicDepClosure(
            pubDecodes ::: privDecodes
          )
        } yield CheckState(
          prevThis = prevThis,
          pubDecodes = pubDecodes,
          privDecodes = privDecodes,
          publicDepClosureDecodes = publicDepClosureDecodes,
          prevPublicDepDecodes = prevPublicDepDecodes
        )

      def check(colorize: Colorize): F[LibConfig.ValidationResult] =
        for {
          cs <- checkState
          allPacks <- cs.packageMap(colorize)
          validated = conf.validate(
            cs.prevThis,
            allPacks.toMap.values.toList,
            cs.pubDecodes ::: cs.privDecodes,
            cs.publicDepClosureDecodes,
            cs.prevPublicDepDecodes
          )
          res <- moduleIOMonad.fromTry(LibConfig.Error.toTry(validated))
        } yield res

      def build(
          colorize: Colorize,
          trans: Transpiler.Optioned[F, P]
      ): F[Doc] =
        for {
          cs <- checkState
          allPacks <- cs.packageMap(colorize)
          validated = conf.validate(
            cs.prevThis,
            allPacks.toMap.values.toList,
            cs.pubDecodes ::: cs.privDecodes,
            cs.publicDepClosureDecodes,
            cs.prevPublicDepDecodes
          )
          vr <- moduleIOMonad.fromTry(LibConfig.Error.toTry(validated))
          // we don't need a valid protoLib which will be published, just for resolving names/versions
          protoLib <- moduleIOMonad.fromEither(
            conf.unvalidatedAssemble(
              cs.prevThis,
              "",
              allPacks.toMap.values.toList,
              vr.unusedTransitiveDeps.iterator.map(_._2).toList
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
          allDeps =
            (cs.pubDecodes.iterator ++ cs.privDecodes.iterator).map { dec =>
              (dec.name.name, dec.version) -> dec.toHashed
            }.toMap

          loadFn = { (dep: proto.LibDependency) =>
            val version = dep.desc
              .flatMap(_.version)
              .fold(Version.zero)(Version.fromProto(_))
            allDeps.get((dep.name, version)) match {
              case Some(lib) => moduleIOMonad.pure(lib)
              case None      =>
                cas.libFromCas(dep).flatMap {
                  case Some(lib) => moduleIOMonad.pure(lib)
                  case None      =>
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
          outputs <- platformIO.withEC {
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
            deps = cs.pubDecodes ::: cs.privDecodes,
            publicDepClosureLibs = cs.publicDepClosureDecodes,
            prevPublicDepLibs = cs.prevPublicDepDecodes
          )
          res <- moduleIOMonad.fromTry(LibConfig.Error.toTry(validated))
        } yield res
    }

    def publicDepClosureFromCas(
        cas: Cas[F, P],
        pubDecodes: List[DecodedLibrary[Algo.Blake3]]
    ): F[List[DecodedLibrary[Algo.Blake3]]] = {
      type Key = (Name, Version)

      def publicDepsOf(
          lib: DecodedLibrary[Algo.Blake3]
      ): List[proto.LibDependency] =
        lib.protoLib.publicDependencies.toList :::
          lib.protoLib.unusedTransitivePublicDependencies.toList

      def depVersion(dep: proto.LibDependency): Either[Throwable, Version] =
        dep.desc.flatMap(_.version) match {
          case Some(v) => Right(Version.fromProto(v))
          case None    =>
            Left(
              CliException(
                "missing version",
                Doc.text(
                  show"public dependency ${dep.name} is missing a version."
                )
              )
            )
        }

      def loop(
          todo: List[proto.LibDependency],
          acc: Map[Key, DecodedLibrary[Algo.Blake3]]
      ): F[Map[Key, DecodedLibrary[Algo.Blake3]]] =
        todo match {
          case Nil         => moduleIOMonad.pure(acc)
          case dep :: rest =>
            moduleIOMonad.fromEither(depVersion(dep)).flatMap { v =>
              val key = (Name(dep.name), v)
              if (acc.contains(key)) loop(rest, acc)
              else
                cas.libFromCas(dep).flatMap {
                  case Some(lib) =>
                    DecodedLibrary.decode(lib).flatMap { dec =>
                      loop(publicDepsOf(dec) ::: rest, acc.updated(key, dec))
                    }
                  case None =>
                    moduleIOMonad.raiseError[Map[
                      Key,
                      DecodedLibrary[Algo.Blake3]
                    ]](
                      CliException(
                        "missing dep from cas",
                        Doc.text(
                          show"missing public dependency ${dep.name} $v in CAS, run `lib fetch`."
                        )
                      )
                    )
                }
            }
        }

      val initial = pubDecodes.iterator.map { dec =>
        ((dec.name, dec.version), dec)
      }.toMap
      val initialTodo = pubDecodes.flatMap(publicDepsOf)
      loop(initialTodo, initial).map(_.values.toList)
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

    val depsCommand = {
      val visibilityOpt: Opts[DepVisibility] =
        Opts
          .flag("public", help = "add a public dependency")
          .as(DepVisibility.Public)
          .orElse(
            Opts
              .flag("private", help = "add a private dependency (default)")
              .as(DepVisibility.Private)
          )
          .orElse(Opts(DepVisibility.Private))

      val depNameOpt =
        Opts.option[Name]("dep", help = "dependency library name")

      val depVersionOpt =
        Opts.option[Version]("version", help = "dependency version")

      val depHashesOpt =
        Opts
          .options[Algo.WithAlgo[HashValue]](
            "hash",
            help = "dependency hash (format blake3:<hex>, repeatable)"
          )
          .map(_.toList)

      val depUrisOpt =
        Opts
          .options[String]("uri", help = "dependency uri (repeatable)")
          .map(_.toList)

      val noFetchOpt =
        Opts
          .flag(
            "no-fetch",
            help = "only update the config file; skip fetching into the CAS"
          )
          .orFalse

      val addCommand =
        Opts.subcommand("add", "add a dependency to this library") {
          (
            ConfigConf.opts,
            depNameOpt,
            depVersionOpt,
            depHashesOpt,
            depUrisOpt,
            visibilityOpt,
            noFetchOpt
          ).mapN {
            (
                fcc,
                depName,
                depVersion,
                depHashes,
                depUris,
                visibility,
                noFetch
            ) =>
              for {
                cc <- fcc
                desc = proto.LibDescriptor(
                  version = Some(depVersion.toProto),
                  hashes = depHashes.map(_.toIdent),
                  uris = depUris
                )
                dep = proto.LibDependency(
                  name = depName.name,
                  desc = Some(desc)
                )
                conf1 = updateDeps(cc.conf, dep, visibility)
                out0 = confOutput(cc.confDir, conf1)
                out <-
                  if (noFetch) moduleIOMonad.pure(out0)
                  else
                    cc.cas.fetchAllDeps(dep :: Nil).map { doc =>
                      Output.many(out0, Output.Basic(doc, None))
                    }
              } yield (out: Output[P])
          }
        }

      val removeCommand =
        Opts.subcommand("remove", "remove a dependency from this library") {
          (ConfigConf.opts, depNameOpt).mapN { (fcc, depName) =>
            for {
              cc <- fcc
              nameStr = depName.name
              pubRemoved = cc.conf.publicDeps.filterNot(_.name == nameStr)
              privRemoved = cc.conf.privateDeps.filterNot(_.name == nameStr)
              _ <-
                if (
                  (pubRemoved.length == cc.conf.publicDeps.length) &&
                  (privRemoved.length == cc.conf.privateDeps.length)
                )
                  moduleIOMonad.raiseError[Unit](
                    CliException.Basic(
                      show"dependency $nameStr not found in public or private deps."
                    )
                  )
                else moduleIOMonad.unit
              conf1 = cc.conf.copy(
                publicDeps = pubRemoved,
                privateDeps = privRemoved
              )
              out = confOutput(cc.confDir, conf1)
            } yield (out: Output[P])
          }
        }

      val listCommand =
        Opts.subcommand("list", "list dependencies in the library config") {
          val jsonFlag =
            Opts.flag("json", help = "output dependencies as json").orFalse

          (ConfigConf.opts, jsonFlag).mapN { (fcc, jsonOut) =>
            for {
              cc <- fcc
              infos = depInfos(cc.conf)
              out =
                if (jsonOut) {
                  val depsJson = Json.JArray(
                    infos.map { info =>
                      val versionJson: Json = info.version match {
                        case Some(v) => Json.JString(v.render)
                        case None    => Json.JNull
                      }
                      Json.JObject(
                        ("name" -> Json.JString(info.name)) ::
                          ("version" -> versionJson) ::
                          ("hash" -> Json.JArray(
                            info.hashes.map(Json.JString(_)).toVector
                          )) ::
                          ("uri" -> Json.JArray(
                            info.uris.map(Json.JString(_)).toVector
                          )) ::
                          ("scope" -> Json.JString(info.visibility.label)) ::
                          Nil
                      )
                    }.toVector
                  )
                  Output.JsonOutput(
                    Json.JObject(("deps" -> depsJson) :: Nil),
                    None
                  )
                } else {
                  def formatList(
                      label: String,
                      items: List[DepInfo]
                  ): Option[Doc] =
                    if (items.isEmpty) None
                    else {
                      val sorted = items.sortBy(_.name)
                      val docs = sorted.map { info =>
                        val versionStr =
                          info.version.map(_.render).getOrElse("unknown")
                        val hashDoc =
                          info.hashes match {
                            case Nil =>
                              Doc.text("hash: (none)")
                            case h :: Nil =>
                              Doc.text(show"hash: $h")
                            case hs =>
                              Doc.text("hashes:") + (Doc.line + Doc.intercalate(
                                Doc.line,
                                hs.map(Doc.text(_))
                              )).nested(2)
                          }
                        val uriDoc =
                          info.uris match {
                            case Nil =>
                              Doc.text("uri: (none)")
                            case u :: Nil =>
                              Doc.text(show"uri: $u")
                            case us =>
                              Doc.text("uris:") + (Doc.line + Doc.intercalate(
                                Doc.line,
                                us.map(Doc.text(_))
                              )).nested(2)
                          }
                        val details = Doc.intercalate(
                          Doc.line,
                          List(
                            Doc.text(show"version: $versionStr"),
                            hashDoc,
                            uriDoc
                          )
                        )
                        Doc.text(info.name) + (Doc.line + details).nested(2)
                      }
                      Some(
                        Doc.text(s"$label deps:") + (Doc.line + Doc.intercalate(
                          Doc.hardLine,
                          docs
                        )).nested(2)
                      )
                    }

                  val (publicDeps, privateDeps) =
                    infos.partition(_.visibility == DepVisibility.Public)

                  val sections = List(
                    formatList("public", publicDeps),
                    formatList("private", privateDeps)
                  ).flatten

                  val doc =
                    if (sections.isEmpty)
                      Doc.text("no dependencies configured.")
                    else
                      Doc.intercalate(Doc.hardLine + Doc.hardLine, sections)

                  Output.Basic(doc, None)
                }
            } yield (out: Output[P])
          }
        }

      Opts.subcommand("deps", "manage dependencies for a library")(
        addCommand.orElse(removeCommand).orElse(listCommand)
      )
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
          casDirOpts,
          Opts
            .flag(
              "fetch-prev-deps",
              help =
                "fetch previous public dependencies from their URIs into the CAS if missing."
            )
            .orFalse,
          gitShaOpt
        )
          .mapN {
            (
                fpnp,
                packs,
                deps,
                optOut,
                prevLibPath,
                casDirFn,
                fetchPrevDeps,
                readGitSha
            ) =>
              for {
                gitSha <- readGitSha
                pnp <- fpnp
                (gitRoot, name, confPath) = pnp
                conf <- readLibConf(name, confPath)
                cas = new Cas(casDirFn(gitRoot), platformIO)
                outPath <- optOut match {
                  case Some(p) => moduleIOMonad.pure(p)
                  case None    =>
                    platformIO.pathF(libraryFileName(name, conf.nextVersion))
                }
                prevLib <- prevLibPath.traverse(platformIO.readLibrary(_))
                prevLibDec <- prevLib.traverse(DecodedLibrary.decode(_))
                packages <- platformIO.readPackages(packs)
                depLibs <- deps.traverse(platformIO.readLibrary(_))
                decLibs <- depLibs.traverse(DecodedLibrary.decode(_))
                depVersion = (d: proto.LibDependency) =>
                  d.desc.flatMap(_.version) match {
                    case None    => Version.zero
                    case Some(v) => Version.fromProto(v)
                  }
                depMap = decLibs.iterator.map { lib =>
                  ((lib.protoLib.name, lib.version), lib)
                }.toMap
                directDeps = conf.publicDeps ::: conf.privateDeps
                directDepDecodes = directDeps.flatMap { dep =>
                  depMap.get((dep.name, depVersion(dep)))
                }
                publicDepClosureLibs <-
                  if (directDepDecodes.isEmpty) moduleIOMonad.pure(Nil)
                  else publicDepClosureFromCas(cas, directDepDecodes)
                prevPublicDepLibs <- prevLibDec match {
                  case None          => moduleIOMonad.pure(Nil)
                  case Some(prevDec) =>
                    val prevDeps = prevDec.protoLib.publicDependencies.toList
                    if (prevDeps.isEmpty) moduleIOMonad.pure(Nil)
                    else {
                      def fromCas(
                          dep: proto.LibDependency
                      ): F[Option[DecodedLibrary[Algo.Blake3]]] =
                        cas
                          .libFromCas(dep)
                          .flatMap(_.traverse(DecodedLibrary.decode(_)))

                      val initial = prevDeps
                        .traverse { dep =>
                          depMap.get((dep.name, depVersion(dep))) match {
                            case Some(lib) => moduleIOMonad.pure(Some(lib))
                            case None      => fromCas(dep)
                          }
                        }

                      initial.flatMap { resolved =>
                        val missing =
                          prevDeps.zip(resolved).collect { case (dep, None) =>
                            dep
                          }

                        if (missing.isEmpty) {
                          moduleIOMonad.pure(resolved.flatten)
                        } else if (fetchPrevDeps) {
                          cas.fetchAllDeps(missing) *>
                            missing
                              .traverse(fromCas(_))
                              .flatMap { fetched =>
                                val stillMissing =
                                  missing.zip(fetched).collect {
                                    case (dep, None) => dep
                                  }
                                if (stillMissing.isEmpty)
                                  moduleIOMonad.pure(
                                    resolved.flatten ++ fetched.flatten
                                  )
                                else {
                                  val missStr = stillMissing
                                    .map(dep =>
                                      show"${dep.name} ${depVersion(dep)}"
                                    )
                                    .mkString(", ")
                                  moduleIOMonad.raiseError[List[
                                    DecodedLibrary[Algo.Blake3]
                                  ]](
                                    CliException.Basic(
                                      s"missing previous public deps from CAS after fetch; ensure URIs are valid. Missing: $missStr"
                                    )
                                  )
                                }
                              }
                        } else {
                          moduleIOMonad.raiseError[List[
                            DecodedLibrary[Algo.Blake3]
                          ]](
                            CliException.Basic(
                              "missing previous public deps from CAS; run `lib fetch`, pass --dep for these libraries, or use --fetch-prev-deps to download them."
                            )
                          )
                        }
                      }
                    }
                }
                maybeNewLib = conf.assemble(
                  vcsIdent = gitSha,
                  prevLibDec,
                  packages,
                  decLibs,
                  publicDepClosureLibs,
                  prevPublicDepLibs
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
              val dep =
                proto.LibDependency(name = cc.conf.name.name, desc = Some(desc))
              cc.cas.fetchIfNeeded(dep).flatMap { fetchMap =>
                val failed = fetchMap.collect { case (hash, Left(err)) =>
                  (hash, err)
                }
                if (failed.isEmpty) moduleIOMonad.unit
                else {
                  val versionStr = desc.version match {
                    case None    => "unknown"
                    case Some(v) => show"${Version.fromProto(v)}"
                  }
                  val header = Doc.text(
                    s"failed to fetch previous ${dep.name} $versionStr"
                  )
                  val detail = Doc.intercalate(
                    Doc.hardLine,
                    failed.toList.map { case (hash, err) =>
                      val errDoc = err match {
                        case ce: CliException => ce.errDoc
                        case other            =>
                          Doc.text(
                            Option(other.getMessage)
                              .getOrElse(other.getClass.getName)
                          )
                      }
                      Doc.text(show"${hash.toIdent}:") + (Doc.line + errDoc)
                        .nested(4)
                    }
                  )
                  val hint = Doc.text(
                    "ensure the previous descriptor has valid uris or pre-populate the CAS."
                  )
                  moduleIOMonad.raiseError[Unit](
                    CliException(
                      "failed to fetch previous",
                      header + Doc.line + detail + Doc.line + hint
                    )
                  )
                }
              }
            }
            prevPubDeps <- cc.conf.previous.traverse { desc =>
              val dep =
                proto.LibDependency(name = cc.conf.name.name, desc = Some(desc))
              cc.cas.libFromCas(dep).flatMap {
                case Some(lib) =>
                  moduleIOMonad.pure(lib.arg.publicDependencies.toList)
                case None =>
                  moduleIOMonad.raiseError[List[proto.LibDependency]](
                    CliException.Basic(
                      show"previous library ${dep.name} not found in CAS, run `lib fetch`."
                    )
                  )
              }
            }
            msg <- cc.cas.fetchAllDeps(
              cc.conf.publicDeps ::: cc.conf.privateDeps ::: prevPubDeps
                .getOrElse(Nil)
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

        val outFileOpt: Opts[P] =
          Opts.option[P](
            "output",
            help = "name of output c code file.",
            short = "o"
          )

        val outDirOrFileOpt: Opts[Ior[P, P]] =
          (Transpiler.outDir[P], outFileOpt.orNone).mapN {
            case (outDir, Some(outFile)) => Ior.both(outDir, outFile)
            case (outDir, None)          => Ior.left(outDir)
          }.orElse(outFileOpt.map(Ior.right(_)))

        val outputSpecOpt: Opts[(Option[P], ClangTranspiler.Output[F, P])] =
          (
            outDirOrFileOpt,
            Opts("output.c").mapValidated(platformIO.path(_)),
            (
              Opts.option[P](
                "exe_out",
                help = "if set, compile the c code to an executable",
                short = "e"
              ),
              ClangTranspiler.Output.ccConfOpt(platformIO)
            ).tupled.orNone
          ).mapN { (outDirOrFile, defaultOut, exeOut) =>
            outDirOrFile match {
              case Ior.Left(outDir)      =>
                (
                  Some(outDir),
                  ClangTranspiler.Output(
                    defaultOut,
                    cOutRelativeToOutDir = true,
                    exeOut = exeOut
                  )
                )
              case Ior.Both(outDir, out) =>
                (
                  Some(outDir),
                  ClangTranspiler.Output(
                    out,
                    cOutRelativeToOutDir = false,
                    exeOut = exeOut
                  )
                )
              case Ior.Right(out)        =>
                (
                  None,
                  ClangTranspiler.Output(
                    out,
                    cOutRelativeToOutDir = false,
                    exeOut = exeOut
                  )
                )
            }
          }

        val buildArgs =
          (
            ConfigConf.opts,
            mainPack,
            outputSpecOpt,
            ClangTranspiler.EmitMode.opts,
            ClangTranspiler.GenExternalsMode.opts
          ).tupled

        (buildArgs, Colorize.optsConsoleDefault).mapN {
          case ((fcc, mainPackOpt, (outDirOpt, output), emit, gen), colorize) =>
            def mode(cc: ConfigConf): F[ClangTranspiler.Mode[F]] =
              mainPackOpt match {
                case Some(m) =>
                  moduleIOMonad.pure(ClangTranspiler.Mode.Main(moduleIOMonad.pure(m)))
                case None    =>
                  cc.conf.defaultMain match {
                    case Some(m) =>
                      moduleIOMonad.pure(
                        ClangTranspiler.Mode.Main(moduleIOMonad.pure(m))
                      )
                    case None    =>
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

            def useOutDir(outDir: P): F[Output[P]] =
              for {
                cc <- fcc
                m <- mode(cc)
                trans = Transpiler.optioned(ClangTranspiler) {
                  ClangTranspiler.Arguments(
                    m,
                    emit,
                    gen,
                    output,
                    outDir,
                    platformIO
                  )
                }
                msg <- cc.build(colorize, trans)
              } yield (Output.Basic(msg, None): Output[P])

            outDirOpt match {
              case Some(outDir) => useOutDir(outDir)
              case None         =>
                platformIO.withTempPrefix("build_outdir")(useOutDir)
            }
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
            ClangTranspiler.Output(o, cOutRelativeToOutDir = true, Some((e, conf)))
          }

        val testArgs =
          (
            ConfigConf.opts,
            // we want to run the test after generating it
            ClangTranspiler.Mode.testOpts[F](executeOpts = Opts(true)),
            clangOut,
            ClangTranspiler.EmitMode.opts,
            ClangTranspiler.GenExternalsMode.opts,
            Transpiler.outDir[P].orNone
          ).tupled

        (testArgs, Colorize.optsConsoleDefault).mapN {
          case ((fcc, test, out, emit, gen, outDirOpt), colorize) =>
            def useOutDir(outDir: P): F[Output[P]] = {
              val trans = Transpiler.optioned(ClangTranspiler) {
                ClangTranspiler.Arguments(
                  test,
                  emit,
                  gen,
                  out,
                  outDir,
                  platformIO
                )
              }
              for {
                cc <- fcc
                // build is the same as test, Transpiler controls the difference
                msg <- cc.build(colorize, trans)
              } yield (Output.Basic(msg, None): Output[P])
            }

            outDirOpt match {
              case Some(outDir) => useOutDir(outDir)
              case None         =>
                platformIO.withTempPrefix("test_outdir")(useOutDir)
            }
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
                case None          => Nil
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
        depsCommand ::
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
    ): SortedMap[
      Algo.WithAlgo[HashValue],
      Algo.WithAlgo[[A] =>> Hashed[A, P]]
    ] =
      hashes(dep)
        .map { withAlgo =>
          val path = hashPath(withAlgo)
          (
            withAlgo,
            withAlgo.mapK(new FunctionK[HashValue, [A] =>> Hashed[A, P]] {
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
            case true  => Monad[F].pure(Right(false)).widen[DownloadRes]
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
                  case Left(err)    =>
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
