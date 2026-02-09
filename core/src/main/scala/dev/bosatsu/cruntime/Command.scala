package dev.bosatsu.cruntime

import cats.syntax.all._
import com.monovore.decline.{Argument, Opts}
import dev.bosatsu.{BuildInfo, Parser, PlatformIO}
import dev.bosatsu.hashing.{Algo, HashValue}
import dev.bosatsu.tool.{CliException, Output}
import org.typelevel.paiges.Doc

object Command {
  private val defaultRepo = "johnynek/bosatsu"

  def opts[F[_], P](platformIO: PlatformIO[F, P]): Opts[F[Output[P]]] = {
    import platformIO.{moduleIOMonad, pathArg, showPath}

    implicit val hashArg: Argument[Algo.WithAlgo[HashValue]] =
      Parser.argFromParser(
        Algo.parseIdent,
        "hash",
        "hash",
        "Use format blake3:<hex>."
      )

    val repoRootOpt: Opts[F[P]] =
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

    val gitShaOpt: Opts[F[String]] =
      Opts
        .option[String](
          "git_sha",
          help =
            "the git-sha to install (default is the git-sha the compiler was built at)"
        )
        .map(moduleIOMonad.pure(_))
        .orElse(
          Opts(
            platformIO.getOrError(
              BuildInfo.gitHeadCommit,
              s"compiler version ${BuildInfo.version} was built without a git-sha; pass --git_sha"
            )
          )
        )

    val archiveOpt: Opts[Option[P]] =
      Opts
        .option[P](
          "archive",
          help = "path to a local c_runtime archive (tar.gz, tgz, tar, or zip)"
        )
        .orNone

    val urlOpt: Opts[Option[String]] =
      Opts
        .option[String](
          "url",
          help =
            "URL to download a c_runtime archive (must match --hash or built-in hash)"
        )
        .orNone

    val hashOpt: Opts[Option[Algo.WithAlgo[HashValue]]] =
      Opts
        .option[Algo.WithAlgo[HashValue]](
          "hash",
          help =
            "expected archive hash (format blake3:<hex>) when downloading from a URL"
        )
        .orNone

    val profileOpt: Opts[String] =
      Opts
        .option[String](
          "profile",
          help = "c runtime build profile: release or debug"
        )
        .orNone
        .map(_.getOrElse("release"))

    def parseBuildHash: F[Option[Algo.WithAlgo[HashValue]]] =
      if (BuildInfo.cRuntimeArchiveHash.isEmpty)
        moduleIOMonad.pure(None)
      else
        Algo.parseIdent.parseAll(BuildInfo.cRuntimeArchiveHash) match {
          case Right(value) => moduleIOMonad.pure(Some(value))
          case Left(_)      =>
            moduleIOMonad.raiseError(
              CliException.Basic(
                s"invalid BOSATSU_C_RUNTIME_HASH in build: ${BuildInfo.cRuntimeArchiveHash}"
              )
            )
        }

    def defaultArchiveUrl(gitSha: String): String =
      s"https://github.com/$defaultRepo/releases/download/v${BuildInfo.version}/bosatsu-c-runtime-${gitSha}.tar.gz"

    def archivePathForUrl(root: P, url: String, gitSha: String): P = {
      val fileName =
        url
          .split('/')
          .lastOption
          .filter(_.nonEmpty)
          .getOrElse(s"bosatsu-c-runtime-${gitSha}.tar.gz")
      platformIO.resolve(
        root,
        ".bosatsuc" :: "c_runtime" :: "archive" :: fileName :: Nil
      )
    }

    def requireFile(path: P): F[P] =
      platformIO.fsDataType(path).flatMap {
        case Some(PlatformIO.FSDataType.File) => moduleIOMonad.pure(path)
        case _                                =>
          moduleIOMonad.raiseError(
            CliException.Basic(show"expected archive file at $path")
          )
      }

    def runtimeRootOpt(base: P): F[Option[P]] = {
      val direct = platformIO.resolve(base, "Makefile")
      val nestedRoot = platformIO.resolve(base, "c_runtime")
      val nested = platformIO.resolve(nestedRoot, "Makefile")
      val isFile = (p: P) =>
        platformIO.fsDataType(p).map {
          case Some(PlatformIO.FSDataType.File) => true
          case _                                => false
        }

      (isFile(direct), isFile(nested)).mapN {
        case (true, _) => Some(base)
        case (_, true) => Some(nestedRoot)
        case _         => None
      }
    }

    def extractArchive(archive: P, dest: P): F[Unit] = {
      val archiveStr = platformIO.pathToString(archive)
      val destStr = platformIO.pathToString(dest)

      val extractCmd =
        if (archiveStr.endsWith(".zip"))
          ("unzip", List("-q", archiveStr, "-d", destStr))
        else if (archiveStr.endsWith(".tar.gz") || archiveStr.endsWith(".tgz"))
          ("tar", List("-xzf", archiveStr, "-C", destStr))
        else if (archiveStr.endsWith(".tar"))
          ("tar", List("-xf", archiveStr, "-C", destStr))
        else
          ("", Nil)

      extractCmd match {
        case ("", _) =>
          moduleIOMonad.raiseError(
            CliException.Basic(
              s"unsupported archive type: $archiveStr (expected tar.gz, tgz, tar, or zip)"
            )
          )
        case (cmd, args) =>
          platformIO.system("mkdir", List("-p", destStr)) *>
            platformIO.system(cmd, args)
      }
    }

    def ensureRuntimeRoot(base: P, archive: P): F[P] =
      runtimeRootOpt(base).flatMap {
        case Some(root) => moduleIOMonad.pure(root)
        case None       =>
          extractArchive(archive, base) *> runtimeRootOpt(base).flatMap {
            case Some(root) => moduleIOMonad.pure(root)
            case None       =>
              moduleIOMonad.raiseError(
                CliException.Basic(
                  show"expected c_runtime sources in $base after extracting $archive"
                )
              )
          }
      }

    val install: Opts[F[Output[P]]] =
      Opts
        .subcommand(
          "install",
          "install the bosatsu c runtime (Makefile honors CC/CFLAGS/CPPFLAGS/LDFLAGS/LIBS and generates cc_conf.json from them)"
        ) {
          (
            repoRootOpt,
            gitShaOpt,
            archiveOpt,
            urlOpt,
            hashOpt,
            profileOpt
          ).mapN { (rootF, shaF, archiveOpt0, urlOpt0, hashOpt0, profile) =>
            for {
              root <- rootF
              gitSha <- shaF
              buildHashOpt <- parseBuildHash
              urlOpt = urlOpt0.orElse(
                buildHashOpt.map(_ => defaultArchiveUrl(gitSha))
              )
              hashOpt = hashOpt0.orElse(buildHashOpt)
              archivePath <- archiveOpt0 match {
                case Some(p) => requireFile(p)
                case None    =>
                  urlOpt match {
                    case None =>
                      moduleIOMonad.raiseError(
                        CliException.Basic(
                          "no archive or url given; pass --archive or --url and --hash"
                        )
                      )
                    case Some(url) =>
                      hashOpt match {
                        case None =>
                          moduleIOMonad.raiseError(
                            CliException.Basic(
                              "no hash available for download; pass --hash or use a release build"
                            )
                          )
                        case Some(hash) =>
                          val archivePath =
                            archivePathForUrl(root, url, gitSha)
                          platformIO
                            .fetchHash(
                              hash.algo,
                              hash.value,
                              archivePath,
                              url
                            )
                            .as(archivePath)
                      }
                  }
              }
              baseDir = platformIO.resolve(
                root,
                ".bosatsuc" :: "c_runtime" :: "src" :: gitSha :: Nil
              )
              runtimeRoot <- ensureRuntimeRoot(baseDir, archivePath)
              _ <- platformIO.system(
                "make",
                List(
                  "-C",
                  platformIO.pathToString(runtimeRoot),
                  "install",
                  s"ROOTDIR=${platformIO.pathToString(root)}",
                  s"VERSION=$gitSha",
                  s"PROFILE=$profile"
                )
              )
              installDir =
                platformIO.resolve(root, ".bosatsuc" :: gitSha :: Nil)
              outDoc = Doc.text(
                show"installed c_runtime into $installDir"
              )
            } yield Output.Basic(outDoc, Option.empty[P])
          }
        }
        .map(_.widen[Output[P]])

    install
  }
}
