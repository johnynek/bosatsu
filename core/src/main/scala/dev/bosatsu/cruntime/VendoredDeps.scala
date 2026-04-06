package dev.bosatsu.cruntime

import cats.syntax.all._
import dev.bosatsu.{Json, PlatformIO}
import dev.bosatsu.Json.{JBool, JObject}
import dev.bosatsu.tool.CliException
import org.typelevel.paiges.Doc
import scala.util.matching.Regex

object VendoredDeps {
  final case class ResolvedDependency[P](
      dependency: CDeps.Dependency,
      buildKey: String,
      buildDir: P,
      metadata: CDeps.Metadata
  )

  final case class BuildInputs[P](
      manifest: CDeps.Manifest,
      resolved: List[ResolvedDependency[P]]
  ) {
    def runtimeCppFlags: List[String] =
      resolved
        .flatMap(_.metadata.runtimeRequirements.bosatsuRuntimeCppflags)
        .distinct

    def includeFlags: List[String] =
      resolved
        .flatMap(_.metadata.includeDirs.map("-I" + _))
        .distinct

    def linkFlags: List[String] =
      resolved
        .flatMap(rd => rd.metadata.staticLibs ::: rd.metadata.systemLinkFlags)
        .distinct
  }

  private val metadataFileName = "metadata.json"
  private val successFileName = "success"
  private val pkgConfigDir = List("lib", "pkgconfig")

  def maybeLoadManifest[F[_], P](
      runtimeRoot: P
  )(platformIO: PlatformIO[F, P]): F[Option[CDeps.Manifest]] = {
    import platformIO.moduleIOMonad

    val manifestPath = platformIO.resolve(runtimeRoot, CDeps.ManifestPath)
    platformIO.fsDataType(manifestPath).flatMap {
      case None => moduleIOMonad.pure(None)
      case Some(PlatformIO.FSDataType.Dir) =>
        moduleIOMonad.raiseError(
          CliException.Basic(
            s"expected vendored dependency manifest at ${platformIO.pathToString(manifestPath)} to be a file"
          )
        )
      case Some(PlatformIO.FSDataType.File) =>
        readManifest(manifestPath)(platformIO).map(Some(_))
    }
  }

  def ensure[F[_], P](
      repoRoot: P,
      runtimeRoot: P,
      profile: String
  )(platformIO: PlatformIO[F, P]): F[Option[BuildInputs[P]]] = {
    import platformIO.moduleIOMonad

    maybeLoadManifest(runtimeRoot)(platformIO).flatMap {
      case None => moduleIOMonad.pure(None)
      case Some(manifest) =>
        CDeps.orderedDependencies(manifest) match {
          case Left(err) =>
            moduleIOMonad.raiseError(
              CliException.Basic(s"invalid vendored dependency manifest: $err")
            )
          case Right(ordered) =>
            for {
              context <- buildContext(manifest.recipeVersion, profile)(platformIO)
              resolved <- ordered.foldLeftM(
                (List.empty[ResolvedDependency[P]], Map.empty[String, ResolvedDependency[P]])
              ) { case ((acc, byName), dependency) =>
                ensureDependency(repoRoot, dependency, context, byName)(
                  platformIO
                ).map { resolved =>
                  (acc :+ resolved, byName.updated(dependency.name, resolved))
                }
              }
            } yield Some(BuildInputs(manifest, resolved._1))
        }
    }
  }

  private def readManifest[F[_], P](
      manifestPath: P
  )(platformIO: PlatformIO[F, P]): F[CDeps.Manifest] = {
    import platformIO.moduleIOMonad

    platformIO.readUtf8(manifestPath).flatMap { content =>
      CDeps.parseManifestString(content) match {
        case Right(manifest) => moduleIOMonad.pure(manifest)
        case Left(msg) =>
          moduleIOMonad.raiseError(
            CliException.Basic(
              s"failed to parse vendored dependency manifest ${platformIO.pathToString(manifestPath)}: $msg"
            )
          )
      }
    }
  }

  private def ensureDependency[F[_], P](
      repoRoot: P,
      dependency: CDeps.Dependency,
      context: CDeps.BuildContext,
      resolvedDependencies: Map[String, ResolvedDependency[P]]
  )(platformIO: PlatformIO[F, P]): F[ResolvedDependency[P]] = {
    import platformIO.moduleIOMonad

    val buildKey =
      CDeps.buildKey(
        dependency,
        context,
        transitiveBuildKeys(dependency, resolvedDependencies)
      )
    val buildDir = buildCacheDir(repoRoot, buildKey)(platformIO)

    cachedMetadata(buildDir)(platformIO).flatMap {
      case Some(metadata) =>
        moduleIOMonad.pure(
          ResolvedDependency(dependency, buildKey, buildDir, metadata)
        )
      case None =>
        for {
          archive <- ensureArchive(repoRoot, dependency)(platformIO)
          sourceRoot <- ensureSourceTree(repoRoot, dependency, archive)(platformIO)
          metadata <- platformIO.withTempPrefix(s"bosatsu-${dependency.name}-") {
            tempRoot =>
              publishDependency(
                tempRoot,
                buildDir,
                sourceRoot,
                dependency,
                buildKey,
                context
              )(platformIO)
          }
        } yield ResolvedDependency(dependency, buildKey, buildDir, metadata)
    }
  }

  private def publishDependency[F[_], P](
      tempRoot: P,
      finalBuildDir: P,
      sourceRoot: P,
      dependency: CDeps.Dependency,
      buildKey: String,
      context: CDeps.BuildContext
  )(platformIO: PlatformIO[F, P]): F[CDeps.Metadata] = {
    import platformIO.moduleIOMonad

    val publishRoot = platformIO.resolve(tempRoot, "publish")
    val buildDir = platformIO.resolve(tempRoot, "build")
    val prefix = platformIO.resolve(publishRoot, "prefix")
    val finalPrefix = platformIO.resolve(finalBuildDir, "prefix")

    for {
      _ <- ensureDir(publishRoot)(platformIO)
      _ <- ensureDir(buildDir)(platformIO)
      _ <- runRecipe(dependency, sourceRoot, buildDir, prefix, context.profile)(
        platformIO
      )
      metadata <- buildMetadata(
        dependency,
        buildKey,
        context,
        prefix,
        finalPrefix
      )(platformIO)
      _ <- writeMetadata(publishRoot, metadata)(platformIO)
      _ <- replaceDirectory(finalBuildDir, publishRoot)(platformIO)
      loaded <- cachedMetadata(finalBuildDir)(platformIO).flatMap {
        case Some(value) => moduleIOMonad.pure(value)
        case None =>
          moduleIOMonad.raiseError(
            CliException.Basic(
              s"failed to publish vendored dependency ${dependency.name} into ${platformIO.pathToString(finalBuildDir)}"
            )
          )
      }
    } yield loaded
  }

  private def buildMetadata[F[_], P](
      dependency: CDeps.Dependency,
      buildKey: String,
      context: CDeps.BuildContext,
      installedPrefix: P,
      recordedPrefix: P
  )(platformIO: PlatformIO[F, P]): F[CDeps.Metadata] = {
    import platformIO.moduleIOMonad

    val staticLibFile = staticLibFileName(dependency)
    val includeDir = platformIO.resolve(installedPrefix, "include")
    val libDir = platformIO.resolve(installedPrefix, "lib")
    val staticLib = platformIO.resolve(libDir, staticLibFile)
    val recordedIncludeDir = platformIO.resolve(recordedPrefix, "include")
    val recordedLibDir = platformIO.resolve(recordedPrefix, "lib")
    val recordedStaticLib = platformIO.resolve(recordedLibDir, staticLibFile)

    for {
      _ <- requireDir(includeDir, s"vendored include dir for ${dependency.name}")(
        platformIO
      )
      _ <- requireFile(staticLib, s"vendored static lib for ${dependency.name}")(
        platformIO
      )
      systemLinkFlags <- systemLinkFlagsFor(dependency, installedPrefix)(platformIO)
      normalizedOs = CDeps.normalizeOs(context.os)
      normalizedArch = CDeps.normalizeArch(context.arch)
      normalizedFamily = CDeps.normalizeToolchainFamily(context.toolchainFamily)
      metadata = CDeps.Metadata(
        schemaVersion = CDeps.SchemaVersion,
        name = dependency.name,
        version = dependency.version,
        recipe = dependency.recipe,
        sourceHash = dependency.hash.toIdent,
        buildKey = buildKey,
        dependencies = dependency.dependencyNames,
        target = CDeps.Target(
          normalizedOs,
          normalizedArch,
          normalizedFamily,
          context.compilerVersion.trim
        ),
        prefix = platformIO.pathToString(recordedPrefix),
        includeDirs = platformIO.pathToString(recordedIncludeDir) :: Nil,
        staticLibs = platformIO.pathToString(recordedStaticLib) :: Nil,
        systemLinkFlags = systemLinkFlags,
        runtimeRequirements = runtimeRequirementsFor(dependency)
      )
    } yield metadata
  }

  private def runRecipe[F[_], P](
      dependency: CDeps.Dependency,
      sourceRoot: P,
      buildDir: P,
      prefix: P,
      profile: String
  )(platformIO: PlatformIO[F, P]): F[Unit] =
    dependency.recipe match {
      case CDeps.BdwgcCmakeStatic =>
        runBdwgcRecipe(dependency, sourceRoot, buildDir, prefix, profile)(
          platformIO
        )
      case other =>
        platformIO.moduleIOMonad.raiseError(
          CliException.Basic(s"unsupported vendored dependency recipe: $other")
        )
    }

  private def runBdwgcRecipe[F[_], P](
      dependency: CDeps.Dependency,
      sourceRoot: P,
      buildDir: P,
      prefix: P,
      profile: String
  )(platformIO: PlatformIO[F, P]): F[Unit] = {
    import platformIO.moduleIOMonad

    val threadsafe =
      optionBool(dependency.optionsJson, "threadsafe").getOrElse(true)

    if (!threadsafe)
      platformIO.moduleIOMonad.raiseError(
        CliException.Basic("bdwgc recipe requires threadsafe=true")
      )
    else {
      val buildType =
        if (profile.equalsIgnoreCase("debug")) "Debug" else "Release"

      val configureArgs =
        List(
          "-S",
          platformIO.pathToString(sourceRoot),
          "-B",
          platformIO.pathToString(buildDir),
          s"-DCMAKE_BUILD_TYPE=$buildType",
          s"-DCMAKE_INSTALL_PREFIX=${platformIO.pathToString(prefix)}",
          "-DBUILD_SHARED_LIBS=OFF",
          "-DBUILD_TESTING=OFF",
          "-Denable_threads=ON"
        )

      platformIO.system("cmake", configureArgs) *>
        platformIO.system(
          "cmake",
          List(
            "--build",
            platformIO.pathToString(buildDir),
            "--target",
            "install"
          )
        )
    }
  }

  private def optionBool(options: Json, key: String): Option[Boolean] =
    options match {
      case obj: JObject =>
        obj.toMap.get(key).collect { case JBool(value) => value }
      case _ => None
    }

  private[cruntime] def staticLibFileName(
      dependency: CDeps.Dependency
  ): String =
    dependency.recipe match {
      case CDeps.BdwgcCmakeStatic => "libgc.a"
      case CDeps.LibuvCmakeStatic => "libuv.a"
      case _                      => s"lib${dependency.name}.a"
    }

  private[cruntime] def runtimeRequirementsFor(
      dependency: CDeps.Dependency
  ): CDeps.RuntimeRequirements =
    dependency.recipe match {
      case CDeps.BdwgcCmakeStatic
          if optionBool(dependency.optionsJson, "threadsafe").getOrElse(true) =>
        CDeps.RuntimeRequirements(
          "-DGC_THREADS" :: Nil,
          "-DGC_THREADS" :: Nil
        )
      case _ =>
        CDeps.RuntimeRequirements(Nil, Nil)
    }

  private def systemLinkFlagsFor[F[_], P](
      dependency: CDeps.Dependency,
      installedPrefix: P
  )(platformIO: PlatformIO[F, P]): F[List[String]] =
    dependency.recipe match {
      case CDeps.BdwgcCmakeStatic =>
        bdwgcSystemLinkFlags(installedPrefix)(platformIO)
      case _ =>
        platformIO.moduleIOMonad.pure(Nil)
    }

  private def buildContext[F[_], P](
      recipeVersion: Int,
      profile: String
  )(platformIO: PlatformIO[F, P]): F[CDeps.BuildContext] = {
    import platformIO.moduleIOMonad

    val trackedEnvNames =
      List("CC", "AR", "CFLAGS", "CPPFLAGS", "LDFLAGS", "LIBS", "PKG_CONFIG")

    def readEnv(name: String): F[Option[String]] =
      platformIO.env(name).map(_.map(_.trim).filter(_.nonEmpty))

    def resolveExecutable(cmd: String): F[String] =
      if (cmd.contains("/") || cmd.contains("\\")) moduleIOMonad.pure(cmd)
      else
        platformIO
          .systemStdout("which", cmd :: Nil)
          .map(_.trim)
          .handleError(_ => cmd)

    def cmdVersion(cmd: String): F[String] =
      platformIO
        .systemStdout(cmd, "--version" :: Nil)
        .map(_.trim)
        .handleError(_ => "")

    for {
      ccEnv <- readEnv("CC")
      compilerCmd = ccEnv.getOrElse("cc")
      compilerPath <- resolveExecutable(compilerCmd)
      compilerVersion <- cmdVersion(compilerPath)
      arEnv <- readEnv("AR")
      arCmd = arEnv.getOrElse("ar")
      archiverPath <- resolveExecutable(arCmd).map(Some(_)).handleError(_ => None)
      archiverVersion <- archiverPath match {
        case Some(path) =>
          cmdVersion(path).map(Some(_)).handleError(_ => None)
        case None => moduleIOMonad.pure(None)
      }
      envPairs <- trackedEnvNames.traverse { name =>
        readEnv(name).map(_.map(value => name -> value))
      }
      hostOs <- platformIO.hostOs
      hostArch <- platformIO.hostArch
      toolchainFamily = CDeps.detectToolchainFamily(compilerPath, compilerVersion)
    } yield CDeps.BuildContext(
      os = CDeps.normalizeOs(hostOs),
      arch = CDeps.normalizeArch(hostArch),
      toolchainFamily = toolchainFamily,
      compilerPath = compilerPath,
      compilerVersion = compilerVersion,
      archiverPath = archiverPath,
      archiverVersion = archiverVersion,
      profile = profile,
      recipeVersion = recipeVersion,
      relevantEnv = envPairs.flatten.toMap
    )
  }

  private def ensureArchive[F[_], P](
      repoRoot: P,
      dependency: CDeps.Dependency
  )(platformIO: PlatformIO[F, P]): F[P] = {
    import platformIO.moduleIOMonad

    val archivePath = archiveCachePath(repoRoot, dependency)(platformIO)

    def fetchFromUri(uri: String): F[Either[PlatformIO.FetchHashFailure, Unit]] =
      platformIO.fetchHash(
        dependency.hash.algo,
        dependency.hash.value,
        archivePath,
        uri
      )

    def loop(
        uris: List[String],
        failures: List[PlatformIO.FetchHashFailure]
    ): F[P] =
      uris match {
        case Nil =>
          val errDoc =
            Doc.intercalate(
              Doc.hardLine + Doc.hardLine,
              failures.reverse.map(PlatformIO.FetchHashFailure.toDoc)
            )
          moduleIOMonad.raiseError(
            CliException(
              s"failed to fetch vendored dependency archive for ${dependency.name}",
              errDoc
            )
          )
        case uri :: rest =>
          fetchFromUri(uri).flatMap {
            case Right(_) => moduleIOMonad.pure(archivePath)
            case Left(err) => loop(rest, err :: failures)
          }
      }

    for {
      archiveParent <- parentOrError(archivePath, "archive parent")(platformIO)
      _ <- ensureDir(archiveParent)(platformIO)
      validExisting <- verifiedHash(archivePath, dependency)(platformIO)
      archive <- if (validExisting) moduleIOMonad.pure(archivePath)
      else loop(dependency.uris, Nil)
    } yield archive
  }

  private def verifiedHash[F[_], P](
      archivePath: P,
      dependency: CDeps.Dependency
  )(platformIO: PlatformIO[F, P]): F[Boolean] = {
    import platformIO.moduleIOMonad

    platformIO.fsDataType(archivePath).flatMap {
      case Some(PlatformIO.FSDataType.File) =>
        platformIO.readBytes(archivePath).map { bytes =>
          dependency.hash.algo.hashBytes(bytes).hex == dependency.hash.value.hex
        }
      case _ =>
        platformIO.moduleIOMonad.pure(false)
    }
  }

  private def ensureSourceTree[F[_], P](
      repoRoot: P,
      dependency: CDeps.Dependency,
      archivePath: P
  )(platformIO: PlatformIO[F, P]): F[P] = {
    import platformIO.moduleIOMonad

    val sourceBase = sourceCacheDir(repoRoot, dependency)(platformIO)
    val sourceRoot = platformIO.resolve(sourceBase, dependency.sourceSubdir)

    platformIO.fsDataType(sourceRoot).flatMap {
      case Some(PlatformIO.FSDataType.Dir) =>
        moduleIOMonad.pure(sourceRoot)
      case _ =>
        for {
          parent <- parentOrError(sourceBase, "source parent")(platformIO)
          _ <- ensureDir(parent)(platformIO)
          _ <- removeTree(sourceBase)(platformIO).handleError(_ => ())
          _ <- platformIO.withTempPrefix(s"bosatsu-${dependency.name}-src-") {
            tempRoot =>
              val publishRoot = platformIO.resolve(tempRoot, "publish")
              for {
                _ <- ensureDir(publishRoot)(platformIO)
                _ <- extractArchive(archivePath, publishRoot)(platformIO)
                _ <- requireDir(
                  platformIO.resolve(publishRoot, dependency.sourceSubdir),
                  s"extracted source root for ${dependency.name}"
                )(platformIO)
                _ <- moveDirectory(publishRoot, sourceBase)(platformIO)
              } yield ()
          }
          _ <- requireDir(sourceRoot, s"source cache for ${dependency.name}")(
            platformIO
          )
        } yield sourceRoot
    }
  }

  private def cachedMetadata[F[_], P](
      buildDir: P
  )(platformIO: PlatformIO[F, P]): F[Option[CDeps.Metadata]] = {
    import platformIO.moduleIOMonad

    val metadataPath = platformIO.resolve(buildDir, metadataFileName)
    val successPath = platformIO.resolve(buildDir, successFileName)

    (
      platformIO.fileExists(metadataPath),
      platformIO.fileExists(successPath)
    ).mapN(_ && _).flatMap {
      case false => moduleIOMonad.pure(None)
      case true  =>
        platformIO.readUtf8(metadataPath).flatMap { content =>
          CDeps.parseMetadataString(content) match {
            case Left(_) => moduleIOMonad.pure(None)
            case Right(metadata) =>
              validateMetadataPaths(metadata)(platformIO).map {
                case true  => Some(metadata)
                case false => None
              }
          }
        }
    }
  }

  private def validateMetadataPaths[F[_], P](
      metadata: CDeps.Metadata
  )(platformIO: PlatformIO[F, P]): F[Boolean] = {
    import platformIO.moduleIOMonad

    def exists(pathStr: String): F[Boolean] =
      platformIO
        .pathF(pathStr)
        .flatMap(platformIO.fsDataType)
        .map(_.nonEmpty)
        .handleError(_ => false)

    val checks =
      metadata.includeDirs.traverse(exists) ::
        metadata.staticLibs.traverse(exists) ::
        Nil

    checks.sequence.map(_.flatten.forall(identity)).handleError(_ => false)
  }

  private def bdwgcSystemLinkFlags[F[_], P](
      prefix: P
  )(platformIO: PlatformIO[F, P]): F[List[String]] = {
    import platformIO.moduleIOMonad

    val pcPath =
      platformIO.resolve(prefix, pkgConfigDir ::: "bdw-gc.pc" :: Nil)

    platformIO.fsDataType(pcPath).flatMap {
      case Some(PlatformIO.FSDataType.File) =>
        platformIO.readUtf8(pcPath).map(parsePkgConfigSystemFlags)
      case _ =>
        moduleIOMonad.pure(Nil)
    }
  }

  private[cruntime] def parsePkgConfigSystemFlags(content: String): List[String] = {
    val variablePattern = "\\$\\{([^}]+)\\}".r

    val (variables, fields) =
      content.linesIterator.foldLeft((Map.empty[String, String], Map.empty[String, String])) {
        case ((vars, acc), rawLine) =>
          val line = rawLine.trim
          if (line.isEmpty || line.startsWith("#")) (vars, acc)
          else if (line.contains("=") && !line.contains(":")) {
            val idx = line.indexOf('=')
            val key = line.substring(0, idx).trim
            val value = line.substring(idx + 1).trim
            (vars.updated(key, value), acc)
          } else if (line.contains(":")) {
            val idx = line.indexOf(':')
            val key = line.substring(0, idx).trim
            val value = line.substring(idx + 1).trim
            (vars, acc.updated(key, value))
          } else (vars, acc)
      }

    def expand(input: String): String = {
      @annotation.tailrec
      def loop(current: String, remaining: Int): String =
        if (remaining <= 0) current
        else {
          val next =
            variablePattern.replaceAllIn(
              current,
              m => {
                val key = Option(m.group(1)).getOrElse("")
                val matched = Option(m.matched).getOrElse("")
                Regex.quoteReplacement(variables.getOrElse(key, matched))
              }
            )
          if (next == current) current else loop(next, remaining - 1)
        }

      loop(input, 8)
    }

    val rawFlags =
      List(fields.get("Libs"), fields.get("Libs.private"))
        .flatten
        .flatMap(expand(_).split("\\s+"))
        .map(_.trim)
        .filter(_.nonEmpty)

    rawFlags
      .filterNot(flag => flag == "-lgc" || flag.startsWith("-L"))
      .distinct
  }

  private def transitiveBuildKeys[P](
      dependency: CDeps.Dependency,
      resolvedDependencies: Map[String, ResolvedDependency[P]]
  ): List[String] = {
    def loop(
        name: String,
        seen: Set[String]
    ): List[String] =
      if (seen.contains(name)) Nil
      else
        resolvedDependencies.get(name).toList.flatMap { resolved =>
          resolved.buildKey :: resolved.dependency.dependencyNames.flatMap {
            depName =>
            loop(depName, seen + name)
          }
        }

    dependency.dependencyNames.flatMap(depName => loop(depName, Set.empty)).distinct
  }

  private def writeMetadata[F[_], P](
      publishRoot: P,
      metadata: CDeps.Metadata
  )(platformIO: PlatformIO[F, P]): F[Unit] = {
    import platformIO.moduleIOMonad

    val metadataPath = platformIO.resolve(publishRoot, metadataFileName)
    val successPath = platformIO.resolve(publishRoot, successFileName)

    platformIO.writeBytes(
      metadataPath,
      CDeps.renderMetadata(metadata).getBytes("utf-8")
    ) *>
      platformIO.writeBytes(successPath, Array.emptyByteArray)
  }

  private def archiveCachePath[F[_], P](
      repoRoot: P,
      dependency: CDeps.Dependency
  )(platformIO: PlatformIO[F, P]): P =
    platformIO.resolve(
      repoRoot,
      ".bosatsuc" ::
        "c_deps" ::
        "archives" ::
        dependency.name ::
        dependency.version ::
        dependency.hash.algo.name ::
        dependency.hash.value.hex ::
        archiveFileName(dependency) ::
        Nil
    )

  private def sourceCacheDir[F[_], P](
      repoRoot: P,
      dependency: CDeps.Dependency
  )(platformIO: PlatformIO[F, P]): P =
    platformIO.resolve(
      repoRoot,
      ".bosatsuc" ::
        "c_deps" ::
        "sources" ::
        dependency.name ::
        dependency.version ::
        dependency.hash.algo.name ::
        dependency.hash.value.hex ::
        Nil
    )

  private def buildCacheDir[F[_], P](
      repoRoot: P,
      buildKey: String
  )(platformIO: PlatformIO[F, P]): P =
    platformIO.resolve(
      repoRoot,
      ".bosatsuc" :: "c_deps" :: "builds" :: buildKey :: Nil
    )

  private def archiveFileName(dependency: CDeps.Dependency): String =
    dependency.uris.headOption
      .flatMap(_.split('/').lastOption)
      .map(_.takeWhile(_ != '?'))
      .filter(_.nonEmpty)
      .getOrElse(s"${dependency.name}-${dependency.version}.tar.gz")

  private def ensureDir[F[_], P](
      path: P
  )(platformIO: PlatformIO[F, P]): F[Unit] =
    platformIO.system("mkdir", "-p" :: platformIO.pathToString(path) :: Nil)

  private def removeTree[F[_], P](
      path: P
  )(platformIO: PlatformIO[F, P]): F[Unit] =
    platformIO.system("rm", "-rf" :: platformIO.pathToString(path) :: Nil)

  private def moveDirectory[F[_], P](
      from: P,
      to: P
  )(platformIO: PlatformIO[F, P]): F[Unit] =
    platformIO.system(
      "mv",
      platformIO.pathToString(from) :: platformIO.pathToString(to) :: Nil
    )

  private def replaceDirectory[F[_], P](
      to: P,
      from: P
  )(platformIO: PlatformIO[F, P]): F[Unit] = {
    import platformIO.moduleIOMonad

    for {
      parent <- parentOrError(to, "build cache parent")(platformIO)
      _ <- ensureDir(parent)(platformIO)
      _ <- removeTree(to)(platformIO).handleError(_ => ())
      _ <- moveDirectory(from, to)(platformIO)
    } yield ()
  }

  private def extractArchive[F[_], P](
      archive: P,
      dest: P
  )(platformIO: PlatformIO[F, P]): F[Unit] = {
    import platformIO.moduleIOMonad

    val archiveStr = platformIO.pathToString(archive)
    val destStr = platformIO.pathToString(dest)

    val extractCmd =
      if (archiveStr.endsWith(".zip"))
        ("unzip", List("-q", archiveStr, "-d", destStr))
      else if (archiveStr.endsWith(".tar.gz") || archiveStr.endsWith(".tgz"))
        ("tar", List("-xzf", archiveStr, "-C", destStr))
      else if (archiveStr.endsWith(".tar"))
        ("tar", List("-xf", archiveStr, "-C", destStr))
      else ("", Nil)

    extractCmd match {
      case ("", _) =>
        moduleIOMonad.raiseError(
          CliException.Basic(
            s"unsupported vendored archive type: $archiveStr (expected tar.gz, tgz, tar, or zip)"
          )
        )
      case (cmd, args) =>
        ensureDir(dest)(platformIO) *> platformIO.system(cmd, args)
    }
  }

  private def requireDir[F[_], P](
      path: P,
      label: String
  )(platformIO: PlatformIO[F, P]): F[Unit] =
    requireFsType(path, label, PlatformIO.FSDataType.Dir)(platformIO)

  private def requireFile[F[_], P](
      path: P,
      label: String
  )(platformIO: PlatformIO[F, P]): F[Unit] =
    requireFsType(path, label, PlatformIO.FSDataType.File)(platformIO)

  private def requireFsType[F[_], P](
      path: P,
      label: String,
      expected: PlatformIO.FSDataType
  )(platformIO: PlatformIO[F, P]): F[Unit] = {
    import platformIO.moduleIOMonad

    platformIO.fsDataType(path).flatMap {
      case Some(value) if value == expected =>
        platformIO.moduleIOMonad.pure(())
      case _ =>
        platformIO.moduleIOMonad.raiseError(
          CliException.Basic(
            s"expected $label at ${platformIO.pathToString(path)}"
          )
        )
    }
  }

  private def parentOrError[F[_], P](
      path: P,
      label: String
  )(platformIO: PlatformIO[F, P]): F[P] =
    platformIO.parent(path) match {
      case Some(parent) =>
        platformIO.moduleIOMonad.pure(parent)
      case None =>
        platformIO.moduleIOMonad.raiseError(
          CliException.Basic(
            s"could not determine $label for ${platformIO.pathToString(path)}"
          )
        )
    }
}
