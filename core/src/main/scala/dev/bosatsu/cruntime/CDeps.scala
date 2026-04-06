package dev.bosatsu.cruntime

import cats.syntax.all._
import dev.bosatsu.{Json, Nullable}
import dev.bosatsu.Json.{JObject, JString, Reader, Writer}
import dev.bosatsu.hashing.{Algo, HashValue}
import java.nio.charset.StandardCharsets

object CDeps {
  final val SchemaVersion = 1
  final val ManifestPath = "deps.json"
  final val BdwgcCmakeStatic = "bdwgc-cmake-static"
  final val LibuvCmakeStatic = "libuv-cmake-static"

  implicit val hashReader: Reader[Algo.WithAlgo[HashValue]] =
    Reader
      .fromParser("Algo.WithAlgo[HashValue]", Algo.parseIdent)

  implicit val hashWriter: Writer[Algo.WithAlgo[HashValue]] =
    Writer.from(h => JString(h.toIdent))

  given optionsFieldReader: Reader.ProductFieldReader[Option[Json]] with {
    def read(
        from: Reader.FromObj,
        key: String
    ): Either[(String, Json, Json.Path), Option[Json]] =
      from.optional[Json](key).map {
        case Some(obj: JObject) if obj.keys.isEmpty => None
        case other                                  => other
      }
  }

  given optionsFieldWriter: Writer.ProductFieldWriter[Option[Json]] with {
    def fields(name: String, value: Option[Json]): List[(String, Json)] =
      value match {
        case None                                   => Nil
        case Some(obj: JObject) if obj.keys.isEmpty => Nil
        case Some(other)                            => (name -> Writer.write(other)) :: Nil
      }
  }

  given dependencyListFieldReader: Reader.ProductFieldReader[
    Option[List[String]]
  ] with {
    def read(
        from: Reader.FromObj,
        key: String
    ): Either[(String, Json, Json.Path), Option[List[String]]] =
      from.optional[List[String]](key).map(_.filter(_.nonEmpty))
  }

  given dependencyListFieldWriter: Writer.ProductFieldWriter[
    Option[List[String]]
  ] with {
    def fields(
        name: String,
        value: Option[List[String]]
    ): List[(String, Json)] =
      value.filter(_.nonEmpty) match {
        case Some(items) => (name -> Writer.write(items)) :: Nil
        case None        => Nil
      }
  }

  final case class Manifest(
      schema_version: Int,
      recipe_version: Int,
      dependencies: List[Dependency]
  ) derives Json.Reader, Json.Writer {
    def schemaVersion: Int = schema_version
    def recipeVersion: Int = recipe_version
  }

  final case class Dependency(
      name: String,
      version: String,
      uris: List[String],
      hash: Algo.WithAlgo[HashValue],
      source_subdir: String,
      recipe: String,
      options: Option[Json] = None,
      dependencies: Option[List[String]] = None
  ) derives Json.Reader, Json.Writer {
    def sourceSubdir: String = source_subdir
    def optionsJson: Json = options.getOrElse(Json.JObject(Nil))
    def dependencyNames: List[String] = dependencies.getOrElse(Nil)
  }

  object Dependency {
    def apply(
        name: String,
        version: String,
        uris: List[String],
        hash: Algo.WithAlgo[HashValue],
        sourceSubdir: String,
        recipe: String,
        options: Json,
        dependencies: List[String]
    ): Dependency =
      new Dependency(
        name = name,
        version = version,
        uris = uris,
        hash = hash,
        source_subdir = sourceSubdir,
        recipe = recipe,
        options = options match {
          case obj: JObject if obj.keys.isEmpty => None
          case Json.JNull                       => None
          case other                            => Some(other)
        },
        dependencies = dependencies match {
          case Nil   => None
          case items => Some(items)
        }
      )

    def apply(
        name: String,
        version: String,
        uris: List[String],
        hash: Algo.WithAlgo[HashValue],
        sourceSubdir: String,
        recipe: String,
        options: Json
    ): Dependency =
      apply(name, version, uris, hash, sourceSubdir, recipe, options, Nil)

    def apply(
        name: String,
        version: String,
        uris: List[String],
        hash: Algo.WithAlgo[HashValue],
        sourceSubdir: String,
        recipe: String
    ): Dependency =
      apply(
        name,
        version,
        uris,
        hash,
        sourceSubdir,
        recipe,
        Json.JObject(Nil),
        Nil
      )
  }

  final case class BuildContext(
      os: String,
      arch: String,
      toolchain_family: String,
      compiler_path: String,
      compiler_version: String,
      archiver_path: Nullable[String],
      archiver_version: Nullable[String],
      profile: String,
      recipe_version: Int,
      relevant_env: Map[String, String]
  ) derives Json.Writer {
    def toolchainFamily: String = toolchain_family
    def compilerPath: String = compiler_path
    def compilerVersion: String = compiler_version
    def archiverPath: Option[String] = archiver_path.toOption
    def archiverVersion: Option[String] = archiver_version.toOption
    def recipeVersion: Int = recipe_version
    def relevantEnv: Map[String, String] = relevant_env
  }

  object BuildContext {
    def apply(
        os: String,
        arch: String,
        toolchainFamily: String,
        compilerPath: String,
        compilerVersion: String,
        archiverPath: Option[String],
        archiverVersion: Option[String],
        profile: String,
        recipeVersion: Int,
        relevantEnv: Map[String, String]
    ): BuildContext =
      new BuildContext(
        os = os,
        arch = arch,
        toolchain_family = toolchainFamily,
        compiler_path = compilerPath,
        compiler_version = compilerVersion,
        archiver_path = Nullable.fromOption(archiverPath),
        archiver_version = Nullable.fromOption(archiverVersion),
        profile = profile,
        recipe_version = recipeVersion,
        relevant_env = relevantEnv
      )
  }

  final case class Target(
      os: String,
      arch: String,
      toolchain_family: String,
      toolchain_version: String
  ) derives Json.Reader, Json.Writer {
    def toolchainFamily: String = toolchain_family
    def toolchainVersion: String = toolchain_version
  }

  final case class RuntimeRequirements(
      bosatsu_runtime_cppflags: List[String],
      generated_c_cppflags: List[String]
  ) derives Json.Reader, Json.Writer {
    def bosatsuRuntimeCppflags: List[String] = bosatsu_runtime_cppflags
    def generatedCCppflags: List[String] = generated_c_cppflags
  }

  final case class Metadata(
      schema_version: Int,
      name: String,
      version: String,
      recipe: String,
      source_hash: String,
      build_key: String,
      dependencies: Option[List[String]] = None,
      target: Target,
      prefix: String,
      include_dirs: List[String],
      static_libs: List[String],
      system_link_flags: List[String],
      runtime_requirements: RuntimeRequirements
  ) derives Json.Reader, Json.Writer {
    def schemaVersion: Int = schema_version
    def sourceHash: String = source_hash
    def buildKey: String = build_key
    def dependencyNames: List[String] = dependencies.getOrElse(Nil)
    def includeDirs: List[String] = include_dirs
    def staticLibs: List[String] = static_libs
    def systemLinkFlags: List[String] = system_link_flags
    def runtimeRequirements: RuntimeRequirements = runtime_requirements
  }

  object Metadata {
    def apply(
        schemaVersion: Int,
        name: String,
        version: String,
        recipe: String,
        sourceHash: String,
        buildKey: String,
        dependencies: List[String],
        target: Target,
        prefix: String,
        includeDirs: List[String],
        staticLibs: List[String],
        systemLinkFlags: List[String],
        runtimeRequirements: RuntimeRequirements
    ): Metadata =
      new Metadata(
        schema_version = schemaVersion,
        name = name,
        version = version,
        recipe = recipe,
        source_hash = sourceHash,
        build_key = buildKey,
        dependencies = dependencies match {
          case Nil   => None
          case items => Some(items)
        },
        target = target,
        prefix = prefix,
        include_dirs = includeDirs,
        static_libs = staticLibs,
        system_link_flags = systemLinkFlags,
        runtime_requirements = runtimeRequirements
      )
  }

  private final case class BuildKeyInput(
      name: String,
      version: String,
      hash: String,
      recipe: String,
      recipe_version: Int,
      source_dependencies: List[String],
      transitive_build_keys: List[String],
      context: BuildContext
  ) derives Json.Writer

  def parseManifestString(
      content: String
  ): Either[String, Manifest] =
    Json.parserFile.parseAll(content).leftMap(_.toString).flatMap { json =>
      Json.Reader[Manifest].read(Json.Path.Root, json).leftMap {
        case (msg, _, path) =>
          s"$msg at $path"
      }
    }

  def parseMetadataString(
      content: String
  ): Either[String, Metadata] =
    Json.parserFile.parseAll(content).leftMap(_.toString).flatMap { json =>
      Json.Reader[Metadata].read(Json.Path.Root, json).leftMap {
        case (msg, _, path) =>
          s"$msg at $path"
      }
    }

  def renderManifest(manifest: Manifest): String =
    Writer.write(manifest).render

  def renderMetadata(metadata: Metadata): String =
    Writer.write(metadata).render

  def buildKey(
      dependency: Dependency,
      context: BuildContext,
      transitiveBuildKeys: List[String] = Nil
  ): String = {
    implicit val algo: Algo[Algo.Blake3] = Algo.blake3Algo
    val input = BuildKeyInput(
      dependency.name,
      dependency.version,
      dependency.hash.toIdent,
      dependency.recipe,
      context.recipeVersion,
      dependency.dependencyNames.sorted,
      transitiveBuildKeys.sorted,
      context.copy(
        os = normalizeOs(context.os),
        arch = normalizeArch(context.arch),
        toolchain_family = normalizeToolchainFamily(context.toolchainFamily),
        relevant_env = context.relevantEnv.toList.sortBy(_._1).toMap
      )
    )
    Algo
      .hashBytes[Algo.Blake3](
        Writer.write(input).render.getBytes(StandardCharsets.UTF_8)
      )
      .hex
  }

  def normalizeOs(raw: String): String = {
    val lower = raw.toLowerCase(java.util.Locale.ROOT)
    if (lower.contains("darwin") || lower.contains("mac")) "macos"
    else if (lower.contains("linux")) "linux"
    else if (lower.contains("windows") || lower.startsWith("win")) "windows"
    else lower
  }

  def normalizeArch(raw: String): String = {
    val lower = raw.toLowerCase(java.util.Locale.ROOT)
    lower match {
      case "aarch64" => "arm64"
      case "amd64"   => "x86_64"
      case other     => other
    }
  }

  def normalizeToolchainFamily(raw: String): String =
    raw.toLowerCase(java.util.Locale.ROOT).trim

  def orderedDependencies(
      manifest: Manifest
  ): Either[String, List[Dependency]] = {
    val grouped = manifest.dependencies.groupBy(_.name)
    grouped.collectFirst { case (name, deps) if deps.lengthCompare(1) > 0 => name } match {
      case Some(name) =>
        Left(s"duplicate dependency name in manifest: $name")
      case None =>
        val byName = manifest.dependencies.iterator.map(d => d.name -> d).toMap
        val ordered = scala.collection.mutable.ListBuffer.empty[Dependency]

        def visit(dep: Dependency, active: List[String]): Either[String, Set[String]] =
          if (ordered.exists(_.name == dep.name)) Right(Set.empty)
          else if (active.contains(dep.name)) {
            val cycle = (dep.name :: active).reverse.mkString(" -> ")
            Left(s"dependency cycle detected: $cycle")
          } else {
            dep.dependencyNames.foldLeft[Either[String, Unit]](Right(())) {
              case (acc, depName) =>
                acc.flatMap { _ =>
                  byName.get(depName) match {
                    case Some(next) =>
                      visit(next, dep.name :: active).map(_ => ())
                    case None =>
                      Left(
                        s"${dep.name} depends on missing vendored dependency: $depName"
                      )
                  }
                }
            }.map { _ =>
              if (!ordered.exists(_.name == dep.name)) ordered += dep
              Set(dep.name)
            }
          }

        manifest.dependencies.traverse_(visit(_, Nil)).map(_ => ordered.toList)
    }
  }

  def detectToolchainFamily(
      compilerPath: String,
      compilerVersion: String
  ): String = {
    val haystack =
      s"$compilerPath\n$compilerVersion".toLowerCase(java.util.Locale.ROOT)
    if (haystack.contains("clang-cl")) "clang-cl"
    else if (haystack.contains("apple clang") || haystack.contains("clang"))
      "clang"
    else if (
      haystack.contains("gcc") || haystack.contains("gnu compiler")
    )
      "gcc"
    else if (
      haystack.contains("microsoft") ||
      haystack.contains("cl.exe") ||
      haystack.contains("msvc")
    )
      "msvc"
    else "unknown"
  }
}
