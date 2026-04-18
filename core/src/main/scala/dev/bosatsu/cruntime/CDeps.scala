package dev.bosatsu.cruntime

import cats.syntax.all._
import dev.bosatsu.{Json, Nullable}
import dev.bosatsu.Json.{Reader, Writer}
import dev.bosatsu.hashing.{Algo, HashValue}
import java.nio.charset.StandardCharsets

object CDeps {
  final val SchemaVersion = 1
  final val ManifestPath = "deps.json"
  final val BdwgcCmakeStatic = "bdwgc-cmake-static"
  final val LibuvCmakeStatic = "libuv-cmake-static"

  final case class Manifest(
      schema_version: Int,
      recipe_version: Int,
      dependencies: List[Dependency]
  ) derives Json.Reader, Json.Writer

  final case class Dependency(
      name: String,
      version: String,
      uris: List[String],
      hash: Algo.WithAlgo[HashValue],
      source_subdir: String,
      recipe: String,
      options: Option[Json.JObject] = None,
      dependencies: Option[List[String]] = None
  ) derives Json.Reader, Json.Writer

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
  ) derives Json.Writer

  final case class Target(
      os: String,
      arch: String,
      toolchain_family: String,
      toolchain_version: String
  ) derives Json.Reader, Json.Writer

  final case class RuntimeRequirements(
      bosatsu_runtime_cppflags: List[String],
      generated_c_cppflags: List[String]
  ) derives Json.Reader, Json.Writer

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
  ) derives Json.Reader, Json.Writer

  private final case class BuildKeyInput(
      name: String,
      version: String,
      hash: String,
      recipe: String,
      options: Option[Json],
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
      dependency.options.map(normalizeBuildKeyJson),
      context.recipe_version,
      dependency.dependencies.getOrElse(Nil).sorted,
      transitiveBuildKeys.sorted,
      context.copy(
        os = normalizeOs(context.os),
        arch = normalizeArch(context.arch),
        toolchain_family = normalizeToolchainFamily(context.toolchain_family),
        relevant_env = context.relevant_env.toList.sortBy(_._1).toMap
      )
    )
    Algo
      .hashBytes[Algo.Blake3](
        Writer.write(input).render.getBytes(StandardCharsets.UTF_8)
      )
      .hex
  }

  private def normalizeBuildKeyJson(json: Json): Json =
    json match {
      case Json.JArray(items) =>
        Json.JArray(items.map(normalizeBuildKeyJson))
      case obj: Json.JObject =>
        Json.JObject(
          obj.keys.sorted.map { key =>
            key -> normalizeBuildKeyJson(obj.toMap(key))
          }
        )
      case other =>
        other
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
            dep.dependencies.getOrElse(Nil).foldLeft[Either[String, Unit]](Right(())) {
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
