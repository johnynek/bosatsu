package dev.bosatsu.cruntime

import cats.syntax.all._
import dev.bosatsu.Json
import dev.bosatsu.Json.{JBigInteger, JBool, JObject, JString, Reader, Writer}
import dev.bosatsu.hashing.{Algo, HashValue}
import java.nio.charset.StandardCharsets

object CDeps {
  final val SchemaVersion = 1
  final val ManifestPath = "deps.json"
  final val BdwgcCmakeStatic = "bdwgc-cmake-static"
  final val LibuvCmakeStatic = "libuv-cmake-static"

  final case class Manifest(
      schemaVersion: Int,
      recipeVersion: Int,
      dependencies: List[Dependency]
  )

  final case class Dependency(
      name: String,
      version: String,
      uris: List[String],
      hash: Algo.WithAlgo[HashValue],
      sourceSubdir: String,
      recipe: String,
      options: Json = Json.JObject(Nil),
      dependencies: List[String] = Nil
  )

  final case class BuildContext(
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
  )

  final case class Target(
      os: String,
      arch: String,
      toolchainFamily: String,
      toolchainVersion: String
  )

  final case class RuntimeRequirements(
      bosatsuRuntimeCppflags: List[String],
      generatedCCppflags: List[String]
  )

  final case class Metadata(
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
  )

  private final case class BuildKeyInput(
      name: String,
      version: String,
      hash: String,
      recipe: String,
      recipeVersion: Int,
      sourceDependencies: List[String],
      transitiveBuildKeys: List[String],
      context: BuildContext
  )

  implicit val jsonReader: Reader[Json] =
    new Reader[Json] {
      val describe = "Json"
      def read(
          path: Json.Path,
          j: Json
      ): Either[(String, Json, Json.Path), Json] =
        Right(j)
    }

  implicit val jsonWriter: Writer[Json] =
    Writer.from(identity)

  implicit val intReader: Reader[Int] =
    new Reader[Int] {
      val describe = "Int"
      private val intMin = java.math.BigInteger.valueOf(Int.MinValue.toLong)
      private val intMax = java.math.BigInteger.valueOf(Int.MaxValue.toLong)
      def read(
          path: Json.Path,
          j: Json
      ): Either[(String, Json, Json.Path), Int] =
        j match {
          case JBigInteger(bi)
              if bi.compareTo(intMin) >= 0 && bi.compareTo(intMax) <= 0 =>
            Right(bi.intValue)
          case JBigInteger(bi) =>
            Left((s"$bi cannot fit in Int", j, path))
          case _ =>
            Left((s"expected to find $describe", j, path))
        }
    }

  implicit val intWriter: Writer[Int] =
    Writer.from(i => Json.JNumberStr(i.toString))

  implicit val boolReader: Reader[Boolean] =
    new Reader[Boolean] {
      val describe = "Boolean"
      def read(
          path: Json.Path,
          j: Json
      ): Either[(String, Json, Json.Path), Boolean] =
        j match {
          case JBool(value) => Right(value)
          case _            => Left((s"expected to find $describe", j, path))
        }
    }

  implicit val boolWriter: Writer[Boolean] =
    Writer.from(JBool(_))

  implicit val stringMapWriter: Writer[Map[String, String]] =
    Writer.from { m =>
      JObject(m.toList.sortBy(_._1).map { case (k, v) => k -> JString(v) })
    }

  implicit val hashReader: Reader[Algo.WithAlgo[HashValue]] =
    Reader
      .fromParser("Algo.WithAlgo[HashValue]", Algo.parseIdent)

  implicit val hashWriter: Writer[Algo.WithAlgo[HashValue]] =
    Writer.from(h => JString(h.toIdent))

  implicit val runtimeRequirementsReader: Reader[RuntimeRequirements] =
    new Reader.Obj[RuntimeRequirements] {
      def describe = "RuntimeRequirements"
      def readObj(
          from: Reader.FromObj
      ): Either[(String, Json, Json.Path), RuntimeRequirements] =
        for {
          bosatsuRuntimeCppflags <- from.field[List[String]](
            "bosatsu_runtime_cppflags"
          )
          generatedCCppflags <- from.field[List[String]]("generated_c_cppflags")
        } yield RuntimeRequirements(bosatsuRuntimeCppflags, generatedCCppflags)
    }

  implicit val runtimeRequirementsWriter: Writer[RuntimeRequirements] =
    Writer.from { rr =>
      JObject(
        ("bosatsu_runtime_cppflags" -> Writer.write(
          rr.bosatsuRuntimeCppflags
        )) ::
          ("generated_c_cppflags" -> Writer.write(rr.generatedCCppflags)) ::
          Nil
      )
    }

  implicit val dependencyReader: Reader[Dependency] =
    new Reader.Obj[Dependency] {
      def describe = "Dependency"
      def readObj(
          from: Reader.FromObj
      ): Either[(String, Json, Json.Path), Dependency] =
        for {
          name <- from.field[String]("name")
          version <- from.field[String]("version")
          uris <- from.field[List[String]]("uris")
          hash <- from.field[Algo.WithAlgo[HashValue]]("hash")
          sourceSubdir <- from.field[String]("source_subdir")
          recipe <- from.field[String]("recipe")
          options <- from.optional[Json]("options").map(_.getOrElse(JObject(Nil)))
          dependencies <- from.optional[List[String]]("dependencies").map(
            _.getOrElse(Nil)
          )
        } yield Dependency(
          name,
          version,
          uris,
          hash,
          sourceSubdir,
          recipe,
          options,
          dependencies
        )
    }

  implicit val dependencyWriter: Writer[Dependency] =
    Writer.from { dep =>
      val base =
        List(
          "name" -> Writer.write(dep.name),
          "version" -> Writer.write(dep.version),
          "uris" -> Writer.write(dep.uris),
          "hash" -> Writer.write(dep.hash),
          "source_subdir" -> Writer.write(dep.sourceSubdir),
          "recipe" -> Writer.write(dep.recipe)
        )
      val withOptions =
        dep.options match {
          case obj: JObject if obj.keys.isEmpty => base
          case other                            => base :+ ("options" -> other)
        }
      val withDependencies =
        if (dep.dependencies.isEmpty) withOptions
        else withOptions :+ ("dependencies" -> Writer.write(dep.dependencies))
      JObject(withDependencies)
    }

  implicit val manifestReader: Reader[Manifest] =
    new Reader.Obj[Manifest] {
      def describe = "Manifest"
      def readObj(
          from: Reader.FromObj
      ): Either[(String, Json, Json.Path), Manifest] =
        for {
          schemaVersion <- from.field[Int]("schema_version")
          recipeVersion <- from.field[Int]("recipe_version")
          dependencies <- from.field[List[Dependency]]("dependencies")
        } yield Manifest(schemaVersion, recipeVersion, dependencies)
    }

  implicit val manifestWriter: Writer[Manifest] =
    Writer.from { manifest =>
      JObject(
        ("schema_version" -> Writer.write(manifest.schemaVersion)) ::
          ("recipe_version" -> Writer.write(manifest.recipeVersion)) ::
          ("dependencies" -> Writer.write(manifest.dependencies)) ::
          Nil
      )
    }

  implicit val buildContextWriter: Writer[BuildContext] =
    Writer.from { ctx =>
      JObject(
        ("os" -> Writer.write(ctx.os)) ::
          ("arch" -> Writer.write(ctx.arch)) ::
          ("toolchain_family" -> Writer.write(ctx.toolchainFamily)) ::
          ("compiler_path" -> Writer.write(ctx.compilerPath)) ::
          ("compiler_version" -> Writer.write(ctx.compilerVersion)) ::
          ("archiver_path" -> ctx.archiverPath.fold[Json](Json.JNull)(Writer.write(_))) ::
          ("archiver_version" -> ctx.archiverVersion.fold[Json](Json.JNull)(
            Writer.write(_)
          )) ::
          ("profile" -> Writer.write(ctx.profile)) ::
          ("recipe_version" -> Writer.write(ctx.recipeVersion)) ::
          ("relevant_env" -> Writer.write(ctx.relevantEnv)) ::
          Nil
      )
    }

  implicit val targetReader: Reader[Target] =
    new Reader.Obj[Target] {
      def describe = "Target"
      def readObj(from: Reader.FromObj) =
        for {
          os <- from.field[String]("os")
          arch <- from.field[String]("arch")
          toolchainFamily <- from.field[String]("toolchain_family")
          toolchainVersion <- from.field[String]("toolchain_version")
        } yield Target(os, arch, toolchainFamily, toolchainVersion)
    }

  implicit val targetWriter: Writer[Target] =
    Writer.from { target =>
      JObject(
        ("os" -> Writer.write(target.os)) ::
          ("arch" -> Writer.write(target.arch)) ::
          ("toolchain_family" -> Writer.write(target.toolchainFamily)) ::
          ("toolchain_version" -> Writer.write(target.toolchainVersion)) ::
          Nil
      )
    }

  implicit val metadataReader: Reader[Metadata] =
    new Reader.Obj[Metadata] {
      def describe = "Metadata"
      def readObj(from: Reader.FromObj) =
        for {
          schemaVersion <- from.field[Int]("schema_version")
          name <- from.field[String]("name")
          version <- from.field[String]("version")
          recipe <- from.field[String]("recipe")
          sourceHash <- from.field[String]("source_hash")
          buildKey <- from.field[String]("build_key")
          dependencies <- from.optional[List[String]]("dependencies").map(
            _.getOrElse(Nil)
          )
          target <- from.field[Target]("target")
          prefix <- from.field[String]("prefix")
          includeDirs <- from.field[List[String]]("include_dirs")
          staticLibs <- from.field[List[String]]("static_libs")
          systemLinkFlags <- from.field[List[String]]("system_link_flags")
          runtimeRequirements <- from.field[RuntimeRequirements](
            "runtime_requirements"
          )
        } yield Metadata(
          schemaVersion,
          name,
          version,
          recipe,
          sourceHash,
          buildKey,
          dependencies,
          target,
          prefix,
          includeDirs,
          staticLibs,
          systemLinkFlags,
          runtimeRequirements
        )
    }

  implicit val metadataWriter: Writer[Metadata] =
    Writer.from { metadata =>
      JObject(
        ("schema_version" -> Writer.write(metadata.schemaVersion)) ::
          ("name" -> Writer.write(metadata.name)) ::
          ("version" -> Writer.write(metadata.version)) ::
          ("recipe" -> Writer.write(metadata.recipe)) ::
          ("source_hash" -> Writer.write(metadata.sourceHash)) ::
          ("build_key" -> Writer.write(metadata.buildKey)) ::
          ("dependencies" -> Writer.write(metadata.dependencies)) ::
          ("target" -> Writer.write(metadata.target)) ::
          ("prefix" -> Writer.write(metadata.prefix)) ::
          ("include_dirs" -> Writer.write(metadata.includeDirs)) ::
          ("static_libs" -> Writer.write(metadata.staticLibs)) ::
          ("system_link_flags" -> Writer.write(metadata.systemLinkFlags)) ::
          ("runtime_requirements" -> Writer.write(metadata.runtimeRequirements)) ::
          Nil
      )
    }

  private implicit val buildKeyInputWriter: Writer[BuildKeyInput] =
    Writer.from { input =>
      JObject(
        ("name" -> Writer.write(input.name)) ::
          ("version" -> Writer.write(input.version)) ::
          ("hash" -> Writer.write(input.hash)) ::
          ("recipe" -> Writer.write(input.recipe)) ::
          ("recipe_version" -> Writer.write(input.recipeVersion)) ::
          ("source_dependencies" -> Writer.write(input.sourceDependencies)) ::
          ("transitive_build_keys" -> Writer.write(input.transitiveBuildKeys)) ::
          ("context" -> Writer.write(input.context)) ::
          Nil
      )
    }

  def parseManifestString(
      content: String
  ): Either[String, Manifest] =
    Json.parserFile.parseAll(content).leftMap(_.toString).flatMap { json =>
      manifestReader.read(Json.Path.Root, json).leftMap { case (msg, _, path) =>
        s"$msg at $path"
      }
    }

  def parseMetadataString(
      content: String
  ): Either[String, Metadata] =
    Json.parserFile.parseAll(content).leftMap(_.toString).flatMap { json =>
      metadataReader.read(Json.Path.Root, json).leftMap { case (msg, _, path) =>
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
      dependency.dependencies.sorted,
      transitiveBuildKeys.sorted,
      context.copy(
        os = normalizeOs(context.os),
        arch = normalizeArch(context.arch),
        toolchainFamily = normalizeToolchainFamily(context.toolchainFamily),
        relevantEnv = context.relevantEnv.toList.sortBy(_._1).toMap
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
