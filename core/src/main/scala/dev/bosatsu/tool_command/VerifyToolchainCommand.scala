package dev.bosatsu.tool_command

import cats.data.NonEmptyList
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{BuildInfo, PlatformIO}
import dev.bosatsu.tool.{CliException, Output, ToolchainManifest}
import org.typelevel.paiges.Doc

object VerifyToolchainCommand {
  private def checkStampDoesNotAliasInputs[F[_], P](
      platform: PlatformIO[F, P],
      stamp: P,
      inputs: List[P]
  ): F[Unit] = {
    import platform.{moduleIOMonad, showPath}
    platform.pendingOutputPath(stamp).flatMap(platform.existingFileKey).flatMap {
      case None => moduleIOMonad.unit
      case Some(stampKey) =>
        inputs.traverse(path => platform.existingFileKey(path).map(key => path -> key)).flatMap { keys =>
          keys.collectFirst {
            case (path, Some(inputKey)) if platform.fileKeyHash.eqv(stampKey, inputKey) => path
          } match {
            case Some(path) =>
              moduleIOMonad.raiseError(CliException.Basic(
                show"verification stamp $stamp refers to requested input $path"
              ))
            case None => moduleIOMonad.unit
          }
        }
    }
  }

  private def requestedInputs[P](
      manifest: P,
      compilerJar: P,
      core: ToolchainManifest.CoreSelection[P],
      runtimeArchive: Option[P]
  ): List[P] = {
    val coreInputs = core match {
      case ToolchainManifest.CoreSelection.None        => Nil
      case ToolchainManifest.CoreSelection.Binary(path) => List(path)
      case ToolchainManifest.CoreSelection.Sources(paths) => paths.toList
    }
    manifest :: compilerJar :: (coreInputs ++ runtimeArchive.toList)
  }

  def opts[F[_], P](platform: PlatformIO[F, P]): Opts[F[Output[P]]] = {
    import platform.{moduleIOMonad, pathArg}
    val manifest =
      Opts.option[P]("manifest", "caller-trusted detached toolchain manifest")
    val compiler =
      Opts.option[P]("compiler-jar", "compiler jar whose bytes are verified")
    val coreLibrary =
      Opts.option[P]("core-library", "binary core library to verify").orNone
    val coreSources =
      Opts.options[P]("core-source", "core source file to verify (repeatable)").orEmpty
    val runtime =
      Opts.option[P]("runtime-archive", "compressed C runtime archive to verify").orNone
    val output =
      Opts.option[P]("out", "success stamp written after verification")

    Opts.subcommand(
      "verify-toolchain",
      "verify requested files against a launcher-authenticated manifest; authenticate this jar before execution"
    ) {
      (manifest, compiler, coreLibrary, coreSources, runtime, output).mapN {
        (manifestPath, compilerJar, binaryCore, sourceFiles, runtimeArchive, stamp) =>
          val coreSelection = (binaryCore, sourceFiles.toList) match {
            case (Some(_), _ :: _) =>
              Left(CliException.Basic(
                "choose either --core-library or --core-source, not both"
              ))
            case (Some(path), Nil) => Right(ToolchainManifest.CoreSelection.Binary(path))
            case (None, head :: tail) =>
              Right(
                ToolchainManifest.CoreSelection.Sources(NonEmptyList(head, tail))
              )
            case (None, Nil) => Right(ToolchainManifest.CoreSelection.None)
          }

          for {
            core <- moduleIOMonad.fromEither(coreSelection)
            selected <- ToolchainManifest.read(platform, manifestPath)
            _ <- ToolchainManifest.verify(
              platform,
              selected,
              compilerJar,
              core,
              runtimeArchive,
              BuildInfo.cRuntimeArchiveHash
            )
            _ <- checkStampDoesNotAliasInputs(
              platform,
              stamp,
              requestedInputs(manifestPath, compilerJar, core, runtimeArchive)
            )
          } yield Output.Basic(Doc.text("ok"), Some(stamp)): Output[P]
      }
    }
  }
}
