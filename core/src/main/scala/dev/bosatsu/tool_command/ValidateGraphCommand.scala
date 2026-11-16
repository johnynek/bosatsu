package dev.bosatsu.tool_command

import cats.data.Validated
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.PlatformIO
import dev.bosatsu.graph.CanPromise
import dev.bosatsu.tool.{
  CliException,
  GroupGraph,
  LibraryInspection,
  Output,
  PackageBindings,
  ToolchainManifest
}
import org.typelevel.paiges.Doc

object ValidateGraphCommand {
  private final case class MixedInputs[Path](
      scopeMap: Path,
      dependencyContext: Path,
      toolchainManifest: Path,
      directInterfaces: List[Path]
  )

  private def runMixed[F[_], Path](
      platform: PlatformIO[F, Path],
      root: Path,
      nodes: List[Path],
      scope: GroupGraph.Scope,
      inputs: MixedInputs[Path],
      output: Path
  ): F[Output[Path]] = {
    import platform.moduleIOMonad
    platform.withEC {
      given CanPromise[F] = platform.canPromiseF
      for {
        scopeMap <- PackageBindings.read(platform, inputs.scopeMap, inputs.directInterfaces)
        dependencyContext <- LibraryInspection.readDependencyContext(platform, inputs.dependencyContext)
        toolchain <- ToolchainManifest.read(platform, inputs.toolchainManifest)
        inputPaths = List(root, inputs.scopeMap, inputs.dependencyContext, inputs.toolchainManifest) :::
          nodes ::: inputs.directInterfaces ::: dependencyContext.libraries.map(_.path) :::
          scopeMap.imports.valuesIterator.map(_.publicIndexPath).toList
        _ <- OutputPathChecks.checkOutputFiles(platform, inputPaths, List(output))
        importedInputs <- LibraryInspection.loadImportedInputs(
          platform,
          scopeMap,
          dependencyContext,
          toolchain
        )
        checked <- moduleIOMonad.fromEither(
          PackageBindings.validateAgainstInspection(
            scopeMap,
            importedInputs.publicIndex,
            scopeMap.dependencies
          )
            .leftMap(CliException.Basic(_))
        )
        _ <- GroupGraph.validateMixed(
          root,
          nodes.toSet,
          scope,
          checked,
          importedInputs.graph,
          platform.pathToString
        )(GroupGraph.readNode(platform, _, _))
      } yield Output.Basic(Doc.text("ok"), Some(output)): Output[Path]
    }
  }

  def opts[F[_], P](platform: PlatformIO[F, P]): Opts[F[Output[P]]] = {
    import platform.{moduleIOMonad, pathArg}
    Opts.subcommand("validate-graph", "check local package groups and imported library scopes") {
      val root = Opts.option[P]("root", "root group manifest")
      val nodes = Opts.options[P]("node", "declared dependency manifest (repeatable)").orEmpty
      val local = Opts.flag("local", "check the root and its immediate dependency groups").orFalse
      val scopeMap = Opts.option[P]("scope-map", "checked member-relative imported bindings").orNone
      val dependencyContext =
        Opts.option[P]("dependency-context", "version-1 declared library artifact context").orNone
      val toolchainManifest =
        Opts.option[P]("toolchain-manifest", "selected toolchain and core manifest").orNone
      val directInterfaces = Opts.options[P]("direct-interface", "declared imported signature (repeatable)").orEmpty
      val output = Opts.option[P]("out", "write a validation stamp on success")

      val mixedInputs: Opts[Option[MixedInputs[P]]] =
        (scopeMap, dependencyContext, toolchainManifest, directInterfaces).tupled.mapValidated {
          case (None, None, None, interfaces) if interfaces.isEmpty => Validated.valid(None)
          case (Some(scopes), Some(context), Some(manifest), interfaces) =>
            Validated.valid(Some(MixedInputs(scopes, context, manifest, interfaces)))
          case (None, None, None, _) =>
            Validated.invalidNel("--direct-interface requires mixed validation inputs")
          case _ => Validated.invalidNel(
            "mixed validation requires --scope-map, --dependency-context, and --toolchain-manifest together"
          )
        }

      (root, nodes, local, mixedInputs, output).mapN { (rootPath, nodePaths, isLocal, mixed, outputPath) =>
        val scope = if (isLocal) GroupGraph.Scope.Local else GroupGraph.Scope.Transitive
        mixed match {
          case Some(inputs) => runMixed(platform, rootPath, nodePaths, scope, inputs, outputPath)
          case None =>
            GroupGraph.validate(rootPath, nodePaths.toSet, scope, platform.pathToString)(
              GroupGraph.readNode(platform, _, _)
            ).as(Output.Basic(Doc.text("ok"), Some(outputPath)): Output[P])
        }
      }
    }
  }
}
