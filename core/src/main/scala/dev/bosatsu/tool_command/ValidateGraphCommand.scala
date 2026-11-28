package dev.bosatsu.tool_command

import cats.data.Validated
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{PackageSignatures, PlatformIO}
import dev.bosatsu.graph.CanPromise
import dev.bosatsu.tool.{
  GroupGraph,
  LibraryInspection,
  Output,
  PackageBindings,
  ToolchainManifest
}
import org.typelevel.paiges.Doc

object ValidateGraphCommand {
  private final case class MixedInputs[Path](
      imported: ImportedScopeInputs[Path],
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
    import platform.{moduleIOMonad, showPath}
    platform.withEC {
      given CanPromise[F] = platform.canPromiseF
      for {
        scopeMap <- PackageBindings.read(platform, inputs.imported.scopeMap, inputs.directInterfaces)
        dependencyContext <- LibraryInspection.readDependencyContext(
          platform, inputs.imported.dependencyContext
        )
        toolchain <- ToolchainManifest.read(platform, inputs.imported.toolchainManifest)
        inputPaths = root :: inputs.imported.files ::: nodes ::: inputs.directInterfaces :::
          dependencyContext.libraries.map(_.path) :::
          scopeMap.imports.valuesIterator.map(_.exposuresPath).toList
        _ <- OutputPathChecks.checkOutputFiles(platform, inputPaths, List(output))
        importedInputs <- LibraryInspection.loadImportedInputs(
          platform,
          scopeMap,
          dependencyContext,
          toolchain
        )
        importedSignaturePaths = scopeMap.imports.valuesIterator.map(_.signature.path).toList
        localSignaturePaths = inputs.directInterfaces.filterNot(path =>
          importedSignaturePaths.exists(imported =>
            platform.pathOrdering.compare(path, imported) == 0
          )
        )
        localSignatureBundles <- localSignaturePaths.traverse { path =>
          platform.readInterfaceBundle(path).map(bundle => PackageSignatures.Input(path, None) -> bundle)
        }
        localInterfaces <- moduleIOMonad.fromTry(PackageSignatures.localInterfaces(localSignatureBundles))
        _ <- GroupGraph.validateMixed(
          root,
          nodes.toSet,
          scope,
          scopeMap,
          importedInputs.publicIndex,
          platform.pathToString,
          localInterfaces
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
      val directInterfaces = Opts.options[P]("direct-interface", "declared imported signature (repeatable)").orEmpty
      val output = Opts.option[P]("out", "write a validation stamp on success")

      val mixedInputs: Opts[Option[MixedInputs[P]]] =
        (ImportedScopeInputs.opts[P]("mixed validation"), directInterfaces).tupled.mapValidated {
          case (None, interfaces) if interfaces.isEmpty => Validated.valid(None)
          case (Some(imported), interfaces) => Validated.valid(Some(MixedInputs(imported, interfaces)))
          case (None, _) =>
            Validated.invalidNel("--direct-interface requires mixed validation inputs")
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
