package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.PlatformIO
import dev.bosatsu.tool.{GroupGraph, Output}
import org.typelevel.paiges.Doc

object ValidateGraphCommand {
  def opts[F[_], P](platform: PlatformIO[F, P]): Opts[F[Output[P]]] = {
    import platform.{moduleIOMonad, pathArg}
    Opts.subcommand("validate-graph", "check the membership and edges of local package groups") {
      (
        Opts.option[P]("root", "root group manifest"),
        Opts.options[P]("node", "declared dependency manifest (repeatable)").orEmpty,
        Opts.flag("local", "check the root and its immediate dependency groups").orFalse,
        Opts.option[P]("out", "write a validation stamp on success")
      ).mapN { (root, nodes, local, output) =>
        val scope = if (local) GroupGraph.Scope.Local else GroupGraph.Scope.Transitive
        GroupGraph.validate(root, nodes.toSet, scope, platform.pathToString)(
          GroupGraph.readNode(platform, _, _)
        ).as(Output.Basic(Doc.text("ok"), Some(output)): Output[P])
      }
    }
  }
}
