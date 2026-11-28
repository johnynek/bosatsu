package dev.bosatsu.tool_command

import cats.data.Validated
import cats.syntax.all._
import com.monovore.decline.{Argument, Opts}

/** The three declared files that let `compile-packages` and
  * `validate-graph` check a mixed graph of local groups and imported
  * libraries. They are given together or not at all.
  */
private[tool_command] final case class ImportedScopeInputs[Path](
    scopeMap: Path,
    dependencyContext: Path,
    toolchainManifest: Path
) {
  def files: List[Path] = List(scopeMap, dependencyContext, toolchainManifest)
}

private[tool_command] object ImportedScopeInputs {
  val dependencyContextHelp: String = "version-1 declared library artifact context"

  def opts[Path: Argument](mode: String): Opts[Option[ImportedScopeInputs[Path]]] =
    (
      Opts.option[Path]("scope-map", "version-1 caller-relative package bindings").orNone,
      Opts.option[Path]("dependency-context", dependencyContextHelp).orNone,
      Opts.option[Path]("toolchain-manifest", "selected toolchain and core manifest").orNone
    ).tupled.mapValidated {
      case (None, None, None) => Validated.valid(None)
      case (Some(scopes), Some(context), Some(manifest)) =>
        Validated.valid(Some(ImportedScopeInputs(scopes, context, manifest)))
      case _ => Validated.invalidNel(
        s"$mode requires --scope-map, --dependency-context, and --toolchain-manifest together"
      )
    }
}
