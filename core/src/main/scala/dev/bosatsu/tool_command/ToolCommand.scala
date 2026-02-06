package dev.bosatsu.tool_command

import cats.MonoidK
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.MainModule

object ToolCommand {
  def opts[IO[_], Path](
      module: MainModule[IO, Path]
  ): Opts[module.MainCommand] =
    MonoidK[Opts].combineAllK(
      CheckCommand.opts(module) ::
        TestCommand.opts(module) ::
        EvalCommand.opts(module) ::
        JsonCommand.opts(module) ::
        TranspileCommand.opts(module) ::
        ShowCommand.opts(module) ::
        DepsCommand.opts(module) ::
        ExtractIfaceCommand.opts(module) ::
        AssembleCommand.opts(module) ::
        Nil
    )
}
