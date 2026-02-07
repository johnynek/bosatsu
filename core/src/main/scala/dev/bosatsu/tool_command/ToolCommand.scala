package dev.bosatsu.tool_command

import cats.MonoidK
import com.monovore.decline.Opts
import dev.bosatsu.PlatformIO
import dev.bosatsu.tool.{CommonOpts, Output}

object ToolCommand {
  def opts[F[_], Path](
      platformIO: PlatformIO[F, Path]
  ): Opts[F[Output[Path]]] = {
    val commonOpts = new CommonOpts(platformIO)

    MonoidK[Opts].combineAllK(
      CheckCommand.opts(platformIO, commonOpts) ::
        TestCommand.opts(platformIO, commonOpts) ::
        EvalCommand.opts(platformIO, commonOpts) ::
        JsonCommand.opts(platformIO, commonOpts) ::
        TranspileCommand.opts(platformIO, commonOpts) ::
        ShowCommand.opts(platformIO, commonOpts) ::
        DepsCommand.opts(platformIO, commonOpts) ::
        ExtractIfaceCommand.opts(platformIO) ::
        AssembleCommand.opts(platformIO) ::
        Nil
    )
  }
}
