package dev.bosatsu.tool_command

import cats.MonoidK
import com.monovore.decline.Opts
import dev.bosatsu.PlatformIO
import dev.bosatsu.tool.{CommonOpts, Output}

object ToolCommand {
  def opts[F[_], Path](
      platformIO: PlatformIO[F, Path],
      evalPassthroughArgs: List[String] = Nil
  ): Opts[F[Output[Path]]] = {
    val commonOpts = new CommonOpts(platformIO)

    // Decline help preserves the left-to-right construction order, so keep
    // this list aligned with the intended tool workflow surface.
    MonoidK[Opts].combineAllK(
      CheckCommand.opts(platformIO, commonOpts) ::
        TestCommand.opts(platformIO, commonOpts) ::
        TranspileCommand.opts(platformIO, commonOpts) ::
        JsonCommand.opts(platformIO, commonOpts) ::
        DocCommand.opts(platformIO, commonOpts) ::
        EvalCommand.opts(platformIO, commonOpts, evalPassthroughArgs) ::
        DepsCommand.opts(platformIO, commonOpts) ::
        ShowCommand.opts(platformIO, commonOpts) ::
        AssembleCommand.opts(platformIO) ::
        ExtractIfaceCommand.opts(platformIO) ::
        Nil
    )
  }
}
