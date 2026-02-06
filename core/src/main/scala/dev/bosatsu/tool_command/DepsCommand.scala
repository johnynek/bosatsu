package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{LocationMap, MainModule}
import dev.bosatsu.tool.GraphOutput

object DepsCommand {
  def opts[IO[_], Path](
      module: MainModule[IO, Path]
  ): Opts[module.MainCommand] = {
    import module.MainCommand._
    import LocationMap.Colorize

    Opts.subcommand("deps", "emit a graph description of dependencies")(
      (
        Inputs.depsOpts,
        outputPathOpt.orNone,
        Colorize.optsConsoleDefault,
        GraphOutput.jsonOrDot
      )
        .mapN(Deps(_, _, _, _))
    )
  }
}
