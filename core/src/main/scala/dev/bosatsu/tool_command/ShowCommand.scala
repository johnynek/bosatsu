package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{LocationMap, MainModule}

object ShowCommand {
  def opts[IO[_], Path](
      module: MainModule[IO, Path]
  ): Opts[module.MainCommand] = {
    import module.MainCommand._
    import LocationMap.Colorize

    Opts.subcommand("show", "show compiled packages")(
      (Inputs.showOpts, outputPathOpt.orNone, Colorize.optsConsoleDefault)
        .mapN(Show(_, _, _))
    )
  }
}
