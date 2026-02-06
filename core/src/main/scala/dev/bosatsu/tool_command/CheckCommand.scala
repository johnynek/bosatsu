package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{LocationMap, MainModule}

object CheckCommand {
  def opts[IO[_], Path](
      module: MainModule[IO, Path]
  ): Opts[module.MainCommand] = {
    import module.MainCommand._
    import LocationMap.Colorize

    val checkOpt = (
      Inputs.compileOpts,
      outputPathOpt.orNone,
      interfaceOutputPathOpt.orNone,
      Colorize.optsConsoleDefault
    )
      .mapN(Check(_, _, _, _))

    Opts.subcommand("check", "type check a set of packages")(checkOpt)
  }
}
