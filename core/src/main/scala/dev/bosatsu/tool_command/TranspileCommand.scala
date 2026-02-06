package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{LocationMap, MainModule}

object TranspileCommand {
  def opts[IO[_], Path](
      module: MainModule[IO, Path]
  ): Opts[module.MainCommand] = {
    import module.MainCommand._
    import LocationMap.Colorize

    val transpileOpt = (
      Inputs.runtimeOpts,
      Colorize.optsConsoleDefault,
      transOpt
    )
      .mapN(module.MainCommand.TranspileCommand(_, _, _))

    Opts.subcommand("transpile", "transpile bosatsu into another language")(
      transpileOpt
    )
  }
}
