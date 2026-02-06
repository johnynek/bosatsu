package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{LocationMap, MainModule}

object EvalCommand {
  def opts[IO[_], Path](
      module: MainModule[IO, Path]
  ): Opts[module.MainCommand] = {
    import module.MainCommand._
    import LocationMap.Colorize

    val evalOpt =
      (Inputs.runtimeOpts, mainIdentifierOpt, Colorize.optsConsoleDefault)
        .mapN(Evaluate(_, _, _))

    Opts.subcommand("eval", "evaluate an expression and print the output")(
      evalOpt
    )
  }
}
