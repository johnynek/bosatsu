package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{LocationMap, MainModule}

object TestCommand {
  def opts[IO[_], Path](
      module: MainModule[IO, Path]
  ): Opts[module.MainCommand] = {
    import module.MainCommand._
    import LocationMap.Colorize

    val testOpt =
      (Inputs.runtimeOpts, testIdentifiersOpt, Colorize.optsConsoleDefault)
        .mapN(RunTests(_, _, _))

    Opts.subcommand("test", "test a set of bosatsu modules")(testOpt)
  }
}
