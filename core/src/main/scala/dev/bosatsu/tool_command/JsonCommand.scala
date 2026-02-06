package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{LocationMap, MainModule}

object JsonCommand {
  def opts[IO[_], Path](
      module: MainModule[IO, Path]
  ): Opts[module.MainCommand] = {
    import module.MainCommand._
    import module.platformIO.pathArg
    import LocationMap.Colorize

    def toJsonOpt(modeOpt: Opts[JsonMode]) =
      (
        Inputs.runtimeOpts,
        modeOpt,
        mainIdentifierOpt,
        outputPathOpt.orNone,
        Colorize.optsConsoleDefault
      )
        .mapN(ToJson(_, _, _, _, _))

    val input: Opts[JsonInput] =
      Opts
        .option[Path]("json_input", help = "json input path")
        .map(JsonInput.FromPath(_))
        .orElse(
          Opts
            .option[String]("json_string", help = "json string argument")
            .map(JsonInput.FromString(_))
        )

    val applyInput = input.map(JsonMode.Apply(_))
    val traverseInput = input.map(JsonMode.Traverse(_))

    val subs = Opts
      .subcommand("write", "write a bosatsu expression into json")(
        toJsonOpt(Opts(JsonMode.Write))
      )
      .orElse(
        Opts.subcommand(
          "apply",
          "apply a bosatsu function to a json array argument list"
        )(toJsonOpt(applyInput))
      )
      .orElse(
        Opts.subcommand(
          "traverse",
          "apply a bosatsu function to each element of an array or each value in an object"
        )(toJsonOpt(traverseInput))
      )

    Opts.subcommand("json", "json writing and transformation tools")(subs)
  }
}
