package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.MainModule
import dev.bosatsu.tool.Output

object ExtractIfaceCommand {
  def opts[IO[_], Path](
      module: MainModule[IO, Path]
  ): Opts[module.MainCommand] = {
    import module.MainCommand.FromOutput
    import module.platformIO._

    val inputOpt =
      Opts.option[Path]("input", help = "input .bosatsu_lib file")
    val outputOpt =
      Opts.option[Path](
        "output",
        help = "output .bosatsu_ifacelib file (default: <input>.bosatsu_ifacelib)"
      ).orNone

    Opts.subcommand(
      "extract-iface",
      "convert a .bosatsu_lib to interface-only .bosatsu_ifacelib"
    ) {
      (inputOpt, outputOpt).mapN { (input, output) =>
        val run =
          for {
            lib <- readLibrary(input)
            outPath <- output match {
              case Some(path) => moduleIOMonad.pure(path)
              case None       =>
                val inStr = pathToString(input)
                val outStr =
                  if (inStr.endsWith(".bosatsu_lib"))
                    inStr.stripSuffix(".bosatsu_lib") + ".bosatsu_ifacelib"
                  else s"${inStr}.bosatsu_ifacelib"
                pathF(outStr)
            }
            ifaceOnly = lib.arg.copy(internalPackages = Nil)
          } yield (Output.Library(ifaceOnly, outPath): Output[Path])

        FromOutput("tool extract-iface", run)
      }
    }
  }
}
