package dev.bosatsu.tool

import cats.data.Validated
import com.monovore.decline.Opts

sealed abstract class GraphOutput derives CanEqual
object GraphOutput {
  case object Dot extends GraphOutput
  case object Json extends GraphOutput

  val jsonOrDot: Opts[GraphOutput] =
    Opts
      .option[String]("graph_format", "format of graph, either json or dot")
      .mapValidated {
        case "json" => Validated.valid(Json)
        case "dot"  => Validated.valid(Dot)
        case other  =>
          Validated.invalidNel(s"\"$other\" invalid, expected json or dot")
      }
      .withDefault(Json)
}
