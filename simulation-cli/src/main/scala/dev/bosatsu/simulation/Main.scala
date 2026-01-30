package dev.bosatsu.simulation

import cats.effect.{ExitCode, IO, IOApp}
import com.monovore.decline._

/**
 * Entry point for the bosatsu-sim CLI.
 *
 * Supports two modes:
 * - `sim`: Generate provenance-tracking simulations (Why?, What if?)
 * - `ui`: Generate reactive UI with direct DOM updates from Bosatsu/UI code
 *
 * Usage:
 *   bosatsu-sim sim input.bosatsu config.sim.bosatsu -o output.html
 *   bosatsu-sim ui input.bosatsu -o output.html
 */
object Main extends IOApp {

  sealed trait AppCommand
  case class SimCommand(cmd: SimulationCommand) extends AppCommand
  case class UICmd(cmd: UICommand) extends AppCommand

  val simSubcommand: Opts[AppCommand] =
    Opts.subcommand("sim", "Generate provenance-tracking simulation HTML")(
      SimulationCommand.opts.map(SimCommand(_))
    )

  val uiSubcommand: Opts[AppCommand] =
    Opts.subcommand("ui", "Generate reactive UI HTML from Bosatsu/UI code")(
      UICommand.opts.map(UICmd(_))
    )

  val command: Command[AppCommand] = Command(
    name = "bosatsu-sim",
    header = "Generate interactive HTML from Bosatsu files"
  )(simSubcommand orElse uiSubcommand)

  def run(args: List[String]): IO[ExitCode] =
    command.parse(args) match {
      case Right(SimCommand(cmd)) =>
        cmd.run.as(ExitCode.Success).handleErrorWith { err =>
          IO(System.err.println(s"Error: ${err.getMessage}")).as(ExitCode.Error)
        }
      case Right(UICmd(cmd)) =>
        cmd.run.as(ExitCode.Success).handleErrorWith { err =>
          IO(System.err.println(s"Error: ${err.getMessage}")).as(ExitCode.Error)
        }
      case Left(help) =>
        IO(System.err.println(help.toString)).as(ExitCode.Error)
    }
}
