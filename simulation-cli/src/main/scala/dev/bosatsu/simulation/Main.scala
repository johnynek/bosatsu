package dev.bosatsu.simulation

import cats.effect.{ExitCode, IO, IOApp}

/**
 * Entry point for the bosatsu-sim CLI.
 *
 * This CLI generates interactive HTML demos from .bosatsu files
 * with provenance tracking ("Why?" buttons, "What if?" toggles, parameter sweeps).
 */
object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    SimulationCommand.parse(args) match {
      case Right(cmd) =>
        cmd.run.as(ExitCode.Success).handleErrorWith { err =>
          IO(System.err.println(s"Error: ${err.getMessage}")).as(ExitCode.Error)
        }
      case Left(help) =>
        IO(System.err.println(help.toString)).as(ExitCode.Error)
    }
}
