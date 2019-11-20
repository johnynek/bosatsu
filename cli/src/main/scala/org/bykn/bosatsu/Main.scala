package org.bykn.bosatsu

import scala.util.{ Failure, Success, Try }

import cats.implicits._

object Main {
  def main(args: Array[String]): Unit =
    PathModule.run(args.toList) match {
      case Right(out) =>
        val run = out.flatMap(PathModule.reportOutput(_))
        Try(run.unsafeRunSync) match {
          case Failure(err) =>
            // TODO use some verbosity flag to modulate this
            //err.printStackTrace
            System.err.println(err.getMessage)
            System.exit(1)
          case Success(()) =>
            System.exit(0)
        }
      case Left(help) =>
        System.err.println(help.toString)
        System.exit(1)
    }
}
