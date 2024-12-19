package org.bykn.bosatsu.tool

import cats.{effect => ce}
import cats.effect.{IO, Resource}
import fs2.io.file.Path
import org.bykn.bosatsu.{Par, MainModule, Fs2PlatformIO}

object Fs2Module extends MainModule[IO, Path](Fs2PlatformIO) { self =>
  val parResource: Resource[IO, Par.EC] =
    Resource.make(IO(Par.newService()))(es => IO(Par.shutdownService(es)))
      .map(Par.ecFromService(_))

  def withEC[A](fn: Par.EC => IO[A]): IO[A] =
    parResource.use(fn)

  def fromToolExit(ec: ExitCode): ce.ExitCode =
    ec match {
      case ExitCode.Success => ce.ExitCode.Success
      case ExitCode.Error => ce.ExitCode.Error
    }

  def report(io: IO[Output[Path]]): IO[ce.ExitCode] =
    io.attempt.flatMap {
      case Right(out) => reportOutput(out).map(fromToolExit)
      case Left(err)  => reportException(err).as(ce.ExitCode.Error)
    }

  def reportException(ex: Throwable): IO[Unit] =
    mainExceptionToString(ex) match {
      case Some(msg) =>
        IO.consoleForIO.errorln(msg)
      case None =>
        IO.consoleForIO.errorln("unknown error:\n") *>
          IO.blocking(ex.printStackTrace(System.err))
    }

}
