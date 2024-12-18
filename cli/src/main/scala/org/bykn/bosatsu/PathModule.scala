package org.bykn.bosatsu

import cats.effect.ExitCode
import cats.effect.{IO, Resource}
import java.nio.file.{Path => JPath}
import org.bykn.bosatsu.tool.Output

object PathModule extends MainModule[IO, JPath](IOPlatformIO) { self =>
  type Path = JPath

  val parResource: Resource[IO, Par.EC] =
    Resource.make(IO(Par.newService()))(es => IO(Par.shutdownService(es)))
      .map(Par.ecFromService(_))

  def withEC[A](fn: Par.EC => IO[A]): IO[A] =
    parResource.use(fn)

  def fromToolExit(ec: tool.ExitCode): ExitCode =
    ec match {
      case tool.ExitCode.Success => ExitCode.Success
      case tool.ExitCode.Error => ExitCode.Error
    }

  def report(io: IO[Output[JPath]]): IO[ExitCode] =
    io.attempt.flatMap {
      case Right(out) => reportOutput(out).map(fromToolExit)
      case Left(err)  => reportException(err).as(ExitCode.Error)
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
