package org.bykn.bosatsu.tool

import cats.effect.{IO, Resource}
import fs2.io.file.Path
import org.bykn.bosatsu.{Par, MainModule, Fs2PlatformIO}

object Fs2Module extends MainModule[IO, Path](Fs2PlatformIO) { self =>
  val parResource: Resource[IO, Par.EC] =
    Resource.make(IO(Par.newService()))(es => IO(Par.shutdownService(es)))
      .map(Par.ecFromService(_))

  def withEC[A](fn: Par.EC => IO[A]): IO[A] =
    parResource.use(fn)
}
