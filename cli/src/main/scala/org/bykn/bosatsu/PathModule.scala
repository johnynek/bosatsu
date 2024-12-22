package org.bykn.bosatsu

import cats.effect.{IO, Resource}
import java.nio.file.{Path => JPath}

object PathModule extends MainModule[IO, JPath](IOPlatformIO) { self =>
  type Path = JPath

  val parResource: Resource[IO, Par.EC] =
    Resource.make(IO(Par.newService()))(es => IO(Par.shutdownService(es)))
      .map(Par.ecFromService(_))

  def withEC[A](fn: Par.EC => IO[A]): IO[A] =
    parResource.use(fn)
}
