package org.bykn.bosatsu

import cats.effect.IO
import java.nio.file.Path

object PathModule extends MainModule[IO, Path](IOPlatformIO)
