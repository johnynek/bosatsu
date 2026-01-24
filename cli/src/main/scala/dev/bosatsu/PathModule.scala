package dev.bosatsu

import cats.effect.IO
import java.nio.file.Path

object PathModule extends MainModule[IO, Path](IOPlatformIO)
