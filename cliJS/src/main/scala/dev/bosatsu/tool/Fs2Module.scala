package dev.bosatsu.tool

import cats.effect.IO
import fs2.io.file.Path
import dev.bosatsu.{MainModule, Fs2PlatformIO}

object Fs2Module extends MainModule[IO, Path](Fs2PlatformIO)
