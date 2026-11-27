package dev.bosatsu.tool

import cats.effect.IO
import fs2.io.file.Path
import dev.bosatsu.{BuildInfo, MainModule, Fs2PlatformIO}

/** The cliJS module, running on Node with this build's info.
  *
  * This is the only cliJS file that uses the generated `BuildInfo`, which
  * changes with every commit, so the rest of cliJS doesn't depend on it.
  */
object Fs2Module
    extends MainModule[IO, Path](new Fs2PlatformIO(BuildInfo, None))
