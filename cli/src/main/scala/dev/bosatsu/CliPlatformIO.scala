package dev.bosatsu

/** The JVM platform the cli entry point runs on, with this build's info.
  *
  * This is the only cli file that uses the generated `BuildInfo`, which
  * changes with every commit, so the rest of cli doesn't depend on it.
  */
object CliPlatformIO extends IOPlatformIO(BuildInfo)
