package dev.bosatsu.jsui

import dev.bosatsu.BuildInfo

/** The browser entry point.
  *
  * This is the only jsui file that uses the generated `BuildInfo`, which
  * changes with every commit, so the rest of jsui doesn't depend on it.
  */
object MainApp extends ff4s.IOEntryPoint(new App(BuildInfo))
