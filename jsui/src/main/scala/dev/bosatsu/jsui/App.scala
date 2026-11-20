package dev.bosatsu.jsui

import cats.effect.IO
import cats.effect.kernel.Resource
import dev.bosatsu.{BuildInfo, CompilerBuildInfo}

class App(val buildInfo: CompilerBuildInfo)
    extends ff4s.App[IO, State, Action]
    with View {
  def store: Resource[IO, ff4s.Store[IO, State, Action]] =
    new Store(buildInfo).value
}

object MainApp extends ff4s.IOEntryPoint(new App(BuildInfo))
