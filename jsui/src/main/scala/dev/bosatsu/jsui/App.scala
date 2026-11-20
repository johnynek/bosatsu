package dev.bosatsu.jsui

import cats.effect.IO
import cats.effect.kernel.Resource
import dev.bosatsu.CompilerBuildInfo

class App(val buildInfo: CompilerBuildInfo)
    extends ff4s.App[IO, State, Action]
    with View {
  def store: Resource[IO, ff4s.Store[IO, State, Action]] =
    new Store(buildInfo).value
}
