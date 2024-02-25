package org.bykn.bosatsu.jsui

import cats.effect.IO
import cats.effect.kernel.Resource

class App extends ff4s.App[IO, State, Action] with View {
  def store: Resource[IO, ff4s.Store[IO, State, Action]] = Store.value
}

object MainApp extends ff4s.IOEntryPoint(new App)
