package org.bykn.bosatsu.jsui

import cats.effect.IO
import cats.effect.kernel.Resource

class App extends ff4s.App[IO, State, Action] {
  def store: Resource[IO,ff4s.Store[IO,State,Action]] = Store.value
  def view: dsl.V = View(dsl)
}

object MainApp extends ff4s.IOEntryPoint(new App)