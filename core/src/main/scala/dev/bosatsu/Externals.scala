package dev.bosatsu

import dev.bosatsu.Identifier.Bindable

case class Externals(toMap: Map[(PackageName, Bindable), FfiCall]) {
  def add(pn: PackageName, value: Bindable, f: FfiCall): Externals =
    Externals(toMap + ((pn, value) -> f))

  def add(pn: PackageName, value: String, f: FfiCall): Externals =
    Externals(toMap + ((pn, Identifier.Name(value)) -> f))
}

object Externals {
  def empty: Externals = Externals(Map.empty)
}
