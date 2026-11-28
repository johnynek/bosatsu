package dev.bosatsu

import dev.bosatsu.Identifier.Bindable

case class Externals(toMap: Map[(PackageName, Bindable), FfiCall]) {
  def add(pn: PackageName, value: Bindable, f: FfiCall): Externals =
    Externals(toMap + ((pn, value) -> f))

  /** Registers `f` under `key`, such as `ReservedPackage.Prog.external("pure")`
    * or the compile-time checked `PredefName.external("add")`
    */
  def add(key: (PackageName, Bindable), f: FfiCall): Externals =
    Externals(toMap + (key -> f))
}

object Externals {
  def empty: Externals = Externals(Map.empty)
}
