package dev.bosatsu

case class Externals(toMap: Map[(PackageName, String), FfiCall]) {
  def add(pn: PackageName, value: String, f: FfiCall): Externals =
    Externals(toMap + ((pn, value) -> f))

  def ++(that: Externals): Externals =
    Externals(toMap ++ that.toMap)
}

object Externals {
  def empty: Externals = Externals(Map.empty)
}
