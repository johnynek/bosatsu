package org.bykn.bosatsu

import Identifier.Bindable

case class Program[+T, +D, +S](
    types: T,
    lets: List[(Bindable, RecursionKind, D)],
    externalDefs: List[Bindable],
    from: S
) {

  private[this] lazy val letMap: Map[Bindable, (RecursionKind, D)] =
    lets.iterator.map { case (n, r, d) => (n, (r, d)) }.toMap

  def getLet(name: Bindable): Option[(RecursionKind, D)] = letMap.get(name)
}
