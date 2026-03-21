package dev.bosatsu

import Identifier.Bindable

case class Program[+T, +D, +S](
    types: T,
    lets: List[(Bindable, RecursionKind, D)],
    externalDefs: List[Bindable],
    from: S
) {

  private lazy val letMap: Map[Bindable, (RecursionKind, D)] =
    lets.iterator.map { case (n, r, d) => (n, (r, d)) }.toMap

  def getLet(name: Bindable): Option[(RecursionKind, D)] = letMap.get(name)
}

object Program {
  sealed trait Metadata derives CanEqual
  object Metadata {
    final case class WithSource[+A](
        source: A,
        recursionLints: List[RecursionCheck.Lint]
    ) extends Metadata

    final case class Compiled(
        recursionLints: List[RecursionCheck.Lint]
    ) extends Metadata
  }

  def recursionLints(from: Any): List[RecursionCheck.Lint] =
    from match {
      case Metadata.WithSource(_, lints) => lints
      case Metadata.Compiled(lints)      => lints
      case _                             => Nil
    }
}
