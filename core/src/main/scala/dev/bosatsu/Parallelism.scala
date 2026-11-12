package dev.bosatsu

/** How many workers a `Par` pool keeps running at once, always at least 1. */
opaque type Parallelism = Int

object Parallelism {
  val one: Parallelism = 1

  def fromInt(n: Int): Option[Parallelism] =
    if (n >= 1) Some(n) else None

  def availableProcessors: Parallelism =
    Runtime.getRuntime.availableProcessors().max(1)

  extension (p: Parallelism) def toInt: Int = p
}
