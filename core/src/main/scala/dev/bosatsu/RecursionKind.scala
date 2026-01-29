package dev.bosatsu

import cats.Eq

sealed abstract class RecursionKind(val isRecursive: Boolean) derives CanEqual

object RecursionKind {
  implicit val eqRecursionKind: Eq[RecursionKind] =
    Eq.fromUniversalEquals
  def recursive(isRec: Boolean): RecursionKind =
    if (isRec) Recursive else NonRecursive

  case object NonRecursive extends RecursionKind(false)
  case object Recursive extends RecursionKind(true)
}
