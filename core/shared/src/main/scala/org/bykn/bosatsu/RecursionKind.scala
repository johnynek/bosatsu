package org.bykn.bosatsu

sealed abstract class RecursionKind(val isRecursive: Boolean)

object RecursionKind {
  def recursive(isRec: Boolean): RecursionKind =
    if (isRec) Recursive else NonRecursive

  case object NonRecursive extends RecursionKind(false)
  case object Recursive extends RecursionKind(true)
}
