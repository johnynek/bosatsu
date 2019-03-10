package org.bykn.bosatsu

sealed abstract class RecursionKind(val isRecursive: Boolean)

object RecursionKind {
  case object NonRecursive extends RecursionKind(false)
  case object Recursive extends RecursionKind(true)
}
