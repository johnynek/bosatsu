package dev.bosatsu.smt

import dev.bosatsu.rankn

sealed trait LowerErr derives CanEqual
object LowerErr {
  final case class Unsupported(node: String) extends LowerErr
  final case class TypeMismatch(found: rankn.Type) extends LowerErr
}
