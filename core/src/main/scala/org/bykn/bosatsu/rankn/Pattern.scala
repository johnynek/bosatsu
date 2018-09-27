package org.bykn.bosatsu.rankn

sealed abstract class Pattern

object Pattern {
  case object WildCard extends Pattern
  case class Var(name: String) extends Pattern
  case class PositionalStruct(name: String, params: List[Pattern]) extends Pattern
  //case class BindingStruct(name: String, bindings: List[(String, Pattern)], ignoreRest: Boolean) extends Pattern
}
