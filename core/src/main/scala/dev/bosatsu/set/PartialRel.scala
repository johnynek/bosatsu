package dev.bosatsu.set

sealed abstract class PartialRel derives CanEqual

object PartialRel {
  case object SuperSame extends PartialRel
  case object SubIntersects extends PartialRel
}
