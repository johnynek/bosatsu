package dev.bosatsu.set

sealed abstract class PartialRel

object PartialRel {
  case object SuperSame extends PartialRel
  case object SubIntersects extends PartialRel
}
