package dev.bosatsu.smt

sealed trait SmtSort derives CanEqual
object SmtSort {
  case object IntS extends SmtSort
  case object BoolS extends SmtSort

  type IntSort = IntS.type
  type BoolSort = BoolS.type
}
