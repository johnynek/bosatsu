package dev.bosatsu

import cats.data.NonEmptyList

object InSetCompiler {
  enum CmpOp derives CanEqual {
    case Eq
    case Ne
    case Lt
    case Ge
  }

  enum BoolExpr derives CanEqual {
    case TrueConst
    case FalseConst
    case Compare(op: CmpOp, rhs: Int)
    case And(left: BoolExpr, right: BoolExpr)
    case Or(left: BoolExpr, right: BoolExpr)
    case Not(value: BoolExpr)
  }

  import BoolExpr.*
  import CmpOp.*

  private case class Compiled(cost: Int, expr: BoolExpr)

  def compile(size: Int, members: NonEmptyList[Int]): BoolExpr = {
    val sortedDistinct = members.toList.sorted.distinct
    require(size > 0, s"size must be > 0, found: $size")
    require(
      sortedDistinct.forall(i => (0 <= i) && (i < size)),
      s"members must be in [0, ${size - 1}], found: $sortedDistinct"
    )

    val included = sortedDistinct.toVector
    val includedCompiled = compilePositive(size, included)
    val includedSet = included.toSet
    val excluded = (0 until size).filterNot(includedSet).toVector
    val excludedCompiled = compilePositive(size, excluded)
    val negatedExcluded =
      Compiled(excludedCompiled.cost, not(excludedCompiled.expr))

    if (includedCompiled.cost <= negatedExcluded.cost) includedCompiled.expr
    else negatedExcluded.expr
  }

  def eval(expr: BoolExpr, value: Int): Boolean =
    expr match {
      case TrueConst        => true
      case FalseConst       => false
      case Compare(op, rhs) =>
        op match {
          case Eq => value == rhs
          case Ne => value != rhs
          case Lt => value < rhs
          case Ge => value >= rhs
        }
      case And(left, right) =>
        eval(left, value) && eval(right, value)
      case Or(left, right) =>
        eval(left, value) || eval(right, value)
      case Not(inner) =>
        !eval(inner, value)
    }

  def comparisonCount(expr: BoolExpr): Int =
    expr match {
      case TrueConst | FalseConst =>
        0
      case Compare(_, _) =>
        1
      case And(left, right) =>
        comparisonCount(left) + comparisonCount(right)
      case Or(left, right) =>
        comparisonCount(left) + comparisonCount(right)
      case Not(inner) =>
        comparisonCount(inner)
    }

  private def compilePositive(size: Int, included: Vector[Int]): Compiled =
    if (included.isEmpty) Compiled(0, FalseConst)
    else {
      val n = included.length
      val bestCost = Array.fill(n + 1)(Int.MaxValue)
      val bestExpr = Array.fill[BoolExpr](n + 1)(FalseConst)
      bestCost(n) = 0
      bestExpr(n) = FalseConst

      var i = n - 1
      while (i >= 0) {
        var j = i
        while (j < n) {
          val grouped = compileGroupedSpan(size, included, i, j)
          val nextCost = bestCost(j + 1)
          if (nextCost != Int.MaxValue) {
            val totalCost = grouped.cost + nextCost
            if (totalCost < bestCost(i)) {
              val totalExpr =
                if (nextCost == 0) grouped.expr
                else or(grouped.expr, bestExpr(j + 1))
              bestCost(i) = totalCost
              bestExpr(i) = totalExpr
            }
          }
          j += 1
        }
        i -= 1
      }

      Compiled(bestCost(0), bestExpr(0))
    }

  private def compileGroupedSpan(
      size: Int,
      included: Vector[Int],
      start: Int,
      end: Int
  ): Compiled = {
    val min = included(start)
    val max = included(end)

    var envelope: BoolExpr = TrueConst
    var cost = 0

    if (min > 0) {
      envelope = and(envelope, Compare(Ge, min))
      cost += 1
    }

    if (max < size - 1) {
      envelope = and(envelope, Compare(Lt, max + 1))
      cost += 1
    }

    var nextMemberIdx = start
    var idx = min
    while (idx <= max) {
      if ((nextMemberIdx <= end) && (included(nextMemberIdx) == idx)) {
        nextMemberIdx += 1
      } else {
        envelope = and(envelope, Compare(Ne, idx))
        cost += 1
      }
      idx += 1
    }

    val envelopeCompiled = Compiled(cost, envelope)
    if ((start == end) && (cost > 1)) Compiled(1, Compare(Eq, min))
    else envelopeCompiled
  }

  private def and(left: BoolExpr, right: BoolExpr): BoolExpr =
    (left, right) match {
      case (FalseConst, _) | (_, FalseConst) => FalseConst
      case (TrueConst, r)                    => r
      case (l, TrueConst)                    => l
      case (l, r) if l == r                  => l
      case _                                 => And(left, right)
    }

  private def or(left: BoolExpr, right: BoolExpr): BoolExpr =
    (left, right) match {
      case (TrueConst, _) | (_, TrueConst) => TrueConst
      case (FalseConst, r)                 => r
      case (l, FalseConst)                 => l
      case (l, r) if l == r                => l
      case _                               => Or(left, right)
    }

  private def not(expr: BoolExpr): BoolExpr =
    expr match {
      case TrueConst        => FalseConst
      case FalseConst       => TrueConst
      case Compare(Eq, rhs) => Compare(Ne, rhs)
      case Compare(Ne, rhs) => Compare(Eq, rhs)
      case Compare(Lt, rhs) => Compare(Ge, rhs)
      case Compare(Ge, rhs) => Compare(Lt, rhs)
      case Not(inner)       => inner
      case And(left, right) => or(not(left), not(right))
      case Or(left, right)  => and(not(left), not(right))
    }
}
