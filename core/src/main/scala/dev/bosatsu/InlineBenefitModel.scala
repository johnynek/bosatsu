package dev.bosatsu

object InlineBenefitModel {
  final case class ParamSummary(
      totalUses: Int,
      eagerUses: Int,
      branchOnlyUses: Int,
      directCalleeUses: Int,
      nonDirectCalleeUses: Int,
      cheapPositionUses: Int
  ) {
    def unused: Boolean = totalUses == 0
    def deferrable: Boolean =
      (branchOnlyUses > 0) && (eagerUses == 0)
    def lambdaCalleeOnly: Boolean =
      (totalUses > 0) && (nonDirectCalleeUses == 0)
  }

  final case class ArgSummary(
      demand: ParamSummary,
      isCheap: Boolean,
      resolvesToLambda: Boolean,
      isKnownDirectCallee: Boolean,
      isKnownValue: Boolean
  )

  final case class CallSiteSummary(
      calleeWeight: Int,
      containsLoopLike: Boolean,
      exposesBranching: Boolean,
      exposesFollowOnReduction: Boolean,
      args: Vector[ArgSummary]
  ) {
    def allArgsCheap: Boolean = args.forall(_.isCheap)
  }

  val MaxCallWeightBudget = 80
  val TinyCallWeightBudget = 20

  def shouldInlineCall(summary: CallSiteSummary): Boolean = {
    if (summary.calleeWeight > MaxCallWeightBudget) false
    else {
      val structuralBenefit =
        summary.args.foldLeft(0) { (acc, arg) =>
          val unusedBenefit =
            if (arg.demand.unused && !arg.isCheap) 6
            else if (arg.demand.unused) 5
            else 0
          val deferrableBenefit =
            if (arg.demand.deferrable && !arg.isCheap) 6 else 0
          val directCalleeBenefit =
            if (arg.demand.lambdaCalleeOnly && arg.isKnownDirectCallee) 5
            else 0
          acc + unusedBenefit + deferrableBenefit + directCalleeBenefit
        }
      val loopDevirtualizationBenefit =
        if (summary.containsLoopLike)
          summary.args.foldLeft(0) { (acc, arg) =>
            if (arg.demand.lambdaCalleeOnly && arg.isKnownDirectCallee) acc + 4
            else acc
          }
        else 0

      val hasArgSpecificPayoff =
        summary.args.exists { arg =>
          arg.demand.unused ||
          arg.demand.deferrable ||
          (arg.demand.lambdaCalleeOnly && arg.isKnownDirectCallee) ||
          (arg.isKnownValue && (arg.demand.cheapPositionUses > 0))
        }
      val hasDirectBranchTesterPayoff =
        summary.args.exists { arg =>
          (arg.demand.cheapPositionUses > 0) && arg.isCheap
        }

      val branchingBenefit =
        if (
          summary.exposesBranching &&
          (hasArgSpecificPayoff || hasDirectBranchTesterPayoff)
        ) 4
        else 0
      val selectorBenefit =
        summary.args.foldLeft(0) { (acc, arg) =>
          if (arg.isKnownValue && (arg.demand.cheapPositionUses > 0)) acc + 5
          else acc
        }
      val followOnReductionBenefit =
        if (
          summary.exposesFollowOnReduction &&
          !summary.containsLoopLike &&
          (
            hasArgSpecificPayoff ||
            (
              summary.calleeWeight <= TinyCallWeightBudget &&
              summary.allArgsCheap
            )
          )
        ) 2
        else 0
      val followOnArgBenefit =
        if (
          summary.exposesFollowOnReduction &&
          summary.args.exists(arg => arg.isKnownValue || arg.isKnownDirectCallee)
        ) 2
        else 0
      val totalBenefit =
        structuralBenefit +
          loopDevirtualizationBenefit +
          branchingBenefit +
          selectorBenefit +
          followOnReductionBenefit +
          followOnArgBenefit
      val sizePenalty =
        if (totalBenefit > 0) summary.calleeWeight / 16
        else summary.calleeWeight / 8
      val loopPenalty =
        if (summary.containsLoopLike) 1 else 0

      val score =
        totalBenefit -
          sizePenalty -
          loopPenalty

      (score > 0) && (totalBenefit > 0)
    }
  }

  def shouldInlineTinyReference(
      bodyWeight: Int,
      containsLoopLike: Boolean
  ): Boolean =
    !containsLoopLike && (bodyWeight <= TinyCallWeightBudget)
}
