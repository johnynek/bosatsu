package dev.bosatsu

import cats.data.{
  Chain,
  NonEmptyList,
  NonEmptyChain,
  Validated,
  ValidatedNec
}

import Expr._
import Identifier.Bindable

object UnusedLetCheck {

  private final case class LoopState(
      free: Set[Bindable],
      unused: Chain[(Bindable, Region)]
  )

  private object LoopState {
    val empty: LoopState = LoopState(Set.empty, Chain.empty)
  }

  private sealed trait Work[A]
  private sealed trait BranchGuardState
  private object BranchGuardState {
    case object NoGuard extends BranchGuardState
    case object BoolGuard extends BranchGuardState
    final case class MatchGuard(
        innerNames: List[Bindable],
        hasInnerGuard: Boolean
    ) extends BranchGuardState
  }
  private final case class VisitExpr[A](expr: Expr[A]) extends Work[A]
  private final case class FinishLambda[A](
      args: List[Bindable],
      region: Region
  ) extends Work[A]
  private final case class FinishLet[A](
      arg: Bindable,
      rec: RecursionKind,
      bindRegion: Region
  ) extends Work[A]
  private final case class FinishApp[A](argCount: Int) extends Work[A]
  private final case class VisitBranch[A](
      branch: Branch[A],
      region: Region
  ) extends Work[A]
  private final case class FinishBranch[A](
      outerNames: List[Bindable],
      region: Region,
      guardState: BranchGuardState
  ) extends Work[A]
  private final case class FinishMatch[A](branchCount: Int) extends Work[A]

  private inline def checkArg(
      arg: Bindable,
      reg: Region,
      in: LoopState
  ): LoopState =
    if (in.free(arg)) in.copy(free = in.free - arg)
    else in.copy(unused = in.unused ++ Chain.one((arg, reg)))

  private def checkArgs(
      args: List[Bindable],
      reg: Region,
      in: LoopState
  ): LoopState =
    args.reverseIterator.foldLeft(in) { (state, arg) =>
      checkArg(arg, reg, state)
    }

  private def loop[A: HasRegion](root: Expr[A]): LoopState = {
    var work: List[Work[A]] = VisitExpr(root) :: Nil
    var values: List[LoopState] = Nil

    inline def pushValue(v: LoopState): Unit =
      values = v :: values

    inline def popValue(): LoopState = {
      val head = values.head
      values = values.tail
      head
    }

    while (work.nonEmpty) {
      work.head match {
        case VisitExpr(expr) =>
          work = work.tail
          expr match {
            case Annotation(in, _, _) =>
              work = VisitExpr(in) :: work
            case Generic(_, in) =>
              work = VisitExpr(in) :: work
            case Lambda(args, in, _) =>
              val argNames = args.toList.iterator.map(_._1).toList
              work =
                VisitExpr(in) :: FinishLambda(
                  argNames,
                  HasRegion.region(expr)
                ) :: work
            case Let(arg, rhs, in, rec, tag) =>
              // The region of the let binding itself is not directly tracked.
              // Use the let start and rhs end; nested defs keep their rhs body
              // end in the declaration tag.
              val wholeRegion = HasRegion.region(expr)
              val bindEnd =
                tag match {
                  case Declaration.DefFn(defStmt) =>
                    defStmt.result._1.get.region.end
                  case _ =>
                    HasRegion.region(rhs).end
                }
              val bindRegion = wholeRegion.withEnd(bindEnd)
              work =
                VisitExpr(rhs) :: VisitExpr(in) :: FinishLet(
                  arg,
                  rec,
                  bindRegion
                ) :: work
            case Local(name, _) =>
              pushValue(LoopState(Set(name), Chain.empty))
            case Global(_, _, _) | Literal(_, _) =>
              pushValue(LoopState.empty)
            case App(fn, args, _) =>
              val argList = args.toList
              val argWork =
                argList.iterator.map(VisitExpr(_): Work[A]).toList
              work =
                VisitExpr(fn) :: (argWork ::: FinishApp(argList.size) :: work)
            case Match(arg, branches, _) =>
              // TODO: patterns need their own region (https://github.com/johnynek/bosatsu/issues/132)
              val branchRegions =
                NonEmptyList.fromListUnsafe(
                  branches.toList
                    .scanLeft((HasRegion.region(arg), Option.empty[Region])) {
                      case ((prev, _), branch) =>
                        val caseExpr = branch.expr
                        // between the previous expression and the case is the pattern
                        (
                          HasRegion.region(caseExpr),
                          Some(Region(prev.end, HasRegion.region(caseExpr).start))
                        )
                    }
                    .collect { case (_, Some(r)) => r }
                )
              val branchWork = branchRegions
                .zip(branches)
                .toList
                .map { case (region, branch) =>
                  VisitBranch(branch, region): Work[A]
                }
              work =
                VisitExpr(arg) :: (branchWork ::: FinishMatch(
                  branches.length
                ) :: work)
          }

        case FinishLambda(args, region) =>
          work = work.tail
          val body = popValue()
          var free = body.free
          var unused = body.unused
          args.reverseIterator.foreach { arg =>
            if (free(arg)) free = free - arg
            else unused = unused ++ Chain.one((arg, region))
          }
          pushValue(LoopState(free, unused))

        case FinishLet(arg, rec, bindRegion) =>
          work = work.tail
          val inState = popValue()
          val rhsState = popValue()

          val rhsFree =
            // If this let is recursive it is definitely used, because
            // source conversion has already inserted the self-application.
            if (rec.isRecursive) rhsState.free - arg
            else rhsState.free

          val inChecked = checkArg(arg, bindRegion, inState)
          pushValue(
            LoopState(
              rhsFree ++ inChecked.free,
              rhsState.unused ++ inChecked.unused
            )
          )

        case FinishApp(argCount) =>
          work = work.tail
          var free = Set.empty[Bindable]
          var unused = Chain.empty[(Bindable, Region)]
          val argValues = List.newBuilder[LoopState]
          var idx = 0
          while (idx < argCount) {
            argValues += popValue()
            idx += 1
          }
          val fnState = popValue()
          free = free ++ fnState.free
          unused = unused ++ fnState.unused
          argValues.result().reverseIterator.foreach { argState =>
            free = free ++ argState.free
            unused = unused ++ argState.unused
          }
          pushValue(LoopState(free, unused))

        case VisitBranch(branch, region) =>
          work = work.tail
          val outerNames = branch.pattern.names
          work = branch.guardNode match {
            case Some(Expr.BoolGuard(g)) =>
              VisitExpr(g) :: VisitExpr(branch.expr) :: FinishBranch(
                outerNames,
                region,
                BranchGuardState.BoolGuard
              ) :: work
            case Some(Expr.MatchGuard(argExpr, pattern, guardOpt, _)) =>
              val guardWork =
                guardOpt.fold(List.empty[Work[A]])(g => VisitExpr(g) :: Nil)
              VisitExpr(argExpr) :: (guardWork ::: VisitExpr(branch.expr) :: FinishBranch(
                outerNames,
                region,
                BranchGuardState.MatchGuard(pattern.names, guardOpt.nonEmpty)
              ) :: work)
            case None =>
              VisitExpr(branch.expr) :: FinishBranch(
                outerNames,
                region,
                BranchGuardState.NoGuard
              ) :: work
          }

        case FinishBranch(outerNames, region, guardState) =>
          work = work.tail
          guardState match {
            case _: BranchGuardState.NoGuard.type =>
              val exprState = popValue()
              pushValue(checkArgs(outerNames, region, exprState))
            case _: BranchGuardState.BoolGuard.type =>
              val exprState = popValue()
              val boolGuardState = popValue()
              pushValue(
                checkArgs(
                  outerNames,
                  region,
                  LoopState(
                    boolGuardState.free ++ exprState.free,
                    boolGuardState.unused ++ exprState.unused
                  )
                )
              )
            case BranchGuardState.MatchGuard(innerNames, hasInnerGuard) =>
              val exprState = popValue()
              val innerGuardState =
                if (hasInnerGuard) popValue() else LoopState.empty
              val argState = popValue()
              val innerChecked =
                checkArgs(
                  innerNames,
                  region,
                  LoopState(
                    innerGuardState.free ++ exprState.free,
                    innerGuardState.unused ++ exprState.unused
                  )
                )
              pushValue(
                checkArgs(
                  outerNames,
                  region,
                  LoopState(
                    argState.free ++ innerChecked.free,
                    argState.unused ++ innerChecked.unused
                  )
                )
              )
          }

        case FinishMatch(branchCount) =>
          work = work.tail
          var free = Set.empty[Bindable]
          var unused = Chain.empty[(Bindable, Region)]
          val branchValues = List.newBuilder[LoopState]
          var idx = 0
          while (idx < branchCount) {
            branchValues += popValue()
            idx += 1
          }
          val argState = popValue()
          free = free ++ argState.free
          unused = unused ++ argState.unused
          branchValues.result().reverseIterator.foreach { branchState =>
            free = free ++ branchState.free
            unused = unused ++ branchState.unused
          }
          pushValue(LoopState(free, unused))
      }
    }

    values match {
      case res :: Nil => res
      case _ =>
        sys.error(
          s"UnusedLetCheck internal error: expected exactly one value, found ${values.size}"
        )
    }
  }

  /** Check for any unused lets, defs, or pattern bindings
    */
  def check[A: HasRegion](
      e: Expr[A]
  ): ValidatedNec[(Bindable, Region), Unit] = {
    val chain = loop(e).unused
    val filtered = chain.filterNot { case (b, _) => Identifier.isSynthetic(b) }
    NonEmptyChain.fromChain(filtered) match {
      case None      => Validated.valid(())
      case Some(nec) => Validated.invalid(nec.distinct)
    }
  }

  /** Return the free Bindable names in this expression
    */
  def freeBound[A](e: Expr[A]): Set[Bindable] =
    loop(e)(using HasRegion.instance(_ => Region(0, 0))).free
}
