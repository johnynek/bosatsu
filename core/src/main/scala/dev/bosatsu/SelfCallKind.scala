package dev.bosatsu

import cats.Semigroup
import Identifier.Bindable

sealed abstract class SelfCallKind derives CanEqual {
  import SelfCallKind._

  // if you have two branches in match what is the result
  final inline def merge(inline that: => SelfCallKind): SelfCallKind =
    this match {
      case NonTailCall => NonTailCall
      case NoCall      => that
      case TailCall    =>
        that match {
          case NonTailCall       => NonTailCall
          case TailCall | NoCall => TailCall
        }
    }

  def callNotTail: SelfCallKind =
    this match {
      case NoCall                 => NoCall
      case NonTailCall | TailCall => NonTailCall
    }

  final inline def ifNoCallThen(inline sc: => SelfCallKind): SelfCallKind =
    this match {
      case NoCall => sc
      case other  => other
    }
}

object SelfCallKind {
  case object NoCall extends SelfCallKind
  case object TailCall extends SelfCallKind
  case object NonTailCall extends SelfCallKind

  given cats.Eq[SelfCallKind] = cats.Eq.fromUniversalEquals

  val branchSemigroup: Semigroup[SelfCallKind] =
    Semigroup.instance(_.merge(_))

  val ifNoCallSemigroup: Semigroup[SelfCallKind] =
    Semigroup.instance(_.ifNoCallThen(_))

  private def isFn[A](n: Bindable, te: TypedExpr[A]): Boolean =
    te match {
      case TypedExpr.Generic(_, in)    => isFn(n, in)
      case TypedExpr.Annotation(te, _, _) => isFn(n, te)
      case TypedExpr.Local(vn, _, _)   => vn == n
      case _                           => false
    }

  private def applyWithMode[A](
      n: Bindable,
      te: TypedExpr[A],
      loweredLoopRecur: Boolean,
      loopDepth: Int
  ): SelfCallKind =
    te match {
      case TypedExpr.Generic(_, in)               =>
        applyWithMode(n, in, loweredLoopRecur, loopDepth)
      case TypedExpr.Annotation(te, _, _)         =>
        applyWithMode(n, te, loweredLoopRecur, loopDepth)
      case TypedExpr.AnnotatedLambda(as, body, _) =>
        // let fn = x -> fn(x) in fn(1)
        // is a tail-call
        if (as.exists(_._1 == n)) {
          // shadow
          SelfCallKind.NoCall
        } else {
          applyWithMode(n, body, loweredLoopRecur, loopDepth)
        }
      case TypedExpr.Global(_, _, _, _) => SelfCallKind.NoCall
      case TypedExpr.Local(vn, _, _)    =>
        if (vn != n) SelfCallKind.NoCall
        else SelfCallKind.NonTailCall
      case TypedExpr.App(fn, args, _, _) =>
        val argsCall = args
          .map(applyWithMode(n, _, loweredLoopRecur, loopDepth).callNotTail)
          .reduce(using SelfCallKind.ifNoCallSemigroup)

        argsCall.ifNoCallThen(
          if (isFn(n, fn)) SelfCallKind.TailCall
          else applyWithMode(n, fn, loweredLoopRecur, loopDepth).callNotTail
        )
      case TypedExpr.Let(arg, ex, in, rec, _) =>
        if (arg == n) {
          // shadow
          if (rec.isRecursive) {
            // shadow still in scope in ex
            SelfCallKind.NoCall
          } else {
            // ex isn't in tail position, so if there is a call, we aren't tail
            applyWithMode(n, ex, loweredLoopRecur, loopDepth).callNotTail
          }
        } else {
          applyWithMode(n, ex, loweredLoopRecur, loopDepth).callNotTail
            .merge(applyWithMode(n, in, loweredLoopRecur, loopDepth))
        }
      case TypedExpr.Loop(args, body, _) =>
        val argCalls =
          args.foldLeft(SelfCallKind.NoCall: SelfCallKind) {
            case (acc, (_, expr)) =>
              acc.merge(
                applyWithMode(n, expr, loweredLoopRecur, loopDepth).callNotTail
              )
          }
        if (args.exists(_._1 == n)) argCalls
        else {
          val bodyLoopDepth = if (loweredLoopRecur) loopDepth + 1 else loopDepth

          argCalls.merge(applyWithMode(n, body, loweredLoopRecur, bodyLoopDepth))
        }
      case TypedExpr.Recur(args, _, _) =>
        val argCalls =
          args.foldLeft(SelfCallKind.NoCall: SelfCallKind) { case (acc, expr) =>
            acc.merge(
              applyWithMode(n, expr, loweredLoopRecur, loopDepth).callNotTail
            )
          }

        if (!loweredLoopRecur) argCalls
        else
          argCalls.ifNoCallThen {
            if (loopDepth == 1) SelfCallKind.TailCall
            else SelfCallKind.NoCall
          }
      case TypedExpr.Literal(_, _, _)        => SelfCallKind.NoCall
      case TypedExpr.Match(arg, branches, _) =>
        applyWithMode(n, arg, loweredLoopRecur, loopDepth).callNotTail
          .ifNoCallThen {
            // then we check all the branches
            branches.foldLeft(SelfCallKind.NoCall: SelfCallKind) {
              case (acc, branch) =>
                val branchCalls =
                  if (branch.pattern.names.contains(n)) SelfCallKind.NoCall
                  else {
                    val guardCalls = branch.guard
                      .fold(SelfCallKind.NoCall: SelfCallKind)(
                        applyWithMode(n, _, loweredLoopRecur, loopDepth).callNotTail
                      )
                    guardCalls.merge(
                      applyWithMode(
                        n,
                        branch.expr,
                        loweredLoopRecur,
                        loopDepth
                      )
                    )
                  }
                acc.merge(branchCalls)
            }
          }
    }

  /** assuming expr is bound to nm, what kind of self call does it contain?
    */
  def apply[A](n: Bindable, te: TypedExpr[A]): SelfCallKind =
    applyWithMode(n, te, false, 0)

  /** Same classification as [[apply]], but for cached typed trees after
    * `TypedExprLoopRecurLowering`. In that representation a self tail-call
    * may appear as `Loop(... Recur(...))`, so a `Recur` only counts as a
    * tail self-call when it targets the innermost enclosing lowered loop.
    */
  def afterLoopRecurLowering[A](n: Bindable, te: TypedExpr[A]): SelfCallKind =
    applyWithMode(n, te, true, 0)

}
