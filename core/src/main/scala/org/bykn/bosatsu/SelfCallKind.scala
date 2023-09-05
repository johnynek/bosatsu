package org.bykn.bosatsu

import cats.Semigroup
import Identifier.Bindable

sealed abstract class SelfCallKind {
  import SelfCallKind._

  // if you have two branches in match what is the result
  def merge(that: => SelfCallKind): SelfCallKind =
    this match {
      case NonTailCall => NonTailCall
      case NoCall      => that
      case TailCall =>
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

  def ifNoCallThen(sc: => SelfCallKind): SelfCallKind =
    this match {
      case NoCall => sc
      case other  => other
    }
}

object SelfCallKind {
  case object NoCall extends SelfCallKind
  case object TailCall extends SelfCallKind
  case object NonTailCall extends SelfCallKind

  val branchSemigroup: Semigroup[SelfCallKind] =
    Semigroup.instance(_.merge(_))

  val ifNoCallSemigroup: Semigroup[SelfCallKind] =
    Semigroup.instance(_.ifNoCallThen(_))

  /** assuming expr is bound to nm, what kind of self call does it contain?
    */
  def apply[A](n: Bindable, te: TypedExpr[A]): SelfCallKind =
    te match {
      case TypedExpr.Generic(_, in)               => apply(n, in)
      case TypedExpr.Annotation(te, _)            => apply(n, te)
      case TypedExpr.AnnotatedLambda(as, body, _) =>
        // let fn = x -> fn(x) in fn(1)
        // is a tail-call
        if (as.exists(_._1 == n)) {
          // shadow
          SelfCallKind.NoCall
        } else {
          apply(n, body)
        }
      case TypedExpr.Global(_, _, _, _) => SelfCallKind.NoCall
      case TypedExpr.Local(vn, _, _) =>
        if (vn != n) SelfCallKind.NoCall
        else SelfCallKind.NonTailCall
      case TypedExpr.App(fn, args, _, _) =>
        val argsCall = args
          .map(apply(n, _).callNotTail)
          .reduce(SelfCallKind.ifNoCallSemigroup)

        argsCall.ifNoCallThen(
          fn match {
            case TypedExpr.Local(vn, _, _) if vn == n =>
              SelfCallKind.TailCall
            case _ => apply(n, fn).callNotTail
          }
        )
      case TypedExpr.Let(arg, ex, in, rec, _) =>
        if (arg == n) {
          // shadow
          if (rec.isRecursive) {
            // shadow still in scope in ex
            SelfCallKind.NoCall
          } else {
            // ex isn't in tail position, so if there is a call, we aren't tail
            apply(n, ex).callNotTail
          }
        } else {
          apply(n, ex).callNotTail
            .merge(apply(n, in))
        }
      case TypedExpr.Literal(_, _, _) => SelfCallKind.NoCall
      case TypedExpr.Match(arg, branches, _) =>
        apply(n, arg).callNotTail
          .ifNoCallThen {
            // then we check all the branches
            branches.foldLeft(SelfCallKind.NoCall: SelfCallKind) {
              case (acc, (_, b)) =>
                acc.merge(apply(n, b))
            }
          }
    }

}
