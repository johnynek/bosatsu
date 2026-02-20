package dev.bosatsu

import cats.Eq
import cats.syntax.all._
import dev.bosatsu.rankn.TypeEnv

import Identifier.Bindable

object TypedExprLoopRecurLowering {
  import TypedExpr._

  private def isSelfFn[A](name: Bindable, te: TypedExpr[A]): Boolean =
    te match {
      case Generic(_, in)    => isSelfFn(name, in)
      case Annotation(in, _, _) => isSelfFn(name, in)
      case Local(vn, _, _)   => vn == name
      case _                 => false
    }

  private def rewriteTailCalls[A](
      name: Bindable,
      te: TypedExpr[A],
      tailPos: Boolean,
      canRecur: Boolean
  ): TypedExpr[A] =
    te match {
      case Generic(q, in) =>
        Generic(q, rewriteTailCalls(name, in, tailPos, canRecur))
      case Annotation(in, tpe, qev) =>
        Annotation(rewriteTailCalls(name, in, tailPos, canRecur), tpe, qev)
      case lam @ AnnotatedLambda(args, body, tag) =>
        // Calls in nested lambdas are not in tail position for this function.
        val body1 =
          rewriteTailCalls(name, body, tailPos = false, canRecur = false)
        if (body1 eq body) lam
        else AnnotatedLambda(args, body1, tag)
      case App(fn, args, tpe, tag)
          if tailPos && canRecur && isSelfFn(name, fn) =>
        val args1 =
          ListUtil.mapConserveNel(args)(
            rewriteTailCalls(name, _, tailPos = false, canRecur)
          )
        Recur(args1, tpe, tag)
      case app @ App(fn, args, tpe, tag) =>
        val fn1 = rewriteTailCalls(name, fn, tailPos = false, canRecur)
        val args1 =
          ListUtil.mapConserveNel(args)(
            rewriteTailCalls(name, _, tailPos = false, canRecur)
          )
        if ((fn1 eq fn) && (args1 eq args)) app
        else App(fn1, args1, tpe, tag)
      case let @ Let(arg, ex, in, rec, tag) =>
        if (arg == name) {
          if (rec.isRecursive) {
            val ex1 =
              rewriteTailCalls(name, ex, tailPos = false, canRecur = false)
            val in1 = rewriteTailCalls(name, in, tailPos, canRecur = false)
            if ((ex1 eq ex) && (in1 eq in)) let
            else Let(arg, ex1, in1, rec, tag)
          } else {
            val ex1 = rewriteTailCalls(name, ex, tailPos = false, canRecur)
            val in1 = rewriteTailCalls(name, in, tailPos, canRecur = false)
            if ((ex1 eq ex) && (in1 eq in)) let
            else Let(arg, ex1, in1, rec, tag)
          }
        } else {
          val ex1 = rewriteTailCalls(name, ex, tailPos = false, canRecur)
          val in1 = rewriteTailCalls(name, in, tailPos, canRecur)
          if ((ex1 eq ex) && (in1 eq in)) let
          else Let(arg, ex1, in1, rec, tag)
        }
      case loop @ Loop(args, body, tag) =>
        val args1 = ListUtil.mapConserveNel(args) { arg =>
          val (n, expr) = arg
          val expr1 = rewriteTailCalls(name, expr, tailPos = false, canRecur)
          if (expr1 eq expr) arg else (n, expr1)
        }
        val canRecurBody =
          canRecur && !args.exists { case (n, _) => n == name }
        val body1 = rewriteTailCalls(name, body, tailPos, canRecurBody)
        if ((args1 eq args) && (body1 eq body)) loop
        else Loop(args1, body1, tag)
      case recur @ Recur(args, tpe, tag) =>
        val args1 =
          ListUtil.mapConserveNel(args)(
            rewriteTailCalls(name, _, tailPos = false, canRecur)
          )
        if (args1 eq args) recur
        else Recur(args1, tpe, tag)
      case m @ Match(arg, branches, tag) =>
        val arg1 = rewriteTailCalls(name, arg, tailPos = false, canRecur)
        val branches1 = ListUtil.mapConserveNel(branches) { branch =>
          val p = branch.pattern
          val canRecurBranch =
            canRecur && !p.names.contains(name)
          val guard1 = branch.guard match {
            case Some(g) =>
              val g1 =
                rewriteTailCalls(name, g, tailPos = false, canRecurBranch)
              if (g1 eq g) branch.guard
              else Some(g1)
            case None =>
              None
          }
          val branchExpr1 =
            rewriteTailCalls(name, branch.expr, tailPos, canRecurBranch)
          if (guard1.eq(branch.guard) && (branchExpr1 eq branch.expr)) branch
          else branch.copy(guard = guard1, expr = branchExpr1)
        }
        if ((arg1 eq arg) && (branches1 eq branches)) m
        else Match(arg1, branches1, tag)
      case n @ (Local(_, _, _) | Global(_, _, _, _) | Literal(_, _, _)) =>
        n
    }

  // Returns true if this expression contains a recur that targets the current loop.
  // Recur nodes in nested loops are ignored.
  private def hasOuterRecur[A](te: TypedExpr[A], inNestedLoop: Boolean): Boolean =
    te match {
      case Generic(_, in) =>
        hasOuterRecur(in, inNestedLoop)
      case Annotation(in, _, _) =>
        hasOuterRecur(in, inNestedLoop)
      case AnnotatedLambda(_, in, _) =>
        hasOuterRecur(in, inNestedLoop)
      case App(fn, appArgs, _, _) =>
        hasOuterRecur(fn, inNestedLoop) || appArgs.exists(
          hasOuterRecur(_, inNestedLoop)
        )
      case Let(_, expr, in, _, _) =>
        hasOuterRecur(expr, inNestedLoop) || hasOuterRecur(
          in,
          inNestedLoop
        )
      case Loop(loopArgs, loopBody, _) =>
        loopArgs.exists { case (_, expr) =>
          hasOuterRecur(expr, inNestedLoop)
        } || hasOuterRecur(loopBody, inNestedLoop = true)
      case Recur(_, _, _) =>
        !inNestedLoop
      case Match(arg, branches, _) =>
        hasOuterRecur(arg, inNestedLoop) || branches.exists {
          case Branch(_, guard, branchExpr) =>
            guard.exists(hasOuterRecur(_, inNestedLoop)) ||
            hasOuterRecur(branchExpr, inNestedLoop)
        }
      case Local(_, _, _) | Global(_, _, _, _) | Literal(_, _, _) =>
        false
    }

  private def lowerRecursiveBinding[A](
      name: Bindable,
      te: TypedExpr[A]
  ): Option[TypedExpr[A]] = {
    def loop(expr: TypedExpr[A]): Option[TypedExpr[A]] =
      expr match {
        case Generic(q, in) =>
          loop(in).map(Generic(q, _))
        case Annotation(in, tpe, qev) =>
          loop(in).map(Annotation(_, tpe, qev))
        case AnnotatedLambda(args, body, tag) =>
          val avoid = TypedExpr.allVarsSet(body :: Nil) ++ args.iterator
            .map(_._1)
            .toSet + name
          val fresh = Expr.nameIterator().filterNot(avoid)
          val freshArgs =
            args.map { case (_, tpe) =>
              (fresh.next(), tpe)
            }
          val subMap = args.iterator
            .map(_._1)
            .zip(freshArgs.iterator.map {
              case (n1, _) => { (loc: Local[A]) =>
                Local(n1, loc.tpe, loc.tag)
              }
            })
            .toMap
          val body1 =
            TypedExpr.substituteAll(subMap, body, enterLambda = true).get
          val recurBody =
            rewriteTailCalls(name, body1, tailPos = true, canRecur = true)
          if (!hasOuterRecur(recurBody, inNestedLoop = false)) None
          else {
            val loopArgs = freshArgs.zip(args).map {
              case ((loopName, _), (argName, argTpe)) =>
                (loopName, Local(argName, argTpe, tag): TypedExpr[A])
            }
            Some(AnnotatedLambda(args, Loop(loopArgs, recurBody, tag), tag))
          }
        case _ =>
          None
      }

    loop(te)
  }

  private def lowerExpr[A](te: TypedExpr[A]): TypedExpr[A] =
    te match {
      case g @ Generic(q, in) =>
        val in1 = lowerExpr(in)
        if (in1 eq in) g else Generic(q, in1)
      case a @ Annotation(in, tpe, qev) =>
        val in1 = lowerExpr(in)
        if (in1 eq in) a else Annotation(in1, tpe, qev)
      case lam @ AnnotatedLambda(args, body, tag) =>
        val body1 = lowerExpr(body)
        if (body1 eq body) lam
        else AnnotatedLambda(args, body1, tag)
      case app @ App(fn, args, tpe, tag) =>
        val fn1 = lowerExpr(fn)
        val args1 = ListUtil.mapConserveNel(args)(lowerExpr(_))
        if ((fn1 eq fn) && (args1 eq args)) app
        else App(fn1, args1, tpe, tag)
      case let @ Let(arg, expr, in, rec, tag) =>
        val expr1 = lowerExpr(expr)
        val expr2 =
          if (rec.isRecursive)
            lowerRecursiveBinding(arg, expr1).getOrElse(expr1)
          else expr1
        val in1 = lowerExpr(in)
        if ((expr2 eq expr) && (in1 eq in)) let
        else Let(arg, expr2, in1, rec, tag)
      case loopExpr @ Loop(args, body, tag) =>
        val args1 = ListUtil.mapConserveNel(args) { arg =>
          val (n, initExpr) = arg
          val initExpr1 = lowerExpr(initExpr)
          if (initExpr1 eq initExpr) arg else (n, initExpr1)
        }
        val body1 = lowerExpr(body)
        if ((args1 eq args) && (body1 eq body)) loopExpr
        else Loop(args1, body1, tag)
      case recur @ Recur(args, tpe, tag) =>
        val args1 = ListUtil.mapConserveNel(args)(lowerExpr(_))
        if (args1 eq args) recur
        else Recur(args1, tpe, tag)
      case m @ Match(arg, branches, tag) =>
        val arg1 = lowerExpr(arg)
        val branches1 = ListUtil.mapConserveNel(branches) { branch =>
          val guard1 = branch.guard.map(lowerExpr(_))
          val expr1 = lowerExpr(branch.expr)
          if (guard1.eq(branch.guard) && (expr1 eq branch.expr)) branch
          else branch.copy(guard = guard1, expr = expr1)
        }
        if ((arg1 eq arg) && (branches1 eq branches)) m
        else Match(arg1, branches1, tag)
      case n @ (Local(_, _, _) | Global(_, _, _, _) | Literal(_, _, _)) =>
        n
    }

  def lowerAll[A](
      lets: List[(Bindable, RecursionKind, TypedExpr[A])]
  ): List[(Bindable, RecursionKind, TypedExpr[A])] =
    lets.map { case (n, rec, te) =>
      val lowered = lowerExpr(te)
      val loweredRec =
        if (rec.isRecursive) lowerRecursiveBinding(n, lowered).getOrElse(lowered)
        else lowered
      (n, rec, loweredRec)
    }

  def lowerProgram[A, V](
      prog: Program[TypeEnv[V], TypedExpr[Declaration], A]
  ): Program[TypeEnv[V], TypedExpr[Declaration], A] = {
    val Program(typeEnv, lets, extDefs, stmts) = prog
    Program(typeEnv, lowerAll(lets), extDefs, stmts)
  }

  def lower[A: Eq](te: TypedExpr[A]): Option[TypedExpr[A]] = {
    val lowered = lowerExpr(te)
    if ((lowered: TypedExpr[A]) === te) None
    else Some(lowered)
  }
}
