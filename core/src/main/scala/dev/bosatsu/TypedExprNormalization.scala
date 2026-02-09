package dev.bosatsu

import cats.{Eq, Foldable}
import cats.data.NonEmptyList
import dev.bosatsu.rankn.{Type, TypeEnv}
import dev.bosatsu.pattern.StrPart

import Identifier.{Bindable, Constructor}

import cats.syntax.all._
import java.math.BigInteger

object TypedExprNormalization {
  import TypedExpr._

  type ScopeT[A, S] =
    Map[(Option[PackageName], Bindable), (RecursionKind, TypedExpr[A], S)]
  type ScopeTF[A] = [S] =>> ScopeT[A, S]
  type Scope[A] = FixType.Fix[ScopeTF[A]]

  def emptyScope[A]: Scope[A] =
    FixType.fix[ScopeTF[A]](Map.empty)

  implicit final class ScopeOps[A](private val scope: Scope[A]) extends AnyVal {
    def updated(
        key: Bindable,
        value: (RecursionKind, TypedExpr[A], Scope[A])
    ): Scope[A] =
      FixType.fix[ScopeTF[A]](
        FixType.unfix[ScopeTF[A]](scope).updated((None, key), value)
      )

    def updatedGlobal(
        pack: PackageName,
        key: Bindable,
        value: (RecursionKind, TypedExpr[A], Scope[A])
    ): Scope[A] =
      FixType.fix[ScopeTF[A]](
        FixType.unfix[ScopeTF[A]](scope).updated((Some(pack), key), value)
      )

    def -(key: Bindable): Scope[A] =
      FixType.fix[ScopeTF[A]](
        FixType.unfix[ScopeTF[A]](scope) - (None -> key)
      )

    def --(keys: Iterable[Bindable]): Scope[A] =
      keys.foldLeft(scope)(_ - _)

    def getLocal(
        key: Bindable
    ): Option[(RecursionKind, TypedExpr[A], Scope[A])] =
      FixType.unfix[ScopeTF[A]](scope).get((None, key))

    def getGlobal(
        pack: PackageName,
        n: Bindable
    ): Option[(RecursionKind, TypedExpr[A], Scope[A])] =
      FixType.unfix[ScopeTF[A]](scope).get((Some(pack), n))
  }

  private def nameScope[A](
      b: Bindable,
      r: RecursionKind,
      scope: Scope[A]
  ): (Option[Bindable], Scope[A]) =
    if (r.isRecursive) (Some(b), scope - b)
    else (None, scope)

  def normalizeAll[A: Eq, V](
      pack: PackageName,
      lets: List[(Bindable, RecursionKind, TypedExpr[A])],
      typeEnv: TypeEnv[V]
  )(implicit
      ev: V <:< Kind.Arg
  ): List[(Bindable, RecursionKind, TypedExpr[A])] = {
    @annotation.tailrec
    def loop(
        scope: Scope[A],
        lets: List[(Bindable, RecursionKind, TypedExpr[A])],
        acc: List[(Bindable, RecursionKind, TypedExpr[A])]
    ): List[(Bindable, RecursionKind, TypedExpr[A])] =
      lets match {
        case Nil               => acc.reverse
        case (b, r, t) :: tail =>
          // if we have a recursive value it shadows the scope
          val (optName, s0) = nameScope(b, r, scope)
          val normTE = normalize1(optName, t, s0, typeEnv).get
          val rec1 =
            if (r.isRecursive && (SelfCallKind(b, normTE) == SelfCallKind.NoCall))
              RecursionKind.NonRecursive
            else r
          val scope1 = scope.updatedGlobal(pack, b, (rec1, normTE, s0))
          loop(scope1, tail, (b, rec1, normTE) :: acc)
      }

    loop(emptyScope, lets, Nil)
  }

  def normalizeProgram[A, V](
      p: PackageName,
      fullTypeEnv: TypeEnv[V],
      prog: Program[TypeEnv[V], TypedExpr[Declaration], A]
  )(implicit
      ev: V <:< Kind.Arg
  ): Program[TypeEnv[V], TypedExpr[Declaration], A] = {
    val Program(typeEnv, lets, extDefs, stmts) = prog
    val normalLets = normalizeAll(p, lets, fullTypeEnv)
    Program(typeEnv, normalLets, extDefs, stmts)
  }

  // if you have made one step of progress, use this to recurse
  // so we don't throw away if we don't progress more
  private def normalize1[A: Eq, V](
      namerec: Option[Bindable],
      te: TypedExpr[A],
      scope: Scope[A],
      typeEnv: TypeEnv[V]
  )(implicit ev: V <:< Kind.Arg): Some[TypedExpr[A]] =
    normalizeLetOpt(namerec, te, scope, typeEnv) match {
      case None        => Some(te)
      case s @ Some(_) => s
    }

  private def setType[A](expr: TypedExpr[A], tpe: Type): TypedExpr[A] =
    if (!tpe.sameAs(expr.getType)) Annotation(expr, tpe) else expr

  private def appLambda[A](
      f1: AnnotatedLambda[A],
      args: NonEmptyList[TypedExpr[A]],
      tpe: Type,
      tag: A
  ): TypedExpr[A] = {
    val freesInArgs = TypedExpr.freeVarsSet(args.toList)
    val AnnotatedLambda(lamArgs, expr, _) = f1.unshadow(freesInArgs)
    // Now that we certainly don't shadow we can convert this:
    // ((y1, y2, ..., yn) -> z)(x1, x2, ..., xn) = let y1 = x1 in let y2 = x2 in ... z
    val lets = lamArgs.zip(args).map { case ((n, ltpe), arg) =>
      (n, setType(arg, ltpe))
    }
    val expr2 = setType(expr, tpe)
    TypedExpr.letAllNonRec(lets, expr2, tag)
  }

  private def isSelfFn[A](name: Bindable, te: TypedExpr[A]): Boolean =
    te match {
      case Generic(_, in)    => isSelfFn(name, in)
      case Annotation(in, _) => isSelfFn(name, in)
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
      case Annotation(in, tpe) =>
        Annotation(rewriteTailCalls(name, in, tailPos, canRecur), tpe)
      case lam @ AnnotatedLambda(args, body, tag) =>
        // Calls in nested lambdas are not in tail position for this function.
        val body1 = rewriteTailCalls(name, body, tailPos = false, canRecur = false)
        if (body1 eq body) lam
        else AnnotatedLambda(args, body1, tag)
      case App(fn, args, tpe, tag)
          if tailPos && canRecur && isSelfFn(name, fn) =>
        val args1 = args.map(rewriteTailCalls(name, _, tailPos = false, canRecur))
        Recur(args1, tpe, tag)
      case app @ App(fn, args, tpe, tag) =>
        val fn1 = rewriteTailCalls(name, fn, tailPos = false, canRecur)
        val args1 = args.map(rewriteTailCalls(name, _, tailPos = false, canRecur))
        if ((fn1 eq fn) && (args1 eq args)) app
        else App(fn1, args1, tpe, tag)
      case let @ Let(arg, ex, in, rec, tag) =>
        if (arg == name) {
          if (rec.isRecursive) {
            val ex1 = rewriteTailCalls(name, ex, tailPos = false, canRecur = false)
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
        val args1 = args.map { case (n, expr) =>
          (n, rewriteTailCalls(name, expr, tailPos = false, canRecur))
        }
        val canRecurBody =
          canRecur && !args.exists { case (n, _) => n == name }
        val body1 = rewriteTailCalls(name, body, tailPos, canRecurBody)
        if ((args1 eq args) && (body1 eq body)) loop
        else Loop(args1, body1, tag)
      case recur @ Recur(args, tpe, tag) =>
        val args1 = args.map(rewriteTailCalls(name, _, tailPos = false, canRecur))
        if (args1 eq args) recur
        else Recur(args1, tpe, tag)
      case m @ Match(arg, branches, tag) =>
        val arg1 = rewriteTailCalls(name, arg, tailPos = false, canRecur)
        val branches1 = branches.map { case (p, branchExpr) =>
          val canRecurBranch =
            canRecur && !p.names.contains(name)
          (p, rewriteTailCalls(name, branchExpr, tailPos, canRecurBranch))
        }
        if ((arg1 eq arg) && (branches1 eq branches)) m
        else Match(arg1, branches1, tag)
      case n @ (Local(_, _, _) | Global(_, _, _, _) | Literal(_, _, _)) =>
        n
    }

  private def rewriteTailRecToLoop[A](
      name: Bindable,
      te: TypedExpr[A]
  ): Option[TypedExpr[A]] =
    if (SelfCallKind(name, te) != SelfCallKind.TailCall) None
    else {
      def loop(expr: TypedExpr[A]): Option[TypedExpr[A]] =
        expr match {
          case Generic(q, in) =>
            loop(in).map(Generic(q, _))
          case Annotation(in, tpe) =>
            loop(in).map(Annotation(_, tpe))
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
              .zip(freshArgs.iterator.map { case (n1, _) =>
                { (loc: Local[A]) => Local(n1, loc.tpe, loc.tag) }
              })
              .toMap
            val body1 = TypedExpr.substituteAll(subMap, body, enterLambda = true).get
            val recurBody =
              rewriteTailCalls(name, body1, tailPos = true, canRecur = true)
            val loopArgs = freshArgs.zip(args).map {
              case ((loopName, _), (argName, argTpe)) =>
                (loopName, Local(argName, argTpe, tag): TypedExpr[A])
            }
            Some(AnnotatedLambda(args, Loop(loopArgs, recurBody, tag), tag))
          case _ =>
            None
        }

      loop(te)
    }

  @annotation.tailrec
  private def stripTypeWrappers[A](te: TypedExpr[A]): TypedExpr[A] =
    te match {
      case Generic(_, in)    => stripTypeWrappers(in)
      case Annotation(in, _) => stripTypeWrappers(in)
      case _                 => te
    }

  private def isSameLocalRef[A](expected: Bindable, te: TypedExpr[A]): Boolean =
    stripTypeWrappers(te) match {
      case Local(n, _, _) => n == expected
      case _              => false
    }

  private def combineInvariantFlags(
      left: Option[Vector[Boolean]],
      right: Option[Vector[Boolean]]
  ): Option[Vector[Boolean]] =
    (left, right) match {
      case (None, r) => r
      case (l, None) => l
      case (Some(l), Some(r)) =>
        val merged =
          if (l.length == r.length) {
            l.iterator.zip(r.iterator).map { case (lb, rb) => lb && rb }.toVector
          } else {
            // Defensive fallback: typed programs should never mismatch arities.
            Vector.fill(l.length max r.length)(false)
          }
        Some(merged)
    }

  // Return conjunction flags for outer recur arguments:
  // flag(i) is true iff every outer recur passes the i-th loop binder unchanged.
  private def outerRecurInvariantFlags[A](
      te: TypedExpr[A],
      loopNames: Vector[Bindable],
      inNestedLoop: Boolean
  ): Option[Vector[Boolean]] = {
    def loopList(
        exprs: List[TypedExpr[A]],
        init: Option[Vector[Boolean]]
    ): Option[Vector[Boolean]] =
      exprs.foldLeft(init) { (acc, e) =>
        combineInvariantFlags(
          acc,
          outerRecurInvariantFlags(e, loopNames, inNestedLoop)
        )
      }

    te match {
      case Generic(_, in) =>
        outerRecurInvariantFlags(in, loopNames, inNestedLoop)
      case Annotation(in, _) =>
        outerRecurInvariantFlags(in, loopNames, inNestedLoop)
      case AnnotatedLambda(_, in, _) =>
        outerRecurInvariantFlags(in, loopNames, inNestedLoop)
      case App(fn, args, _, _) =>
        combineInvariantFlags(
          outerRecurInvariantFlags(fn, loopNames, inNestedLoop),
          loopList(args.toList, None)
        )
      case Let(_, expr, in, _, _) =>
        combineInvariantFlags(
          outerRecurInvariantFlags(expr, loopNames, inNestedLoop),
          outerRecurInvariantFlags(in, loopNames, inNestedLoop)
        )
      case Loop(loopArgs, loopBody, _) =>
        val initFlags = loopArgs.toList.foldLeft(Option.empty[Vector[Boolean]]) {
          case (acc, (_, initExpr)) =>
            combineInvariantFlags(
              acc,
              outerRecurInvariantFlags(initExpr, loopNames, inNestedLoop)
            )
        }
        combineInvariantFlags(
          initFlags,
          outerRecurInvariantFlags(loopBody, loopNames, inNestedLoop = true)
        )
      case Recur(args, _, _) if !inNestedLoop =>
        if (args.length == loopNames.length) {
          Some(
            args.iterator
              .zip(loopNames.iterator)
              .map { case (arg, expected) => isSameLocalRef(expected, arg) }
              .toVector
          )
        } else {
          // Defensive fallback: typed programs should never mismatch arities.
          Some(Vector.fill(loopNames.length)(false))
        }
      case Recur(_, _, _) =>
        None
      case Match(arg, branches, _) =>
        combineInvariantFlags(
          outerRecurInvariantFlags(arg, loopNames, inNestedLoop),
          branches.toList.foldLeft(Option.empty[Vector[Boolean]]) {
            case (acc, (_, branchExpr)) =>
              combineInvariantFlags(
                acc,
                outerRecurInvariantFlags(branchExpr, loopNames, inNestedLoop)
              )
          }
        )
      case Local(_, _, _) | Global(_, _, _, _) | Literal(_, _, _) =>
        None
    }
  }

  // Rewrite outer recur nodes by dropping fixed argument positions.
  // Nested loop bodies are excluded so inner recur nodes keep their own arity.
  private def dropOuterRecurArgs[A](
      te: TypedExpr[A],
      dropPositions: Vector[Boolean],
      inNestedLoop: Boolean
  ): TypedExpr[A] =
    te match {
      case g @ Generic(q, in) =>
        val in1 = dropOuterRecurArgs(in, dropPositions, inNestedLoop)
        if (in1 eq in) g else Generic(q, in1)
      case a @ Annotation(in, tpe) =>
        val in1 = dropOuterRecurArgs(in, dropPositions, inNestedLoop)
        if (in1 eq in) a else Annotation(in1, tpe)
      case lam @ AnnotatedLambda(args, body, tag) =>
        val body1 = dropOuterRecurArgs(body, dropPositions, inNestedLoop)
        if (body1 eq body) lam
        else AnnotatedLambda(args, body1, tag)
      case app @ App(fn, args, tpe, tag) =>
        val fn1 = dropOuterRecurArgs(fn, dropPositions, inNestedLoop)
        val args1 = ListUtil.mapConserveNel(args) { arg =>
          dropOuterRecurArgs(arg, dropPositions, inNestedLoop)
        }
        if ((fn1 eq fn) && (args1 eq args)) app
        else App(fn1, args1, tpe, tag)
      case let @ Let(arg, expr, in, rec, tag) =>
        val expr1 = dropOuterRecurArgs(expr, dropPositions, inNestedLoop)
        val in1 = dropOuterRecurArgs(in, dropPositions, inNestedLoop)
        if ((expr1 eq expr) && (in1 eq in)) let
        else Let(arg, expr1, in1, rec, tag)
      case lp @ Loop(args, body, tag) =>
        val args1 = ListUtil.mapConserveNel(args) { case (n, initExpr) =>
          (n, dropOuterRecurArgs(initExpr, dropPositions, inNestedLoop))
        }
        val body1 = dropOuterRecurArgs(body, dropPositions, inNestedLoop = true)
        if ((args1 eq args) && (body1 eq body)) lp
        else Loop(args1, body1, tag)
      case recur @ Recur(args, tpe, tag) =>
        val args1 = ListUtil.mapConserveNel(args) { arg =>
          dropOuterRecurArgs(arg, dropPositions, inNestedLoop)
        }
        if (inNestedLoop) {
          if (args1 eq args) recur
          else Recur(args1, tpe, tag)
        } else if (args1.length == dropPositions.length) {
          val kept = args1.toList.iterator
            .zip(dropPositions.iterator)
            .collect { case (arg, false) => arg }
            .toList
          val args2 = NonEmptyList.fromListUnsafe(kept)
          if ((args1 eq args) && !dropPositions.contains(true)) recur
          else Recur(args2, tpe, tag)
        } else {
          // Defensive fallback: typed programs should never mismatch arities.
          recur
        }
      case m @ Match(arg, branches, tag) =>
        val arg1 = dropOuterRecurArgs(arg, dropPositions, inNestedLoop)
        val branches1 = ListUtil.mapConserveNel(branches) { case (p, branchExpr) =>
          (p, dropOuterRecurArgs(branchExpr, dropPositions, inNestedLoop))
        }
        if ((arg1 eq arg) && (branches1 eq branches)) m
        else Match(arg1, branches1, tag)
      case n @ (Local(_, _, _) | Global(_, _, _, _) | Literal(_, _, _)) =>
        n
    }

  private def freeLocalTypes[A](te: TypedExpr[A]): Map[Bindable, Type] = {
    def addType(
        acc: Map[Bindable, Type],
        name: Bindable,
        tpe: Type
    ): Map[Bindable, Type] =
      acc.get(name) match {
        case Some(_) => acc
        case None    => acc.updated(name, tpe)
      }

    def loop(
        expr: TypedExpr[A],
        bound: Set[Bindable],
        acc: Map[Bindable, Type]
    ): Map[Bindable, Type] =
      expr match {
        case Generic(_, in) =>
          loop(in, bound, acc)
        case Annotation(in, _) =>
          loop(in, bound, acc)
        case AnnotatedLambda(args, body, _) =>
          loop(body, bound ++ args.iterator.map(_._1), acc)
        case App(fn, args, _, _) =>
          args.foldLeft(loop(fn, bound, acc)) { (acc0, a) =>
            loop(a, bound, acc0)
          }
        case Let(arg, expr, in, rec, _) =>
          val acc1 =
            if (rec.isRecursive) loop(expr, bound + arg, acc)
            else loop(expr, bound, acc)
          loop(in, bound + arg, acc1)
        case Loop(args, body, _) =>
          val acc1 = args.toList.foldLeft(acc) { case (acc0, (_, initExpr)) =>
            loop(initExpr, bound, acc0)
          }
          loop(body, bound ++ args.iterator.map(_._1), acc1)
        case Recur(args, _, _) =>
          args.foldLeft(acc)((acc0, a) => loop(a, bound, acc0))
        case Match(arg, branches, _) =>
          val acc1 = loop(arg, bound, acc)
          branches.toList.foldLeft(acc1) { case (acc0, (p, b)) =>
            loop(b, bound ++ p.names, acc0)
          }
        case Local(n, tpe, _) =>
          if (bound(n)) acc
          else addType(acc, n, tpe)
        case Global(_, _, _, _) | Literal(_, _, _) =>
          acc
      }

    loop(te, Set.empty, Map.empty)
  }

  // Return true when fnName appears in a non-direct-call position.
  // Direct calls are applications where the function expression resolves to fnName.
  private def hasEscapingFnRef[A](
      te: TypedExpr[A],
      fnName: Bindable,
      fnVisible: Boolean
  ): Boolean =
    te match {
      case Generic(_, in) =>
        hasEscapingFnRef(in, fnName, fnVisible)
      case Annotation(in, _) =>
        hasEscapingFnRef(in, fnName, fnVisible)
      case AnnotatedLambda(args, body, _) =>
        val fnVisibleBody = fnVisible && !args.exists(_._1 == fnName)
        hasEscapingFnRef(body, fnName, fnVisibleBody)
      case App(fn, args, _, _) =>
        val fnEscapes =
          if (fnVisible && isSameLocalRef(fnName, fn)) false
          else hasEscapingFnRef(fn, fnName, fnVisible)
        fnEscapes || args.exists(hasEscapingFnRef(_, fnName, fnVisible))
      case Let(arg, expr, in, rec, _) =>
        if (arg == fnName) {
          if (rec.isRecursive) false
          else hasEscapingFnRef(expr, fnName, fnVisible)
        } else {
          hasEscapingFnRef(expr, fnName, fnVisible) ||
            hasEscapingFnRef(in, fnName, fnVisible)
        }
      case Loop(args, body, _) =>
        val fnVisibleBody = fnVisible && !args.exists(_._1 == fnName)
        args.exists { case (_, initExpr) =>
          hasEscapingFnRef(initExpr, fnName, fnVisible)
        } || hasEscapingFnRef(body, fnName, fnVisibleBody)
      case Recur(args, _, _) =>
        args.exists(hasEscapingFnRef(_, fnName, fnVisible))
      case Match(arg, branches, _) =>
        hasEscapingFnRef(arg, fnName, fnVisible) || branches.exists {
          case (p, branchExpr) =>
            val fnVisibleBranch = fnVisible && !p.names.contains(fnName)
            hasEscapingFnRef(branchExpr, fnName, fnVisibleBranch)
        }
      case Local(n, _, _) =>
        fnVisible && (n == fnName)
      case Global(_, _, _, _) | Literal(_, _, _) =>
        false
    }

  private def prependArgsToFnCalls[A](
      te: TypedExpr[A],
      fnName: Bindable,
      extraArgs: List[TypedExpr[A]],
      fnVisible: Boolean
  ): TypedExpr[A] =
    te match {
      case g @ Generic(q, in) =>
        val in1 = prependArgsToFnCalls(in, fnName, extraArgs, fnVisible)
        if (in1 eq in) g else Generic(q, in1)
      case a @ Annotation(in, tpe) =>
        val in1 = prependArgsToFnCalls(in, fnName, extraArgs, fnVisible)
        if (in1 eq in) a else Annotation(in1, tpe)
      case lam @ AnnotatedLambda(args, body, tag) =>
        val fnVisibleBody = fnVisible && !args.exists(_._1 == fnName)
        val body1 = prependArgsToFnCalls(body, fnName, extraArgs, fnVisibleBody)
        if (body1 eq body) lam
        else AnnotatedLambda(args, body1, tag)
      case app @ App(fn, args, tpe, tag) =>
        val fnIsCall = fnVisible && isSameLocalRef(fnName, fn)
        val fn1 =
          if (fnIsCall) fn
          else prependArgsToFnCalls(fn, fnName, extraArgs, fnVisible)
        val args1 = ListUtil.mapConserveNel(args) { arg =>
          prependArgsToFnCalls(arg, fnName, extraArgs, fnVisible)
        }
        if (fnIsCall) {
          val args2 = NonEmptyList.fromListUnsafe(extraArgs ::: args1.toList)
          App(fn1, args2, tpe, tag)
        } else if ((fn1 eq fn) && (args1 eq args)) app
        else App(fn1, args1, tpe, tag)
      case let @ Let(arg, expr, in, rec, tag) =>
        if (arg == fnName) {
          if (rec.isRecursive) let
          else {
            val expr1 = prependArgsToFnCalls(expr, fnName, extraArgs, fnVisible)
            if (expr1 eq expr) let
            else Let(arg, expr1, in, rec, tag)
          }
        } else {
          val expr1 = prependArgsToFnCalls(expr, fnName, extraArgs, fnVisible)
          val in1 = prependArgsToFnCalls(in, fnName, extraArgs, fnVisible)
          if ((expr1 eq expr) && (in1 eq in)) let
          else Let(arg, expr1, in1, rec, tag)
        }
      case lp @ Loop(args, body, tag) =>
        val args1 = ListUtil.mapConserveNel(args) { case (n, initExpr) =>
          (n, prependArgsToFnCalls(initExpr, fnName, extraArgs, fnVisible))
        }
        val fnVisibleBody = fnVisible && !args.exists(_._1 == fnName)
        val body1 = prependArgsToFnCalls(body, fnName, extraArgs, fnVisibleBody)
        if ((args1 eq args) && (body1 eq body)) lp
        else Loop(args1, body1, tag)
      case recur @ Recur(args, tpe, tag) =>
        val args1 = ListUtil.mapConserveNel(args) { arg =>
          prependArgsToFnCalls(arg, fnName, extraArgs, fnVisible)
        }
        if (args1 eq args) recur
        else Recur(args1, tpe, tag)
      case m @ Match(arg, branches, tag) =>
        val arg1 = prependArgsToFnCalls(arg, fnName, extraArgs, fnVisible)
        val branches1 = ListUtil.mapConserveNel(branches) { case (p, branchExpr) =>
          val fnVisibleBranch = fnVisible && !p.names.contains(fnName)
          (p, prependArgsToFnCalls(branchExpr, fnName, extraArgs, fnVisibleBranch))
        }
        if ((arg1 eq arg) && (branches1 eq branches)) m
        else Match(arg1, branches1, tag)
      case n @ (Local(_, _, _) | Global(_, _, _, _) | Literal(_, _, _)) =>
        n
    }

  private def prependLambdaArgs[A](
      te: TypedExpr[A],
      extraArgs: List[(Bindable, Type)]
  ): Option[TypedExpr[A]] =
    if (extraArgs.isEmpty) Some(te)
    else
      te match {
        case Generic(q, in) =>
          prependLambdaArgs(in, extraArgs).map(Generic(q, _))
        case Annotation(in, tpe) =>
          prependLambdaArgs(in, extraArgs).map(Annotation(_, tpe))
        case AnnotatedLambda(args, body, tag) =>
          val allArgs = NonEmptyList.fromListUnsafe(extraArgs ::: args.toList)
          Some(AnnotatedLambda(allArgs, body, tag))
        case _ =>
          None
      }

  private case class CapturedClosureVar(
      name: Bindable,
      capture: Bindable,
      tpe: Type
  )

  private def rewriteNonEscapingClosureBinding[A](
      arg: Bindable,
      expr: TypedExpr[A],
      in: TypedExpr[A],
      rec: RecursionKind,
      tag: A
  ): Option[TypedExpr[A]] =
    if (in.notFree(arg)) None
    else {
      val closureNames = TypedExpr.freeVars(expr :: Nil).filterNot(_ == arg)
      if (
        closureNames.isEmpty ||
        hasEscapingFnRef(expr, arg, fnVisible = true) ||
        hasEscapingFnRef(in, arg, fnVisible = true)
      ) None
      else {
        val closureTypes = freeLocalTypes(expr)
        closureNames
          .traverse { n =>
            closureTypes.get(n).map(tpe => (n, tpe))
          }
          .flatMap { namedTypes =>
            val avoid =
              TypedExpr.allVarsSet(expr :: in :: Nil).toSet ++ closureNames + arg
            val fresh = Expr.nameIterator().filterNot(avoid)
            val captures = namedTypes.map { case (n, tpe) =>
              CapturedClosureVar(n, fresh.next(), tpe)
            }
            val renameMap: Map[Bindable, Local[A] => TypedExpr[A]] =
              captures.iterator.map { case CapturedClosureVar(n, cap, _) =>
                n -> { (loc: Local[A]) =>
                  Local(cap, loc.tpe, loc.tag): TypedExpr[A]
                }
              }.toMap

            val expr1 = TypedExpr.substituteAll(renameMap, expr, enterLambda = true).get
            val extraParams = captures.map(c => (c.capture, c.tpe))
            prependLambdaArgs(expr1, extraParams).map { expr2 =>
              val callExtraArgs: List[TypedExpr[A]] =
                captures.map(c => Local(c.capture, c.tpe, tag): TypedExpr[A])
              val expr3 =
                prependArgsToFnCalls(expr2, arg, callExtraArgs, fnVisible = true)
              val in1 =
                prependArgsToFnCalls(in, arg, callExtraArgs, fnVisible = true)
              val rewrittenLet = Let(arg, expr3, in1, rec, tag)
              NonEmptyList.fromList(
                captures.map { c =>
                  (c.capture, Local(c.name, c.tpe, tag): TypedExpr[A])
                }
              ) match {
                case Some(captureLets) =>
                  TypedExpr.letAllNonRec(captureLets, rewrittenLet, tag)
                case None =>
                  rewrittenLet
              }
            }
          }
      }
    }

  /** if the te is not in normal form, transform it into normal form
    */
  private def normalizeLetOpt[A: Eq, V](
      namerec: Option[Bindable],
      te: TypedExpr[A],
      scope: Scope[A],
      typeEnv: TypeEnv[V]
  )(implicit ev: V <:< Kind.Arg): Option[TypedExpr[A]] = {

    val kindOf: Type => Option[Kind] =
      Type.kindOfOption { case const @ Type.TyConst(_) =>
        typeEnv.getType(const).map(_.kindOf)
      }

    te match {
      case g @ Generic(_, Annotation(term, _))
          if g.getType.sameAs(term.getType) =>
        normalize1(namerec, term, scope, typeEnv)
      case Generic(q0, Generic(q1, in)) =>
        val term = Generic(q0.concat(q1), in)
        normalize1(namerec, term, scope, typeEnv)
      case Generic(quant, in) =>
        val sin = normalize1(namerec, in, scope, typeEnv).get
        val g1 = TypedExpr.normalizeQuantVars(quant, sin)
        if (g1 === te) None
        else Some(g1)
      case Annotation(term, tpe) =>
        // if we annotate twice, we can ignore the inner annotation
        // we should have type annotation where we normalize type parameters
        val e1 = normalize1(namerec, term, scope, typeEnv).get
        (e1, tpe) match {
          case _ if e1.getType.sameAs(tpe) =>
            // the type is already right
            Some(e1)
          case (gen @ Generic(_, _), rho: Type.Rho) =>
            val inst = TypedExpr.instantiateTo(gen, rho, kindOf)
            // we compare thes to te because instantiate
            // can add an Annotation back
            if ((inst: TypedExpr[A]) =!= te) Some(inst)
            else None
          case (notSameTpe, _) =>
            val nt = Type.normalize(tpe)
            if (notSameTpe eq term) {
              if (nt == tpe) None
              else Some(Annotation(term, nt))
            } else Some(Annotation(notSameTpe, nt))
        }

      case AnnotatedLambda(lamArgs0, expr, tag) =>
        lazy val anons: Iterator[Bindable] = Expr
          .nameIterator()
          .filterNot(expr.freeVarsDup.toSet)

        val bodyScope = scope -- lamArgs0.toList.map(_._1)
        val e1 = normalize1(None, expr, bodyScope, typeEnv).get

        var changed = false
        val lamArgs = lamArgs0.map { case (n, t) =>
          val n1 =
            if (e1.notFree(n)) {
              // n is not used.
              val next = anons.next()
              changed = changed || (next != n)
              next
            } else {
              n
            }
          (n1, Type.normalize(t))
        }

        val lambda1 = AnnotatedLambda(lamArgs, e1, tag)
        val maybeLooped =
          namerec.flatMap { recName =>
            rewriteTailRecToLoop(recName, lambda1)
              .filterNot(_ === lambda1)
          }

        maybeLooped match {
          case Some(looped) =>
            normalize1(None, looped, scope, typeEnv)
          case None if changed =>
            normalize1(namerec, lambda1, scope, typeEnv)
          case None =>

            def doesntUseArgs(te: TypedExpr[A]): Boolean =
              lamArgs.forall { case (n, _) => te.notFree(n) }

            // assuming b is bound below lamArgs, return true if it doesn't shadow an arg
            def doesntShadow(b: Bindable): Boolean =
              !lamArgs.exists { case (n, _) => n == b }

            def matchesArgs(nel: NonEmptyList[TypedExpr[A]]): Boolean =
              (nel.length == lamArgs.length) && lamArgs.iterator
                .zip(nel.iterator)
                .forall {
                  case ((lamN, _), Local(argN, _, _)) => lamN == argN
                  case _                              => false
                }

            e1 match {
              case App(fn, aargs, _, _)
                  if matchesArgs(aargs) && doesntUseArgs(fn) =>
                // x -> f(x) == f (eta conversion)
                // note, e1 is already normalized, so fn is normalized
                Some(setType(fn, te.getType))
              case Let(arg1, ex, in, rec, tag1)
                  if !Impl.isSimple(ex, lambdaSimple = true) && doesntUseArgs(
                    ex
                  ) && doesntShadow(arg1) =>
                // x ->
                //   y = z
                //   f(y)
                // same as:
                // y = z
                // x -> f(y)
                // avoid recomputing y if y is not simple. Note, we consider a lambda simple
                // since when compiling we can lift lambdas out anyway, so they are at most 1 allocation
                // but possibly 0.
                //
                // TODO: we could reorder Lets if we have several in a row
                normalize1(
                  None,
                  Let(arg1, ex, AnnotatedLambda(lamArgs, in, tag), rec, tag1),
                  scope,
                  typeEnv
                )
              case m @ Match(arg1, branches, tag1)
                  if lamArgs.forall { case (arg, _) =>
                    arg1.notFree(arg)
                  } && ((branches.length > 1) || !Impl.isSimple(
                    arg1,
                    lambdaSimple = true
                  )) =>
                // x -> match z: w
                // convert to match z: x -> w
                // but don't bother if the arg is simple or there is only 1 branch + simple arg
                val b1 = branches.traverse { case (p, b) =>
                  if (
                    !lamArgs.exists { case (arg, _) => p.names.contains(arg) }
                  ) {
                    Some((p, AnnotatedLambda(lamArgs, b, tag)))
                  } else None
                }
                b1 match {
                  case None =>
                    if ((m eq expr) && (lamArgs === lamArgs0)) None
                    else Some(AnnotatedLambda(lamArgs, m, tag))
                  case Some(bs) =>
                    val m1 = Match(arg1, bs, tag1)
                    normalize1(namerec, m1, scope, typeEnv)
                }
              case notApp =>
                if ((notApp eq expr) && (lamArgs === lamArgs0)) None
                else Some(AnnotatedLambda(lamArgs, notApp, tag))
            }
        }
      case Literal(_, _, _) =>
        // these are fundamental
        None
      case Global(p, n: Constructor, tpe0, tag) =>
        val tpe = Type.normalize(tpe0)
        if (tpe == tpe0) None
        else Some(Global(p, n, tpe, tag))
      case Global(p, n: Bindable, tpe0, tag) =>
        scope.getGlobal(p, n).flatMap {

          case (RecursionKind.NonRecursive, te, _)
              if Impl.isSimple(te, lambdaSimple = false) =>
            // inlining lambdas naively can cause an exponential blow up in size
            Some(te)
          case _ =>
            val tpe = Type.normalize(tpe0)
            if (tpe == tpe0) None
            else Some(Global(p, n, tpe, tag))
        }
      case Local(n, tpe0, tag) =>
        // TODO we could look in the scope
        // and potentially simplify, but maybe it
        // is too late here, we want to do that when
        // we have another potential optimization?
        val tpe = Type.normalize(tpe0)
        if (tpe == tpe0) None
        else Some(Local(n, tpe, tag))
      case Impl.IntAlgebraic.Optimized(optTE) =>
        Some(optTE)
      // TODO: we could implement much of the predef at compile time
      case App(fn, args, tpe0, tag) =>
        val tpe = Type.normalize(tpe0)
        val f1 = normalize1(None, fn, scope, typeEnv).get
        // the second and third branches use this but the first doesn't
        // make it lazy so we don't recurse more than needed
        lazy val a1 = ListUtil.mapConserveNel(args) { a =>
          normalize1(None, a, scope, typeEnv).get
        }
        val ws = Impl.WithScope(scope, ev.substituteCo[TypeEnv](typeEnv))

        f1 match {
          // TODO: what if f1: Generic(_, AnnotatedLambda(_, _, _))
          // we should still be able ton convert this to a let by
          // instantiating to the right args
          case ws.ResolveToLambda(Nil, args1, body, ftag) =>
            val lam = AnnotatedLambda(args1, body, ftag)
            val l = appLambda[A](lam, args, tpe, tag)
            normalize1(namerec, l, scope, typeEnv)
          case lam @ AnnotatedLambda(_, _, _) =>
            val l = appLambda[A](lam, args, tpe, tag)
            normalize1(namerec, l, scope, typeEnv)
          case Let(arg1, ex, in, rec, tag1) if a1.forall(_.notFree(arg1)) =>
            // (app (let x y z) w) == (let x y (app z w)) if w does not have x free
            normalize1(
              namerec,
              Let(arg1, ex, App(in, args, tpe, tag), rec, tag1),
              scope,
              typeEnv
            )
          case _ =>
            if ((f1 eq fn) && (tpe == tpe0) && (a1 eq args)) None
            else Some(App(f1, a1, tpe, tag))
        }
      case Let(arg, ex, in, rec, tag) =>
        // note, Infer has already checked
        // to make sure rec is accurate
        val (ni, si) = nameScope(arg, rec, scope)
        val ex1 = normalize1(ni, ex, si, typeEnv).get
        val (rec1, ex2) =
          if (rec.isRecursive) {
            val ex2 = rewriteTailRecToLoop(arg, ex1).getOrElse(ex1)
            val rec1 =
              if (SelfCallKind(arg, ex2) == SelfCallKind.NoCall)
                RecursionKind.NonRecursive
              else rec
            (rec1, ex2)
          } else (rec, ex1)

        if (!rec1.isRecursive && isSameLocalRef(arg, ex2)) {
          // Non-recursive identity lets are pure no-ops:
          // let x = x in body  ==>  body
          normalize1(namerec, in, scope, typeEnv)
        } else
          ex2 match {
            case Let(ex1a, ex1ex, ex1in, RecursionKind.NonRecursive, ex1tag)
                if !rec1.isRecursive && in.notFree(ex1a) =>
              // according to a SPJ paper, it is generally better
              // to float lets out of nesting inside in:
              // let foo = let bar = x in bar in foo
              //
              // is better to write:
              // let bar = x in let foo = bar in foo
              // since you are going to evaluate and keep in scope
              // the expression
              // we can lift
              val l1 = Let(
                ex1a,
                ex1ex,
                Let(arg, ex1in, in, RecursionKind.NonRecursive, tag),
                RecursionKind.NonRecursive,
                ex1tag
              )
              normalize1(namerec, l1, scope, typeEnv)
            case _ =>
              val scopeIn = si.updated(arg, (rec1, ex2, si))

              val in1 = normalize1(namerec, in, scopeIn, typeEnv).get
              val maybeRewritten =
                if (rec.isRecursive)
                  rewriteNonEscapingClosureBinding(arg, ex2, in1, rec1, tag)
                else None

              maybeRewritten match {
                case Some(rewritten) =>
                  normalize1(namerec, rewritten, scope, typeEnv)
                case None =>
                  in1 match {
                    case Match(marg, branches, mtag)
                        if !rec1.isRecursive && marg.notFree(arg) && branches.exists {
                          case (p, r) => p.names.contains(arg) || r.notFree(arg)
                        } =>
                      // x = y
                      // match z:
                      //   case w: ww
                      //
                      // can be rewritten as
                      // match z:
                      //   case w:
                      //     x = y
                      //     ww
                      //
                      // when z is not free in x, and at least one branch is not free in x
                      val b1 = branches.map { case (p, r) =>
                        if (p.names.contains(arg) || r.notFree(arg)) (p, r)
                        else (p, Let(arg, ex2, r, rec1, tag))
                      }
                      normalize1(namerec, Match(marg, b1, mtag), scope, typeEnv)
                    case _ =>
                      val cnt = in1.freeVarsDup.count(_ == arg)
                      if (cnt > 0) {
                        // the arg is needed
                        val isSimp = Impl.isSimple(ex2, lambdaSimple = true)
                        val shouldInline = (!rec1.isRecursive) && {
                          (cnt == 1) || isSimp
                        }
                        // we don't want to inline a value that is itself a function call
                        // inside of lambdas
                        val inlined =
                          if (shouldInline)
                            substitute(arg, ex2, in1, enterLambda = isSimp)
                          else None
                        inlined match {
                          case Some(il) =>
                            normalize1(namerec, il, scope, typeEnv)
                          case None =>
                            val step = Let(arg, ex2, in1, rec1, tag)
                            if ((step: TypedExpr[A]) === te) None
                            else normalize1(namerec, step, scope, typeEnv)
                        }
                      } else {
                        // let x = y in z if x isn't free in z = z
                        Some(in1)
                      }
                  }
              }
          }
      case Loop(args, body, tag) =>
        def hasOuterRecur(te: TypedExpr[A], inNestedLoop: Boolean): Boolean =
          te match {
            case Generic(_, in) =>
              hasOuterRecur(in, inNestedLoop)
            case Annotation(in, _) =>
              hasOuterRecur(in, inNestedLoop)
            case AnnotatedLambda(_, in, _) =>
              hasOuterRecur(in, inNestedLoop)
            case App(fn, appArgs, _, _) =>
              hasOuterRecur(fn, inNestedLoop) || appArgs.exists(
                hasOuterRecur(_, inNestedLoop)
              )
            case Let(_, expr, in, _, _) =>
              hasOuterRecur(expr, inNestedLoop) || hasOuterRecur(in, inNestedLoop)
            case Loop(loopArgs, loopBody, _) =>
              loopArgs.exists { case (_, expr) =>
                hasOuterRecur(expr, inNestedLoop)
              } || hasOuterRecur(loopBody, inNestedLoop = true)
            case Recur(_, _, _) =>
              !inNestedLoop
            case Match(arg, branches, _) =>
              hasOuterRecur(arg, inNestedLoop) || branches.exists {
                case (_, branchExpr) =>
                  hasOuterRecur(branchExpr, inNestedLoop)
              }
            case Local(_, _, _) | Global(_, _, _, _) | Literal(_, _, _) =>
              false
          }

        val args1 = ListUtil.mapConserveNel(args) { case (n, expr) =>
          (n, normalize1(None, expr, scope, typeEnv).get)
        }
        val bodyScope = scope -- args1.toList.map(_._1)
        val body1 = normalize1(None, body, bodyScope, typeEnv).get

        if (!hasOuterRecur(body1, inNestedLoop = false)) {
          normalize1(
            namerec,
            TypedExpr.letAllNonRec(args1, body1, tag),
            scope,
            typeEnv
          )
        } else {
          val loopNames = args1.iterator.map(_._1).toVector
          val invariantFlags =
            outerRecurInvariantFlags(body1, loopNames, inNestedLoop = false)
              .getOrElse(Vector.fill(loopNames.length)(false))

          // Loop/Recur requires at least one argument.
          // Keep the final slot to guarantee we never create an empty loop.
          val liftFlags =
            invariantFlags.updated(invariantFlags.length - 1, false)

          if (liftFlags.contains(true)) {
            val (liftedRev, keptRev) =
              args1.toList.iterator
                .zip(liftFlags.iterator)
                .foldLeft(
                  (
                    List.empty[(Bindable, TypedExpr[A])],
                    List.empty[(Bindable, TypedExpr[A])]
                  )
                ) { case ((liftedAcc, keptAcc), (argDef, shouldLift)) =>
                  if (shouldLift) (argDef :: liftedAcc, keptAcc)
                  else (liftedAcc, argDef :: keptAcc)
                }

            val lifted = liftedRev.reverse
            val kept = keptRev.reverse
            val body2 = dropOuterRecurArgs(body1, liftFlags, inNestedLoop = false)
            val loop2 = Loop(NonEmptyList.fromListUnsafe(kept), body2, tag)
            val rewritten =
              NonEmptyList.fromList(lifted) match {
                case Some(liftedNel) =>
                  // Lifted loop invariants no longer need to be threaded through recur.
                  TypedExpr.letAllNonRec(liftedNel, loop2, tag)
                case None =>
                  loop2
              }

            normalize1(namerec, rewritten, scope, typeEnv)
          } else {
            val loop1 = Loop(args1, body1, tag)
            if ((loop1: TypedExpr[A]) === te) None
            else Some(loop1)
          }
        }
      case Recur(args, tpe0, tag) =>
        val tpe = Type.normalize(tpe0)
        val args1 = ListUtil.mapConserveNel(args) { arg =>
          normalize1(None, arg, scope, typeEnv).get
        }
        val recur1 = Recur(args1, tpe, tag)
        if ((recur1: TypedExpr[A]) === te) None
        else Some(recur1)

      case Match(_, NonEmptyList((p, e), Nil), _)
          if !e.freeVarsDup.exists(p.names.toSet) =>
        // match x:
        //   foo: fn
        //
        // where foo has no names can become just fn
        normalize1(namerec, e, scope, typeEnv)
      case Match(arg, NonEmptyList((Pattern.SinglyNamed(y), e), Nil), tag) =>
        // match x:
        //   y: fn
        // let y = x in fn
        normalize1(
          namerec,
          Let(y, arg, e, RecursionKind.NonRecursive, tag),
          scope,
          typeEnv
        )
      case Match(arg, branches, tag) =>
        def ncount(
            shadows: Iterable[Bindable],
            e: TypedExpr[A]
        ): (Int, TypedExpr[A]) =
          // the final result of the branch is what is assigned to the name
          normalizeLetOpt(None, e, scope -- shadows, typeEnv) match {
            case None    => (0, e)
            case Some(e) => (1, e)
          }
        // we can remove any bindings that aren't used in branches
        val (changed0, branches1) =
          branches
            .traverse { case (p, t) =>
              val (c, t1) = ncount(p.names, t)
              val freeT1 = t1.freeVarsDup.toSet
              // we don't need to keep any variables that aren't free
              // TODO: we can still replace total matches with _
              // such as Foo(_, _, _) for structs or unions that are total
              val p1 = p.filterVars(freeT1)
              val c1 = if (p1 == p) c else (c + 1)
              (c1, (p1, t1))
            }
        // due to total matches, the last branch without any bindings
        // can always be rewritten as _
        val (changed1, branches1a) =
          branches1.last._1 match {
            case Pattern.WildCard =>
              (changed0, branches1)
            case notWild if notWild.names.isEmpty =>
              val newb = branches1.init ::: ((
                Pattern.WildCard,
                branches1.last._2
              ) :: Nil)
              // this newb list clearly has more than 0 elements
              (changed0 + 1, NonEmptyList.fromListUnsafe(newb))
            case _ =>
              (changed0, branches1)
          }
        val a1 = normalize1(None, arg, scope, typeEnv).get
        if (changed1 == 0) {
          val m1 = Match(a1, branches, tag)
          Impl.maybeEvalMatch(m1, scope) match {
            case None =>
              // if only the arg changes, there
              // is no need to rerun the normalization
              // because normalization of branches
              // does not depend on the arg
              if (a1 eq arg) None
              else Some(m1)
            case Some(m2) =>
              // TODO: we may not have a proof that m2 is smaller
              // than m1. requiring m2.size < m1.size fails some tests
              // we can possibly simplify this now:
              normalize1(namerec, m2, scope, typeEnv)
          }
        } else {
          // there has been some change, so
          // see if that unlocked any new changes
          normalize1(namerec, Match(a1, branches1a, tag), scope, typeEnv)
        }
    }
  }

  def normalize[A: Eq](te: TypedExpr[A]): Option[TypedExpr[A]] =
    normalizeLetOpt(None, te, emptyScope, TypeEnv.empty)

  private object Impl {

    def scopeMatches[A](
        names: Set[Bindable],
        scope: Scope[A],
        scope1: Scope[A]
    ): Boolean =
      names.forall { b =>
        (scope.getLocal(b), scope1.getLocal(b)) match {
          case (None, None)                             => true
          case (Some((r1, t1, s1)), Some((r2, t2, s2))) =>
            (r1 == r2) &&
            (t1.void === t2.void) &&
            scopeMatches(t1.freeVarsDup.toSet, s1, s2)
          case _ => false
        }
      }

    case class WithScope[A](scope: Scope[A], typeEnv: TypeEnv[Kind.Arg]) {
      private lazy val kindOf: Type => Option[Kind] =
        Type.kindOfOption { case const @ Type.TyConst(_) =>
          typeEnv.getType(const).map(_.kindOf)
        }

      object ResolveToLambda {
        // this is a parameter that we can tune to change inlining Global Lambdas
        val MaxSize = 10

        // TODO: don't we need to worry about the type environment for locals? They
        // can also capture type references to outer Generics
        def unapply(te: TypedExpr[A]): Option[
          (
              List[(Type.Var.Bound, Kind)],
              NonEmptyList[(Bindable, Type)],
              TypedExpr[A],
              A
          )
        ] =
          te match {
            case Annotation(
                  ResolveToLambda((h :: t), args, ex, tag),
                  rho: Type.Rho
                ) =>
              val body = AnnotatedLambda(args, ex, tag)
              val quant = Quantification.ForAll(NonEmptyList(h, t))
              val asGen = Generic(quant, body)

              TypedExpr.instantiateTo(asGen, rho, kindOf) match {
                case AnnotatedLambda(a, e, t) => Some((Nil, a, e, t))
                case Generic(
                      Quantification.ForAll(nel),
                      AnnotatedLambda(a, e, t)
                    ) =>
                  Some((nel.toList, a, e, t))
                case _ => None
              }
            case Generic(
                  Quantification.ForAll(frees),
                  ResolveToLambda(f1, args, ex, tag)
                ) =>
              Some((frees.toList ::: f1, args, ex, tag))
            case AnnotatedLambda(args, expr, ltag) =>
              Some((Nil, args, expr, ltag))
            case Global(p, n: Bindable, _, _) =>
              scope.getGlobal(p, n).flatMap {
                // only inline global lambdas if they are somewhat small, otherwise we will
                // tend to transitively inline everything into one big function and blow the stack
                case (RecursionKind.NonRecursive, te, scope1)
                    if te.size < MaxSize =>
                  val s1 = WithScope(scope1, typeEnv)
                  te match {
                    case s1.ResolveToLambda(frees, args, expr, ltag) =>
                      // we can't just replace variables if the scopes don't match.
                      // we could also repair the scope by making a let binding
                      // for any names that don't match (which has to be done recursively
                      if (
                        scopeMatches(
                          expr.freeVarsDup.toSet -- args.iterator.map(_._1),
                          scope,
                          scope1
                        )
                      ) {
                        Some((frees, args, expr, ltag))
                      } else None
                    case _ => None
                  }
                case _ => None
              }
            case Local(nm, _, _) =>
              scope.getLocal(nm).flatMap {
                // Local lambdas tend to be small, so inline them always if we can
                case (RecursionKind.NonRecursive, te, scope1) =>
                  val s1 = WithScope(scope1, typeEnv)
                  te match {
                    case s1.ResolveToLambda(frees, args, expr, ltag) =>
                      // we can't just replace variables if the scopes don't match.
                      // we could also repair the scope by making a let binding
                      // for any names that don't match (which has to be done recursively
                      if (
                        scopeMatches(
                          expr.freeVarsDup.toSet -- args.iterator.map(_._1),
                          scope,
                          scope1
                        )
                      ) {
                        Some((frees, args, expr, ltag))
                      } else None
                    case _ => None
                  }
                case _ => None
              }
            case _ => None
          }
      }
    }

    final def isSimpleNotTail[A](
        ex: TypedExpr[A],
        lambdaSimple: Boolean
    ): Boolean =
      isSimple(ex, lambdaSimple)

    @annotation.tailrec
    final def isSimple[A](ex: TypedExpr[A], lambdaSimple: Boolean): Boolean =
      ex match {
        case Literal(_, _, _) | Local(_, _, _) | Global(_, _, _, _) => true
        case App(_, _, _, _)                                        => false
        case Annotation(t, _)         => isSimple(t, lambdaSimple)
        case Generic(_, t)            => isSimple(t, lambdaSimple)
        case AnnotatedLambda(_, _, _) =>
          // maybe inline lambdas so we can possibly
          // apply (x -> f)(g) => let x = g in f
          lambdaSimple
        case Let(_, ex, in, _, _) =>
          isSimpleNotTail(ex, lambdaSimple) && isSimple(in, lambdaSimple)
        case Loop(_, _, _) | Recur(_, _, _) =>
          false
        case Match(arg, branches, _) =>
          branches.tail.isEmpty && isSimpleNotTail(arg, lambdaSimple) && {
            // match f: case p: r
            // is the same as
            // let p = f in r
            val (_, rest) = branches.head
            isSimple(rest, lambdaSimple)
          }
      }

    sealed abstract class EvalResult[A]
    object EvalResult {
      case class Cons[A](
          pack: PackageName,
          cons: Constructor,
          args: List[TypedExpr[A]]
      ) extends EvalResult[A]
      case class Constant[A](lit: Lit) extends EvalResult[A]
    }

    object FnArgs {
      def unapply[A](
          te: TypedExpr[A]
      ): Option[(TypedExpr[A], NonEmptyList[TypedExpr[A]])] =
        te match {
          case App(fn, args, _, _) => Some((fn, args))
          case _                   => None
        }
    }

    def evaluate[A](te: TypedExpr[A], scope: Scope[A]): Option[EvalResult[A]] =
      te match {
        case Literal(lit, _, _) => Some(EvalResult.Constant(lit))
        case Local(b, _, _)     =>
          scope.getLocal(b).flatMap {
            case (RecursionKind.NonRecursive, t, s) =>
              // local values may have free values defined in
              // their scope. we could handle these with let bindings
              if (scopeMatches(t.freeVarsDup.toSet, s, scope)) evaluate(t, s)
              else None
            case _ => None
          }
        case Let(arg, expr, in, RecursionKind.NonRecursive, _) =>
          evaluate(
            in,
            scope.updated(arg, (RecursionKind.NonRecursive, expr, scope))
          )
        case FnArgs(fn, args) =>
          evaluate(fn, scope).map {
            case EvalResult.Cons(p, c, ahead) =>
              EvalResult.Cons(p, c, ahead ::: args.toList)
            // $COVERAGE-OFF$
            case EvalResult.Constant(c) =>
              // this really shouldn't happen,
              sys.error(
                s"unreachable: cannot apply a constant: $te => ${fn.repr} => $c"
              )
            // $COVERAGE-ON$
          }
        case Global(pack, cons: Constructor, _, _) =>
          Some(EvalResult.Cons(pack, cons, Nil))
        case Global(pack, n: Bindable, _, _) =>
          scope.getGlobal(pack, n).flatMap {
            case (RecursionKind.NonRecursive, t, s) =>
              // Global values never have free values,
              // so it is safe to substitute into our current scope
              evaluate(t, s)
            case _ => None
          }
        case Generic(_, in) =>
          // if we can evaluate, we are okay
          evaluate(in, scope)
        case Annotation(te, _) =>
          evaluate(te, scope)
        case _ =>
          None
      }

    type Pat = Pattern[(PackageName, Constructor), Type]
    type Branch[A] = (Pat, TypedExpr[A])

    def maybeEvalMatch[A: Eq](
        m: Match[? <: A],
        scope: Scope[A]
    ): Option[TypedExpr[A]] = {
      evaluate(m.arg, scope).flatMap {
        case EvalResult.Cons(p, c, args) =>
          val alen = args.length

          def isTotal(p: Pat): Boolean =
            p match {
              case Pattern.WildCard | Pattern.Var(_) => true
              case Pattern.Named(_, p)               => isTotal(p)
              case Pattern.Annotation(p, _)          => isTotal(p)
              case Pattern.Union(h, t) => isTotal(h) || t.exists(isTotal)
              case _                   => false
            }

          // The Option signals we can't complete
          def filterPat(pat: Pat): Option[Option[Pat]] =
            pat match {
              case ps @ Pattern.PositionalStruct((p0, c0), args0) =>
                if (p0 == p && c0 == c && args0.length == alen)
                  Some(Some(ps))
                else Some(None) // we definitely don't match this branch
              case Pattern.Named(n, p) =>
                filterPat(p).map { p1 =>
                  p1.map(bp => Pattern.Named(n, bp))
                }
              case Pattern.Annotation(p, _) =>
                // The annotation is only used at inference time, the values have already been typed
                filterPat(p)
              case Pattern.Union(h, t) =>
                (filterPat(h), t.traverse(filterPat))
                  .mapN { (optP1, p2s) =>
                    val flatP2s: List[Pat] = p2s.toList.flatten
                    optP1 match {
                      case None =>
                        flatP2s match {
                          case Nil    => None
                          case h :: t => Some(Pattern.union(h, t))
                        }
                      case Some(p1) => Some(Pattern.union(p1, flatP2s))
                    }
                  }
              case Pattern.WildCard | Pattern.Var(_) => Some(Some(pat))
              case Pattern.ListPat(_)                =>
                // TODO some of these patterns we could evaluate
                None
              case _ => None
            }

          object MaybeNamedStruct {
            def unapply(p: Pat): Option[(List[Bindable], List[Pat])] =
              p match {
                case Pattern.Named(n, MaybeNamedStruct(ns, pats)) =>
                  Some((n :: ns, pats))
                case Pattern.PositionalStruct(_, pats) =>
                  Some((Nil, pats))
                case Pattern.WildCard => Some((Nil, args.as(Pattern.WildCard)))
                case Pattern.Var(n)   =>
                  Some((n :: Nil, args.as(Pattern.WildCard)))
                case _ =>
                  None
              }
          }

          m.branches
            .traverse { case (p, r) => filterPat(p).map((_, r)) }
            // if we can check all the branches for a match, maybe we can evaluate
            .flatMap { branches =>
              val candidates: List[(Pat, TypedExpr[A])] =
                branches.collect { case (Some(p), r) => (p, r) }

              candidates match {
                // $COVERAGE-OFF$
                case Nil =>
                  // TODO hitting this looks like a bug
                  sys.error(
                    s"no branch matched in ${m.repr} matched: $p::$c(${args.map(_.repr)})"
                  )
                // $COVERAGE-ON$
                case (MaybeNamedStruct(b, pats), r) :: rest
                    if rest.isEmpty || pats.forall(isTotal) =>
                  // If there are no more items, or all inner patterns are total, we are done
                  // exactly one matches, this can be a sequential match
                  def matchAll(
                      argPat: List[
                        (
                            TypedExpr[A],
                            Pattern[(PackageName, Constructor), Type]
                        )
                      ]
                  ): TypedExpr[A] =
                    argPat match {
                      case Nil            => r
                      case (a, p) :: tail =>
                        val tr = matchAll(tail)
                        p match {
                          case Pattern.WildCard =>
                            // we don't care about this value
                            tr
                          case Pattern.Var(b) =>
                            Let(b, a, tr, RecursionKind.NonRecursive, m.tag)
                          case _ =>
                            // This will get simplified later
                            Match(a, NonEmptyList.one((p, tr)), m.tag)
                        }
                    }

                  val res = matchAll(args.zip(pats))
                  Some(
                    b.foldRight(res)(
                      Let(_, m.arg, _, RecursionKind.NonRecursive, m.tag)
                    )
                  )
                case h :: t =>
                  // more than one branch might match, wait till runtime
                  val m1 = Match(m.arg, NonEmptyList(h, t), m.tag)
                  if ((m1: TypedExpr[A]) === m) None
                  else Some(m1)
              }
            }

        case EvalResult.Constant(li) =>

          def makeLet(
              p: Pattern[(PackageName, Constructor), Type]
          ): Option[List[(Bindable, Lit)]] =
            p match {
              case Pattern.Named(v, p) =>
                makeLet(p).map((v, li) :: _)
              case Pattern.WildCard => Some(Nil)
              case Pattern.Var(v)   =>
                Some((v, li) :: Nil)
              case Pattern.Annotation(p, _) => makeLet(p)
              case Pattern.Literal(litj)    =>
                if (li === litj) Some(Nil)
                else None
              case Pattern.Union(h, t) =>
                (h :: t).toList.iterator.map(makeLet).reduce(_.orElse(_))
              case sp @ Pattern.StrPat(_) =>
                li match {
                  case Lit.Str(str) =>
                    StrPart.matchPattern(str, sp)
                  // $COVERAGE-OFF$ these are ill-typed so should be unreachable
                  case _ => None
                }

              case Pattern.PositionalStruct(_, _) | Pattern.ListPat(_) =>
                //
                None
              // $COVERAGE-ON$
            }

          Foldable[NonEmptyList]
            .collectFirstSome[Branch[A], TypedExpr[A]](m.branches) {
              case (p, r) =>
                makeLet(p).map { binds =>
                  binds.foldRight(r) { case ((n, li), r) =>
                    val te = Literal[A](li, Type.getTypeOf(li), m.arg.tag)
                    Let(n, te, r, RecursionKind.NonRecursive, m.tag)
                  }
                }
            }
      }
    }

    sealed abstract class IntAlgebraic[A] {
      lazy val toTypedExpr: TypedExpr[A] = {
        import IntAlgebraic._

        this match {
          case Add(left, right, addFn, tag) =>
            App(
              addFn,
              NonEmptyList(left.toTypedExpr, right.toTypedExpr :: Nil),
              Type.IntType,
              tag
            )
          case Times(left, right, timesFn, tag) =>
            App(
              timesFn,
              NonEmptyList(left.toTypedExpr, right.toTypedExpr :: Nil),
              Type.IntType,
              tag
            )
          case Sub(left, right, subFn, tag) =>
            App(
              subFn,
              NonEmptyList(left.toTypedExpr, right.toTypedExpr :: Nil),
              Type.IntType,
              tag
            )
          case LiteralInt(bi, tag) =>
            Literal(Lit.Integer(bi), Type.IntType, tag)
          case OpaqueInt(intValue) => intValue
        }
      }

      def tag: A
    }
    object IntAlgebraic {
      case class Add[A](
          left: IntAlgebraic[A],
          right: IntAlgebraic[A],
          addFn: TypedExpr[A],
          tag: A
      ) extends IntAlgebraic[A]
      case class Times[A](
          left: IntAlgebraic[A],
          right: IntAlgebraic[A],
          timesFn: TypedExpr[A],
          tag: A
      ) extends IntAlgebraic[A]
      case class Sub[A](
          left: IntAlgebraic[A],
          right: IntAlgebraic[A],
          subFn: TypedExpr[A],
          tag: A
      ) extends IntAlgebraic[A]
      case class LiteralInt[A](bi: BigInteger, tag: A) extends IntAlgebraic[A]
      case class OpaqueInt[A](intValue: TypedExpr[A]) extends IntAlgebraic[A] {
        def tag: A = intValue.tag
      }

      private val Fn2Int = Type.Fun(
        NonEmptyList(Type.IntType, Type.IntType :: Nil),
        Type.IntType
      )

      def optimize[A](
          root: IntAlgebraic[A],
          te: TypedExpr[A]
      ): Option[TypedExpr[A]] = {

        case class Table(
            invert: Vector[TypedExpr[Unit]],
            toMap: Map[TypedExpr[Unit], (Int, NonEmptyList[TypedExpr[A]])],
            litTags: Map[BigInteger, A]
        ) {

          def idOf(te: TypedExpr[A]): (Table, Int) = {
            val teVoid = te.void
            toMap.get(teVoid) match {
              case Some((idx, nel)) =>
                // just add this one to the list
                val nel1 = te :: nel
                (copy(toMap = toMap.updated(teVoid, (idx, nel1))), idx)
              case None =>
                val invert1 = invert :+ teVoid
                val idx = invert.length
                val toMap1 = toMap.updated(teVoid, (idx, NonEmptyList(te, Nil)))
                (Table(invert1, toMap1, litTags), idx)
            }
          }
          def typeExpr(idx: Int): TypedExpr[A] =
            // we should only call this for ids we have assigned. We could enforce this
            // by using an abstract type and provide the Hash[Idx] and Order[Idx] but it's
            // not needed
            toMap(invert(idx))._2.last

          def biTag(bi: BigInteger, tag: A): Table =
            // keep only the first tag we find, this is lossy
            copy(litTags =
              if (litTags.contains(bi)) litTags else (litTags.updated(bi, tag))
            )

          def tagForBi(bi: BigInteger): A =
            litTags.get(bi) match {
              case Some(a) => a
              case None    => te.tag
            }
          def tagFromUntagged(teu: TypedExpr[Unit]): A =
            toMap.get(teu) match {
              case Some((_, nel)) => nel.last.tag
              case None           =>
                // just use the root tag
                te.tag
            }
        }

        val EmptyTable: Table = {
          val t0 = Table(Vector.empty, Map.empty, Map.empty)
          // insert the root TypedExpr into the table
          val (t1, _) = t0.idOf(te)
          t1
        }

        def toExpr(
            ia: IntAlgebraic[A],
            table: Table
        ): (Table, RingOpt.Expr[Int]) =
          ia match {
            case Add(left, right, addFn, tag) =>
              val (te0, _) = table.idOf(addFn)
              val (te1, _) = te0.idOf(ia.toTypedExpr)
              val (te2, l) = toExpr(left, te1)
              val (te3, r) = toExpr(right, te2)
              (te3, l + r)
            case Times(left, right, timesFn, tag) =>
              val (te0, _) = table.idOf(timesFn)
              val (te1, _) = te0.idOf(ia.toTypedExpr)
              val (te2, l) = toExpr(left, te1)
              val (te3, r) = toExpr(right, te2)
              (te3, l * r)
            case Sub(left, right, subFn, tag) =>
              val (te0, _) = table.idOf(subFn)
              val (te1, _) = te0.idOf(ia.toTypedExpr)
              val (te2, l) = toExpr(left, te1)
              val (te3, r) = toExpr(right, te2)
              (te3, l - r)
            case LiteralInt(bi, tag) =>
              (table.biTag(bi, tag), RingOpt.canonInt(BigInt(bi)))
            case OpaqueInt(intValue) =>
              val (te1, idx) = table.idOf(intValue)
              (te1, RingOpt.Symbol(idx))
          }

        def toAlg(
            e: RingOpt.Expr[Int],
            table: Table
        ): IntAlgebraic[A] =
          e match {
            case RingOpt.Zero =>
              LiteralInt(BigInteger.ZERO, table.tagForBi(BigInteger.ZERO))
            case RingOpt.One =>
              LiteralInt(BigInteger.ONE, table.tagForBi(BigInteger.ONE))
            case RingOpt.Integer(toBigInt) =>
              val bi = toBigInt.bigInteger
              LiteralInt(bi, table.tagForBi(bi))
            case RingOpt.Symbol(idx) =>
              OpaqueInt(table.typeExpr(idx))
            case RingOpt.Add(pos, RingOpt.Neg(neg)) =>
              // pos + (-neg) == pos - neg
              val untaggedFn = Global(
                PackageName.PredefName,
                Identifier.Name("sub"),
                Fn2Int,
                ()
              )
              val posIA = toAlg(pos, table)
              val negIA = toAlg(neg, table)
              val subTag = table.tagFromUntagged(untaggedFn)
              val subFn = untaggedFn.copy(tag = subTag)
              val fakeTagged = Sub(posIA, negIA, subFn, subTag)
              val realTag = table.tagFromUntagged(fakeTagged.toTypedExpr.void)
              Sub(posIA, negIA, subFn, realTag)
            case RingOpt.Add(RingOpt.Neg(neg), pos) =>
              // (-neg) + pos == pos - neg
              val untaggedFn = Global(
                PackageName.PredefName,
                Identifier.Name("sub"),
                Fn2Int,
                ()
              )
              val posIA = toAlg(pos, table)
              val negIA = toAlg(neg, table)
              val subTag = table.tagFromUntagged(untaggedFn)
              val subFn = untaggedFn.copy(tag = subTag)
              val fakeTagged = Sub(posIA, negIA, subFn, subTag)
              val realTag = table.tagFromUntagged(fakeTagged.toTypedExpr.void)
              Sub(posIA, negIA, subFn, realTag)
            case RingOpt.Add(left, right) =>
              val untaggedFn = Global(
                PackageName.PredefName,
                Identifier.Name("add"),
                Fn2Int,
                ()
              )
              val leftIA = toAlg(left, table)
              val rightIA = toAlg(right, table)
              val addTag = table.tagFromUntagged(untaggedFn)
              val addFn = untaggedFn.copy(tag = addTag)
              val fakeTagged = Add(leftIA, rightIA, addFn, addTag)
              val realTag = table.tagFromUntagged(fakeTagged.toTypedExpr.void)
              Add(leftIA, rightIA, addFn, realTag)
            case RingOpt.Mult(left, right) =>
              val untaggedFn = Global(
                PackageName.PredefName,
                Identifier.Name("mul"),
                Fn2Int,
                ()
              )
              val leftIA = toAlg(left, table)
              val rightIA = toAlg(right, table)
              val timesTag = table.tagFromUntagged(untaggedFn)
              val timesFn = untaggedFn.copy(tag = timesTag)
              val fakeTagged = Times(leftIA, rightIA, timesFn, timesTag)
              val realTag = table.tagFromUntagged(fakeTagged.toTypedExpr.void)
              Times(leftIA, rightIA, timesFn, realTag)
            case neg @ RingOpt.Neg(_) =>
              toAlg(RingOpt.Add(RingOpt.Zero, neg), table)
          }

        def collectOpaque(
            ia: IntAlgebraic[A],
            acc: List[TypedExpr[A]]
        ): List[TypedExpr[A]] =
          ia match {
            case Add(l, r, _, _) =>
              collectOpaque(r, collectOpaque(l, acc))
            case Times(l, r, _, _) =>
              collectOpaque(r, collectOpaque(l, acc))
            case Sub(l, r, _, _) =>
              collectOpaque(r, collectOpaque(l, acc))
            case LiteralInt(_, _) =>
              acc
            case OpaqueInt(intValue) =>
              intValue :: acc
          }

        def replaceOpaque(
            ia: IntAlgebraic[A],
            repl: Map[TypedExpr[Unit], Bindable]
        ): IntAlgebraic[A] =
          ia match {
            case Add(l, r, addFn, tag) =>
              Add(replaceOpaque(l, repl), replaceOpaque(r, repl), addFn, tag)
            case Times(l, r, timesFn, tag) =>
              Times(
                replaceOpaque(l, repl),
                replaceOpaque(r, repl),
                timesFn,
                tag
              )
            case Sub(l, r, subFn, tag) =>
              Sub(replaceOpaque(l, repl), replaceOpaque(r, repl), subFn, tag)
            case LiteralInt(_, _) =>
              ia
            case OpaqueInt(intValue) =>
              repl.get(intValue.void) match {
                case None     => ia
                case Some(nm) =>
                  OpaqueInt(Local(nm, intValue.getType, intValue.tag))
              }
          }

        def dedupOpaque(ia: IntAlgebraic[A]): TypedExpr[A] = {
          val opaqueList = collectOpaque(ia, Nil).reverse
          if (opaqueList.isEmpty) ia.toTypedExpr
          else {
            val seen = scala.collection.mutable.LinkedHashMap
              .empty[TypedExpr[Unit], TypedExpr[A]]
            val counts = scala.collection.mutable.HashMap
              .empty[TypedExpr[Unit], Int]

            opaqueList.foreach { te =>
              val key = te.void
              if (!seen.contains(key)) seen.update(key, te)
              counts.updateWith(key) {
                case None    => Some(1)
                case Some(c) => Some(c + 1)
              }
            }

            val dupKeys = seen.iterator.collect {
              case (key, rep)
                  if (counts.getOrElse(key, 0) > 1) &&
                    !Impl.isSimple(rep, lambdaSimple = false) =>
                key
            }.toList

            if (dupKeys.isEmpty) ia.toTypedExpr
            else {
              val avoid = TypedExpr.allVarsSet(te :: Nil)
              val used = scala.collection.mutable.HashSet
                .empty[Bindable] ++ avoid
              val names = rankn.Type.allBinders.iterator.map { b =>
                Identifier.synthetic(b.name)
              }

              def nextName(): Bindable = {
                var n = names.next()
                while (used(n)) n = names.next()
                used.add(n)
                n
              }

              val bindings = dupKeys.map { key =>
                val nm = nextName()
                (key, nm, seen(key))
              }

              val repl = bindings.iterator.map { case (key, nm, _) =>
                (key, nm)
              }.toMap

              val ia1 = replaceOpaque(ia, repl)
              val te1 = ia1.toTypedExpr
              val bindsNel = NonEmptyList.fromListUnsafe(
                bindings.map { case (_, nm, expr) => (nm, expr) }
              )
              TypedExpr.letAllNonRec(bindsNel, te1, te.tag)
            }
          }
        }

        val (table, expr) = toExpr(root, EmptyTable)
        val w = RingOpt.Weights(mult = 3, add = 1, neg = 1)
        val norm = RingOpt.normalize(expr, w)

        if (cats.Hash[RingOpt.Expr[Int]].eqv(norm, expr)) {
          // the expression is exactly the same
          None
        } else {
          val ia1 = toAlg(norm, table)
          val te1 = ia1.toTypedExpr
          if (te1.void === root.toTypedExpr.void) {
            // since the AST of RingOpt isn't the same as TypedExpr
            // we can improve the RingOpt AST but the resulting TypedExpr
            // AST may be exactly the same. Unfortunately, this check
            // is safer to do.
            // for instance: 0 - x can be optimized to -x in RingOpt
            // but currently, the predef doesn't have negate, and just
            // uses sub to do this (which at codegen time we can always)
            // optimize into a negate at the top level as needed.
            None
          } else {
            Some(dedupOpaque(ia1))
          }
        }
      }

      object Optimized {
        def unapply[A](te: TypedExpr[A]): Option[TypedExpr[A]] =
          Impl.IntAlgebraic.unapply(te) match {
            case Some(ia) => optimize(ia, te)
            case None     => None
          }
      }

      def unapply[A](te: TypedExpr[A]): Option[IntAlgebraic[A]] =
        te match {

          case App(
                addFn @ Global(
                  PackageName.PredefName,
                  Identifier.Name("add"),
                  Fn2Int,
                  _
                ),
                NonEmptyList(left, right :: _),
                _,
                tag
              ) =>
            val leftArg = unapply[A](left).getOrElse(OpaqueInt(left))
            val rightArg = unapply[A](right).getOrElse(OpaqueInt(right))
            Some(Add(leftArg, rightArg, addFn, tag))
          case App(
                timesFn @ Global(
                  PackageName.PredefName,
                  Identifier.Name("mul"),
                  Fn2Int,
                  _
                ),
                NonEmptyList(left, right :: _),
                _,
                tag
              ) =>
            val leftArg = unapply[A](left).getOrElse(OpaqueInt(left))
            val rightArg = unapply[A](right).getOrElse(OpaqueInt(right))
            Some(Times(leftArg, rightArg, timesFn, tag))
          case App(
                subFn @ Global(
                  PackageName.PredefName,
                  Identifier.Name("sub"),
                  Fn2Int,
                  _
                ),
                NonEmptyList(left, right :: _),
                _,
                tag
              ) =>
            val leftArg = unapply[A](left).getOrElse(OpaqueInt(left))
            val rightArg = unapply[A](right).getOrElse(OpaqueInt(right))
            Some(Sub(leftArg, rightArg, subFn, tag))
          case Literal(Lit.Integer(bi), _, tag) => Some(LiteralInt(bi, tag))
          case _                                => None
        }
    }
  }

}
