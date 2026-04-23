package dev.bosatsu

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.all._
import dev.bosatsu.rankn.{Type, TypeEnv}

import Identifier.Bindable

object TypedExprLoopRecurLowering {
  import TypedExpr._

  private type Group = NonEmptyList[(Bindable, Type)]

  // Local minimal-allocation Option product for this file.
  private inline def map2[A, B, C](
      oa: Option[A],
      inline ob: => Option[B]
  )(inline fn: (A, B) => C): Option[C] =
    if (oa.isDefined) {
      // we don't have to compute ob if oa isn't defined
      val compB = ob
      if (compB.isDefined) Some(fn(oa.get, compB.get))
      else None
    } else None

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
          val canRecurOuter =
            canRecur && !branch.pattern.names.contains(name)
          val canRecurInner =
            canRecurOuter && !branch.guardBindings.contains(name)
          val guard1 = branch.mapGuardNodeExprScoped(
            rewriteTailCalls(name, _, tailPos = false, canRecurOuter),
            rewriteTailCalls(name, _, tailPos = false, canRecurInner)
          )
          val branchExpr1 =
            rewriteTailCalls(name, branch.expr, tailPos, canRecurInner)
          if (guard1.eq(branch.guardNode) && (branchExpr1 eq branch.expr)) branch
          else
            branch.copyNode(
              branch.pattern,
              guard1,
              branchExpr1
            )(using branch.patternRegion)
        }
        if ((arg1 eq arg) && (branches1 eq branches)) m
        else Match(m.matchKind, arg1, branches1, tag)
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
          branch =>
            branch.guardExprIterator.exists(hasOuterRecur(_, inNestedLoop)) ||
            hasOuterRecur(branch.expr, inNestedLoop)
        }
      case Local(_, _, _) | Global(_, _, _, _) | Literal(_, _, _) =>
        false
    }

  // Reader-only helper: this inspects shape and must never be used to rebuild
  // expressions, since it drops type wrappers.
  @annotation.tailrec
  private def stripTypeWrappers[A](te: TypedExpr[A]): TypedExpr[A] =
    te match {
      case Generic(_, in)    => stripTypeWrappers(in)
      case Annotation(in, _, _) => stripTypeWrappers(in)
      case other             => other
    }

  private def isPredefBoolCtor[A](
      te: TypedExpr[A],
      ctorName: String
  ): Boolean =
    stripTypeWrappers(te) match {
      case Global(
            PackageName.PredefName,
            Identifier.Constructor(`ctorName`),
            _,
            _
          ) =>
        true
      case _ =>
        false
    }

  private def decodeSourceLoopMatchGuard[A](
      guardExpr: TypedExpr[A]
  ): Option[TypedExpr.MatchGuard[A]] =
    stripTypeWrappers(guardExpr) match {
      // Recursive source `loop`/`recur` branches should have already
      // classified whole-guard `matches` forms as scoped guards. If such a
      // branch still reaches loop lowering in the canonical `match ... True/False`
      // encoding, recover the scoped form before binder-sensitive rewrites.
      case Match(argExpr, branches, _) =>
        branches.toList match {
          case head :: tail :: Nil
              if tail.pattern == Pattern.WildCard &&
                tail.guardNode.isEmpty &&
                isPredefBoolCtor(head.expr, "True") &&
                isPredefBoolCtor(tail.expr, "False") =>
            Some(
              TypedExpr.MatchGuard(
                argExpr,
                head.pattern,
                head.guard
              )(using head.patternRegion)
            )
          case _ =>
            None
        }
      case _ =>
        None
    }

  private def recursiveGuardRecoveryProvenance(
      tag: Any,
      branchCount: Int
  ): Option[Vector[Boolean]] =
    tag match {
      case Declaration.Match(kind, _, cases) if kind.isRecursive =>
        val provenance =
          cases.get.toList.map(_.guard.exists(Declaration.ConditionalMatch.unapply(_).nonEmpty))
        if (provenance.lengthCompare(branchCount) == 0)
          Some(provenance.toVector)
        else None
      case _ =>
        None
    }

  private def restoreRecursiveMatchGuard[A](
      branch: TypedExpr.Branch[A],
      allowRecovery: Boolean
  ): TypedExpr.Branch[A] =
    if (!allowRecovery) branch
    else branch.guardNode match {
      case Some(TypedExpr.BoolGuard(guardExpr)) =>
        decodeSourceLoopMatchGuard(guardExpr) match {
          case Some(matchGuard) =>
            branch.copyNode(branch.pattern, Some(matchGuard), branch.expr)(using
              branch.patternRegion
            )
          case None             =>
            branch
        }
      case _ =>
        branch
    }

  private case class GroupedLambda[A](
      groups: Vector[NonEmptyList[(Bindable, Type)]],
      terminalBody: TypedExpr[A],
      rebuild: TypedExpr[A] => TypedExpr[A]
  )

  // Collect nested lambda groups from a recursive binding expression.
  // We allow Generic/Annotation wrappers and non-recursive Let wrappers
  // between groups, e.g. fn -> (x = 2; (a, b) -> ...).
  private def collectGroupedLambdas[A](
      expr: TypedExpr[A]
  ): Option[GroupedLambda[A]] = {
    // `rebuild` only replays the original wrapper/binder path and swaps the
    // already-scoped terminal body expression, so scope and type shadowing are
    // preserved by construction.
    def scanBody(
        te: TypedExpr[A],
        groups: Vector[Group],
        rebuild: TypedExpr[A] => TypedExpr[A]
    ): Option[GroupedLambda[A]] =
      te match {
        case Generic(q, in) =>
          scanBody(in, groups, in1 => rebuild(Generic(q, in1)))
        case Annotation(in, tpe, qev) =>
          scanBody(in, groups, in1 => rebuild(Annotation(in1, tpe, qev)))
        case Let(arg, ex, in, RecursionKind.NonRecursive, tag) =>
          scanBody(
            in,
            groups,
            in1 => rebuild(Let(arg, ex, in1, RecursionKind.NonRecursive, tag))
          )
        case _ =>
          scan(te, groups, rebuild)
      }

    def scan(
        te: TypedExpr[A],
        groups: Vector[Group],
        rebuild: TypedExpr[A] => TypedExpr[A]
    ): Option[GroupedLambda[A]] =
      te match {
        case Generic(q, in) =>
          scan(in, groups, in1 => rebuild(Generic(q, in1)))
        case Annotation(in, tpe, qev) =>
          scan(in, groups, in1 => rebuild(Annotation(in1, tpe, qev)))
        case AnnotatedLambda(args, body, tag) =>
          scanBody(
            body,
            groups :+ args,
            body1 => rebuild(AnnotatedLambda(args, body1, tag))
          )
        case _ =>
          if (groups.isEmpty) None
          else Some(GroupedLambda(groups, te, rebuild))
      }

    scan(expr, Vector.empty, identity)
  }

  private case class RewriteResult[A](
      expr: TypedExpr[A],
      changed: Boolean,
      sawSelfRef: Boolean
  )

  private def rewriteGroupedSelfCalls[A](
      te: TypedExpr[A],
      fnName: Bindable,
      helperName: Bindable,
      helperFnType: Type,
      groupedArities: Vector[Int],
      canRecur: Boolean
  ): Option[RewriteResult[A]] = {

    def isSelfHead(head: TypedExpr[A], canRecurHere: Boolean): Boolean =
      canRecurHere && (
        stripTypeWrappers(head) match {
          case Local(n, _, _) => n == fnName
          case _              => false
        }
      )

    @annotation.tailrec
    def unapplyApp(
        curr: TypedExpr[A],
        acc: List[NonEmptyList[TypedExpr[A]]]
    ): (TypedExpr[A], List[NonEmptyList[TypedExpr[A]]]) =
      curr match {
        case App(fn, args, _, _) =>
          unapplyApp(fn, args :: acc)
        case other =>
          (other, acc)
      }

    def validGroupedArity[T](groups: List[NonEmptyList[T]]): Boolean =
      (groups.length == groupedArities.length) &&
        groups.iterator
          .zip(groupedArities.iterator)
          .forall { case (args, expectedArity) =>
            args.length == expectedArity
          }

    def localNameOf(te: TypedExpr[A]): Option[Bindable] =
      stripTypeWrappers(te) match {
        case Local(n, _, _) => Some(n)
        case _              => None
      }

    def etaExpandedGroups(
        head: TypedExpr[A],
        outerGroups: List[NonEmptyList[TypedExpr[A]]],
        canRecurHere: Boolean
    ): Option[List[NonEmptyList[TypedExpr[A]]]] =
      // Reader-only shape check: wrappers may appear from local ascriptions.
      // We do not rebuild from stripped nodes. Also, loop/recur lowering is
      // gated to monomorphic recursion so recursive polymorphism is skipped.
      stripTypeWrappers(head) match {
        case AnnotatedLambda(lambdaArgs, lambdaBody, _) =>
          outerGroups match {
            case applied :: rest if applied.length == lambdaArgs.length =>
              val (innerHead, innerGroups) =
                unapplyApp(stripTypeWrappers(lambdaBody), Nil)
              if (
                innerGroups.isEmpty || !isSelfHead(innerHead, canRecurHere)
              ) None
              else {
                val lastGroup = innerGroups.last
                val lambdaArgNames = lambdaArgs.toList.map(_._1)
                val lastGroupNames = lastGroup.toList.map(localNameOf)
                if (
                  (lastGroup.length == lambdaArgNames.length) &&
                  lastGroupNames.forall(_.nonEmpty) &&
                  (lastGroupNames.flatten == lambdaArgNames)
                ) Some(innerGroups.init ++ (applied :: rest))
                else None
              }
            case _ =>
              None
          }
        case _ =>
          None
      }

    def rewriteToHelper(
        groups: NonEmptyList[NonEmptyList[TypedExpr[A]]],
        tpe: Type,
        tag: A,
        canRecurHere: Boolean
    ): Option[RewriteResult[A]] =
      groups
        .traverse { group =>
          group.traverse(recur(_, canRecurHere))
        }
        .map { group1 =>
          val flatArgs = group1.tail.foldLeft(group1.head.map(_.expr)) {
            (acc, group) =>
              acc.concatNel(group.map(_.expr))
          }
          val app1 = App(
            Local(helperName, helperFnType, tag),
            flatArgs,
            tpe,
            tag
          )
          // A successful helper rewrite always changes shape and always
          // corresponds to a recursive self-call.
          RewriteResult(app1, changed = true, sawSelfRef = true)
        }

    def recur(
        expr: TypedExpr[A],
        canRecur: Boolean
    ): Option[RewriteResult[A]] =
      expr match {
        case g @ Generic(q, in) =>
          recur(in, canRecur).map { in1 =>
            val expr1 =
              if (in1.changed) Generic(q, in1.expr)
              else g
            RewriteResult(expr1, in1.changed, in1.sawSelfRef)
          }
        case a @ Annotation(in, tpe, qev) =>
          recur(in, canRecur).map { in1 =>
            val expr1 =
              if (in1.changed) Annotation(in1.expr, tpe, qev)
              else a
            RewriteResult(expr1, in1.changed, in1.sawSelfRef)
          }
        case lam @ AnnotatedLambda(args, body, tag) =>
          val canRecurBody = canRecur && !args.exists(_._1 == fnName)
          recur(body, canRecurBody).map { body1 =>
            val expr1 =
              if (body1.changed) AnnotatedLambda(args, body1.expr, tag)
              else lam
            RewriteResult(expr1, changed = body1.changed, body1.sawSelfRef)
          }
        case app @ App(fn, args, tpe, tag) =>
          val (head, groups) = unapplyApp(app, Nil)
          if (isSelfHead(head, canRecur)) {
            if (!validGroupedArity(groups)) None
            else
              NonEmptyList
                .fromList(groups)
                .flatMap(rewriteToHelper(_, tpe, tag, canRecur))
          } else {
            // Non-self heads can still encode grouped recursion when they are
            // eta-expanded lambdas whose body calls `fnName`.
            etaExpandedGroups(head, groups, canRecur) match {
              case Some(expandedGroups) =>
                if (!validGroupedArity(expandedGroups)) None
                else
                  NonEmptyList
                    .fromList(expandedGroups)
                    .flatMap(rewriteToHelper(_, tpe, tag, canRecur))
              case None =>
                map2(
                  recur(fn, canRecur),
                  args.traverse(recur(_, canRecur))
                ) { case (fn1, args1) =>
                  val changed = fn1.changed || args1.exists(_.changed)
                  val sawSelf = fn1.sawSelfRef || args1.exists(_.sawSelfRef)
                  val expr1 =
                    if (changed) App(fn1.expr, args1.map(_.expr), tpe, tag)
                    else app
                  RewriteResult(expr1, changed, sawSelf)
                }
            }
          }
        case let @ Let(arg, ex, in, rec, tag) =>
          val (exCanRecur, inCanRecur) =
            if (arg == fnName) {
              // `in` is under the new binder, so outer `fnName` is shadowed.
              // For recursive lets, `ex` is also scoped to the inner binder.
              if (rec.isRecursive) (false, false)
              else (canRecur, false)
            } else (canRecur, canRecur)
          map2(
            recur(ex, exCanRecur),
            recur(in, inCanRecur)
          ) { (ex1, in1) =>
            val changed = ex1.changed || in1.changed
            val sawSelf = ex1.sawSelfRef || in1.sawSelfRef
            val expr1 =
              if (changed) Let(arg, ex1.expr, in1.expr, rec, tag)
              else let
            RewriteResult(expr1, changed, sawSelf)
          }
        case m @ Match(arg, branches, tag) =>
          def rewriteGuard(
              branch: TypedExpr.Branch[A],
              canRecurOuter: Boolean,
              canRecurInner: Boolean
          ): Option[(Option[TypedExpr.BranchGuard[A]], Boolean, Boolean)] =
            branch.guardNode match {
              case Some(TypedExpr.BoolGuard(guardExpr)) =>
                recur(guardExpr, canRecurOuter).map { guard1 =>
                  val changed = guard1.changed
                  val guardNode1 =
                    if (changed) Some(TypedExpr.BoolGuard(guard1.expr))
                    else branch.guardNode
                  (guardNode1, changed, guard1.sawSelfRef)
                }
              case Some(guard @ TypedExpr.MatchGuard(argExpr, pattern, guardOpt)) =>
                map2(
                  recur(argExpr, canRecurOuter),
                  guardOpt.traverse(recur(_, canRecurInner))
                ) { (arg1, innerGuard1) =>
                  val changed = arg1.changed || innerGuard1.exists(_.changed)
                  val sawSelf = arg1.sawSelfRef || innerGuard1.exists(_.sawSelfRef)
                  val guardNode1 =
                    if (!changed) branch.guardNode
                    else
                      Some(
                        TypedExpr.MatchGuard(
                          arg1.expr,
                          pattern,
                          innerGuard1.map(_.expr)
                        )(using guard.patternRegion)
                      )
                  (guardNode1, changed, sawSelf)
                }
              case None =>
                Some((None, false, false))
            }

          map2(
            recur(arg, canRecur),
            branches.traverse { branch =>
              val canRecurOuter =
                canRecur && !branch.pattern.names.contains(fnName)
              val canRecurInner =
                canRecurOuter && !branch.guardBindings.contains(fnName)
              map2(
                rewriteGuard(branch, canRecurOuter, canRecurInner),
                recur(branch.expr, canRecurInner)
              ) { (guard1, expr1) =>
                val (guardNode1, guardChanged, guardSawSelf) = guard1
                val changed = guardChanged || expr1.changed
                val sawSelf = guardSawSelf || expr1.sawSelfRef
                val branch1 =
                  if (changed) {
                    branch.copyNode(branch.pattern, guardNode1, expr1.expr)(using
                      branch.patternRegion
                    )
                  } else branch
                (branch1, changed, sawSelf)
              }
            }
          ) { (arg1, branches1) =>
            val changed = arg1.changed || branches1.exists(_._2)
            val sawSelf = arg1.sawSelfRef || branches1.exists(_._3)
            RewriteResult(
              Match(m.matchKind, arg1.expr, branches1.map(_._1), tag),
              changed,
              sawSelf
            )
          }
        case Loop(_, _, _) | Recur(_, _, _) =>
          // Conservative fallback: these should not appear in pre-lowered
          // recursive definition bodies. If they do, skip this rewrite.
          None
        case Local(n, _, _) if canRecur && (n == fnName) =>
          // Bare self-reference escapes the supported full-application shape.
          None
        case n @ (Local(_, _, _) | Global(_, _, _, _) | Literal(_, _, _)) =>
          Some(RewriteResult(n, changed = false, sawSelfRef = false))
      }

    recur(te, canRecur)
  }

  private def isMonomorphicRecursiveBinding[A](
      fnName: Bindable,
      expr: TypedExpr[A]
  ): Boolean = {
    val expectedFnType = stripTypeWrappers(expr).getType
    val expectedFnTypeVars =
      Type.freeBoundTyVars(expectedFnType :: Nil).toSet

    def isSelfHead(head: TypedExpr[A], canRecurHere: Boolean): Boolean =
      canRecurHere && (
        stripTypeWrappers(head) match {
          case Local(n, _, _) => n == fnName
          case _              => false
        }
      )

    def loop(
        te: TypedExpr[A],
        canRecur: Boolean,
        expectedVarsInScope: Set[Type.Var.Bound],
        expectedVarsShadowed: Boolean
    ): Boolean =
      te match {
        case Generic(q, in) =>
          val qVars = q.vars.iterator.map(_._1).toSet
          // Track where expected function-type vars are bound, and detect when
          // a nested Generic reuses one (shadowing). Self-call checks under
          // that shadowed scope are rejected below.
          val expectedVarsShadowed1 =
            expectedVarsShadowed || qVars.exists(expectedVarsInScope)
          val expectedVarsInScope1 =
            expectedVarsInScope ++ qVars.intersect(expectedFnTypeVars)
          loop(in, canRecur, expectedVarsInScope1, expectedVarsShadowed1)
        case Annotation(in, _, _) =>
          loop(in, canRecur, expectedVarsInScope, expectedVarsShadowed)
        case AnnotatedLambda(args, body, _) =>
          // Lambda binders only affect term-variable recursion visibility.
          // Type-level stability is checked on self-call heads at App sites.
          val canRecurBody = canRecur && !args.exists(_._1 == fnName)
          loop(body, canRecurBody, expectedVarsInScope, expectedVarsShadowed)
        case App(fn, args, _, _) =>
          val selfHeadTypeStable =
            if (isSelfHead(fn, canRecur))
              !expectedVarsShadowed && fn.getType.sameAs(expectedFnType)
            else true
          selfHeadTypeStable &&
            loop(fn, canRecur, expectedVarsInScope, expectedVarsShadowed) &&
            args.forall(
              loop(_, canRecur, expectedVarsInScope, expectedVarsShadowed)
            )
        case Let(arg, ex, in, rec, _) =>
          if (arg == fnName) {
            if (rec.isRecursive)
              loop(ex, canRecur = false, expectedVarsInScope, expectedVarsShadowed) && loop(
              in,
              canRecur = false,
              expectedVarsInScope,
              expectedVarsShadowed
            )
            else
              loop(ex, canRecur, expectedVarsInScope, expectedVarsShadowed) && loop(
                in,
                canRecur = false,
                expectedVarsInScope,
                expectedVarsShadowed
              )
          } else {
            loop(ex, canRecur, expectedVarsInScope, expectedVarsShadowed) &&
            loop(in, canRecur, expectedVarsInScope, expectedVarsShadowed)
          }
        case Loop(args, body, _) =>
          val argsStable = args.forall { case (_, initExpr) =>
            loop(initExpr, canRecur, expectedVarsInScope, expectedVarsShadowed)
          }
          val canRecurBody = canRecur && !args.exists(_._1 == fnName)
          argsStable && loop(
            body,
            canRecurBody,
            expectedVarsInScope,
            expectedVarsShadowed
          )
        case Recur(args, _, _) =>
          args.forall(loop(_, canRecur, expectedVarsInScope, expectedVarsShadowed))
        case Match(arg, branches, _) =>
          loop(arg, canRecur, expectedVarsInScope, expectedVarsShadowed) &&
            branches.forall { branch =>
              val canRecurOuter =
                canRecur && !branch.pattern.names.contains(fnName)
              val canRecurInner =
                canRecurOuter && !branch.guardBindings.contains(fnName)
              val guardValid =
                branch.guardNode match {
                  case Some(TypedExpr.BoolGuard(guardExpr)) =>
                    loop(
                      guardExpr,
                      canRecurOuter,
                      expectedVarsInScope,
                      expectedVarsShadowed
                    )
                  case Some(TypedExpr.MatchGuard(argExpr, _, guardOpt)) =>
                    loop(
                      argExpr,
                      canRecurOuter,
                      expectedVarsInScope,
                      expectedVarsShadowed
                    ) &&
                      guardOpt.forall(
                        loop(
                          _,
                          canRecurInner,
                          expectedVarsInScope,
                          expectedVarsShadowed
                        )
                      )
                  case None =>
                    true
                }
              guardValid &&
              loop(
                branch.expr,
                canRecurInner,
                expectedVarsInScope,
                expectedVarsShadowed
              )
            }
        case Local(_, _, _) | Global(_, _, _, _) | Literal(_, _, _) =>
          true
      }

    loop(
      expr,
      canRecur = true,
      expectedVarsInScope = Set.empty,
      expectedVarsShadowed = false
    )
  }

  private case class MonomorphicRec[A](name: Bindable, expr: TypedExpr[A])

  private def toMonomorphicRec[A](
      name: Bindable,
      expr: TypedExpr[A]
  ): Option[MonomorphicRec[A]] =
    if (isMonomorphicRecursiveBinding(name, expr))
      Some(MonomorphicRec(name, expr))
    else None

  // TypedExpr loop/recur lowering is currently restricted to monomorphic
  // recursion so loop argument types remain stable in the typed AST. Generic
  // wrappers are still allowed when recursive calls preserve the same
  // instantiated self-call head type.
  // Polymorphic recursion can be optimized later after type erasure:
  // https://github.com/johnynek/bosatsu/issues/1749
  // Phase 1: grouped self-call flattening. This rewrites grouped recursive
  // calls (including non-tail ones) through a helper to avoid closure
  // allocations on recursive steps.
  private def flattenGroupedSelfCalls[A](
      mono: MonomorphicRec[A]
  ): Option[TypedExpr[A]] =
    collectGroupedLambdas(mono.expr).flatMap { grouped =>
      if (grouped.groups.lengthCompare(2) < 0) {
        // Only one group: there is nothing to flatten.
        None
      } else {
        // We already checked that groups has 2+ items so this is safe.
        val groupsNel = NonEmptyList.fromListUnsafe(grouped.groups.toList)
        val groupedArgs = groupsNel.flatMap(identity)
        val groupedArgNames = groupedArgs.map(_._1).toList
        if (groupedArgNames.toSet.size != groupedArgNames.size) None
        else {
          val used = scala.collection.mutable.HashSet.empty[Bindable] ++
            TypedExpr.allVarsSet(mono.expr :: Nil).toSet
          used.add(mono.name): Unit
          val names = Identifier.Bindable.syntheticIterator

          def freshName(): Bindable = {
            var n = names.next()
            while (used(n)) n = names.next()
            used.add(n): Unit
            n
          }

          val helperName = freshName()
          val helperArgs = groupedArgs.map { case (_, tpe) =>
            (freshName(), tpe)
          }
          val helperArgPairs = groupedArgs.zip(helperArgs)
          val renameMap = helperArgPairs.iterator.map {
            case ((oldName, _), (newName, _)) =>
              oldName -> { (loc: Local[A]) =>
                Local(newName, loc.tpe, loc.tag): TypedExpr[A]
              }
          }.toMap

          val substitutedBody =
            TypedExpr.substituteAll(
              renameMap,
              grouped.terminalBody,
              enterLambda = true
            ).get

          val helperFnType = Type.Fun(
            helperArgs.map(_._2),
            substitutedBody.getType
          )
          val groupedArities = grouped.groups.map(_.length)

          rewriteGroupedSelfCalls(
            substitutedBody,
            mono.name,
            helperName,
            helperFnType,
            groupedArities,
            canRecur = true
          ).flatMap { rewritten =>
            if (!rewritten.sawSelfRef) None
            else {
              val helperDef = AnnotatedLambda(
                helperArgs,
                rewritten.expr,
                grouped.terminalBody.tag
              )
              val initArgs = groupedArgs.map { case (n, tpe) =>
                Local(n, tpe, grouped.terminalBody.tag): TypedExpr[A]
              }
              val helperCall = App(
                Local(helperName, helperFnType, grouped.terminalBody.tag),
                initArgs,
                grouped.terminalBody.getType,
                grouped.terminalBody.tag
              )
              val terminalBody1 = Let(
                helperName,
                helperDef,
                helperCall,
                RecursionKind.Recursive,
                grouped.terminalBody.tag
              )
              Some(grouped.rebuild(terminalBody1))
            }
          }
        }
      }
    }

  private def lowerRecursiveBinding[A](
      mono: MonomorphicRec[A]
  ): Option[TypedExpr[A]] = {
    val name = mono.name
    val te = mono.expr
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
            val fresh = Identifier.Bindable.freshSyntheticIterator(avoid)
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
          if (rec.isRecursive) {
            val mono0 = toMonomorphicRec(arg, expr1)
            // Phase 1: flatten grouped recursive self calls.
            val expr1a = mono0.flatMap(flattenGroupedSelfCalls(_)).getOrElse(expr1)
            val expr1b =
              if (expr1a eq expr1) expr1
              else lowerExpr(expr1a)
            // Phase 2: lower tail recursion to Loop/Recur.
            toMonomorphicRec(arg, expr1b)
              .flatMap(lowerRecursiveBinding(_))
              .getOrElse(expr1b)
          }
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
        val branches0 =
          recursiveGuardRecoveryProvenance(tag, branches.length) match {
            case Some(provenance) =>
              val restored =
                branches.toList
                  .zip(provenance.iterator)
                  .map { case (branch, allowRecovery) =>
                    restoreRecursiveMatchGuard(branch, allowRecovery)
                  }
              NonEmptyList.fromListUnsafe(restored)
            case None =>
              branches
          }
        val arg1 = lowerExpr(arg)
        val branches1 = ListUtil.mapConserveNel(branches0) { branch =>
          val guard1 = branch.mapGuardNodeExpr(lowerExpr(_))
          val expr1 = lowerExpr(branch.expr)
          if (guard1.eq(branch.guardNode) && (expr1 eq branch.expr)) branch
          else branch.copyNode(branch.pattern, guard1, expr1)(using
            branch.patternRegion
          )
        }
        if ((arg1 eq arg) && (branches1 eq branches)) m
        else Match(m.matchKind, arg1, branches1, tag)
      case n @ (Local(_, _, _) | Global(_, _, _, _) | Literal(_, _, _)) =>
        n
    }

  def lowerAll[A](
      lets: List[(Bindable, RecursionKind, TypedExpr[A])]
  ): List[(Bindable, RecursionKind, TypedExpr[A])] =
    lets.map { case (n, rec, te) =>
      val lowered = lowerExpr(te)
      val loweredRec =
        if (rec.isRecursive) {
          val mono0 = toMonomorphicRec(n, lowered)
          val lowered1 = mono0.flatMap(flattenGroupedSelfCalls(_)).getOrElse(lowered)
          val lowered2 =
            if (lowered1 eq lowered) lowered
            else lowerExpr(lowered1)
          toMonomorphicRec(n, lowered2)
            .flatMap(lowerRecursiveBinding(_))
            .getOrElse(lowered2)
        }
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
