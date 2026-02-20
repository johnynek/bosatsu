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
      ob: Option[B]
  )(inline fn: (A, B) => C): Option[C] =
    if (oa.isDefined && ob.isDefined) Some(fn(oa.get, ob.get))
    else None

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

  // Reader-only helper: this inspects shape and must never be used to rebuild
  // expressions, since it drops type wrappers.
  @annotation.tailrec
  private def stripTypeWrappers[A](te: TypedExpr[A]): TypedExpr[A] =
    te match {
      case Generic(_, in)    => stripTypeWrappers(in)
      case Annotation(in, _, _) => stripTypeWrappers(in)
      case other             => other
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
      tailPos: Boolean,
      canRecur: Boolean
  ): Option[RewriteResult[A]] = {

    def isSelfHead(head: TypedExpr[A], canRecurHere: Boolean): Boolean =
      stripTypeWrappers(head) match {
        case Local(n, _, _) => canRecurHere && (n == fnName)
        case _              => false
      }

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
                !isSelfHead(innerHead, canRecurHere) || innerGroups.isEmpty
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
        groups: List[NonEmptyList[TypedExpr[A]]],
        tpe: Type,
        tag: A,
        canRecurHere: Boolean
    ): Option[RewriteResult[A]] =
      groups
        .traverse { group =>
          group.traverse(recur(_, tailPos = false, canRecurHere))
        }
        .map { group1 =>
          val flatArgs =
            group1 match {
              case h :: t =>
                t.foldLeft(h.map(_.expr)) { (acc, group) =>
                  acc.concatNel(group.map(_.expr))
                }
              case Nil =>
                throw new IllegalStateException(
                  "rewriteToHelper requires at least one argument group"
                )
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
        tailPos: Boolean,
        canRecur: Boolean
    ): Option[RewriteResult[A]] =
      expr match {
        case g @ Generic(q, in) =>
          recur(in, tailPos, canRecur).map { in1 =>
            val expr1 =
              if (in1.changed) Generic(q, in1.expr)
              else g
            RewriteResult(expr1, in1.changed, in1.sawSelfRef)
          }
        case a @ Annotation(in, tpe, qev) =>
          recur(in, tailPos, canRecur).map { in1 =>
            val expr1 =
              if (in1.changed) Annotation(in1.expr, tpe, qev)
              else a
            RewriteResult(expr1, in1.changed, in1.sawSelfRef)
          }
        case lam @ AnnotatedLambda(args, body, tag) =>
          val canRecurBody = canRecur && !args.exists(_._1 == fnName)
          recur(body, tailPos = false, canRecurBody).map { body1 =>
            // Nested lambda bodies are always visited with tailPos = false, so
            // successful traversal cannot rewrite them to helper calls.
            assert(!body1.changed)
            RewriteResult(lam, changed = false, body1.sawSelfRef)
          }
        case app @ App(fn, args, tpe, tag) =>
          val (head, groups) = unapplyApp(app, Nil)
          if (isSelfHead(head, canRecur)) {
            if (!validGroupedArity(groups) || !tailPos) None
            else rewriteToHelper(groups, tpe, tag, canRecur)
          } else {
            // Non-self heads can still encode grouped recursion when they are
            // eta-expanded lambdas whose body calls `fnName`.
            etaExpandedGroups(head, groups, canRecur) match {
              case Some(expandedGroups) =>
                if (!validGroupedArity(expandedGroups) || !tailPos) None
                else rewriteToHelper(expandedGroups, tpe, tag, canRecur)
              case None =>
                map2(
                  recur(fn, tailPos = false, canRecur),
                  args.traverse(recur(_, tailPos = false, canRecur))
                ) { case (fn1, args1) =>
                  val sawSelf = fn1.sawSelfRef || args1.exists(_.sawSelfRef)
                  RewriteResult(app, changed = false, sawSelf)
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
            recur(ex, tailPos = false, exCanRecur),
            recur(in, tailPos, inCanRecur)
          ) { (ex1, in1) =>
            val changed = ex1.changed || in1.changed
            val sawSelf = ex1.sawSelfRef || in1.sawSelfRef
            val expr1 =
              if (changed) Let(arg, ex1.expr, in1.expr, rec, tag)
              else let
            RewriteResult(expr1, changed, sawSelf)
          }
        case Match(arg, branches, tag) =>
          map2(
            recur(arg, tailPos = false, canRecur),
            branches.traverse { branch =>
              val canRecurBranch =
                canRecur && !branch.pattern.names.contains(fnName)
              map2(
                branch.guard.traverse(recur(_, tailPos = false, canRecurBranch)),
                recur(branch.expr, tailPos, canRecurBranch)
              ) { (guard1, expr1) =>
                val changed =
                  guard1.exists(_.changed) || expr1.changed
                val sawSelf =
                  guard1.exists(_.sawSelfRef) || expr1.sawSelfRef
                val branch1 =
                  if (changed) {
                    branch.copy(
                      guard = guard1.map(_.expr),
                      expr = expr1.expr
                    )
                  } else branch
                (branch1, changed, sawSelf)
              }
            }
          ) { (arg1, branches1) =>
            val changed = arg1.changed || branches1.exists(_._2)
            val sawSelf = arg1.sawSelfRef || branches1.exists(_._3)
            RewriteResult(Match(arg1.expr, branches1.map(_._1), tag), changed, sawSelf)
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

    recur(te, tailPos, canRecur)
  }

  private def isMonomorphicRecursiveBinding[A](
      fnName: Bindable,
      expr: TypedExpr[A]
  ): Boolean = {
    val expectedFnType = stripTypeWrappers(expr).getType

    def loop(te: TypedExpr[A], canRecur: Boolean): Boolean =
      te match {
        case Generic(_, in) =>
          loop(in, canRecur)
        case Annotation(in, _, _) =>
          loop(in, canRecur)
        case AnnotatedLambda(args, body, _) =>
          val canRecurBody = canRecur && !args.exists(_._1 == fnName)
          loop(body, canRecurBody)
        case App(fn, args, _, _) =>
          val selfHeadTypeStable =
            stripTypeWrappers(fn) match {
              case Local(n, _, _) if canRecur && (n == fnName) =>
                fn.getType.sameAs(expectedFnType)
              case _ =>
                true
            }
          selfHeadTypeStable &&
            loop(fn, canRecur) &&
            args.forall(loop(_, canRecur))
        case Let(arg, ex, in, rec, _) =>
          if (arg == fnName) {
            if (rec.isRecursive) loop(ex, canRecur = false) && loop(
              in,
              canRecur = false
            )
            else loop(ex, canRecur) && loop(in, canRecur = false)
          } else {
            loop(ex, canRecur) && loop(in, canRecur)
          }
        case Loop(args, body, _) =>
          val argsStable = args.forall { case (_, initExpr) =>
            loop(initExpr, canRecur)
          }
          val canRecurBody = canRecur && !args.exists(_._1 == fnName)
          argsStable && loop(body, canRecurBody)
        case Recur(args, _, _) =>
          args.forall(loop(_, canRecur))
        case Match(arg, branches, _) =>
          loop(arg, canRecur) &&
            branches.forall { branch =>
              val canRecurBranch =
                canRecur && !branch.pattern.names.contains(fnName)
              branch.guard.forall(loop(_, canRecurBranch)) &&
              loop(branch.expr, canRecurBranch)
            }
        case Local(_, _, _) | Global(_, _, _, _) | Literal(_, _, _) =>
          true
      }

    loop(expr, canRecur = true)
  }

  // TypedExpr loop/recur lowering is currently restricted to monomorphic
  // recursion so loop argument types remain stable in the typed AST. Generic
  // wrappers are still allowed when recursive calls preserve the same
  // instantiated self-call head type.
  // Polymorphic recursion can be optimized later after type erasure:
  // https://github.com/johnynek/bosatsu/issues/1749
  // This is the grouped-call optimization pass: rewrite multi-group tail
  // recursive calls through a helper so downstream loop/recur lowering can
  // avoid allocating closures on each iteration.
  private def rewriteMultiGroupTailRec[A](
      fnName: Bindable,
      expr: TypedExpr[A]
  ): Option[TypedExpr[A]] =
    if (!isMonomorphicRecursiveBinding(fnName, expr)) None
    else {
      collectGroupedLambdas(expr).flatMap { grouped =>
        if (grouped.groups.lengthCompare(2) < 0) None
        else {
          val groupedArgs = grouped.groups.toList.flatMap(_.toList)
          val groupedArgNames = groupedArgs.map(_._1)
          if (groupedArgNames.toSet.size != groupedArgNames.size) None
          else {
            val used = scala.collection.mutable.HashSet.empty[Bindable] ++
              TypedExpr.allVarsSet(expr :: Nil).toSet
            used.add(fnName): Unit
            val names = Expr.nameIterator()

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
              NonEmptyList.fromListUnsafe(helperArgs.map(_._2)),
              substitutedBody.getType
            )
            val groupedArities = grouped.groups.iterator.map(_.length).toVector

            rewriteGroupedSelfCalls(
              substitutedBody,
              fnName,
              helperName,
              helperFnType,
              groupedArities,
              tailPos = true,
              canRecur = true
            ).flatMap { rewritten =>
              if (!rewritten.sawSelfRef) None
              else {
                val helperDef = AnnotatedLambda(
                  NonEmptyList.fromListUnsafe(helperArgs),
                  rewritten.expr,
                  grouped.terminalBody.tag
                )
                val initArgs = groupedArgs.map { case (n, tpe) =>
                  Local(n, tpe, grouped.terminalBody.tag): TypedExpr[A]
                }
                val helperCall = App(
                  Local(helperName, helperFnType, grouped.terminalBody.tag),
                  NonEmptyList.fromListUnsafe(initArgs),
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
    }

  private def lowerRecursiveBinding[A](
      name: Bindable,
      te: TypedExpr[A]
  ): Option[TypedExpr[A]] =
    if (!isMonomorphicRecursiveBinding(name, te)) None
    else {
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
          if (rec.isRecursive && isMonomorphicRecursiveBinding(arg, expr1)) {
            val expr1a = rewriteMultiGroupTailRec(arg, expr1).getOrElse(expr1)
            val expr1b =
              if (expr1a eq expr1) expr1
              else lowerExpr(expr1a)
            lowerRecursiveBinding(arg, expr1b).getOrElse(expr1b)
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
        if (rec.isRecursive && isMonomorphicRecursiveBinding(n, lowered)) {
          val lowered1 = rewriteMultiGroupTailRec(n, lowered).getOrElse(lowered)
          val lowered2 =
            if (lowered1 eq lowered) lowered
            else lowerExpr(lowered1)
          lowerRecursiveBinding(n, lowered2).getOrElse(lowered2)
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
