package dev.bosatsu

import cats.Order
import cats.data.NonEmptyList
import cats.implicits._
import dev.bosatsu.Identifier.Bindable
import dev.bosatsu.Matchless.{Expr, Global, Lambda, ParamDemand}
import dev.bosatsu.graph.Toposort

import scala.collection.immutable.SortedMap

object MatchlessGlobalInlining {
  private final case class SummaryKey[K](
      scope: K,
      pack: PackageName,
      name: Bindable
  )

  sealed trait InlineSummary[A] {
    def bodyWeight: Int
    def containsWhileExpr: Boolean
    def exposesFreeMutableAnon: Boolean
  }
  final case class LambdaSummary[A](
      lambda: Lambda[A],
      bodyWeight: Int,
      paramDemand: Vector[ParamDemand],
      containsWhileExpr: Boolean,
      exposesFreeMutableAnon: Boolean,
      exposesBranching: Boolean,
      exposesFollowOnReduction: Boolean
  ) extends InlineSummary[A] {
    def arity: Int = lambda.arity
  }
  final case class ValueSummary[A](
      value: Expr[A],
      bodyWeight: Int,
      containsWhileExpr: Boolean,
      exposesFreeMutableAnon: Boolean
  ) extends InlineSummary[A]

  private val InlineBodyWeightBudget = InlineBenefitModel.MaxCallWeightBudget
  private val TinyBodyWeightBudget = InlineBenefitModel.TinyCallWeightBudget
  private val CheapArgWeightBudget = 3
  private val MaxExpensiveArgUses = 1

  private def cleanedCompiled[K: Order](
      rawCompiled: SortedMap[K, MatchlessFromTypedExpr.Compiled[K]],
      localPassOptions: Matchless.LocalPassOptions
  )(implicit ec: Par.EC): SortedMap[K, MatchlessFromTypedExpr.Compiled[K]] = {
    val scopeTasks = rawCompiled.iterator.toList.map { case (scope, packs) =>
      Par.start {
        val packTasks = packs.toList.map { case (pack, lets) =>
          Par.start {
            pack -> lets.map { case (name, expr) =>
              (name, Matchless.postLoweringCleanup(expr, localPassOptions))
            }
          }
        }
        scope -> packTasks.map(Par.await).toMap
      }
    }

    SortedMap.from(scopeTasks.map(Par.await))(using rawCompiled.ordering)
  }

  def optimize[K: Order](
      rawCompiled: SortedMap[K, MatchlessFromTypedExpr.Compiled[K]],
      topoSort: Toposort.Result[(K, PackageName)],
      depFor: (K, PackageName) => K,
      localPassOptions: Matchless.LocalPassOptions
  )(implicit ec: Par.EC): SortedMap[K, MatchlessFromTypedExpr.Compiled[K]] = {
    val cleanedBase = cleanedCompiled(rawCompiled, localPassOptions)

    val layers =
      topoSort.layers ++ topoSort.loopNodes
        .sorted(using Order[(K, PackageName)].toOrdering)
        .map(NonEmptyList.one)

    layers.foldLeft((cleanedBase, Map.empty[SummaryKey[K], InlineSummary[K]])) {
      case ((compiledAcc, summaryAcc), layer) =>
        val tasks = layer.toList.map { case (scope, pack) =>
          Par.start {
            val lets =
              cleanedBase
                .get(scope)
                .flatMap(_.get(pack))
                .getOrElse(
                  sys.error(
                    s"missing cleaned Matchless package $pack in scope $scope during global inlining"
                  )
                )
            rewritePackage(
              scope,
              pack,
              lets,
              summaryAcc,
              depFor,
              localPassOptions
            )
          }
        }

        val layerResults = tasks.map(Par.await)

        val nextCompiled =
          layerResults.foldLeft(compiledAcc) {
            case (acc, (scope, pack, lets, _)) =>
              val scopeMap = acc.getOrElse(scope, Map.empty)
              acc.updated(scope, scopeMap.updated(pack, lets))
          }

        val nextSummaries =
          layerResults.foldLeft(summaryAcc) { case (acc, (_, _, _, published)) =>
            acc ++ published
          }

        (nextCompiled, nextSummaries)
    }._1
  }

  private def rewritePackage[K: Order](
      scope: K,
      pack: PackageName,
      cleanedLets: List[(Bindable, Expr[K])],
      priorSummaries: Map[SummaryKey[K], InlineSummary[K]],
      depFor: (K, PackageName) => K,
      localPassOptions: Matchless.LocalPassOptions
  ): (K, PackageName, List[(Bindable, Expr[K])], Map[SummaryKey[K], InlineSummary[K]]) = {
    val byName = cleanedLets.toMap
    val localOrder = localDependencyOrder(scope, pack, cleanedLets, depFor)

    val (rewrittenByName, published) =
      localOrder.foldLeft((Map.empty[Bindable, Expr[K]], priorSummaries)) {
        case ((rewritten, available), name) =>
          val expr0 =
            byName.getOrElse(
              name,
              sys.error(
                s"missing let $name in cleaned Matchless package $pack for scope $scope"
              )
            )
          val rewrittenExpr =
            Matchless.refreshAnonBinders(
              Matchless.postLoweringCleanup(
                rewriteExpr(expr0, available, depFor),
                localPassOptions
              )
            )
          val available1 =
            summarize(rewrittenExpr).fold(available) { summary =>
              available.updated(SummaryKey(scope, pack, name), summary)
            }
          (rewritten.updated(name, rewrittenExpr), available1)
      }

    val publishedHere = published -- priorSummaries.keySet
    val finalLets = cleanedLets.map { case (name, expr) =>
      (name, rewrittenByName.getOrElse(name, expr))
    }

    (scope, pack, finalLets, publishedHere)
  }

  private def localDependencyOrder[K: Order](
      scope: K,
      pack: PackageName,
      lets: List[(Bindable, Expr[K])],
      depFor: (K, PackageName) => K
  ): List[Bindable] = {
    case class LocalNode(index: Int, name: Bindable)
    given Ordering[LocalNode] = Ordering.by((node: LocalNode) =>
      (node.index, node.name)
    )

    val nodeByName = lets.zipWithIndex.iterator.map { case ((name, _), idx) =>
      name -> LocalNode(idx, name)
    }.toMap
    val exprByName = lets.toMap

    def exprFor(name: Bindable): Expr[K] =
      exprByName.getOrElse(
        name,
        sys.error(
          s"missing let $name in Matchless dependency order for package $pack in scope $scope"
        )
      )

    val topo = Toposort.sort(nodeByName.values) { node =>
      collectGlobals(exprFor(node.name))
        .iterator
        .collect {
          case (from, `pack`, name)
              if (name != node.name) && Order[K].eqv(depFor(from, pack), scope) =>
            nodeByName.get(name)
        }
        .flatten
        .toList
    }

    val ordered = topo.layers.iterator.flatMap(_.toList).map(_.name).toList
    val missing =
      lets.iterator.map(_._1).filterNot(ordered.toSet).toList

    ordered ::: missing
  }

  private def collectGlobals[A](
      expr: Expr[A]
  ): Set[(A, PackageName, Bindable)] = {
    type Node = Matchless.Expr[A] | Matchless.BoolExpr[A]

    @annotation.tailrec
    def loop(
        todo: List[Node],
        acc: Set[(A, PackageName, Bindable)]
    ): Set[(A, PackageName, Bindable)] =
      todo match {
        case head :: tail =>
          head match {
            case Lambda(captures, _, _, body) =>
              loop(body :: captures ::: tail, acc)
            case Matchless.WhileExpr(cond, effectExpr, _) =>
              loop(cond :: effectExpr :: tail, acc)
            case Matchless.App(fn, args) =>
              loop(fn :: args.toList ::: tail, acc)
            case Matchless.Let(_, value, in) =>
              loop(value :: in :: tail, acc)
            case Matchless.LetMut(_, in) =>
              loop(in :: tail, acc)
            case Matchless.If(cond, thenExpr, elseExpr) =>
              loop(cond :: thenExpr :: elseExpr :: tail, acc)
            case Matchless.SwitchVariant(on, _, cases, default) =>
              loop(on :: default.toList ::: cases.toList.map(_._2) ::: tail, acc)
            case Matchless.Always(cond, thenExpr) =>
              loop(cond :: thenExpr :: tail, acc)
            case Matchless.PrevNat(of) =>
              loop(of :: tail, acc)
            case Global(from, pkg, name) =>
              loop(tail, acc + ((from, pkg, name)))
            case ge: Matchless.GetEnumElement[?] =>
              loop(ge.arg :: tail, acc)
            case gs: Matchless.GetStructElement[?] =>
              loop(gs.arg :: tail, acc)
            case Matchless.EqualsLit(arg, _) =>
              loop(arg :: tail, acc)
            case Matchless.LtEqLit(arg, _) =>
              loop(arg :: tail, acc)
            case Matchless.EqualsNat(arg, _) =>
              loop(arg :: tail, acc)
            case Matchless.And(left, right) =>
              loop(left :: right :: tail, acc)
            case Matchless.CheckVariant(arg, _, _, _) =>
              loop(arg :: tail, acc)
            case Matchless.CheckVariantSet(arg, _, _, _) =>
              loop(arg :: tail, acc)
            case Matchless.SetMut(_, value) =>
              loop(value :: tail, acc)
            case Matchless.LetBool(_, value, in) =>
              loop(value :: in :: tail, acc)
            case Matchless.LetMutBool(_, in) =>
              loop(in :: tail, acc)
            case Matchless.Local(_) | Matchless.ClosureSlot(_) | Matchless.LocalAnon(_) |
                Matchless.LocalAnonMut(_) | Matchless.Literal(_) |
                Matchless.MakeEnum(_, _, _) | Matchless.MakeStruct(_) |
                _: Matchless.SuccNat.type | _: Matchless.ZeroNat.type |
                _: Matchless.TrueConst.type =>
              loop(tail, acc)
          }
        case _ =>
          acc
      }

    loop(expr :: Nil, Set.empty)
  }

  private def rewriteExpr[K](
      expr: Expr[K],
      summaries: Map[SummaryKey[K], InlineSummary[K]],
      depFor: (K, PackageName) => K
  ): Expr[K] = {
    var remainingInlineBudget =
      InlineBenefitModel.definitionInlineBudget(Matchless.exprWeight(expr))

    def summaryKey(from: K, pack: PackageName, name: Bindable): SummaryKey[K] =
      SummaryKey(depFor(from, pack), pack, name)

    def cheapReferenceValue(value: Expr[K]): Option[Matchless.CheapExpr[K]] =
      value match {
        case lit @ Matchless.Literal(_) =>
          Some(lit)
        case global @ Matchless.Global(_, _, _) =>
          Some(global)
        case local @ Matchless.Local(_) =>
          Some(local)
        case slot @ Matchless.ClosureSlot(_) =>
          Some(slot)
        case anon @ Matchless.LocalAnon(_) =>
          Some(anon)
        case anonMut @ Matchless.LocalAnonMut(_) =>
          Some(anonMut)
        case ge @ Matchless.GetEnumElement(_, _, _, _) =>
          Some(ge)
        case gs @ Matchless.GetStructElement(_, _, _) =>
          Some(gs)
        case _ =>
          None
      }

    def inlineReferenceExpr(
        from: K,
        pack: PackageName,
        name: Bindable,
        allowNonCheap: Boolean
    ): Option[Expr[K]] =
      summaries
        .get(summaryKey(from, pack, name))
        .filter(shouldInlineReference)
        .flatMap {
          case summary: LambdaSummary[K] =>
            if (allowNonCheap) Some(summary.lambda) else None
          case summary: ValueSummary[K]  =>
            if (allowNonCheap) Some(summary.value)
            else cheapReferenceValue(summary.value)
        }

    def knownSummaryValue(
        ex: Expr[K],
        seen: Set[SummaryKey[K]]
    ): Option[Expr[K]] =
      ex match {
        case Matchless.Global(from, pack, name) =>
          val key = summaryKey(from, pack, name)
          if (seen(key)) None
          else
            summaries.get(key).flatMap {
              case summary: ValueSummary[K] =>
                knownSummaryValue(summary.value, seen + key).orElse(Some(summary.value))
              case _ =>
                None
            }
        case lit @ Matchless.Literal(_) =>
          Some(lit)
        case enumExpr @ Matchless.MakeEnum(_, 0, _) =>
          Some(enumExpr)
        case structExpr @ Matchless.MakeStruct(0) =>
          Some(structExpr)
        case Matchless.ZeroNat =>
          Some(Matchless.ZeroNat)
        case Matchless.App(cons @ Matchless.MakeEnum(_, arity, _), args)
            if args.length == arity =>
          args.toList
            .traverse(knownSummaryValue(_, seen))
            .map(args1 => Matchless.App(cons, NonEmptyList.fromListUnsafe(args1)))
        case Matchless.App(cons @ Matchless.MakeStruct(arity), args)
            if args.length == arity =>
          args.toList
            .traverse(knownSummaryValue(_, seen))
            .map(args1 => Matchless.App(cons, NonEmptyList.fromListUnsafe(args1)))
        case Matchless.App(Matchless.SuccNat, NonEmptyList(arg, Nil)) =>
          knownSummaryValue(arg, seen)
            .map(arg1 => Matchless.App(Matchless.SuccNat, NonEmptyList.one(arg1)))
        case Matchless.PrevNat(of) =>
          knownSummaryValue(of, seen).collect {
            case Matchless.App(Matchless.SuccNat, NonEmptyList(prev, Nil)) => prev
          }
        case Matchless.GetEnumElement(arg, variant, index, size) =>
          knownSummaryValue(arg, seen).flatMap {
            case Matchless.App(Matchless.MakeEnum(v, arity, _), args)
                if (v == variant) && (arity == size) =>
              args.toList.lift(index)
            case _ =>
              None
          }
        case Matchless.GetStructElement(arg, index, size) =>
          knownSummaryValue(arg, seen).flatMap {
            case Matchless.App(Matchless.MakeStruct(arity), args) if arity == size =>
              args.toList.lift(index)
            case value if (size == 1) && (index == 0) =>
              Some(value)
            case _ =>
              None
          }
        case _ =>
          None
      }

    def loop(ex: Expr[K]): Expr[K] =
      ex match {
        case Matchless.Lambda(captures, recursiveName, args, body) =>
          Matchless.Lambda(captures.map(loop), recursiveName, args, loop(body))
        case Matchless.WhileExpr(cond, effectExpr, result) =>
          Matchless.WhileExpr(loopBool(cond), loop(effectExpr), result)
        case Matchless.App(fn, args) =>
          val fn1 =
            fn match {
              case global @ Matchless.Global(_, _, _) =>
                global
              case other =>
                loop(other)
            }
          val args1 = args.map(loop)
          inlineGlobal(fn1, args1) match {
            case Some(inlined) =>
              inlined
            case None =>
              Matchless.inlineApplyArgs(fn1, args1) match {
                case app @ Matchless.App(_, _) => app
                case reduced                   => loop(reduced)
              }
          }
        case Matchless.Let(arg, value, in) =>
          Matchless.Let(arg, loop(value), loop(in))
        case Matchless.LetMut(name, in) =>
          Matchless.LetMut(name, loop(in))
        case Matchless.If(cond, thenExpr, elseExpr) =>
          Matchless.If(loopBool(cond), loop(thenExpr), loop(elseExpr))
        case Matchless.SwitchVariant(on, famArities, cases, default) =>
          Matchless.SwitchVariant(
            loopCheap(on),
            famArities,
            cases.map { case (variant, branch) =>
              (variant, loop(branch))
            },
            default.map(loop)
          )
        case Matchless.Always(cond, thenExpr) =>
          Matchless.Always(loopBool(cond), loop(thenExpr))
        case Matchless.PrevNat(of) =>
          Matchless.PrevNat(loop(of))
        case global @ Matchless.Global(from, pack, name) =>
          inlineReferenceExpr(from, pack, name, allowNonCheap = true)
            .fold(global: Expr[K])(loop)
        case Matchless.GetEnumElement(arg, variant, index, size) =>
          val rewritten =
            Matchless.GetEnumElement(loopCheap(arg), variant, index, size)
          knownSummaryValue(rewritten, Set.empty).fold(rewritten: Expr[K])(loop)
        case Matchless.GetStructElement(arg, index, size) =>
          val rewritten =
            Matchless.GetStructElement(loopCheap(arg), index, size)
          knownSummaryValue(rewritten, Set.empty).fold(rewritten: Expr[K])(loop)
        case Matchless.Local(_) | Matchless.ClosureSlot(_) |
            Matchless.LocalAnon(_) | Matchless.LocalAnonMut(_) |
            Matchless.Literal(_) | Matchless.MakeEnum(_, _, _) |
            Matchless.MakeStruct(_) | Matchless.SuccNat | Matchless.ZeroNat =>
          ex
      }

    def loopCheap(ex: Matchless.CheapExpr[K]): Matchless.CheapExpr[K] =
      ex match {
        case global @ Matchless.Global(from, pack, name) =>
          inlineReferenceExpr(from, pack, name, allowNonCheap = false) match {
            case Some(inlined) =>
              inlined match {
                case cheap: Matchless.CheapExpr[K] =>
                  loopCheap(cheap)
                case notCheap =>
                  throw new IllegalStateException(
                    s"expected cheap global inline during Matchless global inlining, found: $notCheap"
                  )
              }
            case None =>
              global
          }
        case _ =>
          loop(ex) match {
            case ch: Matchless.CheapExpr[K] => ch
            case notCheap                   =>
              throw new IllegalStateException(
                s"expected cheap expression during Matchless global inlining, found: $notCheap"
              )
          }
      }

    def loopBool(ex: Matchless.BoolExpr[K]): Matchless.BoolExpr[K] =
      ex match {
        case Matchless.EqualsLit(arg, lit) =>
          Matchless.EqualsLit(loopCheap(arg), lit)
        case Matchless.LtEqLit(arg, lit) =>
          Matchless.LtEqLit(loopCheap(arg), lit)
        case Matchless.EqualsNat(arg, nat) =>
          Matchless.EqualsNat(loopCheap(arg), nat)
        case Matchless.And(left, right) =>
          Matchless.And(loopBool(left), loopBool(right))
        case Matchless.CheckVariant(arg, expect, size, famArities) =>
          Matchless.CheckVariant(loopCheap(arg), expect, size, famArities)
        case Matchless.CheckVariantSet(arg, expect, size, famArities) =>
          Matchless.CheckVariantSet(loopCheap(arg), expect, size, famArities)
        case Matchless.SetMut(target, value) =>
          Matchless.SetMut(target, loop(value))
        case Matchless.LetBool(arg, value, in) =>
          Matchless.LetBool(arg, loop(value), loopBool(in))
        case Matchless.LetMutBool(name, in) =>
          Matchless.LetMutBool(name, loopBool(in))
        case Matchless.TrueConst =>
          Matchless.TrueConst
      }

    def inlineGlobal(
        fn: Expr[K],
        args: NonEmptyList[Expr[K]]
    ): Option[Expr[K]] =
      fn match {
        case Global(from, pack, name) =>
          val key = summaryKey(from, pack, name)
          summaries
            .get(key)
            .collect { case summary: LambdaSummary[K] => summary }
            .flatMap { summary =>
              shouldInline(summary, args, remainingInlineBudget).map { inlineCost =>
                (summary, inlineCost)
              }
            }
            .map { case (summary, inlineCost) =>
              remainingInlineBudget = (remainingInlineBudget - inlineCost).max(0)
              loop(Matchless.inlineApplyArgs(summary.lambda, args))
            }
        case _ =>
          None
      }

    loop(expr)
  }

  private def shouldInline[K](
      summary: LambdaSummary[K],
      args: NonEmptyList[Expr[K]],
      remainingBudget: Int
  ): Option[Int] = {
    if (summary.exposesFreeMutableAnon) None
    else if (summary.arity != args.length) None
    else {
      val argList = args.toList
      val hasKnownSelector =
        summary.paramDemand.iterator.zip(argList.iterator).exists {
          case (demand, arg) =>
            (demand.cheapPositionUses > 0) && isKnownArgumentValue(arg)
        }
      val whileUnknownDirectCallee =
        summary.containsWhileExpr &&
          summary.paramDemand.iterator.zip(argList.iterator).exists {
            case (demand, arg) =>
              demand.lambdaCalleeOnly && !isKnownDirectCalleeArgument(arg)
          }
      val deferredCheapPositionArgument =
        summary.paramDemand.iterator.zip(argList.iterator).exists {
          case (demand, arg) =>
            (demand.eagerUses == 0) &&
            (demand.cheapPositionUses > 0) &&
            !isCheapArgument(arg)
        }
      val duplicatedDeferredExpensiveArg =
        summary.paramDemand.iterator.zip(argList.iterator).exists {
          case (demand, arg) =>
            (demand.eagerUses == 0) &&
            (demand.totalUses > MaxExpensiveArgUses) &&
            !isCheapArgument(arg)
        }

      if (
        whileUnknownDirectCallee ||
        (!hasKnownSelector && deferredCheapPositionArgument) ||
        (!hasKnownSelector && duplicatedDeferredExpensiveArg)
      ) None
      else {
        val callSummary =
          InlineBenefitModel.CallSiteSummary(
            calleeWeight = summary.bodyWeight,
            containsLoopLike = summary.containsWhileExpr,
            exposesBranching = summary.exposesBranching,
            exposesFollowOnReduction = summary.exposesFollowOnReduction,
            args = summary.paramDemand.iterator
              .zip(argList.iterator)
              .map { case (demand, arg) =>
                InlineBenefitModel.ArgSummary(
                  demand = InlineBenefitModel.ParamSummary(
                    totalUses = demand.totalUses,
                    eagerUses = demand.eagerUses,
                    branchOnlyUses = demand.branchOnlyUses,
                    directCalleeUses = demand.directCalleeUses,
                    nonDirectCalleeUses = demand.nonDirectCalleeUses,
                    cheapPositionUses = demand.cheapPositionUses
                  ),
                  isCheap = isCheapArgument(arg),
                  resolvesToLambda = resolvesToLambda(arg).nonEmpty,
                  isKnownDirectCallee = isKnownDirectCalleeArgument(arg),
                  isKnownValue = isKnownArgumentValue(arg)
                )
              }
              .toVector
          )

        val inlineCost = InlineBenefitModel.callInlineBudgetCost(callSummary)

        if (
          InlineBenefitModel.shouldInlineCall(callSummary) &&
          (inlineCost <= remainingBudget)
        ) Some(inlineCost)
        else None
      }
    }
  }

  private def shouldInlineReference[K](summary: InlineSummary[K]): Boolean =
    summary match {
      case summary: LambdaSummary[K] =>
        summary.lambda.captures.isEmpty &&
          InlineBenefitModel.shouldInlineTinyReference(
            summary.bodyWeight,
            summary.containsWhileExpr
          )
      case summary: ValueSummary[K]  =>
        !summary.exposesFreeMutableAnon &&
          InlineBenefitModel.shouldInlineTinyReference(
            summary.bodyWeight,
            summary.containsWhileExpr
          )
    }

  private def resolvesToLambda[A](expr: Expr[A]): Option[Lambda[A]] =
    Matchless.recoverTopLevelLambda(expr) match {
      case lam: Lambda[A] => Some(lam)
      case _              => None
    }

  private def isKnownDirectCalleeArgument[A](expr: Expr[A]): Boolean =
    expr match {
      case Matchless.Global(_, _, _) =>
        true
      case _ =>
        resolvesToLambda(expr).nonEmpty
    }

  private def isCheapArgument[A](expr: Expr[A]): Boolean =
    Matchless.isTriviallyCheap(expr) ||
      (!Matchless.hasSideEffect(expr) &&
        !Matchless.Expr.readsMutable(expr) &&
        !Matchless.Expr.containsWhileExpr(expr) &&
        (Matchless.exprWeight(expr) <= CheapArgWeightBudget))

  private def isKnownArgumentValue[A](expr: Expr[A]): Boolean =
    expr match {
      case Matchless.MakeEnum(_, 0, _) |
          Matchless.MakeStruct(0) =>
        true
      case Matchless.App(Matchless.MakeEnum(_, arity, _), args) if args.length == arity =>
        args.forall(isKnownArgumentValue)
      case Matchless.App(Matchless.MakeStruct(arity), args) if args.length == arity =>
        args.forall(isKnownArgumentValue)
      case _ =>
        false
    }

  private def isDirectForwardedValue[A](expr: Expr[A]): Boolean =
    expr match {
      case Matchless.Local(_) | Matchless.ClosureSlot(_) => true
      case _                                             => false
    }

  @annotation.tailrec
  private def exposesFollowOnReduction[A](expr: Expr[A]): Boolean =
    expr match {
      case Matchless.App(fn, args)
          if args.forall(isDirectForwardedValue) &&
            (fn match {
              case Matchless.Global(_, _, _) | Matchless.Local(_) |
                  Matchless.ClosureSlot(_) | Matchless.MakeStruct(_) |
                  Matchless.MakeEnum(_, _, _) | Matchless.SuccNat =>
                true
              case _ =>
                false
            }) =>
        true
      case Matchless.GetEnumElement(arg, _, _, _) if isDirectForwardedValue(arg) =>
        true
      case Matchless.GetStructElement(arg, _, _) if isDirectForwardedValue(arg) =>
        true
      case Matchless.PrevNat(of) if isDirectForwardedValue(of) =>
        true
      case Matchless.Let(_, value, in)
          if isCheapArgument(value) &&
            !Matchless.hasSideEffect(value) &&
            !Matchless.Expr.readsMutable(value) =>
        exposesFollowOnReduction(in)
      case _ =>
        false
    }

  @annotation.tailrec
  private def exposesBranching[A](expr: Expr[A]): Boolean =
    expr match {
      case Matchless.If(_, _, _) | Matchless.SwitchVariant(_, _, _, _) =>
        true
      case Matchless.Let(_, value, in)
          if isCheapArgument(value) &&
            !Matchless.hasSideEffect(value) &&
            !Matchless.Expr.readsMutable(value) =>
        exposesBranching(in)
      case _ =>
        false
    }

  private def summarize[K](
      expr: Expr[K]
  ): Option[InlineSummary[K]] =
    Matchless.recoverTopLevelLambda(expr) match {
      case lam: Lambda[K] =>
        val bodyWeight = Matchless.exprWeight(expr)
        if ((bodyWeight > InlineBodyWeightBudget) || lam.recursiveName.nonEmpty) None
        else {
          Some(
            LambdaSummary(
              lambda = lam,
              bodyWeight = bodyWeight,
              paramDemand = Matchless.parameterDemandSummary(lam),
              containsWhileExpr = Matchless.Expr.containsWhileExpr(expr),
              exposesFreeMutableAnon = freeMutableAnonIds(expr).nonEmpty,
              exposesBranching = exposesBranching(lam.body),
              exposesFollowOnReduction = exposesFollowOnReduction(lam.body)
            )
          )
        }
      case _ =>
        summarizeValue(expr)
    }

  private def summarizeValue[K](
      expr: Expr[K]
  ): Option[InlineSummary[K]] = {
    def knownValueNoGlobals(ex: Expr[K]): Option[Expr[K]] =
      ex match {
        case lit @ Matchless.Literal(_) =>
          Some(lit)
        case enumExpr @ Matchless.MakeEnum(_, 0, _) =>
          Some(enumExpr)
        case structExpr @ Matchless.MakeStruct(0) =>
          Some(structExpr)
        case Matchless.ZeroNat =>
          Some(Matchless.ZeroNat)
        case Matchless.App(cons @ Matchless.MakeEnum(_, arity, _), args)
            if args.length == arity =>
          args.toList
            .traverse(knownValueNoGlobals)
            .map(args1 => Matchless.App(cons, NonEmptyList.fromListUnsafe(args1)))
        case Matchless.App(cons @ Matchless.MakeStruct(arity), args)
            if args.length == arity =>
          args.toList
            .traverse(knownValueNoGlobals)
            .map(args1 => Matchless.App(cons, NonEmptyList.fromListUnsafe(args1)))
        case Matchless.App(Matchless.SuccNat, NonEmptyList(arg, Nil)) =>
          knownValueNoGlobals(arg)
            .map(arg1 => Matchless.App(Matchless.SuccNat, NonEmptyList.one(arg1)))
        case Matchless.PrevNat(of) =>
          knownValueNoGlobals(of).collect {
            case Matchless.App(Matchless.SuccNat, NonEmptyList(prev, Nil)) => prev
          }
        case Matchless.GetEnumElement(arg, variant, index, size) =>
          knownValueNoGlobals(arg).flatMap {
            case Matchless.App(Matchless.MakeEnum(v, arity, _), args)
                if (v == variant) && (arity == size) =>
              args.toList.lift(index)
            case _ =>
              None
          }
        case Matchless.GetStructElement(arg, index, size) =>
          knownValueNoGlobals(arg).flatMap {
            case Matchless.App(Matchless.MakeStruct(arity), args) if arity == size =>
              args.toList.lift(index)
            case value if (size == 1) && (index == 0) =>
              Some(value)
            case _ =>
              None
          }
        case _ =>
          None
      }

    val containsWhile = Matchless.Expr.containsWhileExpr(expr)
    val exposesFreeMutableAnon = freeMutableAnonIds(expr).nonEmpty

    if (
      containsWhile ||
      exposesFreeMutableAnon ||
      Matchless.hasSideEffect(expr) ||
      Matchless.Expr.readsMutable(expr)
    ) None
    else
      knownValueNoGlobals(expr).flatMap { value =>
        val bodyWeight = Matchless.exprWeight(value)
        if (bodyWeight <= TinyBodyWeightBudget)
          Some(
            ValueSummary(
              value = value,
              bodyWeight = bodyWeight,
              containsWhileExpr = false,
              exposesFreeMutableAnon = false
            )
          )
        else None
      }
  }

  private def freeMutableAnonIds[A](expr: Expr[A]): Set[Long] = {
    def loopExpr(ex: Expr[A], boundMuts: Set[Long]): Set[Long] =
      ex match {
        case Lambda(captures, _, _, body) =>
          captures.iterator.foldLeft(loopExpr(body, boundMuts)) { case (acc, cap) =>
            acc ++ loopExpr(cap, boundMuts)
          }
        case Matchless.WhileExpr(cond, effectExpr, result) =>
          val resultIds =
            if (boundMuts(result.ident)) Set.empty else Set(result.ident)
          resultIds ++ loopBool(cond, boundMuts) ++ loopExpr(effectExpr, boundMuts)
        case Matchless.App(fn, args) =>
          args.iterator.foldLeft(loopExpr(fn, boundMuts)) { case (acc, arg) =>
            acc ++ loopExpr(arg, boundMuts)
          }
        case Matchless.Let(_, value, in) =>
          loopExpr(value, boundMuts) ++ loopExpr(in, boundMuts)
        case Matchless.LetMut(name, in) =>
          loopExpr(in, boundMuts + name.ident)
        case Matchless.If(cond, thenExpr, elseExpr) =>
          loopBool(cond, boundMuts) ++ loopExpr(thenExpr, boundMuts) ++ loopExpr(
            elseExpr,
            boundMuts
          )
        case Matchless.SwitchVariant(on, _, cases, default) =>
          loopExpr(on, boundMuts) ++
            cases.iterator.foldLeft(Set.empty[Long]) { case (acc, (_, branch)) =>
              acc ++ loopExpr(branch, boundMuts)
            } ++
            default.fold(Set.empty[Long])(loopExpr(_, boundMuts))
        case Matchless.Always(cond, thenExpr) =>
          loopBool(cond, boundMuts) ++ loopExpr(thenExpr, boundMuts)
        case Matchless.PrevNat(of) =>
          loopExpr(of, boundMuts)
        case Matchless.LocalAnonMut(id) =>
          if (boundMuts(id)) Set.empty else Set(id)
        case ge: Matchless.GetEnumElement[?] =>
          loopExpr(ge.arg, boundMuts)
        case gs: Matchless.GetStructElement[?] =>
          loopExpr(gs.arg, boundMuts)
        case Matchless.Local(_) | Matchless.Global(_, _, _) | Matchless.ClosureSlot(_) |
            Matchless.LocalAnon(_) | Matchless.Literal(_) |
            Matchless.MakeEnum(_, _, _) | Matchless.MakeStruct(_) |
            Matchless.SuccNat | Matchless.ZeroNat =>
          Set.empty
      }

    def loopBool(
        ex: Matchless.BoolExpr[A],
        boundMuts: Set[Long]
    ): Set[Long] =
      ex match {
        case Matchless.EqualsLit(expr, _) =>
          loopExpr(expr, boundMuts)
        case Matchless.LtEqLit(expr, _) =>
          loopExpr(expr, boundMuts)
        case Matchless.EqualsNat(expr, _) =>
          loopExpr(expr, boundMuts)
        case Matchless.And(left, right) =>
          loopBool(left, boundMuts) ++ loopBool(right, boundMuts)
        case Matchless.CheckVariant(expr, _, _, _) =>
          loopExpr(expr, boundMuts)
        case Matchless.CheckVariantSet(expr, _, _, _) =>
          loopExpr(expr, boundMuts)
        case Matchless.SetMut(target, value) =>
          val targetIds =
            if (boundMuts(target.ident)) Set.empty else Set(target.ident)
          targetIds ++ loopExpr(value, boundMuts)
        case Matchless.LetBool(_, value, in) =>
          loopExpr(value, boundMuts) ++ loopBool(in, boundMuts)
        case Matchless.LetMutBool(name, in) =>
          loopBool(in, boundMuts + name.ident)
        case Matchless.TrueConst =>
          Set.empty
      }

    loopExpr(expr, Set.empty)
  }
}
