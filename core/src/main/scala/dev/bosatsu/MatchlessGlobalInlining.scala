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

  final case class InlineSummary[A](
      lambda: Lambda[A],
      bodyWeight: Int,
      paramDemand: Vector[ParamDemand],
      containsWhileExpr: Boolean,
      exposesFreeMutableAnon: Boolean,
      alwaysInline: Boolean
  ) {
    def arity: Int = lambda.arity
  }

  private val InlineBodyWeightBudget = 80
  private val TinyBodyWeightBudget = 10
  private val CheapArgWeightBudget = 3
  private val MaxExpensiveArgUses = 1

  def optimize[K: Order](
      rawCompiled: SortedMap[K, MatchlessFromTypedExpr.Compiled[K]],
      topoSort: Toposort.Result[(K, PackageName)],
      depFor: (K, PackageName) => K
  )(implicit ec: Par.EC): SortedMap[K, MatchlessFromTypedExpr.Compiled[K]] = {
    val cleanedBase = SortedMap.from(rawCompiled.iterator.map { case (scope, packs) =>
      val packs1 = packs.iterator.map { case (pack, lets) =>
        pack -> lets.map { case (name, expr) =>
          (name, Matchless.postLoweringCleanup(expr))
        }
      }.toMap
      scope -> packs1
    })(using rawCompiled.ordering)

    val layers =
      topoSort.layers ++ topoSort.loopNodes
        .sorted(using Order[(K, PackageName)].toOrdering)
        .map(NonEmptyList.one)

    layers.foldLeft((cleanedBase, Map.empty[SummaryKey[K], InlineSummary[K]])) {
      case ((compiledAcc, summaryAcc), layer) =>
        val tasks = layer.toList.map { case (scope, pack) =>
          Par.start {
            val rawLets =
              rawCompiled
                .get(scope)
                .flatMap(_.get(pack))
                .getOrElse(Nil)
            rewritePackage(scope, pack, rawLets, summaryAcc, depFor)
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
      rawLets: List[(Bindable, Expr[K])],
      priorSummaries: Map[SummaryKey[K], InlineSummary[K]],
      depFor: (K, PackageName) => K
  ): (K, PackageName, List[(Bindable, Expr[K])], Map[SummaryKey[K], InlineSummary[K]]) = {
    val cleanedLets = rawLets.map { case (name, expr) =>
      (name, Matchless.postLoweringCleanup(expr))
    }
    val byName = cleanedLets.toMap
    val localOrder = localDependencyOrder(scope, pack, cleanedLets, depFor)

    val (rewrittenByName, published) =
      localOrder.foldLeft((Map.empty[Bindable, Expr[K]], priorSummaries)) {
        case ((rewritten, available), name) =>
          val expr0 = byName.getOrElse(name, Matchless.UnitExpr)
          val rewrittenExpr =
            Matchless.postLoweringCleanup(
              rewriteExpr(expr0, available, depFor)
            )
          val available1 =
            summarize(pack, name, rewrittenExpr).fold(available) { summary =>
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
    )(using
      Ordering.Tuple2(using
        Ordering.Int,
        Identifier.Bindable.bindableOrder.toOrdering
      )
    )

    val nodeByName = lets.zipWithIndex.iterator.map { case ((name, _), idx) =>
      name -> LocalNode(idx, name)
    }.toMap
    val exprByName = lets.toMap

    val topo = Toposort.sort(nodeByName.values) { node =>
      collectGlobals(exprByName.getOrElse(node.name, Matchless.UnitExpr))
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
    def loopExpr(ex: Expr[A], acc: Set[(A, PackageName, Bindable)]): Set[(A, PackageName, Bindable)] =
      ex match {
        case Lambda(captures, _, _, body) =>
          val acc1 = captures.foldLeft(acc) { case (nextAcc, capture) =>
            loopExpr(capture, nextAcc)
          }
          loopExpr(body, acc1)
        case Matchless.WhileExpr(cond, effectExpr, _) =>
          loopExpr(effectExpr, loopBool(cond, acc))
        case Matchless.App(fn, args) =>
          args.iterator.foldLeft(loopExpr(fn, acc)) { case (nextAcc, arg) =>
            loopExpr(arg, nextAcc)
          }
        case Matchless.Let(_, value, in) =>
          loopExpr(in, loopExpr(value, acc))
        case Matchless.LetMut(_, in) =>
          loopExpr(in, acc)
        case Matchless.If(cond, thenExpr, elseExpr) =>
          loopExpr(elseExpr, loopExpr(thenExpr, loopBool(cond, acc)))
        case Matchless.SwitchVariant(on, _, cases, default) =>
          val acc1 = loopExpr(on, acc)
          val acc2 = default.fold(acc1)(loopExpr(_, acc1))
          cases.foldLeft(acc2) { case (nextAcc, (_, branch)) =>
            loopExpr(branch, nextAcc)
          }
        case Matchless.Always(cond, thenExpr) =>
          loopExpr(thenExpr, loopBool(cond, acc))
        case Matchless.PrevNat(of) =>
          loopExpr(of, acc)
        case Global(from, pkg, name) =>
          acc + ((from, pkg, name))
        case ge: Matchless.GetEnumElement[?] =>
          loopExpr(ge.arg, acc)
        case gs: Matchless.GetStructElement[?] =>
          loopExpr(gs.arg, acc)
        case Matchless.Local(_) | Matchless.ClosureSlot(_) | Matchless.LocalAnon(_) |
            Matchless.LocalAnonMut(_) | Matchless.Literal(_) |
            Matchless.MakeEnum(_, _, _) | Matchless.MakeStruct(_) |
            Matchless.SuccNat | Matchless.ZeroNat =>
          acc
      }

    def loopBool(
        ex: Matchless.BoolExpr[A],
        acc: Set[(A, PackageName, Bindable)]
    ): Set[(A, PackageName, Bindable)] =
      ex match {
        case Matchless.EqualsLit(expr, _) =>
          loopExpr(expr, acc)
        case Matchless.LtEqLit(expr, _) =>
          loopExpr(expr, acc)
        case Matchless.EqualsNat(expr, _) =>
          loopExpr(expr, acc)
        case Matchless.And(left, right) =>
          loopBool(right, loopBool(left, acc))
        case Matchless.CheckVariant(expr, _, _, _) =>
          loopExpr(expr, acc)
        case Matchless.CheckVariantSet(expr, _, _, _) =>
          loopExpr(expr, acc)
        case Matchless.SetMut(_, value) =>
          loopExpr(value, acc)
        case Matchless.LetBool(_, value, in) =>
          loopBool(in, loopExpr(value, acc))
        case Matchless.LetMutBool(_, in) =>
          loopBool(in, acc)
        case Matchless.TrueConst =>
          acc
      }

    loopExpr(expr, Set.empty)
  }

  private def rewriteExpr[K](
      expr: Expr[K],
      summaries: Map[SummaryKey[K], InlineSummary[K]],
      depFor: (K, PackageName) => K
  ): Expr[K] = {
    def builtinLambda(
        pack: PackageName,
        name: Bindable
    ): Option[Lambda[K]] =
      if (pack != PackageName.PredefName) None
      else
        name match {
          case Identifier.Name("and") =>
            val left = Identifier.Name("left")
            val right = Identifier.Name("right")
            Some(
              Lambda(
                captures = Nil,
                recursiveName = None,
                args = NonEmptyList.of(left, right),
                body = Matchless.If(
                  Matchless.isTrueExpr(Matchless.Local(left)),
                  Matchless.Local(right),
                  Matchless.FalseExpr
                )
              )
            )
          case Identifier.Name("or")  =>
            val left = Identifier.Name("left")
            val right = Identifier.Name("right")
            Some(
              Lambda(
                captures = Nil,
                recursiveName = None,
                args = NonEmptyList.of(left, right),
                body = Matchless.If(
                  Matchless.isTrueExpr(Matchless.Local(left)),
                  Matchless.TrueExpr,
                  Matchless.Local(right)
                )
              )
            )
          case _ =>
            None
        }

    def builtinInline(
        pack: PackageName,
        name: Bindable,
        args: NonEmptyList[Expr[K]]
    ): Option[Expr[K]] =
      builtinLambda(pack, name).map(lam =>
        loop(Matchless.inlineApplyArgs(lam, args))
      )

    def loop(ex: Expr[K]): Expr[K] =
      ex match {
        case Matchless.Lambda(captures, recursiveName, args, body) =>
          Matchless.Lambda(captures.map(loop), recursiveName, args, loop(body))
        case Matchless.WhileExpr(cond, effectExpr, result) =>
          Matchless.WhileExpr(loopBool(cond), loop(effectExpr), result)
        case Matchless.App(fn, args) =>
          val fn1 = loop(fn)
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
        case Matchless.Global(_, pack, name) =>
          builtinLambda(pack, name).getOrElse(ex)
        case ge: Matchless.GetEnumElement[?] =>
          ge.copy(arg = loopCheap(ge.arg))
        case gs: Matchless.GetStructElement[?] =>
          gs.copy(arg = loopCheap(gs.arg))
        case Matchless.Local(_) | Matchless.ClosureSlot(_) |
            Matchless.LocalAnon(_) | Matchless.LocalAnonMut(_) |
            Matchless.Literal(_) | Matchless.MakeEnum(_, _, _) |
            Matchless.MakeStruct(_) | Matchless.SuccNat | Matchless.ZeroNat =>
          ex
      }

    def loopCheap(ex: Matchless.CheapExpr[K]): Matchless.CheapExpr[K] =
      loop(ex) match {
        case ch: Matchless.CheapExpr[K] => ch
        case notCheap                   =>
          throw new IllegalStateException(
            s"expected cheap expression during Matchless global inlining, found: $notCheap"
          )
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
          builtinInline(pack, name, args).orElse {
            val resolvedScope = depFor(from, pack)
            val key = SummaryKey(resolvedScope, pack, name)
            summaries.get(key).filter(summary =>
              shouldInline(summary, args)
            ).map { summary =>
              loop(Matchless.inlineApplyArgs(summary.lambda, args))
            }
          }
        case _ =>
          None
      }

    loop(expr)
  }

  private def shouldInline[K](
      summary: InlineSummary[K],
      args: NonEmptyList[Expr[K]]
  ): Boolean = {
    if (summary.exposesFreeMutableAnon) false
    else if (summary.arity != args.length) false
    else {
      val argList = args.toList
      val whileLambdaArgument =
        // Recursive/while-lowered helpers remain eligible in general, but
        // direct-callee lambda substitution through those loops needs a more
        // specific proof than v1 has.
        summary.containsWhileExpr &&
          summary.paramDemand.iterator.zip(argList.iterator).exists {
            case (demand, arg) =>
              demand.lambdaCalleeOnly && resolvesToLambda(arg).nonEmpty
          }
      val nonCheapCheapPositionArgument =
        summary.paramDemand.iterator.zip(argList.iterator).exists {
          case (demand, arg) =>
            (demand.cheapPositionUses > 0) && !isCheapArgument(arg)
        }
      val eagerNonCheapArgument =
        summary.paramDemand.iterator.zip(argList.iterator).exists {
          case (demand, arg) =>
            !isCheapArgument(arg) &&
            !demand.unused &&
            !demand.deferrable &&
            !demand.lambdaCalleeOnly
        }
      val duplicatedExpensiveArg =
        summary.paramDemand.iterator.zip(argList.iterator).exists {
          case (demand, arg) =>
            (demand.totalUses > MaxExpensiveArgUses) && !isCheapArgument(arg)
        }

      if (
        whileLambdaArgument ||
        nonCheapCheapPositionArgument ||
        eagerNonCheapArgument ||
        duplicatedExpensiveArg
      ) false
      else if (summary.alwaysInline) true
      else {
        val hasUnusedBenefit =
          summary.paramDemand.iterator.zip(argList.iterator).exists {
            case (demand, arg) =>
              demand.unused && !Matchless.isTriviallyCheap(arg)
          }
        val hasDeferrableBenefit =
          summary.paramDemand.iterator.zip(argList.iterator).exists {
            case (demand, arg) =>
              demand.deferrable && !isCheapArgument(arg)
          }
        val hasLambdaBenefit =
          summary.paramDemand.iterator.zip(argList.iterator).exists {
            case (demand, arg) =>
              demand.lambdaCalleeOnly && resolvesToLambda(arg).nonEmpty
          }
        val tinyPure =
          (summary.bodyWeight <= TinyBodyWeightBudget) && !summary.containsWhileExpr
        val score =
          (if (hasUnusedBenefit) 3 else 0) +
            (if (hasDeferrableBenefit) 4 else 0) +
            (if (hasLambdaBenefit) 3 else 0) +
            (if (tinyPure) 2 else 0) -
            (if (summary.containsWhileExpr) 1 else 0) -
            (summary.bodyWeight / 20)

        score > 0
      }
    }
  }

  private def resolvesToLambda[A](expr: Expr[A]): Option[Lambda[A]] =
    Matchless.recoverTopLevelLambda(expr) match {
      case lam: Lambda[A] => Some(lam)
      case _              => None
    }

  private def isCheapArgument[A](expr: Expr[A]): Boolean =
    Matchless.isTriviallyCheap(expr) ||
      (!Matchless.hasSideEffect(expr) &&
        !Matchless.Expr.readsMutable(expr) &&
        !Matchless.Expr.containsWhileExpr(expr) &&
        (Matchless.exprWeight(expr) <= CheapArgWeightBudget))

  private def summarize[K](
      pack: PackageName,
      name: Bindable,
      expr: Expr[K]
  ): Option[InlineSummary[K]] =
    Matchless.recoverTopLevelLambda(expr) match {
      case lam: Lambda[K] =>
        val bodyWeight = Matchless.exprWeight(expr)
        if ((bodyWeight > InlineBodyWeightBudget) || lam.recursiveName.nonEmpty) None
        else {
          Some(
            InlineSummary(
              lambda = lam,
              bodyWeight = bodyWeight,
              paramDemand = Matchless.parameterDemandSummary(lam),
              containsWhileExpr = Matchless.Expr.containsWhileExpr(expr),
              exposesFreeMutableAnon = freeMutableAnonIds(expr).nonEmpty,
              alwaysInline = alwaysInline(pack, name)
            )
          )
        }
      case _ =>
        None
    }

  private def alwaysInline(
      pack: PackageName,
      name: Bindable
  ): Boolean =
    (pack == PackageName.PredefName) && (name match {
      case Identifier.Name("and") | Identifier.Name("or") => true
      case _                                              => false
    })

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
