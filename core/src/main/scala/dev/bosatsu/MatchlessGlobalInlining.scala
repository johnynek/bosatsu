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
      exposesFreeMutableAnon: Boolean
  ) {
    def arity: Int = lambda.arity
  }

  private val InlineBodyWeightBudget = 80
  private val TinyBodyWeightBudget = 20
  private val CheapArgWeightBudget = 3
  private val MaxExpensiveArgUses = 1

  private def builtinLambda[A](
      pack: PackageName,
      name: Bindable
  ): Option[Lambda[A]] =
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

  private def builtinSummary[K](
      pack: PackageName,
      name: Bindable
  ): Option[InlineSummary[K]] =
    builtinLambda[K](pack, name).map { lam =>
      InlineSummary(
        lambda = lam,
        bodyWeight = Matchless.exprWeight(lam),
        paramDemand = Matchless.parameterDemandSummary(lam),
        containsWhileExpr = false,
        exposesFreeMutableAnon = false
      )
    }

  private def cleanedCompiled[K: Order](
      rawCompiled: SortedMap[K, MatchlessFromTypedExpr.Compiled[K]]
  )(implicit ec: Par.EC): SortedMap[K, MatchlessFromTypedExpr.Compiled[K]] = {
    val scopeTasks = rawCompiled.iterator.toList.map { case (scope, packs) =>
      Par.start {
        val packTasks = packs.toList.map { case (pack, lets) =>
          Par.start {
            pack -> lets.map { case (name, expr) =>
              (name, Matchless.postLoweringCleanup(expr))
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
      depFor: (K, PackageName) => K
  )(implicit ec: Par.EC): SortedMap[K, MatchlessFromTypedExpr.Compiled[K]] = {
    val cleanedBase = cleanedCompiled(rawCompiled)

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
            rewritePackage(scope, pack, lets, summaryAcc, depFor)
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
      depFor: (K, PackageName) => K
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
                rewriteExpr(expr0, available, depFor)
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
    def loop(ex: Expr[K]): Expr[K] =
      ex match {
        case Matchless.Lambda(captures, recursiveName, args, body) =>
          Matchless.Lambda(captures.map(loop), recursiveName, args, loop(body))
        case Matchless.WhileExpr(cond, effectExpr, result) =>
          Matchless.WhileExpr(loopBool(cond), loop(effectExpr), result)
        case Matchless.App(fn, args) =>
          val fn1 =
            fn match {
              case global: Matchless.Global[?] =>
                global.asInstanceOf[Matchless.Expr[K]]
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
          val key = SummaryKey(depFor(from, pack), pack, name)
          summaries
            .get(key)
            .orElse(builtinSummary(pack, name))
            .filter(shouldInlineReference)
            .fold(global: Expr[K])(summary => loop(summary.lambda))
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
          val resolvedScope = depFor(from, pack)
          val key = SummaryKey(resolvedScope, pack, name)
          summaries.get(key).orElse(builtinSummary(pack, name)).filter(summary =>
            shouldInline(summary, args)
          ).map { summary =>
            loop(Matchless.inlineApplyArgs(summary.lambda, args))
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
        val cheapCallBenefit =
          argList.forall(isCheapArgument) &&
            (summary.bodyWeight <= TinyBodyWeightBudget)
        val tinyPure =
          (summary.bodyWeight <= TinyBodyWeightBudget) && !summary.containsWhileExpr
        val score =
          (if (hasUnusedBenefit) 4 else 0) +
            (if (hasDeferrableBenefit) 4 else 0) +
            (if (hasLambdaBenefit) 3 else 0) +
            (if (cheapCallBenefit) 2 else 0) +
            (if (tinyPure) 2 else 0) -
            (if (summary.containsWhileExpr) 1 else 0) -
            (summary.bodyWeight / 20)

        score > 0
      }
    }
  }

  private def shouldInlineReference[K](summary: InlineSummary[K]): Boolean =
    summary.lambda.captures.isEmpty &&
      !summary.containsWhileExpr &&
      (summary.bodyWeight <= TinyBodyWeightBudget)

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
              exposesFreeMutableAnon = freeMutableAnonIds(expr).nonEmpty
            )
          )
        }
      case _ =>
        None
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
