package dev.bosatsu.rankn

import cats.data.{Chain, NonEmptyChain}
import cats.syntax.all._
import dev.bosatsu.{Expr, HasRegion, Identifier, PackageName, RecursionKind}
import dev.bosatsu.Identifier.Bindable

object NameCheck {
  type Let[A] = (Bindable, RecursionKind, Expr[A])

  final case class Result[A](
      typecheckLets: List[Let[A]],
      nameErrorLets: Set[Bindable],
      blockedLets: Set[Bindable],
      samePackageDeps: Map[Bindable, Set[Bindable]]
  )

  private def inferErrorScope(
      globalScope: Map[Infer.Name, Type],
      localScope: Set[Bindable]
  ): Map[Infer.Name, Type] =
    if (localScope.isEmpty) globalScope
    else {
      globalScope ++ localScope.iterator.map { b =>
        ((Option.empty[PackageName], b: Identifier), Type.UnitType)
      }
    }

  private def collectMissingNames[A: HasRegion](
      expr: Expr[A],
      globalScope: Map[Infer.Name, Type],
      localScope: Set[Bindable]
  ): Chain[Infer.Error.NameError] =
    expr match {
      case Expr.Annotation(inner, _, _) =>
        collectMissingNames(inner, globalScope, localScope)
      case Expr.Local(name, tag)        =>
        if (localScope(name)) Chain.empty
        else {
          Chain.one(
            Infer.Error.VarNotInScope(
              (None, name),
              inferErrorScope(globalScope, localScope),
              HasRegion.region(tag)
            )
          )
        }
      case Expr.Generic(_, inner)       =>
        collectMissingNames(inner, globalScope, localScope)
      case Expr.Global(pack, name, tag) =>
        val inferredName = (Some(pack), name)
        if (globalScope.contains(inferredName)) Chain.empty
        else {
          Chain.one(
            Infer.Error.VarNotInScope(
              inferredName,
              inferErrorScope(globalScope, localScope),
              HasRegion.region(tag)
            )
          )
        }
      case Expr.App(fn, args, _)        =>
        collectMissingNames(fn, globalScope, localScope) ++
          args.foldMap(collectMissingNames(_, globalScope, localScope))
      case Expr.Lambda(args, inner, _)  =>
        val withArgs = localScope ++ args.toList.iterator.map(_._1)
        collectMissingNames(inner, globalScope, withArgs)
      case Expr.Let(arg, bound, in, rec, _) =>
        val boundScope =
          if (rec.isRecursive) localScope + arg
          else localScope
        collectMissingNames(bound, globalScope, boundScope) ++
          collectMissingNames(in, globalScope, localScope + arg)
      case Expr.Literal(_, _) =>
        Chain.empty
      case Expr.Match(arg, branches, _) =>
        val argErrors = collectMissingNames(arg, globalScope, localScope)
        val branchErrors =
          branches.toList.foldMap { branch =>
            val withPat = localScope ++ branch.pattern.names
            val guardErrors =
              branch.guard.fold(Chain.empty[Infer.Error.NameError]) { guard =>
                collectMissingNames(guard, globalScope, withPat)
              }
            guardErrors ++ collectMissingNames(branch.expr, globalScope, withPat)
          }
        argErrors ++ branchErrors
    }

  private def samePackageDepsOf[A](
      pack: PackageName,
      expr: Expr[A]
  ): Set[Bindable] =
    expr.globals.iterator.collect {
      case Expr.Global(`pack`, n, _) =>
        n.toBindable
    }.flatten.toSet

  def checkLets[A: HasRegion](
      pack: PackageName,
      lets: List[Let[A]],
      initialScope: Map[Infer.Name, Type]
  ): (Option[NonEmptyChain[Infer.Error.NameError]], Result[A]) = {
    case class CheckedLet(let: Let[A], errors: Chain[Infer.Error.NameError])

    val checkedLets = {
      val (_, rev) =
        lets.foldLeft(
          (initialScope, List.empty[CheckedLet])
        ) { case ((topScope, acc), let @ (name, rec, expr)) =>
          val selfName = (Some(pack), name: Identifier)
          val scopeForExpr =
            if (rec.isRecursive) {
              topScope.updatedWith(selfName) {
                case some @ Some(_) => some
                case None           => Some(Type.UnitType)
              }
            } else topScope

          val topLevelLocalScope =
            if (rec.isRecursive) Set(name)
            else Set.empty[Bindable]
          val errors =
            collectMissingNames(expr, scopeForExpr, topLevelLocalScope)
          val nextTopScope =
            topScope.updatedWith(selfName) {
              case some @ Some(_) => some
              case None           => Some(Type.UnitType)
            }

          (nextTopScope, CheckedLet(let, errors) :: acc)
        }
      rev.reverse
    }

    val nameErrorLets: Set[Bindable] =
      checkedLets.collect {
        case CheckedLet((name, _, _), errs) if errs.nonEmpty =>
          name
      }.toSet

    val samePackageDeps: Map[Bindable, Set[Bindable]] =
      lets.iterator.map { case (name, _, expr) =>
        (name, samePackageDepsOf(pack, expr))
      }.toMap

    val dependents: Map[Bindable, Set[Bindable]] =
      samePackageDeps.iterator
        .flatMap { case (dependent, deps) =>
          deps.iterator.map((_, dependent))
        }
        .toList
        .groupBy(_._1)
        .iterator
        .map { case (dependency, pairs) =>
          (dependency, pairs.iterator.map(_._2).toSet)
        }
        .toMap

    val blockedLets: Set[Bindable] = {
      val blocked = scala.collection.mutable.Set.empty[Bindable]
      val queue = scala.collection.mutable.Queue.empty[Bindable]

      nameErrorLets.foreach { root =>
        blocked += root
        queue.enqueue(root)
      }

      // Block all transitive dependents so we only typecheck independent lets.
      while (queue.nonEmpty) {
        val next = queue.dequeue()
        dependents.getOrElse(next, Set.empty).foreach { dependent =>
          if (!blocked(dependent)) {
            blocked += dependent
            queue.enqueue(dependent)
          }
        }
      }

      blocked.toSet
    }

    val result = Result(
      typecheckLets = lets.filterNot { case (name, _, _) => blockedLets(name) },
      nameErrorLets = nameErrorLets,
      blockedLets = blockedLets,
      samePackageDeps = samePackageDeps
    )

    val allErrors =
      checkedLets.foldMap(_.errors)

    (NonEmptyChain.fromChain(allErrors), result)
  }
}
