package dev.bosatsu

import cats.Applicative
import cats.data.{
  Chain,
  NonEmptyList,
  Validated,
  ValidatedNec,
  Writer,
  NonEmptyChain
}
import cats.implicits._

import Expr._
import Identifier.{Bindable, Constructor}

object UnusedLetCheck {

  enum UnusedKind derives CanEqual {
    case Standard
    case MatchesPatternBinding
  }

  case class UnusedBinding(
      name: Bindable,
      region: Region,
      kind: UnusedKind
  ) derives CanEqual

  private type WriterChain[A] = Writer[Chain[UnusedBinding], A]
  private val ap = Applicative[WriterChain]
  private val empty: WriterChain[Set[Bindable]] =
    ap.pure(Set.empty)

  private def checkArg(
      arg: Bindable,
      reg: => Region,
      w: WriterChain[Set[Bindable]]
  ) =
    w.flatMap { free =>
      if (free(arg)) ap.pure(free - arg)
      else {
        // this arg is free:
        ap.pure(free).tell(Chain.one(UnusedBinding(arg, reg, UnusedKind.Standard)))
      }
    }

  private def isLoweredMatchesExpr[A: HasRegion](
      matchExpr: Expr.Match[A]
  ): Boolean = {
    def isBool(
        expr: Expr[A],
        cons: Constructor,
        matchRegion: Region
    ): Boolean =
      expr match {
        case Expr.Global(PackageName.PredefName, c: Constructor, _)
            if (c == cons) && HasRegion.region(expr.tag).eqv(matchRegion) =>
          true
        case _ =>
          false
      }

    val matchRegion = HasRegion.region(matchExpr.tag)

    matchExpr.branches.toList match {
      case first :: second :: Nil =>
        first.guard.isEmpty &&
        second.guard.isEmpty &&
        (second.pattern == Pattern.WildCard) &&
        isBool(first.expr, Constructor("True"), matchRegion) &&
        isBool(second.expr, Constructor("False"), matchRegion)
      case _ =>
        false
    }
  }

  private def loop[A: HasRegion](
      e: Expr[A]
  ): WriterChain[Set[Bindable]] =
    e match {
      case Annotation(expr, _, _) =>
        loop(expr)
      case Generic(_, in)        => loop(in)
      case Lambda(args, expr, _) =>
        args.toList.foldRight(loop(expr)) { (arg, res) =>
          checkArg(arg._1, HasRegion.region(e), res)
        }
      case Let(arg, expr, in, rec, _) =>
        val exprCheck = loop(expr)
        val exprRes =
          // if it is recursive, it is definitely used, because
          // that is automatically applied in source conversions
          if (rec.isRecursive) exprCheck.map(_ - arg) else exprCheck
        // the region of the let isn't directly tracked, but
        // it would start with the whole region starts and end at expr
        val inCheck = checkArg(
          arg, {
            val wholeRegion = HasRegion.region(e)
            val endRegion = HasRegion.region(expr)
            val bindRegion = wholeRegion.withEnd(endRegion.end)
            bindRegion
          },
          loop(in)
        )
        (exprRes, inCheck).mapN(_ ++ _)
      case Local(name, _) =>
        // this is a free variable:
        ap.pure(Set(name))
      case Global(_, _, _) | Literal(_, _) => empty
      case App(fn, args, _)                =>
        (loop(fn), args.traverse(loop(_))).mapN(_ ++ _.reduce)
      case m @ Match(arg, branches, _) =>
        val argCheck = loop(arg)
        val isLoweredMatches = isLoweredMatchesExpr(m)
        // TODO: patterns need their own region (https://github.com/johnynek/bosatsu/issues/132)
        val branchRegions =
          NonEmptyList.fromListUnsafe(
            branches.toList
              .scanLeft((HasRegion.region(arg), Option.empty[Region])) {
                case ((prev, _), branch) =>
                  val caseExpr = branch.expr
                  // between the previous expression and the case is the pattern
                  (
                    HasRegion.region(caseExpr),
                    Some(Region(prev.end, HasRegion.region(caseExpr).start))
                  )
              }
              .collect { case (_, Some(r)) => r }
          )
        val bcheck = branchRegions
          .zip(branches)
          .toList
          .zipWithIndex
          .traverse { case ((region, branch), idx) =>
            (
              branch.guard.traverse(loop).map(_.getOrElse(Set.empty[Bindable])),
              loop(branch.expr)
            ).mapN(_ ++ _).flatMap { frees =>
              val thisPatNames = branch.pattern.names
              val unused = thisPatNames.filterNot(frees)
              val nextFrees = frees -- thisPatNames
              val kind =
                if (isLoweredMatches && idx == 0)
                  UnusedKind.MatchesPatternBinding
                else UnusedKind.Standard
              val errs = unused.map(UnusedBinding(_, region, kind))

              ap.pure(nextFrees).tell(Chain.fromSeq(errs))
            }
          }
          .map(_.combineAll)

        (argCheck, bcheck).mapN(_ ++ _)
    }

  /** Check for any unused lets, defs, or pattern bindings
    */
  def check[A: HasRegion](
      e: Expr[A]
  ): ValidatedNec[UnusedBinding, Unit] = {
    val (chain, _) = loop(e).run
    val filtered = chain.filterNot(u => Identifier.isSynthetic(u.name))
    NonEmptyList.fromList(filtered.toList.distinct) match {
      case None      => Validated.valid(())
      case Some(nel) => Validated.invalid(NonEmptyChain.fromNonEmptyList(nel))
    }
  }

  /** Return the free Bindable names in this expression
    */
  def freeBound[A](e: Expr[A]): Set[Bindable] =
    loop(e)(using HasRegion.instance(_ => Region(0, 0))).run._2
}
