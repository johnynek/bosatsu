package org.bykn.bosatsu

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
import Identifier.Bindable

object UnusedLetCheck {

  private[this] val ap = Applicative[Writer[Chain[(Bindable, Region)], *]]
  private[this] val empty: Writer[Chain[(Bindable, Region)], Set[Bindable]] =
    ap.pure(Set.empty)

  private[this] def checkArg(
      arg: Bindable,
      reg: => Region,
      w: Writer[Chain[(Bindable, Region)], Set[Bindable]]
  ) =
    w.flatMap { free =>
      if (free(arg)) ap.pure(free - arg)
      else {
        // this arg is free:
        ap.pure(free).tell(Chain.one((arg, reg)))
      }
    }

  private[this] def loop[A: HasRegion](
      e: Expr[A]
  ): Writer[Chain[(Bindable, Region)], Set[Bindable]] =
    e match {
      case Annotation(expr, _, _) =>
        loop(expr)
      case Generic(_, in) => loop(in)
      case Lambda(arg, _, expr, _) =>
        checkArg(arg, HasRegion.region(e), loop(expr))
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
            val bindRegion = wholeRegion.copy(end = endRegion.end)
            bindRegion
          },
          loop(in)
        )
        (exprRes, inCheck).mapN(_ ++ _)
      case Local(name, _) =>
        // this is a free variable:
        ap.pure(Set(name))
      case Global(_, _, _) | Literal(_, _) => empty
      case App(fn, arg, _) =>
        (loop(fn), loop(arg)).mapN(_ ++ _)
      case Match(arg, branches, _) =>
        val argCheck = loop(arg)
        // TODO: patterns need their own region
        val branchRegions =
          NonEmptyList.fromListUnsafe(
            branches.toList
              .scanLeft((HasRegion.region(arg), Option.empty[Region])) {
                case ((prev, _), (_, caseExpr)) =>
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
          .traverse { case (region, (pat, expr)) =>
            loop(expr).flatMap { frees =>
              val thisPatNames = pat.names
              val unused = thisPatNames.filterNot(frees)
              val nextFrees = frees -- thisPatNames

              ap.pure(nextFrees).tell(Chain.fromSeq(unused.map((_, region))))
            }
          }
          .map(_.combineAll)

        (argCheck, bcheck).mapN(_ ++ _)
    }

  /** Check for any unused lets, defs, or pattern bindings
    */
  def check[A: HasRegion](
      e: Expr[A]
  ): ValidatedNec[(Bindable, Region), Unit] = {
    val (chain, _) = loop(e).run
    NonEmptyChain.fromChain(chain) match {
      case None      => Validated.valid(())
      case Some(nec) => Validated.invalid(nec.distinct)
    }
  }

  /** Return the free Bindable names in this expression
    */
  def freeBound[A](e: Expr[A]): Set[Bindable] =
    loop(e)(HasRegion.instance(_ => Region(0, 0))).run._2
}
