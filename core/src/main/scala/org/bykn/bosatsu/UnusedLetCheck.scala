package org.bykn.bosatsu

import cats.Applicative
import cats.data.{Chain, Validated, ValidatedNec, Writer, NonEmptyChain}
import cats.implicits._

import Expr._
import Identifier.Bindable

object UnusedLetCheck {

  private[this] val ap = Applicative[Writer[Chain[(Bindable, Region)], *]]
  private[this] val empty: Writer[Chain[(Bindable, Region)], Set[Bindable]] = ap.pure(Set.empty)

  private[this] def checkArg(arg: Bindable, reg: => Region, w: Writer[Chain[(Bindable, Region)], Set[Bindable]]) =
    w.flatMap { free =>
      if (free(arg)) ap.pure(free - arg)
      else {
        // this arg is free:
        ap.pure(free).tell(Chain.one((arg, reg)))
      }
    }

  private[this] def loop[A: HasRegion](e: Expr[A]): Writer[Chain[(Bindable, Region)], Set[Bindable]] =
    e match {
      case Annotation(expr, _, _) =>
        loop(expr)
      case AnnotatedLambda(arg, _, expr, _) =>
        checkArg(arg, HasRegion.region(e), loop(expr))
      case Lambda(arg, expr, _) =>
        checkArg(arg, HasRegion.region(e), loop(expr))
      case Let(arg, expr, in, rec, _) =>
        val exprCheck = loop(expr)
        val exprRes =
          // if it is recursive, it is definitely used, because
          // that is automatically applied in source conversions
          if (rec.isRecursive) exprCheck.map(_ - arg) else exprCheck
        val inCheck = checkArg(arg, HasRegion.region(e), loop(in))
        (exprRes, inCheck).mapN(_ ++ _)
      case Local(name, _) =>
        // this is a free variable:
        ap.pure(Set(name))
      case Global(_, _, _) | Literal(_, _) => empty
      case App(fn, arg, _) =>
        (loop(fn), loop(arg)).mapN(_ ++ _)
      case Match(arg, branches, _) =>
        // TODO: patterns need their own region
        val argCheck = loop(arg)
        val bcheck = branches.traverse { case (pat, expr) =>
          val region = HasRegion.region(expr)
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

  /**
   * Check for any unused lets, defs, or pattern bindings
   */
  def check[A: HasRegion](e: Expr[A]): ValidatedNec[(Bindable, Region), Unit] = {
    val (chain, _) = loop(e).run
    NonEmptyChain.fromChain(chain) match {
      case None => Validated.valid(())
      case Some(nec) => Validated.invalid(nec.distinct)
    }
  }

  /**
   * Return the free Bindable names in this expression
   */
  def freeBound[A](e: Expr[A]): Set[Bindable] =
    loop(e)(HasRegion.instance(_ => Region(0, 0))).run._2
}
