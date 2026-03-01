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
import Identifier.Bindable

object UnusedLetCheck {

  private type WriterChain[A] = Writer[Chain[(Bindable, Region)], A]
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
        ap.pure(free).tell(Chain.one((arg, reg)))
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
      case Match(arg, branches, _) =>
        val argCheck = loop(arg)
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
          .traverse { case (region, branch) =>
            (
              branch.guard.traverse(loop).map(_.getOrElse(Set.empty[Bindable])),
              loop(branch.expr)
            ).mapN(_ ++ _).flatMap { frees =>
              val thisPatNames = branch.pattern.names
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
    val filtered = chain.filterNot { case (b, _) => Identifier.isSynthetic(b) }
    NonEmptyChain.fromChain(filtered) match {
      case None      => Validated.valid(())
      case Some(nec) => Validated.invalid(nec.distinct)
    }
  }

  /** Return the free Bindable names in this expression
    */
  def freeBound[A](e: Expr[A]): Set[Bindable] =
    loop(e)(using HasRegion.instance(_ => Region(0, 0))).run._2
}
