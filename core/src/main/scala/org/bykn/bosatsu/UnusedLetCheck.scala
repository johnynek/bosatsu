package org.bykn.bosatsu

import cats.Applicative
import cats.data.{Chain, Validated, ValidatedNec, Writer, NonEmptyChain}
import cats.implicits._

import Identifier.Bindable

object UnusedLetCheck {
  def check[A: HasRegion](e: Expr[A]): ValidatedNec[(Bindable, Region), Unit] = {
    import Expr._

    val ap = Applicative[Writer[Chain[(Bindable, Region)], ?]]
    val empty: Writer[Chain[(Bindable, Region)], Set[Bindable]] = ap.pure(Set.empty)

    def loop(e: Expr[A]): Writer[Chain[(Bindable, Region)], Set[Bindable]] =
      e match {
        case Annotation(expr, _, _) =>
          loop(expr)
        case AnnotatedLambda(arg, _, expr, _) =>
          loop(expr).flatMap { free =>
            if (free(arg)) ap.pure(free - arg)
            else {
              // this arg is free:
              ap.pure(free).tell(Chain.one((arg, HasRegion.region(e))))
            }
          }
        case Lambda(arg, expr, _) =>
          loop(expr).flatMap { free =>
            if (free(arg)) ap.pure(free - arg)
            else {
              // this arg is free:
              ap.pure(free).tell(Chain.one((arg, HasRegion.region(e))))
            }
          }
        case Let(arg, expr, in, rec, _) =>
          val exprCheck = loop(expr)
          val inCheck = loop(in)
          // before typechecking, we make all defs recursive
          // TODO: be more precise earlier
          val exprRes = if (rec.isRecursive) exprCheck.map(_ - arg) else exprCheck
          val inRes = inCheck.flatMap { free =>
            if (free(arg)) ap.pure(free - arg)
            else {
              // this arg is free:
              // TODO we need a region on the arg
              ap.pure(free).tell(Chain.one((arg, HasRegion.region(e))))
            }
          }
          (exprRes, inRes).mapN(_ ++ _)
        case Var(None, name: Bindable, _) =>
          // this is a free variable:
          ap.pure(Set(name))
        case Var(_, _, _) | Literal(_, _) => empty
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

    val (chain, _) = loop(e).run
    NonEmptyChain.fromChain(chain) match {
      case None => Validated.valid(())
      case Some(nec) => Validated.invalid(nec.distinct)
    }
  }
}
