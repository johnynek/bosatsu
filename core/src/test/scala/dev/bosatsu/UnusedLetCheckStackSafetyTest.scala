package dev.bosatsu

import cats.data.{Validated, ValidatedNec}

import Expr._
import Identifier.Bindable

class UnusedLetCheckStackSafetyTest extends munit.FunSuite {

  private val region: Region = Region.empty
  private given HasRegion[Region] = HasRegion.instance(identity)

  private def deepUnusedLetChain(depth: Int): Expr[Region] = {
    val base: Expr[Region] = Literal(Lit.Integer(0L), region)
    (0 until depth).foldRight(base) { (idx, in) =>
      val name: Bindable = Identifier.Name(s"x$idx")
      val rhs: Expr[Region] = Literal(Lit.Integer(idx.toLong), region)
      Let(name, rhs, in, RecursionKind.NonRecursive, region)
    }
  }

  private def warmUnusedLetCheck(): Unit =
    UnusedLetCheck.check(deepUnusedLetChain(4)) match {
      case Validated.Invalid(errs) =>
        assertEquals(errs.length.toInt, 4)
      case Validated.Valid(_) =>
        fail("expected warm-up run to report unused-let errors")
    }

  Platform.onJvm(
    test("UnusedLetCheck.check should not overflow on deep let chains") {
      val depth = sys.props.get("repro.unusedLetDepth").fold(3200)(_.toInt)
      val stackBytes =
        sys.props.get("repro.stackBytes").fold(96L * 1024L)(_.toLong)
      warmUnusedLetCheck()
      val expr = deepUnusedLetChain(depth)

      @volatile var failure: Option[Throwable] = None
      @volatile var result: Option[ValidatedNec[(Bindable, Region), Unit]] =
        None

      val thread = new Thread(
        null,
        new Runnable {
          def run(): Unit =
            try {
              result = Some(UnusedLetCheck.check(expr))
            } catch {
              case t: Throwable =>
                failure = Some(t)
            }
        },
        "unused-let-check-small-stack",
        stackBytes
      )

      thread.start()
      thread.join()

      failure match {
        case Some(_: StackOverflowError) =>
          fail(
            s"UnusedLetCheck.check overflowed on a deep let chain (depth=$depth, stackBytes=$stackBytes)"
          )
        case Some(other) =>
          val trace =
            other.getStackTrace.iterator.take(40).mkString("\n")
          fail(s"unexpected failure: $other\n$trace")
        case None =>
          result match {
            case Some(Validated.Invalid(errs)) =>
              assertEquals(errs.length.toInt, depth)
            case Some(Validated.Valid(_)) =>
              fail("expected unused-let errors for every binding in the chain")
            case None =>
              fail("UnusedLetCheck thread did not produce a result")
          }
      }
    }
  )
}
