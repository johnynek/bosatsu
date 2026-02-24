package dev.bosatsu

import cats.data.Validated
import cats.data.NonEmptyList
import IorMethods.IorExtension

class TypedExprRecursionCheckTest extends munit.FunSuite with ParTest {
  private val pack = PackageName.parts("TypedRecursionCheck")

  private def formatErrors(
      source: String,
      errs: cats.data.NonEmptyList[PackageError]
  ): String = {
    val sm = Map(pack -> (LocationMap(source), "<test>"))
    errs.toList
      .map(_.message(sm, LocationMap.Colorize.None))
      .mkString("\n-----\n")
  }

  private def recursionErrorsOf(
      source: String
  ): Either[cats.data.NonEmptyList[PackageError], Unit] = {
    val stmts = TestUtils.statementsOf(source)
    val parsed = Package.fromStatements(pack, stmts)
    given cats.Show[String] = cats.Show.fromToString
    PackageMap
      .typeCheckParsed(
        NonEmptyList.one((("<generated>", LocationMap(source)), parsed)),
        Nil,
        "<predef>",
        CompileOptions.Default
      )
      .strictToValidated match {
      case Validated.Valid(_) =>
        Right(())
      case Validated.Invalid(errs) =>
        Left(errs)
    }
  }

  private def allowed(source: String): Unit =
    recursionErrorsOf(source) match {
      case Right(_) =>
        ()
      case Left(errs) =>
        fail(s"expected success, got errors:\n${formatErrors(source, errs)}")
    }

  private def disallowed(source: String): Unit =
    recursionErrorsOf(source) match {
      case Right(_) =>
        fail("expected recursion-check failure")
      case Left(errs) =>
        val recursionErrs = errs.toList.collect { case r: PackageError.RecursionError =>
          r
        }
        if (recursionErrs.nonEmpty) ()
        else fail(s"expected recursion error, got:\n${formatErrors(source, errs)}")
    }

  test("substructural recursion remains allowed in typed checker") {
    allowed("""#
def len(lst):
  recur lst:
    case []: 0
    case [_, *tail]: len(tail)
""")
  }

  test("recur with no recursive call is rejected") {
    disallowed("""#
def fn(x):
  recur x:
    case y: 0
""")
  }

  test("tuple recur targets allow lexicographic decrease") {
    allowed("""#
enum Nat: Zero, Succ(prev: Nat)

def ack(n, m):
  recur (n, m):
    case (Zero, _): Succ(m)
    case (Succ(n_prev), Zero): ack(n_prev, Succ(Zero))
    case (Succ(n_prev), Succ(m_prev)): ack(n_prev, ack(n, m_prev))
""")
  }

  test("tuple recur targets reject non-lexicographic recursion") {
    disallowed("""#
enum Nat: Zero, Succ(prev: Nat)

def bad(n, m):
  recur (n, m):
    case (Succ(n_prev), _): bad(n_prev, Succ(m))
    case (Zero, Succ(m_prev)): bad(Succ(Zero), m_prev)
    case (Zero, Zero): Zero
""")
  }

  test("recur target must be argument name or tuple of names") {
    disallowed("""#
def invalid_target(x, y):
  recur (x, 1):
    case (_, _): invalid_target(x, y)
""")
  }

  test("recur target tuple cannot contain duplicates") {
    disallowed("""#
def dup(x, y):
  recur (x, x):
    case (_, _): dup(x, y)
""")
  }

  test("recursive calls in guards follow same legality rules") {
    disallowed("""#
def len(lst):
  recur lst:
    case []: 0
    case [_, *tail] if len(lst) matches 0: 1
    case [_, *_]: 2
""")
  }

  test("recur inside def with wildcard argument patterns remains valid") {
    allowed("""#
enum Thing:
  Thing1
  Thing2(a: Int, t: Thing)

def bar(y, _: String, x):
  recur x:
    case Thing1: y
    case Thing2(i, t): bar(i, "boom", t)

main = bar(1, "", Thing1)
""")
  }

  test("passing recursive function value through reachable continuation is allowed") {
    allowed("""#
enum Cont[a: *]:
  Item(a: a)
  Next(use: (Cont[a] -> a) -> a)

def loop[a](box: Cont[a]) -> a:
  recur box:
    case Item(a): a
    case Next(cont_fn): cont_fn(loop)

main: Int = loop(Item(1))
""")
  }
}
