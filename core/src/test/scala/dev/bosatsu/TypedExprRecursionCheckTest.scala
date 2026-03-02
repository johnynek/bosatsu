package dev.bosatsu

import cats.Show
import cats.data.{NonEmptyList, Validated}
import IorMethods.IorExtension

class TypedExprRecursionCheckTest extends munit.FunSuite with ParTest {
  private val pack = PackageName.parts("TypedRecursionCheck")

  private def formatErrors(
      source: String,
      errs: NonEmptyList[PackageError]
  ): String = {
    val sm = Map(pack -> (LocationMap(source), "<test>"))
    errs.toList
      .map(_.message(sm, LocationMap.Colorize.None))
      .mkString("\n-----\n")
  }

  private def recursionErrorsOf(
      source: String
  ): Either[NonEmptyList[PackageError], Unit] = {
    val stmts = TestUtils.statementsOf(source)
    val parsed = Package.fromStatements(pack, stmts)
    given Show[String] = Show.fromToString
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

  private def typedLetsOf(
      source: String
  ): (
      rankn.TypeEnv[Kind.Arg],
      List[(Identifier.Bindable, RecursionKind, TypedExpr[Declaration])],
      List[Statement]
  ) = {
    val stmts = TestUtils.statementsOf(source)
    val parsed = Package.fromStatements(pack, stmts)
    given Show[String] = Show.fromToString
    PackageMap
      .typeCheckParsed(
        NonEmptyList.one((("<generated>", LocationMap(source)), parsed)),
        Nil,
        "<predef>",
        CompileOptions.Default
      ) match {
      case cats.data.Ior.Left(errs) =>
        fail(s"failed to infer test source:\n${formatErrors(source, errs)}")
      case cats.data.Ior.Right(inferred) =>
        inferred.toMap.get(pack) match {
          case Some(pkg) =>
            val (prog, _) = pkg.program
            (prog.types, prog.lets, stmts)
          case None      =>
            fail("internal test error: inferred package missing")
        }
      case cats.data.Ior.Both(errs, inferred) =>
        fail(s"unexpected warnings/errors while inferring test source:\n${formatErrors(source, errs)}")
        inferred.toMap.get(pack) match {
          case Some(pkg) =>
            val (prog, _) = pkg.program
            (prog.types, prog.lets, stmts)
          case None      =>
            fail("internal test error: inferred package missing")
        }
    }
  }

  test("substructural recursion remains allowed in typed checker") {
    allowed("""#
def len(lst):
  recur lst:
    case []: 0
    case [_, *tail]: len(tail)
""")
  }

  test("loop accepts tail-recursive structural recursion") {
    allowed("""#
enum Nat: Zero, Succ(prev: Nat)

def len(lst, acc):
  loop lst:
    case []: acc
    case [_, *tail]: len(tail, Succ(acc))
""")
  }

  test("loop rejects non-tail recursive branch results") {
    disallowed("""#
enum Nat: Zero, Succ(prev: Nat)

def len(lst):
  loop lst:
    case []: Zero
    case [_, *tail]: Succ(len(tail))
""")
  }

  test("recur continues to allow non-tail recursive branch results") {
    allowed("""#
enum Nat: Zero, Succ(prev: Nat)

def len(lst):
  recur lst:
    case []: Zero
    case [_, *tail]: Succ(len(tail))
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

  test("tuple loop targets still allow lexicographic decrease") {
    allowed("""#
enum Nat: Zero, Succ(prev: Nat)

def down2(n, m):
  loop (n, m):
    case (Zero, _): m
    case (Succ(n_prev), Zero): down2(n_prev, Zero)
    case (Succ(_), Succ(m_prev)): down2(n, m_prev)
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

  test("loop rejects recursive calls in guards even when decreasing") {
    disallowed("""#
def len(lst):
  loop lst:
    case []: 0
    case [_, *tail] if len(tail) matches 0: 1
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

  test("loop rejects passing recursive function values as non-tail calls") {
    disallowed("""#
enum Cont[a: *]:
  Item(a: a)
  Next(use: (Cont[a] -> a) -> a)

def loop[a](box: Cont[a]) -> a:
  loop box:
    case Item(a): a
    case Next(cont_fn): cont_fn(loop)

main: Int = loop(Item(1))
""")
  }

  test("def loop remains a legal function name") {
    allowed("""#
def loop(x): x
main = loop(1)
""")
  }

  test("moderately large list literals do not overflow recursion checker stack") {
    val n = 211
    val items = List.fill(n)("\"x\"").mkString(", ")
    val source = s"""#
vals: List[String] = [$items]
main = vals
"""

    val (fullTypeEnv, lets, stmts) = typedLetsOf(source)
    val topLevelDefs = TypedExprRecursionCheck.topLevelDefArgs(stmts)
    var failure: Option[Throwable] = None
    var result: Option[TypedExprRecursionCheck.Res[Unit]] = None

    val thread = new Thread(
      null,
      new Runnable {
        def run(): Unit =
          try {
            result = Some(
              TypedExprRecursionCheck.checkLets(
                pack,
                fullTypeEnv,
                lets,
                topLevelDefs
              )
            )
          } catch {
            case t: Throwable =>
              failure = Some(t)
          }
      },
      "typed-recursion-check-small-stack",
      96L * 1024L
    )

    thread.start()
    thread.join()

    failure match {
      case Some(_: StackOverflowError) =>
        fail("recursion checker overflowed on a moderately large list literal")
      case Some(other) =>
        throw other
      case None =>
        result match {
          case Some(Validated.Valid(_)) => ()
          case Some(Validated.Invalid(errs)) =>
            fail(
              s"expected recursion checker success, got:\n${errs.iterator.mkString("\n")}"
            )
          case None =>
            fail("recursion checker thread did not produce a result")
        }
    }
  }
}
