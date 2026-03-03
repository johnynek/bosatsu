package dev.bosatsu

import cats.Show
import cats.data.{NonEmptyList, Validated}
import IorMethods.IorExtension
import scala.util.Try

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

  private def disallowedWithMessage(source: String)(assertMessage: String => Unit): Unit =
    recursionErrorsOf(source) match {
      case Right(_) =>
        fail("expected recursion-check failure")
      case Left(errs) =>
        val recursionErrs = errs.toList.collect { case r: PackageError.RecursionError =>
          r
        }
        recursionErrs match {
          case Nil =>
            fail(s"expected recursion error, got:\n${formatErrors(source, errs)}")
          case _   =>
            assertMessage(recursionErrs.map(_.err.message).mkString("\n-----\n"))
        }
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

  test("recur supports Int targets with provable decrease and non-negative next values") {
    allowed("""#
def int_loop[a](i: Int, state: a, fn: (Int, a) -> (Int, a)) -> a:
  recur i:
    case _ if cmp_Int(i, 0) matches GT:
      (next_i, next_state) = fn(i, state)
      c = match cmp_Int(next_i, 0):
        case LT | EQ: False
        case _: True
      if c:
        cmp = cmp_Int(next_i, i)
        decreasing = match cmp:
          case LT | EQ: cmp matches LT
          case _: False
        if decreasing:
          int_loop(next_i, next_state, fn)
        else:
          next_state
      else:
        next_state
    case _:
      state
""")
  }

  test("tuple recur targets allow mixed structural and Int decreases") {
    allowed("""#
enum Nat: Zero, Succ(prev: Nat)

def mixed(n: Nat, i: Int) -> Int:
  recur (n, i):
    case (Zero, _):
      i
    case (Succ(prev), (1 | 2) as one_or_two):
      mixed(prev, one_or_two.sub(1))
    case (Succ(prev), i1):
      next_i = i1.sub(1)
      if cmp_Int(next_i, 0) matches GT:
        mixed(n, next_i)
      else:
        mixed(prev, 10)
""")
  }

  test("Int recursion allows i.sub(2) when guard proves i > 1") {
    allowed("""#
def ok(i: Int) -> Int:
  recur i:
    case _ if cmp_Int(i, 1) matches GT:
      ok(i.sub(2))
    case _:
      i
""")
  }

  test("loop Int recursion accepts equivalent add(-1) decrement") {
    allowed("""#
def demo(n: Int) -> Int:
  def go(rem: Int, acc: Int) -> Int:
    loop rem:
      case _ if cmp_Int(rem, 0) matches GT:
        go(rem.add(-1), acc.add(rem))
      case _:
        acc

  go(n, 0)
""")
  }

  test("loop Int recursion accepts infix decrement via top-level operator alias") {
    allowed("""#
def operator +(a, b): a.add(b)
def operator -(a, b): a.sub(b)
def operator >(a, b): cmp_Int(a, b) matches GT

def demo(n: Int) -> Int:
  def go(rem: Int, acc: Int) -> Int:
    loop rem:
      case _ if rem > 0:
        go(rem - 1, acc + rem)
      case _:
        acc

  go(n, 0)
""")
  }

  test("Int recursion rejects non-decreasing recursive calls") {
    disallowedWithMessage("""#
def bad(i: Int) -> Int:
  recur i:
    case _ if cmp_Int(i, 0) matches GT:
      bad(i)
    case _:
      i
""") { msg =>
      assert(clue(msg).contains("cannot prove Int recursion obligation for bad"))
      assert(clue(msg).contains("recur target: i"))
      assert(clue(msg).contains("path condition:"))
    }
  }

  test("Int recursion rejects calls that do not prove non-negative next values") {
    disallowedWithMessage("""#
def bad(i: Int) -> Int:
  recur i:
    case _ if cmp_Int(i, 0) matches GT:
      bad(i.sub(2))
    case _:
      i
""") { msg =>
      assert(clue(msg).contains("cannot prove Int recursion obligation for bad: (>= "))
      assert(clue(msg).contains("recur target: i"))
      assert(clue(msg).contains("path condition:"))
    }
  }

  test("Int recursion lowers bool matches to ite when computing next Int") {
    allowed("""#
def bool_if(i: Int) -> Int:
  recur i:
    case _ if cmp_Int(i, 0) matches GT:
      next_i = match cmp_Int(i, 1) matches GT:
        case True: i.sub(2)
        case False: i.sub(1)
      bool_if(next_i)
    case _:
      i
""")
  }

  test("Int recursion lowers comparison matches with let-bound guards") {
    allowed("""#
def cmp_guarded(i: Int) -> Int:
  recur i:
    case _ if (
      c = match cmp_Int(i, 2):
        case LT if cmp_Int(i, 0) matches GT: GT
        case LT: LT
        case EQ: GT
        case GT: LT
      c
    ) matches GT:
      next_i = match cmp_Int(i, 2):
        case LT: 0
        case EQ: 1
        case GT: i.sub(1)
      cmp_guarded(next_i)
    case _:
      i
""")
  }

  test("Int recursion reports unsupported SMT Int lowering in recursive args") {
    disallowedWithMessage("""#
def id(i: Int) -> Int:
  f = x -> x
  f(i)

def bad(i: Int) -> Int:
  recur i:
    case _ if cmp_Int(i, 0) matches GT:
      bad(id(i))
    case _:
      i
""") { msg =>
      assert(clue(msg).contains("unable to lower recursive argument"))
      assert(clue(msg).contains("recur target: i"))
      assert(clue(msg).contains("hint: rewrite recursive argument using canonical Int operations"))
    }
  }

  test("Int recursion rejects non-local Int expressions that are equal to current value") {
    disallowed("""#
def bad(i: Int) -> Int:
  recur i:
    case _ if cmp_Int(i, 0) matches GT:
      bad(i.add(0))
    case _:
      i
""")
  }

  test("Int recursion rejects increasing Int expressions immediately") {
    disallowed("""#
def bad(i: Int) -> Int:
  recur i:
    case _ if cmp_Int(i, 0) matches GT:
      bad(i.add(1))
    case _:
      i
""")
  }

  test("Int recursion lowering handles div/mod by zero semantics") {
    disallowed("""#
def bad(i: Int) -> Int:
  recur i:
    case _ if cmp_Int(i, 0) matches GT:
      bad(i.div(0).add(i.mod_Int(0)))
    case _:
      i
""")
  }

  test("Int recursion allows divide-and-conquer recursion on integer splits") {
    allowed("""#
def split_sum(i: Int) -> Int:
  recur i:
    case _ if cmp_Int(i, 1) matches GT:
      i1 = i.div(2)
      i2 = i.sub(i1)
      split_sum(i1).add(split_sum(i2))
    case _:
      i
""")
  }

  test("nested matches in recur branches contribute pattern and guard path facts") {
    allowed("""#
def via_match(i: Int) -> Int:
  recur i:
    case _ if cmp_Int(i, 0) matches GT:
      match cmp_Int(i, 2):
        case LT if eq_Int(i, 1):
          via_match(0)
        case EQ:
          via_match(1)
        case _:
          via_match(i.sub(1))
    case _:
      i
""")
  }

  test("loop branches inherit negated guarded fallthrough facts for Int recursion") {
    allowed("""#
def countdown(fuel, stack):
  loop (fuel, stack):
    case (_, []): fuel
    case (_, _) if cmp_Int(fuel, 0) matches LT | EQ: fuel
    case (_, [_, *tail]): countdown(fuel.sub(1), tail)
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

  test("Platform.onJvm only evaluates on JVM") {
    assertEquals(Try(Platform.onJvm(sys.error("boom"))).isFailure, Platform.isJvm)
  }

  Platform.onJvm(
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
  )
}
