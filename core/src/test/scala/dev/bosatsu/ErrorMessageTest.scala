package dev.bosatsu

import Value._

import cats.Show
import cats.data.Validated
import dev.bosatsu.LocationMap.Colorize
import scala.concurrent.duration.DurationInt

class ErrorMessageTest extends munit.FunSuite with ParTest {
  override val munitTimeout = 180.seconds

  import TestUtils._

  private def unusedLetMessage(source: String): String = {
    val parsed = Parser.parse(Package.parser(None), source) match {
      case Validated.Valid((lm, pack)) =>
        (("0", lm), pack) :: Nil
      case Validated.Invalid(errs) =>
        fail(s"parse failed: $errs")
    }

    val withPre = PackageMap.withPredefA(("predef", LocationMap("")), parsed)
    val withPrePaths = withPre.map { case ((path, _), p) => (path, p) }

    implicit val showString: Show[String] = Show.fromToString
    val errsOpt = PackageMap.resolveThenInfer(withPrePaths, Nil).left
    val errs = errsOpt.getOrElse(fail("expected unused let error"))
    val sourceMap = PackageMap.buildSourceMap(withPre)
    val msgOpt = errs.toList.collectFirst {
      case e @ PackageError.UnusedLets(_, _) =>
        e.message(sourceMap, Colorize.None)
      case e @ PackageError.UnusedLetError(_, _) =>
        e.message(sourceMap, Colorize.None)
    }
    msgOpt.getOrElse(fail(s"expected unused let error, found: $errs"))
  }

  test("unused top-level let points to the whole binding") {
    val source =
      """package A
        |export main
        |
        |x = 1
        |main = 2
        |""".stripMargin

    val message = unusedLetMessage(source)
    assert(message.contains("unused value 'x'"), message)
    assert(message.contains("x = 1"), message)
    assert(message.contains("How to resolve:"), message)
    assert(message.contains("add 'x' to exports"), message)
    assert(message.contains("rebind it as `_ = <expr>`"), message)
    assert(message.contains("use it from `tests`"), message)
    assert(message.contains("final value of type Bosatsu::Test"), message)
    assert(message.contains("use it from `main`"), message)
    assert(message.contains("final non-test value"), message)

    val pointerLines = message.linesIterator.filter(_.contains("^")).toList
    assertEquals(pointerLines.length, 1, message)

    val pointerWidth = pointerLines.head.count(_ == '^')
    assert(pointerWidth >= 3, message)
  }

  test("multiple unused top-level values show a shared count and hint block") {
    val source =
      """package A
        |export main
        |
        |x = 1
        |y = 2
        |main = 3
        |""".stripMargin

    val message = unusedLetMessage(source)
    assert(message.contains("unused value 'x'"), message)
    assert(message.contains("unused value 'y'"), message)
    assert(message.contains("found 2 unused values."), message)
    assert(message.contains("How to resolve:"), message)
    assert(message.contains("add needed values to exports"), message)
    assertEquals(
      message.split("How to resolve:").length - 1,
      1,
      message
    )
  }

  test("test matching unions") {
    evalTest(
      List("""
package Foo

struct Pair(a, b)

x = Pair(Pair(1, "1"), "2")

main = match x:
  case Pair(_, "2" | "3"): "good"
  case _: "bad"
"""),
      "Foo",
      Str("good")
    )

    evalTest(
      List("""package Foo

enum Res: Err(a), Good(a)

x = Err('good')

def run(z):
  Err(y) | Good(y) = z
  y

main = run(x)
"""),
      "Foo",
      Str("good")
    )

    evalTest(
      List("""
package Foo

enum Res[a, b]: Err(a: a), Good(a:a, b: b)

x = Err("good")

def run(z):
  Err(y) | Good(y, _) = z
  y

main = run(x)
"""),
      "Foo",
      Str("good")
    )

    evalFail(List("""
package Err

enum IntOrString: IntCase(i: Int), StringCase(i: Int, s: String)

def go(x):
  # if we remove z, this is well typed, but an error nonetheless
  IntCase(y) | StringCase(y, z) = x
  y

main = go(IntCase(42))
""")) { case _: PackageError.TypeErrorIn => () }

    val errPack = """
package Err

enum IntOrString: IntCase(i: Int), StringCase(s: String)

def go(x):
  # this is illtyped
  IntCase(y) | StringCase(y) = x
  y

main = go(IntCase(42))
"""
    val packs =
      Map((PackageName.parts("Err"), (LocationMap(errPack), "Err.bosatsu")))
    evalFail(List(errPack)) { case te: PackageError.TypeErrorIn =>
      val msg = te.message(packs, Colorize.None)
      assert(
        msg.contains(
          "type error: expected type Int but found type String"
        )
      )
      ()
    }

    evalTest(
      List("""
package Union

enum IntOrString: IntCase(i: Int), StringCase(s: String)

def go(x):
  # this is a total match, and doesn't bind incompatible
  # types to the same name
  IntCase(_) | StringCase(_) = x
  42

main = go(IntCase(42))
"""),
      "Union",
      VInt(42)
    )
  }

  test("forbid the y-combinator") {
    evalFail(List("""
package Y

struct W(fn: W[a, b] -> a -> b)

def call(w0, w1):
  match w0:
    case W(fn): trace("fn(w1)", fn(w1))

def y(f):
  g = w -> a -> trace("calling f", f(call(w, w), a))
  g(W(g))

def ltEqZero(i):
  i.cmp_Int(0) matches (LT | EQ)

fac = trace("made fac", y((f, i) -> 1 if ltEqZero(i) else f(i).mul(i)))

main = fac(6)
""")) { case PackageError.KindInferenceError(_, _, _) =>
      ()
    }

    evalFail(List("""
package Y
struct W(wf: f[a, b] -> a -> b)

def apply(w):
  W(fn) = w
  fn(w)
""")) { case err: PackageError.TypeErrorIn =>
      val message = err.message(Map.empty, Colorize.None)
      assert(message.contains("illegal recursive type or function"))
      ()
    }
  }

  test(
    "guarded branches do not establish totality and unguarded fallback restores it"
  ) {
    evalFail(List("""
package Total

main = match True:
  case x if x matches True: 1
""")) { case te @ PackageError.TotalityCheckError(_, _) =>
      val msg = te.message(Map.empty, Colorize.None)
      assert(msg.contains("guarded branches do not count toward totality"))
      ()
    }

    evalTest(
      List("""
package Total

main = match True:
  case x if x matches True: 1
  case _: 0
"""),
      "Total",
      VInt(1)
    )

    evalFail(List("""
package Total

main = match True:
  case _: 1
  case True if (True matches True): 2
""")) { case PackageError.TotalityCheckError(_, _) => () }
  }

  test("unused let fails compilation") {
    evalFail(List("""
package A

# this shouldn't compile, z is unused
def plus(x, y):
  z = 1
  x.add(y)

main = plus(1, 2)
""")) { case le @ PackageError.UnusedLetError(_, _) =>
      val msg = le.message(Map.empty, Colorize.None)
      assert(!msg.contains("Name("))
      assert(msg.contains("unused value 'z'\n  Region(68,73)"))
      assert(
        msg.contains("if intentional, ignore it with `_`"),
        msg
      )
      ()
    }

    evalFail(List("""
package A

# this shouldn't compile, z is unused
z = 1

def plus(x, y):
  x.add(y)

main = plus(1, 2)
    """)) { case le @ PackageError.UnusedLets(_, _) =>
      val msg = le.message(Map.empty, Colorize.None)
      assert(!msg.contains("Name("))
      assert(msg.contains("unused value 'z'\n  Region("))
      assert(msg.contains("add 'z' to exports"))
      ()
    }
  }

  test("test some error messages") {
    evalFail(
      List(
        """
package A

a = 1
""",
        """
package B

from A import a

main = a"""
      )
    ) { case PackageError.UnknownImportName(_, _, _, _, _) => () }

    evalFail(List("""
package B

from A import a

main = a""")) { case PackageError.UnknownImportPackage(_, _) => () }

    evalFail(List("""
package B

main = a""")) { case te: PackageError.TypeErrorIn =>
      val msg = te.message(Map.empty, Colorize.None)
      assert(!msg.contains("Name("))
      assert(msg.contains("package B\nUnknown name `a`."))
      ()
    }

    evalFail(List("""
package A

x = missing_name
y = missing_name

main = 1
""")) { case te: PackageError.TypeErrorIn =>
      val msg = te.message(Map.empty, Colorize.None)
      assert(msg.contains("Unknown name `missing_name`."))
      assert(msg.contains("This unknown name appears 2 times."))
      ()
    }

    val useBeforeDefCode = """
package P

main = foo

foo = 1
"""
    val useBeforeDefMap =
      Map(
        (PackageName.parts("P"), (LocationMap(useBeforeDefCode), "P.bosatsu"))
      )

    evalFail(List(useBeforeDefCode)) { case te: PackageError.TypeErrorIn =>
      val msg = te.message(useBeforeDefMap, Colorize.None)
      assert(msg.contains("""name "foo" is used before it is defined."""))
      assert(
        msg.contains(
          "this use site uses foo, but foo is defined later in this file"
        )
      )
      assert(msg.contains("Use site:"))
      assert(msg.contains("main = foo"))
      assert(msg.contains("Definition site:"))
      assert(msg.contains("foo = 1"))
      ()
    }

    evalFail(List("""
package B

x = 1

main = match x:
  case Foo: 2
""")) { case te @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      val msg = te.message(Map.empty, Colorize.None)
      assert(!msg.contains("Name("))
      assert(msg.contains("package B\nUnknown constructor `Foo`."))
      ()
    }

    evalFail(List("""
package B

struct X

main = match 1:
  case X1: 0
""")) { case te @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      assertEquals(
        te.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package B\nUnknown constructor `X1`.\nDid you mean constructor `X`?\nRegion(49,50)"
      )
      ()
    }

    evalFail(List("""
package A

main = match [1, 2, 3]:
  case []: 0
  case [*a, *b, _]: 2
""")) { case te @ PackageError.TotalityCheckError(_, _) =>
      assertEquals(
        te.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package A\nRegion(19,70)\nmultiple splices in pattern, only one per match allowed"
      )
      ()
    }

    evalFail(List("""
package A

enum Foo: Bar(a), Baz(b)

main = match Bar(a):
  case Baz(b): b
""")) { case te @ PackageError.TotalityCheckError(_, _) =>
      assertEquals(
        te.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package A\nRegion(45,75)\nnon-total match, missing: Bar(_)"
      )
      ()
    }

    evalFail(List("""
package A

def fn(x):
  recur x:
    case y: 0

main = fn
""")) { case te @ PackageError.RecursionError(_, _) =>
      assertEquals(
        te.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package A\nrecur but no recursive call to fn\nRegion(25,47)\n"
      )
      ()
    }

    evalFail(List("""
package A

def parse_loopTypo(x):
  recur x:
    case 0: parse_loop(x)
    case _: parse_loop(x)

main = parse_loopTypo
""")) { case te @ PackageError.RecursionError(_, _) =>
      val msg = te.message(Map.empty, Colorize.None)
      assert(
        msg.contains(
          "Function name looks renamed: declared `parse_loopTypo`, but recursive calls use `parse_loop`."
        )
      )
      assert(
        msg.contains(
          "Did you mean `parse_loopTypo` in recursive calls? (2 occurrences)"
        )
      )
      ()
    }

    evalFail(List("""
package A

def fn(x):
  recur 10:
    case y: 0

main = fn
""")) { case te @ PackageError.RecursionError(_, _) =>
      assertEquals(
        te.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package A\nrecur not on an argument to the def of fn, args: (x)\nRegion(25,48)\n"
      )
      ()
    }

    evalFail(List("""
package A

def fn(x):
  recur y:
    case y: 0

main = fn
""")) { case te @ PackageError.RecursionError(_, _) =>
      assertEquals(
        te.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package A\nrecur not on an argument to the def of fn, args: (x)\nRegion(25,47)\n"
      )
      ()
    }

    evalFail(List("""
package A

def fn(x):
  recur x:
    case y:
      recur x:
        case z: 100

main = fn
""")) { case te @ PackageError.RecursionError(_, _) =>
      assertEquals(
        te.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package A\nunexpected recur: may only appear unnested inside a def\nRegion(52,80)\n"
      )
      ()
    }

    evalFail(List("""
package A

def fn(x):
  fn = 100
  recur x:
    case y:
      recur x:
        case z: 100

main = fn
""")) { case te @ PackageError.RecursionError(_, _) =>
      assertEquals(
        te.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package A\nillegal shadowing on: fn. Recursive shadowing of def names disallowed\nRegion(25,91)\n"
      )
      ()
    }

    evalFail(List("""
package A

def fn(x, y):
  match x:
    case 0: y
    case x: fn(x - 1, y + 1)

main = fn
""")) { case te @ PackageError.RecursionError(_, _) =>
      assertEquals(
        te.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package A\ninvalid recursion on fn. Consider replacing `match` with `recur`.\nRegion(63,79)\n"
      )
      ()
    }

    evalFail(List("""
package A

def fn(x, y):
  match x:
    case 0: y
    case x: x

main = fn(0, 1, 2)
""")) { case te: PackageError.TypeErrorIn =>
      assert(
        te.message(Map.empty, Colorize.None)
          .contains("does not match function with 3 arguments at:")
      )
      ()
    }

    // we should have the region set inside
    val code1571 = """
package A

def fn(x):
  recur x:
    case []: 0
    case [_, *y]: fn(y, 1)

main = fn([1, 2])
"""
    evalFail(code1571 :: Nil) { case te: PackageError.TypeErrorIn =>
      // Make sure we point at the function directly
      assertEquals(code1571.substring(67, 69), "fn")
      val msg = te.message(Map.empty, Colorize.None)
      assert(
        msg.contains(
          "the first type is a function with one argument and the second is a function with 2 arguments"
        ) ||
          msg.contains(
            "the first type is a function with 2 arguments and the second is a function with one argument"
          )
      )
      assert(
        msg.contains("Region(67,69)")
      )
      ()
    }

    evalFail(
      List(
        """
package A

export foo

foo = 3
""",
        """
package B
from A import fooz

baz = fooz
"""
      )
    ) { case te @ PackageError.UnknownImportName(_, _, _, _, _) =>
      assertEquals(
        te.message(
          Map.empty,
          Colorize.None
        ),
        "in <unknown source> package: A does not have name fooz. Nearest: foo"
      )
      ()
    }

    evalFail(
      List(
        """
package A

export foo

foo = 3
bar = 3
""",
        """
package B
from A import bar

baz = bar
"""
      )
    ) { case te @ PackageError.UnknownImportName(_, _, _, _, _) =>
      assertEquals(
        te.message(
          Map.empty,
          Colorize.None
        ),
        "in <unknown source> package: A has bar but it is not exported. Add to exports"
      )
      ()
    }

    val missingIfaceImport = PackageError.UnknownImportFromInterface(
      PackageName.parts("Bosatsu", "Prog"),
      PackageName.parts("Bosatsu", "Prog"),
      Nil,
      ImportedName.OriginalName(Identifier.Name("ignore_env"), ()),
      Nil
    )
    assertEquals(
      missingIfaceImport.message(Map.empty, Colorize.None),
      "in file: <unknown source>, package Bosatsu/Prog\ndoes not have name ignore_env."
    )
  }

  test("record patterns") {
    runBosatsuTest(
      List("""
package A

struct Pair(first, second)

f2 = match Pair(1, "1"):
  case Pair { first, ... }: first

tests = TestSuite("test record",
  [
    Assertion(f2.eq_Int(1), "f2 == 1"),
  ])
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""
package A

struct Pair(first, second)

res = (
  Pair { first, ... } = Pair(1, 2)
  first
)

tests = TestSuite("test record",
  [
    Assertion(res.eq_Int(1), "res == 1"),
  ])
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""
package A

struct Pair(first, second)

get = Pair { first, ...} -> first

res = get(Pair(1, "two"))

tests = TestSuite("test record",
  [
    Assertion(res.eq_Int(1), "res == 1"),
  ])
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""
package A

struct Pair(first, second)

get = Pair { first: f, ...} -> f

res = get(Pair(1, "two"))

tests = TestSuite("test record",
  [
    Assertion(res.eq_Int(1), "res == 1"),
  ])
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""
package A

struct Pair(first, second)

get = Pair(first, ...) -> first

res = get(Pair(1, "two"))

tests = TestSuite("test record",
  [
    Assertion(res.eq_Int(1), "res == 1"),
  ])
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""
package A

struct Pair(first, second)

get = Pair(first, ...) -> first

res = get(Pair { first: 1, second: "two" })

tests = TestSuite("test record",
  [
    Assertion(res.eq_Int(1), "res == 1"),
  ])
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""
package A

struct Pair(first, second)

get = Pair(first, ...) -> first

first = 1

res = get(Pair { first, second: "two" })

tests = TestSuite("test record",
  [
    Assertion(res.eq_Int(1), "res == 1"),
  ])
"""),
      "A",
      1
    )

    evalFail(List("""
package A

struct Pair(first, second)

get = Pair(first, ...) -> first

# missing second
first = 1
res = get(Pair { first })
""")) { case s @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      s.message(Map.empty, Colorize.None); ()
    }

    evalFail(List("""
package A

struct Pair(first, second)

get = Pair(first, ...) -> first

# third is unknown
first = 1
second = 3
res = get(Pair { first, second, third })
""")) { case s @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      s.message(Map.empty, Colorize.None); ()
    }

    evalFail(List("""
package A

struct Pair(first, second)

get = Pair { first } -> first

res = get(Pair(1, "two"))
""")) { case s @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      s.message(Map.empty, Colorize.None); ()
    }

    evalFail(List("""
package A

struct Pair(first, second)

# Pair has two fields
get = Pair(first) -> first

res = get(Pair(1, "two"))
""")) { case s @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      s.message(Map.empty, Colorize.None); ()
    }

    evalFail(List("""
package A

struct Pair(first, second)

# Pair does not have a field called sec
get = Pair { first, sec: _ } -> first

res = get(Pair(1, "two"))
""")) { case s @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      s.message(Map.empty, Colorize.None); ()
    }

    evalFail(List("""
package A

struct Pair(first, second)

# Pair does not have a field called sec
get = Pair { first, sec: _, ... } -> first

res = get(Pair(1, "two"))
""")) { case s @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      s.message(Map.empty, Colorize.None); ()
    }

    evalFail(List("""
package A

struct Pair(first, second)

# Pair has two fields, not three
get = Pair(first, _, _) -> first

res = get(Pair(1, "two"))
""")) { case s @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      s.message(Map.empty, Colorize.None); ()
    }

    evalFail(List("""
package A

struct Pair(first, second)

# Pair has two fields, not three
get = Pair(first, _, _, ...) -> first

res = get(Pair(1, "two"))
""")) { case s @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      s.message(Map.empty, Colorize.None); ()
    }

    runBosatsuTest(
      List("""
package A

struct Pair(first, second)

get = Pair(first, _, ...) -> first

res = get(Pair(1, "two"))

tests = TestSuite("test record",
  [
    Assertion(res.eq_Int(1), "res == 1"),
  ])
"""),
      "A",
      1
    )

    evalFail(List("""
package A

struct Pair(first, second)

main = Nope { first: 1, second: "two" }
""")) { case s @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      val msg = s.message(Map.empty, Colorize.None)
      assert(msg.contains("Unknown constructor `Nope`."))
      ()
    }

    evalFail(List("""
package A

struct Pair(first, second)

get = Nope(first, ...) -> first

main = get(Pair(1, "two"))
""")) { case s @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      val msg = s.message(Map.empty, Colorize.None)
      assert(msg.contains("Unknown constructor `Nope`."))
      ()
    }

    evalFail(List("""
package A

struct Pair(first, second)

get = Nope { first } -> first

main = get(Pair(1, "two"))
""")) { case s @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      val msg = s.message(Map.empty, Colorize.None)
      assert(msg.contains("Unknown constructor `Nope`."))
      ()
    }

    evalFail(List("""
package A

struct Pair(first, second)

get = Nope { first, ... } -> first

main = get(Pair(1, "two"))
""")) { case s @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      val msg = s.message(Map.empty, Colorize.None)
      assert(msg.contains("Unknown constructor `Nope`."))
      ()
    }
  }

  test("shadowing of external def isn't allowed") {
    evalFail(List("""
package A

external def foo(x: String) -> List[String]

def foo(x): x

""")) { case s @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      assertEquals(
        s.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package A\nbind names foo shadow external def\nRegion(57,71)"
      )
      ()
    }

    evalFail(List("""
package A

external def foo(x: String) -> List[String]

foo = 1

""")) { case s @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      assertEquals(
        s.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package A\nbind names foo shadow external def\nRegion(57,65)"
      )
      ()
    }

    evalFail(List("""
package A

external def foo(x: String) -> List[String]

external def foo(x: String) -> List[String]
""")) { case s @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      assertEquals(
        s.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package A\nexternal def: foo defined multiple times\nRegion(21,55)"
      )
      ()
    }
  }

  test("type parameters must be supersets for structs and enums fails") {
    evalFail(List("""
package Err

struct Foo[a](a)

main = Foo(1, "2")
""")) { case sce @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      assertEquals(
        sce.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package Err\nFoo found declared: [a], not a superset of [b]\nRegion(14,30)"
      )
      ()
    }

    evalFail(List("""
package Err

struct Foo[a](a: a, b: b)

main = Foo(1, "2")
""")) { case sce @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      assertEquals(
        sce.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package Err\nFoo found declared: [a], not a superset of [a, b]\nRegion(14,39)"
      )
      ()
    }

    evalFail(List("""
package Err

enum Enum[a]: Foo(a)

main = Foo(1, "2")
""")) { case sce @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      val msg = sce.message(Map.empty, Colorize.None)
      assert(
        msg.contains("type variable `a` is ambiguous in Enum[a]")
      )
      assert(
        msg.contains("Either remove it or use it in one of the enum variants")
      )
      assert(msg.contains("Region(14,34)"))
      ()
    }

    evalFail(List("""
package Err

enum Enum[a]: Foo(a: a), Bar(a: b)

main = Foo(1, "2")
""")) { case sce @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      val msg = sce.message(Map.empty, Colorize.None)
      assert(
        msg.contains("Enum.Bar is missing type parameter declarations for [b]")
      )
      assert(msg.contains("enum Enum[a, b]"))
      assert(msg.contains("Bar[b]("))
      assert(msg.contains("Region("))
      ()
    }
  }

  test("test duplicate package message") {
    val pack =
      """
        |package Err
        |
        |from Bosatsu/Predef import foldl_List
        |
        |main = 1
        |""".stripMargin

    evalFail(List(pack, pack)) {
      case sce @ PackageError.DuplicatedPackageError(_) =>
        assertEquals(
          sce.message(
            Map.empty,
            Colorize.None
          ),
          "package Err duplicated in 0, 1"
        )
        ()
    }
  }

  test("test bad list pattern message") {
    evalFail(List("""
package Err

x = [1, 2, 3]

main = match x:
  case [*_, *_]: "bad"
  case _: "still bad"

""")) { case sce @ PackageError.TotalityCheckError(_, _) =>
      assertEquals(
        sce.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package Err\nRegion(36,89)\nmultiple splices in pattern, only one per match allowed"
      )
      ()
    }
  }

  test("test bad string pattern message") {
    val dollar = '$'
    evalFail(List(s"""
package Err

x = "foo bar"

main = match x:
  case "$dollar{_}$dollar{_}": "bad"
  case _: "still bad"

""")) { case sce @ PackageError.TotalityCheckError(_, _) =>
      val dollar = '$'
      assertEquals(
        sce.message(Map.empty, Colorize.None),
        s"in file: <unknown source>, package Err\nRegion(36,91)\ninvalid string pattern: '$dollar{_}$dollar{_}' (adjacent string bindings aren't allowed)"
      )
      ()
    }
  }

  test("colliding type names cause errors") {
    evalFail(List(s"""
package Err

struct Foo

struct Foo(x)

main = Foo(1)
""")) { case sce @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      assertEquals(
        sce.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package Err\ntype name: Foo defined multiple times\nRegion(14,24)"
      )
      ()
    }
  }

  test("colliding constructor names cause errors") {
    evalFail(List(s"""
package Err

enum Bar: Foo

struct Foo(x)

main = Foo(1)
""")) { case sce @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      assertEquals(
        sce.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package Err\nconstructor: Foo defined multiple times\nRegion(14,27)"
      )
      ()
    }
  }

  test("non binding top levels are allowed: issue 1639") {
    evalTest(
      List("""
package A

# this is basically a typecheck only
_ = add(1, 2)
main = 10
"""),
      "A",
      VInt(10)
    )

    evalTest(
      List("""
package A

# this is basically a typecheck only
(_, _) = (1, "1")
main = 10
"""),
      "A",
      VInt(10)
    )

    evalTest(
      List("""
package A

struct Foo(x, y)
# this is basically a typecheck only
Foo(_, _) = Foo(1, "1")
main = 10
"""),
      "A",
      VInt(10)
    )

    evalTest(
      List("""
package A

x = 1
_ = x
(_, _) = (x, 1)
main = 10
"""),
      "A",
      VInt(10)
    )

    evalFail(List("""
package A

(_, _) = add(1, "x")
main = 10
""")) { case te: PackageError.TypeErrorIn =>
      val msg = te.message(Map.empty, Colorize.None)
      assert(msg.contains("package A"), msg)
      assert(msg.contains("type mismatch in call to Bosatsu/Predef::add"), msg)
      assert(msg.contains("Region(28,31)"), msg)
      ()
    }
  }

  test("match pattern mismatch reports scrutinee and pattern types") {
    val src = """
package A

def under_twenty(n: Int) -> Int:
  match n:
    case "oops" | 2 | 6: 3

main = under_twenty(3)
"""

    evalFail(List(src)) { case te: PackageError.TypeErrorIn =>
      val msg = te.message(Map.empty, Colorize.None)
      assert(msg.contains("pattern type mismatch"), msg)
      assert(msg.contains("expected scrutinee type: Int"), msg)
      assert(msg.contains("found pattern type: String"), msg)
      ()
    }
  }

  test(
    "repeated related mismatches show all evidence sites in combined errors"
  ) {
    val region0 = Region(10, 11)
    val region1 = Region(20, 21)
    val region2 = Region(30, 31)
    val err = rankn.Infer.Error.Combine(
      rankn.Infer.Error.NotUnifiable(
        rankn.Type.IntType,
        rankn.Type.StrType,
        region0,
        region1,
        rankn.Infer.Error.Direction.ExpectRight
      ),
      rankn.Infer.Error.NotUnifiable(
        rankn.Type.IntType,
        rankn.Type.StrType,
        region0,
        region2,
        rankn.Infer.Error.Direction.ExpectRight
      )
    )

    val pe = PackageError.TypeErrorIn(
      err,
      PackageName.parts("A"),
      lets = Nil,
      externals = Map.empty,
      letNameRegions = Map.empty,
      localTypeNames = Set.empty
    )
    val msg = pe.message(Map.empty, Colorize.None)
    assert(msg.contains("evidence sites:"), msg)
    assert(msg.contains("Region(20,21)"), msg)
    assert(msg.contains("Region(30,31)"), msg)
  }

  test("call mismatch keeps expected/found orientation after meta solving") {
    val src = """
package A

def same(x: a, y: a) -> Bool:
  True

main = same(1, "x")
"""

    evalFail(List(src)) { case te: PackageError.TypeErrorIn =>
      val msg = te.message(Map.empty, Colorize.None)
      assert(msg.contains("type mismatch in call to same"), msg)
      assert(msg.contains("expected: Int"), msg)
      assert(msg.contains("found: String"), msg)
      ()
    }
  }

  test("recursion check with shadowing") {
    evalFail(List("""
package S

enum Thing:
  Thing1, Thing2(a: Int, t: Thing)

def bar(y, _: String, x):
  x = Thing2(0, x)
  recur x:
    case Thing1: y
    case Thing2(i, t): bar(i, "boom", t)

test = Assertion(True, "")
""")) { case re @ PackageError.RecursionError(_, _) =>
      assertEquals(
        re.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package S\nrecur not on an argument to the def of bar, args: (y, _: String, x)\nRegion(107,175)\n"
      )
      ()
    }
  }

  test("bindings can't be duplicated in patterns, issue 584") {
    evalFail(List("""
package Foo

out = match (1,2):
  case (a, a): a

test = Assertion(True, "")
""")) { case sce @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      assertEquals(
        sce.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package Foo\nrepeated bindings in pattern: a\nRegion(48,49)"
      )
      ()
    }
    evalFail(List("""
package Foo

out = match [(1,2), (1, 0)]:
  case [(a, a), (1, 0)]: a
  case _: 0

test = Assertion(True, "")
""")) { case sce @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      assertEquals(
        sce.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package Foo\nrepeated bindings in pattern: a\nRegion(68,69)"
      )
      ()
    }
    evalFail(List("""
package Foo

x = Some(1)

out = match x:
  case Some(y) as y: y
  case _: 0

test = Assertion(True, "")
""")) { case sce @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      val msg = sce.message(
        Map.empty,
        Colorize.None
      )
      assert(msg.contains("repeated bindings in pattern: y"), msg)
      ()
    }
    runBosatsuTest(
      List("""
package Foo

out = match [(1,2), (1, 0)]:
  case [(a, _) | (_, a), (1, 0)]: a
  case _: 0

test = Assertion(out.eq_Int(1), "")
"""),
      "Foo",
      1
    )
  }

  test("unknown type constructor message is good. issue 653") {
    evalFail(List("""
package Foo

struct Bar(baz: Either[Int, String])

test = Assertion(True, "")

""")) { case sce @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      assertEquals(
        sce.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package Foo\nunknown type: Either\nRegion(14,50)"
      )
      ()
    }
  }

  test("its an error to export a value and not its type. issue 782") {
    evalFail("""
package Foo

export bar

struct Bar

bar = Bar

""" :: """
package UseBar
from Foo import bar

x = bar
""" :: Nil) { case sce =>
      assertEquals(
        sce.message(
          Map.empty,
          Colorize.None
        ),
        "in <unknown source> export bar of type Bar has an unexported (private) type."
      )
      ()
    }
  }

  test("test def with type params") {
    runBosatsuTest(
      List("""
package Foo

def foo[a](a: a) -> a:
  x: a = a
  def again(x: a): x
  def and_again[b](x: b): x
  and_again(again(x))
  
test = Assertion(foo(True), "")
"""),
      "Foo",
      1
    )

    evalFail(List("""
package Foo

def foo[a](a: a) -> a:
  x: a = a
  def again(x: a): x
  def and_again[b](x: a): x
  and_again(again(x))

""")) { case sce @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      assertEquals(
        sce.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package Foo\nand_again found declared types: [b], not a subset of [a]\nRegion(71,118)"
      )
      ()
    }
  }

  test("ill-kinded structs point to the right region") {

    evalFail(List("""
package Foo

struct Foo(a: f[a], b: f)
""")) { case kie @ PackageError.KindInferenceError(_, _, _) =>
      assertEquals(
        kie.message(Map.empty, Colorize.None),
        """in file: <unknown source>, package Foo
shape error: expected kind(f) and * to match in the constructor Foo

Region(14,39)"""
      )
      ()
    }

    evalFail(List("""
package Foo

struct Foo[a: *](a: a[Int])
""")) { case kie @ PackageError.KindInferenceError(_, _, _) =>
      assertEquals(
        kie.message(Map.empty, Colorize.None),
        """in file: <unknown source>, package Foo
shape error: expected * -> ? but found * in the constructor Foo inside type a[Int]

Region(14,41)"""
      )
      ()
    }
  }

  test("ill kinded code examples") {
    evalFail(List("""
package Foo

# this is an higher kinded apply
struct Foo[t: (* -> *) -> *, a: (* -> *)](value: t[a])

struct Id(a)
# this code could run if we ignored kinds
def makeFoo(v: Int): Foo(Id(v))

""")) { case kie: PackageError.TypeErrorIn =>
      assertEquals(
        kie.message(Map.empty, Colorize.None),
        """in file: <unknown source>, package Foo
kind error: the type: ?0 of kind: (* -> *) -> * at: 
Region(183,188)

cannot be unified with the type Id of kind: +* -> *
because the first kind does not subsume the second."""
      )
      ()
    }

    evalFail(List("""
package Foo

# this is an higher kinded apply
struct Foo[t: (* -> *) -> *, a: (* -> *)](value: t[a])

struct Id(a)
# this code could run if we ignored kinds
def makeFoo(v: Int) -> Foo[Id, Int]: Foo(Id(v))

""")) { case kie: PackageError.TypeErrorIn =>
      assertEquals(
        kie.message(Map.empty, Colorize.None),
        """in file: <unknown source>, package Foo
kind error: the type: Foo[Id] is invalid because the left Foo has kind ((* -> *) -> *) -> (* -> *) -> * and the right Id has kind +* -> * but left cannot accept the kind of the right:
Region(195,205)"""
      )
      ()
    }

  }

  test("print a decent message when arguments are omitted") {
    evalFail(List("""
package QS

def quick_sort0(cmp, left, right):
  recur left:
    case []: right
    case [pivot, *tail]:
      if right matches ([] | [_]): right
      else:
        smaller = [x for x in right if cmp(x, pivot) matches (LT | EQ)]
        bigger = [x for x in right if cmp(x, pivot) matches GT]
        smalls = quick_sort0(cmp, tail, smaller)
        # we accidentally omit bigger below
        bigs = quick_sort0(cmp, tail)
        [*smalls, *bigs]
""")) { case kie: PackageError.TypeErrorIn =>
      assertEquals(
        kie.message(Map.empty, Colorize.None),
        """in file: <unknown source>, package QS
type error: expected type Fn2
Region(415,424)
but found type Fn3[(?17, ?9) -> Comparison]
hint: the first type is a function with 2 arguments and the second is a function with 3 arguments.
Region(403,414)"""
      )
      ()
    }

  }

  test("error early on a bad type in a recursive function") {
    val testCode = """
package BadRec

enum N: Z, S(n: N)

def toInt(n: N, acc: Int) -> Int:
  recur n:
    case Z: acc
    case S(n): toInt(n, "foo")

"""
    evalFail(List(testCode)) { case kie: PackageError.TypeErrorIn =>
      val message = kie.message(Map.empty, Colorize.None)
      assert(message.contains("Region(122,127)"))
      val badRegion = testCode.substring(122, 127)
      assertEquals(badRegion, "\"foo\"")
      ()
    }
  }

  test("we get error messages from multiple type errors top level") {
    val testCode = """
package ErrorCheck

x: Int = "1"
y: String = 1

"""
    evalFail(List(testCode)) { case kie: PackageError.TypeErrorIn =>
      val message = kie.message(Map.empty, Colorize.None)
      assert(message.contains("Region(30,33)"))
      assertEquals(testCode.substring(30, 33), "\"1\"")
      assert(message.contains("Region(46,47)"))
      assertEquals(testCode.substring(46, 47), "1")
      ()
    }
  }

  test("we get error messages from multiple type errors top nested") {
    val testCode = """
package ErrorCheck

z = (
  x: Int = "1"
  y: String = 1
  (x, y)
)

"""
    evalFail(List(testCode)) { case kie: PackageError.TypeErrorIn =>
      val message = kie.message(Map.empty, Colorize.None)
      assert(message.contains("Region(38,41)"))
      assertEquals(testCode.substring(38, 41), "\"1\"")
      assert(message.contains("Region(56,57)"))
      assertEquals(testCode.substring(56, 57), "1")
      ()
    }
  }

  test("missing enum branch type params reports branch-scoped error") {
    val testCode = """
package ErrorCheck

enum FreeF[a]:
  Pure(a: a)
  Mapped(prev: FreeF[b], fn: b -> a)
"""

    evalFail(List(testCode)) {
      case sce @ PackageError.SourceConverterErrorsIn(_, _, _) =>
        val message = sce.message(Map.empty, Colorize.None)
        assert(
          message.contains(
            "FreeF.Mapped is missing type parameter declarations for [b]"
          )
        )
        assert(message.contains("enum FreeF[a, b]"))
        assert(message.contains("Mapped[b]("))
        assert(message.contains("Region("))
        ()
    }
  }

  test("ill-kinded enum branch type params reports constructor context") {
    val testCode = """
package ErrorCheck

enum FreeF[a]:
  Pure(a: a)
  Mapped[b](prev: FreeF[b], fn: b[a])
"""

    def assertMessage(message: String): Unit = {
      assert(message.contains("constructor Mapped"))
      assert(message.contains("b[a]"))
    }

    evalFail(List(testCode)) {
      case kie @ PackageError.KindInferenceError(_, _, _) =>
        assertMessage(kie.message(Map.empty, Colorize.None))
      case kie @ PackageError.TypeErrorIn(_, _, _, _, _, _) =>
        assertMessage(kie.message(Map.empty, Colorize.None))
    }
  }

  test("enum type parameter ownership collisions report scopes") {
    val testCode = """
package ErrorCheck

enum Foo[a]:
  Bar[a](get: a)
"""

    evalFail(List(testCode)) {
      case sce @ PackageError.SourceConverterErrorsIn(_, _, _) =>
        val message = sce.message(Map.empty, Colorize.None)
        assert(
          message.contains(
            "Foo has intersecting explicit type parameter declarations"
          )
        )
        assert(
          message.contains(
            "All explicit type-parameter groups must have non-intersecting type variable sets"
          )
        )
        assert(
          message.contains(
            "a: enum Foo[a], branch Bar[a]"
          )
        )
        assert(message.contains("Region("))
        ()
    }
  }

  test("tuples bigger than 32 fail") {
    val testCode = """
package ErrorCheck

z = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
  21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
  31, 32, 33)

"""
    evalFail(List(testCode)) {
      case kie @ PackageError.SourceConverterErrorsIn(_, _, _) =>
        val message = kie.message(Map.empty, Colorize.None)
        assert(
          message.contains(
            "invalid tuple size. Found 33, but maximum allowed 32"
          )
        )
        assert(message.contains("Region(25,154)"))
        ()
    }

    val testCode1 = """
package ErrorCheck

z = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
  21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
  31, 32)

res = z matches (1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
  21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
  31, 32, 33)

"""
    evalFail(List(testCode1)) {
      case kie @ PackageError.SourceConverterErrorsIn(_, _, _) =>
        val message = kie.message(Map.empty, Colorize.None)
        assert(
          message.contains(
            "invalid tuple size. Found 33, but maximum allowed 32"
          )
        )
        assert(message.contains("Region(158,297)"))
        ()
    }
  }

  test("kind errors show the region of the type") {
    val testCode = """
package ErrorCheck

struct Foo[a: -*](get: a)

"""
    evalFail(List(testCode)) {
      case kie @ PackageError.KindInferenceError(_, _, _) =>
        val message = kie.message(Map.empty, Colorize.None)
        assert(message.contains("Region(21,46)"))
        assertEquals(testCode.substring(21, 46), "struct Foo[a: -*](get: a)")
        ()
    }
  }

  test("external defs with explicit type parameters exactly match") {
    val testCode = """
package ErrorCheck

external def foo[b](lst: List[a]) -> a

"""
    evalFail(List(testCode)) {
      case kie @ PackageError.SourceConverterErrorsIn(_, _, _) =>
        val message = kie.message(Map.empty, Colorize.None)
        assert(message.contains("Region(30,59)"))
        assert(message.contains("[b], not the same as [a]"))
        assertEquals(
          testCode.substring(30, 59),
          "def foo[b](lst: List[a]) -> a"
        )
        ()
    }
  }

  test("test duplicate import error messages") {
    val testCode = List(
      """
package P1
export foo

foo = 1

""",
      """
package P2
export foo

foo = 2
""",
      """
package P3

from P1 import foo
from P2 import foo

main = foo
"""
    )

    evalFail(testCode) { case kie @ PackageError.DuplicatedImport(_, _) =>
      val message = kie.message(Map.empty, Colorize.None)
      assertEquals(
        message,
        "duplicate import in <unknown source> package P3\n\tfrom P1 import foo\n\tfrom P2 import foo\n"
      )
      ()
    }

    // explicit predefs are allowed
    runBosatsuTest(
      List("""
package P

from Bosatsu/Predef import foldl_List

main = Assertion(True, "")
"""),
      "P",
      1
    )
    // explicit predefs renamed are allowed
    runBosatsuTest(
      List("""
package P

from Bosatsu/Predef import foldl_List as fl

res = [].fl(True, (_, _) -> False)

main = Assertion(res, "")
"""),
      "P",
      1
    )
    evalFail(List("""
package P

from Bosatsu/Predef import concat as fl
from Bosatsu/Predef import foldl_List as fl

res = [].fl(True, (_, _) -> False)

main = Assertion(res, "")
    """)) { case kie @ PackageError.DuplicatedImport(_, _) =>
      val message = kie.message(Map.empty, Colorize.None)
      assertEquals(
        message,
        "duplicate import in <unknown source> package P\n\tfrom Bosatsu/Predef import concat as fl\n\tfrom Bosatsu/Predef import foldl_List as fl\n"
      )
      ()
    }
  }

  test(
    "unknown name suggestions prefer local names and include substring matches"
  ) {
    val testCode = List("""
package P1

fofoooooooo = 1

ofof = 2

main = fof
""")

    evalFail(testCode) { case kie: PackageError.TypeErrorIn =>
      val message = kie.message(Map.empty, Colorize.None)
      assert(message.contains("in file: <unknown source>, package P1"))
      assert(message.contains("Unknown name `fof`."))
      assert(
        message.contains(
          "Did you mean one of: local value `fofoooooooo`, local value `ofof`?"
        )
      )
      assert(message.contains("Region(47,50)"))
      ()
    }
  }

  test("unknown name suggestions include candidates when one is a substring") {
    val testCode = List("""
package P1

foo = 1

main = xxfoo
""")

    evalFail(testCode) { case kie: PackageError.TypeErrorIn =>
      val message = kie.message(Map.empty, Colorize.None)
      assert(message.contains("Unknown name `xxfoo`."))
      assert(message.contains("Did you mean local value `foo`?"))
      ()
    }
  }

  test("unknown constructor in type errors suggests nearest constructors") {
    val pack = PackageName.parts("P")
    val miss = Identifier.Constructor("JNul")
    val known = Identifier.Constructor("JNull")
    val inferEnv = new rankn.Infer.Env(
      rankn.RefSpace.constRef(0L),
      Map.empty,
      Map(
        (pack, known) -> (
          Nil,
          Nil,
          Nil,
          rankn.Type.Const
            .Defined(pack, TypeName(Identifier.Constructor("Json")))
        )
      ),
      Map.empty
    )
    val err = PackageError.TypeErrorIn(
      rankn.Infer.Error
        .UnknownConstructor((pack, miss), Region(0, 1), inferEnv),
      pack,
      Nil,
      Map.empty,
      Map.empty,
      Set.empty
    )
    val message = err.message(Map.empty, Colorize.None)
    assert(message.contains("Unknown constructor `JNul`."), message)
    assert(message.contains("Did you mean constructor `JNull`?"), message)
  }

  test("repeated unknown constructors are aggregated with occurrence counts") {
    val pack = PackageName.parts("P")
    val miss = Identifier.Constructor("JNul")
    val known = Identifier.Constructor("JNull")
    val inferEnv = new rankn.Infer.Env(
      rankn.RefSpace.constRef(0L),
      Map.empty,
      Map(
        (pack, known) -> (
          Nil,
          Nil,
          Nil,
          rankn.Type.Const
            .Defined(pack, TypeName(Identifier.Constructor("Json")))
        )
      ),
      Map.empty
    )
    val err = PackageError.TypeErrorIn(
      rankn.Infer.Error.Combine(
        rankn.Infer.Error
          .UnknownConstructor((pack, miss), Region(0, 1), inferEnv),
        rankn.Infer.Error
          .UnknownConstructor((pack, miss), Region(2, 3), inferEnv)
      ),
      pack,
      Nil,
      Map.empty,
      Map.empty,
      Set.empty
    )
    val message = err.message(Map.empty, Colorize.None)
    assert(message.contains("Unknown constructor `JNul`."), message)
    assert(
      message.contains("This unknown constructor appears 2 times."),
      message
    )
  }

  test(
    "unknown constructor in totality diagnostics suggests nearest constructors"
  ) {
    val pack = PackageName.parts("P")
    val known = Identifier.Constructor("JNull")
    val miss = Identifier.Constructor("JNul")
    val jsonDt = rankn.DefinedType[Nothing](
      packageName = pack,
      name = TypeName(Identifier.Constructor("Json")),
      annotatedTypeParams = Nil,
      constructors = List(rankn.ConstructorFn(known, Nil))
    )
    val typeEnv = rankn.TypeEnv.fromDefinitions(List(jsonDt))
    given Region = Region(0, 1)
    val tag = Declaration.Var(Identifier.Name("x"))
    val pat: Pattern[(PackageName, Identifier.Constructor), rankn.Type] =
      Pattern.PositionalStruct((pack, miss), Nil)
    val lit = Expr.Literal[Declaration](Lit.Integer(0L), tag)
    val matchExpr = Expr.Match(
      lit,
      cats.data.NonEmptyList.one(Expr.Branch(pat, None, lit)),
      tag
    )
    val totalityErr = PackageError.TotalityCheckError(
      pack,
      TotalityCheck.InvalidPattern(
        matchExpr,
        TotalityCheck.UnknownConstructor((pack, miss), pat, typeEnv)
      )
    )
    val message = totalityErr.message(Map.empty, Colorize.None)
    assert(message.contains("Unknown constructor `JNul`."))
    assert(message.contains("Did you mean constructor `JNull`?"))
  }

  test(
    "source converter unknown constructor supports multiple suggestions and context"
  ) {
    val miss = Identifier.Constructor("JBoool")
    val pat = Pattern.PositionalStruct(
      Pattern.StructKind.Named(miss, Pattern.StructKind.Style.TupleLike),
      Pattern.WildCard :: Nil
    )
    val err = SourceConverter.UnknownConstructor(
      miss,
      pat,
      List(
        Identifier.Constructor("JBool"),
        Identifier.Constructor("JBoole"),
        Identifier.Constructor("JBooolX")
      ),
      Region(0, 1)
    )
    val message = err.message
    assert(message.contains("Unknown constructor `JBoool`."))
    assert(message.contains("Nearest constructors in scope:"))
    assert(message.contains(" in"))
  }

  test("source converter unknown constructor can omit suggestions") {
    val miss = Identifier.Constructor("Nope")
    val pat = Pattern.PositionalStruct(
      Pattern.StructKind.Named(miss, Pattern.StructKind.Style.TupleLike),
      Nil
    )
    val err = SourceConverter.UnknownConstructor(miss, pat, Nil, Region(0, 1))
    val message = err.message
    assertEquals(message, "Unknown constructor `Nope`.")
  }

  test(
    "name suggestion helper handles edge inputs and low-confidence candidates"
  ) {
    val zeroCount = NameSuggestion.nearest(
      Identifier.Name("abc"),
      List(NameSuggestion.Candidate(Identifier.Name("abcd"), "x")),
      0
    )
    assertEquals(zeroCount, Nil)

    val emptyQuery = NameSuggestion.nearest(
      Identifier.Name(""),
      List(NameSuggestion.Candidate(Identifier.Name("abcd"), "x")),
      3
    )
    assertEquals(emptyQuery, Nil)

    val emptyCandidate = NameSuggestion.nearest(
      Identifier.Name("abcd"),
      List(NameSuggestion.Candidate(Identifier.Name(""), "x")),
      3
    )
    assertEquals(emptyCandidate, Nil)

    val lowConfidence = NameSuggestion.nearest(
      Identifier.Name("abcdef"),
      List(NameSuggestion.Candidate(Identifier.Name("uvwxyz"), "x")),
      3
    )
    assertEquals(lowConfidence, Nil)
  }

  test(
    "name suggestion fallback heuristic catches close edits without shared prefixes"
  ) {
    val suggestions = NameSuggestion.nearest(
      Identifier.Name("abbbbbbb"),
      List(
        NameSuggestion.Candidate(Identifier.Name("axbbbbbb"), "fallback-hit")
      ),
      3
    )
    assertEquals(suggestions.map(_.value), List("fallback-hit"))
  }

  test("name suggestion deduplicates names and keeps strongest scope match") {
    val suggestions = NameSuggestion.nearest(
      Identifier.Name("computd"),
      List(
        NameSuggestion.Candidate(
          Identifier.Name("computed"),
          "imported",
          NameSuggestion.ScopePriority.Imported
        ),
        NameSuggestion.Candidate(
          Identifier.Name("computed"),
          "local",
          NameSuggestion.ScopePriority.Local
        )
      ),
      3
    )
    assertEquals(suggestions.map(_.value), List("local"))
  }

}
