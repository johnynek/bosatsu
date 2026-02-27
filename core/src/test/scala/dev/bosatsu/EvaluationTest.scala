package dev.bosatsu

import Value._

import scala.concurrent.duration.DurationInt

class EvaluationTest extends munit.FunSuite with ParTest {
  override val munitTimeout = 180.seconds

  import TestUtils._

  test("simple evaluation") {
    evalTest(
      List("""
package Foo

x = 1
"""),
      "Foo",
      VInt(1)
    )

    evalTest(List("x = 1"), "Package0", VInt(1))

    evalTest(
      List("""
# test shadowing
x = match 1: case x: x
"""),
      "Package0",
      VInt(1)
    )

    evalTest(
      List("""
package Foo

# exercise calling directly a lambda
x = (y -> y)("hello")
"""),
      "Foo",
      Str("hello")
    )

    runBosatsuTest(
      List("""
package Foo

foo = "hello"

def eq_String(a, b):
  match cmp_String(a, b):
    case EQ: True
    case _: False

test = Assertion(eq_String("hello", foo), "checking equality")
"""),
      "Foo",
      1
    )

    runBosatsuTest(
      List("""
package Foo

def let(arg, in): in(arg)

foo = (
  x <- let(3)
  x.add(1)
)

test = Assertion(foo matches 4, "checking equality")
"""),
      "Foo",
      1
    )

    runBosatsuTest(
      List("""
package Foo

test = TestSuite("three trivial tests", [ Assertion(True, "t0"),
    Assertion(True, "t1"),
    Assertion(True, "t2"),
    ])
"""),
      "Foo",
      3
    )
  }

  test("test if/else") {
    evalTest(
      List("""
package Foo

x = 1

z = match x.cmp_Int(1):
  case EQ:
    "foo"
  case _:
    "bar"
"""),
      "Foo",
      Str("foo")
    )

    evalTest(
      List("""
package Foo

x = 1

# here if the single expression python style
z = "foo" if x.eq_Int(2) else "bar"
"""),
      "Foo",
      Str("bar")
    )
  }

  test("exercise option from predef") {
    evalTest(
      List("""
package Foo

x = Some(1)

z = match x:
  case Some(v): add(v, 10)
  case None: 0
"""),
      "Foo",
      VInt(11)
    )

    // Use a local name collision and see it not have a problem
    evalTest(
      List("""
package Foo

enum Option: None, Some(get)

x = Some(1)

z = match x:
  case Some(v): add(v, 10)
  case None: 0
"""),
      "Foo",
      VInt(11)
    )

    evalTest(
      List("""
package Foo

# try the above example, with Some first
enum Option: Some(get), None

x = Some(1)

z = match x:
  case None: 0
  case Some(v): add(v, 10)
"""),
      "Foo",
      VInt(11)
    )
  }

  test("test matching literals") {
    evalTest(
      List("""
package Foo

x = 1

main = match x:
  case 1: "good"
  case _: "bad"
"""),
      "Foo",
      Str("good")
    )

    evalTest(
      List("""
package Foo

x = [1]

# test using List literals
main = match x:
  case EmptyList: "empty"
  case NonEmptyList(...): "notempty"
"""),
      "Foo",
      Str("notempty")
    )

    evalTest(
      List("""
package Foo

x = "1"

main = match x:
  case "1": "good"
  case _: "bad"
"""),
      "Foo",
      Str("good")
    )

    evalTest(
      List("""
package Foo

struct Pair(fst, snd)

x = Pair(1, "1")

main = match x:
  case Pair(_, "1"): "good"
  case _: "bad"
"""),
      "Foo",
      Str("good")
    )
  }

  test("test tuples") {
    evalTest(
      List("""
package Foo

x = (1, "1")

main = match x:
  case (_, "1"): "good"
  case _: "bad"
"""),
      "Foo",
      Str("good")
    )

    evalTest(
      List("""
package Foo

x = (1, "1")

def go(u):
  # ignore that u is unused
  _ = u
  (_, y) = x
  match y:
    case "1": "good"
    case _: "bad"

main = go(())
"""),
      "Foo",
      Str("good")
    )
  }

  test(
    "unused locals can be consumed with unnamed patterns in blocks and defs"
  ) {
    evalTest(
      List("""
package A

x = (
  foo = 2
  _ = foo
  3
)

main = x
"""),
      "A",
      VInt(3)
    )

    evalTest(
      List("""
package A

def x(z):
  foo = 2
  _ = foo
  z

main = x(7)
"""),
      "A",
      VInt(7)
    )
  }

  test("do a fold") {
    evalTest(
      List("""
package Foo

three = [1, 2]

def sum(ls):
  ls.foldl_List(0, add)

sum0 = sum(three)
sum1 = three.foldl_List(0, (x, y) -> add(x, y))

same = sum0.eq_Int(sum1)
"""),
      "Foo",
      True
    )

    evalTest(
      List("""
package Foo

three = [1, 2]

sum0 = three.foldl_List(0, add)
sum1 = three.foldl_List(0, (x, y) -> add(x, y))

same = sum0.eq_Int(sum1)
"""),
      "Foo",
      True
    )

  }

  test("test Int functions") {
    evalTest(
      List("""
package Foo

main = 6.mod_Int(4)
"""),
      "Foo",
      VInt(2)
    )

    evalTest(
      List("""
package Foo

main = match 6.div(4):
  case 0: 42
  case 1: 100
  case x: x
"""),
      "Foo",
      VInt(100)
    )

    evalTest(
      List("""
package Foo

main = 6.gcd_Int(3)
"""),
      "Foo",
      VInt(3)
    )
  }

  test("test Float64 functions") {
    runBosatsuTest(
      List("""
package Foo

def eqf(a, b): cmp_Float64(a, b) matches EQ

nan0 = divf(0.0, 0.0)
inf0 = divf(1.0, 0.0)
ninf0 = divf(-1.0, 0.0)

def classify(x):
  match x:
    case .NaN: 0
    case 0.0: 1
    case 1.5: 2
    case _: 3

test = TestSuite("float", [
  Assertion(eqf(addf(1.25, 2.5), 3.75), "addf"),
  Assertion(eqf(subf(5.5, 2.25), 3.25), "subf"),
  Assertion(eqf(timesf(1.5, 2.0), 3.0), "timesf"),
  Assertion(eqf(divf(7.5, 2.5), 3.0), "divf"),
  Assertion(eqf(.NaN, nan0), "nan equality"),
  Assertion(eqf(∞, inf0), "literal infinity"),
  Assertion(eqf(-∞, ninf0), "literal negative infinity"),
  Assertion(cmp_Float64(nan0, 0.0) matches LT, "nan sorts first"),
  Assertion(cmp_Float64(0.0, nan0) matches GT, "non-nan sorts after"),
  Assertion(cmp_Float64(-0.0, 0.0) matches EQ, "signed zero compare"),
  Assertion(cmp_Float64(inf0, ninf0) matches GT, "inf order"),
  Assertion(classify(nan0) matches 0, "nan literal pattern"),
  Assertion(classify(.NaN) matches 0, "explicit nan pattern"),
  Assertion(classify(-0.0) matches 1, "signed zero pattern equality got ${int_to_String(classify(-0.0))}"),
  Assertion(classify(0.0) matches 1, "zero pattern"),
])
"""),
      "Foo",
      15
    )
  }

  test("test Float64 string conversion function") {
    runBosatsuTest(
      List(
        """
package Bosatsu/Num/Float64

from Bosatsu/Predef import Float64

export (
  float64_bits_to_Int,
  float64_to_String,
  inf,
  int_bits_to_Float64,
  neg_inf,
  string_to_Float64,
)

external def float64_bits_to_Int(x: Float64) -> Int
external def float64_to_String(x: Float64) -> String
external def int_bits_to_Float64(int: Int) -> Float64
external def string_to_Float64(s: String) -> Option[Float64]

inf: Float64 = ∞
neg_inf: Float64 = -∞
""",
        """
package Foo

from Bosatsu/Num/Float64 import (
  float64_bits_to_Int,
  float64_to_String,
  inf,
  int_bits_to_Float64,
  neg_inf,
  string_to_Float64,
)

nan0 = divf(0.0, 0.0)
test = TestSuite("float64_to_String", [
  Assertion(float64_to_String(1.0) matches "1.0", "1.0"),
  Assertion(float64_to_String(1.25) matches "1.25", "1.25"),
  Assertion(float64_to_String(inf) matches "∞", "inf"),
  Assertion(float64_to_String(neg_inf) matches "-∞", "neg inf"),
  Assertion(float64_bits_to_Int(int_bits_to_Float64(0x8000_0000_0000_0000)) matches 0x8000_0000_0000_0000, "signed zero bits"),
  Assertion(float64_bits_to_Int(int_bits_to_Float64(-1)) matches 0xffff_ffff_ffff_ffff, "int bits low64"),
  Assertion(
    (
      nanf = int_bits_to_Float64(0x7ff8_0000_0000_00ab)
      parsed = string_to_Float64(float64_to_String(nanf))
      match parsed:
        case Some(v): float64_bits_to_Int(v) matches 0x7ff8_0000_0000_00ab
        case None: False
    ),
    "nan payload roundtrip"),
  Assertion(string_to_Float64("not-a-float") matches None, "invalid parse"),
  Assertion(
    (
      match string_to_Float64(".NaN"):
        case Some(_): True
        case None: False
    ),
    ".NaN parses"),
  Assertion(
    (
      match string_to_Float64(float64_to_String(nan0)):
        case Some(_): True
        case None: False
    ),
    "nan string parses"),
])
"""
      ),
      "Foo",
      10
    )
  }

  test("use range") {
    evalTest(
      List("""
package Foo

export zip

three = [0, 1]
# exercise the built-in range function
threer = range(3)

def zip(as, bs):
  recur as:
    case []: []
    case [ah, *atail]:
      match bs:
        case []: []
        case [bh, *btail]: [(ah, bh), *zip(atail, btail)]

def and(a, b):
  b if a else False

def same_items(items, eq):
  def test(p):
    (a, b) = p
    eq(a, b)

  items.foldl_List(True, (res, t) -> and(res, test(t)))

def eq_list(a, b, fn):
  same_items(zip(a, b), fn)

same = eq_list(three, threer, eq_Int)
"""),
      "Foo",
      True
    )

    evalTest(
      List("""
package Foo
export zip

def zip(as: List[a], bs: List[b]) -> List[(a, b)]:
  recur as:
    case []: []
    case [ah, *atail]:
      match bs:
        case []: []
        case [bh, *btail]: [(ah, bh), *zip(atail, btail)]

main = 1
"""),
      "Foo",
      VInt(1)
    )

  }

  test("test range_fold") {
    evalTest(
      List("""
package Foo

main = range_fold(0, 10, 0, add)
"""),
      "Foo",
      VInt(45)
    )

    evalTest(
      List("""
package Foo

main = range_fold(0, 10, 0, (_, y) -> y)
"""),
      "Foo",
      VInt(9)
    )

    evalTest(
      List("""
package Foo

main = range_fold(0, 10, 100, (x, _) -> x)
"""),
      "Foo",
      VInt(100)
    )
  }

  test("test some list matches") {
    evalTest(
      List("""
package Foo

def headOption(as):
  match as:
    case []: None
    case [a, *_]: Some(a)

main = headOption([1])
"""),
      "Foo",
      SumValue(1, ProductValue.single(VInt(1)))
    )

    runBosatsuTest(
      List("""
package Foo

def exists(as):
  as matches [*_, True, *_]

def not(x): False if x else True

test = TestSuite("exists", [
  Assertion(exists([True]), "[True]"),
  Assertion(exists([False, True]), "[False, True]"),
  Assertion(exists([True, False]), "[True, False]"),
  Assertion(not(exists([])), "![]"),
  Assertion(not(exists([False])), "![False]"),
  ])
"""),
      "Foo",
      5
    )

    evalTest(
      List("""
package Foo

def hasTwo(as):
  as matches [*_, 2, *_]

main = match (hasTwo([1, 2, 3]), hasTwo([1, 3])):
  case (True, False): 1
  case _: 0
"""),
      "Foo",
      VInt(1)
    )
  }

  test("test generics in defs") {
    evalTest(
      List("""
package Foo

def id(x: a) -> a:
  x

main = id(1)
"""),
      "Foo",
      VInt(1)
    )
  }

  test("exercise struct creation") {
    evalTest(
      List("""
package Foo

struct Bar(a: Int)

main = Bar(1)
"""),
      "Foo",
      VInt(1)
    )

    evalTest(
      List("""
package Foo

struct Bar(a: Int)

# destructuring top-level let
Bar(main) = Bar(1)
"""),
      "Foo",
      VInt(1)
    )

    evalTest(
      List("""
package Foo

struct Bar(a: Int)

# destructuring top-level let
Bar(main: Int) = Bar(1)
"""),
      "Foo",
      VInt(1)
    )

    evalTest(
      List("""
package Foo

struct Bar(a: Int)

y = Bar(1)
# destructuring top-level let
Bar(main: Int) = y
"""),
      "Foo",
      VInt(1)
    )

    evalTestJson(
      List("""
package Foo

struct Bar(a: Int, s: String)

main = Bar(1, "foo")
"""),
      "Foo",
      Json.JObject(
        List("a" -> Json.JNumberStr("1"), "s" -> Json.JString("foo"))
      )
    )
  }

  test("test some type errors") {
    evalFail(List("""
package Foo

main = if True:
  1
else:
  "1"
""")) { case _: PackageError.TypeErrorIn => () }
  }

  test(
    "test the list literals work even when we have conflicting local names"
  ) {
    evalTest(
      List("""
package Foo

struct EmptyList

main = [1, 2]
"""),
      "Foo",
      VList.Cons(VInt(1), VList.Cons(VInt(2), VList.VNil))
    )

    evalTest(
      List("""
package Foo

struct NonEmptyList

main = [1, 2]
"""),
      "Foo",
      VList.Cons(VInt(1), VList.Cons(VInt(2), VList.VNil))
    )

    evalTest(
      List("""
package Foo

def concat(a): a

main = concat([1, 2])
"""),
      "Foo",
      VList.Cons(VInt(1), VList.Cons(VInt(2), VList.VNil))
    )
  }

  test("check type aligned enum") {
    evalTest(
      List("""
package A

enum GoodOrBad:
  Bad(a: a), Good(a: a)

def unbox(gb: GoodOrBad[a]):
  match gb:
    case Good(g): g
    case Bad(b): b

(main: Int) = unbox(Good(42))
"""),
      "A",
      VInt(42)
    )

    evalTest(
      List("""
package A

enum GoodOrBad:
  Bad(a: a), Good(a: a)

Bad(main) | Good(main) = Good(42)
"""),
      "A",
      VInt(42)
    )
  }

  test("nontotal matches fail even if not at runtime") {
    evalFail(List("""
package Total

enum Opt: Nope, Yep(get)

something = Yep(
  1)

one = match something:
  case Yep(a): a

main = one
""")) { case PackageError.TotalityCheckError(_, _) => () }
  }

  test("unreachable patterns are an error") {
    evalFail(List("""
package Total

enum Opt: Nope, Yep(get)

something = Yep(
  1)

one = match something:
  case Yep(a): a
  case Nope: 0
  case _: 42

main = one
""")) { case PackageError.TotalityCheckError(_, _) => () }
  }

  test(
    "guard canonicalization handles True, False, and locally shadowed True"
  ) {
    evalTest(
      List("""
package GuardCanonTrue

main = match True:
  case _ if True: 1
"""),
      "GuardCanonTrue",
      VInt(1)
    )

    evalFail(List("""
package GuardCanonFalse

main = match True:
  case _ if False: 1
""")) { case PackageError.TotalityCheckError(_, _) => () }

    evalFail(List("""
package GuardCanonShadow

from Bosatsu/Predef import False as True

main = match True:
  case _ if True: 1
""")) { case PackageError.TotalityCheckError(_, _) => () }
  }

  test(
    "pattern guards evaluate correctly in matrix and ordered match compilation"
  ) {
    // matrix path (orthogonal constructor patterns)
    evalTest(
      List("""
package GuardMatrix

main = match Some(1):
  case Some(v) if v.eq_Int(0): 0
  case Some(v) if v.eq_Int(1): 7
  case Some(v): v
  case None: -1
"""),
      "GuardMatrix",
      VInt(7)
    )

    // ordered path (non-orthogonal list glob patterns)
    evalTest(
      List("""
package GuardOrdered

main = match [1, 2, 3]:
  case [*_, 2, *_] if False: 0
  case [*_, 2, *_]: 1
  case _: 2
"""),
      "GuardOrdered",
      VInt(1)
    )
  }

  test("string glob capture before char rest matches with accumulated prefix") {
    evalTest(
      List("""
package StringGlobCapture

def check(s):
  match s:
    case "${prefix}$.{ch}z":
      if prefix matches "ab":
        ch matches .'c'
      else:
        False
    case _:
      False

input = concat_String([char_to_String(.'a'), char_to_String(.'b'), char_to_String(.'c'), "z"])
main = check(input)
"""),
      "StringGlobCapture",
      True
    )

    evalTest(
      List("""
package StringGlobCaptureUnicode

def check(s):
  match s:
    case "${prefix}$.{ch}z":
      if prefix matches "é":
        ch matches .'β'
      else:
        False
    case _:
      False

input = concat_String([char_to_String(.'é'), char_to_String(.'β'), "z"])
main = check(input)
"""),
      "StringGlobCaptureUnicode",
      True
    )

    evalTest(
      List("""
package StringGlobCaptureNoMatch

def check(s):
  match s:
    case "${prefix}$.{ch}z":
      if prefix matches "ab":
        ch matches .'c'
      else:
        False
    case _:
      True

input = concat_String([char_to_String(.'a'), char_to_String(.'b'), char_to_String(.'c')])
main = check(input)
"""),
      "StringGlobCaptureNoMatch",
      True
    )
  }

  test("Leibniz type equality example") {
    evalTest(
      List("""
package A

export just_foo

struct Leib(subst: forall f: * -> *. f[a] -> f[b])

struct Id(a)

def coerce(a, leib):
  Leib(subst) = leib
  Id(b) = subst(Id(a))
  b

# there is really only one (polymorphic) value of Leib
refl = Leib(x -> x)

enum StringOrInt:
  IsStr(s: String, leib: Leib[String, a])
  IsInt(i: Int, leib: Leib[Int, a])

str = IsStr("foo", refl)
int = IsInt(42, refl)

# this takes StringOrInt[a] and returns a
def getValue(v: StringOrInt[a]) -> a:
  match v:
    case IsStr(s, leib): coerce(s, leib)
    case IsInt(i, leib): coerce(i, leib)

just_foo = getValue(str)
main = getValue(int)
"""),
      "A",
      VInt(42)
    )

    // If we leave out the coerce it fails
    evalFail(List("""
package A

struct Leib(subst: forall f. f[a] -> f[b])

# there is really only one (polymorphic) value of Leib
refl = Leib(x -> x)

enum StringOrInt:
  IsStr(s: String, leib: Leib[String, a])
  IsInt(i: Int, leib: Leib[Int, a])

str = IsStr("foo", refl)
int = IsInt(42, refl)

# this takes StringOrInt[a] and returns a
def getValue(v):
  match v:
    case IsStr(s, _): s
    case IsInt(i, _): i

main = getValue(int)
""")) { case _: PackageError.TypeErrorIn => () }

  }

  test("overly generic methods fail compilation") {
    evalFail(List("""
package A

# this shouldn't compile, a is too generic
def plus(x: a, y):
  x.add(y)

main = plus(1, 2)
""")) { case _: PackageError.TypeErrorIn => () }
  }

  test("structual recursion is allowed") {
    evalTest(
      List("""
package A

def len(lst, acc):
  recur lst:
    case []: acc
    case [_, *tail]: len(tail, acc.add(1))

main = len([1, 2, 3], 0)
"""),
      "A",
      VInt(3)
    )

    evalTest(
      List("""
package A

enum PNat: One, Even(of: PNat), Odd(of: PNat)

def toInt(pnat):
  recur pnat:
    case One: 1
    case Even(of): toInt(of).mul(2)
    case Odd(of): toInt(of).mul(2).add(1)

main = toInt(Even(Even(One)))
"""),
      "A",
      VInt(4)
    )

    evalFail(List("""
package A

enum Foo: Bar, Baz

def bad(foo):
  recur foo:
    case Bar: 0
    case baz: bad(baz)

main = bad(Bar)
""")) { case PackageError.RecursionError(_, _) => () }

    evalTest(
      List("""
package A

big_list = range(3_000)

main = big_list.foldl_List(0, add)
"""),
      "A",
      VInt((0 until 3000).sum)
    )

    def sumFn(n: Int): Int = if (n <= 0) 0 else { sumFn(n - 1) + n }
    evalTest(
      List("""
package A

enum Nat: Zero, Succ(of: Nat)

def toInt(pnat):
  recur pnat:
    case Zero: 0
    case Succ(n): toInt(n).add(1)

def sum(nat):
  recur nat:
    case Zero: 0
    case Succ(n): sum(n).add(toInt(nat))

main = sum(Succ(Succ(Succ(Zero))))
"""),
      "A",
      VInt(sumFn(3))
    )

    // try with Succ first in the Nat
    evalTest(
      List("""
package A

enum Nat: Zero, Succ(of: Nat)

def toInt(pnat):
  recur pnat:
    case Succ(n): toInt(n).add(1)
    case Zero: 0

def sum(nat):
  recur nat:
    case Succ(n): sum(n).add(toInt(nat))
    case Zero: 0

main = sum(Succ(Succ(Succ(Zero))))
"""),
      "A",
      VInt(sumFn(3))
    )
  }

  test("we can mix literal and enum forms of List") {
    evalTest(
      List("""
package A

def len(lst, acc):
  recur lst:
    case EmptyList: acc
    case [_, *tail]: len(tail, acc.add(1))

main = len([1, 2, 3], 0)
"""),
      "A",
      VInt(3)
    )
    evalTest(
      List("""
package A

def len(lst, acc):
  recur lst:
    case []: acc
    case NonEmptyList(_, tail): len(tail, acc.add(1))

main = len([1, 2, 3], 0)
"""),
      "A",
      VInt(3)
    )
  }

  test("list comphension test") {
    evalTest(
      List("""
package A

main = [x for x in range(4)].foldl_List(0, add)
"""),
      "A",
      VInt(6)
    )
    evalTest(
      List("""
package A

main = [*[x] for x in range(4)].foldl_List(0, add)
"""),
      "A",
      VInt(6)
    )

    evalTest(
      List("""
package A

doub = [(x, x) for x in range(4)]

main = [x.mul(y) for (x, y) in doub].foldl_List(0, add)
"""),
      "A",
      VInt(1 + 4 + 9)
    )
    evalTest(
      List("""
package A

main = [x for x in range(4) if x.eq_Int(2)].foldl_List(0, add)
"""),
      "A",
      VInt(2)
    )

    evalTest(
      List("""
package A

main = [*[x, x] for x in range(4) if x.eq_Int(2)].foldl_List(0, add)
"""),
      "A",
      VInt(4)
    )

    evalTest(
      List("""
package A

def eq_List(lst1, lst2):
  recur lst1:
    case []:
      match lst2:
        case []: True
        case _: False
    case [h1, *t1]:
      match lst2:
        case []: False
        case [h2, *t2]:
          eq_List(t1, t2) if eq_Int(h1, h2) else False

lst1 = [0, 0, 1, 1, 2, 2, 3, 3]
lst2 = [*[x, x] for x in range(4)]
lst3 = [*[y, y] for (_, y) in [(x, x) for x in range(4)]]

main = match (eq_List(lst1, lst2), eq_List(lst1, lst3)):
  case (True, True): 1
  case _           : 0
"""),
      "A",
      VInt(1)
    )
  }

  test("test fib using recursion") {
    evalTest(
      List("""
package A

enum Nat: Z, S(p: Nat)

def fib(n):
  recur n:
    case Z: 1
    case S(Z): 1
    case S(S(n2) as n1): fib(n1).add(fib(n2))

# fib(5) = 1, 1, 2, 3, 5, 8
main = fib(S(S(S(S(S(Z))))))
"""),
      "A",
      VInt(8)
    )

    evalTest(
      List("""
package A

enum Nat[a]: Z, S(p: Nat[a])

def fib(n):
  recur n:
    case Z: 1
    case S(Z): 1
    case S(S(n2) as n1): fib(n1).add(fib(n2))

# fib(5) = 1, 1, 2, 3, 5, 8
main = fib(S(S(S(S(S(Z))))))
"""),
      "A",
      VInt(8)
    )

    evalTest(
      List("""
package A

enum Nat: S(p: Nat), Z

def fib(n):
  recur n:
    case Z: 1
    case S(Z): 1
    case S(S(n2) as n1): fib(n1).add(fib(n2))

# fib(5) = 1, 1, 2, 3, 5, 8
main = fib(S(S(S(S(S(Z))))))
"""),
      "A",
      VInt(8)
    )
  }

  test("test matching the front of a list") {
    evalTest(
      List("""
package A

def bad_len(list):
  recur list:
    case []: 0
    case [*init, _]: bad_len(init).add(1)

main = bad_len([1, 2, 3, 5])
"""),
      "A",
      VInt(4)
    )

    evalTest(
      List("""
package A

def last(list):
  match list:
    case []: -1
    case [*_, s]: s

main = last([1, 2, 3, 5])
"""),
      "A",
      VInt(5)
    )
  }
  test("test a named pattern that doesn't match") {
    evalTest(
      List("""
package A

def bad_len(list):
  recur list:
    case []: 0
    case [2] | [3]: -1
    case [*_, 4 as four]:
      #ignore_binding,
      _ = four
      -1
    case [100, *_]: -1
    case [*init, (_: Int) as last]:
      #ignore binding
      _ = last
      bad_len(init).add(1)

main = bad_len([1, 2, 3, 5])
"""),
      "A",
      VInt(4)
    )
  }
  test("uncurry2") {
    evalTest(
      List("""
package A

struct TwoVar(one, two)

constructed = uncurry2(x -> y -> TwoVar(x, y))(1, "two")

main = match constructed:
  case TwoVar(1, "two"): "good"
  case _: "bad"
"""),
      "A",
      Str("good")
    )
  }
  test("uncurry3") {
    evalTest(
      List("""
package A

struct ThreeVar(one, two, three)

constructed = uncurry3(x -> y -> z -> ThreeVar(x, y, z))(1, "two", 3)

main = match constructed:
  case ThreeVar(1, "two", 3): "good"
  case _: "bad"
"""),
      "A",
      Str("good")
    )
  }

  test("Dict methods") {
    evalTest(
      List("""
package A

e = empty_Dict(string_Order)

e1 = e.add_key("hello", "world")

main = e1.get_key("hello")
"""),
      "A",
      VOption.some(Str("world"))
    )

    evalTest(
      List("""
package A

e = empty_Dict(string_Order)

e1 = e.clear_Dict().add_key("hello2", "world2")

main = e1.get_key("hello")
"""),
      "A",
      VOption.none
    )

    evalTest(
      List("""
package A

e = empty_Dict(string_Order)

e1 = e.add_key("hello", "world")
e2 = e1.remove_key("hello")

main = e2.get_key("hello")
"""),
      "A",
      VOption.none
    )

    evalTest(
      List("""
package A

e1 = empty_Dict(string_Order)
e2 = e1.add_key("hello", "world").add_key("hello1", "world1")
lst = e2.items()

main = match lst:
  case [("hello", "world"), ("hello1", "world1")]: "good"
  case _: "bad"
"""),
      "A",
      Str("good")
    )

    evalTest(
      List("""
package A

e1 = {}
e2 = e1.add_key("hello", "world").add_key("hello1", "world1")
lst = e2.items()

main = match lst:
  case [("hello", "world"), ("hello1", "world1")]: "good"
  case _: "bad"
"""),
      "A",
      Str("good")
    )

    evalTest(
      List("""
package A

e = {
      "hello": "world",
      "hello1":
        "world1" }
lst = e.items()

main = match lst:
  case [("hello", "world"), ("hello1", "world1")]: "good"
  case _: "bad"
"""),
      "A",
      Str("good")
    )

    evalTest(
      List("""
package A

pairs = [("hello", "world"), ("hello1", "world1")]

e = { k: v for (k, v) in pairs }
lst = e.items()

main = match lst:
  case [("hello", "world"), ("hello1", "world1")]: "good"
  case _: "bad"
"""),
      "A",
      Str("good")
    )

    evalTest(
      List("""
package A

pairs = [("hello", 42), ("hello1", 24)]

def is_hello(s):
  s.cmp_String("hello") matches EQ

e = { k: v for (k, v) in pairs if is_hello(k) }
lst = e.items()

main = match lst:
  case [("hello", res)]: res
  case _: -1
"""),
      "A",
      VInt(42)
    )

    evalTestJson(
      List("""
package Foo

bar = {'a': '1', 's': 'foo' }

main = bar
"""),
      "Foo",
      Json.JObject(List("a" -> Json.JString("1"), "s" -> Json.JString("foo")))
    )

    evalTestJson(
      List("""
package Foo

# we need a static type to convert to json
bar: Dict[String, Option[Int]] = {'a': None, 's': None }

main = bar
"""),
      "Foo",
      Json.JObject(List("a" -> Json.JNull, "s" -> Json.JNull))
    )

    evalTestJson(
      List("""
package Foo

bar = {'a': None, 's': Some(1) }

main = bar
"""),
      "Foo",
      Json.JObject(List("a" -> Json.JNull, "s" -> Json.JNumberStr("1")))
    )

    evalTestJson(
      List("""
package Foo

bar = {'a': [], 's': [1] }

main = bar
"""),
      "Foo",
      Json.JObject(
        List(
          "a" -> Json.JArray(Vector.empty),
          "s" -> Json.JArray(Vector(Json.JNumberStr("1")))
        )
      )
    )

    evalTestJson(
      List("""
package Foo

bar = {'a': True, 's': False }

main = bar
"""),
      "Foo",
      Json.JObject(List("a" -> Json.JBool(true), "s" -> Json.JBool(false)))
    )

    evalTestJson(
      List("""
package Foo

main = 1.25
"""),
      "Foo",
      Json.JNumberStr("1.25")
    )

    evalTestJson(
      List("""
package Foo

main = (1, "1", ())
"""),
      "Foo",
      Json.JArray(Vector(Json.JNumberStr("1"), Json.JString("1"), Json.JNull))
    )

    evalTestJson(
      List("""
package Foo

main = [Some(Some(1)), Some(None), None]
"""),
      "Foo",
      Json.JArray(
        Vector(
          Json.JArray(Vector(Json.JNumberStr("1"))),
          Json.JArray(Vector(Json.JNull)),
          Json.JArray(Vector.empty)
        )
      )
    )

    evalTestJson(
      List("""
package Foo

enum FooBar: Foo(foo), Bar(bar)

main = [Foo(1), Bar("1")]
"""),
      "Foo",
      Json.JArray(
        Vector(
          Json.JObject(List("foo" -> Json.JNumberStr("1"))),
          Json.JObject(List("bar" -> Json.JString("1")))
        )
      )
    )
  }

  test("json handling of Nat special case") {
    evalTestJson(
      List("""
package Foo

enum Nat: Z, S(n: Nat)

main = [Z, S(Z), S(S(Z))]
"""),
      "Foo",
      Json.JArray(
        Vector(Json.JNumberStr("0"), Json.JNumberStr("1"), Json.JNumberStr("2"))
      )
    )
  }

  test("json with backticks") {
    evalTestJson(
      List("""
package Foo

struct Foo(`struct`, `second key`, `enum`, `def`)

`package` = 2

main = Foo(1, `package`, 3, 4)
"""),
      "Foo",
      Json.JObject(
        List(
          ("struct" -> Json.JNumberStr("1")),
          ("second key" -> Json.JNumberStr("2")),
          ("enum" -> Json.JNumberStr("3")),
          ("def" -> Json.JNumberStr("4"))
        )
      )
    )
  }

  test("test operators") {
    evalTest(
      List("""
package A

operator + = add
operator * = mul

main = 1 + 2 * 3
"""),
      "A",
      VInt(7)
    )
  }

  test("patterns in lambdas") {
    runBosatsuTest(
      List("""
package A

# you can't write x: Int -> x.add(1)
# since Int -> looks like a type
# you need to protect it in a ( )
inc: Int -> Int = x -> x.add(1)
inc2: Int -> Int = (x: Int) -> x.add(1)

test = Assertion(inc(1).eq_Int(inc2(1)), "inc(1) == 2")
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""
package A

def inc(x: Int): x.add(1)

test = Assertion(inc(1).eq_Int(2), "inc(1) == 2")
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""
package A

struct Foo(v)

inc = Foo(x) -> x.add(1)

test0 = Assertion(inc(Foo(1)).eq_Int(2), "inc(Foo(1)) == 2")

enum FooBar: F(x), B(x)

inc2 = (F(x) | B(x), Foo(y)) -> x.add(y)
test1 = Assertion(inc2(F(1), Foo(1)).eq_Int(2), "inc2(F(1), Foo(1)) == 2")
test2 = Assertion(inc2(B(1), Foo(1)).eq_Int(2), "inc2(B(1), Foo(1)) == 2")

# with an outer tuple wrapping
inc3 = ((F(x) | B(x), Foo(y))) -> x.add(y)
test3 = Assertion(inc3((F(1), Foo(1))).eq_Int(2), "inc3((F(1), Foo(1))) == 2")
test4 = Assertion(inc3((B(1), Foo(1))).eq_Int(2), "inc3((B(1), Foo(1))) == 2")

# with a custom struct
struct Pair(x, y)
inc4 = Pair(F(x) | B(x), Foo(y)) -> x.add(y)
test5 = Assertion(inc4(Pair(F(1), Foo(1))).eq_Int(2), "inc4(Pair(F(1), Foo(1))) == 2")
test6 = Assertion(inc4(Pair(B(1), Foo(1))).eq_Int(2), "inc4(Pair(B(1), Foo(1))) == 2")

suite = TestSuite("match tests", [test0, test1, test2, test3, test4, test5, test6])
"""),
      "A",
      7
    )

    runBosatsuTest(
      List("""
package A

struct Foo(v)

def inc(Foo(x)): x.add(1)

test0 = Assertion(inc(Foo(1)).eq_Int(2), "inc(Foo(1)) == 2")

enum FooBar: F(x), B(x)

def inc2(F(x) | B(x), Foo(y)): x.add(y)
test1 = Assertion(inc2(F(1), Foo(1)).eq_Int(2), "inc2(F(1), Foo(1)) == 2")
test2 = Assertion(inc2(B(1), Foo(1)).eq_Int(2), "inc2(B(1), Foo(1)) == 2")

# with an outer tuple wrapping
def inc3((F(x) | B(x), Foo(y))): x.add(y)
test3 = Assertion(inc3((F(1), Foo(1))).eq_Int(2), "inc3((F(1), Foo(1))) == 2")
test4 = Assertion(inc3((B(1), Foo(1))).eq_Int(2), "inc3((B(1), Foo(1))) == 2")

# with a custom struct
struct Pair(x, y)
def inc4(Pair(F(x) | B(x), Foo(y))): x.add(y)
test5 = Assertion(inc4(Pair(F(1), Foo(1))).eq_Int(2), "inc4(Pair(F(1), Foo(1))) == 2")
test6 = Assertion(inc4(Pair(B(1), Foo(1))).eq_Int(2), "inc4(Pair(B(1), Foo(1))) == 2")

suite = TestSuite("match tests", [test0, test1, test2, test3, test4, test5, test6])
"""),
      "A",
      7
    )
  }

  test("pattern example from pair to triple") {
    runBosatsuTest(
      List("""
package A

struct Pair(f, s)
struct Trip(f, s, t)

Trip(a, b, c) = match Pair(1, "two"):
  case Pair(f, s): Trip(3, s, f)

bgood = match b:
  case "two": True
  case _: False

tests = TestSuite("test triple",
  [ Assertion(a.eq_Int(3), "a == 3"),
    Assertion(bgood, b),
    Assertion(c.eq_Int(1), "c == 1") ])
"""),
      "A",
      3
    )
  }

  test("regression from a map_List/list comprehension example from snoble") {
    runBosatsuTest(
      List("""
package RecordSet/Library

enum RowEntry[w]:
  REBool(value: w[Bool])
  REInt(value: w[Int])
  REString(value: w[String])

struct RecordField[t](name: String, to_entry: forall w: * -> *. w[t] -> RowEntry[w])
struct RecordValue[t](value: t)
struct RecordGetter[shape, t](
  field: shape[RecordField] -> RecordField[t],
  value: shape[RecordValue] -> RecordValue[t]
)
struct RecordRowEntry[w, t](row_entry: RowEntry[w])

struct RecordSet[shape](
  fields: shape[RecordField],
  rows: List[shape[RecordValue]],
  getters: shape[RecordGetter[shape]],
  traverse: forall w1: * -> *, w2: * -> *. shape[w1] -> (forall ss. w1[ss] -> w2[ss]) -> shape[w2],
  record_to_list: forall w: * -> *. shape[RecordRowEntry[w]] -> List[RowEntry[w]]
)

def get[shape: (* -> *) -> *, t](sh: shape[RecordValue], RecordGetter(_, getter): RecordGetter[shape, t]) -> t:
  RecordValue(result) = sh.getter()
  result

def create_field[shape: (* -> *) -> *, t](rf: RecordField[t], fn: shape[RecordValue] -> t):
 RecordGetter(_ -> rf, sh -> RecordValue(fn(sh)))

def list_of_rows[shape: (* -> *) -> *](RecordSet(fields, rows, getters, traverse, record_to_list): RecordSet[shape]):
  def getter_to_row_entry(row: shape[RecordValue]):
    (result_fn: forall tt. RecordGetter[shape, tt] -> RecordRowEntry[RecordValue, tt]) = RecordGetter(get_field, get_value) ->
      RecordField(_, to_entry) = get_field(fields)
      RecordRowEntry(to_entry(get_value(row)))
    result_fn
  # This code should work the same if, map_List or list comprehension, but didn't previously
  #rows.map_List(row -> record_to_list(getters.traverse(getter_to_row_entry(row))))
  [record_to_list(getters.traverse()(getter_to_row_entry(row))) for row in rows]

struct RestructureOutput[shape1, shape2](
  reshaperFields: shape1[RecordField] -> shape2[RecordField],
  reshaperValues: shape1[RecordValue] -> shape2[RecordValue],
  getters: shape2[RecordGetter[shape2]],
  traverse: forall w1: * -> *, w2: * -> *. shape2[w1] -> (forall ss. w1[ss] -> w2[ss]) -> shape2[w2],
  record_to_list: forall w: * -> *. shape2[RecordRowEntry[w]] -> List[RowEntry[w]]
)
def restructure[
    shape1: (* -> *) -> *,
    shape2: (* -> *) -> *
  ](RecordSet(fields, rows, getters, _, _): RecordSet[shape1], f: shape1[RecordGetter[shape1]] -> RestructureOutput[shape1, shape2]) -> RecordSet[shape2]:
  RestructureOutput(reshaperF, reshaperV, new_getters, traverse, record_to_list) = f(getters)
  RecordSet(reshaperF(fields), rows.map_List(reshaperV), new_getters, traverse, record_to_list)

def concat_records(RecordSet(fields, rows, getters, traverse, record_to_list), more_rows):
  RecordSet(fields, rows.concat(more_rows), getters, traverse, record_to_list)

struct NilShape[w: * -> *]
struct PS[t, rest, w](left: w[t], right: rest[w])

new_record_set = RecordSet(NilShape, [], NilShape, NilShape -> _ -> NilShape, NilShape -> [])

(ps_end: forall t: (* -> *) -> *. RestructureOutput[t, NilShape]) = RestructureOutput(
  _ -> NilShape,
  _ -> NilShape,
  NilShape,
  _ -> _ -> NilShape,
  _ -> []
)

def ps[
    shape1: (* -> *) -> *,
    shape2: (* -> *) -> *,
    t
](
  RecordGetter(fF, fV): RecordGetter[shape1, t],
  RestructureOutput(reshaper1F, reshaper1V, getters1, traverse1, record_to_list1): RestructureOutput[shape1, shape2]):
  getters2 = getters1.traverse1()(RecordGetter(f1, v1) -> RecordGetter(PS(_, sh2) -> f1(sh2), PS(_, sh2) -> v1(sh2)))
  RestructureOutput(
    sh1 -> PS(fF(sh1), reshaper1F(sh1)),
    sh1 -> PS(fV(sh1), reshaper1V(sh1)),
    PS(RecordGetter(PS(x,_) -> x, PS(x,_) -> x), getters2),
    PS(x, sh2) -> g -> PS(g(x), sh2.traverse1()(g)),
    PS(RecordRowEntry(row_entry), sh2) -> [row_entry].concat(record_to_list1(sh2))
  )

def string_field[shape: (* -> *) -> *](name, fn: shape[RecordValue] -> String): RecordField(name, REString).create_field(fn)
def int_field[shape: (* -> *) -> *](name, fn: shape[RecordValue] -> Int): RecordField(name, REInt).create_field(fn)
def bool_field[shape: (* -> *) -> *](name, fn: shape[RecordValue] -> Bool): RecordField(name, REBool).create_field(fn)

##################################################

def and(x, y):
  y if x else False

operator && = and

def equals(compare, x, y):
  compare(x,y) matches EQ

def cmp_Bool(x, y):
  match (x, y):
    case (True, False): GT
    case (False, True): LT
    case _: EQ

def equal_List(is_equal, l1, l2):
  recur l1:
    case []: match l2:
      case []: True
      case _: False
    case [h1, *r1]: match l2:
      case []: False
      case [h2, *r2]: is_equal(h1, h2) && equal_List(is_equal, r1, r2)

def equal_RowEntry(re1, re2):
  match (re1, re2):
    case (REBool(RecordValue(x1)), REBool(RecordValue(x2))): cmp_Bool.equals(x1, x2)
    case (REInt(RecordValue(x1)), REInt(RecordValue(x2))): cmp_Int.equals(x1, x2)
    case (REString(RecordValue(x1)), REString(RecordValue(x2))): cmp_String.equals(x1, x2)
    case _: False

equal_rows = (a, b) -> equal_List(equal_RowEntry, a, b)

##################################################

rs_empty = new_record_set.restructure(
  _ -> ps("String field".string_field(_ -> ""),
  ps("Int field".int_field(_ -> 0),
  ps("Bool field".bool_field(_ -> True),
  ps_end))))

rs = rs_empty.concat_records([PS(RecordValue("a"), PS(RecordValue(1), PS(RecordValue(False), NilShape)))])

rs0 = rs.restructure(PS(a, PS(b, PS(c, _))) -> ps(c, ps(b, ps(a, ps("Plus 2".int_field( r -> r.get(b).add(2) ), ps_end)))))

tests = TestSuite("reordering",
  [
    Assertion(equal_rows.equal_List(rs0.list_of_rows(), [[REBool(RecordValue(False)), REInt(RecordValue(1)), REString(RecordValue("a")), REInt(RecordValue(3))]]), "swap")
  ]
)
"""),
      "RecordSet/Library",
      1
    )
  }

  test("exercise total matching inside of a struct with a list") {
    runBosatsuTest(
      List("""package A

struct ListWrapper(items: List[a], b: Bool)

w = ListWrapper([], True)

ListWrapper([*_], r) = w

tests = Assertion(r, "match with total list pattern")
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""package A

struct ListWrapper2(items: List[a], others: List[b], b: Bool)

w = ListWrapper2([], [], True)

ListWrapper2(_, _, r) = w

tests = Assertion(r, "match with total list pattern")
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""package A

struct ListWrapper(items: List[(a, b)], b: Bool)

w = ListWrapper([], True)

ListWrapper(_, r) = w

tests = Assertion(r, "match with total list pattern")
"""),
      "A",
      1
    )
  }

  test("test scoping bug (issue #311)") {

    runBosatsuTest(
      List("""package A

struct Foo(x, y)

Foo { x, ... } = Foo(42, "42")

tests = TestSuite("test record",
  [
    Assertion(x.eq_Int(42), "x == 42"),
  ])
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""package A

struct Foo(x, y)

a = Foo(42, "42")
x = match a:
  case Foo(y, _): y

tests = TestSuite("test record",
  [
    Assertion(x.eq_Int(42), "x == 42"),
  ])
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""package A

struct Foo(x, y)

a = Foo(42, "42")
x = match a:
  case Foo(y, _): y

def add_x(a):
  # note x has a closure over a, but
  # evaluated here the local a might
  # shadow in the case of the bug
  add(a, x)

# should be 43
y = add_x(1)

tests = TestSuite("test record",
  [
    Assertion(y.eq_Int(43), "y == 43"),
  ])
"""),
      "A",
      1
    )

  }

  test("test ordered shadowing issue #328") {
    runBosatsuTest(
      List("""package A

one = 1

two = one.add(1)

one = "one"

good = match (one, two):
  case ("one", 2): True
  case _:     False

tests = TestSuite("test",
  [
    Assertion(good, ""),
  ])
"""),
      "A",
      1
    )

    // test record syntax
    runBosatsuTest(
      List("""package A

struct Foo(one)

one = 1

two = one.add(1)

foo = Foo { one }

one = "one"

good = match (one, two, foo):
  case ("one", 2, Foo(1)): True
  case _:     False

tests = TestSuite("test",
  [
    Assertion(good, ""),
  ])
"""),
      "A",
      1
    )

    // test local shadowing of a duplicate
    runBosatsuTest(
      List("""package A

one = 1

two = one.add(1)

incA = one -> one.add(1)
def incB(one): one.add(1)

one = "one"

good = match (one, two, incA(0), incB(1)):
  case ("one", 2, 1, 2): True
  case _:     False

tests = TestSuite("test",
  [
    Assertion(good, ""),
  ])
"""),
      "A",
      1
    )

    // test an example using a predef function, like add
    runBosatsuTest(
      List("""package A
export add

# this should be add from predef
two = add(1, 1)

def add(x, y):
  x.sub(y)

good = match two:
  case 2: True
  case _:     False

tests = TestSuite("test",
  [
    Assertion(good, ""),
  ])
"""),
      "A",
      1
    )
  }

  test("test meta escape bug") {
    runBosatsuTest(
      List("""
package A

struct Build[f]
struct File

def useList(args: List[Build[File]]):
  # ignore args
  _ = args
  True

check = useList([])

tests = Assertion(check, "none")
"""),
      "A",
      1
    )
  }

  test("test parsing type annotations") {
    runBosatsuTest(
      List("""
package A

x: Int = 1

y = (
  z: Int = x
  z
)

tests = Assertion(y.eq_Int(x), "none")
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""
package A

x: Int = 1

y = (
  z: Int = x
  z: Int
)

tests = Assertion(y.eq_Int(x), "none")
"""),
      "A",
      1
    )
  }

  test("improve coverage of typedexpr normalization") {
    runBosatsuTest(
      List("""
package A

enum MyBool: T, F
main = match T:
  case T: (x -> x)(True)
  case F: False

tests = Assertion(main, "t1")
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""
package A

f = _ -> True

fn = x -> f(x)

# trigger an optimization to remove y
tests = Assertion(fn((y = 1
# ignore y
_ = y
2)), "t1")
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""
package A

def inc(x):
  w = 1
  y = x
  z = y
  y = w
  z.add(y)

tests = Assertion(inc(1).eq_Int(2), "t1")
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""
package A

w = 1

def inc(x):
  match w:
    case 1:
      y = x
      z = y
      y = w
      z.add(y)
    case x: x

tests = Assertion(inc(1).eq_Int(2), "t1")
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""
package QueueTest

struct Queue[a](front: List[a], back: List[a])

def fold_Queue(Queue(f, b): Queue[a], binit: b, fold_fn: (b, a) -> b) -> b:
  front = f.foldl_List(binit, fold_fn)
  b.reverse().foldl_List(front, fold_fn)

test = Assertion(Queue([1], [2]).fold_Queue(0, add).eq_Int(3), "foldQueue")
"""),
      "QueueTest",
      1
    )

    runBosatsuTest(
      List("""
package A

three = (x = 1
y -> x.add(y))(2)

test = Assertion(three.eq_Int(3), "let inside apply")
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""
package A

substitute = (
  x = 40
  y = x.add(1)
  y.add(1)
)

test = Assertion(substitute.eq_Int(42), "basis substitution")
"""),
      "A",
      1
    )
  }

  test("we can use .( ) to get |> like syntax for lambdas") {
    runBosatsuTest(
      List("""
package A

three = 2.(x -> add(x, 1))()

test = Assertion(three.eq_Int(3), "let inside apply")
"""),
      "A",
      1
    )
  }

  test("recursion check with _ pattern: issue 573") {
    runBosatsuTest(
      List("""
package VarSet/Recursion

test = Assertion(True, "")

enum Thing:
  Thing1, Thing2(a: Int, t: Thing)

def bar(y, _: String, x):
  recur x:
    case Thing1: y
    case Thing2(i, t): bar(i, "boom", t)
"""),
      "VarSet/Recursion",
      1
    )
  }

  test("test some complex list patterns, issue 574") {
    runBosatsuTest(
      List("""
package Foo

out = match [(True, 2), (True, 0)]:
  case [*_, (True, x), *_, (False, _)]: x
  case [*_, (True, _), *_, (_, y)]: y
  case _: -1

test = Assertion(out.eq_Int(0), "")
"""),
      "Foo",
      1
    )
  }

  test("Match on constructors from another package") {
    runBosatsuTest(
      """
package Foo

export FooE()

enum FooE: Foo1, Foo2
""" ::
        """
package Bar

from Foo import Foo1, Foo2

x = Foo1
m = match x:
      case Foo1: True
      case Foo2: False

test = Assertion(m, "x matches Foo1")
""" :: Nil,
      "Bar",
      1
    )
  }

  test("ill-kinded functions fail to compile") {

    evalFail(List("""
package Foo

struct RecordValue[t](value: t)
struct RecordGetter[shape, t](
  value: shape[RecordValue] -> RecordValue[t]
)

# shape is (* -> *) -> *
def get[shape](sh: shape[RecordValue], RecordGetter(getter): RecordGetter[shape, t]) -> t:
  RecordValue(result) = sh.getter()
  result
""")) { case _: PackageError.TypeErrorIn => () }
  }

  test("test quicklook example") {
    runBosatsuTest(
      List("""
package Foo

export comp, ignore

def f(fn: forall a. List[a] -> List[a]) -> Int:
  fn([1]).foldl_List(0, (x, _) -> x.add(1))

def g(b: Bool) -> (forall a. List[a] -> List[a]):
  match b:
    case True: x -> x
    case False: _ -> []

h = b -> f(g(b))

def id(a): a
def single(a): [a]    

def foo1(fn) -> Int:
  fn.foldl_List(0, (x, _) -> x.add(1))

def foo2(fn: List[forall a. a -> a]) -> Int:
  fn.foldl_List(0, (x, _) -> x.add(1))

count0 = foo1(single(id))
count = foo2(single(id))

single_id1: forall a. List[a -> a] = single(id)
single_id2: List[forall a. a -> a] = single(id)

struct Pair1(fst: a, snd: a)

pair = Pair1(single_id1, single_id2)

comp = x -> f(g(x))

ignore: exists a. a = (pair, h, count, foo1, count0)

test = Assertion(True, "")
"""),
      "Foo",
      1
    )
  }

  test("example from issue #264") {
    runBosatsuTest(
      """
package SubsumeTest

def lengths(l1: List[Int], l2: List[String], maybeFn: Option[forall tt. List[tt] -> Int]):
  match maybeFn:
    case Some(fn): fn(l1).add(fn(l2))
    case None: 0

def lengths2(l1: List[Int], l2: List[String], maybeFn: forall tt. Option[List[tt] -> Int]):
  match maybeFn:
    case Some(fn): fn(l1).add(fn(l2))
    case None: 0

test = Assertion(lengths([], [], None).add(lengths2([], [], None)) matches 0, "test")

# this is a test that doesn't forget that we have the empty list:
x = match []:
      case []: 0
      case [h, *_]: (h: forall a. a)
    """ :: Nil,
      "SubsumeTest",
      1
    )
  }

  test("declaring a generic parameter works fine") {
    runBosatsuTest(
      List("""
package Generic

enum NEList[a: +*]:
  One(head: a)
  Many(head: a, tail: NEList[a])  

def head(nel: NEList[a]) -> a:
  match nel:
    case One(a) | Many(a, _): a

test = Assertion(head(One(True)), "")
"""),
      "Generic",
      1
    )

    runBosatsuTest(
      List("""
package Generic

enum NEList[a: +*]:
  One(head: a)
  Many(head: a, tail: NEList[a])  

def head[a](nel: NEList[a]) -> a:
  match nel:
    case One(a) | Many(a, _): a

test = Assertion(head(One(True)), "")
"""),
      "Generic",
      1
    )

    // With recursion
    runBosatsuTest(
      List("""
package Generic

enum NEList[a: +*]:
  One(head: a)
  Many(head: a, tail: NEList[a])  

def last(nel: NEList[a]) -> a:
  recur nel:
    case One(a): a
    case Many(_, tail): last(tail)

test = Assertion(last(One(True)), "")
"""),
      "Generic",
      1
    )

    // With recursion
    runBosatsuTest(
      List("""
package Generic

enum NEList[a: +*]:
  One(head: a)
  Many(head: a, tail: NEList[a])  

def last[a](nel: NEList[a]) -> a:
  recur nel:
    case One(a): a
    case Many(_, tail): last(tail)

test = Assertion(last(One(True)), "")
"""),
      "Generic",
      1
    )
  }

  test("support polymorphic recursion") {
    runBosatsuTest(
      List("""
package PolyRec

test = Assertion(True, "")         

enum Nat: NZero, NSucc(n: Nat)

def poly_rec(count: Nat, a: a) -> a:
    recur count:
        case NZero: a
        case NSucc(prev):
          # make a call with a different type
          (_, b) = poly_rec(prev, ("foo", a))
          b
"""),
      "PolyRec",
      1
    )

    runBosatsuTest(
      List("""
package PolyRec

test = Assertion(True, "")         

enum Nat: NZero, NSucc(n: Nat)

def call(a):
    def poly_rec(count: Nat, a: a) -> a:
        recur count:
            case NZero: a
            case NSucc(prev):
              # make a call with a different type
              (_, b) = poly_rec(prev, ("foo", a))
              b
    # call a polymorphic recursion internally to exercise different code paths
    poly_rec(NZero, a)
"""),
      "PolyRec",
      1
    )

  }

  test("polymorphic recursion runs in non-tail position") {
    evalTest(
      List("""
package PolyRec

enum Nat: Z, S(prev: Nat)

struct Box[a](value: a)

def box_more[a](count: Nat, box: Box[a]) -> Nat:
  recur count:
    case Z: Z
    case S(prev): S(box_more(prev, Box(box)))

def to_int(n: Nat) -> Int:
  recur n:
    case Z: 0
    case S(prev): to_int(prev).add(1)

start = S(S(S(Z)))
main = to_int(box_more(start, Box("x")))
"""),
      "PolyRec",
      VInt(3)
    )
  }

  test("recursion on continuations") {
    evalTest(
      List("""
package A
enum Cont:
  Item(a: Int)
  Next(use: (Cont -> Int) -> Int)

def map(ca: Cont, fn: Int -> Int) -> Cont:
  Next(cont -> fn(cont(ca)))

b = Item(1).map(x -> x.add(1))

def loop(box: Cont) -> Int:
  recur box:
    case Item(a): a
    case Next(cont_fn):
      cont_fn(cont -> loop(cont))

v = loop(b)
main = v
"""),
      "A",
      VInt(2)
    )

    // Generic version
    evalTest(
      List("""
package A

export ignore

enum Cont[a: *]:
  Item(a: a)
  Next(use: (Cont[a] -> a) -> a)

def map[a](ca: Cont[a], fn: a -> a) -> Cont[a]:
  Next(cont -> fn(cont(ca)))

def loop[a](box: Cont[a]) -> a:
  recur box:
    case Item(a): a
    case Next(cont_fn): cont_fn(loop)

loopgen: forall a. Cont[a] -> a = loop

ignore: exists a. a = loopgen

b: Cont[Int] = Item(1).map(x -> x.add(1))
main: Int = loop(b)
"""),
      "A",
      VInt(2)
    )

    // this example also exercises polymorphic recursion
    evalTest(
      List("""
package A
enum Box[a: +*]:
  Item(a: a)
  Next(fn: forall res. (forall b. (Box[b], b -> a) -> res) -> res)

b = Item(1)

def loop[a](box: Box[a]) -> a:
  recur box:
    case Item(a): a
    case Next(cont): cont((bx, fn) -> fn(loop(bx)))

v = loop(b)
main = v
"""),
      "A",
      VInt(1)
    )
  }

  test("test character literals") {
    runBosatsuTest(
      List("""
package Foo

good1 = match .'x':
  case .'y': False
  case .'x': True
  case _: False

test1 = Assertion(good1, "simple match")

just_x = .'x'
good2 = match "$.{just_x}":
  case "$.{x}": x matches .'x'
  case _: False

test2 = Assertion(good2, "interpolation match")

def last(str) -> Option[Char]:
  match str:
    case "": None
    case "${_}$.{c}": Some(c)

test3 = Assertion(last("foo") matches Some(.'o'), "last test")   
all = TestSuite("chars", [test1, test2, test3])
"""),
      "Foo",
      3
    )
  }

  test("test universal quantified list match") {
    runBosatsuTest(
      List("""
package Foo

empty: (forall a. List[a]) = []

res = match empty:
  case []: 0
  case [_, *_]: 1

test = Assertion(res matches 0, "one")
"""),
      "Foo",
      1
    )

  }

  test("existential quantification in a match") {
    runBosatsuTest(
      List("""
package Foo
export maybeMapped, FreeF

enum FreeF[a]:
  Pure(a: a)
  Mapped(tup: exists b. (FreeF[b], b -> a))

def pure(a: a) -> FreeF[a]: Pure(a)
def map[a, b](f: FreeF[a], fn: a -> b) -> FreeF[b]:
  tup: exists x. (FreeF[x], x -> b) = (f, fn)
  Mapped(tup)

def maybeMapped[a](f: FreeF[a]) -> exists b. Option[(FreeF[b], b -> a)]:
  match f:
    case Mapped(e_print): Some(e_print)
    case _: None

def run[a](fa: FreeF[a]) -> a:
  recur fa:
    case Pure(a): a
    case Mapped((prev, fn)):
      fn(run(prev))

res = run(pure(0).map(x -> x.add(1)))
test = Assertion(res matches 1, "one")
"""),
      "Foo",
      1
    )

    runBosatsuTest(
      List("""
package Foo

enum ListE[a]:
  Empty
  Cons(a: a, tail: exists b. (b, ListE[b]))

def cons[a, b](tup: (a, b), tail: ListE[b]) -> ListE[a]:
  (a, b) = tup
  Cons(a, (b, tail))

def uncons[a](l: ListE[a]) -> exists b. Option[((a, b), ListE[b])]: 
  match l:
    case Empty: None
    case Cons(a, (b, t)): Some(((a, b), t))

res = cons((1, 0), Empty).uncons()
test = Assertion(res matches Some(((1, _), Empty)), "one")
"""),
      "Foo",
      1
    )
  }

  test("existential quantification in enum branch type params") {
    runBosatsuTest(
      List("""
package Foo
export maybeMapped, FreeF

enum FreeF[a]:
  Pure(a: a)
  Mapped[b](prev: FreeF[b], fn: b -> a)

def pure(a: a) -> FreeF[a]: Pure(a)
def map[a, b](f: FreeF[a], fn: a -> b) -> FreeF[b]:
  Mapped(f, fn)

def maybeMapped[a](f: FreeF[a]) -> exists b. Option[(FreeF[b], b -> a)]:
  match f:
    case Mapped(prev, fn): Some((prev, fn))
    case _: None

def run[a](fa: FreeF[a]) -> a:
  recur fa:
    case Pure(a): a
    case Mapped(prev, fn):
      fn(run(prev))

res = run(pure(0).map(x -> x.add(1)))
test = Assertion(res matches 1, "one")
"""),
      "Foo",
      1
    )
  }

  test("test non-base 10 literals") {
    runBosatsuTest(
      List("""
package Foo

test = TestSuite("bases",
  [
    Assertion(0x10 matches 16, "1"),
    Assertion(0X10 matches 16, "2"),
    Assertion(0o10 matches 8, "3"),
    Assertion(0O10 matches 8, "4"),
    Assertion(0b10 matches 2, "5"),
    Assertion(0B10 matches 2, "6"),
    Assertion(16 matches 0x10, "7"),
    Assertion(16 matches 0X10, "8"),
    Assertion(8 matches 0o10, "9"),
    Assertion(8 matches 0O10, "10"),
    Assertion(2 matches 0b10, "11"),
    Assertion(2 matches 0B10, "12"),
  ])
"""),
      "Foo",
      12
    )
  }

  test("test nested if matches") {
    runBosatsuTest(
      List("""
package Foo

export fn

def fn(x):
  if x matches True: False
  elif x matches False: True
  else: False

test = Assertion(fn(False), "")
"""),
      "Foo",
      1
    )
  }

  test("array externals evaluate") {
    runBosatsuTest(
      List("""
package Bosatsu/Collection/Array

external struct Array[a: +*]

external empty_Array: forall a. Array[a]
external def tabulate_Array[a](n: Int, fn: Int -> a) -> Array[a]
external def from_List_Array[a](xs: List[a]) -> Array[a]
external def to_List_Array[a](ary: Array[a]) -> List[a]
external def size_Array[a](ary: Array[a]) -> Int
external def get_map_Array[a, b](ary: Array[a], idx: Int, default: Unit -> b, fn: a -> b) -> b
external def get_or_Array[a](ary: Array[a], idx: Int, default: Unit -> a) -> a
external def foldl_Array[a, b](ary: Array[a], init: b, fn: (b, a) -> b) -> b
external def map_Array[a, b](ary: Array[a], fn: a -> b) -> Array[b]
external def set_or_self_Array[a](ary: Array[a], idx: Int, value: a) -> Array[a]
external def sort_Array[a](ary: Array[a], fn: (a, a) -> Comparison) -> Array[a]
external def concat_all_Array[a](arrays: List[Array[a]]) -> Array[a]
external def slice_Array[a](ary: Array[a], start: Int, end: Int) -> Array[a]

def get_Array[a](ary: Array[a], idx: Int) -> Option[a]:
  get_map_Array(ary, idx, _ -> None, x -> Some(x))

def cmp_pair(left: (Int, String), right: (Int, String)) -> Comparison:
  (li, _) = left
  (ri, _) = right
  cmp_Int(li, ri)

a5 = tabulate_Array(5, i -> i)

tests = TestSuite("array eval", [
  Assertion(to_List_Array(a5) matches [0, 1, 2, 3, 4], "tabulate"),
  Assertion(size_Array(a5) matches 5, "size"),
  Assertion(get_Array(a5, 2) matches Some(2), "get some"),
  Assertion(get_Array(a5, -1) matches None, "get none"),
  Assertion(get_or_Array(a5, 20, _ -> 10) matches 10, "get_or"),
  Assertion(foldl_Array(a5, 0, add) matches 10, "fold"),
  Assertion(to_List_Array(map_Array(a5, x -> x.add(1))) matches [1, 2, 3, 4, 5], "map"),
  Assertion(to_List_Array(set_or_self_Array(a5, 1, 9)) matches [0, 9, 2, 3, 4], "set in range"),
  Assertion(to_List_Array(slice_Array(a5, 1, 4)) matches [1, 2, 3], "slice"),
  Assertion(to_List_Array(slice_Array(a5, -2, 2)) matches [0, 1], "slice clamp"),
  Assertion(to_List_Array(slice_Array(a5, 4, 1)) matches [], "slice invalid"),
  Assertion(to_List_Array(concat_all_Array([a5, tabulate_Array(2, i -> i)])) matches [0, 1, 2, 3, 4, 0, 1], "concat"),
  Assertion(
    to_List_Array(
      sort_Array(
        from_List_Array([(1, "a"), (0, "z"), (1, "b"), (1, "c")]),
        cmp_pair
      )
    ) matches [(0, "z"), (1, "a"), (1, "b"), (1, "c")],
    "sort"
  ),
])
"""),
      "Bosatsu/Collection/Array",
      13
    )
  }

  test("bytes externals evaluate") {
    runBosatsuTest(
      List(
        """
package Bosatsu/Collection/Array

export (
  Array,
  from_List_Array,
  to_List_Array,
)

external struct Array[a: +*]

external def from_List_Array[a](xs: List[a]) -> Array[a]
external def to_List_Array[a](ary: Array[a]) -> List[a]
""",
        """
package Bosatsu/IO/Bytes

from Bosatsu/Collection/Array import Array, from_List_Array, to_List_Array

external struct Bytes

external empty_Bytes: Bytes
external def from_List_Int(ints: List[Int]) -> Bytes
external def from_Array_Int(ints: Array[Int]) -> Bytes
external def to_List_Int(bytes: Bytes) -> List[Int]
external def to_Array_Int(bytes: Bytes) -> Array[Int]
external def size_Bytes(bytes: Bytes) -> Int
external def get_map_Bytes[a](bytes: Bytes, idx: Int, default: Unit -> a, fn: Int -> a) -> a
external def get_or_Bytes(bytes: Bytes, idx: Int, default: Unit -> Int) -> Int
external def foldl_Bytes[a](bytes: Bytes, init: a, fn: (a, Int) -> a) -> a
external def concat_all_Bytes(chunks: List[Bytes]) -> Bytes
external def slice_Bytes(bytes: Bytes, start: Int, end: Int) -> Bytes
external def starts_with_Bytes(bytes: Bytes, prefix: Bytes) -> Bool
external def ends_with_Bytes(bytes: Bytes, suffix: Bytes) -> Bool
external def find_Bytes(bytes: Bytes, needle: Bytes, start: Int) -> Int

def get_Bytes(bytes: Bytes, idx: Int) -> Option[Int]:
  get_map_Bytes(bytes, idx, _ -> None, x -> Some(x))

b = from_List_Int([-1, 0, 1, 255, 256, 257])

tests = TestSuite("bytes eval", [
  Assertion(to_List_Int(b) matches [255, 0, 1, 255, 0, 1], "normalize"),
  Assertion(to_List_Array(to_Array_Int(b)) matches [255, 0, 1, 255, 0, 1], "to array"),
  Assertion(size_Bytes(b) matches 6, "size"),
  Assertion(get_Bytes(b, 1) matches Some(0), "get some"),
  Assertion(get_Bytes(b, 6) matches None, "get none"),
  Assertion(get_or_Bytes(b, 100, _ -> 7) matches 7, "get_or"),
  Assertion(foldl_Bytes(b, 0, add) matches 512, "foldl"),
  Assertion(to_List_Int(slice_Bytes(b, 1, 4)) matches [0, 1, 255], "slice"),
  Assertion(starts_with_Bytes(b, from_List_Int([255, 0])), "starts_with"),
  Assertion(ends_with_Bytes(b, from_List_Int([0, 1])), "ends_with"),
  Assertion(find_Bytes(b, from_List_Int([1, 255]), 0) matches 2, "find"),
  Assertion(find_Bytes(b, from_List_Int([]), -3) matches 0, "find empty"),
  Assertion(
    to_List_Int(concat_all_Bytes([from_List_Int([1]), from_List_Int([2, 3]), empty_Bytes])) matches [1, 2, 3],
    "concat"
  ),
  Assertion(
    to_List_Int(from_Array_Int(from_List_Array([511, -1, 0, 1]))) matches [255, 255, 0, 1],
    "from array"
  ),
])
"""
      ),
      "Bosatsu/IO/Bytes",
      14
    )
  }

  if (Platform.isScalaJvm)
    test("prog and io/std externals evaluate and run recursively") {
      val progPack = Predef.loadFileInCompile("test_workspace/Prog.bosatsu")
      val charPack = Predef.loadFileInCompile("test_workspace/Char.bosatsu")
      val ioErrorPack =
        Predef.loadFileInCompile("test_workspace/Bosatsu/IO/Error.bosatsu")
      val bytesPack = """
package Bosatsu/IO/Bytes

export (
  Bytes,
)

external struct Bytes
"""
      val ioCorePack =
        Predef.loadFileInCompile("test_workspace/Bosatsu/IO/Core.bosatsu")
      val ioStdPack =
        Predef.loadFileInCompile("test_workspace/Bosatsu/IO/Std.bosatsu")

      val progRunPack = """
package ProgRun

from Bosatsu/Prog import Prog, Main, pure, recover, await, recursive
from Bosatsu/IO/Std import println, print, print_err, print_errln, read_stdin_utf8_bytes
from Bosatsu/IO/Error import IOError

sum_to = recursive(loop -> (
    def go(state):
      (i, acc) = state
      if cmp_Int(i, 0) matches EQ | LT:
        pure(acc)
      else:
        loop((i.sub(1), add(acc, i)))
    go
  ))

main = Main(args -> (
    bad <- read_stdin_utf8_bytes(-1).recover(_ -> pure("<bad>")).await()
    stdin <- read_stdin_utf8_bytes(1).await()
    _ <- print("start|").await()
    _ <- println("stdin=${stdin}|bad=${bad}").await()
    arg_count = args.foldl_List(0, (n, _) -> add(n, 1))
    _ <- print_err("args=").await()
    _ <- print_errln(int_to_String(arg_count)).await()
    s <- sum_to((10000, 0)).await()
    _ <- println("sum=${int_to_String(s)}").await()
    pure(0)
  ).recover(_ -> pure(0))
)
"""

      testInferred(
        List(
          progPack,
          charPack,
          ioErrorPack,
          bytesPack,
          ioCorePack,
          ioStdPack,
          progRunPack
        ),
        "ProgRun",
        { (pm, mainPack) =>
          val ev =
            library.LibraryEvaluation.fromPackageMap(pm, Predef.jvmExternals)
          val (mainEval, _) =
            ev.evaluateMainValue(mainPack)
              .fold(err => fail(err.toString), identity)

          val run =
            PredefImpl.runProgMain(
              mainEval.value,
              List("one", "two"),
              "\u00E9xyz"
            )

          run.result match {
            case Right(VInt(i)) => assertEquals(i.intValue, 0)
            case other          => fail(s"unexpected prog result: $other")
          }

          assertEquals(
            run.stdout,
            "start|stdin=\u00E9|bad=<bad>\nsum=50005000\n"
          )
          assertEquals(run.stderr, "args=2\n")
        }
      )
    }

  test(
    "default-backed constructor calls work across package boundaries with partial fields and stable record-field order"
  ) {
    evalTest(
      List(
        """package Provider
export (Rec(), AllDefault())

struct Rec(a: Int, b: Int = 7, c: Int = 9)
struct AllDefault(x: Int = 11, y: Int = 22)
""",
        """package Consumer
from Provider import Rec, AllDefault

main = match (Rec { a: 1 }, Rec { c: 4, a: 2 }, Rec { a: 3, c: 5 }, AllDefault {}):
  case (Rec(1, 7, 9), Rec(2, 7, 4), Rec(3, 7, 5), AllDefault(11, 22)): 1
  case _: 0
"""
      ),
      "Consumer",
      VInt(1)
    )
  }

  test(
    "struct update syntax evaluates by overriding explicit fields and copying omitted ones"
  ) {
    evalTest(
      List(
        """package Test
struct Foo(a: Int, b: Int, c: Int)

base = Foo(1, 2, 3)
main = match Foo { b: 9, ..base }:
  case Foo(1, 9, 3): 1
  case _: 0
"""
      ),
      "Test",
      VInt(1)
    )
  }

  test("tuple update syntax works through TupleN record constructors") {
    evalTest(
      List(
        """package Test

t = (1, 2)
main = match Tuple2 { item1: 9, ..t }:
  case (9, 2): 1
  case _: 0
"""
      ),
      "Test",
      VInt(1)
    )
  }

  test(
    "enum default field can reference an earlier constructor and typechecks polymorphically"
  ) {
    evalTest(
      List(
        """package Test
enum MyList[a]:
  MyEmpty
  MyCons(head: a, tail: MyList[a] = MyEmpty)

main = match MyCons { head: 1 }:
  case MyCons(1, MyEmpty): 1
  case _: 0
"""
      ),
      "Test",
      VInt(1)
    )
  }

  test(
    "generic constructor defaults close explicit type vars and evaluate at call sites"
  ) {
    evalTest(
      List(
        """package Test
struct G[with_t](with: Option[with_t] = None)

main = match (G {}, G { with: Some(1) }):
  case (G(None), G(Some(1))): 1
  case _: 0
"""
      ),
      "Test",
      VInt(1)
    )
  }

  test(
    "generic constructor defaults with non-canonical names work across package boundaries"
  ) {
    evalTest(
      List(
        """package Provider
export (Step())

struct Step[with_t](with: Option[with_t] = None, name: Option[String] = None)
""",
        """package Consumer
from Provider import Step

main = match (Step { name: Some("x") }, Step { with: Some(1) }):
  case (Step(None, Some("x")), Step(Some(1), None)): 1
  case _: 0
"""
      ),
      "Consumer",
      VInt(1)
    )
  }

}
