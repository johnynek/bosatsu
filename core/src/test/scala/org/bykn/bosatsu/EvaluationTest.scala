package org.bykn.bosatsu

import Value._

import LocationMap.Colorize
import org.scalatest.funsuite.AnyFunSuite

class EvalationTest extends AnyFunSuite with ParTest {

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
  match string_Order_fn(a, b):
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

  test("test matching unions") {
    evalTest(
      List("""
package Foo

struct Pair(a, b)

x = Pair(Pair(1, "1"), "2")

main = match x:
  Pair(_, "2" | "3"): "good"
  _: "bad"
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
""")) { case PackageError.TypeErrorIn(_, _) => () }

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
    evalFail(List(errPack)) { case te @ PackageError.TypeErrorIn(_, _) =>
      val msg = te.message(packs, Colorize.None)
      assert(
        msg.contains(
          "type error: expected type Bosatsu/Predef::Int to be the same as type Bosatsu/Predef::String"
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
  EmptyList: "empty"
  NonEmptyList(...): "notempty"
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

  test("do a fold") {
    evalTest(
      List("""
package Foo

three = [1, 2]

def sum(ls):
  ls.foldLeft(0, add)

sum0 = sum(three)
sum1 = three.foldLeft(0, (x, y) -> add(x, y))

same = sum0.eq_Int(sum1)
"""),
      "Foo",
      True
    )

    evalTest(
      List("""
package Foo

three = [1, 2]

sum0 = three.foldLeft(0, add)
sum1 = three.foldLeft(0, \x, y -> add(x, y))

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

  test("use range") {
    evalTest(
      List("""
package Foo

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

  items.foldLeft(True, (res, t) -> and(res, test(t)))

def eq_list(a, b, fn):
  same_items(zip(a, b), fn)

same = eq_list(three, threer)(eq_Int)
"""),
      "Foo",
      True
    )

    evalTest(
      List("""
package Foo

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
      SumValue(1, ConsValue(VInt(1), UnitValue))
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
""")) { case PackageError.TypeErrorIn(_, _) => () }
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

main = [1, 2]
"""),
      "Foo",
      VList.Cons(VInt(1), VList.Cons(VInt(2), VList.VNil))
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

fac = trace("made fac", y(\f, i -> 1 if ltEqZero(i) else f(i).times(i)))

main = fac(6)
""")) { case PackageError.KindInferenceError(_, _, _) =>
      ()
    }
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
  Yep(a): a

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

  test("Leibniz type equality example") {
    evalTest(
      List("""
package A

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
""")) { case PackageError.TypeErrorIn(_, _) => () }

  }

  test("overly generic methods fail compilation") {
    evalFail(List("""
package A

# this shouldn't compile, a is too generic
def plus(x: a, y):
  x.add(y)

main = plus(1, 2)
""")) { case PackageError.TypeErrorIn(_, _) => () }
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
      assert(msg.contains("unused let binding: z\n  Region(68,73)"))
      ()
    }
  }

  test("structual recursion is allowed") {
    evalTest(
      List("""
package A

def len(lst, acc):
  recur lst:
    []: acc
    [_, *tail]: len(tail, acc.add(1))

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
    One: 1
    Even(of): toInt(of).times(2)
    Odd(of): toInt(of).times(2).add(1)

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
    Bar: 0
    baz: bad(baz)

main = bad(Bar)
""")) { case PackageError.RecursionError(_, _) => () }

    evalTest(
      List("""
package A

big_list = range(3_000)

main = big_list.foldLeft(0, add)
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
    Zero: 0
    Succ(n): toInt(n).add(1)

def sum(nat):
  recur nat:
    Zero: 0
    Succ(n): sum(n).add(toInt(nat))

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
    Succ(n): toInt(n).add(1)
    Zero: 0

def sum(nat):
  recur nat:
    Succ(n): sum(n).add(toInt(nat))
    Zero: 0

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
    EmptyList: acc
    [_, *tail]: len(tail, acc.add(1))

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
    []: acc
    NonEmptyList(_, tail): len(tail, acc.add(1))

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

main = [x for x in range(4)].foldLeft(0, add)
"""),
      "A",
      VInt(6)
    )
    evalTest(
      List("""
package A

main = [*[x] for x in range(4)].foldLeft(0, add)
"""),
      "A",
      VInt(6)
    )

    evalTest(
      List("""
package A

doub = [(x, x) for x in range(4)]

main = [x.times(y) for (x, y) in doub].foldLeft(0, add)
"""),
      "A",
      VInt(1 + 4 + 9)
    )
    evalTest(
      List("""
package A

main = [x for x in range(4) if x.eq_Int(2)].foldLeft(0, add)
"""),
      "A",
      VInt(2)
    )

    evalTest(
      List("""
package A

main = [*[x, x] for x in range(4) if x.eq_Int(2)].foldLeft(0, add)
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
    Z: 1
    S(Z): 1
    S(S(n2) as n1): fib(n1).add(fib(n2))

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
    Z: 1
    S(Z): 1
    S(S(n2) as n1): fib(n1).add(fib(n2))

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
    Z: 1
    S(Z): 1
    S(S(n2) as n1): fib(n1).add(fib(n2))

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

tuple = (1, "two")
constructed = uncurry2(TwoVar, tuple)

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

tuple = (1, "two", 3)
constructed = uncurry3(ThreeVar, tuple)

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

e1 = e.clear_Dict.add_key("hello2", "world2")

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
lst = e2.items

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
lst = e2.items

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
lst = e.items

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
lst = e.items

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
  match s.string_Order_fn("hello"):
    case EQ: True
    case _: False

e = { k: v for (k, v) in pairs if is_hello(k) }
lst = e.items

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
operator * = times

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

# you can't write \x: Int -> x.add(1)
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

main = a""")) { case te @ PackageError.TypeErrorIn(_, _) =>
      val msg = te.message(Map.empty, Colorize.None)
      assert(!msg.contains("Name("))
      assert(msg.contains("package B\nname \"a\" unknown"))
      ()
    }

    evalFail(List("""
package B

x = 1

main = match x:
  case Foo: 2
""")) { case te @ PackageError.SourceConverterErrorIn(_, _) =>
      val msg = te.message(Map.empty, Colorize.None)
      assert(!msg.contains("Name("))
      assert(msg.contains("package B\nunknown constructor Foo"))
      ()
    }

    evalFail(List("""
package B

struct X

main = match 1:
  case X1: 0
""")) { case te @ PackageError.SourceConverterErrorIn(_, _) =>
      assert(
        te.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package B\nunknown constructor X1\nRegion(49,50)"
      )
      ()
    }

    evalFail(List("""
package A

main = match [1, 2, 3]:
  case []: 0
  case [*a, *b, _]: 2
""")) { case te @ PackageError.TotalityCheckError(_, _) =>
      assert(
        te.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package A\nRegion(19,70)\nmultiple splices in pattern, only one per match allowed"
      )
      ()
    }

    evalFail(List("""
package A

enum Foo: Bar(a), Baz(b)

main = match Bar(a):
  case Baz(b): b
""")) { case te @ PackageError.TotalityCheckError(_, _) =>
      assert(
        te.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package A\nRegion(45,75)\nnon-total match, missing: Bar(_)"
      )
      ()
    }

    evalFail(List("""
package A

def fn(x):
  recur x:
    y: 0

main = fn
""")) { case te @ PackageError.RecursionError(_, _) =>
      assert(
        te.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package A\nrecur but no recursive call to fn\nRegion(25,42)\n"
      )
      ()
    }

    evalFail(List("""
package A

def fn(x):
  recur 10:
    y: 0

main = fn
""")) { case te @ PackageError.RecursionError(_, _) =>
      assert(
        te.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package A\nrecur not on an argument to the def of fn, args: x\nRegion(25,43)\n"
      )
      ()
    }

    evalFail(List("""
package A

def fn(x):
  recur y:
    y: 0

main = fn
""")) { case te @ PackageError.RecursionError(_, _) =>
      assert(
        te.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package A\nrecur not on an argument to the def of fn, args: x\nRegion(25,42)\n"
      )
      ()
    }

    evalFail(List("""
package A

def fn(x):
  recur x:
    y:
      recur x:
        z: 100

main = fn
""")) { case te @ PackageError.RecursionError(_, _) =>
      assert(
        te.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package A\nunexpected recur: may only appear unnested inside a def\nRegion(47,70)\n"
      )
      ()
    }

    evalFail(List("""
package A

def fn(x):
  fn = 100
  recur x:
    y:
      recur x:
        z: 100

main = fn
""")) { case te @ PackageError.RecursionError(_, _) =>
      assert(
        te.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package A\nillegal shadowing on: fn. Recursive shadowing of def names disallowed\nRegion(25,81)\n"
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
      assert(
        te.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package A\ninvalid recursion on fn\nRegion(63,79)\n"
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
""")) { case te @ PackageError.TypeErrorIn(_, _) =>
      assert(
        te.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package A\ntype error: expected type Bosatsu/Predef::Int to be the same as type ?2 -> ?3\nhint: this often happens when you apply the wrong number of arguments to a function.\nRegion(73,84)"
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
      assert(
        te.message(
          Map.empty,
          Colorize.None
        ) == "in <unknown source> package: A does not have name fooz. Nearest: foo"
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
      assert(
        te.message(
          Map.empty,
          Colorize.None
        ) == "in <unknown source> package: A has bar but it is not exported. Add to exports"
      )
      ()
    }
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

  // TODO: make this compile with fixed kinds
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
  RecordValue(result) = sh.getter
  result

def create_field[shape: (* -> *) -> *, t](rf: RecordField[t], fn: shape[RecordValue] -> t):
 RecordGetter(_ -> rf, sh -> RecordValue(fn(sh)))

def list_of_rows[shape: (* -> *) -> *](RecordSet(fields, rows, getters, traverse, record_to_list): RecordSet[shape]):
  def getter_to_row_entry(row: shape[RecordValue]):
    (result_fn: forall tt. RecordGetter[shape, tt] -> RecordRowEntry[RecordValue, tt]) = \RecordGetter(get_field, get_value) ->
      RecordField(_, to_entry) = get_field(fields)
      RecordRowEntry(to_entry(get_value(row)))
    result_fn
  # This code should work the same if, map_List or list comprehension, but didn't previously
  #rows.map_List(row -> record_to_list(getters.traverse(getter_to_row_entry(row))))
  [record_to_list(getters.traverse(getter_to_row_entry(row))) for row in rows]

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
struct PS[t,rest,w](left: w[t], right: rest[w])

new_record_set = RecordSet(NilShape, [], NilShape, \NilShape, _ -> NilShape, NilShape -> [])

(ps_end: forall t: (* -> *) -> *. RestructureOutput[t, NilShape]) = RestructureOutput(
  _ -> NilShape,
  _ -> NilShape,
  NilShape,
  \_, _ -> NilShape,
  _ -> []
)

def ps[
    shape1: (* -> *) -> *,
    shape2: (* -> *) -> *,
    t
](
  RecordGetter(fF, fV): RecordGetter[shape1, t],
  RestructureOutput(reshaper1F, reshaper1V, getters1, traverse1, record_to_list1): RestructureOutput[shape1, shape2]):
  getters2 = getters1.traverse1(\RecordGetter(f1, v1) -> RecordGetter(\PS(_, sh2) -> f1(sh2), \PS(_, sh2) -> v1(sh2)))
  RestructureOutput(
    \sh1 -> PS(fF(sh1), reshaper1F(sh1)),
    \sh1 -> PS(fV(sh1), reshaper1V(sh1)),
    PS(RecordGetter(\PS(x,_) -> x, \PS(x,_) -> x), getters2),
    \PS(x, sh2), g -> PS(g(x), sh2.traverse1(g)),
    \PS(RecordRowEntry(row_entry), sh2) -> [row_entry].concat(record_to_list1(sh2))
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
    case (REString(RecordValue(x1)), REString(RecordValue(x2))): string_Order_fn.equals(x1, x2)
    case _: False

equal_rows = equal_List(equal_RowEntry)

##################################################

rs_empty = new_record_set.restructure(
  _ -> ps("String field".string_field(_ -> ""),
  ps("Int field".int_field(_ -> 0),
  ps("Bool field".bool_field(_ -> True),
  ps_end))))

rs = rs_empty.concat_records([PS(RecordValue("a"), PS(RecordValue(1), PS(RecordValue(False), NilShape)))])

rs0 = rs.restructure(\PS(a, PS(b, PS(c, _))) -> ps(c, ps(b, ps(a, ps("Plus 2".int_field( r -> r.get(b).add(2) ), ps_end)))))

tests = TestSuite("reordering",
  [
    Assertion(equal_rows.equal_List(rs0.list_of_rows, [[REBool(RecordValue(False)), REInt(RecordValue(1)), REString(RecordValue("a")), REInt(RecordValue(3))]]), "swap")
  ]
)
"""),
      "RecordSet/Library",
      1
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
""")) { case s @ PackageError.SourceConverterErrorIn(_, _) =>
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
""")) { case s @ PackageError.SourceConverterErrorIn(_, _) =>
      s.message(Map.empty, Colorize.None); ()
    }

    evalFail(List("""
package A

struct Pair(first, second)

get = Pair { first } -> first

res = get(Pair(1, "two"))
""")) { case s @ PackageError.SourceConverterErrorIn(_, _) =>
      s.message(Map.empty, Colorize.None); ()
    }

    evalFail(List("""
package A

struct Pair(first, second)

# Pair has two fields
get = Pair(first) -> first

res = get(Pair(1, "two"))
""")) { case s @ PackageError.SourceConverterErrorIn(_, _) =>
      s.message(Map.empty, Colorize.None); ()
    }

    evalFail(List("""
package A

struct Pair(first, second)

# Pair does not have a field called sec
get = \Pair { first, sec: _ } -> first

res = get(Pair(1, "two"))
""")) { case s @ PackageError.SourceConverterErrorIn(_, _) =>
      s.message(Map.empty, Colorize.None); ()
    }

    evalFail(List("""
package A

struct Pair(first, second)

# Pair does not have a field called sec
get = Pair { first, sec: _, ... } -> first

res = get(Pair(1, "two"))
""")) { case s @ PackageError.SourceConverterErrorIn(_, _) =>
      s.message(Map.empty, Colorize.None); ()
    }

    evalFail(List("""
package A

struct Pair(first, second)

# Pair has two fields, not three
get = Pair(first, _, _) -> first

res = get(Pair(1, "two"))
""")) { case s @ PackageError.SourceConverterErrorIn(_, _) =>
      s.message(Map.empty, Colorize.None); ()
    }

    evalFail(List("""
package A

struct Pair(first, second)

# Pair has two fields, not three
get = Pair(first, _, _, ...) -> first

res = get(Pair(1, "two"))
""")) { case s @ PackageError.SourceConverterErrorIn(_, _) =>
      s.message(Map.empty, Colorize.None); ()
    }
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

  test("shadowing of external def isn't allowed") {
    evalFail(List("""
package A

external def foo(x: String) -> List[String]

def foo(x): x

""")) { case s @ PackageError.SourceConverterErrorIn(_, _) =>
      assert(
        s.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package A\nbind names foo shadow external def\nRegion(57,71)"
      )
      ()
    }

    evalFail(List("""
package A

external def foo(x: String) -> List[String]

foo = 1

""")) { case s @ PackageError.SourceConverterErrorIn(_, _) =>
      assert(
        s.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package A\nbind names foo shadow external def\nRegion(57,65)"
      )
      ()
    }

    evalFail(List("""
package A

external def foo(x: String) -> List[String]

external def foo(x: String) -> List[String]
""")) { case s @ PackageError.SourceConverterErrorIn(_, _) =>
      assert(
        s.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package A\nexternal def: foo defined multiple times\nRegion(21,55)"
      )
      ()
    }
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

  test("type parameters must be supersets for structs and enums fails") {
    evalFail(List("""
package Err

struct Foo[a](a)

main = Foo(1, "2")
""")) { case sce @ PackageError.SourceConverterErrorIn(_, _) =>
      assert(
        sce.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package Err\nFoo found declared: [a], not a superset of [b]\nRegion(14,30)"
      )
      ()
    }

    evalFail(List("""
package Err

struct Foo[a](a: a, b: b)

main = Foo(1, "2")
""")) { case sce @ PackageError.SourceConverterErrorIn(_, _) =>
      assert(
        sce.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package Err\nFoo found declared: [a], not a superset of [a, b]\nRegion(14,39)"
      )
      ()
    }

    evalFail(List("""
package Err

enum Enum[a]: Foo(a)

main = Foo(1, "2")
""")) { case sce @ PackageError.SourceConverterErrorIn(_, _) =>
      assert(
        sce.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package Err\nEnum found declared: [a], not a superset of [b]\nRegion(14,34)"
      )
      ()
    }

    evalFail(List("""
package Err

enum Enum[a]: Foo(a: a), Bar(a: b)

main = Foo(1, "2")
""")) { case sce @ PackageError.SourceConverterErrorIn(_, _) =>
      assert(
        sce.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package Err\nEnum found declared: [a], not a superset of [a, b]\nRegion(14,48)"
      )
      ()
    }
  }

  test("test duplicate import message") {
    evalFail(List("""
package Err

from Bosatsu/Predef import foldLeft

main = 1
""")) { case sce @ PackageError.DuplicatedImport(_) =>
      assert(
        sce.message(
          Map.empty,
          Colorize.None
        ) == "duplicate import in <unknown source> package Bosatsu/Predef imports foldLeft as foldLeft"
      )
      ()
    }
  }

  test("test duplicate package message") {
    val pack =
      """
        |package Err
        |
        |from Bosatsu/Predef import foldLeft
        |
        |main = 1
        |""".stripMargin

    evalFail(List(pack, pack)) {
      case sce @ PackageError.DuplicatedPackageError(_) =>
        assert(
          sce.message(
            Map.empty,
            Colorize.None
          ) == "package Err duplicated in 0, 1"
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
      assert(
        sce.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package Err\nRegion(36,89)\nmultiple splices in pattern, only one per match allowed"
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
      assert(
        sce.message(Map.empty, Colorize.None) ==
          s"in file: <unknown source>, package Err\nRegion(36,91)\ninvalid string pattern: '$dollar{_}$dollar{_}' (adjacent bindings aren't allowed)"
      )
      ()
    }
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

def fold_Queue(Queue(f, b): Queue[a], binit: b, fold_fn: b -> a -> b) -> b:
  front = f.foldLeft(binit, fold_fn)
  b.reverse.foldLeft(front, fold_fn)

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

three = 2.(x -> add(x, 1))

test = Assertion(three.eq_Int(3), "let inside apply")
"""),
      "A",
      1
    )
  }

  test("colliding type names cause errors") {
    evalFail(List(s"""
package Err

struct Foo

struct Foo(x)

main = Foo(1)
""")) { case sce @ PackageError.SourceConverterErrorIn(_, _) =>
      assert(
        sce.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package Err\ntype name: Foo defined multiple times\nRegion(14,24)"
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
""")) { case sce @ PackageError.SourceConverterErrorIn(_, _) =>
      assert(
        sce.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package Err\nconstructor: Foo defined multiple times\nRegion(14,27)"
      )
      ()
    }
  }

  test("non binding top levels work") {
    runBosatsuTest(
      List("""
package A

# this is basically a typecheck only
_ = add(1, 2)

test = Assertion(True, "")
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""
package A

# this is basically a typecheck only
x = (1, "1")
(_, _) = x

test = Assertion(True, "")
"""),
      "A",
      1
    )

    runBosatsuTest(
      List("""
package A

struct Foo(x, y)
# this is basically a typecheck only
x = Foo(1, "1")
Foo(_, _) = x

test = Assertion(True, "")
"""),
      "A",
      1
    )
  }

  test("recursion check with _ pattern: issue 573") {
    runBosatsuTest(
      List("""
package VarSet/Recursion

enum Thing:
  Thing1, Thing2(a: Int, t: Thing)

def bar(y, _: String, x):
  recur x:
    Thing1: y
    Thing2(i, t): bar(i, "boom", t)

test = Assertion(True, "")
"""),
      "VarSet/Recursion",
      1
    )
  }

  test("recursion check with shadowing") {
    evalFail(List("""
package S

enum Thing:
  Thing1, Thing2(a: Int, t: Thing)

def bar(y, _: String, x):
  x = Thing2(0, x)
  recur x:
    Thing1: y
    Thing2(i, t): bar(i, "boom", t)

test = Assertion(True, "")
""")) { case re @ PackageError.RecursionError(_, _) =>
      assert(
        re.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package S\nrecur not on an argument to the def of bar, args: y, _: String, x\nRegion(107,165)\n"
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
""")) { case sce @ PackageError.SourceConverterErrorIn(_, _) =>
      assert(
        sce.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package Foo\nrepeated bindings in pattern: a\nRegion(48,49)"
      )
      ()
    }
    evalFail(List("""
package Foo

out = match [(1,2), (1, 0)]:
  case [(a, a), (1, 0)]: a
  case _: 0

test = Assertion(True, "")
""")) { case sce @ PackageError.SourceConverterErrorIn(_, _) =>
      assert(
        sce.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package Foo\nrepeated bindings in pattern: a\nRegion(68,69)"
      )
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

  test("unknown type constructor message is good. issue 653") {
    evalFail(List("""
package Foo

struct Bar(baz: Either[Int, String])

test = Assertion(True, "")

""")) { case sce @ PackageError.SourceConverterErrorIn(_, _) =>
      assert(
        sce.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package Foo\nunknown type: Either\nRegion(14,50)"
      )
      ()
    }
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
      assert(
        sce.message(
          Map.empty,
          Colorize.None
        ) == "in <unknown source> export bar of type Foo::Bar has an unexported (private) type."
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

""")) { case sce @ PackageError.SourceConverterErrorIn(_, _) =>
      assert(
        sce.message(
          Map.empty,
          Colorize.None
        ) == "in file: <unknown source>, package Foo\nand_again found declared types: [b], not a subset of [a]\nRegion(71,118)"
      )
      ()
    }
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
  RecordValue(result) = sh.getter
  result
""")) { case PackageError.TypeErrorIn(_, _) => () }
  }

  test("test quicklook example") {
    runBosatsuTest(
      List("""
package Foo

def f(fn: forall a. List[a] -> List[a]) -> Int:
  fn([1]).foldLeft(0)(\x, _ -> x.add(1))

def g(b: Bool) -> (forall a. List[a] -> List[a]):
  match b:
    case True: x -> x
    case False: _ -> []

def id(a): a
def single(a): [a]    

def foo1(fn) -> Int:
  fn.foldLeft(0)(\x, _ -> x.add(1))

def foo2(fn: List[forall a. a -> a]) -> Int:
  fn.foldLeft(0)(\x, _ -> x.add(1))

count = foo1(single(id))
count = foo2(single(id))

single_id1: forall a. List[a -> a] = single(id)
single_id2: List[forall a. a -> a] = single(id)

struct Pair1(fst: a, snd: a)

pair = Pair1(single_id1, single_id2)

comp = x -> f(g(x))
  
test = Assertion(True, "")
"""),
      "Foo",
      1
    )
  }

  test("ill-kinded structs point to the right region") {

    evalFail(List("""
package Foo

struct Foo(a: f[a], b: f)
""")) { case kie @ PackageError.KindInferenceError(_, _, _) =>
      assert(
        kie.message(Map.empty, Colorize.None) ==
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
      assert(
        kie.message(Map.empty, Colorize.None) ==
          """in file: <unknown source>, package Foo shape error: expected * -> ? but found * in the constructor Foo inside type a[Bosatsu/Predef::Int]

Region(14,41)"""
      )
      ()
    }
  }

  test("example from issue #264") {
    runBosatsuTest(
      """
package SubsumeTest

def lengths(l1: List[Int], l2: List[String], maybeFn: Option[forall tt. List[tt] -> Int]):
  match maybeFn:
    Some(fn): fn(l1).add(fn(l2))
    None: 0

def lengths2(l1: List[Int], l2: List[String], maybeFn: forall tt. Option[List[tt] -> Int]):
  match maybeFn:
    Some(fn): fn(l1).add(fn(l2))
    None: 0

# this is a test that doesn't forget that we have the empty list:
x = match []:
      case []: 0
      case [h, *_]: (h: forall a. a)

test = Assertion(lengths([], [], None) matches 0, "test")
    """ :: Nil,
      "SubsumeTest",
      1
    )
  }

  test("ill kinded code examples") {
    evalFail(List("""
package Foo

# this is an higher kinded apply
struct Foo[t: (* -> *) -> *, a: (* -> *)](value: t[a])

struct Id(a)
# this code could run if we ignored kinds
def makeFoo(v: Int): Foo(Id(v))

""")) { case kie @ PackageError.TypeErrorIn(_, _) =>
      assert(
        kie.message(Map.empty, Colorize.None) ==
          """in file: <unknown source>, package Foo
kind error: the type: ?1 of kind: * -> * at: 
Region(183,188)

cannot be unified with the type Bosatsu/Predef::Int of kind: *
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

""")) { case kie @ PackageError.TypeErrorIn(_, _) =>
      assert(
        kie.message(Map.empty, Colorize.None) ==
          """in file: <unknown source>, package Foo
kind error: the type: Foo::Foo[Foo::Id] is invalid because the left Foo::Foo has kind ((* -> *) -> *) -> (* -> *) -> * and the right Foo::Id has kind +* -> * but left cannot accept the kind of the right:
Region(195,205)"""
      )
      ()
    }

  }
  test("print a decent message when there is an unknown meta") {
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
""")) { case kie @ PackageError.TypeErrorIn(_, _) =>
      assert(
        kie.message(Map.empty, Colorize.None) ==
          """in file: <unknown source>, package QS
Unexpected unknown: the type: ?50 of kind: * at: 
Region(396,450)

inside the type ?51[?52]
this sometimes happens when a function arg has been omitted, or an illegal recursive type or function."""
      )
      ()
    }

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
}
