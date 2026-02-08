package dev.bosatsu

import Value._

import LocationMap.Colorize
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

def let(arg): in -> in(arg)

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

fac = trace("made fac", y((f, i) -> 1 if ltEqZero(i) else f(i).times(i)))

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
      assert(msg.contains("unused let binding: z\n  Region(54,55)"))
      ()
    }
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
    case Even(of): toInt(of).times(2)
    case Odd(of): toInt(of).times(2).add(1)

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

main = [x.times(y) for (x, y) in doub].foldl_List(0, add)
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
      assert(msg.contains("package B\nname \"a\" unknown"))
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
      assert(msg.contains("package B\nunknown constructor Foo"))
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
        "in file: <unknown source>, package B\nunknown constructor X1\nRegion(49,50)"
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
      assert(
        te.message(Map.empty, Colorize.None)
          .contains(
            "the first type is a function with one argument and the second is a function with 2 arguments"
          )
      )
      assert(
        te.message(Map.empty, Colorize.None)
          .contains("Region(67,69)")
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
      assert(msg.contains("Either remove it or use it in one of the enum variants"))
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

  test("non binding top levels don't work") {
    evalFail(List("""
package A

# this is basically a typecheck only
_ = add(1, 2)

""")) { case sce @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      assertEquals(
        sce.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package A\n_ does not bind any names.\nRegion(53,62)"
      )
      ()
    }

    evalFail(List("""
package A

# this is basically a typecheck only
(_, _) = (1, "1")
""")) { case sce @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      assertEquals(
        sce.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package A\n(_, _) does not bind any names.\nRegion(58,66)"
      )
      ()
    }

    evalFail(List("""
package A

struct Foo(x, y)
# this is basically a typecheck only
Foo(_, _) = Foo(1, "1")

""")) { case sce @ PackageError.SourceConverterErrorsIn(_, _, _) =>
      assertEquals(
        sce.message(
          Map.empty,
          Colorize.None
        ),
        "in file: <unknown source>, package A\nFoo(_, _) does not bind any names.\nRegion(78,89)"
      )
      ()
    }
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
      assertEquals(
        sce.message(
          Map.empty,
          Colorize.None
        ),
        "in <unknown source> export bar of type Foo::Bar has an unexported (private) type."
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
shape error: expected * -> ? but found * in the constructor Foo inside type a[Bosatsu/Predef::Int]

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

cannot be unified with the type Foo::Id of kind: +* -> *
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
kind error: the type: Foo::Foo[Foo::Id] is invalid because the left Foo::Foo has kind ((* -> *) -> *) -> (* -> *) -> * and the right Foo::Id has kind +* -> * but left cannot accept the kind of the right:
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
type error: expected type Bosatsu/Predef::Fn3[(?17, ?9) -> Bosatsu/Predef::Comparison]
Region(403,414)
to be the same as type Bosatsu/Predef::Fn2
hint: the first type is a function with 3 arguments and the second is a function with 2 arguments.
Region(415,424)"""
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
    # TODO it's weird that removing the [a] breaks this
    # if a type isn't mentioned in an outer scope, we should assume it's local
    def poly_rec[a](count: Nat, a: a) -> a:
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
    case Next(cont): cont((box, fn) -> fn(loop(box)))

v = loop(b)
main = v
"""),
      "A",
      VInt(1)
    )
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
      case kie @ PackageError.TypeErrorIn(_, _, _, _)    =>
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

  test("prog and io/std externals evaluate and run recursively") {
    val progPack = Predef.loadFileInCompile("test_workspace/Prog.bosatsu")
    val ioErrorPack =
      Predef.loadFileInCompile("test_workspace/Bosatsu/IO/Error.bosatsu")
    val ioStdPack =
      Predef.loadFileInCompile("test_workspace/Bosatsu/IO/Std.bosatsu")

    val progRunPack = """
package ProgRun

from Bosatsu/Prog import Prog, Main, pure, read_env, recover, await, recursive
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

main = Main(
  (
    args <- read_env.await()
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
      List(progPack, ioErrorPack, ioStdPack, progRunPack),
      "ProgRun",
      { (pm, mainPack) =>
        val ev = library.LibraryEvaluation.fromPackageMap(pm, Predef.jvmExternals)
        val (mainEval, _) =
          ev.evaluateMainValue(mainPack).fold(err => fail(err.toString), identity)

        val run =
          PredefImpl.runProgMain(mainEval.value, List("one", "two"), "\u00E9xyz")

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

  test("we always suggest names which share a prefix with an unknown name") {
    val testCode = List("""
package P1

fofoooooooo = 1

ofof = 2

main = fof
""")

    evalFail(testCode) { case kie: PackageError.TypeErrorIn =>
      val message = kie.message(Map.empty, Colorize.None)
      assertEquals(
        message,
        """in file: <unknown source>, package P1
name "fof" unknown.
Closest: fofoooooooo, ofof.

Region(47,50)"""
      )
      ()
    }
  }
}
