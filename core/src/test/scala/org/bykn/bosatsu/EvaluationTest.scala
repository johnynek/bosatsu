package org.bykn.bosatsu

import org.scalatest.FunSuite

import Evaluation.Value._

class EvaluationTest extends FunSuite {

  import TestUtils._

  test("simple evaluation") {
    evalTest(
      List("""
package Foo

x = 1
"""), "Foo", VInt(1))

    evalTest(
      List("""
package Foo

# test shadowing
x = match 1: x: x
"""), "Foo", VInt(1))

    evalTest(
      List("""
package Foo

# exercise calling directly a lambda
x = (\y -> y)("hello")
"""), "Foo", Str("hello"))
  }

    runBosatsuTest(
      List("""
package Foo

foo = "hello"

def eq_String(a, b):
  match string_Order_fn(a, b):
    EQ: True
    _: False

test = Assertion(eq_String("hello", foo), "checking equality")
"""), "Foo", 1)

    runBosatsuTest(
      List("""
package Foo

test = TestSuite("three trivial tests", [ Assertion(True, "t0"),
    Assertion(True, "t1"),
    Assertion(True, "t2"),
    ])
"""), "Foo", 3)

  test("test if/else") {
    evalTest(
      List("""
package Foo

x = 1

z = match x.cmp_Int(1):
  EQ:
    "foo"
  _:
    "bar"
"""), "Foo", Str("foo"))

    evalTest(
      List("""
package Foo

x = 1

# here if the single expression python style
z = "foo" if x.eq_Int(2) else "bar"
"""), "Foo", Str("bar"))
  }

  test("exercise option from predef") {
    evalTest(
      List("""
package Foo

x = Some(1)

z = match x:
  Some(v): add(v, 10)
  None: 0
"""), "Foo", VInt(11))

    // Use a local name collision and see it not have a problem
    evalTest(
      List("""
package Foo

enum Option: None, Some(get)

x = Some(1)

z = match x:
  Some(v): add(v, 10)
  None: 0
"""), "Foo", VInt(11))
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
"""), "Foo", Str("good"))

    evalTest(
      List("""
package Foo

enum Res: Err(a), Good(a)

x = Err("good")

def run(z):
  Err(y) | Good(y) = z
  y

main = run(x)
"""), "Foo", Str("good"))

    evalFail(
      List("""
package Err

enum IntOrString: IntCase(i: Int), StringCase(i: Int, s: String)

def go(x):
  # if we remove z, this is well typed, but an error nonetheless
  IntCase(y) | StringCase(y, z) = x
  y

main = go(IntCase(42))
"""), "Err") { case PackageError.TypeErrorIn(_, _) => () }

    val errPack = """
package Err

enum IntOrString: IntCase(i: Int), StringCase(s: String)

def go(x):
  # this is illtyped
  IntCase(y) | StringCase(y) = x
  y

main = go(IntCase(42))
"""
    val packs = Map((PackageName.parts("Err"), (LocationMap(errPack), "Err.bosatsu")))
    evalFail(List(errPack), "Err") { case te@PackageError.TypeErrorIn(_, _) =>
      val msg = te.message(packs)
      assert(msg.contains("type error: expected type Bosatsu/Predef::Int to be the same as type Bosatsu/Predef::String"))
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
"""), "Union", VInt(42))
  }

  test("test matching literals") {
    evalTest(
      List("""
package Foo

x = 1

main = match x:
  1: "good"
  _: "bad"
"""), "Foo", Str("good"))

    evalTest(
      List("""
package Foo

x = [1]

# test using List literals
main = match x:
  EmptyList: "empty"
  NonEmptyList(...): "notempty"
"""), "Foo", Str("notempty"))

    evalTest(
      List("""
package Foo

x = "1"

main = match x:
  "1": "good"
  _: "bad"
"""), "Foo", Str("good"))

    evalTest(
      List("""
package Foo

struct Pair(fst, snd)

x = Pair(1, "1")

main = match x:
  Pair(_, "1"): "good"
  _: "bad"
"""), "Foo", Str("good"))
  }

  test("test tuples") {
    evalTest(
      List("""
package Foo

x = (1, "1")

main = match x:
  (_, "1"): "good"
  _: "bad"
"""), "Foo", Str("good"))

    evalTest(
      List("""
package Foo

x = (1, "1")

def go(u):
  (_, y) = x
  match y:
    "1": "good"
    _: "bad"

main = go(())
"""), "Foo", Str("good"))
  }

  test("do a fold") {
    evalTest(
      List("""
package Foo

three = [1, 2]

def sum(ls):
  ls.foldLeft(0, add)

sum0 = sum(three)
sum1 = three.foldLeft(0, \x, y -> add(x, y))

same = sum0.eq_Int(sum1)
"""), "Foo", True)

    evalTest(
      List("""
package Foo

three = [1, 2]

sum0 = three.foldLeft(0, add)
sum1 = three.foldLeft(0, \x, y -> add(x, y))

same = sum0.eq_Int(sum1)
"""), "Foo", True)

  }

  test("test zero arg defs") {
    evalTest(
      List("""
package Foo

def foo: 42

main = foo
"""), "Foo", VInt(42))
  }

  test("test Int functions") {
    evalTest(
      List("""
package Foo

main = 6.mod_Int(4)
"""), "Foo", VInt(2))

    evalTest(
      List("""
package Foo

main = match 6.div(4):
  Some(0): 42
  Some(1): 100
  Some(x): x
  None: -1
"""), "Foo", VInt(100))

    evalTest(
      List("""
package Foo

main = 6.gcd_Int(3)
"""), "Foo", VInt(3))
  }

  test("use range") {
    evalTest(
      List("""
package Foo

three = [0, 1]
# exercise the built-in range function (not implementable in bosatsu)
threer = range(3)

def zip(as, bs):
  recur as:
    []: []
    [ah, *atail]:
      match bs:
        []: []
        [bh, *btail]: [(ah, bh), *zip(atail, btail)]

def and(a, b):
  b if a else False

def same_items(items, eq):
  def test(p):
    (a, b) = p
    eq(a, b)

  items.foldLeft(True, \res, t -> and(res, test(t)))

def eq_list(a, b, fn):
  same_items(zip(a, b), fn)

same = eq_list(three, threer)(eq_Int)
"""), "Foo", True)

evalTest(
  List("""
package Foo

def zip(as: List[a], bs: List[b]) -> List[(a, b)]:
  recur as:
    []: []
    [ah, *atail]:
      match bs:
        []: []
        [bh, *btail]: [(ah, bh), *zip(atail, btail)]

main = 1
"""), "Foo", VInt(1))

  }

  test("test range_fold") {
evalTest(
  List("""
package Foo

main = range_fold(0, 10, 0, add)
"""), "Foo", VInt(45))

evalTest(
  List("""
package Foo

main = range_fold(0, 10, 0, \x, y -> y)
"""), "Foo", VInt(9))

evalTest(
  List("""
package Foo

main = range_fold(0, 10, 100, \x, y -> x)
"""), "Foo", VInt(100))
  }

  test("test some list matches") {
    evalTest(
      List("""
package Foo

def headOption(as):
  match as:
    []: None
    [a, *_]: Some(a)

main = headOption([1])
"""), "Foo", SumValue(1, ConsValue(VInt(1), UnitValue)))
  }

  test("test generics in defs") {
    evalTest(
      List("""
package Foo

def id(x: a) -> a:
  x

main = id(1)
"""), "Foo", VInt(1))
  }

  test("exercise struct creation") {
    evalTest(
      List("""
package Foo

struct Bar(a: Int)

main = Bar(1)
"""), "Foo",
  ConsValue(VInt(1), UnitValue))

    evalTest(
      List("""
package Foo

struct Bar(a: Int)

# destructuring top-level let
Bar(main) = Bar(1)
"""), "Foo", VInt(1))

    evalTest(
      List("""
package Foo

struct Bar(a: Int)

# destructuring top-level let
Bar(main: Int) = Bar(1)
"""), "Foo", VInt(1))

    evalTest(
      List("""
package Foo

struct Bar(a: Int)

y = Bar(1)
# destructuring top-level let
Bar(main: Int) = y
"""), "Foo", VInt(1))

    evalTestJson(
      List("""
package Foo

struct Bar(a: Int, s: String)

main = Bar(1, "foo")
"""), "Foo", Json.JObject(List("a" -> Json.JNumberStr("1"), "s" -> Json.JString("foo"))))
  }

  test("test some type errors") {
    evalFail(
      List("""
package Foo

main = if True:
  1
else:
  "1"
"""), "Foo") { case PackageError.TypeErrorIn(_, _) => () }
  }

  test("test the list literals work even when we have conflicting local names") {
    evalTest(
      List("""
package Foo

struct EmptyList

main = [1, 2]
"""), "Foo",
  VList.Cons(VInt(1), VList.Cons(VInt(2), VList.VNil)))

    evalTest(
      List("""
package Foo

struct NonEmptyList

main = [1, 2]
"""), "Foo",
  VList.Cons(VInt(1), VList.Cons(VInt(2), VList.VNil)))

    evalTest(
      List("""
package Foo

def concat(a): a

main = [1, 2]
"""), "Foo",
  VList.Cons(VInt(1), VList.Cons(VInt(2), VList.VNil)))
  }

  test("forbid the y-combinator") {
    evalFail(
      List("""
package Y

struct W(fn: W[a, b] -> a -> b)

def call(w0, w1):
  match w0:
    W(fn): trace("fn(w1)", fn(w1))

def y(f):
  g = \w -> \a -> trace("calling f", f(call(w, w), a))
  g(W(g))

def ltEqZero(i):
  match i.cmp_Int(0):
    GT: False
    _: True

fac = trace("made fac", y(\f, i -> 1 if ltEqZero(i) else f(i).times(i)))

main = fac(6)
"""), "Y") { case PackageError.CircularType(_, _) => () }
  }

  test("check type aligned enum") {
  evalTest(
    List("""
package A

enum GoodOrBad:
  Bad(a: a), Good(a: a)

def unbox(gb: GoodOrBad[a]):
  match gb:
    Good(g): g
    Bad(b): b

(main: Int) = unbox(Good(42))
"""), "A", VInt(42))

  evalTest(
    List("""
package A

enum GoodOrBad:
  Bad(a: a), Good(a: a)

Bad(main) | Good(main) = Good(42)
"""), "A", VInt(42))
  }

  test("nontotal matches fail even if not at runtime") {
    evalFail(
      List("""
package Total

enum Opt: Nope, Yep(get)

something = Yep(
  1)

one = match something:
  Yep(a): a

main = one
"""), "Total") { case PackageError.TotalityCheckError(_, _) => () }
  }

  test("Leibniz type equality example") {
  evalTest(
    List("""
package A

struct Leib(subst: forall f. f[a] -> f[b])

struct Id(a)

def coerce(a, leib):
  Leib(subst) = leib
  Id(b) = subst(Id(a))
  b

# there is really only one (polymorphic) value of Leib
refl = Leib(\x -> x)

enum StringOrInt:
  IsStr(s: String, leib: Leib[String, a])
  IsInt(i: Int, leib: Leib[Int, a])

str = IsStr("foo", refl)
int = IsInt(42, refl)

# this takes StringOrInt[a] and returns a
def getValue(v: StringOrInt[a]) -> a:
  match v:
    IsStr(s, leib): coerce(s, leib)
    IsInt(i, leib): coerce(i, leib)

main = getValue(int)
"""), "A", VInt(42))

  // If we leave out the coerce it fails
  evalFail(
    List("""
package A

struct Leib(subst: forall f. f[a] -> f[b])

# there is really only one (polymorphic) value of Leib
refl = Leib(\x -> x)

enum StringOrInt:
  IsStr(s: String, leib: Leib[String, a])
  IsInt(i: Int, leib: Leib[Int, a])

str = IsStr("foo", refl)
int = IsInt(42, refl)

# this takes StringOrInt[a] and returns a
def getValue(v):
  match v:
    IsStr(s, _): s
    IsInt(i, _): i

main = getValue(int)
"""), "A"){ case PackageError.TypeErrorIn(_, _) => () }

  }

  test("overly generic methods fail compilation") {
    evalFail(
      List("""
package A

# this shouldn't compile, a is too generic
def plus(x: a, y):
  x.add(y)

main = plus(1, 2)
"""), "A"){ case PackageError.TypeErrorIn(_, _) => () }
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
"""), "A", VInt(3))

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
"""), "A", VInt(4))

  evalFail(
    List("""
package A

enum Foo: Bar, Baz

def bad(foo):
  recur foo:
    Bar: 0
    baz: bad(baz)

main = bad(Bar)
"""), "A"){ case PackageError.RecursionError(_, _) => () }

  evalTest(
    List("""
package A

big_list = range(3_000)

main = big_list.foldLeft(0, \x, y -> x.add(y))
"""), "A", VInt((0 until 3000).sum))

  def sumFn(n: Int): Int = if (n <= 0) 0 else { sumFn(n-1) + n }
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
"""), "A", VInt(sumFn(3)))

  }

  test("list comphension test") {
  evalTest(
    List("""
package A

main = [x for x in range(4)].foldLeft(0, add)
"""), "A", VInt(6))
  evalTest(
    List("""
package A

main = [*[x] for x in range(4)].foldLeft(0, add)
"""), "A", VInt(6))

  evalTest(
    List("""
package A

doub = [(x, x) for x in range(4)]

main = [x.times(y) for (x, y) in doub].foldLeft(0, add)
"""), "A", VInt(1 + 4 + 9))
  evalTest(
    List("""
package A

main = [x for x in range(4) if x.eq_Int(2)].foldLeft(0, add)
"""), "A", VInt(2))

  evalTest(
    List("""
package A

main = [*[x, x] for x in range(4) if x.eq_Int(2)].foldLeft(0, add)
"""), "A", VInt(4))

  evalTest(
    List("""
package A

def eq_List(lst1, lst2):
  recur lst1:
    []:
      match lst2:
        []: True
        _: False
    [h1, *t1]:
      match lst2:
        []: False
        [h2, *t2]:
          eq_List(t1, t2) if eq_Int(h1, h2) else False

lst1 = [0, 0, 1, 1, 2, 2, 3, 3]
lst2 = [*[x, x] for x in range(4)]
lst3 = [*[y, y] for (y, y) in [(x, x) for x in range(4)]]

main = match (eq_List(lst1, lst2), eq_List(lst1, lst3)):
  (True, True): 1
  notTrue: 0
"""), "A", VInt(1))
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
    S(n1@S(n2)): fib(n1).add(fib(n2))

# fib(5) = 1, 1, 2, 3, 5, 8
main = fib(S(S(S(S(S(Z))))))
"""), "A", VInt(8))
  }

  test("test matching the front of a list") {
    evalTest(List("""
package A

def bad_len(list):
  recur list:
    []: 0
    [*init, _]: bad_len(init).add(1)

main = bad_len([1, 2, 3, 5])
"""), "A", VInt(4))

    evalTest(List("""
package A

def last(list):
  match list:
    []: -1
    [*_, s]: s

main = last([1, 2, 3, 5])
"""), "A", VInt(5))
  }
  test("test a named pattern that doesn't match") {
    evalTest(List("""
package A

def bad_len(list):
  recur list:
    []: 0
    [2] | [3]: -1
    [*_, four@4]: -1
    [100, *_]: -1
    [*init, last@(_: Int)]: bad_len(init).add(1)

main = bad_len([1, 2, 3, 5])
"""), "A", VInt(4))
  }
  test("uncurry2") {
    evalTest(List("""
package A

struct TwoVar(one, two)

tuple = (1, "two")
constructed = uncurry2(TwoVar, tuple)

main = match constructed:
  TwoVar(1, "two"): "good"
  _: "bad"
"""), "A", Str("good"))
  }
  test("uncurry3") {
    evalTest(List("""
package A

struct ThreeVar(one, two, three)

tuple = (1, "two", 3)
constructed = uncurry3(ThreeVar, tuple)

main = match constructed:
  ThreeVar(1, "two", 3): "good"
  _: "bad"
"""), "A", Str("good"))
  }

  test("Dict methods") {
    evalTest(List("""
package A

e = empty_Dict(string_Order)

e1 = e.add_key("hello", "world")

main = e1.get_key("hello")
"""), "A", VOption.some(Str("world")))

    evalTest(List("""
package A

e = empty_Dict(string_Order)

e1 = e.clear_Dict.add_key("hello2", "world2")

main = e1.get_key("hello")
"""), "A", VOption.none)

    evalTest(List("""
package A

e = empty_Dict(string_Order)

e1 = e.add_key("hello", "world")
e2 = e1.remove_key("hello")

main = e2.get_key("hello")
"""), "A", VOption.none)

    evalTest(List("""
package A

e1 = empty_Dict(string_Order)
e2 = e1.add_key("hello", "world").add_key("hello1", "world1")
lst = e2.items

main = match lst:
  [("hello", "world"), ("hello1", "world1")]: "good"
  _: "bad"
"""), "A", Str("good"))

    evalTest(List("""
package A

e1 = {}
e2 = e1.add_key("hello", "world").add_key("hello1", "world1")
lst = e2.items

main = match lst:
  [("hello", "world"), ("hello1", "world1")]: "good"
  _: "bad"
"""), "A", Str("good"))

    evalTest(List("""
package A

e = {
      "hello": "world",
      "hello1":
        "world1" }
lst = e.items

main = match lst:
  [("hello", "world"), ("hello1", "world1")]: "good"
  _: "bad"
"""), "A", Str("good"))

    evalTest(List("""
package A

pairs = [("hello", "world"), ("hello1", "world1")]

e = { k: v for (k, v) in pairs }
lst = e.items

main = match lst:
  [("hello", "world"), ("hello1", "world1")]: "good"
  _: "bad"
"""), "A", Str("good"))

    evalTest(List("""
package A

pairs = [("hello", 42), ("hello1", 24)]

def is_hello(s):
  match s.string_Order_fn("hello"):
    EQ: True
    _: False

e = { k: v for (k, v) in pairs if is_hello(k) }
lst = e.items

main = match lst:
  [("hello", res)]: res
  _: -1
"""), "A", VInt(42))

    evalTestJson(
      List("""
package Foo

bar = {'a': '1', 's': 'foo' }

main = bar
"""), "Foo", Json.JObject(List("a" -> Json.JString("1"), "s" -> Json.JString("foo"))))

    evalTestJson(
      List("""
package Foo

bar = {'a': None, 's': None }

main = bar
"""), "Foo", Json.JObject(List("a" -> Json.JNull, "s" -> Json.JNull)))

    evalTestJson(
      List("""
package Foo

bar = {'a': None, 's': Some(1) }

main = bar
"""), "Foo", Json.JObject(List("a" -> Json.JNull, "s" -> Json.JNumberStr("1"))))

    evalTestJson(
      List("""
package Foo

bar = {'a': [], 's': [1] }

main = bar
"""), "Foo", Json.JObject(
  List("a" -> Json.JArray(Vector.empty),
       "s" -> Json.JArray(Vector(Json.JNumberStr("1"))))))

    evalTestJson(
      List("""
package Foo

bar = {'a': True, 's': False }

main = bar
"""), "Foo", Json.JObject(
  List("a" -> Json.JBool(true),
       "s" -> Json.JBool(false))))

    evalTestJson(
      List("""
package Foo

main = (1, "1", ())
"""), "Foo", Json.JArray(
  Vector(Json.JNumberStr("1"),
    Json.JString("1"),
    Json.JNull)))

    evalTestJson(
      List("""
package Foo

main = [Some(Some(1)), Some(None), None]
"""), "Foo",
  Json.JArray(
    Vector(
      Json.JArray(Vector(Json.JNumberStr("1"))),
      Json.JArray(Vector(Json.JNull)),
      Json.JArray(Vector.empty)
      )))

    evalTestJson(
      List("""
package Foo

enum FooBar: Foo(foo), Bar(bar)

main = [Foo(1), Bar("1")]
"""), "Foo",
  Json.JArray(
    Vector(
      Json.JObject(
        List("foo" -> Json.JNumberStr("1"))),
      Json.JObject(
        List("bar" -> Json.JString("1"))))))
  }

  test("json with backticks") {
    evalTestJson(
      List("""
package Foo

struct Foo(`struct`, `second key`, `enum`, `def`)

`package` = 2

main = Foo(1, `package`, 3, 4)
"""), "Foo",
  Json.JObject(
    List(
      ("struct" -> Json.JNumberStr("1")),
      ("second key" -> Json.JNumberStr("2")),
      ("enum" -> Json.JNumberStr("3")),
      ("def" -> Json.JNumberStr("4")))
    ))
  }

  test("test operators") {
    evalTest(List("""
package A

operator + = add
operator * = times

main = 1 + 2 * 3
"""), "A", VInt(7))
  }

  test("patterns in lambdas") {
    runBosatsuTest(List("""
package A

# you can't write \x: Int -> x.add(1)
# since Int -> looks like a type
# you need to protect it in a ( )
inc: Int -> Int = \x -> x.add(1)
inc2: Int -> Int = \(x: Int) -> x.add(1)

test = Assertion(inc(1).eq_Int(inc2(1)), "inc(1) == 2")
"""), "A", 1)

    runBosatsuTest(List("""
package A

def inc(x: Int): x.add(1)

test = Assertion(inc(1).eq_Int(2), "inc(1) == 2")
"""), "A", 1)

    runBosatsuTest(List("""
package A

struct Foo(v)

inc = \Foo(x) -> x.add(1)

test0 = Assertion(inc(Foo(1)).eq_Int(2), "inc(Foo(1)) == 2")

enum FooBar: F(x), B(x)

inc2 = \F(x) | B(x), Foo(y) -> x.add(y)
test1 = Assertion(inc2(F(1), Foo(1)).eq_Int(2), "inc2(F(1), Foo(1)) == 2")
test2 = Assertion(inc2(B(1), Foo(1)).eq_Int(2), "inc2(B(1), Foo(1)) == 2")

# with an outer tuple wrapping
inc3 = \(F(x) | B(x), Foo(y)) -> x.add(y)
test3 = Assertion(inc3((F(1), Foo(1))).eq_Int(2), "inc3((F(1), Foo(1))) == 2")
test4 = Assertion(inc3((B(1), Foo(1))).eq_Int(2), "inc3((B(1), Foo(1))) == 2")

# with a custom struct
struct Pair(x, y)
inc4 = \Pair(F(x) | B(x), Foo(y)) -> x.add(y)
test5 = Assertion(inc4(Pair(F(1), Foo(1))).eq_Int(2), "inc4(Pair(F(1), Foo(1))) == 2")
test6 = Assertion(inc4(Pair(B(1), Foo(1))).eq_Int(2), "inc4(Pair(B(1), Foo(1))) == 2")

suite = TestSuite("match tests", [test0, test1, test2, test3, test4, test5, test6])
"""), "A", 7)

    runBosatsuTest(List("""
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
"""), "A", 7)
  }

  test("test some error messages") {
    evalFail(
      List("""
package A

a = 1
""", """
package B

import A [ a ]

main = a"""), "B") { case PackageError.UnknownImportName(_, _, _, _) => () }

    evalFail(
      List("""
package B

import A [ a ]

main = a"""), "B") { case PackageError.UnknownImportPackage(_, _) => () }

    evalFail(
      List("""
package B

main = a"""), "B") { case te@PackageError.TypeErrorIn(_, _) =>
    val msg = te.message(Map.empty)
    assert(!msg.contains("Name("))
    assert(msg.contains("package B, name \"a\" unknown"))
    ()
  }

    evalFail(
      List("""
package B

x = 1

main = match x:
  Foo: 2
"""), "B") { case te@PackageError.SourceConverterErrorIn(_, _) =>
    val msg = te.message(Map.empty)
    assert(!msg.contains("Name("))
    assert(msg.contains("package B, unknown constructor Foo"))
    ()
  }

    evalFail(
      List("""
package B

struct X

main = match 1:
  X1: 0
"""), "B") { case te@PackageError.SourceConverterErrorIn(_, _) =>
      val b = assert(te.message(Map.empty) == "in file: <unknown source>, package B, unknown constructor X1\nRegion(44,45)")
      ()
    }

    evalFail(
      List("""
package A

main = match [1, 2, 3]:
  []: 0
  [*a, _, *b]: 2
"""), "A") { case te@PackageError.TotalityCheckError(_, _) =>
      val b = assert(te.message(Map.empty) == "in file: <unknown source>, package A\nRegion(19,60)\nmultiple splices in pattern, only one per match allowed")
      ()
    }

    evalFail(
      List("""
package A

enum Foo: Bar(a), Baz(b)

main = match Bar(a):
  Baz(b): b
"""), "A") { case te@PackageError.TotalityCheckError(_, _) =>
      val b = assert(te.message(Map.empty) == "in file: <unknown source>, package A\nRegion(45,70)\nnon-total match, missing: Bar(_)")
      ()
    }

    evalFail(
      List("""
package A

def fn(x):
  recur x:
    y: 0

main = fn
"""), "A") { case te@PackageError.RecursionError(_, _) =>
      val b = assert(te.message(Map.empty) == "in file: <unknown source>, package A, recur but no recursive call to fn\nRegion(25,42)\n")
      ()
    }

    evalFail(
      List("""
package A

def fn(x):
  recur 10:
    y: 0

main = fn
"""), "A") { case te@PackageError.RecursionError(_, _) =>
      val b = assert(te.message(Map.empty) == "in file: <unknown source>, package A, recur not on an argument to the def of fn, args: x\nRegion(25,43)\n")
      ()
    }

    evalFail(
      List("""
package A

def fn(x):
  recur y:
    y: 0

main = fn
"""), "A") { case te@PackageError.RecursionError(_, _) =>
      val b = assert(te.message(Map.empty) == "in file: <unknown source>, package A, recur not on an argument to the def of fn, args: x\nRegion(25,42)\n")
      ()
    }

    evalFail(
      List("""
package A

def fn(x):
  recur x:
    y:
      recur x:
        z: 100

main = fn
"""), "A") { case te@PackageError.RecursionError(_, _) =>
      val b = assert(te.message(Map.empty) == "in file: <unknown source>, package A, unexpected recur: may only appear unnested inside a def\nRegion(47,70)\n")
      ()
    }

    evalFail(
      List("""
package A

def fn(x):
  fn = 100
  recur x:
    y:
      recur x:
        z: 100

main = fn
"""), "A") { case te@PackageError.RecursionError(_, _) =>
      val b = assert(te.message(Map.empty) == "in file: <unknown source>, package A, illegal shadowing on: fn. Recursive shadowing of def names disallowed\nRegion(25,81)\n")
      ()
    }

    evalFail(
      List("""
package A

def fn(x, y):
  match x:
    0: y
    x: fn(x - 1, y + 1)

main = fn
"""), "A") { case te@PackageError.RecursionError(_, _) =>
      val b = assert(te.message(Map.empty) == "in file: <unknown source>, package A, invalid recursion on fn\nRegion(53,69)\n")
      ()
    }

    evalFail(
      List("""
package A

export [ foo ]

foo = 3
""", """
package B
import A [ fooz ]

baz = fooz
"""), "B") { case te@PackageError.UnknownImportName(_, _, _, _) =>
      val b = assert(te.message(Map.empty) == "in <unknown source> package: A does not have name fooz. Nearest: foo")
      ()
    }

    evalFail(
      List("""
package A

export [ foo ]

foo = 3
bar = 3
""", """
package B
import A [ bar ]

baz = bar
"""), "B") { case te@PackageError.UnknownImportName(_, _, _, _) =>
      val b = assert(te.message(Map.empty) == "in <unknown source> package: A has bar but it is not exported. Add to exports")
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
  Pair(f, s): Trip(3, s, f)

bgood = match b:
  "two": True
  _: False

tests = TestSuite("test triple",
  [ Assertion(a.eq_Int(3), "a == 3"),
    Assertion(bgood, b),
    Assertion(c.eq_Int(1), "c == 1") ])
"""), "A", 3)
  }

  test("regression from a map_List/list comprehension example from snoble") {
    runBosatsuTest(
      List("""
package RecordSet/Library

enum RowEntry[w]:
  REBool(value: w[Bool])
  REInt(value: w[Int])
  REString(value: w[String])

struct RecordField[t](name: String, to_entry: forall w. w[t] -> RowEntry[w])
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
  traverse: forall w1,w2. shape[w1] -> (forall ss. w1[ss] -> w2[ss]) -> shape[w2],
  record_to_list: forall w. shape[RecordRowEntry[w]] -> List[RowEntry[w]]
)

def get(sh: shape[RecordValue], RecordGetter(_, getter): RecordGetter[shape, t]) -> t:
  RecordValue(result) = sh.getter
  result

def create_field(rf: RecordField[t], fn: shape[RecordValue] -> t):
  RecordGetter(\sh -> rf, \sh -> RecordValue(fn(sh)))

def list_of_rows(RecordSet(fields, rows, getters, traverse, record_to_list): RecordSet[shape]):
  def getter_to_row_entry(row: shape[RecordValue]):
    (result_fn: forall tt. RecordGetter[shape, tt] -> RecordRowEntry[RecordValue, tt]) = \RecordGetter(get_field, get_value) ->
      RecordField(name, to_entry) = get_field(fields)
      RecordRowEntry(to_entry(get_value(row)))
    result_fn
  # This code should work the same if, map_List or list comprehension, but didn't previously
  #rows.map_List(\row -> record_to_list(getters.traverse(getter_to_row_entry(row))))
  [record_to_list(getters.traverse(getter_to_row_entry(row))) for row in rows]

struct RestructureOutput[shape1, shape2](
  reshaperFields: shape1[RecordField] -> shape2[RecordField],
  reshaperValues: shape1[RecordValue] -> shape2[RecordValue],
  getters: shape2[RecordGetter[shape2]],
  traverse: forall w1,w2. shape2[w1] -> (forall ss. w1[ss] -> w2[ss]) -> shape2[w2],
  record_to_list: forall w. shape2[RecordRowEntry[w]] -> List[RowEntry[w]]
)
def restructure(RecordSet(fields, rows, getters, _, _): RecordSet[shape1], f: shape1[RecordGetter[shape1]] -> RestructureOutput[shape1, shape2]) -> RecordSet[shape2]:
  RestructureOutput(reshaperF, reshaperV, new_getters, traverse, record_to_list) = f(getters)
  RecordSet(reshaperF(fields), rows.map_List(reshaperV), new_getters, traverse, record_to_list)

def concat_records(RecordSet(fields, rows, getters, traverse, record_to_list), more_rows):
  RecordSet(fields, rows.concat(more_rows), getters, traverse, record_to_list)

struct NilShape[w]
struct PS[t,rest,w](left: w[t], right: rest[w])

new_record_set = RecordSet(NilShape, [], NilShape, \NilShape,fn -> NilShape, \NilShape -> [])

(ps_end: forall t. RestructureOutput[t, NilShape]) = RestructureOutput(
  \ns -> NilShape,
  \ns -> NilShape,
  NilShape,
  \ns,fn -> NilShape,
  \ns -> []
)

def ps(
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

def string_field(name, fn: shape[RecordValue] -> String): RecordField(name, REString).create_field(fn)
def int_field(name, fn: shape[RecordValue] -> Int): RecordField(name, REInt).create_field(fn)
def bool_field(name, fn: shape[RecordValue] -> Bool): RecordField(name, REBool).create_field(fn)

##################################################

def and(x, y):
  y if x else False

operator && = and

def equals(compare, x, y):
  match compare(x,y):
    EQ: True
    _: False

def cmp_Bool(x, y):
  match (x, y):
    (True, False): GT
    (False, True): LT
    _: EQ

def equal_List(is_equal, l1, l2):
  recur l1:
    []: match l2:
      []: True
      _: False
    [h1, *r1]: match l2:
      []: False
      [h2, *r2]: is_equal(h1, h2) && equal_List(is_equal, r1, r2)

def equal_RowEntry(re1, re2):
  match (re1, re2):
    (REBool(RecordValue(x1)), REBool(RecordValue(x2))): cmp_Bool.equals(x1, x2)
    (REInt(RecordValue(x1)), REInt(RecordValue(x2))): cmp_Int.equals(x1, x2)
    (REString(RecordValue(x1)), REString(RecordValue(x2))): string_Order_fn.equals(x1, x2)
    _: False

equal_rows = equal_List(equal_RowEntry)

##################################################

rs_empty = new_record_set.restructure(\ns -> ps("String field".string_field(\_ -> ""), ps("Int field".int_field(\_ -> 0), ps("Bool field".bool_field(\_ -> True), ps_end))))

rs = rs_empty.concat_records([PS(RecordValue("a"), PS(RecordValue(1), PS(RecordValue(False), NilShape)))])

rs0 = rs.restructure(\PS(a, PS(b, PS(c, _))) -> ps(c, ps(b, ps(a, ps("Plus 2".int_field( \r -> r.get(b).add(2) ), ps_end)))))

tests = TestSuite("reordering",
  [
    Assertion(equal_rows.equal_List(rs0.list_of_rows, [[REBool(RecordValue(False)), REInt(RecordValue(1)), REString(RecordValue("a")), REInt(RecordValue(3))]]), "swap")
  ]
)
"""), "RecordSet/Library", 1)
  }

  test("record patterns") {
    runBosatsuTest(
      List("""
package A

struct Pair(first, second)

f2 = match Pair(1, "1"):
  Pair { first, ... }: first

tests = TestSuite("test record",
  [
    Assertion(f2.eq_Int(1), "f2 == 1"),
  ])
"""), "A", 1)

    runBosatsuTest(
      List("""
package A

struct Pair(first, second)

def res:
  Pair { first, ... } = Pair(1, 2)
  first

tests = TestSuite("test record",
  [
    Assertion(res.eq_Int(1), "res == 1"),
  ])
"""), "A", 1)

    runBosatsuTest(
      List("""
package A

struct Pair(first, second)

get = \Pair { first, ...} -> first

res = get(Pair(1, "two"))

tests = TestSuite("test record",
  [
    Assertion(res.eq_Int(1), "res == 1"),
  ])
"""), "A", 1)

    runBosatsuTest(
      List("""
package A

struct Pair(first, second)

get = \Pair { first: f, ...} -> f

res = get(Pair(1, "two"))

tests = TestSuite("test record",
  [
    Assertion(res.eq_Int(1), "res == 1"),
  ])
"""), "A", 1)

    runBosatsuTest(
      List("""
package A

struct Pair(first, second)

get = \Pair(first, ...) -> first

res = get(Pair(1, "two"))

tests = TestSuite("test record",
  [
    Assertion(res.eq_Int(1), "res == 1"),
  ])
"""), "A", 1)

    runBosatsuTest(
      List("""
package A

struct Pair(first, second)

get = \Pair(first, ...) -> first

res = get(Pair { first: 1, second: "two" })

tests = TestSuite("test record",
  [
    Assertion(res.eq_Int(1), "res == 1"),
  ])
"""), "A", 1)

    runBosatsuTest(
      List("""
package A

struct Pair(first, second)

get = \Pair(first, ...) -> first

first = 1

res = get(Pair { first, second: "two" })

tests = TestSuite("test record",
  [
    Assertion(res.eq_Int(1), "res == 1"),
  ])
"""), "A", 1)

    evalFail(
      List("""
package A

struct Pair(first, second)

get = \Pair(first, ...) -> first

# missing second
first = 1
res = get(Pair { first })
"""), "A") { case s@PackageError.SourceConverterErrorIn(_, _) => s.message(Map.empty); () }

    evalFail(
      List("""
package A

struct Pair(first, second)

get = \Pair(first, ...) -> first

# third is unknown
first = 1
second = 3
res = get(Pair { first, second, third })
"""), "A") { case s@PackageError.SourceConverterErrorIn(_, _) => s.message(Map.empty); () }

    evalFail(
      List("""
package A

struct Pair(first, second)

get = \Pair { first } -> first

res = get(Pair(1, "two"))
"""), "A") { case s@PackageError.SourceConverterErrorIn(_, _) => s.message(Map.empty); () }

    evalFail(
      List("""
package A

struct Pair(first, second)

# Pair has two fields
get = \Pair(first) -> first

res = get(Pair(1, "two"))
"""), "A") { case s@PackageError.SourceConverterErrorIn(_, _) => s.message(Map.empty); () }

    evalFail(
      List("""
package A

struct Pair(first, second)

# Pair does not have a field called sec
get = \Pair { first, sec: _ } -> first

res = get(Pair(1, "two"))
"""), "A") { case s@PackageError.SourceConverterErrorIn(_, _) => s.message(Map.empty); () }

    evalFail(
      List("""
package A

struct Pair(first, second)

# Pair does not have a field called sec
get = \Pair { first, sec: _, ... } -> first

res = get(Pair(1, "two"))
"""), "A") { case s@PackageError.SourceConverterErrorIn(_, _) => s.message(Map.empty); () }

    evalFail(
      List("""
package A

struct Pair(first, second)

# Pair has two fields, not three
get = \Pair(first, _, _) -> first

res = get(Pair(1, "two"))
"""), "A") { case s@PackageError.SourceConverterErrorIn(_, _) => s.message(Map.empty); () }

    evalFail(
      List("""
package A

struct Pair(first, second)

# Pair has two fields, not three
get = \Pair(first, _, _, ...) -> first

res = get(Pair(1, "two"))
"""), "A") { case s@PackageError.SourceConverterErrorIn(_, _) => s.message(Map.empty); () }
  }

  test("test scoping bug (issue #311)") {

    runBosatsuTest(List("""package A

struct Foo(x, y)

Foo { x, ... } = Foo(42, "42")

tests = TestSuite("test record",
  [
    Assertion(x.eq_Int(42), "x == 42"),
  ])
"""), "A", 1)

    runBosatsuTest(List("""package A

struct Foo(x, y)

a = Foo(42, "42")
x = match a:
   Foo(y, _): y

tests = TestSuite("test record",
  [
    Assertion(x.eq_Int(42), "x == 42"),
  ])
"""), "A", 1)

    runBosatsuTest(List("""package A

struct Foo(x, y)

a = Foo(42, "42")
x = match a:
   Foo(y, _): y

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
"""), "A", 1)

  }

  test("test ordered shadowing issue #328") {
    runBosatsuTest(List("""package A

def one: 1

two = one.add(1)

def one: "one"

good = match (one, two):
  ("one", 2): True
  _:     False

tests = TestSuite("test",
  [
    Assertion(good, ""),
  ])
"""), "A", 1)

    // test an example using a predef function, like add
    runBosatsuTest(List("""package A

# this should be add from predef
two = add(1, 1)

def add(x, y):
  x.sub(y)

good = match two:
  2: True
  _:     False

tests = TestSuite("test",
  [
    Assertion(good, ""),
  ])
"""), "A", 1)
  }

  test("shadowing of external def isn't allowed") {
    evalFail(
      List("""
package A

external def foo(x: String) -> List[String]

def foo(x): x

"""), "A") { case s@PackageError.SourceConverterErrorIn(_, _) =>
      assert(s.message(Map.empty) == "in file: <unknown source>, package A, bind names foo shadow external def\nRegion(57,71)")
      ()
    }

    evalFail(
      List("""
package A

external def foo(x: String) -> List[String]

foo = 1

"""), "A") { case s@PackageError.SourceConverterErrorIn(_, _) =>
      assert(s.message(Map.empty) == "in file: <unknown source>, package A, bind names foo shadow external def\nRegion(57,65)")
      ()
    }

    evalFail(
      List("""
package A

external def foo(x: String) -> List[String]

external def foo(x: String) -> List[String]
"""), "A") { case s@PackageError.SourceConverterErrorIn(_, _) =>
      assert(s.message(Map.empty) == "in file: <unknown source>, package A, foo defined multiple times\nRegion(21,55)")
      ()
    }
  }

  test("test meta escape bug") {
    runBosatsuTest(List("""
package A

struct Build[f]
struct File

def useList(args: List[Build[File]]):
  True

check = useList([])

tests = Assertion(check, "none")
"""), "A", 1)
  }

  test("type parameters must be supersets for structs and enums fails") {
evalFail(
      List("""
package Err

struct Foo[a](a)

main = Foo(1, "2")
"""), "Err") { case sce@PackageError.SourceConverterErrorIn(_, _) =>
      assert(sce.message(Map.empty) == "in file: <unknown source>, package Err, Foo found declared: [a], not a superset of [anon0]\nRegion(14,30)")
      ()
    }

evalFail(
      List("""
package Err

struct Foo[a](a: a, b: b)

main = Foo(1, "2")
"""), "Err") { case sce@PackageError.SourceConverterErrorIn(_, _) =>
      assert(sce.message(Map.empty) == "in file: <unknown source>, package Err, Foo found declared: [a], not a superset of [a, b]\nRegion(14,39)")
      ()
    }

evalFail(
      List("""
package Err

enum Enum[a]: Foo(a)

main = Foo(1, "2")
"""), "Err") { case sce@PackageError.SourceConverterErrorIn(_, _) =>
      assert(sce.message(Map.empty) == "in file: <unknown source>, package Err, Enum found declared: [a], not a superset of [anon0]\nRegion(14,34)")
      ()
    }

evalFail(
      List("""
package Err

enum Enum[a]: Foo(a: a), Bar(a: b)

main = Foo(1, "2")
"""), "Err") { case sce@PackageError.SourceConverterErrorIn(_, _) =>
      assert(sce.message(Map.empty) == "in file: <unknown source>, package Err, Enum found declared: [a], not a superset of [a, b]\nRegion(14,48)")
      ()
    }
  }

}
