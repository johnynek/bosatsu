package dev.bosatsu

import cats.data.Validated

import cats.implicits._

class DefRecursionCheckTest extends munit.FunSuite {

  def allowed(teStr: String) = {
    val stmt = TestUtils.statementsOf(teStr)
    stmt.traverse_(DefRecursionCheck.checkStatement(_)) match {
      case Validated.Valid(_)      => ()
      case Validated.Invalid(errs) =>
        fail(s"$errs")
    }
  }

  def disallowed(teStr: String) = {
    val stmt = TestUtils.statementsOf(teStr)
    stmt.traverse_(DefRecursionCheck.checkStatement(_)) match {
      case Validated.Valid(_)   => fail("expected failure")
      case Validated.Invalid(_) => ()
    }
  }

  test("non recursive cases are allowed") {
    allowed("""#
x = 1
""")
  }

  test("zero arg recursion isn't allowed") {
    allowed("""#
x = 1
x = x.plus(1)
""")
    // shadowing is okay
    allowed("""#
foo = (
  x = 1
  x = 1.plus(1)
  x)
""")

    // typechecking catches this issue
    allowed("""#
foo = (
  x = x
  x)
""")
    allowed("""#
foo = (
  x = 1
  x)
""")
  }

  test("shadowing defs is never okay") {
    disallowed("""#
def foo(x):
  foo = x -> 1
  foo(x)
""")
    disallowed("""#
def foo(x):
  def foo(x): x
  foo(x)
""")

    disallowed("""#
def foo(foo): foo
""")
  }

  test("substructural recursion is okay") {
    allowed("""#
def len(lst):
  recur lst:
    case []: 0
    case [_, *tail]: len(tail)
""")
    allowed("""#
def len(lst):
  empty = 0
  recur lst:
    case []: empty
    case [_, *tail]: len(tail).add(1)
""")
    allowed("""#
def len(lst, acc):
  recur lst:
    case []: acc
    case [_, *tail]: len(tail, acc.add(1))
""")
    disallowed("""#
def len(lst):
  recur lst:
    case [*list]: len(list)
""")
    disallowed("""#
def len(lst):
  recur lst:
    case [h, *t]: len(lst)
    case []: 0
""")
    disallowed("""#
def len(lst):
  recur lst:
    case []: 0
    case [_, *tail]:
      # we can't trivially see this is okay
      fn = arg -> arg(tail)
      fn(len)
""")
  }

  test("shadowing def isn't okay") {
    disallowed("""#
def len(lst):
  recur lst:
    case []:
      len = x -> 0
      len(lst)
    case [_, *tail]: len(tail)
""")

    disallowed("""#
def len(lst):
  len = "shadow"
  recur lst:
    case []: 0
    case [_, *tail]: len(tail)
""")
    disallowed("""#
def len(lst):
  recur lst:
    case []: 0
    case [_, *tail]:
      len = "shadow"
      len(tail)
""")
    disallowed("""#
def len(lst):
  recur lst:
    case []: 0
    case [_, *tail]:
      # shadowing len is not okay
      fn = len -> len(tail)
      fn(len)
""")
    disallowed("""#
def len(lst):
  recur lst:
    case []: 0
    case [_, *len]:
      # shadowing len is not okay
      12
""")
    disallowed("""#
len = 2
def len(lst):
  # even though len exists above, it is shadowed
  # by this method, and currently inaccessible
  x = len
  recur lst:
    case []: 0
    case [_, *_]:
      # shadowing len is not okay
      12
""")
    disallowed("""#
def len(lst):
  def len_helper(l, r):
    len = l.add(r)
    len
  recur lst:
    case []: 0
    case [_, *t]: len_helper(len(t), 1)
""")
  }

  test("infinite loop isn't okay") {
    disallowed("""#
def loop(x):
  loop(x)
""")
  }

  test("recur can only appear in a def") {
    disallowed("""#
def len(lst):
  recur lst:
    case []: 0
    case [_, *tail]: 1
""")

    disallowed("""#
def len(lst):
  def foo(x):
    recur lst:
      case 0: 1
  recur lst:
    case []: 0
    case [_, *tail]: 1
""")
  }

  test("nested recursion is not allowed") {
    disallowed("""#
def len(lst):
  def foo(x): x
  recur lst:
    case []: 0
    case [_, *tail]: 1
""")
    disallowed("""#
def len(lst):
  def bar(x):
    def foo(x):
      recur x:
        case []: 0
        case [_, *t]: foo(t)
    foo(x)
  recur lst:
    case []: 0
    case [_, *tail]: 1
""")

    allowed("""#
def len(lst):
  def foo(x): x
  recur lst:
    case []: 0
    case [_, *tail]: len(tail)
""")
  }

  test("all recursions must be on the same variable") {
    // if not, we can make an infinite loop, e.g.:
    // this a great example from @snoble
    // https://github.com/johnynek/bosatsu/pull/168#issuecomment-472724055
    disallowed("""#
def foo(lstA, lstB):
  x = recur lstA:
    case []: 1
    case [a, *as]: foo(as, [1,*lstB])
  y = recur lstB:
    case []: 2
    case [b, *bs]: foo([1, *lstA], bs)
  x.add(y)
""")
  }

  test("multiple arguments") {
    allowed("""#
def len(lst, a):
  recur lst:
    case []: a
    case [_, *tail]: len(tail, a.plus(1))
""")
    allowed("""#
def len(a, lst):
  recur lst:
    case []: a
    case [_, *tail]: len(a.plus(1), tail)
""")
  }
  test("recur is not return") {
    allowed("""#
def dots(lst):
  a = recur lst:
    case []: ""
    case [head, *tail]: dots(tail).concat(head)
  a.concat(".")
""")
  }
  test("multiple recur is not allowed") {
    disallowed("""#
def foo(lst):
  a = recur lst:
    case []: 0
    case [_, *tail]: foo(tail).plus(1)

  b = recur lst:
    case []: 1
    case [_, *tail]: foo(tail).plus(2)

  a.plus(b)
""")

    disallowed("""#
def foo(lstA, lstB):
  a = recur lstA:
    case []: 0
    case [_, *tail]: foo(tail, listB).plus(1)

  b = recur lstB:
    case []: 1
    case [_, *tail]: foo(lstA.concat(1), tail).plus(2)

  a.plus(b)
""")
  }
  test("zip") {
    allowed("""#
def zip(lstA, lstB):
  match lstA:
    case []: []
    case [headA, *tailA]: recur lstB:
      case []: []
      case [headB, *tailB]: [(headA, headB)].concat(zip(tailA, tailB))
""")
    allowed("""#
def zip(lstA, lstB):
  recur lstA:
    case []: []
    case [headA, *tailA]: match lstB:
      case []: []
      case [headB, *tailB]: [(headA, headB)].concat(zip(tailA, tailB))
""")
  }

  test("two side by side defs are allowed") {
    allowed("""#
def foo(x):
  def bar(x):
    recur x:
      case []: 0
      case [_, *t]: bar(t)

  def baz(x):
    recur x:
      case []: 0
      case [_, *t]: baz(t)
  baz([bar([1])])
""")
  }

  test("we must recur on a literal arg") {
    allowed("""#
def foo(x):
  recur x:
    case []: 0
    case [_, *t]: foo(t)
""")
    disallowed("""#
def foo(x):
  recur bar(x):
    case []: 0
    case [_, *t]: foo(t)
""")
    disallowed("""#
def foo(x):
  recur y:
    case []: 0
    case [_, *t]: foo(t)
""")
  }

  test("we must recur in the right argument position") {
    disallowed("""#
def len(acc, x):
  recur x:
    case []: acc
    case [_, *t]: len(t)
""")
  }
  test("we can use a list comprehension") {
    allowed("""#
def nest(lst):
  recur lst:
    case []: []
    case [_, *tail]: [NonEmptyList(h, nest(tail)) for h in lst]
""")
  }

  test("we can't use an outer def recursively") {
    disallowed("""#
def foo(x):
  def bar(y):
    foo(y)
  bar(x)
""")
  }

  test("we can make a recursive def in another recursive def") {
    allowed("""#
def len(lst):
  # this is doing nothing, but is a nested recursion
  def len0(lst):
    recur lst:
      case []: 0
      case [_, *t]: len0(t).add(1)

  recur lst:
    case []: 0
    case [_, *t]: len(t)
""")
  }

  test("we can call a non-outer function in a recur branch") {
    allowed("""#
def id(x): x

def len(lst):
  recur lst:
    case []: 0
    case [_, *t]: id(len(t))
""")
  }

  test("tree example") {
    allowed("""#
struct Tree(x: Int, items: List[Tree])

def sum_all(t):
  recur t:
    case []: 0
    case [Tree(x, children), *tail]: x + sum_all(children) + sum_all(tail)
""")
  }

  test("we can recur on cont") {
    allowed("""#
enum Cont:
  Item(a: Int)
  Next(use: (Cont -> Int) -> Int)

def loop(box: Cont) -> Int:
  recur box:
    case Item(a): a
    case Next(cont_fn):
      cont_fn(cont -> loop(cont))
    """)

    allowed("""#
enum Cont:
  Item(a: Int)
  Next(use: (Cont -> Int) -> Int)

def loop(box: Cont) -> Int:
  recur box:
    case Item(a): a
    case Next(cont_fn):
      cont_fn(loop)
    """)
  }

  test("we can't trick the checker with a let shadow") {
    disallowed("""#
struct Box(a)

def anything0[a, b](box: Box[a]) -> b:
  recur box:
    case Box(b):
      # shadow to trick
      b = Box(b)
      anything0(b)

bottom: forall a. a = anything0(Box(1)) 

""")
  }

  test("we can't trick the checker with a match shadow") {
    disallowed("""#
struct Box(a)

def anything0[a, b](box: Box[a]) -> b:
  recur box:
    case Box(b):
      # shadow to trick
      match Box(b):
        case b: anything0(b)

bottom: forall a. a = anything0(Box(1)) 

""")

    disallowed("""#
struct Box(a)

def anything0[a, b](box: Box[a]) -> b:
  recur box:
    case Box(b):
      # shadow to trick
      recur Box(b):
        case b: anything0(b)

bottom: forall a. a = anything0(Box(1)) 

""")
  }

  test("we can't trick the checker with a lambda-let shadow") {
    disallowed("""#
struct Box(a)

def anything0[a, b](box: Box[a]) -> b:
  recur box:
    case Box(b):
      # shadow to trick
      (b -> anything0(b))(Box(b))

bottom: forall a. a = anything0(Box(1)) 

""")
  }

  test("we can't trick the checker with a def-let shadow") {
    disallowed("""#
struct Box(a)

def anything0[a, b](box: Box[a]) -> b:
  recur box:
    case Box(b):
      # shadow to trick
      def trick(b): anything0(b)

      trick(Box(b))

bottom: forall a. a = anything0(Box(1)) 

""")
  }
}
