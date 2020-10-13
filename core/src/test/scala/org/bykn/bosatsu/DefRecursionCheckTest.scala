package org.bykn.bosatsu

import cats.data.Validated
import org.scalatest.funsuite.AnyFunSuite

class DefRecursionCheckTest extends AnyFunSuite {

  def allowed(teStr: String) = {
    val stmt = TestUtils.statementsOf(PackageName.PredefName, teStr)
    stmt.traverse_(DefRecursionCheck.checkStatement(_)) match {
      case Validated.Valid(_) => succeed
      case Validated.Invalid(errs) =>
        fail(s"$errs")
    }
  }

  def disallowed(teStr: String) = {
    val stmt = TestUtils.statementsOf(PackageName.PredefName, teStr)
    stmt.traverse_(DefRecursionCheck.checkStatement(_)) match {
      case Validated.Valid(_) => fail("expected failure")
      case Validated.Invalid(_) => succeed
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
def foo:
  x = 1
  x = 1.plus(1)
  x
""")

    disallowed("""#
def foo: foo
""")

    // typechecking catches this issue
    allowed("""#
def foo:
  x = x
  x
""")
    allowed("""#
def foo:
  x = 1
  x
""")
  }

  test("shadowing defs is never okay") {
    disallowed("""#
def foo(x):
  foo = \x -> 1
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
    []: 0
    [_, *tail]: len(tail)
""")
    allowed("""#
def len(lst):
  empty = 0
  recur lst:
    []: empty
    [_, *tail]: len(tail).add(1)
""")
    allowed("""#
def len(lst, acc):
  recur lst:
    []: acc
    [_, *tail]: len(tail, acc.add(1))
""")
    disallowed("""#
def len(lst):
  recur lst:
    [*list]: len(list)
""")
    disallowed("""#
def len(lst):
  recur lst:
    [h, *t]: len(lst)
    []: 0
""")
    disallowed("""#
def len(lst):
  recur lst:
    []: 0
    [_, *tail]:
      # we can't trivially see this is okay
      fn = \arg -> arg(tail)
      fn(len)
""")
  }

  test("shadowing def isn't okay") {
    disallowed("""#
def len(lst):
  recur lst:
    []:
      len = \x -> 0
      len(lst)
    [_, *tail]: len(tail)
""")

    disallowed("""#
def len(lst):
  len = "shadow"
  recur lst:
    []: 0
    [_, *tail]: len(tail)
""")
    disallowed("""#
def len(lst):
  recur lst:
    []: 0
    [_, *tail]:
      len = "shadow"
      len(tail)
""")
    disallowed("""#
def len(lst):
  recur lst:
    []: 0
    [_, *tail]:
      # shadowing len is not okay
      fn = \len -> len(tail)
      fn(len)
""")
    disallowed("""#
def len(lst):
  recur lst:
    []: 0
    [_, *len]:
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
    []: 0
    [_, *_]:
      # shadowing len is not okay
      12
""")
    disallowed("""#
def len(lst):
  def len_helper(l, r):
    len = l.add(r)
    len
  recur lst:
    []: 0
    [_, *t]: len_helper(len(t), 1)
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
    []: 0
    [_, *tail]: 1
""")

    disallowed("""#
def len(lst):
  def foo(x):
    recur lst:
      0: 1
  recur lst:
    []: 0
    [_, *tail]: 1
""")
  }

  test("nested recursion is not allowed") {
    disallowed("""#
def len(lst):
  def foo(x): x
  recur lst:
    []: 0
    [_, *tail]: 1
""")
    disallowed("""#
def len(lst):
  def bar(x):
    def foo(x):
      recur x:
        []: 0
        [_, *t]: foo(t)
    foo(x)
  recur lst:
    []: 0
    [_, *tail]: 1
""")

    allowed("""#
def len(lst):
  def foo(x): x
  recur lst:
    []: 0
    [_, *tail]: len(tail)
""")
  }

  test("all recursions must be on the same variable") {
    // if not, we can make an infinite loop, e.g.:
    // this a great example from @snoble
    // https://github.com/johnynek/bosatsu/pull/168#issuecomment-472724055
    disallowed("""#
def foo(lstA, lstB):
  x = recur lstA:
    []: 1
    [a, *as]: foo(as, [1,*lstB])
  y = recur lstB:
    []: 2
    [b, *bs]: foo([1, *lstA], bs)
  x.add(y)
""")
  }

  test("multiple arguments") {
    allowed("""#
def len(lst, a):
  recur lst:
    []: a
    [_, *tail]: len(tail, a.plus(1))
""")
    allowed("""#
def len(a, lst):
  recur lst:
    []: a
    [_, *tail]: len(a.plus(1), tail)
""")
  }
  test("recur is not return") {
    allowed("""#
def dots(lst):
  a = recur lst:
    []: ""
    [head, *tail]: dots(tail).concat(head)
  a.concat(".")
""")
  }
  test("multiple recur is not allowed") {
    disallowed("""#
def foo(lst):
  a = recur lst:
    []: 0
    [_, *tail]: foo(tail).plus(1)

  b = recur lst:
    []: 1
    [_, *tail]: foo(tail).plus(2)

  a.plus(b)
""")

    disallowed("""#
def foo(lstA, lstB):
  a = recur lstA:
    []: 0
    [_, *tail]: foo(tail, listB).plus(1)

  b = recur lstB:
    []: 1
    [_, *tail]: foo(lstA.concat(1), tail).plus(2)

  a.plus(b)
""")
  }
  test("zip") {
    allowed("""#
def zip(lstA, lstB):
  match lstA:
    []: []
    [headA, *tailA]: recur lstB:
      []: []
      [headB, *tailB]: [(headA, headB)].concat(zip(tailA, tailB))
""")
    allowed("""#
def zip(lstA, lstB):
  recur lstA:
    []: []
    [headA, *tailA]: match lstB:
      []: []
      [headB, *tailB]: [(headA, headB)].concat(zip(tailA, tailB))
""")
  }

  test("two side by side defs are allowed") {
    allowed("""#
def foo(x):
  def bar(x):
    recur x:
      []: 0
      [_, *t]: bar(t)

  def baz(x):
    recur x:
      []: 0
      [_, *t]: baz(t)
  baz([bar([1])])
""")
  }

  test("we must recur on a literal arg") {
    allowed("""#
def foo(x):
  recur x:
    []: 0
    [_, *t]: foo(t)
""")
    disallowed("""#
def foo(x):
  recur bar(x):
    []: 0
    [_, *t]: foo(t)
""")
    disallowed("""#
def foo(x):
  recur y:
    []: 0
    [_, *t]: foo(t)
""")
  }

  test("we must recur in the right argument position") {
    disallowed("""#
def len(acc, x):
  recur x:
    []: acc
    [_, *t]: len(t)
""")
  }
  test("we can use a list comprehension") {
    allowed("""#
def nest(lst):
  recur lst:
    []: []
    [_, *tail]: [NonEmptyList(h, nest(tail)) for h in lst]
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
      []: 0
      [_, *t]: len0(t).add(1)

  recur lst:
    []: 0
    [_, *t]: len(t)
""")
  }

  test("we can call a non-outer function in a recur branch") {
allowed("""#
def id(x): x

def len(lst):
  recur lst:
    []: 0
    [_, *t]: id(len(t))
""")
  }
}
