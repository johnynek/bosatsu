package org.bykn.bosatsu

import cats.data.Validated
import org.scalatest.FunSuite

class DefRecursionCheckTest extends FunSuite {

  def allowed(teStr: String) = {
    val stmt = TestUtils.statementOf(Predef.packageName, teStr)
    DefRecursionCheck.checkStatement(stmt) match {
      case Validated.Valid(_) => succeed
      case Validated.Invalid(errs) =>
        fail(s"$errs")
    }
  }

  def disallowed(teStr: String) = {
    val stmt = TestUtils.statementOf(Predef.packageName, teStr)
    DefRecursionCheck.checkStatement(stmt) match {
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
recursive def foo: foo
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

  test("shadowing is okay") {
    allowed("""#
def foo(x):
  foo = \x -> 1
  foo(x)
""")
    allowed("""#
def foo(x):
  def foo(x): x
  foo(x)
""")

    allowed("""#
def foo(foo): foo
""")
  }

  test("substructural recursion is okay") {
    allowed("""#
recursive def len(lst):
  recur lst:
    []: 0
    [_, *tail]: len(tail)
""")
    allowed("""#
recursive def len(lst):
  empty = 0
  recur lst:
    []: empty
    [_, *tail]: len(tail).add(1)
""")
    allowed("""#
recursive def len(lst, acc):
  recur lst:
    []: acc
    [_, *tail]: len(tail, acc.add(1))
""")
    disallowed("""#
recursive def len(lst):
  recur lst:
    [*list]: len(list)
""")
    disallowed("""#
recursive def len(lst):
  recur lst:
    [h, *t]: len(lst)
    []: 0
""")
    disallowed("""#
recursive def len(lst):
  recur lst:
    []: 0
    [_, *tail]:
      # we can't trivially see this is okay
      fn = \arg -> arg(tail)
      fn(len)
""")
  }

  test("shadowing recursive def isn't okay") {
    disallowed("""#
recursive def len(lst):
  recur lst:
    []:
      len = \x -> 0
      len(lst)
    [_, *tail]: len(tail)
""")

    disallowed("""#
recursive def len(lst):
  len = "shadow"
  recur lst:
    []: 0
    [_, *tail]: len(tail)
""")
    disallowed("""#
recursive def len(lst):
  recur lst:
    []: 0
    [_, *tail]:
      len = "shadow"
      len(tail)
""")
    disallowed("""#
recursive def len(lst):
  recur lst:
    []: 0
    [_, *tail]:
      # shadowing len is not okay
      fn = \len -> len(tail)
      fn(len)
""")
    disallowed("""#
recursive def len(lst):
  recur lst:
    []: 0
    [_, *len]:
      # shadowing len is not okay
      12
""")
    disallowed("""#
len = 2
recursive def len(lst):
  # even though len exists above, it is shadowed
  # by this method, and currently inaccessible
  x = len
  recur lst:
    []: 0
    [_, *_]:
      # shadowing len is not okay
      12
""")
  }

  test("infinite loop isn't okay") {
    allowed("""#
def loop(x):
  loop(x)
""")
  }

  test("recur can only appear in a recursive def") {
    disallowed("""#
def len(lst):
  recur lst:
    []: 0
    [_, *tail]: 1
""")

    disallowed("""#
recursive def len(lst):
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
recursive def len(lst):
  recursive def foo(x): x
  recur lst:
    []: 0
    [_, *tail]: 1
""")

    allowed("""#
recursive def len(lst):
  def foo(x): x
  recur lst:
    []: 0
    [_, *tail]: 1
""")
  }
  test("multiple arguments") {
    allowed("""#
recursive def len(lst, a):
  recur lst:
    []: a
    [_, *tail]: len(tail, a.plus(1))
""")
    allowed("""#
recursive def len(a, lst):
  recur lst:
    []: a
    [_, *tail]: len(a.plus(1), tail)
""")
  }
  test("recur is not return") {
    allowed("""#
recursive def dots(lst):
  a = recur lst:
    []: ""
    [head, *tail]: dots(tail).concat(head)
  a.concat(".")
""")
  }
  test("multiple recur") {
    allowed("""#
recursive def foo(lst):
  a = recur lst:
    []: 0
    [_, *tail]: foo(tail).plus(1)

  b = recur lst:
    []: 1
    [_, *tail]: foo(tail).plus(2)

  a.plus(b)
""")

    allowed("""#
recursive def foo(lstA, lstB):
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
recursive def zip(lstA, lstB):
  match lstA:
    []: []
    [headA, *tailA]: recur lstB:
      []: []
      [headB, *tailB]: [(headA, headB)].concat(zip(tailA, tailB))
""")
    allowed("""#
recursive def zip(lstA, lstB):
  recur lstA:
    []: []
    [headA, *tailA]: match lstB:
      []: []
      [headB, *tailB]: [(headA, headB)].concat(zip(tailA, tailB))
""")
  }
}
