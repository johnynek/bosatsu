package org.bykn.bosatsu

import org.scalatest.funsuite.AnyFunSuite

class Regressions extends AnyFunSuite with ParTest {
  import TestUtils.*

  test("test complex recursion case from #196") {
    evalFail(List("""
package Foo

struct Field(name: String, extract: a -> b)

def applyFields(fields, row):
  recur fields:
    case (f, Some(s)):
      Field(_, fn) = f
      rec = applyFields(s, row)
      (fn(row), Some(rec))
    case (f, None):
      Field(_, fn) = f
      (fn(row), None)

hlist = (Field("a", x -> "a"), Some((Field("b", x -> "b"), None)))
main = applyFields(hlist, 1)
""")) { case PackageError.TypeErrorIn(_, _) => () }
  }
}
