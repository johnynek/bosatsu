package org.bykn.bosatsu

import org.scalatest.FunSuite

class Regressions extends FunSuite {
  import TestUtils._

  test("test complex recursion case from #196") {
    evalFail(List("""
package Foo

struct Field(name: String, extract: a -> b)

def applyFields(fields, row):
  recur fields:
    (f, Some(s)):
      Field(_, fn) = f
      rec = applyFields(s, row)
      (fn(row), Some(rec))
    (f, None):
      Field(_, fn) = f
      (fn(row), None)

hlist = (Field("a", \x -> "a"), Some((Field("b", \x -> "b"), None)))
main = applyFields(hlist, 1)
"""), "Foo") { case PackageError.TypeErrorIn(_, _) => () }
  }
}
