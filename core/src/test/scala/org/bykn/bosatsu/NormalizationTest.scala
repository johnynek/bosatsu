package org.bykn.bosatsu

import org.scalatest.FunSuite

class NormalizationTest extends FunSuite {
  import TestUtils._
  import NormalExpression._
  import Lit._
  import Normalization._
  //   test("package import normalization") {
//     val p1 = parse(
// """
// package Foo
// export [ main ]

// main = 1
// """)

//     val p2 = parse(
// """
// package Foo2
// import Foo [ main as mainFoo ]
// export [ main, ]

// main = mainFoo
// """)

//     val (_, validatedPackageMap) = PackageMap.resolveThenInfer(List(((), p1), ((), p2)))
//     val packageMap = validatedPackageMap match {
//       case Validated.Valid(rpm) => rpm
//       case Validated.Invalid(err) => fail(err.toString)
//     }

//     succeed
//   }


  test("simple package normalizes") {
      normalizeTest(
        List("""
package Willem/Foo

struct Pair(first, second)

def bar(x):
  baz = \y -> Pair(x, y)
  baz(10)

main = bar(5)
"""
        ), "Willem/Foo", NormalExpressionTag(Literal(Str("aa")), Set())
      )
  }
}
