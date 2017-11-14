package org.bykn.edgemar

import cats.data.Validated
import fastparse.all._
import org.scalatest.FunSuite

class PackageTest extends FunSuite {

  def parse(s: String): Package[PackageName] =
    Package.parser.parse(s) match {
      case Parsed.Success(p, idx) =>
        assert(idx == s.length)
        p
      case Parsed.Failure(exp, idx, extra) =>
        sys.error(s"failed to parse: $s: $exp at $idx with trace: ${extra.traced.trace}")
    }

  def valid[A, B](v: Validated[A, B]) =
    v match {
      case Validated.Valid(_) => succeed
      case Validated.Invalid(err) => fail(err.toString)
    }

  def invalid[A, B](v: Validated[A, B]) =
    v match {
      case Validated.Valid(err) => fail(err.toString)
      case Validated.Invalid(_) => succeed
    }

  test("simple package resolves") {
    val p1 = parse(
"""
package Foo

main = 1
""")
    val p2 = parse(
"""
package Foo2
import Foo [ main as mainFoo ]

main = 1
""")

    val p3 = parse(
"""
package Foo
import Foo2 [ main as mainFoo ]

main = 1
""")

    valid(PackageMap.resolveAll(List(p1)))
    valid(PackageMap.resolveAll(List(p1, p2)))
    invalid(PackageMap.resolveAll(List(p2, p3))) // loop here
  }
}
