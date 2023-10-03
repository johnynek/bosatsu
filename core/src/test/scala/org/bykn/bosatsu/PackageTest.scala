package org.bykn.bosatsu

import cats.Show
import cats.data.{Validated, ValidatedNel}

import IorMethods.IorExtension
import org.scalatest.funsuite.AnyFunSuite

class PackageTest extends AnyFunSuite with ParTest {

  def resolveThenInfer(
      ps: Iterable[Package.Parsed]
  ): ValidatedNel[PackageError, PackageMap.Inferred] = {
    implicit val showInt: Show[Int] = Show.fromToString
    PackageMap
      .resolveThenInfer(ps.toList.zipWithIndex.map(_.swap), Nil)
      .strictToValidated
  }

  def parse(s: String): Package.Parsed =
    Parser.unsafeParse(Package.parser(None), s)

  def parseUnit(ss: Iterable[String]) =
    resolveThenInfer(ss.map(parse(_)))

  def valid[A, B](v: Validated[A, B]) =
    v match {
      case Validated.Valid(_)     => succeed
      case Validated.Invalid(err) => fail(err.toString)
    }

  def invalid[A, B](v: Validated[A, B]) =
    v match {
      case Validated.Valid(err) => fail(err.toString)
      case Validated.Invalid(_) => succeed
    }

  test("simple package resolves") {
    val p1 = parse("""
package Foo
export main

main = 1
""")
    val p2 = parse("""
package Foo2
from Foo import main as mainFoo
export main,

main = mainFoo
""")

    val p3 = parse("""
package Foo
from Foo2 import main as mainFoo

main = 1
""")

    valid(resolveThenInfer(List(p1)))
    valid(resolveThenInfer(List(p1, p2)))
    invalid(resolveThenInfer(List(p2, p3))) // loop here

    val p4 = parse("""
package P4
from Foo2 import main as one

external def add(a: a, b: a) -> a

main = add(one, 42)
""")
    valid(resolveThenInfer(List(p1, p2, p4)))

    val p5 = parse("""
package P5

export Option(), List(), head, tail

enum Option:
  None
  Some(a)

enum List:
  Empty
  NonEmpty(head, tail)

def head(list):
  match list:
    case Empty:
      None
    case NonEmpty(h, _):
      Some(h)

def tail(list):
  match list:
    case Empty: None
    case NonEmpty(_, t): Some(t)
""")

    val p6 = parse("""
package P6
from P5 import Option, List, NonEmpty, Empty, head,  tail
export data

data = NonEmpty(1, NonEmpty(2, Empty))

main = head(data)
""")
    valid(resolveThenInfer(List(p5, p6)))

    val p7 = parse("""
package P7
from P6 import data as p6_data
from P5 import Option, List, NonEmpty as Cons, Empty as Nil, head,  tail

data = Cons(1, Cons(2, Nil))
data1 = Cons(0, p6_data)

main = head(data1)
""")
    valid(resolveThenInfer(List(p5, p6, p7)))
  }

  test("test Predef working") {

    assert(Package.predefPackage != null)

    val p = parse("""
package UsePredef

def maybeOne(x):
  if x.eq_Int(1):
    Some(x)
  else:
    None

main = maybeOne(42)
""")
    valid(resolveThenInfer(PackageMap.withPredef(p :: Nil)))
  }

  test("test using a renamed type") {

    val p1 = parse("""
package R1

export Foo(), mkFoo, takeFoo

struct Foo

mkFoo = Foo
def takeFoo(foo):
  match foo:
    case Foo:
      0
""")

    val p2 = parse("""
package R2
from R1 import Foo as Bar, mkFoo, takeFoo

# note Bar is the same as foo
struct Baz(b: Bar)

baz = Baz(mkFoo)

main = takeFoo(mkFoo)

main2 = match baz:
  case Baz(fooAsBar):
    # here we pass a fooAsBar which has type Bar =:= Foo to takeFoo
    takeFoo(fooAsBar)
""")
    valid(resolveThenInfer(List(p1, p2)))
  }
}
