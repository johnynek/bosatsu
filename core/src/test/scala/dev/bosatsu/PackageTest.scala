package dev.bosatsu

import cats.Show
import cats.data.{NonEmptyList, Validated, ValidatedNel}

import IorMethods.IorExtension

class PackageTest extends munit.FunSuite with ParTest {

  def resolveThenInfer(
      ps: Iterable[Package.Parsed],
      compileOptions: CompileOptions
  ): ValidatedNel[PackageError, PackageMap.Inferred] = {
    implicit val showInt: Show[Int] = Show.fromToString
    PackageMap
      .resolveThenInfer(ps.toList.zipWithIndex.map(_.swap), Nil, compileOptions)
      .strictToValidated
  }

  def resolveThenInfer(
      ps: Iterable[Package.Parsed]
  ): ValidatedNel[PackageError, PackageMap.Inferred] =
    resolveThenInfer(ps, CompileOptions.Default)

  def parse(s: String): Package.Parsed =
    Parser.unsafeParse(Package.parser(None), s)

  def parseUnit(ss: Iterable[String]) =
    resolveThenInfer(ss.map(parse(_)))

  def valid[A, B](v: Validated[A, B]) =
    v match {
      case Validated.Valid(_)     => ()
      case Validated.Invalid(err) => fail(err.toString)
    }

  def invalid[B](v: Validated[NonEmptyList[PackageError], B]) =
    v match {
      case Validated.Valid(err)   => fail(err.toString)
      case Validated.Invalid(err) =>
        err.toList.foreach(_.message(Map.empty, LocationMap.Colorize.None))
        ()
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
    valid(resolveThenInfer(List(p1, p2)).map { pmap =>
      assertEquals(
        pmap.toMap(PackageName.parts("Foo2")).allImportPacks,
        List(
          PackageName.parts("Foo")
        )
      )
      assertEquals(
        pmap
          .toMap(PackageName.parts("Foo2"))
          .toIface
          .visibleDepPackages,
        List(PackageName.PredefName)
      )
    })
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
from P5 import Option, List, NonEmpty, Empty, head
export data

data = NonEmpty(1, NonEmpty(2, Empty))

main = head(data)
""")
    valid(resolveThenInfer(List(p5, p6)).map { pmap =>
      assertEquals(
        pmap.toMap(PackageName.parts("P6")).allImportPacks,
        List(
          PackageName.parts("P5")
        )
      )
      assertEquals(
        pmap.toMap(PackageName.parts("P6")).toIface.visibleDepPackages,
        List(
          PackageName.PredefName,
          PackageName.parts("P5")
        )
      )
    })

    val p7 = parse("""
package P7
from P6 import data as p6_data
from P5 import Option, List, NonEmpty as Cons, Empty as Nil, head

export data

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
export main, main2

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

  test("unused imports is an error") {
    val p1 = parse("""
package Foo
export main

main = 1
""")
    val p2 = parse("""
package Foo2
from Foo import main as mainFoo
export main,

main = 2
""")

    invalid(resolveThenInfer(List(p1, p2)))

    val p3 = parse("""
package Foo
export main, Foo

enum Foo: Bar, Baz

main = 1
""")
    val p4 = parse("""
package Foo2
from Foo import main as mainFoo, Foo
export main,

main = 2
""")

    invalid(resolveThenInfer(List(p3, p4)))
  }

  test("type imports used in external declarations are counted as used") {
    val p1 = parse("""
package BytesPkg
export Bytes

external struct Bytes
""")

    val p2 = parse("""
package UsesExternal
from BytesPkg import Bytes
export now, wrap

external now: Bytes
external def wrap(b: Bytes) -> Bytes
""")

    valid(resolveThenInfer(List(p1, p2)))
  }

  test("default-backed record construction requires constructor export") {
    val typeOnly = parse("""
package P1
export Rec

struct Marker
struct Rec(a: Marker = Marker)
""")

    val consumer = parse("""
package P2
from P1 import Rec
export main

main = Rec {}
""")

    invalid(resolveThenInfer(List(typeOnly, consumer)))
  }

  test("default-backed record construction works when constructor is exported") {
    val provider = parse("""
package P1
export Rec()

struct Marker
struct Rec(a: Marker = Marker)
""")

    val consumer = parse("""
package P2
from P1 import Rec
export main

main = Rec {}
""")

    valid(resolveThenInfer(List(provider, consumer)))
  }

  test("record constructor still requires non-defaulted fields") {
    val pack = parse("""
package P1

struct Marker
struct Rec(a, b: Marker = Marker)

main = Rec {}
""")

    invalid(resolveThenInfer(List(pack)))
  }

  test("compile options can disable optimization passes") {
    val pack = parse("""
package Foo

helper = 1
main = helper
""")

    val optimized = resolveThenInfer(List(pack))
    val noOpt = resolveThenInfer(List(pack), CompileOptions.NoOptimize)

    def defsOf(
        inferred: ValidatedNel[PackageError, PackageMap.Inferred]
    ): List[String] =
      inferred match {
        case Validated.Invalid(errs) =>
          fail(errs.toString)
        case Validated.Valid(pmap)   =>
          pmap
            .toMap(PackageName.parts("Foo"))
            .program
            ._1
            .lets
            .map(_._1.asString)
      }

    assert(!defsOf(optimized).contains("helper"), defsOf(optimized).toString)
    assert(defsOf(noOpt).contains("helper"), defsOf(noOpt).toString)
  }
}
