package dev.bosatsu

import cats.Show
import cats.data.{NonEmptyList, Validated, ValidatedNel}

import IorMethods.IorExtension

class PackageTest extends munit.FunSuite with ParTest {

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
      assertEquals(pmap.toMap(PackageName.parts("Foo2")).allImportPacks, List(
          PackageName.parts("Foo")
        ))
      assertEquals(pmap
          .toMap(PackageName.parts("Foo2"))
          .toIface
          .visibleDepPackages, List(PackageName.PredefName))
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
      assertEquals(pmap.toMap(PackageName.parts("P6")).allImportPacks, List(
          PackageName.parts("P5")
        ))
      assertEquals(pmap.toMap(PackageName.parts("P6")).toIface.visibleDepPackages, List(
          PackageName.PredefName,
          PackageName.parts("P5")
        ))
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

  // =========================================================================
  // numericPackage tests
  // =========================================================================

  test("numericPackage parses correctly") {
    val numeric = Package.numericPackage
    assertEquals(numeric.name.asString, "Bosatsu/Numeric")
    // Should have exports for Double type and operators
    assert(numeric.exports.nonEmpty)
    // Should have program statements
    assert(numeric.program.nonEmpty)
  }

  test("numericPackage has Double type") {
    val numeric = Package.numericPackage
    // Look for Double type export
    val hasDouble = numeric.exports.exists { exp =>
      exp.name.sourceCodeRepr == "Double"
    }
    assert(hasDouble, "numericPackage should export Double type")
  }

  // =========================================================================
  // ioPackage tests
  // =========================================================================

  test("ioPackage parses correctly") {
    val io = Package.ioPackage
    assertEquals(io.name.asString, "Bosatsu/IO")
    // Should have exports
    assert(io.exports.nonEmpty)
    // Should have program statements
    assert(io.program.nonEmpty)
  }

  test("ioPackage has IO type") {
    val io = Package.ioPackage
    // Look for IO type export
    val hasIO = io.exports.exists { exp =>
      exp.name.sourceCodeRepr == "IO"
    }
    assert(hasIO, "ioPackage should export IO type")
  }

  // =========================================================================
  // Package utility method tests
  // =========================================================================

  test("fromStatements creates a Package") {
    val pn = PackageName.parts("Test", "Package")
    val stmts = List.empty[Statement]
    val pkg = Package.fromStatements(pn, stmts)

    assertEquals(pkg.name, pn)
    assertEquals(pkg.imports, Nil)
    assertEquals(pkg.exports, Nil)
    assertEquals(pkg.program, stmts)
  }

  test("Package.withImport adds import") {
    val pn = PackageName.parts("Test")
    val pkg = Package.fromStatements(pn, Nil)
    val importPn = PackageName.parts("Other")
    val imp = Import(importPn, NonEmptyList.one(ImportedName.OriginalName(Identifier.unsafeBindable("foo"), ())))

    val pkgWithImport = pkg.withImport(imp)
    assertEquals(pkgWithImport.imports.length, 1)
    assertEquals(pkgWithImport.imports.head.pack, importPn)
  }

  test("Package.mapProgram transforms program") {
    val pn = PackageName.parts("Test")
    val pkg: Package[PackageName, Unit, Unit, List[Statement]] = Package(pn, Nil, Nil, Nil)
    val mapped = pkg.mapProgram(_ => 42)

    assertEquals(mapped.name, pn)
    assertEquals(mapped.program, 42)
  }

  test("Package.replaceImports replaces imports") {
    val pn = PackageName.parts("Test")
    val importPn = PackageName.parts("Orig")
    val imp = Import(importPn, NonEmptyList.one(ImportedName.OriginalName(Identifier.unsafeBindable("x"), ())))
    val pkg: Package[PackageName, Unit, Unit, List[Statement]] = Package(pn, List(imp), Nil, Nil)

    val newImportPn = PackageName.parts("New")
    val newImp = Import(newImportPn, NonEmptyList.one(ImportedName.OriginalName(Identifier.unsafeBindable("y"), "data")))
    val replaced = pkg.replaceImports(List(newImp))

    assertEquals(replaced.imports.length, 1)
    assertEquals(replaced.imports.head.pack, newImportPn)
  }

  test("Package equals works correctly") {
    val pn1 = PackageName.parts("Test")
    val pn2 = PackageName.parts("Other")
    val pkg1 = Package.fromStatements(pn1, Nil)
    val pkg2 = Package.fromStatements(pn1, Nil)
    val pkg3 = Package.fromStatements(pn2, Nil)

    assertEquals(pkg1, pkg2)
    assert(!pkg1.equals(pkg3))
    // Test equals with non-Package type
    assert(!pkg1.equals("not a package"))
    assert(!pkg1.equals(null))
  }

  test("Package hashCode is consistent") {
    val pn = PackageName.parts("Test")
    val pkg1 = Package.fromStatements(pn, Nil)
    val pkg2 = Package.fromStatements(pn, Nil)

    assertEquals(pkg1.hashCode, pkg2.hashCode)
  }

  test("Package.orderByName orders by name") {
    val order = Package.orderByName[PackageName, Unit, Unit, List[Statement]]
    val pkg1 = Package.fromStatements(PackageName.parts("Alpha"), Nil)
    val pkg2 = Package.fromStatements(PackageName.parts("Beta"), Nil)

    assert(order.compare(pkg1, pkg2) < 0)
    assert(order.compare(pkg2, pkg1) > 0)
    assertEquals(order.compare(pkg1, pkg1), 0)
  }

  // =========================================================================
  // Document instances tests
  // =========================================================================

  test("document for Parsed produces output") {
    val pkg = parse("""
package TestDoc
export main

main = 1
""")
    val doc = implicitly[org.typelevel.paiges.Document[Package.Parsed]].document(pkg)
    val rendered = doc.render(80)
    assert(rendered.contains("package TestDoc"))
    assert(rendered.contains("main"))
  }

  test("document handles empty imports and exports") {
    val pn = PackageName.parts("Empty")
    val pkg: Package.Parsed = Package(pn, Nil, Nil, Nil)
    val doc = implicitly[org.typelevel.paiges.Document[Package.Parsed]].document(pkg)
    val rendered = doc.render(80)
    assert(rendered.contains("package Empty"))
  }

  // =========================================================================
  // headerParser tests
  // =========================================================================

  test("headerParser with default package") {
    val defaultPn = PackageName.parts("Default")
    val parser = Package.headerParser(Some(defaultPn))

    // Without package declaration, should use default
    val result = parser.parse("export foo\n")
    assert(result.isRight)
    result.foreach { case (_, (pn, _, _)) =>
      assertEquals(pn, defaultPn)
    }
  }

  test("headerParser parses imports") {
    val parser = Package.headerParser(None)
    val input = """package Test
from Other import foo
export bar
"""
    val result = parser.parse(input)
    assert(result.isRight)
    result.foreach { case (_, (pn, imports, exports)) =>
      assertEquals(pn, PackageName.parts("Test"))
      assertEquals(imports.length, 1)
      assertEquals(exports.length, 1)
    }
  }
}
