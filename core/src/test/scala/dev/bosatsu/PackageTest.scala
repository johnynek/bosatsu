package dev.bosatsu

import cats.{Id, Show}
import cats.data.{NonEmptyList, Validated, ValidatedNel}

import IorMethods.IorExtension

class PackageTest extends munit.FunSuite with ParTest {

  def resolveThenInfer(
      ps: Iterable[Package.Parsed],
      compileOptions: CompileOptions
  ): ValidatedNel[PackageError, PackageMap.Compiled] = {
    implicit val showInt: Show[Int] = Show.fromToString
    val packs =
      ps.toList
    val withPredef =
      if (packs.exists(_.name == PackageName.PredefName)) packs
      else PackageMap.withPredef(packs, compileOptions.mode)
    PackageMap
      .resolveThenInfer(
        withPredef.zipWithIndex.map(_.swap),
        Nil,
        compileOptions
      )
      .strictToValidated
  }

  def resolveThenInfer(
      ps: Iterable[Package.Parsed]
  ): ValidatedNel[PackageError, PackageMap.Compiled] =
    resolveThenInfer(ps, CompileOptions.Default)

  def parse(s: String): Package.Parsed =
    Parser.unsafeParse(Package.parser, s)

  def parseUnit(ss: Iterable[String]) =
    resolveThenInfer(ss.map(parse(_)))

  def typeCheckParsed(
      ps: List[Package.Parsed],
      compileOptions: CompileOptions
  ): ValidatedNel[PackageError, PackageMap.Compiled] = {
    val withPaths = ps.zipWithIndex.map { case (pack, idx) =>
      ((idx.toString, LocationMap("")), pack)
    }
    PackageMap
      .typeCheckParsed(
        NonEmptyList.fromList(withPaths).get,
        Nil,
        "predef",
        compileOptions
      )
      .strictToValidated
  }

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
          PackageName.PredefName,
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
exposes P5

data = NonEmpty(1, NonEmpty(2, Empty))

main = head(data)
""")
    valid(resolveThenInfer(List(p5, p6)).map { pmap =>
      assertEquals(
        pmap.toMap(PackageName.parts("P6")).allImportPacks,
        List(
          PackageName.PredefName,
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
exposes P5

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
exposes BytesPkg

external now: Bytes
external def wrap(b: Bytes) -> Bytes
""")

    valid(resolveThenInfer(List(p1, p2)))
  }

  test("type-only packages can use imported types in local type declarations") {
    val base = parse("""
package Base
export Foo()

struct Foo
""")

    val typeOnly = parse("""
package TypeOnly
from Base import Foo
export Bar()
exposes Base

struct Bar(value: Foo)
""")

    val valueOnly = parse("""
package ValueOnly
from TypeOnly import Bar
export main
exposes TypeOnly

def main(value: Bar):
  value
""")

    valid(resolveThenInfer(List(base, typeOnly, valueOnly)).map { pmap =>
      assertEquals(
        pmap.toMap(PackageName.parts("TypeOnly")).toIface.visibleDepPackages,
        List(PackageName.parts("Base"), PackageName.parts("TypeOnly"))
      )
      assertEquals(
        pmap.toMap(PackageName.parts("ValueOnly")).toIface.visibleDepPackages,
        List(PackageName.PredefName, PackageName.parts("TypeOnly"))
      )
    })
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
exposes P1

main = Rec {}
""")

    invalid(resolveThenInfer(List(typeOnly, consumer)))
  }

  test(
    "default-backed record construction works when constructor is exported"
  ) {
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
exposes P1

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
        inferred: ValidatedNel[PackageError, PackageMap.Compiled]
    ): List[String] =
      inferred match {
        case Validated.Invalid(errs) =>
          fail(errs.toString)
        case Validated.Valid(pmap) =>
          pmap
            .toMap(PackageName.parts("Foo"))
            .program
            ._1
            .lets
            .map(_._1.sourceCodeRepr)
      }

    assert(!defsOf(optimized).contains("helper"), defsOf(optimized).toString)
    assert(defsOf(noOpt).contains("helper"), defsOf(noOpt).toString)
  }

  test("type-check mode includes todo from internal predef") {
    val pack = parse("""
package Foo

main = todo(42)
""")

    valid(typeCheckParsed(pack :: Nil, CompileOptions.TypeCheckOnly))
    invalid(typeCheckParsed(pack :: Nil, CompileOptions.Default))
  }

  test("internal predef exports todo only in type-check mode") {
    val emitExports =
      PackageMap
        .predefCompiledForMode(CompileOptions.Mode.Emit)
        .exports
        .map(_.name.sourceCodeRepr)
    val typeCheckExports =
      PackageMap
        .predefCompiledForMode(CompileOptions.Mode.TypeCheckOnly)
        .exports
        .map(_.name.sourceCodeRepr)

    assert(!emitExports.contains("todo"), emitExports.toString)
    assert(typeCheckExports.contains("todo"), typeCheckExports.toString)
  }

  test("effective predef sources respect explicit Predef interfaces") {
    val pack = parse("""
package Foo

main = 1
""")
    val predefIface =
      Package.interfaceOf(
        PackageMap.predefCompiledForMode(CompileOptions.Mode.Emit)
      )

    val source =
      PackageMap.SourceUnit.fromParsedWithoutLocation[Id, Int]((0, pack))
    val withoutIface =
      PackageMap.effectivePredefSources(
        NonEmptyList.one(source),
        Nil,
        -1,
        CompileOptions.Mode.TypeCheckOnly
      )
    val withIface =
      PackageMap.effectivePredefSources(
        NonEmptyList.one(source),
        predefIface :: Nil,
        -1,
        CompileOptions.Mode.TypeCheckOnly
      )

    assert(withoutIface.usesInternalPredefSource)
    assertEquals(
      withoutIface.sourceUnits.count(_.packageName == PackageName.PredefName),
      1
    )
    assert(!withIface.usesInternalPredefSource)
    assertEquals(
      withIface.sourceUnits.count(_.packageName == PackageName.PredefName),
      0
    )
  }

  test("exported type aliases can be imported transparently") {
    val exported = parse("""
package Alias/Export
export Box, Foo, mkFoo

struct Box(item)

type Foo = Box[Int]

mkFoo: Foo = Box(1)
""")

    val imported = parse("""
package Alias/Import
from Alias/Export import Foo, mkFoo
export main
exposes Alias/Export

main: Foo = mkFoo
""")

    valid(resolveThenInfer(PackageMap.withPredef(List(exported, imported))))
  }

  test("exported type aliases may not reference private local types") {
    invalid(resolveThenInfer(PackageMap.withPredef(List(parse("""
package Alias/Export
export Foo, mkFoo

struct Box(item)

type Foo = Box[Int]

mkFoo: Foo = Box(1)
""")))))
  }

  test("exported values may not reference private higher-kinded aliases") {
    invalid(resolveThenInfer(PackageMap.withPredef(List(parse("""
package Alias/HiddenHK
export Either, Monad, main

enum Either[a, b]:
  Left(left: a), Right(right: b)

type EitherFlip[b, a] = Either[a, b]

struct Monad(pure: forall a. a -> f[a], bind: forall a, b. (f[a], a -> f[b]) -> f[b])

def either_flip_bind(value, bind_fn):
  match value:
    case Left(a): bind_fn(a)
    case Right(b): Right(b)

main: forall b. Monad[EitherFlip[b]] = Monad(Left, either_flip_bind)
""")))))
  }

  test("alias and type name collisions are rejected") {
    invalid(resolveThenInfer(PackageMap.withPredef(List(parse("""
package Alias/Collision

type Foo = Int
struct Foo

main = 1
""")))))
  }

  test("type aliases may only reference prior local type definitions") {
    invalid(resolveThenInfer(PackageMap.withPredef(List(parse("""
package Alias/Forward

type Foo = Bar
type Bar = Int

main = 1
""")))))

    invalid(resolveThenInfer(PackageMap.withPredef(List(parse("""
package Alias/Self

type Foo = Foo

main = 1
""")))))
  }

  test("exposed dependency normalization follows the exported API") {
    val dep = parse("""
package Dep/Types
export Dep(), Alias, ext

struct Dep
type Alias = Dep
external ext: Dep
""")

    val opaque = parse("""
package Main/Opaque
from Dep/Types import Dep
export Wrapped

struct Wrapped(value: Dep)
""")

    val constructors = parse("""
package Main/Open
from Dep/Types import Dep
export Wrapped()
exposes Dep/Types

struct Wrapped(value: Dep)
""")

    val alias = parse("""
package Main/Alias
from Dep/Types import Alias
export AliasBox
exposes Dep/Types

type AliasBox = Alias
""")

    val inferred = parse("""
package Main/Inferred
from Dep/Types import ext
export exposed
exposes Dep/Types

exposed = ext
""")

    val external = parse("""
package Main/External
from Dep/Types import Dep
export now
exposes Dep/Types

external now: Dep
""")

    val sameLibraryBase = parse("""
package Same/Base
export Shared()

struct Shared
""")

    val sameLibraryUser = parse("""
package Same/User
from Same/Base import Shared
export wrap
exposes Same/Base

def wrap(value: Shared) -> Shared:
  value
""")

    valid(
      resolveThenInfer(
        List(
          dep,
          opaque,
          constructors,
          alias,
          inferred,
          external,
          sameLibraryBase,
          sameLibraryUser
        )
      ).map { pmap =>
        assertEquals(
          pmap.toMap(PackageName.parts("Dep", "Types")).exposedDepPackages,
          Nil
        )
        assertEquals(
          pmap.toMap(PackageName.parts("Main", "Opaque")).exposedDepPackages,
          Nil
        )
        assertEquals(
          pmap.toMap(PackageName.parts("Main", "Open")).exposedDepPackages,
          List(PackageName.parts("Dep", "Types"))
        )
        assertEquals(
          pmap.toMap(PackageName.parts("Main", "Alias")).exposedDepPackages,
          List(PackageName.parts("Dep", "Types"))
        )
        assertEquals(
          pmap.toMap(PackageName.parts("Main", "Inferred")).exposedDepPackages,
          List(PackageName.parts("Dep", "Types"))
        )
        assertEquals(
          pmap.toMap(PackageName.parts("Main", "External")).exposedDepPackages,
          List(PackageName.parts("Dep", "Types"))
        )
        assertEquals(
          pmap.toMap(PackageName.parts("Same", "User")).exposedDepPackages,
          List(PackageName.parts("Same", "Base"))
        )
        assertEquals(PackageMap.predefCompiled.exposedDepPackages, Nil)
      }
    )
  }
}
