package dev.bosatsu

import dev.bosatsu.Identifier.{Constructor, Name}
import dev.bosatsu.codegen.clang.{Code, RuntimeIdent}
import scala.compiletime.testing.{typeCheckErrors, typeChecks}

class CheckedNamesTest extends munit.FunSuite {

  private val predefSource =
    "core/src/main/resources/bosatsu/predef.bosatsu"

  test("a Predef value compiles only if Predef exports it") {
    assertEquals(PredefName.value("cmp_Int"), Name("cmp_Int"))
    assert(typeChecks("""PredefName.value("cmp_Int")"""))
    assertEquals(
      typeCheckErrors("""PredefName.value("cmp_Itn")""").map(_.message),
      List(
        s"`cmp_Itn` is not one of the exported values of Bosatsu/Predef in $predefSource. Did you mean `cmp_Int`?"
      )
    )
  }

  test("a Predef constructor compiles only if its type is exported with ()") {
    assertEquals(PredefName.constructor("True"), Constructor("True"))
    assertEquals(PredefNames.NonEmptyList, Constructor("NonEmptyList"))
    assertEquals(
      typeCheckErrors("""PredefName.constructor("Ture")""").map(_.message),
      List(
        s"`Ture` is not one of the constructors of the types exported with () of Bosatsu/Predef in $predefSource. Did you mean `True`?"
      )
    )
    // a value is not a constructor
    assertEquals(
      typeCheckErrors("""PredefName.constructor("cmp_Int")""").length,
      1
    )
  }

  test("a Predef type compiles only if Predef exports it") {
    assertEquals(PredefName.typeName("Bool"), TypeName("Bool"))
    // Char is exported without its constructors
    assert(typeChecks("""PredefName.typeName("Char")"""))
    assertEquals(
      typeCheckErrors("""PredefName.typeName("Boolean")""").map(_.message),
      List(
        s"`Boolean` is not one of the exported types of Bosatsu/Predef in $predefSource."
      )
    )
  }

  test("an external compiles only if its package declares it external") {
    assertEquals(
      PredefName.external("add"),
      (PackageName.PredefName, Name("add"))
    )
    assertEquals(
      ReservedPackage.Prog.external("pure"),
      (PackageName.parts("Bosatsu", "Prog"), Name("pure"))
    )
    // `external empty_Bytes: Bytes` declares an external value
    assert(typeChecks("""ReservedPackage.IoBytes.external("empty_Bytes")"""))
    assertEquals(
      typeCheckErrors("""ReservedPackage.Prog.external("puer")""")
        .map(_.message),
      List(
        "`puer` is not one of the external declarations of Bosatsu/Prog in test_workspace/Prog.bosatsu. Did you mean `pure`?"
      )
    )
    // map is defined in Bosatsu, not external
    assertEquals(
      typeCheckErrors("""ReservedPackage.Prog.external("map")""").length,
      1
    )
    // an external of another package
    assertEquals(
      typeCheckErrors("""PredefName.external("pure")""").length,
      1
    )
  }

  test("the reserved package names match their sources") {
    assertEquals(
      ReservedPackage.Prog.name,
      PackageName.parts("Bosatsu", "Prog")
    )
    assertEquals(
      ReservedPackage.Array.name,
      PackageName.parts("Bosatsu", "Collection", "Array")
    )
    assertEquals(
      typeCheckErrors(
        """CheckedNames.packageName("test_workspace/Prog.bosatsu", "Bosatsu/Prgo")"""
      ).map(_.message),
      List(
        "`Bosatsu/Prgo` is not the package of test_workspace/Prog.bosatsu, which is `Bosatsu/Prog`"
      )
    )
  }

  test("a C runtime identifier compiles only if a header names it") {
    assertEquals(RuntimeIdent("bsts_integer_cmp"), Code.Ident("bsts_integer_cmp"))
    assert(typeChecks("""RuntimeIdent("BSTS_NAT_0")"""))
    assertEquals(
      typeCheckErrors("""RuntimeIdent("bsts_integer_cpm")""").map(_.message),
      List(
        "`bsts_integer_cpm` is not one of the identifiers in the c_runtime/*.h headers. Did you mean `bsts_integer_cmp`?"
      )
    )
  }

  test("a check needs string literals") {
    assertEquals(
      typeCheckErrors(
        """{ val n = "cmp_Int"; PredefName.value(n) }"""
      ).length,
      1
    )
  }

  test("scanning the Predef source finds its names") {
    val names = BosatsuSourceNames.scan(predefSource, Predef.predefString) match {
      case Right(names) => names
      case Left(err)    => fail(err)
    }
    assertEquals(names.packageName, "Bosatsu/Predef")
    assert(names.exportedValues.contains("cmp_Int"), names.exportedValues)
    assert(!names.exportedValues.contains("Bool"), names.exportedValues)
    assert(names.exportedTypes.contains("Char"), names.exportedTypes)
    assert(names.exportedTypes.contains("Bool"), names.exportedTypes)
    assertEquals(
      names.constructors.take(2),
      List("False", "True")
    )
    assert(names.constructors.contains("Tuple32"), names.constructors)
    assert(!names.constructors.contains("Int"), names.constructors)
    assert(names.externals.contains("cmp_Int"), names.externals)
    // a Bosatsu def, not an external
    assert(!names.externals.contains("reverse"), names.externals)
  }

  test("scanning a source reads each layout the reserved packages use") {
    val src =
      """package Foo/Bar
        |
        |export Baz(), Qux, quux, operator +, # a comment
        |
        |# a comment that names enum Nope
        |enum Baz[a: +*]: One(a: a, b: (a, a)), Two
        |  Three(
        |    x: List[a]
        |  )
        |  Four
        |
        |external quux: Int
        |external def corge(x: Int) -> Int
        |external type Qux
        |""".stripMargin
    assertEquals(
      BosatsuSourceNames.scan("foo.bosatsu", src),
      Right(
        BosatsuSourceNames(
          "Foo/Bar",
          List("quux"),
          List("Baz", "Qux"),
          List("One", "Two", "Three", "Four"),
          List("quux", "corge")
        )
      )
    )
  }

  test("scanning a source it can't read is an error") {
    def err(src: String): String =
      BosatsuSourceNames.scan("x.bosatsu", src).swap.getOrElse(fail(src))

    assertEquals(err("export (a)\n"), "x.bosatsu has no `package` line")
    assertEquals(err("package A\n"), "x.bosatsu has no export list")
    assertEquals(
      err("package A\nexport (a,\n"),
      "x.bosatsu: the export list on line 2 has no closing `)`"
    )
    assertEquals(
      err("package A\nexport a.b\n"),
      "x.bosatsu: can't read the export list entry `a.b`"
    )
    assertEquals(
      err("package A\nexport B()\n"),
      "x.bosatsu exports B() but declares no top-level enum or struct B"
    )
    assertEquals(
      err("package A\nexport B()\nenum B\n"),
      "x.bosatsu: enum B on line 3 has no `:`"
    )
    assertEquals(
      err("package A\nexport B()\nenum B:\n"),
      "x.bosatsu: enum B on line 3 has no variants"
    )
    assertEquals(
      err("package A\nexport B()\nenum B: C, d\n"),
      "x.bosatsu: can't read the variant `d` of enum B on line 3"
    )
  }

  test("scanning C finds identifiers outside comments and literals") {
    val src =
      """#include "not_an_ident.h"
        |// line_comment
        |/* block
        |   comment */
        |BValue bsts_f(int x) { return x + 0x1fULL + 'q' + "s\"tr"; }
        |""".stripMargin
    assertEquals(
      CRuntimeNames.identifiers(src),
      Set("include", "BValue", "bsts_f", "int", "x", "return")
    )
  }
}
