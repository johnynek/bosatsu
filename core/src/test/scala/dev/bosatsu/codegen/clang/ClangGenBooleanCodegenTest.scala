package dev.bosatsu.codegen.clang

import cats.data.NonEmptyList
import dev.bosatsu.IorMethods.IorExtension
import dev.bosatsu.{
  CompileOptions,
  Identifier,
  LocationMap,
  Package,
  PackageMap,
  PackageName,
  Par,
  Parser
}

class ClangGenBooleanCodegenTest extends munit.FunSuite {
  private val pkg = PackageName.parts("Test", "BooleanTruthTable")

  private enum Expectation {
    case Const(value: Int)
    case Expr(expr: String)
  }

  private def boolLit(bit: Boolean): String = if (bit) "True" else "False"

  private def binaryDef(bits: Int): String = {
    val ff = boolLit((bits & 1) != 0)
    val ft = boolLit((bits & 2) != 0)
    val tf = boolLit((bits & 4) != 0)
    val tt = boolLit((bits & 8) != 0)

    s"""def b$bits(a: Bool, b: Bool) -> Bool:
       |  match a:
       |    case False:
       |      match b:
       |        case False: $ff
       |        case True: $ft
       |    case True:
       |      match b:
       |        case False: $tf
       |        case True: $tt
       |""".stripMargin
  }

  private def binaryTupleDef(bits: Int): String = {
    val ff = boolLit((bits & 1) != 0)
    val ft = boolLit((bits & 2) != 0)
    val tf = boolLit((bits & 4) != 0)
    val tt = boolLit((bits & 8) != 0)

    s"""def bm$bits(a: Bool, b: Bool) -> Bool:
       |  match (a, b):
       |    case (False, False): $ff
       |    case (False, True): $ft
       |    case (True, False): $tf
       |    case (True, True): $tt
       |""".stripMargin
  }

  private def unaryDef(bits: Int): String = {
    val f = boolLit((bits & 1) != 0)
    val t = boolLit((bits & 2) != 0)

    s"""def u$bits(a: Bool) -> Bool:
       |  match a:
       |    case False: $f
       |    case True: $t
       |""".stripMargin
  }

  private lazy val source: String = {
    val binary = (0 to 15).map(binaryDef)
    val binaryTuple = (0 to 15).map(binaryTupleDef)
    val unary = (0 to 3).map(unaryDef)
    val allNestedNames =
      ((0 to 15).map(i => s"b$i") ++ (0 to 3).map(i => s"u$i")).mkString(", ")
    val allTupleNames = (0 to 15).map(i => s"bm$i").mkString(", ")

    (s"""package Test/BooleanTruthTable
        |
        |${binary.mkString("\n\n")}
        |
        |${binaryTuple.mkString("\n\n")}
        |
        |${unary.mkString("\n\n")}
        |
        |all_nested = ($allNestedNames)
        |all_tuple = ($allTupleNames)
        |main = (all_nested, all_tuple)
        |""").stripMargin
  }

  private lazy val packageMap: PackageMap.Typed[Any] = {
    val pack = Parser.unsafeParse(Package.parser, source)
    val nel = NonEmptyList.one((("test", LocationMap(source)), pack))
    Par.noParallelism {
      PackageMap
        .typeCheckParsed(nel, Nil, "<predef>", CompileOptions.Default)
        .strictToValidated
        .fold(errs => fail(errs.toList.mkString("\n")), identity)
    }
  }

  private lazy val rendered: String =
    Par.withEC {
      ClangGen(packageMap).renderMain(
        pkg,
        Identifier.Name("main"),
        Code.Ident("run_main")
      )
    } match {
      case Left(err) =>
        fail(err.toString)
      case Right(doc) =>
        doc.render(120)
    }

  private def extractFunction(name: String): String = {
    val marker = s"BValue ___bsts_g_Test_l_BooleanTruthTable_l_${name}("
    val start = rendered.indexOf(marker)
    assert(start >= 0, s"missing function for $name")

    val braceStart = rendered.indexOf('{', start)
    assert(braceStart >= 0, s"missing open brace for $name")

    var idx = braceStart
    var depth = 0
    var done = false
    while ((idx < rendered.length) && !done) {
      rendered.charAt(idx) match {
        case '{' => depth += 1
        case '}' =>
          depth -= 1
          if (depth == 0) done = true
        case _ =>
      }
      idx += 1
    }

    assert(done, s"missing close brace for $name")
    rendered.substring(start, idx)
  }

  private val binaryExpectations: Map[Int, Expectation] = Map(
    0 -> Expectation.Const(0),
    1 -> Expectation.Expr(
      "(get_variant_value(__bsts_b_a0) != 1) && (get_variant_value(__bsts_b_b0) != 1)"
    ),
    2 -> Expectation.Expr(
      "(get_variant_value(__bsts_b_a0) != 1) && (get_variant_value(__bsts_b_b0) == 1)"
    ),
    3 -> Expectation.Expr(
      "get_variant_value(__bsts_b_a0) != 1"
    ),
    4 -> Expectation.Expr(
      "(get_variant_value(__bsts_b_a0) == 1) && (get_variant_value(__bsts_b_b0) != 1)"
    ),
    5 -> Expectation.Expr(
      "get_variant_value(__bsts_b_b0) != 1"
    ),
    6 -> Expectation.Expr(
      "(get_variant_value(__bsts_b_a0) == 1) != (get_variant_value(__bsts_b_b0) == 1)"
    ),
    7 -> Expectation.Expr(
      "(get_variant_value(__bsts_b_a0) != 1) || (get_variant_value(__bsts_b_b0) != 1)"
    ),
    8 -> Expectation.Expr(
      "(get_variant_value(__bsts_b_a0) == 1) && (get_variant_value(__bsts_b_b0) == 1)"
    ),
    9 -> Expectation.Expr(
      "(get_variant_value(__bsts_b_a0) == 1) == (get_variant_value(__bsts_b_b0) == 1)"
    ),
    10 -> Expectation.Expr(
      "get_variant_value(__bsts_b_b0) == 1"
    ),
    11 -> Expectation.Expr(
      "(get_variant_value(__bsts_b_a0) != 1) || (get_variant_value(__bsts_b_b0) == 1)"
    ),
    12 -> Expectation.Expr(
      "get_variant_value(__bsts_b_a0) == 1"
    ),
    13 -> Expectation.Expr(
      "(get_variant_value(__bsts_b_a0) == 1) || (get_variant_value(__bsts_b_b0) != 1)"
    ),
    14 -> Expectation.Expr(
      "(get_variant_value(__bsts_b_a0) == 1) || (get_variant_value(__bsts_b_b0) == 1)"
    ),
    15 -> Expectation.Const(1)
  )

  private val unaryExpectations: Map[Int, Expectation] = Map(
    0 -> Expectation.Const(0),
    1 -> Expectation.Expr("get_variant_value(__bsts_b_a0) != 1"),
    2 -> Expectation.Expr("get_variant_value(__bsts_b_a0) == 1"),
    3 -> Expectation.Const(1)
  )

  private def assertExpectation(
      fnName: String,
      expectation: Expectation,
      checkExactExpr: Boolean
  ): Unit = {
    val fn = extractFunction(fnName)
    val normalizedFn =
      fn
        .replace("== 0", "!= 1")
        .replace("!= 0", "== 1")
    expectation match {
      case Expectation.Const(value) =>
        assert(fn.contains(s"alloc_enum0($value)"), fn)
        assert(!fn.contains("get_variant_value(__bsts_b_a0)"), fn)
        assert(!fn.contains("get_variant_value(__bsts_b_b0)"), fn)
      case Expectation.Expr(expr) =>
        if (checkExactExpr) {
          val expected = s"return alloc_enum0($expr);"
          assert(normalizedFn.contains(expected), fn)
        } else {
          assert(fn.contains("return alloc_enum0("), fn)
        }
    }
    assert(!fn.contains("if ("), fn)
    assert(!fn.contains("?"), fn)
    assert(!fn.contains("alloc_struct"), fn)
    assert(!fn.contains("get_struct_index"), fn)
  }

  test("all 16 binary Boolean truth tables map to optimized C") {
    (0 to 15).foreach { bits =>
      assertExpectation(
        s"b$bits",
        binaryExpectations(bits),
        checkExactExpr = true
      )
      assertExpectation(
        s"bm$bits",
        binaryExpectations(bits),
        checkExactExpr = false
      )
    }
  }

  test("all 4 unary Boolean truth tables map to optimized C") {
    (0 to 3).foreach { bits =>
      assertExpectation(
        s"u$bits",
        unaryExpectations(bits),
        checkExactExpr = true
      )
    }
  }
}
