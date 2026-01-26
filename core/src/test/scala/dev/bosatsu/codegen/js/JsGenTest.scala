package dev.bosatsu.codegen.js

import munit.ScalaCheckSuite
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll
import dev.bosatsu.{Identifier, Lit, Matchless}
import dev.bosatsu.Identifier.Name
import cats.data.NonEmptyList

class JsGenTest extends ScalaCheckSuite {

  import Matchless._

  // ==================
  // Helper Functions
  // ==================

  def renderAndCheck(expr: Matchless.Expr[Unit], check: String => Boolean): Boolean = {
    val result = JsGen.renderExpr(expr)
    check(result)
  }

  def assertRenders(expr: Matchless.Expr[Unit], expected: String)(implicit loc: munit.Location): Unit = {
    val result = JsGen.renderExpr(expr)
    assertEquals(result, expected)
  }

  def bindable(name: String): Identifier.Bindable = Name(name)

  // ==================
  // Literal Tests
  // ==================

  test("Literal integer renders correctly") {
    assertRenders(Literal(Lit.Integer(42)), "42")
    assertRenders(Literal(Lit.Integer(-5)), "(-5)")
    assertRenders(Literal(Lit.Integer(0)), "0")
  }

  test("Literal string renders correctly") {
    assertRenders(Literal(Lit.Str("hello")), "\"hello\"")
    assertRenders(Literal(Lit.Str("")), "\"\"")
  }

  test("Literal char renders correctly") {
    assertRenders(Literal(Lit.Chr("a")), "\"a\"")
    assertRenders(Literal(Lit.Chr("\n")), "\"\\n\"")
  }

  // ==================
  // Variable Tests
  // ==================

  test("Local variable renders correctly") {
    assertRenders(Local(bindable("x")), "x")
    assertRenders(Local(bindable("myVar")), "myVar")
  }

  test("Reserved word escaping works") {
    // "class" is a reserved word
    val ident = JsGen.escape(bindable("class"))
    assertEquals(ident.name, "_class")
  }

  test("Operator escaping works") {
    val op = Identifier.Operator("+")
    val ident = JsGen.escape(op)
    assert(ident.name.startsWith("op_"))
  }

  // ==================
  // Enum/Struct Tests
  // ==================

  test("MakeEnum with zero arity renders as array") {
    assertRenders(MakeEnum(0, 0, List(0)), "[0]")
    assertRenders(MakeEnum(1, 0, List(0, 0)), "[1]")
  }

  test("MakeEnum with arity renders as function") {
    val result = JsGen.renderExpr(MakeEnum(1, 2, List(0, 2)))
    assert(result.contains("=>"), s"Expected arrow function, got: $result")
    assert(result.contains("[1,"), s"Expected array with variant tag, got: $result")
  }

  test("MakeStruct with zero arity renders as empty array") {
    assertRenders(MakeStruct(0), "[]")
  }

  test("MakeStruct with arity renders as function") {
    val result = JsGen.renderExpr(MakeStruct(2))
    assert(result.contains("=>"), s"Expected arrow function, got: $result")
  }

  // ==================
  // Nat Tests
  // ==================

  test("ZeroNat renders as 0") {
    assertRenders(ZeroNat, "0")
  }

  test("SuccNat renders as increment function") {
    val result = JsGen.renderExpr(SuccNat)
    assert(result.contains("=>"), s"Expected arrow function, got: $result")
    assert(result.contains("n + 1") || result.contains("n+1"), s"Expected increment, got: $result")
  }

  test("PrevNat renders as decrement") {
    val result = JsGen.renderExpr(PrevNat(Local(bindable("n"))))
    assert(result.contains("n - 1") || result.contains("n-1"), s"Expected decrement, got: $result")
  }

  // ==================
  // Application Tests
  // ==================

  test("Simple function application renders") {
    val app = App(Local(bindable("f")), NonEmptyList.of(Literal(Lit.Integer(1))))
    assertRenders(app, "f(1)")
  }

  test("Multiple argument application renders") {
    val app = App(
      Local(bindable("add")),
      NonEmptyList.of(Literal(Lit.Integer(1)), Literal(Lit.Integer(2)))
    )
    assertRenders(app, "add(1, 2)")
  }

  // ==================
  // Lambda Tests
  // ==================

  test("Simple lambda renders") {
    val lam = Lambda(
      Nil,
      None,
      NonEmptyList.of(bindable("x")),
      Local(bindable("x"))
    )
    val result = JsGen.renderExpr(lam)
    assert(result.contains("=>"), s"Expected arrow function, got: $result")
  }

  test("Multi-arg lambda renders") {
    val lam = Lambda(
      Nil,
      None,
      NonEmptyList.of(bindable("x"), bindable("y")),
      App(Local(bindable("add")), NonEmptyList.of(Local(bindable("x")), Local(bindable("y"))))
    )
    val result = JsGen.renderExpr(lam)
    assert(result.contains("=>"), s"Expected arrow function, got: $result")
    assert(result.contains("add(x, y)"), s"Expected application in body, got: $result")
  }

  // ==================
  // If Tests
  // ==================

  test("If expression renders as ternary") {
    val ifExpr = If(
      TrueConst,
      Literal(Lit.Integer(1)),
      Literal(Lit.Integer(2))
    )
    val result = JsGen.renderExpr(ifExpr)
    assert(result.contains("?") && result.contains(":"), s"Expected ternary, got: $result")
  }

  // ==================
  // Let Tests
  // ==================

  test("Let binding renders with IIFE") {
    val letExpr = Let(
      Right(bindable("x")),
      Literal(Lit.Integer(42)),
      Local(bindable("x"))
    )
    val result = JsGen.renderExpr(letExpr)
    assert(result.contains("const x"), s"Expected const binding, got: $result")
    assert(result.contains("return"), s"Expected return, got: $result")
  }

  // ==================
  // GetElement Tests
  // ==================

  test("GetEnumElement renders as array access") {
    val get = GetEnumElement(Local(bindable("e")), 0, 1, 2)
    val result = JsGen.renderExpr(get)
    assert(result.contains("[2]"), s"Expected index 2 (1+1), got: $result")
  }

  test("GetStructElement renders as array access") {
    val get = GetStructElement(Local(bindable("s")), 0, 2)
    val result = JsGen.renderExpr(get)
    assert(result.contains("[0]"), s"Expected index 0, got: $result")
  }

  // ==================
  // Boolean Expression Tests
  // ==================

  test("TrueConst renders as true") {
    val ifExpr = If(TrueConst, Literal(Lit.Integer(1)), Literal(Lit.Integer(2)))
    val result = JsGen.renderExpr(ifExpr)
    assert(result.contains("true"), s"Expected true, got: $result")
  }

  test("And expression renders") {
    val andExpr = And(TrueConst, TrueConst)
    val ifExpr = If(andExpr, Literal(Lit.Integer(1)), Literal(Lit.Integer(2)))
    val result = JsGen.renderExpr(ifExpr)
    assert(result.contains("&&"), s"Expected &&, got: $result")
  }

  test("CheckVariant renders as array index comparison") {
    val check = CheckVariant(Local(bindable("x")), 1, 0, List(0, 0))
    val ifExpr = If(check, Literal(Lit.Integer(1)), Literal(Lit.Integer(2)))
    val result = JsGen.renderExpr(ifExpr)
    assert(result.contains("[0]"), s"Expected index access, got: $result")
    assert(result.contains("=== 1") || result.contains("===1"), s"Expected comparison with 1, got: $result")
  }

  // ==================
  // Module Rendering Tests
  // ==================

  test("renderModule produces valid code") {
    val bindings = List(
      (bindable("x"), Literal(Lit.Integer(42))),
      (bindable("y"), Literal(Lit.Str("hello")))
    )
    val result = JsGen.renderModule(bindings)
    assert(result.contains("const x = 42"), s"Expected x binding, got: $result")
    assert(result.contains("const y = \"hello\""), s"Expected y binding, got: $result")
  }

  // ==================
  // Property Tests
  // ==================

  val genSimpleLit: Gen[Lit] = Gen.oneOf(
    Gen.choose(-1000L, 1000L).map(Lit.Integer(_)),
    Gen.alphaNumStr.map(Lit.Str(_)),
    Gen.alphaChar.map(c => Lit.Chr(c.toString))
  )

  property("literal rendering produces non-empty output") {
    forAll(genSimpleLit) { lit =>
      val result = JsGen.renderExpr(Literal(lit))
      result.nonEmpty
    }
  }

  property("variable names don't contain JS reserved words as standalone") {
    forAll(Gen.alphaStr.filter(_.nonEmpty)) { name =>
      val ident = JsGen.escape(Name(name))
      !JsGen.jsReservedWords.contains(ident.name)
    }
  }

  property("escaped names are valid JS identifiers") {
    forAll(Gen.alphaNumStr.filter(_.nonEmpty)) { name =>
      val ident = JsGen.escape(Name(name))
      // Check first char is letter or underscore
      val first = ident.name.head
      (first.isLetter || first == '_' || first == '$') &&
        ident.name.forall(c => c.isLetterOrDigit || c == '_' || c == '$')
    }
  }
}
