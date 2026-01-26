package dev.bosatsu.codegen.js

import munit.FunSuite
import dev.bosatsu.{Identifier, Lit, Matchless, PackageName}
import dev.bosatsu.Identifier.Name
import cats.data.NonEmptyList

/**
 * Integration tests for JS transpilation.
 * Tests the full pipeline from Matchless expressions to runnable JavaScript.
 */
class JsTranspilerTest extends FunSuite {

  import Matchless._

  def bindable(name: String): Identifier.Bindable = Name(name)

  // ==================
  // Full Module Tests
  // ==================

  test("full module with multiple bindings generates valid JS") {
    val bindings: List[(Identifier.Bindable, Expr[Unit])] = List(
      (bindable("x"), Literal(Lit.Integer(42))),
      (bindable("y"), Literal(Lit.Str("hello"))),
      (bindable("add"), Lambda(
        Nil,
        None,
        NonEmptyList.of(bindable("a"), bindable("b")),
        Local(bindable("a"))  // simplified - just returns a
      ))
    )

    val result = JsGen.renderModule(bindings)

    // Should produce valid const declarations
    assert(result.contains("const x = 42"), s"Expected x binding, got: $result")
    assert(result.contains("const y = \"hello\""), s"Expected y binding, got: $result")
    assert(result.contains("const add ="), s"Expected add binding, got: $result")
    assert(result.contains("=>"), s"Expected arrow function, got: $result")
  }

  test("nested let expressions generate IIFEs") {
    val expr: Expr[Unit] = Let(
      Right(bindable("x")),
      Literal(Lit.Integer(1)),
      Let(
        Right(bindable("y")),
        Literal(Lit.Integer(2)),
        Local(bindable("x"))
      )
    )

    val result = JsGen.renderExpr(expr)

    // Should have nested IIFEs or proper scoping
    assert(result.contains("const x"), s"Expected x binding, got: $result")
    assert(result.contains("const y"), s"Expected y binding, got: $result")
  }

  test("conditional expressions generate ternary operators") {
    val expr: Expr[Unit] = If(
      TrueConst,
      Literal(Lit.Integer(1)),
      Literal(Lit.Integer(2))
    )

    val result = JsGen.renderExpr(expr)
    assert(result.contains("?"), s"Expected ternary operator, got: $result")
    assert(result.contains(":"), s"Expected colon in ternary, got: $result")
  }

  test("function application with multiple args") {
    val expr: Expr[Unit] = App(
      Lambda(
        Nil,
        None,
        NonEmptyList.of(bindable("x"), bindable("y"), bindable("z")),
        Local(bindable("x"))
      ),
      NonEmptyList.of(
        Literal(Lit.Integer(1)),
        Literal(Lit.Integer(2)),
        Literal(Lit.Integer(3))
      )
    )

    val result = JsGen.renderExpr(expr)
    assert(result.contains("1, 2, 3") || result.contains("1,2,3"),
      s"Expected function call with three args, got: $result")
  }

  // ==================
  // Enum/ADT Tests
  // ==================

  test("enum construction produces arrays") {
    // MakeEnum(variant=1, arity=2, familyArities=List(0, 2))
    // This represents a variant with 2 fields
    val expr: Expr[Unit] = MakeEnum(1, 2, List(0, 2))

    val result = JsGen.renderExpr(expr)
    // Should produce an arrow function that creates [1, arg0, arg1]
    assert(result.contains("=>"), s"Expected arrow function for enum constructor, got: $result")
    assert(result.contains("[1,"), s"Expected array with variant tag, got: $result")
  }

  test("struct construction produces arrays") {
    val expr: Expr[Unit] = MakeStruct(3)

    val result = JsGen.renderExpr(expr)
    // Should produce an arrow function that creates [arg0, arg1, arg2]
    assert(result.contains("=>"), s"Expected arrow function for struct, got: $result")
  }

  test("variant checking generates array access") {
    val expr: BoolExpr[Unit] = CheckVariant(Local(bindable("x")), 1, 0, List(0, 1))

    // Wrap in an If to test
    val ifExpr: Expr[Unit] = If(expr, Literal(Lit.Integer(1)), Literal(Lit.Integer(0)))
    val result = JsGen.renderExpr(ifExpr)

    assert(result.contains("[0]"), s"Expected variant tag check at index 0, got: $result")
    assert(result.contains("=== 1") || result.contains("===1"),
      s"Expected comparison with variant 1, got: $result")
  }

  // ==================
  // Natural Number Tests
  // ==================

  test("Nat operations generate correct arithmetic") {
    val zero: Expr[Unit] = ZeroNat
    val succ: Expr[Unit] = SuccNat
    val prev: Expr[Unit] = PrevNat(Local(bindable("n")))

    val zeroResult = JsGen.renderExpr(zero)
    val succResult = JsGen.renderExpr(succ)
    val prevResult = JsGen.renderExpr(prev)

    assertEquals(zeroResult, "0")
    assert(succResult.contains("+ 1") || succResult.contains("+1"),
      s"Expected increment, got: $succResult")
    assert(prevResult.contains("- 1") || prevResult.contains("-1"),
      s"Expected decrement, got: $prevResult")
  }

  // ==================
  // String Escaping Tests
  // ==================

  test("string literals with special characters are escaped") {
    val expr: Expr[Unit] = Literal(Lit.Str("hello\nworld\ttab\"quote"))

    val result = JsGen.renderExpr(expr)
    assert(result.contains("\\n"), s"Expected escaped newline, got: $result")
    assert(result.contains("\\t"), s"Expected escaped tab, got: $result")
    assert(result.contains("\\\""), s"Expected escaped quote, got: $result")
  }

  // ==================
  // Always (Conditional) Tests
  // ==================

  test("Always generates conditional evaluation") {
    // Always(cond, expr) - evaluates expr only if cond is true
    val expr: Expr[Unit] = Always(TrueConst, Literal(Lit.Integer(42)))

    val result = JsGen.renderExpr(expr)
    // Should generate some form of conditional or direct value
    assert(result.nonEmpty, s"Expected non-empty result, got: $result")
  }

  // ==================
  // Complex Expression Tests
  // ==================

  test("complex nested expression compiles") {
    // let x = 1 in
    // let y = (if true then 2 else 3) in
    // f(x, y)
    val expr: Expr[Unit] = Let(
      Right(bindable("x")),
      Literal(Lit.Integer(1)),
      Let(
        Right(bindable("y")),
        If(TrueConst, Literal(Lit.Integer(2)), Literal(Lit.Integer(3))),
        App(
          Local(bindable("f")),
          NonEmptyList.of(Local(bindable("x")), Local(bindable("y")))
        )
      )
    )

    val result = JsGen.renderExpr(expr)

    assert(result.contains("const x"), s"Expected x binding, got: $result")
    assert(result.contains("const y"), s"Expected y binding, got: $result")
    assert(result.contains("f(x, y)") || result.contains("f(x,y)"),
      s"Expected function call, got: $result")
    assert(result.contains("?"), s"Expected ternary for if, got: $result")
  }

  // ==================
  // Module Rendering Tests
  // ==================

  test("empty module renders empty string") {
    val result = JsGen.renderModule(List.empty)
    assertEquals(result, "")
  }

  test("module with reserved word identifier escapes correctly") {
    val bindings: List[(Identifier.Bindable, Expr[Unit])] = List(
      (bindable("class"), Literal(Lit.Integer(42))),
      (bindable("function"), Literal(Lit.Str("test")))
    )

    val result = JsGen.renderModule(bindings)

    // Should escape reserved words
    assert(result.contains("const _class") || result.contains("const $class"),
      s"Expected escaped 'class', got: $result")
    assert(result.contains("const _function") || result.contains("const $function"),
      s"Expected escaped 'function', got: $result")
  }
}
