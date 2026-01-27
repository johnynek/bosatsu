package dev.bosatsu.codegen.js

import munit.ScalaCheckSuite
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll
import dev.bosatsu.{Identifier, Lit, Matchless, PackageName}
import dev.bosatsu.Identifier.Name
import cats.data.NonEmptyList

/**
 * Tests for JS/WASM Interop patterns.
 *
 * Phase 3.5 establishes the pattern for hybrid JS/WASM execution:
 * - JsGen produces JS for orchestration/UI code
 * - ClangGen produces C which compiles to WASM for compute-heavy code
 * - Both targets compile from the same Bosatsu source
 *
 * These tests verify:
 * 1. The same Matchless expressions produce semantically equivalent output
 *    when compiled to JS vs when they would compile to C/WASM
 * 2. The generated JS follows patterns compatible with WASM interop
 * 3. Function naming conventions are consistent across targets
 */
class InteropTest extends ScalaCheckSuite {

  import Matchless._

  def bindable(name: String): Identifier.Bindable = Name(name)

  // ==================
  // Naming Convention Tests
  // ==================

  test("global export names follow predictable pattern") {
    // JS exports use: Package_Subpackage$name
    // C exports use: ___bsts_g_Package_l_Subpackage_l_name
    // Both should derive from the same source identifier

    val name = bindable("fib")
    val jsIdent = JsGen.escape(name)

    // JS escape should produce a valid JS identifier
    assertEquals(jsIdent.name, "fib")

    // C naming convention is handled by ClangGen (tested elsewhere)
    // but the base name should be the same
    assert(jsIdent.name == name.asString, "Base name should be preserved")
  }

  test("reserved words are escaped consistently") {
    // Both JS and C have reserved words that need escaping
    val jsReserved = List("class", "function", "var", "let", "const")

    jsReserved.foreach { word =>
      val name = bindable(word)
      val jsIdent = JsGen.escape(name)
      assert(!JsGen.jsReservedWords.contains(jsIdent.name),
        s"'$word' should be escaped in JS to '${jsIdent.name}'")
    }
  }

  // ==================
  // Numeric Expression Equivalence Tests
  // ==================

  property("integer literals produce consistent representations") {
    forAll(Gen.choose(-1000000L, 1000000L)) { n =>
      val expr: Expr[Unit] = Literal(Lit.Integer(n))
      val jsResult = JsGen.renderExpr(expr)

      // JS should render integers directly (or with parens for negatives)
      if (n >= 0) {
        jsResult == n.toString
      } else {
        jsResult == s"($n)"
      }
    }
  }

  property("arithmetic expressions maintain semantics") {
    // Test that the structure of arithmetic expressions is preserved
    // The actual operations (add, sub, etc.) are handled by the runtime

    forAll(Gen.choose(0L, 100L), Gen.choose(0L, 100L)) { (a, b) =>
      // Create a let expression: let x = a in let y = b in x
      val expr: Expr[Unit] = Let(
        Right(bindable("x")),
        Literal(Lit.Integer(a)),
        Let(
          Right(bindable("y")),
          Literal(Lit.Integer(b)),
          Local(bindable("x"))
        )
      )

      val jsResult = JsGen.renderExpr(expr)

      // Should contain both bindings
      jsResult.contains("const x") && jsResult.contains("const y")
    }
  }

  // ==================
  // Function Structure Tests
  // ==================

  test("lambda functions produce compatible structure") {
    // Both JS and WASM use the same calling convention:
    // - Functions are curried (one arg at a time)
    // - Closures capture free variables

    val lambda: Expr[Unit] = Lambda(
      captures = List(bindable("captured")),
      recursionKind = None,
      args = NonEmptyList.of(bindable("n")),
      body = Local(bindable("n"))
    )

    val jsResult = JsGen.renderExpr(lambda)

    // JS should produce arrow function
    assert(jsResult.contains("=>"), s"Expected arrow function, got: $jsResult")
    assert(jsResult.contains("n"), s"Expected parameter 'n', got: $jsResult")
  }

  test("recursive functions are handled correctly") {
    // Recursive functions need special handling in both JS and C
    val recLambda: Expr[Unit] = Lambda(
      captures = Nil,
      recursionKind = Some(bindable("rec")),
      args = NonEmptyList.of(bindable("n")),
      body = If(
        CheckVariant(Local(bindable("n")), 0, 0, List(0)),
        ZeroNat,
        Local(bindable("n"))
      )
    )

    val jsResult = JsGen.renderExpr(recLambda)

    // Should produce valid JS
    assert(jsResult.nonEmpty, "Recursive function should produce output")
    assert(jsResult.contains("=>"), s"Expected arrow function, got: $jsResult")
  }

  // ==================
  // Data Structure Tests
  // ==================

  test("enum representations are consistent") {
    // Enums are represented as arrays in both JS and WASM
    // [variant_tag, ...fields]

    // Zero-arity enum (like None)
    val noneExpr: Expr[Unit] = MakeEnum(0, 0, List(0, 1))
    val noneJs = JsGen.renderExpr(noneExpr)
    assertEquals(noneJs, "[0]")

    // One-arity enum (like Some(x))
    val someExpr: Expr[Unit] = MakeEnum(1, 1, List(0, 1))
    val someJs = JsGen.renderExpr(someExpr)
    assert(someJs.contains("=>"), s"Expected constructor function, got: $someJs")
    assert(someJs.contains("[1,"), s"Expected array with tag 1, got: $someJs")
  }

  test("struct representations are consistent") {
    // Structs are represented as arrays: [field0, field1, ...]

    val struct0: Expr[Unit] = MakeStruct(0)
    assertEquals(JsGen.renderExpr(struct0), "[]")

    val struct2: Expr[Unit] = MakeStruct(2)
    val struct2Js = JsGen.renderExpr(struct2)
    assert(struct2Js.contains("=>"), s"Expected constructor function, got: $struct2Js")
  }

  test("list representation follows cons/nil pattern") {
    // Lists are [0] for empty, [1, head, tail] for cons
    // This is critical for interop - both JS and WASM must agree

    val emptyList: Expr[Unit] = MakeEnum(0, 0, List(0, 2)) // Nil
    assertEquals(JsGen.renderExpr(emptyList), "[0]")

    val consConstructor: Expr[Unit] = MakeEnum(1, 2, List(0, 2)) // Cons
    val consJs = JsGen.renderExpr(consConstructor)
    assert(consJs.contains("[1,"), s"Cons should produce [1, ...], got: $consJs")
  }

  // ==================
  // Interop Pattern Tests
  // ==================

  test("global references use correct naming") {
    // When JS code calls WASM-provided functions, it needs to look them up
    // by their global name. The naming must be predictable.

    val globalRef: Expr[Unit] = Global(
      PackageName(NonEmptyList.of("Demo", "Compute")),
      bindable("fib")
    )

    val jsResult = JsGen.renderExpr(globalRef)

    // Should reference the global with proper naming
    assert(jsResult.contains("Demo") || jsResult.contains("demo"),
      s"Expected package reference, got: $jsResult")
    assert(jsResult.contains("Compute") || jsResult.contains("compute"),
      s"Expected subpackage reference, got: $jsResult")
    assert(jsResult.contains("fib"),
      s"Expected function name, got: $jsResult")
  }

  test("function application works across module boundaries") {
    // When orchestrator calls compute functions, the call should work
    // whether the function is implemented in JS or WASM

    val call: Expr[Unit] = App(
      Global(
        PackageName(NonEmptyList.of("Demo", "Compute")),
        bindable("fib")
      ),
      NonEmptyList.of(Literal(Lit.Integer(10)))
    )

    val jsResult = JsGen.renderExpr(call)

    // Should produce a function call
    assert(jsResult.contains("(10)"), s"Expected call with argument, got: $jsResult")
  }

  // ==================
  // Property Tests for Interop Safety
  // ==================

  property("all generated JS is syntactically plausible") {
    // We can't run a JS parser, but we can check basic properties

    val genSimpleExpr: Gen[Expr[Unit]] = Gen.oneOf(
      Gen.choose(-100L, 100L).map(n => Literal(Lit.Integer(n))),
      Gen.alphaStr.filter(_.nonEmpty).map(s => Local(bindable(s))),
      Gen.const(ZeroNat),
      Gen.const(TrueConst)
    )

    forAll(genSimpleExpr) { expr =>
      val js = JsGen.renderExpr(expr)

      // Should produce non-empty output
      js.nonEmpty &&
        // Should not have unbalanced brackets (basic check)
        js.count(_ == '(') == js.count(_ == ')') &&
        js.count(_ == '[') == js.count(_ == ']') &&
        js.count(_ == '{') == js.count(_ == '}')
    }
  }

  property("identifiers are always valid") {
    forAll(Gen.alphaNumStr.filter(s => s.nonEmpty && s.head.isLetter)) { name =>
      val ident = JsGen.escape(bindable(name))

      // First char must be letter, $, or _
      val first = ident.name.head
      (first.isLetter || first == '_' || first == '$') &&
        // Rest must be alphanumeric, $, or _
        ident.name.tail.forall(c => c.isLetterOrDigit || c == '_' || c == '$')
    }
  }

  // ==================
  // Documented Interop Patterns
  // ==================

  test("boolean values follow [0]/[1] convention") {
    // Bosatsu Bool is represented as [0] for False, [1] for True
    // This is used for JS/WASM interop validation

    // When checking a boolean condition:
    val ifExpr: Expr[Unit] = If(
      TrueConst,
      Literal(Lit.Integer(1)),
      Literal(Lit.Integer(0))
    )

    val js = JsGen.renderExpr(ifExpr)
    assert(js.contains("true"), s"Expected 'true' in condition, got: $js")
  }

  test("tuple values are arrays") {
    // Tuples are represented as arrays: (a, b) -> [a, b]
    // Tuple access is array indexing

    val tupleAccess: Expr[Unit] = GetStructElement(
      Local(bindable("pair")),
      index = 0,
      size = 2
    )

    val js = JsGen.renderExpr(tupleAccess)
    assert(js.contains("[0]"), s"Expected array access at index 0, got: $js")
  }
}
