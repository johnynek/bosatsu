package dev.bosatsu.codegen.js

import munit.ScalaCheckSuite
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll
import dev.bosatsu.{Identifier, Lit, Matchless, PackageName}
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

  test("Literal string renders as Bosatsu string") {
    // Strings are converted to Bosatsu's internal string representation
    val helloResult = JsGen.renderExpr(Literal(Lit.Str("hello")))
    assert(helloResult.contains("_js_to_bosatsu_string"), s"Expected Bosatsu string conversion, got: $helloResult")
    assert(helloResult.contains("\"hello\""), s"Expected hello in string, got: $helloResult")

    val emptyResult = JsGen.renderExpr(Literal(Lit.Str("")))
    assert(emptyResult.contains("_js_to_bosatsu_string"), s"Expected Bosatsu string conversion, got: $emptyResult")
  }

  test("Literal char renders as Bosatsu char (single-element string)") {
    // Chars are represented as [1, char, [0]] (single-element linked list)
    val aResult = JsGen.renderExpr(Literal(Lit.Chr("a")))
    assert(aResult.contains("[1,"), s"Expected array with cons tag, got: $aResult")
    assert(aResult.contains("\"a\""), s"Expected char 'a' in array, got: $aResult")
    assert(aResult.contains("[0]"), s"Expected nil terminator, got: $aResult")

    val newlineResult = JsGen.renderExpr(Literal(Lit.Chr("\n")))
    assert(newlineResult.contains("[1,"), s"Expected array with cons tag, got: $newlineResult")
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
    // Strings are converted to Bosatsu string representation
    assert(result.contains("const y = _js_to_bosatsu_string(\"hello\")"), s"Expected y binding with Bosatsu string, got: $result")
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

  // ==================
  // Additional Coverage Tests
  // ==================

  test("escape handles Backticked identifiers") {
    val backticked = Identifier.Backticked("myVar")
    val ident = JsGen.escape(backticked)
    assertEquals(ident.name, "myVar")
  }

  test("escape handles names starting with digit") {
    val name = Name("123abc")
    val ident = JsGen.escape(name)
    // Should prefix with underscore
    assert(ident.name.startsWith("_"), s"Expected underscore prefix, got: ${ident.name}")
  }

  test("escape handles empty base name") {
    // A backticked empty string would result in empty escaped name
    val backticked = Identifier.Backticked("")
    val ident = JsGen.escape(backticked)
    // Should have underscore prefix
    assertEquals(ident.name, "_")
  }

  test("escapePackage creates valid module name") {
    val pack = PackageName.parse("Bosatsu/Predef").get
    val result = JsGen.escapePackage(pack)
    assertEquals(result, "Bosatsu_Predef")
  }

  test("qualifiedName creates unique identifier") {
    val pack = PackageName.parse("Bosatsu/Nat").get
    val name = bindable("times2")
    val ident = JsGen.qualifiedName(pack, name)
    assert(ident.name.contains("Bosatsu_Nat"), s"Expected package prefix, got: ${ident.name}")
    assert(ident.name.contains("times2"), s"Expected name, got: ${ident.name}")
  }

  // ==================
  // Env Monad Tests (using map/flatMap directly)
  // ==================

  test("Env.bind creates unique identifier") {
    val env = JsGen.Env.bind(bindable("x"))
    val (state, ident) = JsGen.Env.run(env)
    assertEquals(ident.name, "x")
    assert(state.bindings.contains(bindable("x")))
  }

  test("Env handles shadowing correctly") {
    import cats.syntax.all._
    given cats.Monad[JsGen.Env] = JsGen.Env.envMonad

    val env = JsGen.Env.bind(bindable("x")).flatMap { id1 =>
      JsGen.Env.bind(bindable("x")).flatMap { id2 =>
        JsGen.Env.deref(bindable("x")).map { result =>
          (id1, id2, result)
        }
      }
    }
    val (_, tuple) = JsGen.Env.run(env)
    // Shadowed binding should have different name
    assertNotEquals(tuple._1.name, tuple._2.name)
    assertEquals(tuple._3.name, tuple._2.name)
  }

  test("Env.unbind restores shadowed binding") {
    import cats.syntax.all._
    given cats.Monad[JsGen.Env] = JsGen.Env.envMonad

    val env = JsGen.Env.bind(bindable("x")).flatMap { id1 =>
      JsGen.Env.bind(bindable("x")).flatMap { id2 =>
        JsGen.Env.unbind(bindable("x")).flatMap { _ =>
          JsGen.Env.deref(bindable("x")).map { result =>
            (id1, id2, result)
          }
        }
      }
    }
    val (_, tuple) = JsGen.Env.run(env)
    // After unbind, should get original binding
    assertEquals(tuple._3.name, tuple._1.name)
  }

  test("Env.newTmp generates unique temporary names") {
    import cats.syntax.all._
    given cats.Monad[JsGen.Env] = JsGen.Env.envMonad

    val env = JsGen.Env.newTmp.flatMap { t1 =>
      JsGen.Env.newTmp.flatMap { t2 =>
        JsGen.Env.newTmp.map { t3 =>
          (t1, t2, t3)
        }
      }
    }
    val (_, tuple) = JsGen.Env.run(env)
    assertNotEquals(tuple._1.name, tuple._2.name)
    assertNotEquals(tuple._2.name, tuple._3.name)
    assert(tuple._1.name.startsWith("_tmp"))
  }

  test("Env.anonName returns consistent name for same id") {
    import cats.syntax.all._
    given cats.Monad[JsGen.Env] = JsGen.Env.envMonad

    val env = JsGen.Env.anonName(42L).flatMap { a1 =>
      JsGen.Env.anonName(42L).map { a2 =>
        (a1, a2)
      }
    }
    val (_, tuple) = JsGen.Env.run(env)
    assertEquals(tuple._1.name, tuple._2.name) // Same ID -> same name
  }

  test("Env.anonName creates different names for different ids") {
    import cats.syntax.all._
    given cats.Monad[JsGen.Env] = JsGen.Env.envMonad

    val env = JsGen.Env.anonName(42L).flatMap { a1 =>
      JsGen.Env.anonName(43L).map { a2 =>
        (a1, a2)
      }
    }
    val (_, tuple) = JsGen.Env.run(env)
    assertNotEquals(tuple._1.name, tuple._2.name)
  }

  test("Env.deref returns escaped name for unbound variable") {
    val env = JsGen.Env.deref(bindable("unbound"))
    val (_, ident) = JsGen.Env.run(env)
    assertEquals(ident.name, "unbound")
  }

  // ==================
  // EqualsLit Test
  // ==================

  test("EqualsLit comparison renders") {
    val eq = EqualsLit(Local(bindable("x")), Lit.Integer(42))
    val ifExpr = If(eq, Literal(Lit.Integer(1)), Literal(Lit.Integer(2)))
    val result = JsGen.renderExpr(ifExpr)
    assert(result.contains("42"), s"Expected literal comparison, got: $result")
    assert(result.contains("===") || result.contains("=="), s"Expected equality operator, got: $result")
  }

  // ==================
  // Additional Reserved Word Tests
  // ==================

  test("All common JS reserved words are escaped") {
    val reservedWords = List("class", "function", "var", "let", "const", "this", "super", "return", "if", "else")
    reservedWords.foreach { word =>
      val ident = JsGen.escape(Name(word))
      assertNotEquals(ident.name, word, s"Reserved word '$word' should be escaped")
    }
  }

  test("Common browser globals are escaped") {
    val globals = List("window", "document", "console")
    globals.foreach { word =>
      val ident = JsGen.escape(Name(word))
      assertNotEquals(ident.name, word, s"Browser global '$word' should be escaped")
    }
  }

  test("Standard library names are escaped") {
    val stdLibNames = List("Array", "Object", "String", "Number", "Boolean", "Function", "Math", "JSON")
    stdLibNames.foreach { word =>
      val ident = JsGen.escape(Name(word))
      assertNotEquals(ident.name, word, s"Standard library name '$word' should be escaped")
    }
  }

  // ==================
  // IO Intrinsic Tests
  // ==================

  val IOPackage: PackageName = PackageName.parse("Bosatsu/IO").get
  val UIPackage: PackageName = PackageName.parse("Bosatsu/UI").get

  def ioGlobal(name: String): Matchless.Expr[Unit] =
    Matchless.Global((), IOPackage, Name(name))

  def uiGlobal(name: String): Matchless.Expr[Unit] =
    Matchless.Global((), UIPackage, Name(name))

  test("IOExternal pure generates Pure tagged object") {
    val expr = App(ioGlobal("pure"), NonEmptyList.one(Literal(Lit.Integer(42))))
    val result = JsGen.renderExpr(expr)
    assert(result.contains("\"Pure\""), s"Expected Pure tag, got: $result")
    assert(result.contains("42"), s"Expected value 42, got: $result")
    assert(result.contains("tag"), s"Expected 'tag' key, got: $result")
    assert(result.contains("value"), s"Expected 'value' key, got: $result")
  }

  test("IOExternal flatMap generates FlatMap tagged object") {
    val io = App(ioGlobal("pure"), NonEmptyList.one(Literal(Lit.Integer(1))))
    val fn = Local(bindable("f"))
    val expr = App(ioGlobal("flatMap"), NonEmptyList.of(io, fn))
    val result = JsGen.renderExpr(expr)
    assert(result.contains("\"FlatMap\""), s"Expected FlatMap tag, got: $result")
    assert(result.contains("io"), s"Expected 'io' key, got: $result")
    assert(result.contains("fn"), s"Expected 'fn' key, got: $result")
  }

  test("IOExternal sequence generates Sequence tagged object") {
    val ios = Local(bindable("myList"))
    val expr = App(ioGlobal("sequence"), NonEmptyList.one(ios))
    val result = JsGen.renderExpr(expr)
    assert(result.contains("\"Sequence\""), s"Expected Sequence tag, got: $result")
    assert(result.contains("ios"), s"Expected 'ios' key, got: $result")
  }

  test("IOExternal trace generates Trace tagged object") {
    val unit = MakeStruct(0)
    val expr = App(ioGlobal("trace"), NonEmptyList.one(unit))
    val result = JsGen.renderExpr(expr)
    assert(result.contains("\"Trace\""), s"Expected Trace tag, got: $result")
  }

  test("IOExternal random_Int generates RandomInt tagged object") {
    val min = Literal(Lit.Integer(1))
    val max = Literal(Lit.Integer(10))
    val expr = App(ioGlobal("random_Int"), NonEmptyList.of(min, max))
    val result = JsGen.renderExpr(expr)
    assert(result.contains("\"RandomInt\""), s"Expected RandomInt tag, got: $result")
    assert(result.contains("min"), s"Expected 'min' key, got: $result")
    assert(result.contains("max"), s"Expected 'max' key, got: $result")
  }

  test("UIExternal write generates Write tagged object") {
    val stateObj = Local(bindable("myState"))
    val value = Literal(Lit.Integer(99))
    val expr = App(uiGlobal("write"), NonEmptyList.of(stateObj, value))
    val result = JsGen.renderExpr(expr)
    assert(result.contains("\"Write\""), s"Expected Write tag, got: $result")
    assert(result.contains("state"), s"Expected 'state' key, got: $result")
    assert(result.contains("value"), s"Expected 'value' key, got: $result")
  }

  test("UIExternal on_frame generates RegisterFrameCallback tagged object") {
    val updateFn = Local(bindable("myCallback"))
    val expr = App(uiGlobal("on_frame"), NonEmptyList.one(updateFn))
    val result = JsGen.renderExpr(expr)
    assert(result.contains("\"RegisterFrameCallback\""), s"Expected RegisterFrameCallback tag, got: $result")
    assert(result.contains("updateFn"), s"Expected 'updateFn' key, got: $result")
  }

  test("UIExternal on_click generates handler registration") {
    val handler = Local(bindable("myHandler"))
    val expr = App(uiGlobal("on_click"), NonEmptyList.one(handler))
    val result = JsGen.renderExpr(expr)
    assert(result.contains("data-onclick"), s"Expected data-onclick attribute, got: $result")
    assert(result.contains("_ui_register_handler"), s"Expected handler registration call, got: $result")
    assert(result.contains("\"click\""), s"Expected click event type, got: $result")
  }

  test("UIExternal on_input generates handler registration") {
    val handler = Local(bindable("myHandler"))
    val expr = App(uiGlobal("on_input"), NonEmptyList.one(handler))
    val result = JsGen.renderExpr(expr)
    assert(result.contains("data-oninput"), s"Expected data-oninput attribute, got: $result")
    assert(result.contains("_ui_register_handler"), s"Expected handler registration call, got: $result")
    assert(result.contains("\"input\""), s"Expected input event type, got: $result")
  }

  test("UIExternal read generates property access") {
    val stateObj = Local(bindable("myState"))
    val expr = App(uiGlobal("read"), NonEmptyList.one(stateObj))
    val result = JsGen.renderExpr(expr)
    assert(result.contains(".value"), s"Expected .value property access, got: $result")
  }

  test("IO intrinsics never generate function/thunk wrappers") {
    // Verify IO data structures are plain objects, not wrapped in () => { ... }
    val pureExpr = App(ioGlobal("pure"), NonEmptyList.one(Literal(Lit.Integer(1))))
    val pureResult = JsGen.renderExpr(pureExpr)
    assert(!pureResult.contains("() =>"), s"pure should not generate thunk, got: $pureResult")

    val stateObj = Local(bindable("s"))
    val writeExpr = App(uiGlobal("write"), NonEmptyList.of(stateObj, Literal(Lit.Integer(1))))
    val writeResult = JsGen.renderExpr(writeExpr)
    assert(!writeResult.contains("() =>"), s"write should not generate thunk, got: $writeResult")
  }
}
