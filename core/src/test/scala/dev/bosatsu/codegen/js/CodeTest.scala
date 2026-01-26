package dev.bosatsu.codegen.js

import munit.ScalaCheckSuite
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll

class CodeTest extends ScalaCheckSuite {

  import Code._

  // Helper to render and check
  def assertRenders(c: Code, expected: String)(implicit loc: munit.Location): Unit =
    assertEquals(render(c), expected)

  // ==================
  // Identifier Tests
  // ==================

  test("Ident renders correctly") {
    assertRenders(Ident("foo"), "foo")
    assertRenders(Ident("_bar"), "_bar")
    assertRenders(Ident("$baz"), "$baz")
    assertRenders(Ident("camelCase"), "camelCase")
  }

  // ==================
  // Literal Tests
  // ==================

  test("IntLiteral renders positive numbers") {
    assertRenders(IntLiteral(0), "0")
    assertRenders(IntLiteral(42), "42")
    assertRenders(IntLiteral(1234567890L), "1234567890")
  }

  test("IntLiteral renders negative numbers with parens") {
    assertRenders(IntLiteral(-1), "(-1)")
    assertRenders(IntLiteral(-42), "(-42)")
  }

  test("DoubleLiteral renders correctly") {
    assertRenders(DoubleLiteral(3.14), "3.14")
    assertRenders(DoubleLiteral(0.0), "0.0")
    assertRenders(DoubleLiteral(-2.5), "(-2.5)")
  }

  test("BoolLiteral renders correctly") {
    assertRenders(TrueLit, "true")
    assertRenders(FalseLit, "false")
  }

  test("NullLiteral renders correctly") {
    assertRenders(NullLiteral, "null")
  }

  test("UndefinedLiteral renders correctly") {
    assertRenders(UndefinedLiteral, "undefined")
  }

  test("StringLiteral renders simple strings") {
    assertRenders(StringLiteral("hello"), "\"hello\"")
    assertRenders(StringLiteral(""), "\"\"")
    assertRenders(StringLiteral("foo bar"), "\"foo bar\"")
  }

  test("StringLiteral escapes special characters") {
    assertRenders(StringLiteral("line1\nline2"), "\"line1\\nline2\"")
    assertRenders(StringLiteral("tab\there"), "\"tab\\there\"")
    assertRenders(StringLiteral("quote\"here"), "\"quote\\\"here\"")
    assertRenders(StringLiteral("back\\slash"), "\"back\\\\slash\"")
    assertRenders(StringLiteral("\r"), "\"\\r\"")
  }

  // ==================
  // Arrow Function Tests
  // ==================

  test("ArrowFunction with single param renders without parens") {
    val fn = ArrowFunction(List("x"), Ident("x"))
    assertRenders(fn, "x => x")
  }

  test("ArrowFunction with multiple params renders with parens") {
    val fn = ArrowFunction(List("x", "y"), Ident("x") + Ident("y"))
    assertRenders(fn, "(x, y) => x + y")
  }

  test("ArrowFunction with no params renders with parens") {
    val fn = ArrowFunction(List(), IntLiteral(42))
    assertRenders(fn, "() => 42")
  }

  test("ArrowFunction with block body renders correctly") {
    val fn = ArrowFunction(List("x"), block(Return(Some(Ident("x")))))
    assertRenders(fn, """x => {
  return x;
}""")
  }

  test("ArrowFunction returning object literal wraps in parens") {
    val fn = ArrowFunction(List("x"), ObjectLiteral(List("a" -> Ident("x"))))
    assertRenders(fn, "x => ({ a: x })")
  }

  // ==================
  // Function Tests
  // ==================

  test("Function with name renders correctly") {
    val fn = Function(Some("add"), List("a", "b"), block(Return(Some(Ident("a") + Ident("b")))))
    assertRenders(fn, """function add(a, b) {
  return a + b;
}""")
  }

  test("Anonymous function renders correctly") {
    val fn = Function(None, List("x"), block(Return(Some(Ident("x")))))
    assertRenders(fn, """function(x) {
  return x;
}""")
  }

  // ==================
  // Call Tests
  // ==================

  test("Call renders correctly") {
    val call = Call(Ident("foo"), List(Ident("x"), IntLiteral(42)))
    assertRenders(call, "foo(x, 42)")
  }

  test("Call with no args renders correctly") {
    val call = Call(Ident("doSomething"), List())
    assertRenders(call, "doSomething()")
  }

  test("Call on arrow function wraps in parens") {
    val fn = ArrowFunction(List("x"), Ident("x"))
    val call = Call(fn, List(IntLiteral(5)))
    assertRenders(call, "(x => x)(5)")
  }

  // ==================
  // PropertyAccess Tests
  // ==================

  test("PropertyAccess renders with dot notation") {
    val access = PropertyAccess(Ident("obj"), "prop")
    assertRenders(access, "obj.prop")
  }

  test("PropertyAccess chains correctly") {
    val chain = PropertyAccess(PropertyAccess(Ident("a"), "b"), "c")
    assertRenders(chain, "a.b.c")
  }

  // ==================
  // IndexAccess Tests
  // ==================

  test("IndexAccess renders correctly") {
    val access = IndexAccess(Ident("arr"), IntLiteral(0))
    assertRenders(access, "arr[0]")
  }

  test("IndexAccess with string key renders correctly") {
    val access = IndexAccess(Ident("obj"), StringLiteral("key"))
    assertRenders(access, "obj[\"key\"]")
  }

  // ==================
  // ArrayLiteral Tests
  // ==================

  test("ArrayLiteral empty renders correctly") {
    assertRenders(ArrayLiteral(List()), "[]")
  }

  test("ArrayLiteral with elements renders correctly") {
    val arr = ArrayLiteral(List(IntLiteral(1), IntLiteral(2), IntLiteral(3)))
    assertRenders(arr, "[1, 2, 3]")
  }

  // ==================
  // ObjectLiteral Tests
  // ==================

  test("ObjectLiteral empty renders correctly") {
    assertRenders(ObjectLiteral(List()), "{}")
  }

  test("ObjectLiteral with properties renders correctly") {
    val obj = ObjectLiteral(List("a" -> IntLiteral(1), "b" -> StringLiteral("two")))
    assertRenders(obj, "{ a: 1, b: \"two\" }")
  }

  // ==================
  // BinExpr Tests
  // ==================

  test("BinExpr arithmetic operators render correctly") {
    assertRenders(Ident("a") + Ident("b"), "a + b")
    assertRenders(Ident("a") - Ident("b"), "a - b")
    assertRenders(Ident("a") * Ident("b"), "a * b")
    assertRenders(Ident("a") / Ident("b"), "a / b")
    assertRenders(Ident("a") % Ident("b"), "a % b")
  }

  test("BinExpr comparison operators render correctly") {
    assertRenders(Ident("a") === Ident("b"), "a === b")
    assertRenders(Ident("a") !== Ident("b"), "a !== b")
    assertRenders(Ident("a") < Ident("b"), "a < b")
    assertRenders(Ident("a") <= Ident("b"), "a <= b")
    assertRenders(Ident("a") > Ident("b"), "a > b")
    assertRenders(Ident("a") >= Ident("b"), "a >= b")
  }

  test("BinExpr logical operators render correctly") {
    assertRenders(Ident("a") && Ident("b"), "a && b")
    assertRenders(Ident("a") || Ident("b"), "a || b")
  }

  test("BinExpr chains maintain associativity") {
    val expr = Ident("a") + Ident("b") + Ident("c")
    assertRenders(expr, "a + b + c")
  }

  // ==================
  // PrefixExpr Tests
  // ==================

  test("PrefixExpr not renders correctly") {
    assertRenders(!Ident("x"), "!x")
  }

  test("PrefixExpr neg renders correctly") {
    assertRenders(-Ident("x"), "-x")
  }

  test("PrefixExpr typeof renders correctly") {
    assertRenders(PrefixExpr(PrefixOp.TypeOf, Ident("x")), "typeof x")
  }

  // ==================
  // Ternary Tests
  // ==================

  test("Ternary renders correctly") {
    val tern = Ternary(Ident("cond"), Ident("a"), Ident("b"))
    assertRenders(tern, "cond ? a : b")
  }

  // ==================
  // NewExpr Tests
  // ==================

  test("NewExpr renders correctly") {
    val expr = NewExpr(Ident("Date"), List())
    assertRenders(expr, "new Date()")
  }

  test("NewExpr with args renders correctly") {
    val expr = NewExpr(Ident("Array"), List(IntLiteral(10)))
    assertRenders(expr, "new Array(10)")
  }

  // ==================
  // Statement Tests
  // ==================

  test("Const renders correctly") {
    assertRenders(Const("x", IntLiteral(42)), "const x = 42;")
  }

  test("Let with value renders correctly") {
    assertRenders(Let("x", Some(IntLiteral(42))), "let x = 42;")
  }

  test("Let without value renders correctly") {
    assertRenders(Let("x", None), "let x;")
  }

  test("Var renders correctly") {
    assertRenders(Var("x", Some(IntLiteral(42))), "var x = 42;")
  }

  test("Assignment renders correctly") {
    assertRenders(Assignment(Ident("x"), IntLiteral(42)), "x = 42;")
  }

  test("Return with value renders correctly") {
    assertRenders(Return(Some(Ident("x"))), "return x;")
  }

  test("Return without value renders correctly") {
    assertRenders(returnVoid, "return;")
  }

  test("ExprStatement renders correctly") {
    assertRenders(ExprStatement(Call(Ident("foo"), List())), "foo();")
  }

  // ==================
  // Block Tests
  // ==================

  test("Block renders correctly") {
    val blk = block(Const("x", IntLiteral(1)), Return(Some(Ident("x"))))
    assertRenders(blk, """{
  const x = 1;
  return x;
}""")
  }

  // ==================
  // IfStatement Tests
  // ==================

  test("IfStatement without else renders correctly") {
    val stmt = IfStatement(Ident("cond"), block(Return(Some(IntLiteral(1)))), None)
    assertRenders(stmt, """if (cond) {
  return 1;
}""")
  }

  test("IfStatement with else renders correctly") {
    val stmt = ifThenElse(Ident("cond"), Return(Some(IntLiteral(1))), Return(Some(IntLiteral(2))))
    assertRenders(stmt, """if (cond) {
  return 1;
} else {
  return 2;
}""")
  }

  test("IfStatement with else if renders correctly") {
    val elseIf = IfStatement(Ident("cond2"), block(Return(Some(IntLiteral(2)))), None)
    val stmt = IfStatement(Ident("cond1"), block(Return(Some(IntLiteral(1)))), Some(Left(elseIf)))
    assertRenders(stmt, """if (cond1) {
  return 1;
} else if (cond2) {
  return 2;
}""")
  }

  // ==================
  // WhileLoop Tests
  // ==================

  test("WhileLoop renders correctly") {
    val loop = WhileLoop(Ident("cond"), block(ExprStatement(Call(Ident("doSomething"), List()))))
    assertRenders(loop, """while (cond) {
  doSomething();
}""")
  }

  // ==================
  // Import/Export Tests
  // ==================

  test("Export renders correctly") {
    assertRenders(Export("foo"), "export {foo};")
  }

  test("ExportDefault renders correctly") {
    assertRenders(ExportDefault(Ident("foo")), "export default foo;")
  }

  test("Import renders correctly") {
    assertRenders(Import(List("foo", "bar"), "./module"), "import {foo, bar} from \"./module\";")
  }

  // ==================
  // TryCatch Tests
  // ==================

  test("TryCatch renders correctly") {
    val stmt = TryCatch(
      block(ExprStatement(Call(Ident("risky"), List()))),
      "e",
      block(ExprStatement(Call(Ident("log"), List(Ident("e"))))),
      None
    )
    assertRenders(stmt, """try {
  risky();
} catch (e) {
  log(e);
}""")
  }

  test("TryCatch with finally renders correctly") {
    val stmt = TryCatch(
      block(ExprStatement(Call(Ident("risky"), List()))),
      "e",
      block(ExprStatement(Call(Ident("log"), List(Ident("e"))))),
      Some(block(ExprStatement(Call(Ident("cleanup"), List()))))
    )
    assertRenders(stmt, """try {
  risky();
} catch (e) {
  log(e);
}finally {
  cleanup();
}""")
  }

  // ==================
  // Throw Tests
  // ==================

  test("Throw renders correctly") {
    assertRenders(Throw(NewExpr(Ident("Error"), List(StringLiteral("oops")))), "throw new Error(\"oops\");")
  }

  // ==================
  // Property Tests
  // ==================

  import JsGenGen._

  property("rendered identifiers are non-empty") {
    forAll(genIdent) { ident =>
      render(ident).nonEmpty
    }
  }

  property("rendered literals are non-empty") {
    forAll(genSimpleLiteral) { lit =>
      render(lit).nonEmpty
    }
  }

  property("rendered expressions are non-empty") {
    forAll(genExpression) { expr =>
      render(expr).nonEmpty
    }
  }

  property("rendered statements are non-empty") {
    forAll(genStatement) { stmt =>
      render(stmt).nonEmpty
    }
  }

  property("string literals start and end with quotes") {
    forAll(genStringLiteral) { lit =>
      val rendered = render(lit)
      rendered.startsWith("\"") && rendered.endsWith("\"")
    }
  }

  property("escapeString is idempotent for safe strings") {
    forAll(Gen.alphaNumStr) { s =>
      // For alphanumeric strings, escape should return the same
      escapeString(s) == s
    }
  }

  property("binary expressions produce valid syntax") {
    forAll(JsGenGen.genBinExpr(2)) { expr =>
      val rendered = render(expr)
      rendered.nonEmpty && !rendered.contains("undefined undefined")
    }
  }

  property("arrow functions produce valid syntax") {
    forAll(JsGenGen.genArrowFunction(1)) { fn =>
      val rendered = render(fn)
      rendered.contains("=>")
    }
  }

  property("blocks have matching braces") {
    forAll(JsGenGen.genBlock(1)) { block =>
      val rendered = render(block)
      val opens = rendered.count(_ == '{')
      val closes = rendered.count(_ == '}')
      opens == closes && opens >= 1
    }
  }

  property("const declarations include 'const' keyword") {
    forAll(JsGenGen.genConst(1)) { c =>
      val rendered = render(c)
      rendered.startsWith("const ")
    }
  }

  property("let declarations include 'let' keyword") {
    forAll(JsGenGen.genLet(1)) { l =>
      val rendered = render(l)
      rendered.startsWith("let ")
    }
  }

  property("return statements include 'return' keyword") {
    forAll(JsGenGen.genReturn(1)) { r =>
      val rendered = render(r)
      rendered.startsWith("return")
    }
  }

  property("identifiers don't contain reserved words") {
    forAll(genIdent) { ident =>
      !JsGenGen.jsReservedWords.contains(ident.name)
    }
  }
}
