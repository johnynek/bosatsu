package dev.bosatsu.simulation

import munit.FunSuite
import dev.bosatsu.{Identifier, Matchless, Lit, PackageName}
import dev.bosatsu.rankn.DataRepr
import dev.bosatsu.pattern.StrPart
import cats.data.NonEmptyList

class DerivationAnalyzerTest extends FunSuite {

  def bindable(name: String): Identifier.Bindable =
    Identifier.unsafeBindable(name)

  // ============================================
  // extractDependencies tests
  // ============================================

  test("extractDependencies from Local") {
    val expr = Matchless.Local(bindable("x"))
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set(bindable("x")))
  }

  test("extractDependencies from Literal") {
    val expr = Matchless.Literal(Lit.Integer(42))
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set.empty[Identifier.Bindable])
  }

  test("extractDependencies from Let removes bound variable") {
    // let y = x in y
    val expr = Matchless.Let(
      Right(bindable("y")),
      Matchless.Local(bindable("x")),
      Matchless.Local(bindable("y"))
    )
    val deps = DerivationAnalyzer.extractDependencies(expr)
    // x is a dependency, but y is bound locally
    assertEquals(deps, Set(bindable("x")))
  }

  test("extractDependencies from Let with LocalAnon") {
    val expr = Matchless.Let(
      Left(Matchless.LocalAnon(1)),
      Matchless.Local(bindable("x")),
      Matchless.Local(bindable("y"))
    )
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set(bindable("x"), bindable("y")))
  }

  test("extractDependencies from App") {
    // f(x, y)
    val fn = Matchless.Local(bindable("f"))
    val args = NonEmptyList.of(
      Matchless.Local(bindable("x")),
      Matchless.Local(bindable("y"))
    )
    val expr = Matchless.App(fn, args)
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set(bindable("f"), bindable("x"), bindable("y")))
  }

  test("extractDependencies from LetMut") {
    val expr = Matchless.LetMut(
      Matchless.LocalAnonMut(1),
      Matchless.Local(bindable("x"))
    )
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set(bindable("x")))
  }

  test("extractDependencies from If") {
    val cond = Matchless.EqualsLit(Matchless.Local(bindable("a")), Lit.Integer(1))
    val expr = Matchless.If(
      cond,
      Matchless.Local(bindable("b")),
      Matchless.Local(bindable("c"))
    )
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set(bindable("a"), bindable("b"), bindable("c")))
  }

  test("extractDependencies from Always") {
    val cond = Matchless.EqualsLit(Matchless.Local(bindable("a")), Lit.Integer(1))
    val expr = Matchless.Always(cond, Matchless.Local(bindable("b")))
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set(bindable("a"), bindable("b")))
  }

  test("extractDependencies from Lambda") {
    // Captures x, args are y, body uses x and z
    val expr = Matchless.Lambda(
      List(Matchless.Local(bindable("x"))),
      None,
      NonEmptyList.of(bindable("y")),
      Matchless.Local(bindable("z"))
    )
    val deps = DerivationAnalyzer.extractDependencies(expr)
    // Captures x, body uses z (y is a param so excluded)
    assertEquals(deps, Set(bindable("x"), bindable("z")))
  }

  test("extractDependencies from WhileExpr") {
    val cond = Matchless.EqualsLit(Matchless.Local(bindable("a")), Lit.Integer(0))
    val expr = Matchless.WhileExpr(cond, Matchless.Local(bindable("b")), Matchless.LocalAnonMut(1))
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set(bindable("a"), bindable("b")))
  }

  test("extractDependencies from PrevNat") {
    val expr = Matchless.PrevNat(Matchless.Local(bindable("n")))
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set(bindable("n")))
  }

  test("extractDependencies from GetEnumElement") {
    val expr = Matchless.GetEnumElement(Matchless.Local(bindable("e")), 0, 1, 2)
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set(bindable("e")))
  }

  test("extractDependencies from GetStructElement") {
    val expr = Matchless.GetStructElement(Matchless.Local(bindable("s")), 0, 2)
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set(bindable("s")))
  }

  test("extractDependencies from Global returns empty") {
    val expr = Matchless.Global((), PackageName.PredefName, bindable("add"))
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set.empty[Identifier.Bindable])
  }

  test("extractDependencies from LocalAnon returns empty") {
    val expr = Matchless.LocalAnon(1)
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set.empty[Identifier.Bindable])
  }

  test("extractDependencies from LocalAnonMut returns empty") {
    val expr = Matchless.LocalAnonMut(1)
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set.empty[Identifier.Bindable])
  }

  test("extractDependencies from ClosureSlot returns empty") {
    val expr = Matchless.ClosureSlot(0)
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set.empty[Identifier.Bindable])
  }

  test("extractDependencies from ZeroNat returns empty") {
    val deps = DerivationAnalyzer.extractDependencies(Matchless.ZeroNat)
    assertEquals(deps, Set.empty[Identifier.Bindable])
  }

  test("extractDependencies from SuccNat returns empty") {
    val deps = DerivationAnalyzer.extractDependencies(Matchless.SuccNat)
    assertEquals(deps, Set.empty[Identifier.Bindable])
  }

  test("extractDependencies from MakeEnum returns empty") {
    val expr = Matchless.MakeEnum(0, 2, List(0, 2))
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set.empty[Identifier.Bindable])
  }

  test("extractDependencies from MakeStruct returns empty") {
    val expr = Matchless.MakeStruct(3)
    val deps = DerivationAnalyzer.extractDependencies(expr)
    assertEquals(deps, Set.empty[Identifier.Bindable])
  }

  // ============================================
  // extractBoolDeps tests
  // ============================================

  test("extractBoolDeps from TrueConst") {
    val deps = DerivationAnalyzer.extractBoolDeps(Matchless.TrueConst)
    assertEquals(deps, Set.empty[Identifier.Bindable])
  }

  test("extractBoolDeps from EqualsLit") {
    val bexpr = Matchless.EqualsLit(Matchless.Local(bindable("x")), Lit.Integer(1))
    val deps = DerivationAnalyzer.extractBoolDeps(bexpr)
    assertEquals(deps, Set(bindable("x")))
  }

  test("extractBoolDeps from EqualsNat") {
    val bexpr = Matchless.EqualsNat(Matchless.Local(bindable("n")), DataRepr.ZeroNat)
    val deps = DerivationAnalyzer.extractBoolDeps(bexpr)
    assertEquals(deps, Set(bindable("n")))
  }

  test("extractBoolDeps from And") {
    val left = Matchless.EqualsLit(Matchless.Local(bindable("a")), Lit.Integer(1))
    val right = Matchless.EqualsLit(Matchless.Local(bindable("b")), Lit.Integer(2))
    val bexpr = Matchless.And(left, right)
    val deps = DerivationAnalyzer.extractBoolDeps(bexpr)
    assertEquals(deps, Set(bindable("a"), bindable("b")))
  }

  test("extractBoolDeps from CheckVariant") {
    val bexpr = Matchless.CheckVariant(Matchless.Local(bindable("e")), 0, 2, List(0, 2))
    val deps = DerivationAnalyzer.extractBoolDeps(bexpr)
    assertEquals(deps, Set(bindable("e")))
  }

  test("extractBoolDeps from SetMut") {
    val bexpr = Matchless.SetMut(Matchless.LocalAnonMut(1), Matchless.Local(bindable("v")))
    val deps = DerivationAnalyzer.extractBoolDeps(bexpr)
    assertEquals(deps, Set(bindable("v")))
  }

  test("extractBoolDeps from MatchString") {
    val bexpr = Matchless.MatchString(
      Matchless.Local(bindable("s")),
      List(StrPart.LitStr("prefix"), StrPart.LitStr("suffix")),
      Nil,
      false
    )
    val deps = DerivationAnalyzer.extractBoolDeps(bexpr)
    assertEquals(deps, Set(bindable("s")))
  }

  test("extractBoolDeps from LetBool with Bindable") {
    val bexpr = Matchless.LetBool(
      Right(bindable("x")),
      Matchless.Local(bindable("a")),
      Matchless.EqualsLit(Matchless.Local(bindable("x")), Lit.Integer(1))
    )
    val deps = DerivationAnalyzer.extractBoolDeps(bexpr)
    // a is a dep, x is bound locally
    assertEquals(deps, Set(bindable("a")))
  }

  test("extractBoolDeps from LetBool with LocalAnon") {
    val bexpr = Matchless.LetBool(
      Left(Matchless.LocalAnon(1)),
      Matchless.Local(bindable("a")),
      Matchless.EqualsLit(Matchless.Local(bindable("b")), Lit.Integer(1))
    )
    val deps = DerivationAnalyzer.extractBoolDeps(bexpr)
    assertEquals(deps, Set(bindable("a"), bindable("b")))
  }

  test("extractBoolDeps from LetMutBool") {
    val bexpr = Matchless.LetMutBool(
      Matchless.LocalAnonMut(1),
      Matchless.EqualsLit(Matchless.Local(bindable("x")), Lit.Integer(1))
    )
    val deps = DerivationAnalyzer.extractBoolDeps(bexpr)
    assertEquals(deps, Set(bindable("x")))
  }

  // ============================================
  // exprToFormula tests
  // ============================================

  test("exprToFormula for Literal Integer") {
    assertEquals(
      DerivationAnalyzer.exprToFormula(Matchless.Literal(Lit.Integer(42))),
      "42"
    )
  }

  test("exprToFormula for Literal String") {
    assertEquals(
      DerivationAnalyzer.exprToFormula(Matchless.Literal(Lit.Str("hello"))),
      "\"hello\""
    )
  }

  test("exprToFormula for Literal Char") {
    assertEquals(
      DerivationAnalyzer.exprToFormula(Matchless.Literal(Lit.Chr("a"))),
      "'a'"
    )
  }

  test("exprToFormula for Local") {
    val expr = Matchless.Local(bindable("x"))
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "x")
  }

  test("exprToFormula for Global with PredefName") {
    val expr = Matchless.Global((), PackageName.PredefName, bindable("add"))
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "add")
  }

  test("exprToFormula for Global with other package") {
    val pkg = PackageName.parse("MyPackage").get
    val expr = Matchless.Global((), pkg, bindable("myFunc"))
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "MyPackage::myFunc")
  }

  test("exprToFormula for LocalAnon") {
    val expr = Matchless.LocalAnon(42)
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "_anon42")
  }

  test("exprToFormula for LocalAnonMut") {
    val expr = Matchless.LocalAnonMut(5)
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "_mut5")
  }

  test("exprToFormula for ClosureSlot") {
    val expr = Matchless.ClosureSlot(3)
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "_slot3")
  }

  test("exprToFormula for predef add converts to infix") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("add"))
    val args = NonEmptyList.of(
      Matchless.Local(bindable("a")),
      Matchless.Local(bindable("b"))
    )
    val expr = Matchless.App(fn, args)
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "(a + b)")
  }

  test("exprToFormula for predef sub converts to infix") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("sub"))
    val args = NonEmptyList.of(
      Matchless.Local(bindable("a")),
      Matchless.Local(bindable("b"))
    )
    val expr = Matchless.App(fn, args)
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "(a - b)")
  }

  test("exprToFormula for predef times converts to infix") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("times"))
    val args = NonEmptyList.of(
      Matchless.Local(bindable("x")),
      Matchless.Local(bindable("y"))
    )
    val expr = Matchless.App(fn, args)
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "(x * y)")
  }

  test("exprToFormula for predef div converts to infix") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("div"))
    val args = NonEmptyList.of(
      Matchless.Local(bindable("a")),
      Matchless.Local(bindable("b"))
    )
    val expr = Matchless.App(fn, args)
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "(a / b)")
  }

  test("exprToFormula for predef mod_Int converts to infix") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("mod_Int"))
    val args = NonEmptyList.of(
      Matchless.Local(bindable("a")),
      Matchless.Local(bindable("b"))
    )
    val expr = Matchless.App(fn, args)
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "(a % b)")
  }

  test("exprToFormula for predef eq_Int converts to infix") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("eq_Int"))
    val args = NonEmptyList.of(
      Matchless.Local(bindable("a")),
      Matchless.Local(bindable("b"))
    )
    val expr = Matchless.App(fn, args)
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "(a == b)")
  }

  test("exprToFormula for predef cmp_Int") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("cmp_Int"))
    val args = NonEmptyList.of(
      Matchless.Local(bindable("a")),
      Matchless.Local(bindable("b"))
    )
    val expr = Matchless.App(fn, args)
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "compare(a, b)")
  }

  test("exprToFormula for predef other function") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("concat_String"))
    val args = NonEmptyList.of(
      Matchless.Local(bindable("a")),
      Matchless.Local(bindable("b"))
    )
    val expr = Matchless.App(fn, args)
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "concat_String(a, b)")
  }

  test("exprToFormula for non-predef App") {
    val fn = Matchless.Local(bindable("myFunc"))
    val args = NonEmptyList.of(Matchless.Local(bindable("x")))
    val expr = Matchless.App(fn, args)
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "myFunc(x)")
  }

  test("exprToFormula for Let with Bindable") {
    val expr = Matchless.Let(
      Right(bindable("x")),
      Matchless.Literal(Lit.Integer(1)),
      Matchless.Local(bindable("x"))
    )
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "let x = 1 in x")
  }

  test("exprToFormula for Let with LocalAnon") {
    val expr = Matchless.Let(
      Left(Matchless.LocalAnon(42)),
      Matchless.Literal(Lit.Integer(1)),
      Matchless.Local(bindable("y"))
    )
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "let _anon42 = 1 in y")
  }

  test("exprToFormula for LetMut") {
    val expr = Matchless.LetMut(
      Matchless.LocalAnonMut(5),
      Matchless.Local(bindable("x"))
    )
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "let mut _mut5 in x")
  }

  test("exprToFormula for If") {
    val cond = Matchless.EqualsLit(Matchless.Local(bindable("x")), Lit.Integer(1))
    val expr = Matchless.If(
      cond,
      Matchless.Literal(Lit.Str("yes")),
      Matchless.Literal(Lit.Str("no"))
    )
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "if (x == 1) then \"yes\" else \"no\"")
  }

  test("exprToFormula for Always") {
    val cond = Matchless.TrueConst
    val expr = Matchless.Always(cond, Matchless.Literal(Lit.Integer(42)))
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "(true; 42)")
  }

  test("exprToFormula for Lambda") {
    val expr = Matchless.Lambda(
      Nil,
      None,
      NonEmptyList.of(bindable("x")),
      Matchless.Local(bindable("x"))
    )
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "(x) => x")
  }

  test("exprToFormula for Lambda with recursion") {
    val expr = Matchless.Lambda(
      Nil,
      Some(bindable("loop")),
      NonEmptyList.of(bindable("x")),
      Matchless.Local(bindable("x"))
    )
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "rec loop (x) => x")
  }

  test("exprToFormula for MakeEnum") {
    val expr = Matchless.MakeEnum(1, 3, List(0, 2, 3))
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "Enum(1, arity=3)")
  }

  test("exprToFormula for MakeStruct") {
    val expr = Matchless.MakeStruct(4)
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "Struct(arity=4)")
  }

  test("exprToFormula for ZeroNat") {
    assertEquals(DerivationAnalyzer.exprToFormula(Matchless.ZeroNat), "0")
  }

  test("exprToFormula for SuccNat") {
    assertEquals(DerivationAnalyzer.exprToFormula(Matchless.SuccNat), "succ")
  }

  test("exprToFormula for PrevNat") {
    val expr = Matchless.PrevNat(Matchless.Local(bindable("n")))
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "pred(n)")
  }

  test("exprToFormula for GetEnumElement") {
    val expr = Matchless.GetEnumElement(Matchless.Local(bindable("e")), 0, 1, 3)
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "e[1]")
  }

  test("exprToFormula for GetStructElement") {
    val expr = Matchless.GetStructElement(Matchless.Local(bindable("s")), 2, 4)
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "s.2")
  }

  test("exprToFormula for WhileExpr") {
    val cond = Matchless.TrueConst
    val expr = Matchless.WhileExpr(cond, Matchless.Local(bindable("x")), Matchless.LocalAnonMut(1))
    assertEquals(DerivationAnalyzer.exprToFormula(expr), "while true do x return LocalAnonMut(1)")
  }

  // ============================================
  // boolExprToFormula tests
  // ============================================

  test("boolExprToFormula for TrueConst") {
    assertEquals(DerivationAnalyzer.boolExprToFormula(Matchless.TrueConst), "true")
  }

  test("boolExprToFormula for EqualsLit Integer") {
    val bexpr = Matchless.EqualsLit(Matchless.Local(bindable("x")), Lit.Integer(42))
    assertEquals(DerivationAnalyzer.boolExprToFormula(bexpr), "(x == 42)")
  }

  test("boolExprToFormula for EqualsLit String") {
    val bexpr = Matchless.EqualsLit(Matchless.Local(bindable("s")), Lit.Str("hello"))
    assertEquals(DerivationAnalyzer.boolExprToFormula(bexpr), "(s == \"hello\")")
  }

  test("boolExprToFormula for EqualsLit Char") {
    val bexpr = Matchless.EqualsLit(Matchless.Local(bindable("c")), Lit.Chr("a"))
    assertEquals(DerivationAnalyzer.boolExprToFormula(bexpr), "(c == 'a')")
  }

  test("boolExprToFormula for EqualsNat ZeroNat") {
    val bexpr = Matchless.EqualsNat(Matchless.Local(bindable("n")), DataRepr.ZeroNat)
    assertEquals(DerivationAnalyzer.boolExprToFormula(bexpr), "(n 0)")
  }

  test("boolExprToFormula for EqualsNat SuccNat") {
    val bexpr = Matchless.EqualsNat(Matchless.Local(bindable("n")), DataRepr.SuccNat)
    assertEquals(DerivationAnalyzer.boolExprToFormula(bexpr), "(n > 0)")
  }

  test("boolExprToFormula for And") {
    val left = Matchless.EqualsLit(Matchless.Local(bindable("a")), Lit.Integer(1))
    val right = Matchless.EqualsLit(Matchless.Local(bindable("b")), Lit.Integer(2))
    val bexpr = Matchless.And(left, right)
    assertEquals(DerivationAnalyzer.boolExprToFormula(bexpr), "((a == 1) && (b == 2))")
  }

  test("boolExprToFormula for CheckVariant") {
    val bexpr = Matchless.CheckVariant(Matchless.Local(bindable("e")), 1, 3, List(0, 2, 3))
    assertEquals(DerivationAnalyzer.boolExprToFormula(bexpr), "(e.tag == 1)")
  }

  test("boolExprToFormula for SetMut") {
    val bexpr = Matchless.SetMut(Matchless.LocalAnonMut(5), Matchless.Literal(Lit.Integer(10)))
    assertEquals(DerivationAnalyzer.boolExprToFormula(bexpr), "(_mut5 := 10)")
  }

  test("boolExprToFormula for MatchString") {
    val bexpr = Matchless.MatchString(
      Matchless.Local(bindable("s")),
      List(StrPart.LitStr("pre"), StrPart.LitStr("suf")),
      Nil,
      false
    )
    assertEquals(DerivationAnalyzer.boolExprToFormula(bexpr), "(s matches LitStr(pre)LitStr(suf))")
  }

  test("boolExprToFormula for LetBool with Bindable") {
    val bexpr = Matchless.LetBool(
      Right(bindable("x")),
      Matchless.Literal(Lit.Integer(1)),
      Matchless.EqualsLit(Matchless.Local(bindable("x")), Lit.Integer(1))
    )
    assertEquals(DerivationAnalyzer.boolExprToFormula(bexpr), "let x = 1 in (x == 1)")
  }

  test("boolExprToFormula for LetBool with LocalAnon") {
    val bexpr = Matchless.LetBool(
      Left(Matchless.LocalAnon(7)),
      Matchless.Literal(Lit.Integer(1)),
      Matchless.TrueConst
    )
    assertEquals(DerivationAnalyzer.boolExprToFormula(bexpr), "let _anon7 = 1 in true")
  }

  test("boolExprToFormula for LetMutBool") {
    val bexpr = Matchless.LetMutBool(
      Matchless.LocalAnonMut(3),
      Matchless.TrueConst
    )
    assertEquals(DerivationAnalyzer.boolExprToFormula(bexpr), "let mut _mut3 in true")
  }

  // ============================================
  // analyzeBinding and analyze tests
  // ============================================

  test("analyzeBinding classifies Assumption when no deps") {
    val expr = Matchless.Literal(Lit.Integer(100))
    val result = DerivationAnalyzer.analyzeBinding(bindable("income"), expr)

    assertEquals(result.name, bindable("income"))
    assertEquals(result.kind, DerivationAnalyzer.Assumption)
    assertEquals(result.dependencies, Set.empty[Identifier.Bindable])
    assertEquals(result.valueType, "number")
  }

  test("analyzeBinding classifies Computation when has deps") {
    // taxable = income - deductions (represented as sub(income, deductions))
    val fn = Matchless.Global((), PackageName.PredefName, bindable("sub"))
    val args = NonEmptyList.of(
      Matchless.Local(bindable("income")),
      Matchless.Local(bindable("deductions"))
    )
    val expr = Matchless.App(fn, args)
    val result = DerivationAnalyzer.analyzeBinding(bindable("taxable"), expr)

    assertEquals(result.name, bindable("taxable"))
    assertEquals(result.kind, DerivationAnalyzer.Computation)
    assert(result.dependencies.contains(bindable("income")))
    assert(result.dependencies.contains(bindable("deductions")))
  }

  test("analyzeBinding classifies Conditional when has If") {
    val cond = Matchless.EqualsLit(
      Matchless.Local(bindable("status")),
      Lit.Integer(1)
    )
    val expr = Matchless.If(
      cond,
      Matchless.Literal(Lit.Integer(100)),
      Matchless.Literal(Lit.Integer(0))
    )
    val result = DerivationAnalyzer.analyzeBinding(bindable("bonus"), expr)

    assertEquals(result.kind, DerivationAnalyzer.Conditional)
  }

  test("analyzeBinding classifies Conditional with nested If in Let") {
    val cond = Matchless.TrueConst
    val ifExpr = Matchless.If(cond, Matchless.Literal(Lit.Integer(1)), Matchless.Literal(Lit.Integer(0)))
    val expr = Matchless.Let(Right(bindable("x")), ifExpr, Matchless.Local(bindable("x")))
    val result = DerivationAnalyzer.analyzeBinding(bindable("y"), expr)
    assertEquals(result.kind, DerivationAnalyzer.Conditional)
  }

  test("analyzeBinding classifies Conditional with If in LetMut") {
    val cond = Matchless.TrueConst
    val ifExpr = Matchless.If(cond, Matchless.Literal(Lit.Integer(1)), Matchless.Literal(Lit.Integer(0)))
    val expr = Matchless.LetMut(Matchless.LocalAnonMut(1), ifExpr)
    val result = DerivationAnalyzer.analyzeBinding(bindable("y"), expr)
    assertEquals(result.kind, DerivationAnalyzer.Conditional)
  }

  test("analyzeBinding classifies Conditional with If in Always") {
    val cond = Matchless.TrueConst
    val ifExpr = Matchless.If(cond, Matchless.Literal(Lit.Integer(1)), Matchless.Literal(Lit.Integer(0)))
    val expr = Matchless.Always(Matchless.TrueConst, ifExpr)
    val result = DerivationAnalyzer.analyzeBinding(bindable("y"), expr)
    assertEquals(result.kind, DerivationAnalyzer.Conditional)
  }

  test("analyzeBinding classifies Conditional with If in App") {
    val cond = Matchless.TrueConst
    val ifExpr = Matchless.If(cond, Matchless.Literal(Lit.Integer(1)), Matchless.Literal(Lit.Integer(0)))
    val fn = Matchless.Local(bindable("f"))
    val expr = Matchless.App(fn, NonEmptyList.of(ifExpr))
    val result = DerivationAnalyzer.analyzeBinding(bindable("y"), expr)
    assertEquals(result.kind, DerivationAnalyzer.Conditional)
  }

  test("analyzeBinding classifies Conditional with If in Lambda body") {
    val cond = Matchless.TrueConst
    val ifExpr = Matchless.If(cond, Matchless.Literal(Lit.Integer(1)), Matchless.Literal(Lit.Integer(0)))
    val expr = Matchless.Lambda(Nil, None, NonEmptyList.of(bindable("x")), ifExpr)
    val result = DerivationAnalyzer.analyzeBinding(bindable("y"), expr)
    assertEquals(result.kind, DerivationAnalyzer.Conditional)
  }

  test("analyzeBinding classifies Conditional with If in WhileExpr") {
    val cond = Matchless.TrueConst
    val ifExpr = Matchless.If(cond, Matchless.Literal(Lit.Integer(1)), Matchless.Literal(Lit.Integer(0)))
    val expr = Matchless.WhileExpr(Matchless.TrueConst, ifExpr, Matchless.LocalAnonMut(1))
    val result = DerivationAnalyzer.analyzeBinding(bindable("y"), expr)
    assertEquals(result.kind, DerivationAnalyzer.Conditional)
  }

  test("analyze filters deps to scope") {
    val income = Matchless.Literal(Lit.Integer(100000))
    val rate = Matchless.Literal(Lit.Integer(25))

    // tax = times(income, external_rate)
    // where external_rate is not in scope
    val fn = Matchless.Global((), PackageName.PredefName, bindable("times"))
    val taxExpr = Matchless.App(
      fn,
      NonEmptyList.of(
        Matchless.Local(bindable("income")),
        Matchless.Local(bindable("external_rate"))  // Not in our bindings
      )
    )

    val bindings = List(
      (bindable("income"), income),
      (bindable("tax"), taxExpr)
    )

    val results = DerivationAnalyzer.analyze(bindings)

    assertEquals(results.length, 2)

    val incomeResult = results.find(_.name == bindable("income")).get
    assertEquals(incomeResult.kind, DerivationAnalyzer.Assumption)

    val taxResult = results.find(_.name == bindable("tax")).get
    assertEquals(taxResult.kind, DerivationAnalyzer.Computation)
    // Only income is in scope, external_rate is filtered out
    assertEquals(taxResult.dependencies, Set(bindable("income")))
  }

  // ============================================
  // inferType tests
  // ============================================

  test("inferType returns number for Integer Literal") {
    assertEquals(
      DerivationAnalyzer.inferType(Matchless.Literal(Lit.Integer(42))),
      "number"
    )
  }

  test("inferType returns string for String Literal") {
    assertEquals(
      DerivationAnalyzer.inferType(Matchless.Literal(Lit.Str("hello"))),
      "string"
    )
  }

  test("inferType returns string for Char Literal") {
    assertEquals(
      DerivationAnalyzer.inferType(Matchless.Literal(Lit.Chr("a"))),
      "string"
    )
  }

  test("inferType returns number for ZeroNat") {
    assertEquals(
      DerivationAnalyzer.inferType(Matchless.ZeroNat),
      "number"
    )
  }

  test("inferType returns number for SuccNat") {
    assertEquals(
      DerivationAnalyzer.inferType(Matchless.SuccNat),
      "number"
    )
  }

  test("inferType returns number for PrevNat") {
    assertEquals(
      DerivationAnalyzer.inferType(Matchless.PrevNat(Matchless.Local(bindable("n")))),
      "number"
    )
  }

  test("inferType returns boolean for zero-arity MakeEnum") {
    assertEquals(
      DerivationAnalyzer.inferType(Matchless.MakeEnum(0, 0, List(0))),
      "boolean"
    )
  }

  test("inferType returns any for non-zero arity MakeEnum") {
    assertEquals(
      DerivationAnalyzer.inferType(Matchless.MakeEnum(0, 2, List(0, 2))),
      "any"
    )
  }

  test("inferType returns number for predef add") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("add"))
    val expr = Matchless.App(fn, NonEmptyList.of(Matchless.Local(bindable("a")), Matchless.Local(bindable("b"))))
    assertEquals(DerivationAnalyzer.inferType(expr), "number")
  }

  test("inferType returns number for predef sub") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("sub"))
    val expr = Matchless.App(fn, NonEmptyList.of(Matchless.Local(bindable("a")), Matchless.Local(bindable("b"))))
    assertEquals(DerivationAnalyzer.inferType(expr), "number")
  }

  test("inferType returns number for predef times") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("times"))
    val expr = Matchless.App(fn, NonEmptyList.of(Matchless.Local(bindable("a")), Matchless.Local(bindable("b"))))
    assertEquals(DerivationAnalyzer.inferType(expr), "number")
  }

  test("inferType returns number for predef div") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("div"))
    val expr = Matchless.App(fn, NonEmptyList.of(Matchless.Local(bindable("a")), Matchless.Local(bindable("b"))))
    assertEquals(DerivationAnalyzer.inferType(expr), "number")
  }

  test("inferType returns number for predef mod_Int") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("mod_Int"))
    val expr = Matchless.App(fn, NonEmptyList.of(Matchless.Local(bindable("a")), Matchless.Local(bindable("b"))))
    assertEquals(DerivationAnalyzer.inferType(expr), "number")
  }

  test("inferType returns boolean for predef eq_Int") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("eq_Int"))
    val expr = Matchless.App(fn, NonEmptyList.of(Matchless.Local(bindable("a")), Matchless.Local(bindable("b"))))
    assertEquals(DerivationAnalyzer.inferType(expr), "boolean")
  }

  test("inferType returns boolean for predef cmp_Int") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("cmp_Int"))
    val expr = Matchless.App(fn, NonEmptyList.of(Matchless.Local(bindable("a")), Matchless.Local(bindable("b"))))
    assertEquals(DerivationAnalyzer.inferType(expr), "boolean")
  }

  test("inferType returns string for predef concat_String") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("concat_String"))
    val expr = Matchless.App(fn, NonEmptyList.of(Matchless.Local(bindable("a")), Matchless.Local(bindable("b"))))
    assertEquals(DerivationAnalyzer.inferType(expr), "string")
  }

  test("inferType returns string for predef int_to_String") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("int_to_String"))
    val expr = Matchless.App(fn, NonEmptyList.of(Matchless.Local(bindable("n"))))
    assertEquals(DerivationAnalyzer.inferType(expr), "string")
  }

  test("inferType returns string for predef char_to_String") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("char_to_String"))
    val expr = Matchless.App(fn, NonEmptyList.of(Matchless.Local(bindable("c"))))
    assertEquals(DerivationAnalyzer.inferType(expr), "string")
  }

  test("inferType returns any for unknown predef function") {
    val fn = Matchless.Global((), PackageName.PredefName, bindable("unknown_func"))
    val expr = Matchless.App(fn, NonEmptyList.of(Matchless.Local(bindable("a"))))
    assertEquals(DerivationAnalyzer.inferType(expr), "any")
  }

  test("inferType returns any for Local") {
    assertEquals(DerivationAnalyzer.inferType(Matchless.Local(bindable("x"))), "any")
  }

  test("inferType returns any for MakeStruct") {
    assertEquals(DerivationAnalyzer.inferType(Matchless.MakeStruct(2)), "any")
  }
}
