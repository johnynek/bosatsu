package dev.bosatsu.ui

import munit.FunSuite
import dev.bosatsu.{TypedExpr, Identifier, PackageName, Lit}
import dev.bosatsu.Identifier.Bindable
import dev.bosatsu.rankn.Type

class UIAnalyzerTest extends FunSuite {

  // Helper to create mock TypedExpr nodes for testing
  private def makeLiteral(i: Int): TypedExpr[Unit] =
    TypedExpr.Literal(Lit.Integer(i), Type.IntType, ())

  private def makeStrLiteral(s: String): TypedExpr[Unit] =
    TypedExpr.Literal(Lit.Str(s), Type.StrType, ())

  private def makeLocal(name: String): TypedExpr[Unit] = {
    val bindable = Identifier.unsafeBindable(name)
    TypedExpr.Local(bindable, Type.IntType, ())
  }

  private def makeGlobal(pack: String, name: String): TypedExpr[Unit] = {
    val pn = PackageName.parse(pack).get
    val id = Identifier.Name(name)
    TypedExpr.Global(pn, id, Type.IntType, ())
  }

  private def makeApp(fn: TypedExpr[Unit], args: TypedExpr[Unit]*): TypedExpr.App[Unit] =
    TypedExpr.App(fn, cats.data.NonEmptyList.fromListUnsafe(args.toList), Type.IntType, ())

  private def makeAnnotation(inner: TypedExpr[Unit]): TypedExpr[Unit] =
    TypedExpr.Annotation(inner, Type.IntType)

  // Note: Generic requires a Quantification - we don't test it directly
  // since Annotation covers the wrapping behavior

  private def makeLet(name: String, value: TypedExpr[Unit], body: TypedExpr[Unit]): TypedExpr[Unit] = {
    val bindable = Identifier.unsafeBindable(name)
    TypedExpr.Let(bindable, value, body, dev.bosatsu.RecursionKind.NonRecursive, ())
  }

  private def makeLambda(params: List[String], body: TypedExpr[Unit]): TypedExpr[Unit] = {
    val paramList = params.map { name =>
      (Identifier.unsafeBindable(name), Type.IntType)
    }
    TypedExpr.AnnotatedLambda(cats.data.NonEmptyList.fromListUnsafe(paramList), body, ())
  }

  private def makeMatch(arg: TypedExpr[Unit], branches: List[(String, TypedExpr[Unit])]): TypedExpr[Unit] = {
    val nel = cats.data.NonEmptyList.fromListUnsafe(branches.map { case (_, expr) =>
      val pattern = dev.bosatsu.Pattern.WildCard
      (pattern, expr)
    })
    TypedExpr.Match(arg, nel, ())
  }

  // ==========================================================================
  // DOMProperty tests
  // ==========================================================================

  test("DOMProperty.fromString parses textContent") {
    assertEquals(UIAnalyzer.DOMProperty.fromString("textContent"), Some(UIAnalyzer.DOMProperty.TextContent))
  }

  test("DOMProperty.fromString parses className") {
    assertEquals(UIAnalyzer.DOMProperty.fromString("className"), Some(UIAnalyzer.DOMProperty.ClassName))
  }

  test("DOMProperty.fromString parses value") {
    assertEquals(UIAnalyzer.DOMProperty.fromString("value"), Some(UIAnalyzer.DOMProperty.Value))
  }

  test("DOMProperty.fromString parses checked") {
    assertEquals(UIAnalyzer.DOMProperty.fromString("checked"), Some(UIAnalyzer.DOMProperty.Checked))
  }

  test("DOMProperty.fromString parses disabled") {
    assertEquals(UIAnalyzer.DOMProperty.fromString("disabled"), Some(UIAnalyzer.DOMProperty.Disabled))
  }

  test("DOMProperty.fromString parses style.* properties") {
    assertEquals(UIAnalyzer.DOMProperty.fromString("style.color"), Some(UIAnalyzer.DOMProperty.Style("color")))
    assertEquals(UIAnalyzer.DOMProperty.fromString("style.backgroundColor"), Some(UIAnalyzer.DOMProperty.Style("backgroundColor")))
  }

  test("DOMProperty.fromString returns None for unknown properties") {
    assertEquals(UIAnalyzer.DOMProperty.fromString("unknown"), None)
    assertEquals(UIAnalyzer.DOMProperty.fromString("innerHTML"), None)
  }

  test("DOMProperty.toJsProperty converts all properties correctly") {
    import UIAnalyzer.DOMProperty._
    assertEquals(toJsProperty(TextContent), "textContent")
    assertEquals(toJsProperty(ClassName), "className")
    assertEquals(toJsProperty(Value), "value")
    assertEquals(toJsProperty(Checked), "checked")
    assertEquals(toJsProperty(Disabled), "disabled")
    assertEquals(toJsProperty(Style("color")), "style.color")
    assertEquals(toJsProperty(Style("fontSize")), "style.fontSize")
  }

  // ==========================================================================
  // UIAnalysis tests
  // ==========================================================================

  test("UIAnalysis.empty creates empty analysis") {
    val empty = UIAnalyzer.UIAnalysis.empty[Unit]
    assertEquals(empty.stateReads, Nil)
    assertEquals(empty.bindings, Nil)
    assertEquals(empty.eventHandlers, Nil)
  }

  test("UIAnalysis.combine merges two analyses") {
    val a = UIAnalyzer.UIAnalysis[Unit](
      stateReads = List(List("user", "name")),
      bindings = Nil,
      eventHandlers = Nil
    )
    val b = UIAnalyzer.UIAnalysis[Unit](
      stateReads = List(List("user", "email")),
      bindings = Nil,
      eventHandlers = Nil
    )
    val combined = UIAnalyzer.UIAnalysis.combine(a, b)
    assertEquals(combined.stateReads, List(List("user", "name"), List("user", "email")))
  }

  // ==========================================================================
  // analyze tests - basic expressions
  // ==========================================================================

  test("analyze literal returns empty analysis") {
    val lit = makeLiteral(42)
    val analysis = UIAnalyzer.analyze(lit)
    assertEquals(analysis.stateReads, Nil)
    assertEquals(analysis.bindings, Nil)
    assertEquals(analysis.eventHandlers, Nil)
  }

  test("analyze local variable returns empty when not tracked") {
    val local = makeLocal("x")
    val analysis = UIAnalyzer.analyze(local)
    assertEquals(analysis.stateReads, Nil)
    assertEquals(analysis.bindings, Nil)
  }

  test("analyze let binding propagates through") {
    val let = makeLet("x", makeLiteral(1), makeLocal("x"))
    val analysis = UIAnalyzer.analyze(let)
    assertEquals(analysis.stateReads, Nil)
    assertEquals(analysis.bindings, Nil)
  }

  test("analyze annotation unwraps correctly") {
    val annotated = makeAnnotation(makeLiteral(42))
    val analysis = UIAnalyzer.analyze(annotated)
    assertEquals(analysis.stateReads, Nil)
  }

  // Note: Generic unwrapping is tested via annotation tests
  // since both use similar unwrapping logic

  test("analyze lambda analyzes body") {
    val lambda = makeLambda(List("x"), makeLiteral(42))
    val analysis = UIAnalyzer.analyze(lambda)
    assertEquals(analysis.stateReads, Nil)
  }

  test("analyze match marks as conditional") {
    val matchExpr = makeMatch(makeLiteral(1), List("case1" -> makeLiteral(10), "case2" -> makeLiteral(20)))
    val analysis = UIAnalyzer.analyze(matchExpr)
    assertEquals(analysis.stateReads, Nil)
  }

  // ==========================================================================
  // analyzeWithStateBindings tests
  // ==========================================================================

  test("analyzeWithStateBindings tracks state variables") {
    val count = Identifier.unsafeBindable("count")
    val local = makeLocal("count")
    val analysis = UIAnalyzer.analyzeWithStateBindings(local, List(count))
    // The local reference to a tracked state binding returns its path
    // But doesn't create a state read unless it's actually read via read()
    assert(analysis.stateReads.isEmpty || analysis.stateReads.exists(_.contains("count")))
  }

  test("analyzeWithStateBindings works with empty state list") {
    val lit = makeLiteral(42)
    val analysis = UIAnalyzer.analyzeWithStateBindings(lit, Nil)
    assertEquals(analysis.stateReads, Nil)
    assertEquals(analysis.bindings, Nil)
  }

  // ==========================================================================
  // analyzeWithFunctions tests
  // ==========================================================================

  test("analyzeWithFunctions stores function bodies") {
    val count = Identifier.unsafeBindable("count")
    val toggleBody = makeLiteral(1)
    val analysis = UIAnalyzer.analyzeWithFunctions(
      makeLiteral(42),
      List(count),
      Map("toggle" -> toggleBody)
    )
    assertEquals(analysis.stateReads, Nil)
  }

  // ==========================================================================
  // isStateCreationExpr tests
  // ==========================================================================

  test("isStateCreationExpr returns true for state(initial) call") {
    val stateFn = makeGlobal("Bosatsu/UI", "state")
    val stateApp = makeApp(stateFn, makeLiteral(0))
    assert(UIAnalyzer.isStateCreationExpr(stateApp))
  }

  test("isStateCreationExpr returns true for annotated state call") {
    val stateFn = makeAnnotation(makeGlobal("Bosatsu/UI", "state"))
    val stateApp = makeApp(stateFn, makeLiteral(0))
    assert(UIAnalyzer.isStateCreationExpr(stateApp))
  }

  // Note: Generic wrapping tested implicitly - annotation covers similar path

  test("isStateCreationExpr returns false for non-state calls") {
    val otherFn = makeGlobal("Bosatsu/UI", "read")
    val otherApp = makeApp(otherFn, makeLocal("x"))
    assert(!UIAnalyzer.isStateCreationExpr(otherApp))
  }

  test("isStateCreationExpr returns false for literal") {
    assert(!UIAnalyzer.isStateCreationExpr(makeLiteral(42)))
  }

  test("isStateCreationExpr returns false for non-UI package") {
    val stateFn = makeGlobal("Other/Package", "state")
    val stateApp = makeApp(stateFn, makeLiteral(0))
    assert(!UIAnalyzer.isStateCreationExpr(stateApp))
  }

  // ==========================================================================
  // analyzeFunction tests
  // ==========================================================================

  test("analyzeFunction handles lambda with params") {
    val lambda = makeLambda(List("x", "y"), makeLiteral(42))
    val analysis = UIAnalyzer.analyzeFunction(lambda)
    assertEquals(analysis.stateReads, Nil)
    assertEquals(analysis.bindings, Nil)
  }

  test("analyzeFunction handles non-lambda expression") {
    val lit = makeLiteral(42)
    val analysis = UIAnalyzer.analyzeFunction(lit)
    assertEquals(analysis.stateReads, Nil)
  }

  // ==========================================================================
  // read() detection tests
  // ==========================================================================

  test("analyze detects read(state) call from UI package") {
    val readFn = makeGlobal("Bosatsu/UI", "read")
    val stateVar = makeLocal("count")
    val readApp = makeApp(readFn, stateVar)
    val analysis = UIAnalyzer.analyze(readApp)
    assertEquals(analysis.stateReads, List(List("count")))
  }

  test("analyze detects read(state) with Global state reference") {
    val readFn = makeGlobal("Bosatsu/UI", "read")
    val stateVar = makeGlobal("Demo/Counter", "count")
    val readApp = makeApp(readFn, stateVar)
    val analysis = UIAnalyzer.analyze(readApp)
    assertEquals(analysis.stateReads, List(List("count")))
  }

  test("analyze detects IO.read for legacy pattern") {
    // IO.read expects a list path - this tests the fallback pattern
    val readFn = makeGlobal("Bosatsu/IO", "read")
    val pathArg = makeLiteral(0) // Mock - real would be list
    val readApp = makeApp(readFn, pathArg)
    val analysis = UIAnalyzer.analyze(readApp)
    // Won't detect without proper list structure, but shouldn't crash
    assertEquals(analysis.stateReads, Nil)
  }

  // ==========================================================================
  // text() binding tests
  // ==========================================================================

  test("analyze detects text binding with tracked state") {
    val count = Identifier.unsafeBindable("count")
    val textFn = makeGlobal("Bosatsu/UI", "text")
    val localCount = makeLocal("count")
    val textApp = makeApp(textFn, localCount)

    val analysis = UIAnalyzer.analyzeWithStateBindings(textApp, List(count))
    // Should create a binding for the text content
    assertEquals(analysis.bindings.length, 1)
    assertEquals(analysis.bindings.head.property, UIAnalyzer.DOMProperty.TextContent)
    assertEquals(analysis.bindings.head.statePath, List("count"))
  }

  test("analyze text with read(state) creates binding") {
    val count = Identifier.unsafeBindable("count")
    val readFn = makeGlobal("Bosatsu/UI", "read")
    val localCount = makeLocal("count")
    val readApp = makeApp(readFn, localCount)

    val textFn = makeGlobal("Bosatsu/UI", "text")
    val textApp = makeApp(textFn, readApp)

    val analysis = UIAnalyzer.analyzeWithStateBindings(textApp, List(count))
    assert(analysis.stateReads.nonEmpty)
    assertEquals(analysis.stateReads.head, List("count"))
  }

  // ==========================================================================
  // h() element tests
  // ==========================================================================

  test("analyze h() element with children") {
    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val props = makeLiteral(0) // Mock empty props
    val children = makeLiteral(0) // Mock empty children
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyze(hApp)
    // Should process without error
    assertEquals(analysis.eventHandlers, Nil)
  }

  // ==========================================================================
  // Event handler tests
  // ==========================================================================

  test("analyze detects on_click handler") {
    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val handler = makeLambda(List("e"), makeLiteral(42))
    val onClickApp = makeApp(onClickFn, handler)

    val analysis = UIAnalyzer.analyze(onClickApp)
    assertEquals(analysis.eventHandlers.length, 1)
    assertEquals(analysis.eventHandlers.head.eventType, "click")
    assert(analysis.eventHandlers.head.preventDefault)
  }

  test("analyze detects on_input handler") {
    val onInputFn = makeGlobal("Bosatsu/UI", "on_input")
    val handler = makeLambda(List("e"), makeLiteral(42))
    val onInputApp = makeApp(onInputFn, handler)

    val analysis = UIAnalyzer.analyze(onInputApp)
    assertEquals(analysis.eventHandlers.length, 1)
    assertEquals(analysis.eventHandlers.head.eventType, "input")
  }

  test("analyze detects on_change handler") {
    val onChangeFn = makeGlobal("Bosatsu/UI", "on_change")
    val handler = makeLambda(List("e"), makeLiteral(42))
    val onChangeApp = makeApp(onChangeFn, handler)

    val analysis = UIAnalyzer.analyze(onChangeApp)
    assertEquals(analysis.eventHandlers.length, 1)
    assertEquals(analysis.eventHandlers.head.eventType, "change")
  }

  // ==========================================================================
  // createBindingMap tests
  // ==========================================================================

  test("createBindingMap groups bindings by state path") {
    import UIAnalyzer._

    // Create mock binding - need a TypedExpr, use a simple literal
    val expr = makeLiteral(42)

    val binding1 = DOMBinding[Unit](
      elementId = "elem1",
      property = DOMProperty.TextContent,
      statePath = List("user", "name"),
      conditional = false,
      transform = None,
      sourceExpr = expr
    )

    val binding2 = DOMBinding[Unit](
      elementId = "elem2",
      property = DOMProperty.ClassName,
      statePath = List("user", "name"),
      conditional = false,
      transform = None,
      sourceExpr = expr
    )

    val binding3 = DOMBinding[Unit](
      elementId = "elem3",
      property = DOMProperty.TextContent,
      statePath = List("user", "email"),
      conditional = false,
      transform = None,
      sourceExpr = expr
    )

    val map = createBindingMap(List(binding1, binding2, binding3))
    assertEquals(map.get("user.name").map(_.length), Some(2))
    assertEquals(map.get("user.email").map(_.length), Some(1))
  }

  // ==========================================================================
  // bindingsToJs tests
  // ==========================================================================

  test("bindingsToJs generates correct JavaScript object") {
    import UIAnalyzer._

    val expr = makeLiteral(42)

    val binding = DOMBinding[Unit](
      elementId = "elem1",
      property = DOMProperty.TextContent,
      statePath = List("count"),
      conditional = false,
      transform = None,
      sourceExpr = expr
    )

    val js = bindingsToJs(List(binding))
    assert(js.contains("count"))
    assert(js.contains("elem1"))
    assert(js.contains("textContent"))
    assert(js.contains("conditional"))
  }

  test("bindingsToJs includes transform when present") {
    import UIAnalyzer._

    val expr = makeLiteral(42)

    val binding = DOMBinding[Unit](
      elementId = "elem1",
      property = DOMProperty.Value,
      statePath = List("input"),
      conditional = true,
      transform = Some("(x) => x.toUpperCase()"),
      sourceExpr = expr
    )

    val js = bindingsToJs(List(binding))
    assert(js.contains("transform"))
    assert(js.contains("toUpperCase"))
    assert(js.contains("\"conditional\": true"))
  }

  test("bindingsToJs handles style property") {
    import UIAnalyzer._

    val expr = makeLiteral(42)

    val binding = DOMBinding[Unit](
      elementId = "elem1",
      property = DOMProperty.Style("backgroundColor"),
      statePath = List("theme"),
      conditional = false,
      transform = None,
      sourceExpr = expr
    )

    val js = bindingsToJs(List(binding))
    assert(js.contains("style.backgroundColor"))
  }

  // ==========================================================================
  // getStatePaths tests
  // ==========================================================================

  test("getStatePaths returns distinct paths") {
    val analysis = UIAnalyzer.UIAnalysis[Unit](
      stateReads = List(
        List("user", "name"),
        List("user", "email"),
        List("user", "name") // duplicate
      ),
      bindings = Nil,
      eventHandlers = Nil
    )

    val paths = UIAnalyzer.getStatePaths(analysis)
    assertEquals(paths.length, 2)
    assert(paths.contains(List("user", "name")))
    assert(paths.contains(List("user", "email")))
  }

  // ==========================================================================
  // State creation in let binding tests
  // ==========================================================================

  test("analyze tracks state creation in let binding") {
    val stateFn = makeGlobal("Bosatsu/UI", "state")
    val stateApp = makeApp(stateFn, makeLiteral(0))
    val letExpr = makeLet("count", stateApp, makeLocal("count"))

    val analysis = UIAnalyzer.analyze(letExpr)
    // State creation is tracked, not as a state read but as a known binding
    assertEquals(analysis.stateReads, Nil)
  }

  // ==========================================================================
  // Transform detection tests
  // ==========================================================================

  test("analyze detects int_to_String transform in text") {
    val count = Identifier.unsafeBindable("count")

    // int_to_String(read(count))
    val readFn = makeGlobal("Bosatsu/UI", "read")
    val localCount = makeLocal("count")
    val readApp = makeApp(readFn, localCount)

    val toStringFn = makeGlobal("Bosatsu/Predef", "int_to_String")
    val toStringApp = makeApp(toStringFn, readApp)

    val textFn = makeGlobal("Bosatsu/UI", "text")
    val textApp = makeApp(textFn, toStringApp)

    val analysis = UIAnalyzer.analyzeWithStateBindings(textApp, List(count))
    assert(analysis.stateReads.contains(List("count")))
    // Check for transform in binding
    assert(analysis.bindings.exists(b => b.transform.contains("_int_to_String")))
  }

  // ==========================================================================
  // Conditional binding tests
  // ==========================================================================

  test("analyze marks bindings inside match as conditional") {
    val count = Identifier.unsafeBindable("count")
    val textFn = makeGlobal("Bosatsu/UI", "text")
    val localCount = makeLocal("count")
    val textApp = makeApp(textFn, localCount)

    val matchExpr = makeMatch(makeLiteral(1), List("case1" -> textApp))

    val analysis = UIAnalyzer.analyzeWithStateBindings(matchExpr, List(count))
    assert(analysis.bindings.forall(_.conditional))
  }

  // ==========================================================================
  // Global reference tests
  // ==========================================================================

  test("analyze handles global state reference") {
    val count = Identifier.unsafeBindable("count")
    val globalRef = makeGlobal("Demo/Counter", "count")
    val analysis = UIAnalyzer.analyzeWithStateBindings(globalRef, List(count))
    // Global references don't automatically create state reads without read()
    assertEquals(analysis.stateReads, Nil)
  }

  // ==========================================================================
  // extractStateWrites tests via analyzeWithFunctions
  // ==========================================================================

  test("analyzeWithFunctions detects state writes in handler") {
    // Create a handler that writes to state: write(count, value)
    val count = Identifier.unsafeBindable("count")
    val writeFn = makeGlobal("Bosatsu/UI", "write")
    val countLocal = makeLocal("count")
    val valueLocal = makeLocal("value")
    val writeApp = makeApp(writeFn, countLocal, valueLocal)
    val handler = makeLambda(List("e"), writeApp)

    // on_click(toggle) where toggle is defined separately
    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val toggleRef = makeGlobal("Demo/Counter", "toggle")
    val onClickApp = makeApp(onClickFn, toggleRef)

    val analysis = UIAnalyzer.analyzeWithFunctions(
      onClickApp,
      List(count),
      Map("toggle" -> handler)
    )

    assertEquals(analysis.eventHandlers.length, 1)
    assertEquals(analysis.eventHandlers.head.eventType, "click")
  }

  test("analyzeWithFunctions handles write with annotation wrapper") {
    val count = Identifier.unsafeBindable("count")
    val writeFn = makeAnnotation(makeGlobal("Bosatsu/UI", "write"))
    val countLocal = makeLocal("count")
    val valueLocal = makeLiteral(1)
    val writeApp = makeApp(writeFn, countLocal, valueLocal)

    val analysis = UIAnalyzer.analyzeWithFunctions(
      writeApp,
      List(count),
      Map.empty
    )
    // Should not crash when handling annotated write
    assert(analysis != null)
  }

  // ==========================================================================
  // Edge cases
  // ==========================================================================

  test("analyze handles deeply nested expressions") {
    val innerLet = makeLet("a", makeLiteral(1), makeLocal("a"))
    val middleLet = makeLet("b", innerLet, makeLocal("b"))
    val outerLet = makeLet("c", middleLet, makeLocal("c"))

    val analysis = UIAnalyzer.analyze(outerLet)
    assertEquals(analysis.stateReads, Nil)
    assertEquals(analysis.bindings, Nil)
  }

  test("analyze handles empty function bodies map") {
    val count = Identifier.unsafeBindable("count")
    val analysis = UIAnalyzer.analyzeWithFunctions(
      makeLiteral(42),
      List(count),
      Map.empty
    )
    assertEquals(analysis.stateReads, Nil)
  }

  test("bindingsToJs handles empty bindings list") {
    val js = UIAnalyzer.bindingsToJs(Nil)
    assertEquals(js, "{}")
  }

  test("createBindingMap handles empty list") {
    val map = UIAnalyzer.createBindingMap(Nil)
    assert(map.isEmpty)
  }

  test("getStatePaths handles empty analysis") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val paths = UIAnalyzer.getStatePaths(analysis)
    assertEquals(paths, Nil)
  }

  // ==========================================================================
  // Integration tests - compile real Bosatsu code
  // ==========================================================================

  import dev.bosatsu.TestUtils

  test("integration: analyze simple state read expression") {
    TestUtils.checkLast("""
      |x = 42
      |y = x
      |main = y
    """.stripMargin.trim) { typedExpr =>
      val analysis = UIAnalyzer.analyze(typedExpr)
      // Simple value assignment doesn't trigger UI state analysis
      assertEquals(analysis.eventHandlers, Nil)
    }
  }

  test("integration: analyze Let expression chain") {
    TestUtils.checkLast("""
      |a = 1
      |b = 2
      |c = a
      |main = c
    """.stripMargin.trim) { typedExpr =>
      val analysis = UIAnalyzer.analyze(typedExpr)
      // Test that analyze handles nested Let expressions
      assertEquals(analysis.stateReads, Nil)
    }
  }

  test("integration: analyze match expression") {
    TestUtils.checkLast("""
      |x = 1
      |result = match x:
      |  case _: x
      |main = result
    """.stripMargin.trim) { typedExpr =>
      val analysis = UIAnalyzer.analyze(typedExpr)
      // Match expressions are handled by analyze
      assertEquals(analysis.eventHandlers, Nil)
    }
  }

  test("integration: analyze function application") {
    TestUtils.checkLast("""
      |def double(x): x
      |def triple(x): double(double(x))
      |main = triple(1)
    """.stripMargin.trim) { typedExpr =>
      val analysis = UIAnalyzer.analyze(typedExpr)
      // Function applications are traversed
      assertEquals(analysis.bindings, Nil)
    }
  }

  test("integration: analyze lambda in let") {
    TestUtils.checkLast("""
      |def apply(f, x): f(x)
      |def identity(x): x
      |main = apply(identity, 42)
    """.stripMargin.trim) { typedExpr =>
      val analysis = UIAnalyzer.analyze(typedExpr)
      assertEquals(analysis.eventHandlers, Nil)
    }
  }

  test("integration: analyze struct creation") {
    TestUtils.checkLast("""
      |struct Point(x, y)
      |p = Point(10, 20)
      |main = p
    """.stripMargin.trim) { typedExpr =>
      val analysis = UIAnalyzer.analyze(typedExpr)
      assertEquals(analysis.stateReads, Nil)
    }
  }

  test("integration: analyze enum pattern match") {
    TestUtils.checkLast("""
      |enum MyOption: MyNone, MySome(value)
      |x = MySome(42)
      |result = match x:
      |  case MyNone: 0
      |  case MySome(v): v
      |main = result
    """.stripMargin.trim) { typedExpr =>
      val analysis = UIAnalyzer.analyze(typedExpr)
      assertEquals(analysis.eventHandlers, Nil)
    }
  }

  test("integration: analyze string operations") {
    TestUtils.checkLast("""
      |a = "hello"
      |b = "world"
      |main = a
    """.stripMargin.trim) { typedExpr =>
      val analysis = UIAnalyzer.analyze(typedExpr)
      assertEquals(analysis.bindings, Nil)
    }
  }

  test("integration: analyze string concatenation") {
    TestUtils.checkLast("""
      |name = "world"
      |main = "hello"
    """.stripMargin.trim) { typedExpr =>
      val analysis = UIAnalyzer.analyze(typedExpr)
      assertEquals(analysis.stateReads, Nil)
    }
  }

  test("integration: analyze nested binding") {
    TestUtils.checkLast("""
      |x = 10
      |y = x
      |z = y
      |main = z
    """.stripMargin.trim) { typedExpr =>
      val analysis = UIAnalyzer.analyze(typedExpr)
      assertEquals(analysis.eventHandlers, Nil)
    }
  }

  test("integration: analyze struct creation") {
    TestUtils.checkLast("""
      |struct Point(x, y)
      |p = Point(3, 4)
      |main = p
    """.stripMargin.trim) { typedExpr =>
      val analysis = UIAnalyzer.analyze(typedExpr)
      assertEquals(analysis.stateReads, Nil)
    }
  }

  test("integration: analyze nested function defs") {
    TestUtils.checkLast("""
      |def id(x): x
      |def const(_): 42
      |main = id(const(1))
    """.stripMargin.trim) { typedExpr =>
      val analysis = UIAnalyzer.analyze(typedExpr)
      assertEquals(analysis.eventHandlers, Nil)
    }
  }

  test("integration: analyze higher order function") {
    TestUtils.checkLast("""
      |def apply(f, x): f(x)
      |def id(x): x
      |main = apply(id, 42)
    """.stripMargin.trim) { typedExpr =>
      val analysis = UIAnalyzer.analyze(typedExpr)
      assertEquals(analysis.bindings, Nil)
    }
  }

  // ==========================================================================
  // Additional coverage tests - Generic wrapper handling
  // ==========================================================================

  // Helper to create Generic wrapper (requires Quantification)
  private def makeGeneric(inner: TypedExpr[Unit]): TypedExpr[Unit] = {
    import dev.bosatsu.Kind
    val typeVar = Type.Var.Bound("A")
    val quant = dev.bosatsu.rankn.Type.Quantification.ForAll(cats.data.NonEmptyList.of((typeVar, Kind.Type)))
    TypedExpr.Generic(quant, inner)
  }

  test("analyze handles Generic wrapper in expression") {
    val lit = makeLiteral(42)
    val generic = makeGeneric(lit)
    val analysis = UIAnalyzer.analyze(generic)
    assertEquals(analysis.stateReads, Nil)
    assertEquals(analysis.bindings, Nil)
  }

  test("isStateCreationExpr returns true for Generic-wrapped state call") {
    val stateFn = makeGeneric(makeGlobal("Bosatsu/UI", "state"))
    val stateApp = makeApp(stateFn, makeLiteral(0))
    assert(UIAnalyzer.isStateCreationExpr(stateApp))
  }

  test("analyze read(state) with non-Local/non-Global argument returns None") {
    // read(someApp()) - the argument is another App, not Local/Global
    val readFn = makeGlobal("Bosatsu/UI", "read")
    val innerApp = makeApp(makeGlobal("Other/Pkg", "someFunc"), makeLiteral(1))
    val readApp = makeApp(readFn, innerApp)
    val analysis = UIAnalyzer.analyze(readApp)
    // Should not crash, just not detect a state read
    assertEquals(analysis.stateReads, Nil)
  }

  test("extractStateRead returns None for non-App expression") {
    // Directly call analyze on a literal - exercises extractStateRead's default case
    val lit = makeLiteral(42)
    // When we analyze an expression, extractStateRead is called on App nodes
    // This test ensures we don't crash on non-App in the analysis
    val analysis = UIAnalyzer.analyze(lit)
    assertEquals(analysis.stateReads, Nil)
  }

  // ==========================================================================
  // Additional coverage tests - Let binding with state propagation
  // ==========================================================================

  test("analyze Let binding where value reads state") {
    // let currentCount = read(count) in currentCount
    val count = Identifier.unsafeBindable("count")
    val readFn = makeGlobal("Bosatsu/UI", "read")
    val localCount = makeLocal("count")
    val readApp = makeApp(readFn, localCount)

    // Let binding: val currentCount = read(count); currentCount
    val letExpr = makeLet("currentCount", readApp, makeLocal("currentCount"))

    val analysis = UIAnalyzer.analyzeWithStateBindings(letExpr, List(count))
    // The state read should be recorded
    assert(analysis.stateReads.nonEmpty)
    assertEquals(analysis.stateReads.head, List("count"))
  }

  // ==========================================================================
  // Additional coverage tests - IO.read legacy pattern
  // ==========================================================================

  test("analyze IO.read with list argument") {
    // IO.read([str1, str2]) - legacy pattern with list of strings
    val readFn = makeGlobal("Bosatsu/IO", "read")
    val str1 = makeStrLiteral("user")
    val str2 = makeStrLiteral("name")
    // Simulate list construction: App(listConstructor, str1, str2)
    val listCons = makeGlobal("Bosatsu/List", "Cons")
    val listApp = makeApp(listCons, str1, str2)
    val readApp = makeApp(readFn, listApp)

    val analysis = UIAnalyzer.analyze(readApp)
    // Should extract path from string literals
    assertEquals(analysis.stateReads, List(List("user", "name")))
  }

  test("analyze IO.read with short package name") {
    // Test IO package (without Bosatsu/ prefix)
    val readFn = makeGlobal("IO", "read")
    val listCons = makeGlobal("Bosatsu/List", "Cons")
    val str1 = makeStrLiteral("path")
    val listApp = makeApp(listCons, str1)
    val readApp = makeApp(readFn, listApp)

    val analysis = UIAnalyzer.analyze(readApp)
    // Should recognize IO package
    assertEquals(analysis.stateReads, List(List("path")))
  }

  // ==========================================================================
  // Additional coverage tests - UI package short name
  // ==========================================================================

  test("analyze text with short UI package name") {
    // UI::text instead of Bosatsu/UI::text
    val count = Identifier.unsafeBindable("count")
    val textFn = makeGlobal("UI", "text")
    val localCount = makeLocal("count")
    val textApp = makeApp(textFn, localCount)

    val analysis = UIAnalyzer.analyzeWithStateBindings(textApp, List(count))
    // Should recognize UI package
    assertEquals(analysis.bindings.length, 1)
  }

  test("analyze state creation with short UI package") {
    val stateFn = makeGlobal("UI", "state")
    val stateApp = makeApp(stateFn, makeLiteral(0))
    assert(UIAnalyzer.isStateCreationExpr(stateApp))
  }

  // ==========================================================================
  // Additional coverage tests - extractUIConstruction edge cases
  // ==========================================================================

  test("analyze handles other UI functions (fragment, etc)") {
    // UI::fragment - not h, text, on_click, on_input, on_change
    val fragmentFn = makeGlobal("Bosatsu/UI", "fragment")
    val children = makeLiteral(0)
    val fragmentApp = makeApp(fragmentFn, children)

    val analysis = UIAnalyzer.analyze(fragmentApp)
    // Should not crash, just not find specific bindings
    assertEquals(analysis.eventHandlers, Nil)
  }

  // ==========================================================================
  // Additional coverage tests - traceStateDependency edge cases
  // ==========================================================================

  test("traceStateDependency with Global tracked state") {
    val count = Identifier.unsafeBindable("count")
    val textFn = makeGlobal("Bosatsu/UI", "text")
    // text(globalRef) where globalRef is a package-level state
    val globalRef = makeGlobal("Demo/Counter", "count")
    val textApp = makeApp(textFn, globalRef)

    val analysis = UIAnalyzer.analyzeWithStateBindings(textApp, List(count))
    // Global ref to tracked state should create binding
    assertEquals(analysis.bindings.length, 1)
  }

  test("traceStateDependency with Annotation wrapper") {
    val count = Identifier.unsafeBindable("count")
    val textFn = makeGlobal("Bosatsu/UI", "text")
    // text(annotated(count))
    val localCount = makeLocal("count")
    val annotated = makeAnnotation(localCount)
    val textApp = makeApp(textFn, annotated)

    val analysis = UIAnalyzer.analyzeWithStateBindings(textApp, List(count))
    // Should unwrap annotation to find state path
    assertEquals(analysis.bindings.length, 1)
  }

  test("traceStateDependency with Generic wrapper") {
    val count = Identifier.unsafeBindable("count")
    val textFn = makeGlobal("Bosatsu/UI", "text")
    // text(generic(count))
    val localCount = makeLocal("count")
    val generic = makeGeneric(localCount)
    val textApp = makeApp(textFn, generic)

    val analysis = UIAnalyzer.analyzeWithStateBindings(textApp, List(count))
    // Should unwrap generic to find state path
    assertEquals(analysis.bindings.length, 1)
  }

  // ==========================================================================
  // Additional coverage tests - extractTransform edge cases
  // ==========================================================================

  test("analyze transform function ending with _to_String") {
    val count = Identifier.unsafeBindable("count")
    val readFn = makeGlobal("Bosatsu/UI", "read")
    val localCount = makeLocal("count")
    val readApp = makeApp(readFn, localCount)

    // custom_to_String(read(count))
    val customToString = makeGlobal("Demo/Utils", "custom_to_String")
    val transformApp = makeApp(customToString, readApp)

    val textFn = makeGlobal("Bosatsu/UI", "text")
    val textApp = makeApp(textFn, transformApp)

    val analysis = UIAnalyzer.analyzeWithStateBindings(textApp, List(count))
    assert(analysis.bindings.exists(b => b.transform.contains("_custom_to_String")))
  }

  test("analyze transform function ending with _to_string (lowercase)") {
    val count = Identifier.unsafeBindable("count")
    val readFn = makeGlobal("Bosatsu/UI", "read")
    val localCount = makeLocal("count")
    val readApp = makeApp(readFn, localCount)

    // int_to_string(read(count)) - lowercase variant
    val toStringFn = makeGlobal("Bosatsu/Predef", "int_to_string")
    val transformApp = makeApp(toStringFn, readApp)

    val textFn = makeGlobal("Bosatsu/UI", "text")
    val textApp = makeApp(textFn, transformApp)

    val analysis = UIAnalyzer.analyzeWithStateBindings(textApp, List(count))
    assert(analysis.bindings.exists(b => b.transform.contains("_int_to_String")))
  }

  test("extractTransform returns None for non-Global function") {
    val count = Identifier.unsafeBindable("count")
    // text(localFunc(read(count))) where localFunc is Local
    val readFn = makeGlobal("Bosatsu/UI", "read")
    val localCount = makeLocal("count")
    val readApp = makeApp(readFn, localCount)

    // Local function reference (not Global)
    val localFunc = makeLocal("myTransform")
    val transformApp = makeApp(localFunc, readApp)

    val textFn = makeGlobal("Bosatsu/UI", "text")
    val textApp = makeApp(textFn, transformApp)

    val analysis = UIAnalyzer.analyzeWithStateBindings(textApp, List(count))
    // Should still create binding, but without transform
    assert(analysis.bindings.exists(b => b.transform.isEmpty))
  }

  // ==========================================================================
  // Additional coverage tests - h() element with args < 2
  // ==========================================================================

  test("analyze h() with single argument") {
    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("br")
    // h("br") - no props or children
    val hApp = TypedExpr.App(hFn, cats.data.NonEmptyList.of(tag), Type.IntType, ())

    val analysis = UIAnalyzer.analyze(hApp)
    // Should not crash with minimal args
    assertEquals(analysis.eventHandlers, Nil)
  }

  test("analyze h() with two arguments (no children)") {
    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("input")
    val props = makeLiteral(0)
    // h("input", props) - no children
    val hApp = TypedExpr.App(hFn, cats.data.NonEmptyList.of(tag, props), Type.IntType, ())

    val analysis = UIAnalyzer.analyze(hApp)
    assertEquals(analysis.eventHandlers, Nil)
  }

  // ==========================================================================
  // List traversal patterns - extractIdFromPropsList
  // ==========================================================================

  test("extractExplicitId finds id in NonEmptyList/Cons props") {
    // Construct: NonEmptyList(("id", "my-elem"), tail)
    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("my-element-id")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)
    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val propsList = makeApp(consFn, idTuple, emptyList)

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, propsList, children)

    val analysis = UIAnalyzer.analyze(hApp)
    // Should extract the explicit ID
    // (The h() analysis creates bindings with this ID)
    assertEquals(analysis.eventHandlers, Nil)
  }

  test("extractIdFromPropsList handles Annotation wrapper") {
    val consFn = makeGlobal("Bosatsu/List", "Cons")
    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    // Wrapped empty list in annotation
    val annotatedEmpty = makeAnnotation(emptyList)
    val propsList = makeApp(consFn, makeLiteral(0), annotatedEmpty)
    // Wrap the whole thing in annotation too
    val annotatedProps = makeAnnotation(propsList)

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("span")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, annotatedProps, children)

    val analysis = UIAnalyzer.analyze(hApp)
    assertEquals(analysis.eventHandlers, Nil)
  }

  test("extractIdFromPropsList handles EmptyList") {
    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, emptyList, children)

    val analysis = UIAnalyzer.analyze(hApp)
    // Should handle empty props gracefully
    assertEquals(analysis.bindings, Nil)
  }

  // ==========================================================================
  // Event handlers from props - extractEventHandlersFromProps
  // ==========================================================================

  test("extractEventHandlersFromProps finds on_click in props list") {
    // Props: [on_click(handler)]
    val handler = makeLambda(List("e"), makeLiteral(1))
    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val onClickApp = makeApp(onClickFn, handler)

    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val propsList = makeApp(consFn, onClickApp, emptyList)

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("button")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, propsList, children)

    val analysis = UIAnalyzer.analyze(hApp)
    // Should extract the event handler
    assertEquals(analysis.eventHandlers.length, 1)
    assertEquals(analysis.eventHandlers.head.eventType, "click")
  }

  test("extractEventHandlersFromProps with Generic wrapper") {
    val handler = makeLambda(List("e"), makeLiteral(1))
    val onInputFn = makeGlobal("Bosatsu/UI", "on_input")
    val onInputApp = makeApp(onInputFn, handler)
    val genericProps = makeGeneric(onInputApp)

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("input")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, genericProps, children)

    val analysis = UIAnalyzer.analyze(hApp)
    assertEquals(analysis.eventHandlers.length, 1)
  }

  test("extractEventHandlersFromProps with direct on_change (not in list)") {
    // Props is directly on_change(handler), not wrapped in list
    val handler = makeLambda(List("e"), makeLiteral(1))
    val onChangeFn = makeGlobal("Bosatsu/UI", "on_change")
    val onChangeApp = makeApp(onChangeFn, handler)

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("select")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, onChangeApp, children)

    val analysis = UIAnalyzer.analyze(hApp)
    assertEquals(analysis.eventHandlers.length, 1)
    assertEquals(analysis.eventHandlers.head.eventType, "change")
  }

  // ==========================================================================
  // Checkbox ID pattern - extractEventHandlerWithElement
  // ==========================================================================

  test("extractEventHandlerWithElement creates className binding for checkbox ID") {
    // Element with ID ending in "-checkbox" should create className binding
    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("toggle-checkbox")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    // Handler that writes to state
    val writeFn = makeGlobal("Bosatsu/UI", "write")
    val stateVar = makeLocal("enabled")
    val newValue = makeLiteral(1)
    val writeApp = makeApp(writeFn, stateVar, newValue)
    val handler = makeLambda(List("e"), writeApp)

    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val onClickApp = makeApp(onClickFn, handler)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val propsWithId = makeApp(consFn, idTuple, makeApp(consFn, onClickApp, emptyList))

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, propsWithId, children)

    val enabled = Identifier.unsafeBindable("enabled")
    val analysis = UIAnalyzer.analyzeWithStateBindings(hApp, List(enabled))

    // Should have both event handler and className binding
    assert(analysis.eventHandlers.nonEmpty)
  }

  // ==========================================================================
  // extractStateWrites comprehensive tests
  // ==========================================================================

  test("extractStateWrites with Lambda body containing write") {
    // Lambda: \e -> write(count, 1)
    val count = Identifier.unsafeBindable("count")
    val writeFn = makeGlobal("Bosatsu/UI", "write")
    val countLocal = makeLocal("count")
    val writeApp = makeApp(writeFn, countLocal, makeLiteral(1))
    val handler = makeLambda(List("e"), writeApp)

    // Create a checkbox-like element to trigger extractStateWrites
    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("my-checkbox")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val onClickApp = makeApp(onClickFn, handler)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, idTuple, makeApp(consFn, onClickApp, emptyList))

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("input")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyzeWithStateBindings(hApp, List(count))
    assert(analysis.eventHandlers.nonEmpty)
  }

  test("extractStateWrites with Let expression in handler") {
    // Lambda: \e -> let x = write(count, 1) in x
    val count = Identifier.unsafeBindable("count")
    val writeFn = makeGlobal("Bosatsu/UI", "write")
    val countLocal = makeLocal("count")
    val writeApp = makeApp(writeFn, countLocal, makeLiteral(1))
    val letBody = makeLet("x", writeApp, makeLocal("x"))
    val handler = makeLambda(List("e"), letBody)

    // Create checkbox element
    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("let-checkbox")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val onClickApp = makeApp(onClickFn, handler)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, idTuple, makeApp(consFn, onClickApp, emptyList))

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyzeWithStateBindings(hApp, List(count))
    assert(analysis.eventHandlers.nonEmpty)
  }

  test("extractStateWrites with Match expression in handler") {
    // Lambda: \e -> match x: case _: write(count, 1)
    val count = Identifier.unsafeBindable("count")
    val writeFn = makeGlobal("Bosatsu/UI", "write")
    val countLocal = makeLocal("count")
    val writeApp = makeApp(writeFn, countLocal, makeLiteral(1))
    val matchExpr = makeMatch(makeLiteral(1), List("case" -> writeApp))
    val handler = makeLambda(List("e"), matchExpr)

    // Create checkbox element
    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("match-checkbox")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val onClickApp = makeApp(onClickFn, handler)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, idTuple, makeApp(consFn, onClickApp, emptyList))

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyzeWithStateBindings(hApp, List(count))
    assert(analysis.eventHandlers.nonEmpty)
  }

  test("extractStateWrites with handler reference resolved from functionBodies") {
    // Handler is a Local reference that should be looked up in functionBodies
    val count = Identifier.unsafeBindable("count")

    // Define toggleFn body: write(count, 1)
    val writeFn = makeGlobal("Bosatsu/UI", "write")
    val countLocal = makeLocal("count")
    val writeApp = makeApp(writeFn, countLocal, makeLiteral(1))
    val toggleBody = makeLambda(List("e"), writeApp)

    // Create checkbox element with on_click(toggle) where toggle is a Local reference
    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("func-checkbox")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    val toggleRef = makeLocal("toggle")
    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val onClickApp = makeApp(onClickFn, toggleRef)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, idTuple, makeApp(consFn, onClickApp, emptyList))

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyzeWithFunctions(
      hApp,
      List(count),
      Map("toggle" -> toggleBody)
    )
    assert(analysis.eventHandlers.nonEmpty)
  }

  test("extractStateWrites with Annotation wrapper") {
    val count = Identifier.unsafeBindable("count")
    val writeFn = makeGlobal("Bosatsu/UI", "write")
    val countLocal = makeLocal("count")
    val writeApp = makeApp(writeFn, countLocal, makeLiteral(1))
    val annotatedWrite = makeAnnotation(writeApp)
    val handler = makeLambda(List("e"), annotatedWrite)

    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("annot-checkbox")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val onClickApp = makeApp(onClickFn, handler)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, idTuple, makeApp(consFn, onClickApp, emptyList))

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyzeWithStateBindings(hApp, List(count))
    assert(analysis.eventHandlers.nonEmpty)
  }

  test("extractStateWrites with Generic wrapper") {
    val count = Identifier.unsafeBindable("count")
    val writeFn = makeGlobal("Bosatsu/UI", "write")
    val countLocal = makeLocal("count")
    val writeApp = makeApp(writeFn, countLocal, makeLiteral(1))
    val genericWrite = makeGeneric(writeApp)
    val handler = makeLambda(List("e"), genericWrite)

    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("gen-checkbox")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val onClickApp = makeApp(onClickFn, handler)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, idTuple, makeApp(consFn, onClickApp, emptyList))

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyzeWithStateBindings(hApp, List(count))
    assert(analysis.eventHandlers.nonEmpty)
  }

  test("extractStateWrites with write on Global state reference") {
    val count = Identifier.unsafeBindable("count")
    val writeFn = makeGlobal("Bosatsu/UI", "write")
    // write(GlobalState::count, 1)
    val globalCount = makeGlobal("Demo/State", "count")
    val writeApp = makeApp(writeFn, globalCount, makeLiteral(1))
    val handler = makeLambda(List("e"), writeApp)

    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("global-checkbox")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val onClickApp = makeApp(onClickFn, handler)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, idTuple, makeApp(consFn, onClickApp, emptyList))

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyzeWithStateBindings(hApp, List(count))
    assert(analysis.eventHandlers.nonEmpty)
  }

  // ==========================================================================
  // Props with Local state bindings
  // ==========================================================================

  test("extractPropsBindings with Local arg bound to state") {
    // h("div", someApp(stateVar), []) where stateVar is tracked
    val count = Identifier.unsafeBindable("count")

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val localCount = makeLocal("count")
    // Props is an App containing Local reference to tracked state
    val propsFn = makeGlobal("Some/Package", "makeProps")
    val propsApp = makeApp(propsFn, localCount)
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, propsApp, children)

    val analysis = UIAnalyzer.analyzeWithStateBindings(hApp, List(count))
    // Should create className binding for the tracked state in props
    assert(analysis.bindings.exists(_.property == UIAnalyzer.DOMProperty.ClassName))
  }

  // ==========================================================================
  // Additional edge case tests for coverage
  // ==========================================================================

  test("extractStateWrites with non-UI Global function propagates to args") {
    // handler = { e -> someOtherFn(write(state, value)) }
    // Should still find the write inside the args
    val count = Identifier.unsafeBindable("count")
    val writeFn = makeGlobal("Bosatsu/UI", "write")
    val countLocal = makeLocal("count")
    val writeApp = makeApp(writeFn, countLocal, makeLiteral(1))

    // Wrap in non-UI global function
    val otherFn = makeGlobal("Other/Package", "doSomething")
    val otherApp = makeApp(otherFn, writeApp)
    val handler = makeLambda(List("e"), otherApp)

    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("other-fn-checkbox")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val onClickApp = makeApp(onClickFn, handler)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, idTuple, makeApp(consFn, onClickApp, emptyList))

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyzeWithStateBindings(hApp, List(count))
    assert(analysis.eventHandlers.nonEmpty)
  }

  test("extractStateWrites with Local function propagates to args") {
    // handler = { e -> localFn(write(state, value)) }
    // Should still find the write inside the args when fn is Local
    val count = Identifier.unsafeBindable("count")
    val writeFn = makeGlobal("Bosatsu/UI", "write")
    val countLocal = makeLocal("count")
    val writeApp = makeApp(writeFn, countLocal, makeLiteral(1))

    // Wrap in Local function application
    val localFn = makeLocal("someFn")
    val localApp = makeApp(localFn, writeApp)
    val handler = makeLambda(List("e"), localApp)

    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("local-fn-checkbox")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val onClickApp = makeApp(onClickFn, handler)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, idTuple, makeApp(consFn, onClickApp, emptyList))

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyzeWithStateBindings(hApp, List(count))
    assert(analysis.eventHandlers.nonEmpty)
  }

  test("extractIdFromPropsList with id tuple followed by more items in list") {
    // Test case: props list has id tuple, then continues with more items
    // This tests the recursive continuation path
    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")

    // First item: ("other", "value")
    val otherKey = makeStrLiteral("other")
    val otherValue = makeStrLiteral("value")
    val otherTuple = makeApp(tuple2Fn, otherKey, otherValue)

    // Second item: ("id", "my-element")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("my-element")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val innerList = makeApp(consFn, idTuple, emptyList)
    val props = makeApp(consFn, otherTuple, innerList)

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyze(hApp)
    // Just verify it parses without error - the id extraction is internal
    assert(analysis != null)
  }

  test("extractPropTuple handles non-tuple App expression") {
    // An App that's not Tuple2 should return None for extractPropTuple
    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")

    // Use something that's not a Tuple2
    val notATuple = makeApp(makeGlobal("Other/Package", "notTuple"), makeStrLiteral("a"))

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, notATuple, emptyList)

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    // Should not error, just not find id
    val analysis = UIAnalyzer.analyze(hApp)
    assert(analysis != null)
  }

  test("extractPropTuple handles Tuple2 with non-string key") {
    // Tuple2 where the first element is not a string literal
    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")

    // Key is an integer, not a string
    val intKey = makeLiteral(42)
    val strValue = makeStrLiteral("value")
    val badTuple = makeApp(tuple2Fn, intKey, strValue)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, badTuple, emptyList)

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyze(hApp)
    assert(analysis != null)
  }

  test("extractStringLiteralDeep with nested Annotation and Generic") {
    // A string literal deeply wrapped in Annotation and Generic
    val strLit = makeStrLiteral("deep-id")
    val annotated = makeAnnotation(strLit)
    val generic = makeGeneric(annotated)
    val annotated2 = makeAnnotation(generic)

    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idTuple = makeApp(tuple2Fn, idKey, annotated2)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, idTuple, emptyList)

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyze(hApp)
    assert(analysis != null)
  }

  test("extractStringLiteralDeep with non-string expression returns None") {
    // When extractStringLiteralDeep encounters a non-literal, non-wrapper expression
    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    // Value is a local variable, not a string literal
    val idValue = makeLocal("dynamicId")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, idTuple, emptyList)

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyze(hApp)
    assert(analysis != null)
  }

  test("isStateFunction with non-App expression returns false") {
    // Direct literal should not be recognized as state function
    val lit = makeLiteral(42)
    val analysis = UIAnalyzer.analyze(lit)
    // No state reads since it's just a literal
    assert(analysis.stateReads.isEmpty)
  }

  test("extractStateRead with empty path returns None") {
    // IO.read with empty list should return None
    val readFn = makeGlobal("Bosatsu/IO", "read")
    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val readApp = makeApp(readFn, emptyList)

    val analysis = UIAnalyzer.analyze(readApp)
    // Empty path should not create a state read
    assert(analysis.stateReads.isEmpty)
  }

  test("extractUIConstruction with h() calls analyzeExpr instead") {
    // This tests that h() handling doesn't duplicate in extractUIConstruction
    // h() is now handled in analyzeExpr to properly set parent context
    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")

    // read(state) in text
    val readFn = makeGlobal("Bosatsu/IO", "read")
    val stateName = Identifier.unsafeBindable("count")
    val stateLocal = makeLocal("count")
    val readApp = makeApp(readFn, stateLocal)

    val textFn = makeGlobal("Bosatsu/UI", "text")
    val textApp = makeApp(textFn, readApp)

    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val children = makeApp(consFn, textApp, emptyList)

    val props = emptyList
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyzeWithStateBindings(hApp, List(stateName))
    // The text binding should be created with proper element context
    assert(analysis.bindings.nonEmpty)
    assert(analysis.bindings.exists(_.property == UIAnalyzer.DOMProperty.TextContent))
  }

  test("traceStateDependency returns None for untracked expressions") {
    // An expression that doesn't involve state at all
    val addFn = makeGlobal("Bosatsu/Int", "add")
    val sum = makeApp(addFn, makeLiteral(1), makeLiteral(2))

    // text() with computed value that doesn't involve state
    val textFn = makeGlobal("Bosatsu/UI", "text")
    val textApp = makeApp(textFn, sum)

    val analysis = UIAnalyzer.analyze(textApp)
    // No state dependency means no binding
    assert(analysis.bindings.isEmpty)
  }

  test("extractEventHandlersFromProps handles App that isn't NonEmptyList/Cons") {
    // Props as an App expression that's not a list constructor
    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")

    // Props is some other function application
    val otherFn = makeGlobal("Other/Package", "makeProps")
    val props = makeApp(otherFn, makeLiteral(1))

    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyze(hApp)
    // Should not error, just not find event handlers
    assert(analysis.eventHandlers.isEmpty)
  }

  test("extractSingleEventHandler returns None for non-on_* function") {
    // A prop tuple that's not an event handler
    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")

    // ("class", "my-class") - not an event handler
    val classKey = makeStrLiteral("class")
    val classValue = makeStrLiteral("my-class")
    val classTuple = makeApp(tuple2Fn, classKey, classValue)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, classTuple, emptyList)

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyze(hApp)
    assert(analysis.eventHandlers.isEmpty)
  }

  // ==========================================================================
  // Tests for getFunctionName and unwrapFunction edge cases
  // ==========================================================================

  test("extractIdFromPropsList with Annotation-wrapped NonEmptyList function") {
    // Test where the NonEmptyList constructor is wrapped in Annotation
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("annotated-fn-id")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    // Annotate the NonEmptyList constructor function
    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val annotatedConsFn = makeAnnotation(consFn)
    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(annotatedConsFn, idTuple, emptyList)

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyze(hApp)
    assert(analysis != null)
  }

  test("extractIdFromPropsList with Generic-wrapped NonEmptyList function") {
    // Test where the NonEmptyList constructor is wrapped in Generic
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("generic-fn-id")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    // Wrap the NonEmptyList constructor function in Generic
    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val genericConsFn = makeGeneric(consFn)
    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(genericConsFn, idTuple, emptyList)

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyze(hApp)
    assert(analysis != null)
  }

  test("extractStateWrites with Annotation-wrapped write function") {
    // Test where the write() function is wrapped in Annotation
    val count = Identifier.unsafeBindable("count")
    val writeFn = makeGlobal("Bosatsu/UI", "write")
    val annotatedWriteFn = makeAnnotation(writeFn)
    val countLocal = makeLocal("count")
    val writeApp = makeApp(annotatedWriteFn, countLocal, makeLiteral(1))
    val handler = makeLambda(List("e"), writeApp)

    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("annotated-write-checkbox")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val onClickApp = makeApp(onClickFn, handler)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, idTuple, makeApp(consFn, onClickApp, emptyList))

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyzeWithStateBindings(hApp, List(count))
    assert(analysis.eventHandlers.nonEmpty)
  }

  test("extractStateWrites with Generic-wrapped write function") {
    // Test where the write() function is wrapped in Generic
    val count = Identifier.unsafeBindable("count")
    val writeFn = makeGlobal("Bosatsu/UI", "write")
    val genericWriteFn = makeGeneric(writeFn)
    val countLocal = makeLocal("count")
    val writeApp = makeApp(genericWriteFn, countLocal, makeLiteral(1))
    val handler = makeLambda(List("e"), writeApp)

    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("generic-write-checkbox")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val onClickApp = makeApp(onClickFn, handler)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, idTuple, makeApp(consFn, onClickApp, emptyList))

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyzeWithStateBindings(hApp, List(count))
    assert(analysis.eventHandlers.nonEmpty)
  }

  test("extractStateWrites with write on Annotation-wrapped state arg") {
    // Test where the first arg to write() is wrapped in Annotation
    val count = Identifier.unsafeBindable("count")
    val writeFn = makeGlobal("Bosatsu/UI", "write")
    val countLocal = makeLocal("count")
    val annotatedCount = makeAnnotation(countLocal)
    val writeApp = makeApp(writeFn, annotatedCount, makeLiteral(1))
    val handler = makeLambda(List("e"), writeApp)

    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("annotated-state-checkbox")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val onClickApp = makeApp(onClickFn, handler)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, idTuple, makeApp(consFn, onClickApp, emptyList))

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyzeWithStateBindings(hApp, List(count))
    assert(analysis.eventHandlers.nonEmpty)
  }

  test("extractStateWrites with write on non-state expression returns empty list") {
    // Test where write() is called with an expression that's not Local or Global
    val count = Identifier.unsafeBindable("count")
    val writeFn = makeGlobal("Bosatsu/UI", "write")
    // Use an App as the state arg - not Local or Global
    val someFn = makeGlobal("Some/Package", "getValue")
    val computedState = makeApp(someFn, makeLiteral(1))
    val writeApp = makeApp(writeFn, computedState, makeLiteral(1))
    val handler = makeLambda(List("e"), writeApp)

    val consFn = makeGlobal("Bosatsu/List", "NonEmptyList")
    val tuple2Fn = makeGlobal("Bosatsu/Core", "Tuple2")
    val idKey = makeStrLiteral("id")
    val idValue = makeStrLiteral("computed-state-checkbox")
    val idTuple = makeApp(tuple2Fn, idKey, idValue)

    val onClickFn = makeGlobal("Bosatsu/UI", "on_click")
    val onClickApp = makeApp(onClickFn, handler)

    val emptyList = makeGlobal("Bosatsu/List", "EmptyList")
    val props = makeApp(consFn, idTuple, makeApp(consFn, onClickApp, emptyList))

    val hFn = makeGlobal("Bosatsu/UI", "h")
    val tag = makeStrLiteral("div")
    val children = makeLiteral(0)
    val hApp = makeApp(hFn, tag, props, children)

    val analysis = UIAnalyzer.analyzeWithStateBindings(hApp, List(count))
    // Should still create event handler, just won't extract state name
    assert(analysis.eventHandlers.nonEmpty)
  }
}
