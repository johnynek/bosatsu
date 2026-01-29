package dev.bosatsu.ui

import munit.FunSuite

class UIAnalyzerTest extends FunSuite {

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
  // createBindingMap tests
  // ==========================================================================

  test("createBindingMap groups bindings by state path") {
    import UIAnalyzer._

    // Create mock binding - need a TypedExpr, use a simple literal
    val expr = dev.bosatsu.TypedExpr.Literal(
      dev.bosatsu.Lit.Integer(42),
      dev.bosatsu.rankn.Type.IntType,
      ()
    )

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

    val expr = dev.bosatsu.TypedExpr.Literal(
      dev.bosatsu.Lit.Integer(42),
      dev.bosatsu.rankn.Type.IntType,
      ()
    )

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

    val expr = dev.bosatsu.TypedExpr.Literal(
      dev.bosatsu.Lit.Integer(42),
      dev.bosatsu.rankn.Type.IntType,
      ()
    )

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
}
