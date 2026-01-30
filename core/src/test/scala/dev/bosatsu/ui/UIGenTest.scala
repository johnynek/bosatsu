package dev.bosatsu.ui

import munit.FunSuite
import dev.bosatsu.{TypedExpr, Lit}
import dev.bosatsu.rankn.Type

class UIGenTest extends FunSuite {

  // Helper to create mock expressions
  private def makeLiteral(i: Int): TypedExpr[Unit] =
    TypedExpr.Literal(Lit.Integer(i), Type.IntType, ())

  // ==========================================================================
  // UIConfig tests
  // ==========================================================================

  test("UIConfig has sensible defaults") {
    val config = UIGen.UIConfig(title = "Test")
    assertEquals(config.title, "Test")
    assertEquals(config.theme, "light")
    assertEquals(config.includeSourceMap, false)
  }

  test("UIConfig accepts custom theme") {
    val config = UIGen.UIConfig(title = "Test", theme = "dark")
    assertEquals(config.theme, "dark")
  }

  test("UIConfig accepts includeSourceMap option") {
    val config = UIGen.UIConfig(title = "Test", includeSourceMap = true)
    assert(config.includeSourceMap)
  }

  // ==========================================================================
  // generate tests
  // ==========================================================================

  test("generate produces valid HTML structure") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test App")
    val vnodeJs = "const main = { type: 'element', tag: 'div' };"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("<!DOCTYPE html>"))
    assert(html.contains("<html lang=\"en\">"))
    assert(html.contains("<head>"))
    assert(html.contains("</head>"))
    assert(html.contains("<body>"))
    assert(html.contains("</body>"))
    assert(html.contains("</html>"))
  }

  test("generate includes title in head") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "My Test Title")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("<title>My Test Title</title>"))
  }

  test("generate escapes HTML in title") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "<script>alert('xss')</script>")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(!html.contains("<script>alert('xss')</script>"))
    assert(html.contains("&lt;script&gt;"))
  }

  test("generate includes style block") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("<style>"))
    assert(html.contains("</style>"))
  }

  test("generate includes app container div") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("<div id=\"app\"></div>"))
  }

  test("generate includes script block") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("<script>"))
    assert(html.contains("</script>"))
  }

  test("generate includes bindings map") {
    val expr = makeLiteral(42)
    val binding = UIAnalyzer.DOMBinding[Unit](
      elementId = "count-display",
      property = UIAnalyzer.DOMProperty.TextContent,
      statePath = List("count"),
      conditional = false,
      transform = None,
      sourceExpr = expr
    )
    val analysis = UIAnalyzer.UIAnalysis[Unit](
      stateReads = List(List("count")),
      bindings = List(binding),
      eventHandlers = Nil
    )
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("const _bindings ="))
    assert(html.contains("count"))
    assert(html.contains("count-display"))
    assert(html.contains("textContent"))
  }

  test("generate includes runtime code") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    // Runtime functions
    assert(html.contains("function read"))
    assert(html.contains("function write"))
    assert(html.contains("function _updateBindings"))
    assert(html.contains("function _renderVNode"))
  }

  test("generate includes event handler registration") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("_registerHandler"))
    assert(html.contains("_initEventHandlers"))
  }

  test("generate includes DOMContentLoaded listener") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("DOMContentLoaded"))
    assert(html.contains("init"))
  }

  test("generate includes vnode execution") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const myVNode = { type: 'text', text: 'hello' };"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("const _vnode = (function()"))
    assert(html.contains("return main"))
  }

  // ==========================================================================
  // generateCounterDemo tests
  // ==========================================================================

  test("generateCounterDemo produces valid HTML") {
    val config = UIGen.UIConfig(title = "Counter")

    val html = UIGen.generateCounterDemo(config)

    assert(html.contains("<!DOCTYPE html>"))
    assert(html.contains("<title>Counter</title>"))
  }

  test("generateCounterDemo includes counter elements") {
    val config = UIGen.UIConfig(title = "Counter")

    val html = UIGen.generateCounterDemo(config)

    assert(html.contains("id=\"count-display\""))
    assert(html.contains("id=\"increment-btn\""))
    assert(html.contains("id=\"decrement-btn\""))
  }

  test("generateCounterDemo includes state management") {
    val config = UIGen.UIConfig(title = "Counter")

    val html = UIGen.generateCounterDemo(config)

    assert(html.contains("const _state = { count: 0 }"))
    assert(html.contains("function read(path)"))
    assert(html.contains("function write(path, value)"))
  }

  test("generateCounterDemo includes bindings") {
    val config = UIGen.UIConfig(title = "Counter")

    val html = UIGen.generateCounterDemo(config)

    assert(html.contains("const _bindings"))
    assert(html.contains("\"count\""))
    assert(html.contains("elementId"))
    assert(html.contains("textContent"))
  }

  test("generateCounterDemo includes increment and decrement functions") {
    val config = UIGen.UIConfig(title = "Counter")

    val html = UIGen.generateCounterDemo(config)

    assert(html.contains("function increment()"))
    assert(html.contains("function decrement()"))
    assert(html.contains("read(\"count\") + 1"))
    assert(html.contains("read(\"count\") - 1"))
  }

  // ==========================================================================
  // Theme tests
  // ==========================================================================

  test("generate applies light theme styles") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test", theme = "light")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    // Light theme colors
    assert(html.contains("#f0f4f8")) // light background
    assert(html.contains("#ffffff")) // white card
  }

  test("generate applies dark theme styles") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test", theme = "dark")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    // Dark theme colors
    assert(html.contains("#1a1a2e")) // dark background
    assert(html.contains("#16213e")) // dark card
  }

  test("generateCounterDemo applies light theme") {
    val config = UIGen.UIConfig(title = "Counter", theme = "light")

    val html = UIGen.generateCounterDemo(config)

    assert(html.contains("#f0f4f8"))
  }

  test("generateCounterDemo applies dark theme") {
    val config = UIGen.UIConfig(title = "Counter", theme = "dark")

    val html = UIGen.generateCounterDemo(config)

    assert(html.contains("#1a1a2e"))
  }

  // ==========================================================================
  // Event handler generation tests
  // ==========================================================================

  test("generate creates event handler for click events") {
    val expr = makeLiteral(42)
    val handler = UIAnalyzer.EventBinding[Unit](
      elementId = "my-button",
      eventType = "click",
      handler = expr,
      preventDefault = true,
      stopPropagation = false
    )
    val analysis = UIAnalyzer.UIAnalysis[Unit](
      stateReads = Nil,
      bindings = Nil,
      eventHandlers = List(handler)
    )
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("_registerHandler"))
    assert(html.contains("\"my-button\""))
    assert(html.contains("\"click\""))
  }

  test("generate creates event handler for input events") {
    val expr = makeLiteral(42)
    val handler = UIAnalyzer.EventBinding[Unit](
      elementId = "my-input",
      eventType = "input",
      handler = expr,
      preventDefault = false,
      stopPropagation = false
    )
    val analysis = UIAnalyzer.UIAnalysis[Unit](
      stateReads = Nil,
      bindings = Nil,
      eventHandlers = List(handler)
    )
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("\"my-input\""))
    assert(html.contains("\"input\""))
  }

  // ==========================================================================
  // Runtime VNode rendering tests
  // ==========================================================================

  test("generate includes _renderVNode function for text nodes") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("case \"text\""))
    assert(html.contains("createTextNode"))
  }

  test("generate includes _renderVNode function for element nodes") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("case \"element\""))
    assert(html.contains("createElement"))
  }

  test("generate includes _renderVNode function for fragment nodes") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("case \"fragment\""))
    assert(html.contains("createDocumentFragment"))
  }

  // ==========================================================================
  // HTML escaping tests
  // ==========================================================================

  test("HTML escaping handles ampersand") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test & Demo")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("Test &amp; Demo"))
  }

  test("HTML escaping handles quotes") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test \"Demo\"")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("Test &quot;Demo&quot;"))
  }

  test("HTML escaping handles apostrophe") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "It's a test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("It&#39;s a test"))
  }

  // ==========================================================================
  // Multiple bindings tests
  // ==========================================================================

  test("generate handles multiple bindings for same state") {
    val expr = makeLiteral(42)
    val binding1 = UIAnalyzer.DOMBinding[Unit](
      elementId = "display1",
      property = UIAnalyzer.DOMProperty.TextContent,
      statePath = List("count"),
      conditional = false,
      transform = None,
      sourceExpr = expr
    )
    val binding2 = UIAnalyzer.DOMBinding[Unit](
      elementId = "display2",
      property = UIAnalyzer.DOMProperty.ClassName,
      statePath = List("count"),
      conditional = false,
      transform = None,
      sourceExpr = expr
    )
    val analysis = UIAnalyzer.UIAnalysis[Unit](
      stateReads = List(List("count")),
      bindings = List(binding1, binding2),
      eventHandlers = Nil
    )
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("display1"))
    assert(html.contains("display2"))
    assert(html.contains("textContent"))
    assert(html.contains("className"))
  }

  // ==========================================================================
  // CSS tests
  // ==========================================================================

  test("generate includes CSS reset") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("margin: 0"))
    assert(html.contains("padding: 0"))
    assert(html.contains("box-sizing: border-box"))
  }

  test("generate includes responsive styles") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("max-width"))
    assert(html.contains("display: flex"))
  }

  test("generate includes button styles") {
    val analysis = UIAnalyzer.UIAnalysis.empty[Unit]
    val config = UIGen.UIConfig(title = "Test")
    val vnodeJs = "const main = {};"

    val html = UIGen.generate(vnodeJs, analysis, config)

    assert(html.contains("button"))
    assert(html.contains("cursor: pointer"))
    assert(html.contains("button:hover"))
    assert(html.contains("button:active"))
  }
}
