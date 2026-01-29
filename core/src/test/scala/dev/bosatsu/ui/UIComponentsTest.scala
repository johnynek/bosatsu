package dev.bosatsu.ui

import munit.FunSuite
import org.scalacheck.Prop.forAll
import munit.ScalaCheckSuite
import SimulationApplet.{Derivation, Assumption, Computed, Conditional, derive, assume => mkAssume, explainDerivation, getAssumptions, simulationState, MutableSimulationState}

/**
 * Comprehensive tests for UI component code generation.
 * These tests ensure the JS/HTML generators produce valid output.
 */
class UIComponentsTest extends ScalaCheckSuite {

  // =========================================================================
  // WhatIfToggle Tests
  // =========================================================================

  test("WhatIfToggle.extractToggles returns distinct assumptions") {
    val a1 = Assumption("x", 1, "input")
    val a2 = Assumption("y", 2, "input")
    val c = Computed("z", 3, "x + y", List(a1, a2))

    val toggles = WhatIfToggle.extractToggles(c)
    assertEquals(toggles.map(_.name).toSet, Set("x", "y"))
  }

  test("WhatIfToggle.extractToggles handles nested dependencies") {
    val a1 = Assumption("a", 10, "input")
    val c1 = Computed("b", 20, "a * 2", List(a1))
    val c2 = Computed("c", 30, "b + 10", List(c1))

    val toggles = WhatIfToggle.extractToggles(c2)
    assertEquals(toggles.map(_.name), List("a"))
  }

  test("WhatIfToggle.analyzeWhatIf finds existing assumption") {
    val a = Assumption("rate", 0.25, "input")
    val c = Computed("tax", 25000.0, "rate * 100000", List(a))

    val result = WhatIfToggle.analyzeWhatIf(c, "rate", 0.30, _ => 30000.0)
    assert(result.isDefined, "result should be defined")
    assertEquals(result.get.assumptionName, "rate")
    assertEquals(result.get.originalValue, 0.25)
    assertEquals(result.get.newValue, 0.30)
  }

  test("WhatIfToggle.analyzeWhatIf returns None for non-existent assumption") {
    val a = Assumption("rate", 0.25, "input")
    val c = Computed("tax", 25000.0, "rate * 100000", List(a))

    val result = WhatIfToggle.analyzeWhatIf(c, "nonexistent", 0.30, _ => 30000.0)
    assert(result.isEmpty, "result should be empty")
  }

  test("WhatIfToggle.generateToggleUI generates boolean toggle") {
    val stmts = WhatIfToggle.generateToggleUI("enabled", "boolean", "container1")
    assert(stmts.nonEmpty, "stmts should be non-empty")
    // Verify the generated code creates a toggle div
    val code = stmts.map(s => dev.bosatsu.codegen.js.Code.toDoc(s).render(80)).mkString("\n")
    assert(code.contains("toggleDiv"), s"code should contain toggleDiv: $code")
    assert(code.contains("what-if-toggle"), s"code should contain what-if-toggle: $code")
  }

  test("WhatIfToggle.generateToggleUI generates number toggle") {
    val stmts = WhatIfToggle.generateToggleUI("rate", "number", "container2")
    assert(stmts.nonEmpty, "stmts should be non-empty")
    val code = stmts.map(s => dev.bosatsu.codegen.js.Code.toDoc(s).render(80)).mkString("\n")
    assert(code.contains("toggleDiv"), s"code should contain toggleDiv: $code")
    assert(code.contains("number"), s"code should contain number: $code")
  }

  test("WhatIfToggle.generateToggleUI generates text toggle for unknown types") {
    val stmts = WhatIfToggle.generateToggleUI("name", "string", "container3")
    assert(stmts.nonEmpty, "stmts should be non-empty")
    val code = stmts.map(s => dev.bosatsu.codegen.js.Code.toDoc(s).render(80)).mkString("\n")
    assert(code.contains("toggleDiv"), s"code should contain toggleDiv: $code")
    assert(code.contains("text"), s"code should contain text: $code")
  }

  test("WhatIfToggle.toggleCSS is valid CSS") {
    val css = WhatIfToggle.toggleCSS
    assert(css.contains(".what-if-toggle"), s"css should contain .what-if-toggle: $css")
    assert(css.contains(".toggle-btn"), s"css should contain .toggle-btn: $css")
    assert(css.contains(".toggle-input"), s"css should contain .toggle-input: $css")
  }

  // =========================================================================
  // WhyExplainer Tests
  // =========================================================================

  test("WhyExplainer.explain Plain format produces text") {
    val a = Assumption("income", 100000, "annual income")
    val result = WhyExplainer.explain(a, WhyExplainer.Plain)
    assert(result.nonEmpty, "result should be non-empty")
    assert(result.contains("income"), s"result should contain income: $result")
  }

  test("WhyExplainer.explain HTML format produces valid HTML") {
    val a = Assumption("income", 100000, "annual income")
    val result = WhyExplainer.explain(a, WhyExplainer.HTML)
    assert(result.contains("<"), s"result should contain <: $result")
    assert(result.contains("income"), s"result should contain income: $result")
  }

  test("WhyExplainer.explain for Computed shows formula") {
    val a = Assumption("rate", 0.25, "tax rate")
    val c = Computed("tax", 25000.0, "rate * 100000", List(a))
    val result = WhyExplainer.explain(c, WhyExplainer.Plain)
    assert(result.contains("tax"), s"result should contain tax: $result")
    assert(result.contains("rate"), s"result should contain rate: $result")
  }

  test("WhyExplainer.explain for Conditional shows condition") {
    val a = Assumption("income", 50000, "income")
    val highIncome = Assumption("highIncome", false, "income >= 40000")
    val cond = Conditional("bracket", "low", highIncome, "highIncome", "high", "low", List(a))
    val result = WhyExplainer.explain(cond, WhyExplainer.Plain)
    assert(result.contains("bracket"), s"result should contain bracket: $result")
  }

  test("WhyExplainer.generateWhyButton creates valid JS") {
    val stmts = WhyExplainer.generateWhyButton("tax", "container1")
    assert(stmts.nonEmpty, "stmts should be non-empty")
    val code = stmts.map(s => dev.bosatsu.codegen.js.Code.toDoc(s).render(80)).mkString("\n")
    assert(code.contains("Why"), s"code should contain Why: $code")
  }

  test("WhyExplainer.generateWhyModal creates valid JS") {
    val stmts = WhyExplainer.generateWhyModal()
    assert(stmts.nonEmpty, "stmts should be non-empty")
    val code = stmts.map(s => dev.bosatsu.codegen.js.Code.toDoc(s).render(80)).mkString("\n")
    assert(code.contains("why-modal"), s"code should contain why-modal: $code")
  }

  test("WhyExplainer.whyModalCSS is valid CSS") {
    val css = WhyExplainer.whyModalCSS
    assert(css.contains(".why-button"), s"css should contain .why-button: $css")
    assert(css.contains(".why-modal"), s"css should contain .why-modal: $css")
  }

  test("WhyExplainer.generateShowWhyFunction creates valid JS") {
    val stmt = WhyExplainer.generateShowWhyFunction()
    val code = dev.bosatsu.codegen.js.Code.toDoc(stmt).render(80)
    assert(code.contains("showWhyExplanation"), s"code should contain showWhyExplanation: $code")
  }

  test("WhyExplainer.generateExplainDerivationFunction creates valid JS") {
    val stmt = WhyExplainer.generateExplainDerivationFunction()
    val code = dev.bosatsu.codegen.js.Code.toDoc(stmt).render(80)
    assert(code.contains("_explainDerivation"), s"code should contain _explainDerivation: $code")
  }

  test("WhyExplainer.generateDerivationFunctions includes all required functions") {
    val stmts = WhyExplainer.generateDerivationFunctions()
    val code = stmts.map(s => dev.bosatsu.codegen.js.Code.toDoc(s).render(80)).mkString("\n")
    // Both functions must be present for _explainDerivation to work
    assert(code.contains("_formatDerivationHeader"), s"code should contain _formatDerivationHeader: $code")
    assert(code.contains("_explainDerivation"), s"code should contain _explainDerivation: $code")
  }

  test("WhyExplainer.explain with Markdown format") {
    val a = Assumption("rate", 0.25, "tax rate")
    val result = WhyExplainer.explain(a, WhyExplainer.Markdown)
    assert(result.contains("**rate**"), s"result should contain markdown bold: $result")
    assert(result.contains("`0.25`"), s"result should contain markdown code: $result")
  }

  // =========================================================================
  // ParameterSweep Tests (code generation)
  // =========================================================================

  test("ParameterSweep.runSweep computes all points") {
    val config = ParameterSweep.SweepConfig("x", 0.0, 10.0, 5)
    val result = ParameterSweep.runSweep(config, (x: Double) => x * x)
    assertEquals(result.points.length, 6) // 0 to 5 inclusive
    assertEquals(result.parameterName, "x")
  }

  test("ParameterSweep.runSweep finds min and max correctly") {
    val config = ParameterSweep.SweepConfig("y", -5.0, 5.0, 10)
    val result = ParameterSweep.runSweep(config, (y: Double) => y * y)
    // Minimum should be at y=0
    assert(result.minResult._2 < 1.0, s"min should be close to 0: ${result.minResult._2}")
    // Maximum should be at y=-5 or y=5
    assert(result.maxResult._2 >= 24.0, s"max should be close to 25: ${result.maxResult._2}")
  }

  test("ParameterSweep.generateSweepSlider creates valid JS") {
    val stmts = ParameterSweep.generateSweepSlider("param", 0.0, 100.0, 1.0, 50.0, "sweep-container")
    assert(stmts.nonEmpty, "stmts should be non-empty")
    val code = stmts.map(s => dev.bosatsu.codegen.js.Code.toDoc(s).render(80)).mkString("\n")
    assert(code.contains("sweep"), s"code should contain sweep: $code")
    assert(code.contains("param"), s"code should contain param: $code")
  }

  test("ParameterSweep.generateSweep2D creates valid JS") {
    val stmts = ParameterSweep.generateSweep2D("x", "y", "result", "canvas-id")
    assert(stmts.nonEmpty, "stmts should be non-empty")
    val code = stmts.map(s => dev.bosatsu.codegen.js.Code.toDoc(s).render(80)).mkString("\n")
    assert(code.contains("canvas") || code.contains("ctx"), s"code should contain canvas/ctx: $code")
  }

  test("ParameterSweep.sweepCSS is valid CSS") {
    val css = ParameterSweep.sweepCSS
    assert(css.contains(".sweep-slider"), s"css should contain .sweep-slider: $css")
    assert(css.contains(".parameter-sweep"), s"css should contain .parameter-sweep: $css")
  }

  // =========================================================================
  // VNode Tests
  // =========================================================================

  test("VNode.el creates Element with tag") {
    val node = VNode.el("div")
    node match {
      case e: VNode.Element => assertEquals(e.tag, "div")
      case _ => fail("Expected Element")
    }
  }

  test("VNode.el with attributes") {
    val attrs = Map[String, AttributeValue](
      "type" -> AttributeValue.StringValue("text"),
      "value" -> AttributeValue.StringValue("hello")
    )
    val node = VNode.el("input", attrs)
    node match {
      case e: VNode.Element =>
        assertEquals(e.tag, "input")
        assertEquals(e.attributes.size, 2)
      case _ => fail("Expected Element")
    }
  }

  test("VNode.el with children") {
    val child = VNode.text("Hello")
    val node = VNode.el("div", child)
    node match {
      case e: VNode.Element =>
        assertEquals(e.children.length, 1)
      case _ => fail("Expected Element")
    }
  }

  test("VNode.text creates Text node") {
    val node = VNode.text("Hello World")
    node match {
      case t: VNode.Text => assertEquals(t.content, "Hello World")
      case _ => fail("Expected Text")
    }
  }

  test("VNode helper methods create correct elements") {
    val div = VNode.div(VNode.text("content"))
    val span = VNode.span(VNode.text("inline"))
    val button = VNode.button(VNode.text("Click"))

    div match { case e: VNode.Element => assertEquals(e.tag, "div"); case _ => fail("") }
    span match { case e: VNode.Element => assertEquals(e.tag, "span"); case _ => fail("") }
    button match { case e: VNode.Element => assertEquals(e.tag, "button"); case _ => fail("") }
  }

  test("VNode.key returns correct key") {
    val withKey = VNode.el("div").asInstanceOf[VNode.Element].copy(key = Some("my-key"))
    assertEquals(withKey.key, Some("my-key"))

    val text = VNode.text("hello")
    assertEquals(text.key, None)
  }

  test("VNode.keyedEl creates element with key") {
    val node = VNode.keyedEl("div", "item-1", VNode.text("content"))
    assertEquals(node.key, Some("item-1"))
    assertEquals(node.tag, "div")
    assertEquals(node.children.length, 1)
  }

  test("VNode common element helpers") {
    val p = VNode.p(VNode.text("paragraph"))
    val h1 = VNode.h1(VNode.text("heading"))
    val h2 = VNode.h2(VNode.text("subheading"))
    val h3 = VNode.h3(VNode.text("sub-subheading"))
    val ul = VNode.ul(VNode.li(VNode.text("item")))
    val input = VNode.input()

    assertEquals(p.tag, "p")
    assertEquals(h1.tag, "h1")
    assertEquals(h2.tag, "h2")
    assertEquals(h3.tag, "h3")
    assertEquals(ul.tag, "ul")
    assertEquals(input.tag, "input")
  }

  test("AttributeValue implicit conversions") {
    import AttributeValue._
    val s: AttributeValue = fromString("hello")
    val i: AttributeValue = fromInt(42)
    val d: AttributeValue = fromDouble(3.14)
    val b: AttributeValue = fromBoolean(true)

    s match { case StringValue(v) => assertEquals(v, "hello"); case _ => fail("") }
    i match { case IntValue(v) => assertEquals(v, 42); case _ => fail("") }
    d match { case DoubleValue(v) => assertEquals(v, 3.14); case _ => fail("") }
    b match { case BoolValue(v) => assertEquals(v, true); case _ => fail("") }
  }

  test("AttributeValue.render produces correct strings") {
    assertEquals(AttributeValue.render(AttributeValue.StringValue("hello")), Some("hello"))
    assertEquals(AttributeValue.render(AttributeValue.IntValue(42)), Some("42"))
    assertEquals(AttributeValue.render(AttributeValue.DoubleValue(3.14)), Some("3.14"))
    assertEquals(AttributeValue.render(AttributeValue.BoolValue(true)), Some(""))
    assertEquals(AttributeValue.render(AttributeValue.BoolValue(false)), None)
    assertEquals(AttributeValue.render(AttributeValue.NullValue), None)
  }

  // =========================================================================
  // ReactiveState Tests
  // =========================================================================

  test("ReactiveState.state creates mutable state") {
    val s = ReactiveState.state(42)
    assertEquals(s.get, 42)
    s.set(100)
    assertEquals(s.get, 100)
  }

  test("ReactiveState.computed creates derived state") {
    val s = ReactiveState.state(10)
    val c = ReactiveState.computed(() => s.get * 2, s)
    assertEquals(c.get, 20)
  }

  test("ReactiveState.store registers and retrieves state") {
    val store = ReactiveState.store()
    val s = store.register("count", 0)
    assertEquals(store.get[Int]("count").map(_.get), Some(0))
    assertEquals(store.names, Set("count"))
  }

  test("ComputedState updates when dependency changes") {
    val s = ReactiveState.state(5)
    val c = ReactiveState.computed(() => s.get + 10, s)
    assertEquals(c.get, 15)
    s.set(20)
    assertEquals(c.get, 30)
  }

  test("ComputedState.dispose stops updates") {
    val s = ReactiveState.state(5)
    val c = new ComputedState(() => s.get + 10, Seq(s))
    assertEquals(c.get, 15)
    c.dispose()
    assert(c.isDisposed, "c should be disposed")
    // After dispose, the cached value remains but won't update
    s.set(100)
    assertEquals(c.get, 15) // Still old value
  }

  test("ComputedState.set throws exception") {
    val s = ReactiveState.state(5)
    val c = ReactiveState.computed(() => s.get + 10, s)
    intercept[UnsupportedOperationException] {
      c.set(999)
    }
  }

  test("ComputedState.update throws exception") {
    val s = ReactiveState.state(5)
    val c = ReactiveState.computed(() => s.get + 10, s)
    intercept[UnsupportedOperationException] {
      c.update(_ + 1)
    }
  }

  test("StateStore.computed throws on missing dependency") {
    val store = ReactiveState.store()
    store.register("a", 1)
    intercept[IllegalArgumentException] {
      store.computed[Int]("c", () => 42, Seq("a", "missing"))
    }
  }

  test("MutableState.update applies function") {
    val s = ReactiveState.state(10)
    s.update(_ * 2)
    assertEquals(s.get, 20)
  }

  test("MutableState.subscribe notifies on change") {
    val s = ReactiveState.state(0)
    var lastValue = -1
    val sub = s.subscribe(v => lastValue = v)
    assertEquals(lastValue, 0) // Initial notification
    s.set(42)
    assertEquals(lastValue, 42)
    sub.cancel()
    assert(!sub.isActive, "sub should not be active")
  }

  test("MutableState.set only notifies on actual change") {
    val s = ReactiveState.state(10)
    var notifyCount = 0
    s.subscribe(_ => notifyCount += 1)
    assertEquals(notifyCount, 1) // Initial
    s.set(10) // Same value
    assertEquals(notifyCount, 1) // No change
    s.set(20) // Different value
    assertEquals(notifyCount, 2)
  }

  // =========================================================================
  // DOMCodegen Tests
  // =========================================================================

  test("DOMCodegen.generate creates valid JS for Element") {
    val el = VNode.el("div", Map[String, AttributeValue]("class" -> AttributeValue.StringValue("container")), VNode.text("Hello"))
    val stmts = DOMCodegen.generate(el, "root")
    assert(stmts.nonEmpty, "stmts should be non-empty")
    val code = stmts.map(s => dev.bosatsu.codegen.js.Code.toDoc(s).render(80)).mkString("\n")
    assert(code.contains("div"), s"code should contain div: $code")
    assert(code.contains("container"), s"code should contain container: $code")
  }

  test("DOMCodegen.generate creates valid JS for Text") {
    val text = VNode.text("Hello World")
    val stmts = DOMCodegen.generate(text, "parent")
    assert(stmts.nonEmpty, "stmts should be non-empty")
    val code = stmts.map(s => dev.bosatsu.codegen.js.Code.toDoc(s).render(80)).mkString("\n")
    assert(code.contains("createTextNode") || code.contains("Hello World"), s"code should contain createTextNode or Hello World: $code")
  }

  test("DOMCodegen.generate handles nested elements") {
    val nested = VNode.div(
      VNode.span(VNode.text("First")),
      VNode.span(VNode.text("Second"))
    )
    val stmts = DOMCodegen.generate(nested, "root")
    assert(stmts.nonEmpty, "stmts should be non-empty")
    val code = stmts.map(s => dev.bosatsu.codegen.js.Code.toDoc(s).render(80)).mkString("\n")
    assert(code.contains("div"), s"code should contain div: $code")
    assert(code.contains("span"), s"code should contain span: $code")
  }

  test("DOMCodegen.generateRenderFunction creates complete function") {
    val el = VNode.div(VNode.text("Hello"))
    val stmt = DOMCodegen.generateRenderFunction(el, "render", "#app")
    val code = dev.bosatsu.codegen.js.Code.toDoc(stmt).render(80)
    assert(code.contains("render"), s"code should contain render: $code")
    assert(code.contains("#app"), s"code should contain #app: $code")
  }

  test("DOMCodegen.generateModule creates exportable module") {
    val el = VNode.div(VNode.text("Hello"))
    val doc = DOMCodegen.generateModule(el, "TestModule")
    val code = doc.render(80)
    assert(code.contains("export"), s"code should contain export: $code")
  }

  test("DOMCodegen.renderToString produces valid HTML") {
    val el = VNode.el("div",
      Map[String, AttributeValue]("class" -> AttributeValue.StringValue("test")),
      VNode.span(VNode.text("Hello"))
    )
    val html = DOMCodegen.renderToString(el)
    assert(html.contains("<div"), s"html should contain <div: $html")
    assert(html.contains("class=\"test\""), s"html should contain class: $html")
    assert(html.contains("<span>Hello</span>"), s"html should contain span: $html")
    assert(html.contains("</div>"), s"html should contain </div>: $html")
  }

  test("DOMCodegen.renderToString escapes HTML in text") {
    val el = VNode.text("<script>alert('xss')</script>")
    val html = DOMCodegen.renderToString(el)
    assert(!html.contains("<script>"), s"html should not contain raw script tag: $html")
    assert(html.contains("&lt;script&gt;"), s"html should escape script tag: $html")
  }

  test("DOMCodegen.renderToString handles void elements") {
    val input = VNode.input()
    val html = DOMCodegen.renderToString(input)
    assert(html == "<input>", s"void element should not have closing tag: $html")
    assert(!html.contains("</input>"), s"void element should not have closing tag: $html")
  }

  // =========================================================================
  // EmbedGenerator Tests
  // =========================================================================

  test("EmbedGenerator.generateEmbed produces valid HTML") {
    val config = EmbedGenerator.EmbedConfig("Test Title")
    val html = EmbedGenerator.generateEmbed(config, Map("x" -> "1"), "// js code")
    assert(html.contains("<!DOCTYPE html>"), s"html should contain doctype: $html")
    assert(html.contains("<title>Test Title</title>"), s"html should contain title: $html")
    assert(html.contains("// js code"), s"html should contain js code: $html")
  }

  test("EmbedGenerator respects theme") {
    val lightConfig = EmbedGenerator.EmbedConfig("T", theme = EmbedGenerator.LightTheme)
    val darkConfig = EmbedGenerator.EmbedConfig("T", theme = EmbedGenerator.DarkTheme)

    val light = EmbedGenerator.generateEmbed(lightConfig, Map("x" -> "1"), "")
    val dark = EmbedGenerator.generateEmbed(darkConfig, Map("x" -> "1"), "")

    assert(light.contains("data-theme=\"light\""), s"light should have light theme: $light")
    assert(dark.contains("data-theme=\"dark\""), s"dark should have dark theme: $dark")
  }

  test("EmbedGenerator.generateLibraryEmbed creates minimal output") {
    val config = EmbedGenerator.EmbedConfig("Test", libraryMode = true)
    val html = EmbedGenerator.generateLibraryEmbed(config, "TestModule", Map("x" -> "1"), "// code")
    assert(!html.contains("<!DOCTYPE html>"), s"library mode should not have doctype: $html")
    assert(html.contains("TestModule-container"), s"library mode should have container: $html")
    assert(html.contains("BosatsuApplets"), s"library mode should register applet: $html")
  }

  test("EmbedGenerator respects width and height") {
    val config = EmbedGenerator.EmbedConfig("Test", width = 800, height = 600)
    val html = EmbedGenerator.generateEmbed(config, Map.empty, "")
    assert(html.contains("800px") || html.contains("max-width: 800"), s"html should respect width")
    assert(html.contains("600") || html.contains("height"), s"html should have height")
  }

  test("EmbedGenerator.LightTheme has expected colors") {
    val theme = EmbedGenerator.LightTheme
    assertEquals(theme.name, "light")
    assert(theme.backgroundColor.nonEmpty, "backgroundColor should be set")
    assert(theme.textColor.nonEmpty, "textColor should be set")
    assert(theme.accentColor.nonEmpty, "accentColor should be set")
    assert(theme.surfaceColor.nonEmpty, "surfaceColor should be set")
  }

  test("EmbedGenerator.DarkTheme has expected colors") {
    val theme = EmbedGenerator.DarkTheme
    assertEquals(theme.name, "dark")
    assert(theme.backgroundColor.nonEmpty, "backgroundColor should be set")
    assert(theme.textColor.nonEmpty, "textColor should be set")
    assert(theme.accentColor.nonEmpty, "accentColor should be set")
    assert(theme.surfaceColor.nonEmpty, "surfaceColor should be set")
  }

  test("EmbedConfig default values") {
    val config = EmbedGenerator.EmbedConfig("Test")
    assertEquals(config.width, 600)
    assertEquals(config.height, 400)
    assertEquals(config.theme, EmbedGenerator.LightTheme)
    assertEquals(config.showWhyButtons, true)
    assertEquals(config.showWhatIfToggles, true)
    assertEquals(config.showParameterSweeps, true)
    assertEquals(config.libraryMode, false)
  }

  // =========================================================================
  // DOMBindings Tests
  // =========================================================================

  test("DOMBindings.empty creates empty bindings") {
    val bindings = DOMBindings.empty
    assert(bindings.stateReads.isEmpty, "stateReads should be empty")
    assert(bindings.stateWrites.isEmpty, "stateWrites should be empty")
    assert(bindings.eventHandlers.isEmpty, "eventHandlers should be empty")
    assertEquals(bindings.renderTarget, None)
  }

  // =========================================================================
  // VNode.Component Tests
  // =========================================================================

  test("VNode.Component renders correctly") {
    val comp = VNode.Component(
      "TestComponent",
      Map.empty,
      () => VNode.div(VNode.text("Component content")),
      Some("comp-key")
    )
    assertEquals(comp.name, "TestComponent")
    assertEquals(comp.key, Some("comp-key"))

    val rendered = comp.render()
    rendered match {
      case e: VNode.Element => assertEquals(e.tag, "div")
      case _ => fail("Expected Element from render")
    }
  }

  test("DOMCodegen.generate expands Component") {
    val comp = VNode.Component(
      "TestComponent",
      Map.empty,
      () => VNode.span(VNode.text("expanded")),
      None
    )
    val stmts = DOMCodegen.generate(comp, "root")
    val code = stmts.map(s => dev.bosatsu.codegen.js.Code.toDoc(s).render(80)).mkString("\n")
    assert(code.contains("span"), s"component should expand to span: $code")
  }

  test("DOMCodegen.renderToString expands Component") {
    val comp = VNode.Component(
      "TestComponent",
      Map.empty,
      () => VNode.p(VNode.text("paragraph")),
      None
    )
    val html = DOMCodegen.renderToString(comp)
    assert(html.contains("<p>paragraph</p>"), s"component should expand: $html")
  }

  // =========================================================================
  // SimulationApplet Tests
  // =========================================================================

  test("Assumption creates derivation with no dependencies") {
    val a = Assumption("x", 42, "test")
    assertEquals(a.name, "x")
    assertEquals(a.value, 42)
    assertEquals(a.description, "test")
    assert(a.dependencies.isEmpty, "assumption should have no dependencies")
  }

  test("Assumption.explain includes name and value") {
    val a = Assumption("rate", 0.25, "tax rate")
    val explanation = a.explain
    assert(explanation.contains("rate"), s"explanation should contain name: $explanation")
    assert(explanation.contains("0.25"), s"explanation should contain value: $explanation")
    assert(explanation.contains("assumption"), s"explanation should indicate assumption: $explanation")
  }

  test("Computed tracks dependencies") {
    val a = Assumption("x", 10, "")
    val c = Computed("y", 20, "x * 2", List(a))
    assertEquals(c.dependencies.length, 1)
    assertEquals(c.dependencies.head, a)
  }

  test("Computed.explain includes formula") {
    val a = Assumption("x", 10, "")
    val c = Computed("y", 20, "x * 2", List(a))
    val explanation = c.explain
    assert(explanation.contains("y"), s"explanation should contain name: $explanation")
    assert(explanation.contains("x * 2"), s"explanation should contain formula: $explanation")
    assert(explanation.contains("20"), s"explanation should contain value: $explanation")
  }

  test("derive helper creates Computed") {
    val a = mkAssume("x", 10, "input")
    val c = derive("y", "x * 2", 20)(a)
    c match {
      case comp: Computed[?] =>
        assertEquals(comp.name, "y")
        assertEquals(comp.formula, "x * 2")
        assertEquals(comp.value, 20)
      case _ => fail("Expected Computed")
    }
  }

  test("explainDerivation produces hierarchical output") {
    val a = mkAssume("x", 10, "input")
    val b = derive("y", "x * 2", 20)(a)
    val c = derive("z", "y + 5", 25)(b)
    val explanation = explainDerivation(c)
    assert(explanation.contains("z"), "should contain z")
    assert(explanation.contains("y"), "should contain y")
    assert(explanation.contains("x"), "should contain x")
  }

  test("getAssumptions extracts all base assumptions") {
    val a1 = mkAssume("x", 10, "")
    val a2 = mkAssume("y", 20, "")
    val c = derive("z", "x + y", 30)(a1, a2)
    val assumptions = getAssumptions(c)
    assertEquals(assumptions.map(_.name).toSet, Set("x", "y"))
  }

  test("MutableSimulationState stores and retrieves derivations") {
    val state = simulationState()
    val a = mkAssume("test", 42, "test value")
    state.set(a)
    val retrieved = state.get[Int]("test")
    assert(retrieved.isDefined, "should retrieve stored derivation")
    assertEquals(retrieved.get.value, 42)
  }

  test("MutableSimulationState.updateAssumption changes value") {
    val state = simulationState()
    state.set(mkAssume("x", 100, ""))
    state.updateAssumption("x", 200)
    assertEquals(state.getAssumption[Int]("x").map(_.value), Some(200))
  }

  test("MutableSimulationState.subscribe notifies on changes") {
    val state = simulationState()
    var notified = false
    state.subscribe(() => notified = true)
    state.set(mkAssume("x", 1, ""))
    assert(notified, "should notify on set")
  }

  test("MutableSimulationState.derivations returns all derivations") {
    val state = simulationState()
    state.set(mkAssume("a", 1, ""))
    state.set(mkAssume("b", 2, ""))
    state.set(derive("c", "a + b", 3)(mkAssume("a", 1, ""), mkAssume("b", 2, "")))
    val derivs = state.derivations
    assertEquals(derivs.size, 3)
    assert(derivs.contains("a"), "should contain a")
    assert(derivs.contains("b"), "should contain b")
    assert(derivs.contains("c"), "should contain c")
  }

  test("Conditional.explain shows branch taken") {
    val cond = Assumption("isHigh", true, "")
    val c = Conditional("rate", 0.5, cond, "isHigh", "high rate", "low rate", List(cond))
    val explanation = c.explain
    assert(explanation.contains("rate"), s"should contain name: $explanation")
    assert(explanation.contains("high rate"), s"should contain taken branch: $explanation")
    assert(explanation.contains("isHigh=true"), s"should show condition: $explanation")
  }

  test("Conditional with false condition shows other branch") {
    val cond = Assumption("isHigh", false, "")
    val c = Conditional("rate", 0.2, cond, "isHigh", "high rate", "low rate", List(cond))
    val explanation = c.explain
    assert(explanation.contains("low rate"), s"should contain taken branch: $explanation")
    assert(explanation.contains("isHigh=false"), s"should show condition: $explanation")
  }
}
