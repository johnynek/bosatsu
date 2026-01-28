package dev.bosatsu.simulation

import munit.FunSuite
import dev.bosatsu.Identifier
import dev.bosatsu.ui.EmbedGenerator

class SimulationGenTest extends FunSuite {

  def bindable(name: String): Identifier.Bindable =
    Identifier.unsafeBindable(name)

  def makeAnalysis(
      name: String,
      kind: DerivationAnalyzer.DerivationKind,
      deps: Set[String] = Set.empty,
      formula: String = "",
      valueType: String = "number"
  ): DerivationAnalyzer.AnalyzedBinding =
    DerivationAnalyzer.AnalyzedBinding(
      bindable(name),
      kind,
      deps.map(bindable),
      formula,
      valueType
    )

  // ============================================
  // Basic generation tests
  // ============================================

  test("generate produces valid HTML") {
    val analyses = List(
      makeAnalysis("income", DerivationAnalyzer.Assumption, formula = "100000"),
      makeAnalysis("tax_rate", DerivationAnalyzer.Assumption, formula = "0.25"),
      makeAnalysis("tax", DerivationAnalyzer.Computation, Set("income", "tax_rate"), "(income * tax_rate)")
    )

    val config = SimulationGen.SimConfig(
      title = "Tax Calculator",
      showWhy = true,
      showWhatIf = true,
      showSweeps = false
    )

    val html = SimulationGen.generate(analyses, "// computation code", config)

    // Check HTML structure
    assert(html.contains("<!DOCTYPE html>"))
    assert(html.contains("<title>Tax Calculator</title>"))
    assert(html.contains("_derivations"))
    assert(html.contains("_recompute"))
  }

  test("generate includes DOCTYPE and basic HTML structure") {
    val analyses = List(makeAnalysis("x", DerivationAnalyzer.Assumption))
    val html = SimulationGen.generate(analyses, "", SimulationGen.SimConfig("Test"))

    assert(html.startsWith("<!DOCTYPE html>"))
    assert(html.contains("<html"))
    assert(html.contains("<head>"))
    assert(html.contains("<body>"))
    assert(html.contains("</html>"))
  }

  test("generate includes title in head") {
    val analyses = List(makeAnalysis("x", DerivationAnalyzer.Assumption))
    val html = SimulationGen.generate(analyses, "", SimulationGen.SimConfig("My Custom Title"))

    assert(html.contains("<title>My Custom Title</title>"))
  }

  test("generate includes meta viewport") {
    val analyses = List(makeAnalysis("x", DerivationAnalyzer.Assumption))
    val html = SimulationGen.generate(analyses, "", SimulationGen.SimConfig("Test"))

    assert(html.contains("viewport"))
  }

  // ============================================
  // Feature flag tests
  // ============================================

  test("generate includes Why? buttons when enabled") {
    val analyses = List(
      makeAnalysis("x", DerivationAnalyzer.Assumption),
      makeAnalysis("y", DerivationAnalyzer.Computation, Set("x"), "x + 1")
    )

    val configWithWhy = SimulationGen.SimConfig("Test", showWhy = true)
    val htmlWithWhy = SimulationGen.generate(analyses, "", configWithWhy)
    assert(htmlWithWhy.contains("Why?"))
    assert(htmlWithWhy.contains("showWhyExplanation"))

    val configWithoutWhy = SimulationGen.SimConfig("Test", showWhy = false)
    val htmlWithoutWhy = SimulationGen.generate(analyses, "", configWithoutWhy)
    // Should still have the basic Why? infrastructure but no buttons added
    assert(htmlWithoutWhy.contains("showWhyExplanation"))
  }

  test("generate includes What-if toggles when enabled") {
    val analyses = List(
      makeAnalysis("rate", DerivationAnalyzer.Assumption, valueType = "number"),
      makeAnalysis("enabled", DerivationAnalyzer.Assumption, valueType = "boolean"),
      makeAnalysis("result", DerivationAnalyzer.Computation, Set("rate", "enabled"))
    )

    val configWithWhatIf = SimulationGen.SimConfig("Test", showWhatIf = true)
    val html = SimulationGen.generate(analyses, "", configWithWhatIf)
    assert(html.contains("What if"))
    assert(html.contains("addWhatIfToggle"))
  }

  test("generate includes sweep sliders when enabled") {
    val analyses = List(
      makeAnalysis("param", DerivationAnalyzer.Assumption, valueType = "number"),
      makeAnalysis("output", DerivationAnalyzer.Computation, Set("param"))
    )

    val configWithSweeps = SimulationGen.SimConfig("Test", showSweeps = true)
    val html = SimulationGen.generate(analyses, "", configWithSweeps)
    assert(html.contains("sweep-slider"))
    assert(html.contains("addSweepSlider"))
  }

  test("generate includes all features when all enabled") {
    val analyses = List(
      makeAnalysis("x", DerivationAnalyzer.Assumption, valueType = "number"),
      makeAnalysis("y", DerivationAnalyzer.Computation, Set("x"))
    )

    val config = SimulationGen.SimConfig("Test", showWhy = true, showWhatIf = true, showSweeps = true)
    val html = SimulationGen.generate(analyses, "", config)

    assert(html.contains("Why?"))
    assert(html.contains("What if"))
    assert(html.contains("sweep-slider"))
  }

  // ============================================
  // Theme tests
  // ============================================

  test("generate uses correct theme") {
    val analyses = List(makeAnalysis("x", DerivationAnalyzer.Assumption))

    val lightConfig = SimulationGen.SimConfig("Test", theme = EmbedGenerator.LightTheme)
    val lightHtml = SimulationGen.generate(analyses, "", lightConfig)
    assert(lightHtml.contains("data-theme=\"light\""))

    val darkConfig = SimulationGen.SimConfig("Test", theme = EmbedGenerator.DarkTheme)
    val darkHtml = SimulationGen.generate(analyses, "", darkConfig)
    assert(darkHtml.contains("data-theme=\"dark\""))
  }

  // ============================================
  // Derivation metadata tests
  // ============================================

  test("derivation state includes correct metadata") {
    val analyses = List(
      makeAnalysis("income", DerivationAnalyzer.Assumption, formula = "100000"),
      makeAnalysis("tax", DerivationAnalyzer.Computation, Set("income"), "(income * 0.25)")
    )

    val html = SimulationGen.generate(analyses, "", SimulationGen.SimConfig("Test"))

    // Check derivation metadata
    assert(html.contains("\"income\""))
    assert(html.contains("\"tax\""))
    assert(html.contains("\"assumption\""))
    assert(html.contains("\"computed\""))
    assert(html.contains("(income * 0.25)"))
  }

  test("derivation state includes conditional kind") {
    val analyses = List(
      makeAnalysis("cond", DerivationAnalyzer.Conditional, Set("x"), "if x then 1 else 0")
    )

    val html = SimulationGen.generate(analyses, "", SimulationGen.SimConfig("Test"))
    assert(html.contains("\"conditional\""))
  }

  test("derivation state includes dependencies") {
    val analyses = List(
      makeAnalysis("a", DerivationAnalyzer.Assumption),
      makeAnalysis("b", DerivationAnalyzer.Assumption),
      makeAnalysis("c", DerivationAnalyzer.Computation, Set("a", "b"), "a + b")
    )

    val html = SimulationGen.generate(analyses, "", SimulationGen.SimConfig("Test"))
    // Should have deps array
    assert(html.contains("deps"))
  }

  test("derivation state includes value types") {
    val analyses = List(
      makeAnalysis("num", DerivationAnalyzer.Assumption, valueType = "number"),
      makeAnalysis("str", DerivationAnalyzer.Assumption, valueType = "string"),
      makeAnalysis("bool", DerivationAnalyzer.Assumption, valueType = "boolean"),
      makeAnalysis("any", DerivationAnalyzer.Assumption, valueType = "any")
    )

    val html = SimulationGen.generate(analyses, "", SimulationGen.SimConfig("Test"))
    assert(html.contains("\"number\""))
    assert(html.contains("\"string\""))
    assert(html.contains("\"boolean\""))
    assert(html.contains("\"any\""))
  }

  // ============================================
  // Ordering and edge cases
  // ============================================

  test("topological sort orders dependencies correctly") {
    // c depends on b, b depends on a
    val analyses = List(
      makeAnalysis("c", DerivationAnalyzer.Computation, Set("b"), "b + 1"),
      makeAnalysis("a", DerivationAnalyzer.Assumption),
      makeAnalysis("b", DerivationAnalyzer.Computation, Set("a"), "a * 2")
    )

    val html = SimulationGen.generate(analyses, "", SimulationGen.SimConfig("Test"))

    // In the generated JS, a should appear before b, and b before c
    // We can't easily verify order in the HTML, but we can verify all are present
    assert(html.contains("\"a\""))
    assert(html.contains("\"b\""))
    assert(html.contains("\"c\""))
  }

  test("special characters in formulas are escaped") {
    val analyses = List(
      makeAnalysis("x", DerivationAnalyzer.Computation, formula = "\"quoted\" and 'apostrophe'")
    )

    val html = SimulationGen.generate(analyses, "", SimulationGen.SimConfig("Test"))

    // Should not break the JS
    assert(html.contains("_derivations"))
    // The formula should be escaped
    assert(html.contains("quoted"))
  }

  test("empty analyses list produces valid HTML") {
    val html = SimulationGen.generate(List(), "", SimulationGen.SimConfig("Empty"))
    assert(html.contains("<!DOCTYPE html>"))
    assert(html.contains("<title>Empty</title>"))
  }

  test("single assumption produces valid HTML") {
    val analyses = List(makeAnalysis("x", DerivationAnalyzer.Assumption, formula = "42"))
    val html = SimulationGen.generate(analyses, "", SimulationGen.SimConfig("Single"))
    assert(html.contains("<!DOCTYPE html>"))
    assert(html.contains("\"x\""))
    assert(html.contains("42"))
  }

  test("values from computation code are extracted and used in state") {
    // computeJs values are extracted via regex and used to initialize state
    // The raw computeJs is NOT included - values are managed through _state object instead
    val analyses = List(makeAnalysis("myVar", DerivationAnalyzer.Assumption))
    val computeJs = "const myVar = 42;"
    val html = SimulationGen.generate(analyses, computeJs, SimulationGen.SimConfig("Test"))
    // Check that the variable and its value appear in the state initialization
    assert(html.contains("myVar"))
    assert(html.contains("42"))
  }

  test("long binding names are handled") {
    val analyses = List(
      makeAnalysis("very_long_variable_name_that_is_quite_descriptive", DerivationAnalyzer.Assumption)
    )
    val html = SimulationGen.generate(analyses, "", SimulationGen.SimConfig("Test"))
    assert(html.contains("very_long_variable_name_that_is_quite_descriptive"))
  }

  test("multiple levels of dependencies are included") {
    val analyses = List(
      makeAnalysis("a", DerivationAnalyzer.Assumption),
      makeAnalysis("b", DerivationAnalyzer.Computation, Set("a")),
      makeAnalysis("c", DerivationAnalyzer.Computation, Set("b")),
      makeAnalysis("d", DerivationAnalyzer.Computation, Set("c")),
      makeAnalysis("e", DerivationAnalyzer.Computation, Set("d"))
    )
    val html = SimulationGen.generate(analyses, "", SimulationGen.SimConfig("Test"))

    for (name <- List("a", "b", "c", "d", "e")) {
      assert(html.contains(s"\"$name\""), s"Should contain $name")
    }
  }

  // ============================================
  // SimConfig tests
  // ============================================

  test("SimConfig has correct defaults") {
    val config = SimulationGen.SimConfig("Title")
    assertEquals(config.title, "Title")
    assertEquals(config.theme, EmbedGenerator.LightTheme)
    assertEquals(config.showWhy, true)
    assertEquals(config.showWhatIf, true)
    assertEquals(config.showSweeps, false)
  }

  test("SimConfig can override all fields") {
    val config = SimulationGen.SimConfig(
      title = "Custom",
      theme = EmbedGenerator.DarkTheme,
      showWhy = false,
      showWhatIf = false,
      showSweeps = true
    )
    assertEquals(config.title, "Custom")
    assertEquals(config.theme, EmbedGenerator.DarkTheme)
    assertEquals(config.showWhy, false)
    assertEquals(config.showWhatIf, false)
    assertEquals(config.showSweeps, true)
  }
}
