package dev.bosatsu.ui

import org.scalacheck.{Arbitrary, Prop}
import Prop.forAll
import SimulationApplet.{Derivation, Assumption, Computed, Conditional, derive, assume => mkAssume, explainDerivation, getAssumptions, simulationState, MutableSimulationState}
import SimulationAppletGen._

class SimulationAppletTest extends munit.ScalaCheckSuite {

  // =========================================================================
  // TEST FIXTURES (following burritoscript patterns)
  // =========================================================================

  def createTaxSimulation(): (Derivation[Double], MutableSimulationState) = {
    val state = simulationState()

    // Build derivation chain for tax calculation
    val taxRate = mkAssume("taxRate", 0.2, "Current tax rate")
    val gdp = mkAssume("gdp", 1000000.0, "Gross Domestic Product")

    val revenue = derive("revenue", "taxRate * gdp", taxRate.value * gdp.value)(taxRate, gdp)
    val efficiency = derive("efficiency", "1 - taxRate^2", 1 - math.pow(taxRate.value, 2))(taxRate)
    val spending = derive("spending", "revenue * 0.8", revenue.value * 0.8)(revenue)

    state.set(taxRate)
    state.set(gdp)
    state.set(revenue)
    state.set(efficiency)
    state.set(spending)

    (spending, state)
  }

  // =========================================================================
  // Derivation Chain Tests
  // =========================================================================

  property("Assumption has no dependencies") {
    forAll(genIntAssumption) { assumption =>
      assumption.dependencies.isEmpty
    }
  }

  property("Assumption explain contains name and value") {
    forAll(genIntAssumption) { assumption =>
      val explanation = assumption.explain
      explanation.contains(assumption.name) && explanation.contains(assumption.value.toString)
    }
  }

  property("getAssumptions extracts all assumptions from derivation chain") {
    forAll(genComputed(3)) { derivation =>
      val assumptions = getAssumptions(derivation)
      assumptions.forall(_.isInstanceOf[Assumption[?]])
    }
  }

  property("explainDerivation produces non-empty output") {
    forAll(genDerivation(2)) { derivation =>
      val explanation = explainDerivation(derivation)
      explanation.nonEmpty
    }
  }

  // =========================================================================
  // Static Provenance Tests (following burritoscript explain.test.ts)
  // =========================================================================

  test("explains input fields as assumptions") {
    val taxRate = mkAssume("taxRate", 0.2, "Current tax rate")
    val explanation = explainDerivation(taxRate)

    assert(explanation.contains("taxRate"))
    assert(explanation.contains("0.2"))
    assert(explanation.contains("assumption"))
  }

  test("explains computed fields with their dependency chain") {
    val taxRate = mkAssume("taxRate", 0.2, "Tax rate")
    val gdp = mkAssume("gdp", 1000000.0, "GDP")
    val revenue = derive("revenue", "taxRate * gdp", taxRate.value * gdp.value)(taxRate, gdp)
    val spending = derive("spending", "revenue * 0.8", revenue.value * 0.8)(revenue)

    val explanation = explainDerivation(spending)

    // Should include all steps in the chain
    assert(explanation.contains("spending"))
    assert(explanation.contains("revenue"))
    assert(explanation.contains("taxRate"))
  }

  test("includes relevant assumptions in explanation") {
    val (spending, _) = createTaxSimulation()
    val explanation = explainDerivation(spending)

    // The explanation should include upstream assumptions
    assert(explanation.contains("taxRate"))
    assert(explanation.contains("gdp"))
  }

  test("builds summary with steps and assumptions") {
    val model = mkAssume("model", "linear", "Tax model type")
    val taxRate = mkAssume("taxRate", 0.3, "Rate")
    val happiness = derive("happiness", "taxRate based on model", 0.7)(model, taxRate)

    val explanation = explainDerivation(happiness)

    assert(explanation.contains("happiness"))
    assert(explanation.contains("model"))
    assert(explanation.contains("taxRate"))
  }

  // =========================================================================
  // SimulationState Tests
  // =========================================================================

  test("SimulationState stores and retrieves derivations") {
    val state = simulationState()
    val assumption = mkAssume("test", 42, "test assumption")
    state.set(assumption)

    val retrieved = state.get[Int]("test")
    assert(retrieved.isDefined)
    assertEquals(retrieved.get.value, 42)
  }

  test("SimulationState tracks assumptions separately") {
    val state = simulationState()
    val a1 = mkAssume("gravity", 500, "gravitational constant")
    val a2 = mkAssume("bounciness", 0.8, "energy retained on bounce")

    state.set(a1)
    state.set(a2)

    assertEquals(state.getAssumption[Int]("gravity").map(_.value), Some(500))
    assertEquals(state.getAssumption[Double]("bounciness").map(_.value), Some(0.8))
  }

  test("SimulationState updateAssumption changes value") {
    val state = simulationState()
    state.set(mkAssume("x", 100, "position"))

    state.updateAssumption("x", 200)

    assertEquals(state.getAssumption[Int]("x").map(_.value), Some(200))
  }

  // =========================================================================
  // WhyExplainer Tests (following burritoscript simulation-ui.test.ts)
  // =========================================================================

  property("WhyExplainer.explain Plain format produces text") {
    forAll(genDerivation(2)) { derivation =>
      val explanation = WhyExplainer.explain(derivation, WhyExplainer.Plain)
      explanation.nonEmpty
    }
  }

  property("WhyExplainer.explain HTML format produces valid HTML") {
    forAll(genDerivation(2)) { derivation =>
      val explanation = WhyExplainer.explain(derivation, WhyExplainer.HTML)
      explanation.contains("<div") && explanation.contains("</div>")
    }
  }

  test("WhyExplainer includes field name in header") {
    val myField = mkAssume("myField", 42, "test")
    val explanation = WhyExplainer.explain(myField, WhyExplainer.HTML)

    assert(explanation.contains("myField"))
  }

  test("WhyExplainer renders dependency chain") {
    val (spending, _) = createTaxSimulation()
    val explanation = WhyExplainer.explain(spending, WhyExplainer.Plain)

    // Should show the full dependency chain
    assert(explanation.contains("spending"))
    assert(explanation.contains("revenue"))
  }

  test("WhyExplainer applies CSS classes by derivation type") {
    val assumption = mkAssume("test", 1, "")
    val computed = derive("comp", "test * 2", 2)(assumption)

    val assumptionHTML = WhyExplainer.explain(assumption, WhyExplainer.HTML)
    val computedHTML = WhyExplainer.explain(computed, WhyExplainer.HTML)

    assert(assumptionHTML.contains("class=\"derivation assumption\""))
    assert(computedHTML.contains("class=\"derivation computed\""))
  }

  // =========================================================================
  // WhatIfToggle Tests (following burritoscript explain.test.ts whatIf)
  // =========================================================================

  property("WhatIfToggle.extractToggles finds all assumptions") {
    forAll(genComputed(3)) { derivation =>
      val toggles = WhatIfToggle.extractToggles(derivation)
      toggles.forall(_.isInstanceOf[Assumption[?]])
    }
  }

  property("WhatIfToggle.extractToggles returns distinct assumptions") {
    forAll(genComputed(3)) { derivation =>
      val toggles = WhatIfToggle.extractToggles(derivation)
      toggles.map(_.name).distinct.size == toggles.size
    }
  }

  test("WhatIfToggle finds directly affected fields") {
    val model = mkAssume("model", "linear", "")
    val happiness = derive("happiness", "based on model", 0.5)(model)

    val toggles = WhatIfToggle.extractToggles(happiness)

    assert(toggles.exists(_.name == "model"))
  }

  test("WhatIfToggle finds transitively affected fields") {
    // revenue -> spending (spending depends on revenue)
    val taxRate = mkAssume("taxRate", 0.2, "")
    val revenue = derive("revenue", "taxRate * 100", 20.0)(taxRate)
    val spending = derive("spending", "revenue * 0.8", 16.0)(revenue)

    val toggles = WhatIfToggle.extractToggles(spending)

    // Should find the upstream taxRate assumption
    assert(toggles.exists(_.name == "taxRate"))
  }

  // =========================================================================
  // ParameterSweep Tests
  // =========================================================================

  property("SweepConfig generates correct number of values") {
    forAll(genSweepConfig) { config =>
      val values = config.values
      values.size == config.steps + 1
    }
  }

  property("SweepConfig values are within bounds") {
    forAll(genSweepConfig) { config =>
      val values = config.values
      values.forall(v => v >= config.minValue && v <= config.maxValue + 0.0001)
    }
  }

  property("SweepConfig values are monotonically increasing") {
    forAll(genSweepConfig) { config =>
      val values = config.values
      values.sliding(2).forall {
        case List(a, b) => a <= b
        case _ => true
      }
    }
  }

  property("runSweep produces result for each value") {
    forAll(genSweepConfig) { config =>
      val result = ParameterSweep.runSweep(config, (x: Double) => x * 2)
      result.points.size == config.values.size
    }
  }

  property("runSweep identifies min and max correctly") {
    forAll(genSweepConfig) { config =>
      val result = ParameterSweep.runSweep(config, (x: Double) => x * x)
      val minResult = result.points.minBy(_._2)
      val maxResult = result.points.maxBy(_._2)
      result.minResult == minResult && result.maxResult == maxResult
    }
  }

  test("SweepConfig stepSize preserves floating point precision") {
    // This test catches the bug where integer division was used
    val config = ParameterSweep.SweepConfig("x", 0.0, 1.0, 10)
    val stepSize = config.stepSize
    // stepSize should be exactly 0.1, not 0 (which integer division would give)
    assert(Math.abs(stepSize - 0.1) < 0.0001, s"stepSize was $stepSize, expected 0.1")
  }

  test("SweepConfig stepSize works for small ranges") {
    val config = ParameterSweep.SweepConfig("y", 0.0, 0.001, 10)
    val stepSize = config.stepSize
    assert(Math.abs(stepSize - 0.0001) < 0.00001, s"stepSize was $stepSize, expected 0.0001")
  }

  test("SweepConfig values are evenly spaced") {
    val config = ParameterSweep.SweepConfig("z", 0.0, 1.0, 4)
    val values = config.values
    // Should be [0.0, 0.25, 0.5, 0.75, 1.0]
    val expected = List(0.0, 0.25, 0.5, 0.75, 1.0)
    values.zip(expected).foreach { case (actual, exp) =>
      assert(Math.abs(actual - exp) < 0.0001, s"value $actual != expected $exp")
    }
  }

  // =========================================================================
  // EmbedGenerator Tests
  // =========================================================================

  property("EmbedGenerator produces valid HTML") {
    forAll(genEmbedConfig, genInitialState) { (config, state) =>
      val html = EmbedGenerator.generateEmbed(config, state, "// test")
      html.contains("<!DOCTYPE html>") &&
        html.contains("<html") &&
        html.contains("</html>") &&
        html.contains("<body>") &&
        html.contains("</body>")
    }
  }

  property("EmbedGenerator includes title in output") {
    forAll(genEmbedConfig) { config =>
      val html = EmbedGenerator.generateEmbed(config, Map("x" -> "1"), "// test")
      html.contains(config.title)
    }
  }

  property("EmbedGenerator respects theme setting") {
    forAll(genEmbedConfig) { config =>
      val html = EmbedGenerator.generateEmbed(config, Map("x" -> "1"), "// test")
      html.contains(s"""data-theme="${config.theme.name}"""")
    }
  }

  property("EmbedGenerator library mode produces compact output") {
    forAll(genEmbedConfig.map(_.copy(libraryMode = true))) { config =>
      val html = EmbedGenerator.generateLibraryEmbed(
        config, "TestModule", Map("x" -> "1"), "// test"
      )
      !html.contains("<!DOCTYPE html>")
    }
  }

  // =========================================================================
  // Integration Tests (following burritoscript patterns)
  // =========================================================================

  test("Full workflow: build metadata, explain, what-if") {
    // Build a realistic tax simulation
    val taxRate = mkAssume("taxRate", 0.2, "Current tax rate")
    val gdp = mkAssume("gdp", 1000000.0, "Gross Domestic Product")
    val model = mkAssume("taxModel", "progressive", "Tax efficiency model")

    val revenue = derive("revenue", "taxRate * gdp * efficiency", taxRate.value * gdp.value * 0.9)(taxRate, gdp, model)
    val happiness = derive("happiness", "1 - taxRate", 1 - taxRate.value)(taxRate)
    val spending = derive("spending", "revenue * 0.8 + happiness * 1000", revenue.value * 0.8 + happiness.value * 1000)(revenue, happiness)

    // Explain spending - should show full dependency chain
    val explanation = WhyExplainer.explain(spending, WhyExplainer.Plain)
    assert(explanation.contains("spending"))
    assert(explanation.contains("revenue"))
    assert(explanation.contains("happiness"))
    assert(explanation.contains("taxRate"))

    // What-if analysis - find toggleable assumptions
    val toggles = WhatIfToggle.extractToggles(spending)
    assert(toggles.exists(_.name == "taxRate"))
    assert(toggles.exists(_.name == "gdp"))
    assert(toggles.exists(_.name == "taxModel"))

    // HTML explanation should have proper structure
    val htmlExplanation = WhyExplainer.explain(spending, WhyExplainer.HTML)
    assert(htmlExplanation.contains("class=\"derivation"))
    assert(htmlExplanation.contains("computed"))
  }

  test("State change triggers listener notification") {
    val state = simulationState()
    var notified = false

    state.set(mkAssume("x", 100, "position"))
    state.subscribe(() => notified = true)
    state.updateAssumption("x", 200)

    assert(notified)
  }

  test("Conditional derivation includes condition in explanation") {
    val highTax = mkAssume("highTax", true, "Is tax rate high?")
    val rate = Conditional(
      "taxRate",
      0.5,
      highTax,
      "highTax",
      "High rate (50%)",
      "Low rate (20%)",
      List(highTax)
    )

    val explanation = WhyExplainer.explain(rate, WhyExplainer.Plain)

    assert(explanation.contains("taxRate"))
    assert(explanation.contains("High rate"))
    assert(explanation.contains("highTax=true"))
  }
}
