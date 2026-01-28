package dev.bosatsu.simulation

import dev.bosatsu.codegen.js.{Code, JsGen}
import dev.bosatsu.ui.{EmbedGenerator, WhyExplainer, WhatIfToggle, ParameterSweep}
import dev.bosatsu.Identifier.Bindable

/**
 * Generator for simulation HTML with provenance tracking.
 *
 * This integrates:
 * - JsGen: Compiles Matchless IR to JavaScript
 * - DerivationAnalyzer: Extracts dependency information
 * - EmbedGenerator: Creates self-contained HTML
 * - WhyExplainer: Generates "Why?" button UI
 * - WhatIfToggle: Generates "What if?" toggle UI
 * - ParameterSweep: Generates parameter sweep UI
 */
object SimulationGen {

  /**
   * Configuration for simulation generation.
   */
  case class SimConfig(
      title: String,
      theme: EmbedGenerator.Theme = EmbedGenerator.LightTheme,
      showWhy: Boolean = true,
      showWhatIf: Boolean = true,
      showSweeps: Boolean = false
  )

  /**
   * Generate a complete interactive HTML simulation.
   */
  def generate(
      analyses: List[DerivationAnalyzer.AnalyzedBinding],
      computeJs: String,
      config: SimConfig
  ): String = {
    // 1. Generate derivation state object
    val derivationState = generateDerivationState(analyses)

    // 2. Generate recompute function
    val recomputeJs = generateRecomputeFunction(analyses)

    // 3. Generate UI initialization code
    val uiJs = generateUICode(analyses, config)

    // 4. Combine all JS
    val fullJs =
      s"""${JsGen.runtimeCode}

// Derivation state tracking
$derivationState

// Computation code (from JsGen)
$computeJs

// Recomputation logic
$recomputeJs

// UI initialization
$uiJs
"""

    // 5. Build initial state from assumptions
    val initialState = analyses
      .filter(_.kind == DerivationAnalyzer.Assumption)
      .map(a => a.name.asString -> "undefined")
      .toMap

    // 6. Generate embed config
    val embedConfig = EmbedGenerator.EmbedConfig(
      title = config.title,
      theme = config.theme,
      showWhyButtons = config.showWhy,
      showWhatIfToggles = config.showWhatIf,
      showParameterSweeps = config.showSweeps
    )

    // 7. Generate HTML
    EmbedGenerator.generateEmbed(embedConfig, initialState, fullJs)
  }

  /**
   * Generate JavaScript object tracking derivation metadata.
   */
  private def generateDerivationState(
      analyses: List[DerivationAnalyzer.AnalyzedBinding]
  ): String = {
    val entries = analyses.map { a =>
      val depsArray = a.dependencies.map(d => s""""${d.asString}"""").mkString("[", ", ", "]")
      val kindStr = a.kind match {
        case DerivationAnalyzer.Assumption  => "assumption"
        case DerivationAnalyzer.Computation => "computed"
        case DerivationAnalyzer.Conditional => "conditional"
      }
      s"""  "${a.name.asString}": {
    type: "$kindStr",
    name: "${a.name.asString}",
    formula: "${escapeJs(a.formula)}",
    valueType: "${a.valueType}",
    deps: $depsArray,
    value: undefined
  }"""
    }

    s"""const _derivations = {
${entries.mkString(",\n")}
};

// Register derivations with the reactive state system
Object.keys(_derivations).forEach(name => {
  _setDerivation(name, _derivations[name]);
});"""
  }

  /**
   * Generate the recompute function that updates derived values.
   */
  private def generateRecomputeFunction(
      analyses: List[DerivationAnalyzer.AnalyzedBinding]
  ): String = {
    // Sort bindings topologically by dependencies
    val sorted = topologicalSort(analyses)

    val updateStmts = sorted.map { a =>
      val name = a.name.asString
      val escaped = JsGen.escape(a.name).name
      s"""  // Update $name
  if (typeof $escaped !== 'undefined') {
    _derivations["$name"].value = $escaped;
    _setState("$name", $escaped);
  }"""
    }

    s"""function _recompute() {
${updateStmts.mkString("\n")}

  // Update UI displays
  Object.keys(_derivations).forEach(name => {
    const el = document.getElementById('val-' + name);
    if (el && _derivations[name].value !== undefined) {
      el.textContent = _formatValue(_derivations[name].value);
    }
  });
}

// Format a value for display
function _formatValue(v) {
  if (typeof v === 'number') {
    return Number.isInteger(v) ? v.toString() : v.toFixed(2);
  }
  if (Array.isArray(v)) {
    // Bosatsu list/enum
    if (v[0] === 0) return 'False/None/[]';
    if (v[0] === 1 && v.length === 1) return 'True';
    return JSON.stringify(v);
  }
  return String(v);
}

// Format derivation header for "Why?" display
function _formatDerivationHeader(d) {
  switch (d.type) {
    case 'assumption':
      return '<span class="name">' + d.name + '</span> = <span class="value">' + _formatValue(d.value) + '</span> <span class="tag">assumption</span>';
    case 'computed':
      return '<span class="name">' + d.name + '</span> = <span class="formula">' + d.formula + '</span> → <span class="value">' + _formatValue(d.value) + '</span>';
    case 'conditional':
      return '<span class="name">' + d.name + '</span> = <span class="value">' + _formatValue(d.value) + '</span> <span class="tag">conditional</span>';
    default:
      return d.name + ' = ' + _formatValue(d.value);
  }
}"""
  }

  /**
   * Generate UI initialization code.
   */
  private def generateUICode(
      analyses: List[DerivationAnalyzer.AnalyzedBinding],
      config: SimConfig
  ): String = {
    val assumptions = analyses.filter(_.kind == DerivationAnalyzer.Assumption)
    val computed = analyses.filter(_.kind != DerivationAnalyzer.Assumption)

    val whyButtons = if (config.showWhy) {
      computed.map { a =>
        s"""  addWhyButton("${a.name.asString}", "val-${a.name.asString}");"""
      }.mkString("\n")
    } else ""

    val whatIfToggles = if (config.showWhatIf) {
      assumptions.map { a =>
        val inputType = a.valueType match {
          case "number"  => "number"
          case "boolean" => "checkbox"
          case _         => "text"
        }
        s"""  addWhatIfToggle("${a.name.asString}", "$inputType", "what-if-toggles");"""
      }.mkString("\n")
    } else ""

    val sweepSliders = if (config.showSweeps) {
      assumptions.filter(_.valueType == "number").map { a =>
        s"""  addSweepSlider("${a.name.asString}", 0, 100, 1, 50, "parameter-sweeps");"""
      }.mkString("\n")
    } else ""

    s"""// Add "Why?" button next to a value display
function addWhyButton(valueName, elementId) {
  const container = document.getElementById(elementId);
  if (!container) return;

  const btn = document.createElement('button');
  btn.textContent = 'Why?';
  btn.className = 'why-button';
  btn.onclick = () => showWhyExplanation(valueName);
  container.parentElement.appendChild(btn);
}

// Show "Why?" modal with derivation chain
function showWhyExplanation(valueName) {
  const d = _getDerivation(valueName);
  if (!d) return;

  const html = _explainDerivation(d, 0);
  document.getElementById('why-explanation').innerHTML = html;
  document.getElementById('why-modal').classList.remove('hidden');
}

// Recursively explain derivation
function _explainDerivation(d, depth) {
  const marginLeft = depth * 20;
  const cls = d.type;
  const header = _formatDerivationHeader(d);
  let result = '<div class="derivation ' + cls + '" style="margin-left: ' + marginLeft + 'px">' + header + '</div>';

  if (d.deps && d.deps.length > 0) {
    d.deps.forEach(depName => {
      const dep = _getDerivation(depName);
      if (dep) {
        result += _explainDerivation(dep, depth + 1);
      }
    });
  }

  return result;
}

// Add "What if?" toggle for an assumption
function addWhatIfToggle(name, inputType, containerId) {
  const container = document.getElementById(containerId);
  if (!container) return;

  const div = document.createElement('div');
  div.className = 'what-if-toggle';

  if (inputType === 'checkbox') {
    div.innerHTML = '<label class="toggle-label"><span class="toggle-name">What if ' + name + ' were </span><button class="toggle-btn" data-value="true">true</button><button class="toggle-btn" data-value="false">false</button><span class="toggle-hint">?</span></label>';
    div.querySelectorAll('.toggle-btn').forEach(btn => {
      btn.onclick = () => {
        const newVal = btn.dataset.value === 'true';
        _setAssumption(name, newVal);
        _recompute();
      };
    });
  } else {
    div.innerHTML = '<label class="toggle-label"><span class="toggle-name">What if ' + name + ' were </span><input type="' + inputType + '" class="toggle-input" /><span class="toggle-hint">?</span></label>';
    const input = div.querySelector('input');
    input.oninput = () => {
      const newVal = inputType === 'number' ? parseFloat(input.value) : input.value;
      if (!isNaN(newVal) || inputType !== 'number') {
        _setAssumption(name, newVal);
        _recompute();
      }
    };
  }

  container.appendChild(div);
}

// Add parameter sweep slider
function addSweepSlider(name, min, max, step, initial, containerId) {
  const container = document.getElementById(containerId);
  if (!container) return;

  const div = document.createElement('div');
  div.className = 'parameter-sweep';
  div.innerHTML = '<div class="sweep-header"><span class="sweep-name">' + name + '</span><span class="sweep-value" id="sweep-val-' + name + '">' + initial + '</span></div><input type="range" class="sweep-slider" min="' + min + '" max="' + max + '" step="' + step + '" value="' + initial + '" /><div class="sweep-bounds"><span>' + min + '</span><span>' + max + '</span></div>';

  const slider = div.querySelector('input');
  slider.oninput = () => {
    const val = parseFloat(slider.value);
    document.getElementById('sweep-val-' + name).textContent = val;
    _setAssumption(name, val);
    _recompute();
  };

  container.appendChild(div);
}

// Initialize UI
function init() {
  // Create "Why?" modal
  const modal = document.createElement('div');
  modal.id = 'why-modal';
  modal.className = 'why-modal hidden';
  modal.innerHTML = '<div class="why-modal-content"><h3>Why this value?</h3><div id="why-explanation"></div><button id="why-modal-close" class="close-btn">Close</button></div>';
  document.body.appendChild(modal);

  document.getElementById('why-modal-close').onclick = () => {
    document.getElementById('why-modal').classList.add('hidden');
  };
  modal.onclick = (e) => {
    if (e.target === modal) modal.classList.add('hidden');
  };

  // Create value displays
  const controlsDiv = document.getElementById('controls');
  if (controlsDiv) {
    Object.keys(_derivations).forEach(name => {
      const d = _derivations[name];
      const div = document.createElement('div');
      div.className = 'control-group';
      div.innerHTML = '<span class="control-label">' + name + ':</span> <span class="value-display" id="val-' + name + '">' + _formatValue(d.value) + '</span>';
      controlsDiv.appendChild(div);
    });
  }

  // Add "Why?" buttons
$whyButtons

  // Add "What if?" toggles
$whatIfToggles

  // Add parameter sweep sliders
$sweepSliders

  // Initial computation
  _recompute();
}"""
  }

  /**
   * Topologically sort bindings by dependencies.
   */
  private def topologicalSort(
      analyses: List[DerivationAnalyzer.AnalyzedBinding]
  ): List[DerivationAnalyzer.AnalyzedBinding] = {
    val byName = analyses.map(a => a.name -> a).toMap

    var result = List.empty[DerivationAnalyzer.AnalyzedBinding]
    var visited = Set.empty[Bindable]

    def visit(a: DerivationAnalyzer.AnalyzedBinding): Unit = {
      if (!visited.contains(a.name)) {
        visited += a.name
        a.dependencies.flatMap(byName.get).foreach(visit)
        result = a :: result
      }
    }

    analyses.foreach(visit)
    result.reverse
  }

  /**
   * Escape a string for use in JavaScript.
   */
  private def escapeJs(s: String): String =
    s.replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")
}
