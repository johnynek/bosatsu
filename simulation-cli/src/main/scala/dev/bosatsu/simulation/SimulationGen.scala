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

    // 4. Skip JsGen's const declarations - they conflict with _recompute's state-based approach
    // JsGen inlines values (e.g., `const monthly_rate = 7 / 1200`) which breaks interactive "What if?"
    // Instead, _recompute handles all computation dynamically from state

    // 5. Combine all JS (without JsGen computation code)
    val fullJs =
      s"""${JsGen.runtimeCode}

// Derivation state tracking
$derivationState

// Recomputation logic (replaces JsGen's const declarations)
$recomputeJs

// UI initialization
$uiJs
"""

    // 5. Build initial state from assumptions (extract actual values from computeJs)
    val valuePattern = """const\s+(\w+)\s*=\s*([^;]+);""".r
    val valueMap = valuePattern.findAllMatchIn(computeJs).map { m =>
      m.group(1) -> m.group(2).trim
    }.toMap

    val initialState = analyses
      .filter(_.kind == DerivationAnalyzer.Assumption)
      .map { a =>
        val name = a.name.asString
        val escaped = JsGen.escape(a.name).name
        val value = valueMap.getOrElse(escaped, "undefined")
        name -> value
      }
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

    // Note: _derivations object is created by EmbedGenerator runtime
    // We populate it here with our analyzed derivations
    val populateStmts = analyses.map { a =>
      val depsArray = a.dependencies.map(d => s""""${d.asString}"""").mkString("[", ", ", "]")
      val kindStr = a.kind match {
        case DerivationAnalyzer.Assumption  => "assumption"
        case DerivationAnalyzer.Computation => "computed"
        case DerivationAnalyzer.Conditional => "conditional"
      }
      s"""_derivations["${a.name.asString}"] = {
  type: "$kindStr",
  name: "${a.name.asString}",
  formula: "${escapeJs(a.formula)}",
  valueType: "${a.valueType}",
  deps: $depsArray,
  value: undefined
};"""
    }

    s"""// Populate derivations (object created by EmbedGenerator runtime)
${populateStmts.mkString("\n")}"""
  }

  /**
   * Generate the recompute function that updates derived values.
   */
  private def generateRecomputeFunction(
      analyses: List[DerivationAnalyzer.AnalyzedBinding]
  ): String = {
    // Sort bindings topologically by dependencies
    val sorted = topologicalSort(analyses)

    // Separate assumptions from computations
    val assumptions = sorted.filter(_.kind == DerivationAnalyzer.Assumption)
    val computations = sorted.filter(_.kind != DerivationAnalyzer.Assumption)

    // Read assumptions from state
    val readStmts = assumptions.map { a =>
      val name = a.name.asString
      val escaped = JsGen.escape(a.name).name
      s"  const $escaped = _getState('$name');"
    }

    // Compute derived values (in topological order)
    val computeStmts = computations.map { a =>
      val escaped = JsGen.escape(a.name).name
      val formula = a.formula
      s"  const $escaped = ${formulaToJs(formula)};"
    }

    // Update derivations (and state for assumptions only - computed values don't have listeners)
    val updateStmts = sorted.map { a =>
      val name = a.name.asString
      val escaped = JsGen.escape(a.name).name
      // Only call _setState for assumptions - they have listeners
      // Computed values just update their derivation value
      if (a.kind == DerivationAnalyzer.Assumption) {
        s"""  _derivations["$name"].value = $escaped;
  _setState("$name", $escaped);"""
      } else {
        s"""  _derivations["$name"].value = $escaped;"""
      }
    }

    s"""function _recompute() {
  // Read current assumption values from state
${readStmts.mkString("\n")}

  // Compute derived values
${computeStmts.mkString("\n")}

  // Update all derivations and state
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

// Substitute variable names in formula with their actual values
function _substituteValues(formula) {
  let result = formula;
  Object.keys(_derivations).forEach(name => {
    const d = _derivations[name];
    if (d.value !== undefined) {
      // Replace whole-word matches only using word boundaries
      const regex = new RegExp('\\\\b' + name + '\\\\b', 'g');
      result = result.replace(regex, _formatValue(d.value));
    }
  });
  return result;
}

// Format derivation header for "Why?" display with value substitution
function _formatDerivationHeader(d) {
  switch (d.type) {
    case 'assumption':
      return '<span class="name">' + d.name + '</span> = <span class="value">' + _formatValue(d.value) + '</span> <span class="tag">input</span>';
    case 'computed':
    case 'conditional':
      // Show: name = formula = substituted = value
      const substituted = _substituteValues(d.formula);
      const isSubstituted = substituted !== d.formula;
      let result = '<span class="name">' + d.name + '</span> = <span class="formula">' + d.formula + '</span>';
      if (isSubstituted) {
        result += '<br>&nbsp;&nbsp;= <span class="substituted">' + substituted + '</span>';
      }
      result += '<br>&nbsp;&nbsp;= <span class="value">' + _formatValue(d.value) + '</span>';
      if (d.type === 'conditional') {
        result += ' <span class="tag">conditional</span>';
      }
      return result;
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
   * Convert a Bosatsu formula to JavaScript.
   *
   * NOTE: This handles Predef method-call syntax (e.g., a.add(b)).
   * For Numeric module operators (*.  +.  -.  /.), use JsGen's proper
   * compilation pipeline instead - those operators are handled correctly
   * by NumericExternal intrinsics in JsGen.
   *
   * See CLAUDE.md: "Never Treat Symptoms with String Replacement"
   */
  private def formulaToJs(formula: String): String = {
    var result = formula
    var changed = true
    var iterations = 0
    while (changed && iterations < 20) {
      val before = result
      result = replaceFunction(result, "add", (a, b) => s"($a + $b)")
      result = replaceFunction(result, "sub", (a, b) => s"($a - $b)")
      result = replaceFunction(result, "times", (a, b) => s"($a * $b)")
      result = replaceFunction(result, "div", (a, b) => s"Math.trunc($a / $b)")
      changed = before != result
      iterations += 1
    }
    result
  }

  /**
   * Replace a function call with two arguments using a custom replacement.
   */
  private def replaceFunction(s: String, fnName: String, replacement: (String, String) => String): String = {
    val fnPattern = (fnName + "\\(").r
    var result = s
    var idx = 0
    while (idx < result.length) {
      fnPattern.findFirstMatchIn(result.substring(idx)) match {
        case Some(m) =>
          val start = idx + m.start
          val argsStart = start + fnName.length + 1
          // Find matching closing paren and split args
          var depth = 1
          var i = argsStart
          var commaPos = -1
          while (i < result.length && depth > 0) {
            result.charAt(i) match {
              case '(' => depth += 1
              case ')' => depth -= 1
              case ',' if depth == 1 && commaPos == -1 => commaPos = i
              case _ =>
            }
            i += 1
          }
          if (depth == 0 && commaPos > 0) {
            val arg1 = result.substring(argsStart, commaPos).trim
            val arg2 = result.substring(commaPos + 1, i - 1).trim
            val replaced = replacement(arg1, arg2)
            result = result.substring(0, start) + replaced + result.substring(i)
            idx = start + replaced.length
          } else {
            idx = idx + m.end
          }
        case None =>
          idx = result.length
      }
    }
    result
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

  /**
   * Generate HTML for a function-based simulation.
   *
   * This is the principled approach where:
   * - The simulation is a pure function
   * - Function parameters ARE the inputs
   * - Return type fields ARE the outputs
   * - TypedExpr.freeVarsSet provides dependency analysis
   */
  def generateFunctionBased(
      funcName: String,
      funcParams: List[(String, String)],
      analyses: List[DerivationAnalyzer.AnalyzedBinding],
      computeJs: String,
      config: SimConfig
  ): String = {
    // 1. Generate derivation state (for input params)
    val derivationState = generateDerivationState(analyses)

    // 2. Generate JavaScript that calls the function
    val callArgs = funcParams.map { case (name, _) => s"""_getState("$name")""" }.mkString(", ")

    val recomputeJs = s"""
// The compiled function from Bosatsu
$computeJs

// Recompute by calling the function with current input values
function _recompute() {
  // Call the function with input values
  const result = $funcName($callArgs);

  // Update result displays
  console.log("$funcName result:", result);

  // For structs, result is an object or array
  // Try to display result fields
  const resultDiv = document.getElementById('results');
  if (resultDiv) {
    if (typeof result === 'object' && result !== null) {
      let html = '';
      // Handle struct (named fields) or tuple (array)
      if (Array.isArray(result)) {
        result.forEach((val, i) => {
          html += '<div class="result-item"><span class="result-label">field_' + i + ':</span> <span class="result-value">' + _formatValue(val) + '</span></div>';
        });
      } else {
        Object.entries(result).forEach(([key, val]) => {
          html += '<div class="result-item"><span class="result-label">' + key + ':</span> <span class="result-value">' + _formatValue(val) + '</span></div>';
        });
      }
      resultDiv.innerHTML = html;
    } else {
      resultDiv.textContent = _formatValue(result);
    }
  }

  // Update input derivations
${funcParams.map { case (name, _) =>
      s"""  _derivations["$name"].value = _getState("$name");"""
    }.mkString("\n")}
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
}"""

    // 3. Generate UI for inputs
    val uiJs = generateFunctionBasedUI(funcParams, config)

    // 4. Combine all JS
    val fullJs = s"""${JsGen.runtimeCode}

// Derivation state tracking for inputs
$derivationState

// Function and recomputation
$recomputeJs

// UI initialization
$uiJs
"""

    // 5. Build initial state from function params (using defaults from config eventually)
    // For now, use simple defaults
    val initialState = funcParams.map { case (name, tpe) =>
      val defaultVal = tpe match {
        case "Int" => "0"
        case "Double" => "0.0"
        case "Bool" => "true"
        case _ => "0"
      }
      name -> defaultVal
    }.toMap

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
   * Generate HTML for a function-based simulation using config values.
   *
   * This is the fully principled approach where:
   * - The simulation is a pure function
   * - UI metadata (labels, ranges, defaults) come from the config file
   * - No hardcoded values
   */
  def generateFunctionBasedWithConfig(
      funcName: String,
      funcParams: List[(String, String)],
      simConfig: ConfigExtractor.SimConfig,
      analyses: List[DerivationAnalyzer.AnalyzedBinding],
      computeJs: String,
      config: SimConfig
  ): String = {
    // 1. Generate derivation state (for input params)
    val derivationState = generateDerivationState(analyses)

    // 2. Generate JavaScript that calls the function
    val callArgs = funcParams.map { case (name, _) => s"""_getState("$name")""" }.mkString(", ")

    // 3. Build output field info from config
    val outputFields = simConfig.outputs.map { case (name, outputConfig) =>
      (name, outputConfig.label, outputConfig.format, outputConfig.primary)
    }

    val recomputeJs = s"""
// The compiled function from Bosatsu
$computeJs

// Recompute by calling the function with current input values
function _recompute() {
  // Call the function with input values
  const result = $funcName($callArgs);

  // Update result displays
  console.log("$funcName result:", result);

  // Update output displays
${outputFields.zipWithIndex.map { case ((name, label, format, primary), idx) =>
      s"""  const el_$name = document.getElementById('result-$name');
  if (el_$name) {
    const value = Array.isArray(result) ? result[$idx] : result.$name;
    el_$name.querySelector('.result-value').textContent = _formatOutput(value, "$format");
  }"""
    }.mkString("\n")}

  // Update input derivations
${funcParams.map { case (name, _) =>
      s"""  if (_derivations["$name"]) _derivations["$name"].value = _getState("$name");"""
    }.mkString("\n")}
}

// Format an output value based on format type
function _formatOutput(v, format) {
  if (v === undefined || v === null) return '—';
  switch (format) {
    case 'currency':
      return '$$' + _formatNumber(v);
    case 'percent':
      return v + '%';
    default:
      return _formatNumber(v);
  }
}

// Format a number with commas
function _formatNumber(v) {
  if (typeof v !== 'number') return String(v);
  return v.toLocaleString();
}

// Format a value for display
function _formatValue(v) {
  if (typeof v === 'number') {
    return Number.isInteger(v) ? v.toLocaleString() : v.toFixed(2);
  }
  if (Array.isArray(v)) {
    // Bosatsu list/enum
    if (v[0] === 0) return 'False/None/[]';
    if (v[0] === 1 && v.length === 1) return 'True';
    return JSON.stringify(v);
  }
  return String(v);
}"""

    // 4. Generate UI with config-driven inputs and outputs
    val uiJs = generateConfigDrivenUI(simConfig, config)

    // 5. Combine all JS
    val fullJs = s"""${JsGen.runtimeCode}

// Derivation state tracking for inputs
$derivationState

// Function and recomputation
$recomputeJs

// UI initialization
$uiJs
"""

    // 6. Build initial state from config defaults
    val initialState = simConfig.inputs.map { case (name, inputConfig) =>
      name -> inputConfig.defaultValue.toString
    }.toMap

    // 7. Generate embed config
    val embedConfig = EmbedGenerator.EmbedConfig(
      title = config.title,
      theme = config.theme,
      showWhyButtons = config.showWhy,
      showWhatIfToggles = config.showWhatIf,
      showParameterSweeps = config.showSweeps
    )

    // 8. Generate HTML
    EmbedGenerator.generateEmbed(embedConfig, initialState, fullJs)
  }

  /**
   * Generate UI initialization using config values for labels, ranges, and defaults.
   */
  private def generateConfigDrivenUI(
      simConfig: ConfigExtractor.SimConfig,
      config: SimConfig
  ): String = {
    // Generate input controls with proper labels, ranges, and defaults
    val inputControls = simConfig.inputs.map { case (name, inputConfig) =>
      val label = escapeJs(inputConfig.label)
      s"""  addConfiguredInput("$name", "$label", ${inputConfig.minValue}, ${inputConfig.maxValue}, ${inputConfig.step}, ${inputConfig.defaultValue}, "inputs");"""
    }.mkString("\n")

    // Generate output displays with proper labels
    val outputDisplays = simConfig.outputs.map { case (name, outputConfig) =>
      val label = escapeJs(outputConfig.label)
      val primary = if (outputConfig.primary) "true" else "false"
      s"""  addOutputDisplay("$name", "$label", $primary, "results");"""
    }.mkString("\n")

    s"""// Add a configured input control with label, range, and default
function addConfiguredInput(name, label, min, max, step, defaultVal, containerId) {
  const container = document.getElementById(containerId);
  if (!container) return;

  const div = document.createElement('div');
  div.className = 'control-group';

  div.innerHTML = '<label class="control-label">' + label + ': <span class="value-display" id="' + name + '-value">' + defaultVal.toLocaleString() + '</span></label>' +
    '<input type="range" id="' + name + '-slider" min="' + min + '" max="' + max + '" step="' + step + '" value="' + defaultVal + '">';

  const slider = div.querySelector('input');
  slider.oninput = () => {
    const val = parseInt(slider.value);
    document.getElementById(name + '-value').textContent = val.toLocaleString();
    _setState(name, val);
    _recompute();
  };

  container.appendChild(div);
}

// Add an output display with label
function addOutputDisplay(name, label, isPrimary, containerId) {
  const container = document.getElementById(containerId);
  if (!container) return;

  const div = document.createElement('div');
  div.id = 'result-' + name;
  div.className = isPrimary ? 'result-item primary' : 'result-item';
  div.innerHTML = '<span class="result-label">' + label + ':</span> <span class="result-value">—</span>';

  container.appendChild(div);
}

// Track current assumption variants
const _assumptions = {};

// Add assumption toggle buttons
function addAssumptionToggle(name, description, defaultVariant, buttonsHtml, containerId) {
  const container = document.getElementById(containerId);
  if (!container) return;

  // Set default variant
  _assumptions[name] = defaultVariant;

  const div = document.createElement('div');
  div.className = 'assumption-toggle';
  div.innerHTML = '<div class="assumption-label">' + description + '</div>' +
    '<div class="variant-buttons">' + buttonsHtml + '</div>';

  // Add click handlers to variant buttons
  div.querySelectorAll('.variant-btn').forEach(btn => {
    btn.addEventListener('click', () => {
      const variant = btn.dataset.variant;
      _assumptions[name] = variant;

      // Update active button state
      div.querySelectorAll('.variant-btn').forEach(b => b.classList.remove('active'));
      btn.classList.add('active');

      // Recompute with new variant
      _recompute();
    });
  });

  container.appendChild(div);
}

// Get the current variant suffix for an assumption
function getVariant(name) {
  return _assumptions[name] || '';
}

// Initialize UI
function init() {
  // Create layout sections
  const main = document.querySelector('main');
  if (main) {
    // Clear existing content
    main.innerHTML = '';

    // Inputs section
    const inputsDiv = document.createElement('div');
    inputsDiv.id = 'inputs';
    inputsDiv.className = 'inputs-section';
    inputsDiv.innerHTML = '<h3>Inputs</h3>';
    main.appendChild(inputsDiv);

    // Results section
    const resultsDiv = document.createElement('div');
    resultsDiv.id = 'results';
    resultsDiv.className = 'results-section';
    resultsDiv.innerHTML = '<h3>Results</h3>';
    main.appendChild(resultsDiv);
  }

  // Add configured input controls
$inputControls

  // Add output displays
$outputDisplays

${if (simConfig.assumptions.nonEmpty) s"""
  // Add assumption toggles section
  const assumptionsDiv = document.createElement('div');
  assumptionsDiv.id = 'assumptions';
  assumptionsDiv.className = 'assumptions-section';
  assumptionsDiv.innerHTML = '<h3>What if...?</h3>';
  document.querySelector('main').appendChild(assumptionsDiv);

  // Add assumption toggle buttons
${generateAssumptionToggles(simConfig.assumptions)}
""" else ""}

  // Initial computation
  _recompute();
}"""
  }

  /**
   * Generate JavaScript for assumption toggle buttons.
   * Each assumption has multiple variants (function suffixes) that can be selected.
   */
  private def generateAssumptionToggles(assumptions: List[ConfigExtractor.AssumptionConfig]): String = {
    assumptions.map { assumption =>
      val name = escapeJs(assumption.name)
      val description = escapeJs(assumption.description)
      val defaultVariant = assumption.variants.headOption.map(_._2).getOrElse("")
      val buttons = assumption.variants.map { case (label, suffix) =>
        val escaped = escapeJs(label)
        val escapedSuffix = escapeJs(suffix)
        s"""<button class="variant-btn${if (suffix == defaultVariant) " active" else ""}" data-assumption="$name" data-variant="$escapedSuffix">$escaped</button>"""
      }.mkString("")

      s"""  addAssumptionToggle("$name", "$description", "$defaultVariant", `$buttons`, "assumptions");"""
    }.mkString("\n")
  }

  /**
   * Generate UI initialization for function-based simulation.
   */
  private def generateFunctionBasedUI(
      funcParams: List[(String, String)],
      config: SimConfig
  ): String = {
    // Generate input controls for each parameter
    val inputControls = funcParams.map { case (name, tpe) =>
      val inputType = tpe match {
        case "Int" | "Double" => "number"
        case "Bool" => "checkbox"
        case _ => "text"
      }
      s"""  addInputControl("$name", "$inputType", "inputs");"""
    }.mkString("\n")

    s"""// Add input control for a function parameter
function addInputControl(name, inputType, containerId) {
  const container = document.getElementById(containerId);
  if (!container) return;

  const div = document.createElement('div');
  div.className = 'input-control';

  if (inputType === 'checkbox') {
    div.innerHTML = '<label class="input-label"><span class="input-name">' + name + '</span><input type="checkbox" class="input-checkbox" checked /></label>';
    const input = div.querySelector('input');
    input.onchange = () => {
      _setState(name, input.checked);
      _recompute();
    };
  } else {
    div.innerHTML = '<label class="input-label"><span class="input-name">' + name + '</span><input type="' + inputType + '" class="input-field" value="0" /></label>';
    const input = div.querySelector('input');
    input.oninput = () => {
      const val = inputType === 'number' ? parseFloat(input.value) : input.value;
      if (!isNaN(val) || inputType !== 'number') {
        _setState(name, val);
        _recompute();
      }
    };
  }

  container.appendChild(div);
}

// Initialize UI
function init() {
  // Create inputs section
  const main = document.querySelector('main');
  if (main) {
    const inputsDiv = document.createElement('div');
    inputsDiv.id = 'inputs';
    inputsDiv.className = 'inputs-section';
    inputsDiv.innerHTML = '<h3>Inputs</h3>';
    main.insertBefore(inputsDiv, main.firstChild);

    const resultsDiv = document.createElement('div');
    resultsDiv.id = 'results';
    resultsDiv.className = 'results-section';
    resultsDiv.innerHTML = '<h3>Results</h3><p>Adjust inputs to see results.</p>';
    main.appendChild(resultsDiv);
  }

  // Add input controls
$inputControls

  // Initial computation
  _recompute();
}"""
  }
}
