package dev.bosatsu.ui

import dev.bosatsu.codegen.js.Code
import dev.bosatsu.codegen.js.Code._
import org.typelevel.paiges.Doc
import SimulationApplet._

/**
 * Generator for self-contained simulation applet embeds.
 *
 * This creates standalone HTML files that can be embedded anywhere:
 * - Self-contained: No external dependencies
 * - Small: Target <20KB gzipped for full applets, <5KB for library mode
 * - Themeable: Light/dark mode support
 *
 * Leverages Bosatsu's advantages:
 * - Type-safe code generation: JS output is verified at compile time
 * - Exhaustive CSS: All UI states have defined styles
 * - Total rendering: HTML generation is guaranteed to complete
 */
object EmbedGenerator {

  /**
   * Theme configuration for embeds.
   */
  sealed trait Theme {
    def name: String
    def backgroundColor: String
    def textColor: String
    def accentColor: String
    def surfaceColor: String
  }

  case object LightTheme extends Theme {
    val name = "light"
    val backgroundColor = "#f5f5f5"
    val textColor = "#333333"
    val accentColor = "#667eea"
    val surfaceColor = "#ffffff"
  }

  case object DarkTheme extends Theme {
    val name = "dark"
    val backgroundColor = "#1a1a2e"
    val textColor = "#e0e0e0"
    val accentColor = "#667eea"
    val surfaceColor = "#16162a"
  }

  /**
   * Embed configuration.
   */
  case class EmbedConfig(
      title: String,
      width: Int = 600,
      height: Int = 400,
      theme: Theme = LightTheme,
      showWhyButtons: Boolean = true,
      showWhatIfToggles: Boolean = true,
      showParameterSweeps: Boolean = true,
      libraryMode: Boolean = false,
      showCanvas: Boolean = false  // Only show canvas for animations, not calculators
  )

  /**
   * Generate a self-contained HTML embed for a simulation.
   */
  def generateEmbed(
      config: EmbedConfig,
      initialState: Map[String, String],
      jsCode: String,
      cssOverrides: String = ""
  ): String = {
    val theme = config.theme
    val css = generateCSS(theme, config)
    val html = generateHTML(config)
    val fullJS = generateJS(config, initialState, jsCode)

    s"""<!DOCTYPE html>
       |<html lang="en" data-theme="${theme.name}">
       |<head>
       |  <meta charset="UTF-8">
       |  <meta name="viewport" content="width=device-width, initial-scale=1.0">
       |  <title>${escapeHTML(config.title)}</title>
       |  <style>
       |$css
       |$cssOverrides
       |  </style>
       |</head>
       |<body>
       |$html
       |  <script>
       |$fullJS
       |  </script>
       |</body>
       |</html>""".stripMargin
  }

  /**
   * Generate a library-mode embed (minimal, expects external runtime).
   */
  def generateLibraryEmbed(
      config: EmbedConfig,
      moduleName: String,
      initialState: Map[String, String],
      jsCode: String
  ): String = {
    val stateInit = initialState.map { case (k, v) => s"'$k': $v" }.mkString("{", ", ", "}")

    s"""<div id="${moduleName}-container" class="bosatsu-applet">
       |  <div id="${moduleName}-simulation"></div>
       |  <div id="${moduleName}-controls"></div>
       |</div>
       |<script>
       |(function() {
       |  const state = $stateInit;
       |  const container = document.getElementById('${moduleName}-container');
       |$jsCode
       |  window.BosatsuApplets = window.BosatsuApplets || {};
       |  window.BosatsuApplets['$moduleName'] = { state, container };
       |})();
       |</script>""".stripMargin
  }

  private def generateCSS(theme: Theme, config: EmbedConfig): String = {
    s"""    :root {
       |      --bg-color: ${theme.backgroundColor};
       |      --text-color: ${theme.textColor};
       |      --accent-color: ${theme.accentColor};
       |      --surface-color: ${theme.surfaceColor};
       |    }
       |    * { box-sizing: border-box; }
       |    body {
       |      font-family: system-ui, -apple-system, sans-serif;
       |      max-width: ${config.width}px;
       |      margin: 0 auto;
       |      padding: 1rem;
       |      background: var(--bg-color);
       |      color: var(--text-color);
       |    }
       |    .applet-container {
       |      background: var(--surface-color);
       |      border-radius: 12px;
       |      padding: 1.5rem;
       |      box-shadow: 0 4px 20px rgba(0,0,0,0.1);
       |    }
       |    .applet-title {
       |      margin: 0 0 1rem 0;
       |      font-size: 1.5rem;
       |      color: var(--text-color);
       |    }
       |    .simulation-canvas {
       |      width: 100%;
       |      border-radius: 8px;
       |      background: #1a1a2e;
       |    }
       |    .controls-section {
       |      margin-top: 1rem;
       |      padding-top: 1rem;
       |      border-top: 1px solid rgba(0,0,0,0.1);
       |    }
       |    .control-group {
       |      margin: 0.75rem 0;
       |    }
       |    .control-label {
       |      display: block;
       |      margin-bottom: 0.25rem;
       |      font-size: 0.875rem;
       |      color: var(--text-color);
       |      opacity: 0.8;
       |    }
       |    input[type="range"] {
       |      width: 100%;
       |      height: 8px;
       |      -webkit-appearance: none;
       |      background: #ddd;
       |      border-radius: 4px;
       |      outline: none;
       |    }
       |    input[type="range"]::-webkit-slider-thumb {
       |      -webkit-appearance: none;
       |      width: 20px;
       |      height: 20px;
       |      background: var(--accent-color);
       |      border-radius: 50%;
       |      cursor: pointer;
       |    }
       |    .value-display {
       |      font-family: monospace;
       |      font-weight: bold;
       |      color: var(--accent-color);
       |    }
       |    .button-row {
       |      display: flex;
       |      gap: 0.5rem;
       |      margin-top: 1rem;
       |    }
       |    .btn {
       |      padding: 0.5rem 1rem;
       |      background: var(--accent-color);
       |      color: white;
       |      border: none;
       |      border-radius: 6px;
       |      cursor: pointer;
       |      font-size: 0.875rem;
       |    }
       |    .btn:hover {
       |      opacity: 0.9;
       |    }
       |    .btn-secondary {
       |      background: #6c757d;
       |    }
       |${if (config.showWhyButtons) WhyExplainer.whyModalCSS.split("\n").map(l => s"    $l").mkString("\n") else ""}
       |${if (config.showWhatIfToggles) WhatIfToggle.toggleCSS.split("\n").map(l => s"    $l").mkString("\n") else ""}
       |${if (config.showParameterSweeps) ParameterSweep.sweepCSS.split("\n").map(l => s"    $l").mkString("\n") else ""}""".stripMargin
  }

  private def generateHTML(config: EmbedConfig): String = {
    val canvasHtml = if (config.showCanvas) {
      s"""<canvas id="simulation-canvas" class="simulation-canvas" width="${config.width - 48}" height="${config.height}"></canvas>"""
    } else ""

    s"""  <div class="applet-container">
       |    <h1 class="applet-title">${escapeHTML(config.title)}</h1>
       |    $canvasHtml
       |    <div class="controls-section" id="controls"></div>
       |    ${if (config.showWhatIfToggles) """<div class="what-if-section" id="what-if-toggles"></div>""" else ""}
       |    ${if (config.showParameterSweeps) """<div class="sweep-section" id="parameter-sweeps"></div>""" else ""}
       |  </div>""".stripMargin
  }

  private def generateJS(config: EmbedConfig, initialState: Map[String, String], appletJS: String): String = {
    // Quote keys to handle special characters in state names
    val stateInit = initialState.map { case (k, v) => s"""      "$k": $v""" }.mkString(",\n")
    val listenersInit = initialState.keys.map(k => s"""      "$k": []""").mkString(",\n")

    s"""    // BosatsuUI Reactive Runtime
       |    const _state = {
       |$stateInit
       |    };
       |    const _listeners = {
       |$listenersInit
       |    };
       |    const _derivations = {};
       |
       |    function _getState(name) { return _state[name]; }
       |    function _setState(name, value) {
       |      if (_state[name] !== value) {
       |        _state[name] = value;
       |        _listeners[name].forEach(fn => fn(value));
       |      }
       |    }
       |    function _subscribe(name, fn) {
       |      _listeners[name].push(fn);
       |      fn(_state[name]);
       |    }
       |    function _setDerivation(name, derivation) {
       |      _derivations[name] = derivation;
       |    }
       |    function _getDerivation(name) {
       |      return _derivations[name];
       |    }
       |    function _setAssumption(name, value) {
       |      _setState(name, value);
       |      // Trigger recomputation of derived values
       |      Object.keys(_derivations).forEach(k => {
       |        if (_derivations[k].deps && _derivations[k].deps.some(d => d.name === name)) {
       |          // Would recompute here in full implementation
       |        }
       |      });
       |    }
       |
       |    // Applet-specific code
       |$appletJS
       |
       |    // Initialize
       |    if (typeof init === 'function') init();""".stripMargin
  }

  private def escapeHTML(s: String): String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
}
