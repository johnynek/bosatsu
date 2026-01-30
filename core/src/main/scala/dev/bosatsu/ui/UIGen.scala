package dev.bosatsu.ui

import dev.bosatsu.codegen.js.{Code, JsGen}
import dev.bosatsu.Identifier.Bindable

/**
 * Generator for reactive UI HTML with direct DOM updates.
 *
 * This generates self-contained HTML from Bosatsu/UI components.
 * Uses UIAnalyzer to extract state→DOM bindings at compile time.
 *
 * The generated HTML includes:
 * - Initial DOM structure from VNode tree
 * - State management runtime
 * - Binding map for O(1) direct DOM updates
 * - Event handler registration
 */
object UIGen {

  /**
   * Configuration for UI generation.
   */
  case class UIConfig(
      title: String,
      theme: String = "light",
      includeSourceMap: Boolean = false
  )

  /**
   * Generate complete HTML for a UI component.
   *
   * @param vnodeJs The compiled JavaScript that produces the VNode tree
   * @param analysis The UIAnalyzer result with bindings and event handlers
   * @param config Generation configuration
   * @return Complete HTML string
   */
  def generate[A](
      vnodeJs: String,
      analysis: UIAnalyzer.UIAnalysis[A],
      config: UIConfig
  ): String = {
    val bindingsJs = UIAnalyzer.bindingsToJs(analysis.bindings)
    val eventHandlersJs = generateEventHandlersJs(analysis.eventHandlers)
    val runtimeJs = generateRuntimeJs()
    val stylesCss = generateStyles(config.theme)

    s"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>${escapeHtml(config.title)}</title>
  <style>
$stylesCss
  </style>
</head>
<body>
  <div id="app"></div>
  <script>
${JsGen.runtimeCode}

// State management
$runtimeJs

// Event handlers
$eventHandlersJs

// Bindings map for O(1) updates
const _bindings = $bindingsJs;

// Compiled VNode tree
const _vnode = (function() {
  $vnodeJs
  return main;
})();

// Initialize app
function init() {
  const app = document.getElementById('app');
  app.appendChild(_renderVNode(_vnode));
  _initEventHandlers();
}

document.addEventListener('DOMContentLoaded', init);
  </script>
</body>
</html>"""
  }

  /**
   * Generate HTML for a simple counter demo (for testing).
   * This uses hardcoded structure until the full pipeline is wired.
   */
  def generateCounterDemo(config: UIConfig): String = {
    val stylesCss = generateStyles(config.theme)

    s"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>${escapeHtml(config.title)}</title>
  <style>
$stylesCss
  </style>
</head>
<body>
  <div id="app">
    <div class="card">
      <h1>Counter</h1>
      <div class="display">
        <span id="count-display" data-bosatsu-id="bosatsu-0">0</span>
      </div>
      <div class="buttons">
        <button id="decrement-btn">-</button>
        <button id="increment-btn">+</button>
      </div>
    </div>
  </div>
  <script>
// State management
const _state = { count: 0 };

// Binding map: state path -> DOM updates
const _bindings = {
  "count": [{
    elementId: "count-display",
    property: "textContent",
    conditional: false
  }]
};

// Element cache for O(1) lookups
const _elements = {};

// Initialize element cache
function _initElements() {
  Object.values(_bindings).flat().forEach(binding => {
    _elements[binding.elementId] = document.getElementById(binding.elementId);
  });
}

// Read state
function read(path) {
  return _state[path];
}

// Write state and trigger updates
function write(path, value) {
  _state[path] = value;
  _updateBindings(path);
}

// Update DOM for a state path
function _updateBindings(path) {
  const bindings = _bindings[path];
  if (!bindings) return;

  bindings.forEach(binding => {
    const el = _elements[binding.elementId];
    if (!el) return;

    const value = _state[path];
    switch (binding.property) {
      case "textContent":
        el.textContent = String(value);
        break;
      case "className":
        el.className = String(value);
        break;
      case "value":
        el.value = String(value);
        break;
    }
  });
}

// Event handlers
function increment() {
  write("count", read("count") + 1);
}

function decrement() {
  write("count", read("count") - 1);
}

// Initialize
document.addEventListener('DOMContentLoaded', () => {
  _initElements();

  document.getElementById('increment-btn').onclick = increment;
  document.getElementById('decrement-btn').onclick = decrement;
});
  </script>
</body>
</html>"""
  }

  /**
   * Generate event handler registration JavaScript.
   */
  private def generateEventHandlersJs[A](handlers: List[UIAnalyzer.EventBinding[A]]): String = {
    val registrations = handlers.map { h =>
      val eventType = h.eventType
      val elementId = h.elementId
      // Handler would be compiled to JS function
      s"""_registerHandler("$elementId", "$eventType", function(e) {
  // Compiled handler
});"""
    }
    s"""const _eventHandlers = {};

function _registerHandler(elementId, eventType, handler) {
  if (!_eventHandlers[elementId]) _eventHandlers[elementId] = {};
  _eventHandlers[elementId][eventType] = handler;
}

function _initEventHandlers() {
  Object.entries(_eventHandlers).forEach(([elementId, events]) => {
    const el = document.getElementById(elementId) ||
               document.querySelector('[data-bosatsu-id="' + elementId + '"]');
    if (el) {
      Object.entries(events).forEach(([eventType, handler]) => {
        el.addEventListener(eventType, handler);
      });
    }
  });
}

${registrations.mkString("\n")}"""
  }

  /**
   * Generate runtime JavaScript for state management and rendering.
   */
  private def generateRuntimeJs(): String = {
    """// State storage
const _state = {};

// Element cache for O(1) lookups
const _elements = {};

// Read state
function read(stateId) {
  const state = _state[stateId];
  return state ? state.value : undefined;
}

// Write state and trigger binding updates
function write(stateId, value) {
  const state = _state[stateId];
  if (state) {
    state.value = value;
    _updateBindings(stateId);
  }
}

// Register a state
function _registerState(id, initialValue) {
  _state[id] = { id: id, value: initialValue };
}

// Update DOM for state changes
function _updateBindings(stateId) {
  const bindings = _bindings[stateId];
  if (!bindings) return;

  bindings.forEach(binding => {
    const el = _elements[binding.elementId] ||
               document.querySelector('[data-bosatsu-id="' + binding.elementId + '"]');
    if (!el) return;

    _elements[binding.elementId] = el;
    const value = read(stateId);

    switch (binding.property) {
      case "textContent":
        el.textContent = String(value);
        break;
      case "className":
        el.className = String(value);
        break;
      case "value":
        el.value = String(value);
        break;
    }
  });
}

// Render VNode to DOM
function _renderVNode(vnode) {
  if (!vnode) return document.createTextNode('');

  switch (vnode.type) {
    case "text":
      return document.createTextNode(vnode.text || '');

    case "element":
      const el = document.createElement(vnode.tag || 'div');

      // Apply props
      if (vnode.props) {
        // Props is array of [key, value] tuples
        const propsArray = Array.isArray(vnode.props) ? vnode.props : [];
        propsArray.forEach(prop => {
          if (Array.isArray(prop) && prop.length >= 2) {
            const [key, value] = prop;
            if (key.startsWith('data-')) {
              el.setAttribute(key, value);
            } else if (key === 'class') {
              el.className = value;
            } else if (key === 'id') {
              el.id = value;
            } else {
              el.setAttribute(key, value);
            }
          }
        });
      }

      // Render children
      if (vnode.children) {
        const children = Array.isArray(vnode.children) ? vnode.children : [];
        children.forEach(child => {
          el.appendChild(_renderVNode(child));
        });
      }

      return el;

    case "fragment":
      const frag = document.createDocumentFragment();
      if (vnode.children) {
        const children = Array.isArray(vnode.children) ? vnode.children : [];
        children.forEach(child => {
          frag.appendChild(_renderVNode(child));
        });
      }
      return frag;

    default:
      return document.createTextNode('');
  }
}"""
  }

  /**
   * Generate CSS styles for the UI.
   */
  private def generateStyles(theme: String): String = {
    val isDark = theme == "dark"
    val bgColor = if (isDark) "#1a1a2e" else "#f0f4f8"
    val cardBg = if (isDark) "#16213e" else "#ffffff"
    val textColor = if (isDark) "#e0e0e0" else "#333333"
    val accentColor = "#667eea"

    s"""* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  background: $bgColor;
  color: $textColor;
  min-height: 100vh;
  display: flex;
  justify-content: center;
  align-items: center;
}

#app {
  width: 100%;
  max-width: 600px;
  padding: 20px;
}

.card {
  background: $cardBg;
  border-radius: 12px;
  padding: 40px;
  box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1);
  text-align: center;
}

h1 {
  margin-bottom: 30px;
  font-weight: 600;
  color: $accentColor;
}

.display {
  font-size: 72px;
  font-weight: bold;
  margin: 30px 0;
  color: $textColor;
}

.buttons {
  display: flex;
  gap: 20px;
  justify-content: center;
}

button {
  width: 80px;
  height: 80px;
  font-size: 32px;
  border: none;
  border-radius: 50%;
  background: $accentColor;
  color: white;
  cursor: pointer;
  transition: transform 0.1s, box-shadow 0.1s;
}

button:hover {
  transform: scale(1.05);
  box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
}

button:active {
  transform: scale(0.95);
}"""
  }

  /**
   * Escape HTML special characters.
   */
  private def escapeHtml(s: String): String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
      .replace("'", "&#39;")
}
