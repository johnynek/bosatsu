package dev.bosatsu.ui

import dev.bosatsu.codegen.js.JsGen

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
   *
   * @param title Page title
   * @param theme Visual theme ("light" or "dark")
   * @param includeSourceMap Reserved for future source map support (not yet implemented)
   */
  case class UIConfig(
      title: String,
      theme: String = "light",
      @scala.deprecated("Not yet implemented", "0.1") includeSourceMap: Boolean = false
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
    val canvasBindingsJs = generateCanvasBindingsJs(analysis.canvasBindings)
    val frameCallbacksJs = generateFrameCallbacksJs(analysis.frameCallbacks)
    val runtimeJs = generateRuntimeJs()
    val canvasRuntimeJs = generateCanvasRuntimeJs()
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

// Canvas runtime
$canvasRuntimeJs

// Event handlers
$eventHandlersJs

// Canvas bindings
$canvasBindingsJs

// Frame callbacks
$frameCallbacksJs

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
  _initCanvasBindings();
  _startAnimationLoop();
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
    // Handler registrations are now generated by JsGen via _ui_register_handler calls
    // in the compiled VNode tree code, not here in UIGen
    s"""const _eventHandlers = {};
let _handlerCounter = 0;

// Called by JsGen-compiled code: _ui_register_handler("click", handlerFn)
// Returns a handler ID string that becomes a data- attribute value
function _ui_register_handler(eventType, handler) {
  const handlerId = '_handler_' + (++_handlerCounter);
  _eventHandlers[handlerId] = { type: eventType, fn: handler };
  return handlerId;
}

function _initEventHandlers() {
  document.querySelectorAll('[data-onclick], [data-oninput], [data-onchange], [data-onkeydown], [data-onkeyup], [data-ondragstart], [data-ondragover], [data-ondrop]').forEach(el => {
    for (const attr of el.attributes) {
      if (attr.name.startsWith('data-on')) {
        const handlerId = attr.value;
        const handlerInfo = _eventHandlers[handlerId];
        if (handlerInfo) {
          el.addEventListener(handlerInfo.type, (e) => {
            let ioResult;
            if (handlerInfo.type === 'input' || handlerInfo.type === 'change') {
              ioResult = handlerInfo.fn(_js_to_bosatsu_string(e.target.value || ''));
            } else if (handlerInfo.type === 'keydown' || handlerInfo.type === 'keyup') {
              ioResult = handlerInfo.fn(_js_to_bosatsu_string(e.key || ''));
            } else {
              ioResult = handlerInfo.fn(undefined);
            }
            // Execute the IO data structure
            _runIO(ioResult);
          });
        }
      }
    }
  });
}"""
  }

  /**
   * Generate runtime JavaScript for state management and rendering.
   */
  private def generateRuntimeJs(): String = {
    """// State storage
const _state = {};

// Element cache for O(1) lookups
const _elements = {};

// IO Monad Interpreter - walks IO data structures and performs effects
function _runIO(io) {
  if (!io || typeof io !== 'object') return undefined;
  switch (io.tag) {
    case 'Pure':
      return io.value;
    case 'Write':
      if (io.state) {
        io.state.value = io.value;
        _updateBindings(io.state.id);
        _updateCanvasBindings(io.state);
      }
      return undefined;
    case 'FlatMap': {
      const result = _runIO(io.io);
      const nextIO = io.fn(result);
      return _runIO(nextIO);
    }
    case 'Sequence': {
      // Walk Bosatsu list: [0] = empty, [1, head, tail] = non-empty
      let cur = io.ios;
      const results = [];
      while (cur && cur[0] !== 0) {
        results.push(_runIO(cur[1]));
        cur = cur[2];
      }
      // Convert back to Bosatsu list
      return results.reduceRight((acc, item) => [1, item, acc], [0]);
    }
    case 'Capture':
      return io.value;
    case 'CaptureFormula':
      return io.value;
    case 'Trace':
      return '[trace]';
    case 'RandomInt': {
      const min = io.min;
      const max = io.max;
      return Math.floor(Math.random() * (max - min + 1)) + min;
    }
    case 'RegisterFrameCallback':
      _frameCallbacks.push(io.updateFn);
      return undefined;
    default:
      console.warn('Unknown IO tag:', io.tag);
      return undefined;
  }
}

// Read state
function read(stateId) {
  const state = _state[stateId];
  return state ? state.value : undefined;
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
}

canvas {
  display: block;
  border-radius: 8px;
}"""
  }

  /**
   * Generate canvas bindings JavaScript.
   */
  private def generateCanvasBindingsJs[A](bindings: List[UIAnalyzer.CanvasBinding[A]]): String = {
    if (bindings.isEmpty) {
      "const _canvasBindings = {};"
    } else {
      // Canvas bindings will be registered at runtime via _ui_register_canvas_render
      """const _canvasBindings = {};

function _ui_register_canvas_render(stateObj, renderFn) {
  const id = '_canvas_' + Object.keys(_canvasBindings).length;
  _canvasBindings[id] = { state: stateObj, render: renderFn };
  return id;
}"""
    }
  }

  /**
   * Generate frame callbacks JavaScript.
   */
  private def generateFrameCallbacksJs[A](callbacks: List[UIAnalyzer.FrameCallback[A]]): String = {
    if (callbacks.isEmpty) {
      "const _frameCallbacks = [];"
    } else {
      // Frame callbacks will be registered at runtime via _ui_register_frame_callback
      """const _frameCallbacks = [];

function _ui_register_frame_callback(updateFn) {
  _frameCallbacks.push(updateFn);
  return []; // Return Unit (empty tuple)
}"""
    }
  }

  /**
   * Generate canvas runtime JavaScript for rendering and animation.
   */
  private def generateCanvasRuntimeJs(): String = {
    """// Canvas contexts cache
const _canvasContexts = {};

// Last frame timestamp for delta time calculation
let _lastFrameTime = 0;

// Maximum delta time (33ms = ~30fps) to prevent physics explosions after tab switch
const MAX_DT = 0.033;

// Execute canvas commands on a 2D context
function _executeCanvas(cmds, ctx) {
  if (!cmds) return;

  // Handle array of commands (from _bosatsu_list_to_array)
  if (Array.isArray(cmds)) {
    cmds.forEach(cmd => _executeCanvas(cmd, ctx));
    return;
  }

  switch (cmds.type) {
    case 'clear':
      ctx.fillStyle = cmds.color;
      ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
      break;

    case 'fill':
      ctx.fillStyle = cmds.color;
      break;

    case 'stroke':
      ctx.strokeStyle = cmds.color;
      break;

    case 'lineWidth':
      ctx.lineWidth = cmds.width;
      break;

    case 'circle':
      ctx.beginPath();
      ctx.arc(cmds.x, cmds.y, cmds.r, 0, Math.PI * 2);
      ctx.fill();
      break;

    case 'rect':
      ctx.fillRect(cmds.x, cmds.y, cmds.w, cmds.h);
      break;

    case 'line':
      ctx.beginPath();
      ctx.moveTo(cmds.x1, cmds.y1);
      ctx.lineTo(cmds.x2, cmds.y2);
      ctx.stroke();
      break;

    case 'text':
      ctx.fillText(cmds.text, cmds.x, cmds.y);
      break;

    case 'arc':
      ctx.beginPath();
      ctx.arc(cmds.x, cmds.y, cmds.r, cmds.start, cmds.end);
      ctx.stroke();
      break;

    case 'save':
      ctx.save();
      break;

    case 'restore':
      ctx.restore();
      break;

    case 'translate':
      ctx.translate(cmds.x, cmds.y);
      break;

    case 'rotate':
      ctx.rotate(cmds.angle);
      break;

    case 'scale':
      ctx.scale(cmds.sx, cmds.sy);
      break;

    case 'sequence':
      if (cmds.commands) {
        cmds.commands.forEach(cmd => _executeCanvas(cmd, ctx));
      }
      break;
  }
}

// Initialize canvas bindings
function _initCanvasBindings() {
  Object.entries(_canvasBindings).forEach(([id, binding]) => {
    // Find canvas element with this binding
    const canvas = document.querySelector('[data-canvas-render="' + id + '"]') ||
                   document.querySelector('canvas[data-bosatsu-id]');
    if (canvas && canvas.getContext) {
      const ctx = canvas.getContext('2d');
      _canvasContexts[id] = ctx;

      // Initial render
      if (binding.state && binding.render) {
        try {
          const cmds = binding.render(binding.state.value);
          _executeCanvas(cmds, ctx);
        } catch (e) {
          console.error('Canvas render error:', e);
        }
      }
    }
  });
}

// Update all canvas bindings for a state change
function _updateCanvasBindings(stateObj) {
  Object.entries(_canvasBindings).forEach(([id, binding]) => {
    if (binding.state === stateObj || binding.state.id === stateObj.id) {
      const ctx = _canvasContexts[id];
      if (ctx && binding.render) {
        try {
          const cmds = binding.render(stateObj.value);
          _executeCanvas(cmds, ctx);
        } catch (e) {
          console.error('Canvas render error:', e);
        }
      }
    }
  });
}

// Animation loop
function _animationLoop(timestamp) {
  if (_lastFrameTime === 0) {
    _lastFrameTime = timestamp;
  }

  // Calculate delta time in seconds, capped to prevent physics explosions
  let dt = (timestamp - _lastFrameTime) / 1000;
  dt = Math.min(dt, MAX_DT);
  _lastFrameTime = timestamp;

  // Call all frame callbacks with delta time, execute resulting IO
  _frameCallbacks.forEach(callback => {
    try {
      const ioResult = callback(dt);
      _runIO(ioResult);
    } catch (e) {
      console.error('Frame callback error:', e);
    }
  });

  // Continue the loop
  if (_frameCallbacks.length > 0) {
    requestAnimationFrame(_animationLoop);
  }
}

// Start the animation loop if there are frame callbacks
function _startAnimationLoop() {
  if (_frameCallbacks.length > 0) {
    _lastFrameTime = 0;
    requestAnimationFrame(_animationLoop);
  }
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
