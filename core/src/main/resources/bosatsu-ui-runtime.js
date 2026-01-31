/**
 * BosatsuUI Runtime
 *
 * The runtime that connects state changes to targeted DOM updates.
 * Unlike React's virtual DOM diffing, this uses pre-computed bindings
 * from static analysis to update only the specific DOM properties that changed.
 *
 * This is a port of burritoUI's runtime.ts adapted for Bosatsu.
 *
 * Flow:
 * 1. Mount: Create DOM from initial VNode, populate element cache
 * 2. State change: Look up bindings for changed path, apply updates directly
 * 3. No diffing, no re-rendering - just targeted property updates
 */

// -----------------------------------------------------------------------------
// Element Cache
// -----------------------------------------------------------------------------

/**
 * Cache for DOM elements by selector/id.
 * Enables O(1) lookup for binding updates.
 */
const _elementCache = new Map();

/**
 * State storage.
 */
let _state = {};

/**
 * Binding registry: statePath -> DOMBinding[]
 * Populated from static analysis output.
 */
let _bindings = {};

/**
 * Cached set of discriminant paths (for fast O(1) lookup).
 * Populated during _initRuntime.
 */
let _discriminantPaths = new Set();

/**
 * Compiled transform functions cache.
 */
const _transformCache = new Map();

// -----------------------------------------------------------------------------
// State Utilities
// -----------------------------------------------------------------------------

/**
 * Get a value at a nested path in an object.
 * @param {unknown} obj - The object to read from
 * @param {string[]} path - Path components
 * @returns {unknown} The value at the path
 */
function _getAtPath(obj, path) {
  if (path.length === 0) return obj;
  if (typeof obj !== 'object' || obj === null) return undefined;

  let current = obj;
  for (const key of path) {
    if (typeof current !== 'object' || current === null) return undefined;
    current = current[key];
  }
  return current;
}

/**
 * Set a value at a nested path in an object (mutably, for performance).
 * @param {unknown} obj - The object to update
 * @param {string[]} path - Path components
 * @param {unknown} value - The new value
 * @returns {unknown} The same object, mutated
 */
function _setAtPath(obj, path, value) {
  if (path.length === 0) return value;

  // Ensure root is an object
  if (typeof obj !== 'object' || obj === null) {
    obj = {};
  }

  // Navigate to parent, creating objects as needed
  let current = obj;
  for (let i = 0; i < path.length - 1; i++) {
    const key = path[i];
    if (typeof current[key] !== 'object' || current[key] === null) {
      current[key] = {};
    }
    current = current[key];
  }

  // Set the final value
  current[path[path.length - 1]] = value;
  return obj;
}

/**
 * Set a value at a nested path in an object (immutably).
 * Use this when you need to preserve the old state (undo, history, etc.)
 * @param {unknown} obj - The object to update
 * @param {string[]} path - Path components
 * @param {unknown} value - The new value
 * @returns {unknown} New object with updated value
 */
function _setAtPathImmutable(obj, path, value) {
  if (path.length === 0) return value;

  const current = (typeof obj === 'object' && obj !== null) ? { ...obj } : {};
  const [first, ...rest] = path;

  current[first] = rest.length === 0
    ? value
    : _setAtPathImmutable(current[first], rest, value);

  return current;
}

/**
 * Deep equality check.
 */
function _deepEqual(a, b) {
  if (a === b) return true;
  if (a === null || b === null) return a === b;
  if (typeof a !== 'object' || typeof b !== 'object') return a === b;

  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    return a.every((val, i) => _deepEqual(val, b[i]));
  }

  if (Array.isArray(a) || Array.isArray(b)) return false;

  const aKeys = Object.keys(a);
  const bKeys = Object.keys(b);

  if (aKeys.length !== bKeys.length) return false;

  return aKeys.every((key) => _deepEqual(a[key], b[key]));
}

// -----------------------------------------------------------------------------
// DOM Utilities
// -----------------------------------------------------------------------------

/**
 * Find an element by selector, using cache for efficiency.
 * @param {string} selector - CSS selector or data-bosatsu-id
 * @param {Element} [root] - Root element to search within
 * @returns {Element|null}
 */
function _findElement(selector, root) {
  // Check cache first
  const cached = _elementCache.get(selector);
  if (cached && document.contains(cached)) {
    return cached;
  }

  // Find and cache
  const searchRoot = root || document;
  const element = searchRoot.querySelector(selector);
  if (element) {
    _elementCache.set(selector, element);
  }
  return element;
}

/**
 * Apply a binding update to an element.
 * @param {Element} element - DOM element
 * @param {string} property - Property name (textContent, className, etc.)
 * @param {unknown} value - New value
 * @param {string} [styleProperty] - CSS property for style bindings
 */
function _applyBindingUpdate(element, property, value, styleProperty) {
  switch (property) {
    case 'textContent':
      element.textContent = String(value ?? '');
      break;

    case 'className':
      element.className = String(value ?? '');
      break;

    case 'value':
      if ('value' in element) {
        element.value = String(value ?? '');
      }
      break;

    case 'checked':
      if ('checked' in element) {
        element.checked = Boolean(value);
      }
      break;

    case 'disabled':
      if ('disabled' in element) {
        element.disabled = Boolean(value);
      }
      break;

    default:
      // Style property
      if (property.startsWith('style.') && styleProperty) {
        element.style[styleProperty] = String(value ?? '');
      }
      break;
  }
}

/**
 * Get or compile a transform function.
 * @param {string} transform - Transform expression
 * @returns {Function}
 */
function _getTransform(transform) {
  const cached = _transformCache.get(transform);
  if (cached) return cached;

  // Compile the transform function
  const fn = new Function('value', `return (${transform})(value)`);
  _transformCache.set(transform, fn);
  return fn;
}

// -----------------------------------------------------------------------------
// Batching Configuration
// -----------------------------------------------------------------------------

/**
 * Batching configuration.
 * @property {number} batchSize - Number of updates before auto-flush (default: Infinity = microtask only)
 * @property {string|number} flushDelay - 'microtask' | 0 (sync) | 16 (frame) | ms delay
 */
let _config = {
  batchSize: Infinity,
  flushDelay: 'microtask',
};

/**
 * Configure batching behavior.
 * @param {Object} options
 * @param {number} [options.batchSize] - Updates before auto-flush (1 = immediate, Infinity = full batching)
 * @param {string|number} [options.flushDelay] - 'microtask' | 0 (sync) | 16 (frame-aligned) | ms
 */
function _configure(options) {
  if (options.batchSize !== undefined) {
    _config.batchSize = options.batchSize;
  }
  if (options.flushDelay !== undefined) {
    _config.flushDelay = options.flushDelay;
  }
}

/**
 * Get current configuration.
 * @returns {Object}
 */
function _getConfig() {
  return { ..._config };
}

// -----------------------------------------------------------------------------
// Batching Support
// -----------------------------------------------------------------------------

let _pendingPaths = new Map();
let _flushScheduled = false;
let _inBatch = false;
let _pendingCount = 0;

/**
 * Flush all pending DOM updates.
 */
function _flushPendingUpdates() {
  _flushScheduled = false;
  _pendingCount = 0;

  _pendingPaths.forEach((path, key) => {
    _applyBindingsForPath(path);
  });

  _pendingPaths.clear();
}

/**
 * Schedule a flush based on configuration.
 */
function _scheduleFlush() {
  if (_inBatch) return;

  // Check if we've hit the batch size threshold
  if (_config.batchSize !== Infinity && _pendingCount >= _config.batchSize) {
    // Immediate flush when batch size exceeded
    _flushPendingUpdates();
    return;
  }

  if (_flushScheduled) return;
  _flushScheduled = true;

  // Schedule based on flushDelay config
  if (_config.flushDelay === 0) {
    // Synchronous - flush immediately
    _flushPendingUpdates();
  } else if (_config.flushDelay === 'microtask') {
    // Default microtask batching
    queueMicrotask(_flushPendingUpdates);
  } else if (typeof _config.flushDelay === 'number') {
    // Time-based delay (e.g., 16ms for frame-aligned)
    setTimeout(_flushPendingUpdates, _config.flushDelay);
  } else {
    // Fallback to microtask
    queueMicrotask(_flushPendingUpdates);
  }
}

/**
 * Queue a path for update.
 * @param {string[]} path
 */
function _queuePathUpdate(path) {
  const key = path.join('.');
  if (!_pendingPaths.has(key)) {
    _pendingCount++;
  }
  _pendingPaths.set(key, path);
  _scheduleFlush();
}

/**
 * Check if a conditional binding should be applied.
 * Returns true if the binding is unconditional OR its condition is met.
 * @param {Object} binding - The binding with optional `when` clause
 * @returns {boolean}
 */
function _shouldApplyBinding(binding) {
  // Unconditional bindings always apply
  if (!binding.when) return true;

  const { discriminant, tag, isTotal } = binding.when;

  // Get the current value at the discriminant path
  const discriminantValue = _getAtPath(_state, discriminant);

  // Total matches (wildcards) always apply
  if (isTotal) return true;

  // Check if the discriminant's tag matches
  // Sum types are represented as { tag: "VariantName", ...fields }
  if (discriminantValue && typeof discriminantValue === 'object') {
    return discriminantValue.tag === tag;
  }

  // For literal matches, compare directly
  return String(discriminantValue) === tag;
}

/**
 * Apply bindings for a changed state path.
 * @param {string[]} path
 */
function _applyBindingsForPath(path) {
  const key = path.join('.');
  const bindings = _bindings[key];

  if (!bindings) return;

  for (const binding of bindings) {
    // Check conditional binding condition
    if (!_shouldApplyBinding(binding)) continue;

    // Find the element
    const element = _findElement(
      binding.elementId.startsWith('#') || binding.elementId.startsWith('[')
        ? binding.elementId
        : `[data-bosatsu-id="${binding.elementId}"]`
    );
    if (!element) continue;

    // Get the new value
    let newValue = _getAtPath(_state, binding.statePath || path);

    // Apply transform if present
    if (binding.transform) {
      try {
        const transformFn = _getTransform(binding.transform);
        newValue = transformFn(newValue);
      } catch (e) {
        console.error(`Failed to apply transform:`, e);
      }
    }

    // Apply the update
    _applyBindingUpdate(element, binding.property, newValue, binding.styleProperty);
  }
}

// -----------------------------------------------------------------------------
// Public API
// -----------------------------------------------------------------------------

/**
 * Initialize the UI runtime with bindings and initial state.
 * @param {Object} bindings - Binding map from static analysis
 * @param {Object} initialState - Initial state object
 */
function _initRuntime(bindings, initialState) {
  _bindings = bindings || {};
  _state = initialState || {};
  _elementCache.clear();
  _pendingPaths.clear();
  _flushScheduled = false;
  _pendingCount = 0;

  // Pre-compute discriminant paths for O(1) lookup
  _discriminantPaths = new Set();
  for (const bindingList of Object.values(_bindings)) {
    for (const binding of bindingList) {
      if (binding.when && binding.when.discriminant) {
        _discriminantPaths.add(binding.when.discriminant.join('.'));
      }
    }
  }
}

/**
 * Get the current state.
 * @returns {Object}
 */
function _getState() {
  return _state;
}

/**
 * Get state at a path.
 * @param {string[]} path
 * @returns {unknown}
 */
function _getStateAt(path) {
  return _getAtPath(_state, path);
}

/**
 * Set state at a path (triggers targeted DOM updates).
 * @param {string[]} path - Path into state
 * @param {unknown} value - New value
 */
function _setState(path, value) {
  const prevValue = _getAtPath(_state, path);

  // Only update if value actually changed
  if (_deepEqual(prevValue, value)) return;

  // Check if this is a discriminant path (affects conditional bindings)
  const isDiscriminant = _isDiscriminantPath(path);

  // Update state immediately
  _state = _setAtPath(_state, path, value);

  // Queue DOM update
  _queuePathUpdate(path);

  // If a discriminant changed, re-evaluate all conditional bindings
  if (isDiscriminant) {
    _reapplyConditionalBindings(path);
  }
}

/**
 * Check if a path is used as a discriminant in any conditional binding.
 * Uses pre-computed cache for O(1) lookup.
 * @param {string[]} path
 * @returns {boolean}
 */
function _isDiscriminantPath(path) {
  return _discriminantPaths.has(path.join('.'));
}

/**
 * Re-apply all conditional bindings that depend on a discriminant.
 * Called when a discriminant path changes value.
 * @param {string[]} discriminantPath
 */
function _reapplyConditionalBindings(discriminantPath) {
  const discKey = discriminantPath.join('.');

  for (const [pathKey, bindings] of Object.entries(_bindings)) {
    for (const binding of bindings) {
      // Skip unconditional bindings
      if (!binding.when) continue;

      // Check if this binding depends on the changed discriminant
      const bindingDiscKey = binding.when.discriminant.join('.');
      if (bindingDiscKey !== discKey) continue;

      // Queue the binding's path for re-evaluation
      const path = pathKey.split('.');
      _queuePathUpdate(path);
    }
  }
}

/**
 * Update state using a function.
 * @param {string[]} path
 * @param {Function} fn
 */
function _updateState(path, fn) {
  const current = _getAtPath(_state, path);
  const newValue = fn(current);
  _setState(path, newValue);
}

/**
 * Execute multiple state updates in a batch.
 * DOM updates are deferred until the batch completes.
 * @param {Function} fn
 */
function _batch(fn) {
  _inBatch = true;
  try {
    fn();
  } finally {
    _inBatch = false;
    _flushPendingUpdates();
  }
}

/**
 * Force immediate application of all pending updates.
 */
function _flush() {
  _flushPendingUpdates();
}

// -----------------------------------------------------------------------------
// VNode to DOM Creation
// -----------------------------------------------------------------------------

/**
 * Create DOM from a VNode.
 * @param {Object} vnode - Virtual node
 * @returns {Node|null}
 */
function _createDOM(vnode) {
  if (vnode === null || vnode === undefined) {
    return null;
  }

  // Text node
  if (vnode.type === 'text' || typeof vnode === 'string') {
    const text = typeof vnode === 'string' ? vnode : vnode.text;
    return document.createTextNode(text);
  }

  // Fragment
  if (vnode.type === 'fragment') {
    const fragment = document.createDocumentFragment();
    if (vnode.children) {
      for (const child of vnode.children) {
        const childDom = _createDOM(child);
        if (childDom) fragment.appendChild(childDom);
      }
    }
    return fragment;
  }

  // Element
  const element = document.createElement(vnode.tag || 'div');

  // Set attributes
  if (vnode.props || vnode.attributes) {
    const attrs = vnode.props || vnode.attributes || {};
    for (const [key, value] of Object.entries(attrs)) {
      if (key === 'className') {
        element.className = String(value);
      } else if (key === 'style' && typeof value === 'object') {
        for (const [prop, val] of Object.entries(value)) {
          element.style[prop] = String(val);
        }
      } else if (key.startsWith('on')) {
        // Skip event handlers in creation - they're attached separately
      } else if (value !== null && value !== undefined && value !== false) {
        element.setAttribute(key, value === true ? '' : String(value));
      }
    }
  }

  // Set data-bosatsu-id if present
  if (vnode.id) {
    element.setAttribute('data-bosatsu-id', vnode.id);
    _elementCache.set(vnode.id, element);
    _elementCache.set(`[data-bosatsu-id="${vnode.id}"]`, element);
  }

  // Create children
  if (vnode.children) {
    for (const child of vnode.children) {
      const childDom = _createDOM(child);
      if (childDom) element.appendChild(childDom);
    }
  }

  return element;
}

/**
 * Mount a VNode to a root element.
 * @param {Element} root - Root DOM element
 * @param {Object} vnode - Virtual node
 * @param {Object} bindings - Binding map
 * @param {Object} initialState - Initial state
 */
function _mount(root, vnode, bindings, initialState) {
  _initRuntime(bindings, initialState);

  const dom = _createDOM(vnode);
  if (dom) {
    root.appendChild(dom);
  }
}

/**
 * Unmount and cleanup.
 * @param {Element} root
 */
function _unmount(root) {
  _pendingPaths.clear();
  _flushScheduled = false;
  if (root) {
    root.innerHTML = '';
  }
  _elementCache.clear();
}

// -----------------------------------------------------------------------------
// Exports (for browser global and module)
// -----------------------------------------------------------------------------

// Make available globally for generated code
if (typeof window !== 'undefined') {
  window.BosatsuUI = {
    // Configuration
    configure: _configure,
    getConfig: _getConfig,

    // State management
    getState: _getState,
    getStateAt: _getStateAt,
    setState: _setState,
    updateState: _updateState,
    batch: _batch,
    flush: _flush,

    // DOM management
    mount: _mount,
    unmount: _unmount,
    createDOM: _createDOM,

    // Internal (for generated code)
    _initRuntime,
    _elementCache,
    _bindings,
    _state,
    _getAtPath,
    _setAtPath,
    _findElement,
    _applyBindingUpdate,
    _shouldApplyBinding,
    _isDiscriminantPath,
    _reapplyConditionalBindings,
  };
}

// Module exports
if (typeof module !== 'undefined' && module.exports) {
  module.exports = {
    configure: _configure,
    getConfig: _getConfig,
    getState: _getState,
    getStateAt: _getStateAt,
    setState: _setState,
    updateState: _updateState,
    batch: _batch,
    flush: _flush,
    mount: _mount,
    unmount: _unmount,
    createDOM: _createDOM,
    _initRuntime,
    _shouldApplyBinding,
    _isDiscriminantPath,
    _reapplyConditionalBindings,
  };
}
