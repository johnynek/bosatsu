package dev.bosatsu.simulation

import cats.effect.IO
import cats.implicits._
import cats.data.{NonEmptyList, ValidatedNel, Validated}
import com.monovore.decline._
import java.nio.file.{Files, Path, Paths}

import dev.bosatsu.{
  Package,
  PackageMap,
  PackageName,
  LocationMap,
  Identifier,
  Par,
  TypedExpr,
  MatchlessFromTypedExpr,
  Predef,
  Numeric,
  UI
}
import dev.bosatsu.codegen.js.{JsGen, SourceMapGenerator}
import dev.bosatsu.ui.{UIAnalyzer, UIGen}
import dev.bosatsu.Parser

/**
 * Command-line interface for generating reactive UI from Bosatsu/UI code.
 *
 * This implements the full pipeline:
 * 1. Parse .bosatsu file with Bosatsu/UI imports
 * 2. Type-check to get TypedExpr AST
 * 3. Run UIAnalyzer to extract state→DOM bindings at compile time
 * 4. Compile to JavaScript using JsGen with UIExternal intrinsics
 * 5. Generate HTML with VNode tree, binding map, and runtime
 *
 * The result is reactive HTML where state changes trigger O(1) direct DOM updates
 * (no virtual DOM diffing required).
 */
case class UICommand(
    input: Path,
    output: Path,
    title: Option[String],
    theme: String,
    includeSourceMap: Boolean
) {

  def run: IO[Unit] = {
    given ec: Par.EC = Par.ecFromExecutionContext(
      using scala.concurrent.ExecutionContext.global
    )

    for {
      // 1. Read the input file
      content <- IO(Files.readString(input))
      _ <- IO(println(s"Parsing ${input.getFileName}..."))

      // 2. Parse the file
      parsed <- IO.fromEither(parseFile(content, input.getFileName.toString))
      lm = LocationMap(content)
      fileName = input.getFileName.toString.stripSuffix(".bosatsu").replace("-", "_")

      _ <- IO(println(s"Package: ${parsed._2.name.asString}"))

      // 3. Type-check with predef, numeric, io, and ui packages
      typeChecked <- IO.fromEither {
        val packs = NonEmptyList.one(((fileName, lm), parsed._2))
        PackageMap.typeCheckParsed(packs, Nil, fileName).toEither.leftMap { errs =>
          val sourceMap: Map[PackageName, (LocationMap, String)] = Map(
            parsed._2.name -> (lm, fileName)
          )
          val errMsg = errs.toList.map(_.message(sourceMap, LocationMap.Colorize.None)).mkString("\n")
          new RuntimeException(s"Type check errors:\n$errMsg")
        }
      }

      // 4. Get the typed package
      typedPackage = typeChecked.toMap.get(parsed._2.name).getOrElse(
        throw new RuntimeException(s"Package ${parsed._2.name} not found after type checking")
      )

      _ <- IO(println(s"Type-checked successfully. Found ${typedPackage.lets.size} binding(s)."))

      // 5. Find the main/view binding that produces VNode
      mainBinding <- IO {
        typedPackage.lets.find { case (name, _, _) =>
          name.asString == "main" || name.asString == "view"
        }.getOrElse(
          throw new RuntimeException("No 'main' or 'view' binding found. UI file must export a view.")
        )
      }
      mainName = mainBinding._1.asString
      mainExpr = mainBinding._3

      _ <- IO(println(s"Found UI binding: $mainName"))

      // 5.5. Scan for state bindings (state(initial) calls)
      stateBindings <- IO {
        typedPackage.lets.collect {
          case (name, _, expr) if UIAnalyzer.isStateCreationExpr(expr) => name
        }
      }
      _ <- IO {
        if (stateBindings.nonEmpty) {
          println(s"Found state variables: ${stateBindings.map(_.asString).mkString(", ")}")
        }
      }

      // 5.6. Build a map of function bodies for resolving named handler references
      functionBodies <- IO {
        typedPackage.lets.collect {
          case (name, _, expr) if !UIAnalyzer.isStateCreationExpr(expr) && name.asString != mainName =>
            name.asString -> expr
        }.toMap
      }
      _ <- IO {
        if (functionBodies.nonEmpty) {
          println(s"Found functions: ${functionBodies.keys.mkString(", ")}")
        }
      }

      // 6. Run UIAnalyzer on the TypedExpr to extract bindings
      _ <- IO(println("Analyzing UI bindings..."))
      analysis = UIAnalyzer.analyzeWithFunctions(mainExpr, stateBindings.toList, functionBodies)

      _ <- IO(println(s"  State reads: ${analysis.stateReads.size}"))
      _ <- IO(println(s"  DOM bindings: ${analysis.bindings.size}"))
      _ <- IO(println(s"  Event handlers: ${analysis.eventHandlers.size}"))

      // Print detailed binding info
      _ <- IO {
        if (analysis.bindings.nonEmpty) {
          println("  Bindings:")
          analysis.bindings.foreach { b =>
            println(s"    ${b.statePath.mkString(".")} → ${b.elementId}.${UIAnalyzer.DOMProperty.toJsProperty(b.property)}")
          }
        }
        if (analysis.eventHandlers.nonEmpty) {
          println("  Event handlers:")
          analysis.eventHandlers.foreach { h =>
            println(s"    ${h.elementId}: ${h.eventType}")
          }
        }
      }

      // 7. Compile to Matchless IR
      _ <- IO(println("Compiling to JavaScript..."))
      matchlessCompiled = MatchlessFromTypedExpr.compile((),
        PackageMap.toAnyTyped(typeChecked)
      )

      packageBindings = matchlessCompiled.getOrElse(typedPackage.name, Nil)

      // 8. Generate JavaScript using JsGen with UIExternal intrinsics
      computeJs = JsGen.renderStatements(packageBindings, Some(typedPackage.name))

      _ <- IO(println(s"Generated ${computeJs.length} bytes of JavaScript"))

      // 9. Generate the HTML
      _ <- IO(println("Generating HTML..."))
      config = UIGen.UIConfig(
        title = title.getOrElse(mainName),
        theme = theme,
        includeSourceMap = includeSourceMap
      )

      html = generateHTML(
        computeJs,
        analysis,
        config,
        stateBindings.map(_.asString).toList,
        input.getFileName.toString,
        content
      )

      // 10. Write output
      _ <- IO(Files.writeString(output, html))
      _ <- IO(println(s"Generated: $output"))
    } yield ()
  }

  private def parseFile(content: String, fileName: String): Either[Throwable, (LocationMap, Package.Parsed)] = {
    Parser.parse(Package.parser(None), content).toEither.leftMap { errs =>
      val lm = LocationMap(content)
      val errMsg = errs.toList.map(_.showContext(LocationMap.Colorize.None).renderTrim(80)).mkString("; ")
      new RuntimeException(s"Parse error in $fileName: $errMsg")
    }.map { case (_, parsed) =>
      (LocationMap(content), parsed)
    }
  }

  /**
   * Generate complete HTML with:
   * - Compiled VNode JavaScript
   * - Binding map for O(1) updates
   * - State management runtime
   * - Event handler registration
   * - Optional source map for debugging
   */
  private def generateHTML[A](
      computeJs: String,
      analysis: UIAnalyzer.UIAnalysis[A],
      config: UIGen.UIConfig,
      stateVarNames: List[String],
      sourceFileName: String,
      sourceContent: String
  ): String = {
    // Generate bindings from UIAnalyzer (automatic detection via handler analysis)
    val bindingsJs = UIAnalyzer.bindingsToJs(analysis.bindings)

    val runtimeJs = generateRuntimeJs()
    val stylesCss = generateStyles(config.theme)

    // Generate code to link state objects to their binding keys
    val stateLinkingCode = stateVarNames.map { name =>
      s"""  if (typeof $name !== 'undefined') _linkStateToBinding($name, "$name");"""
    }.mkString("\n")

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
// ============================================================================
// BosatsuUI Runtime
// ============================================================================

${JsGen.runtimeCode}

// ============================================================================
// State Management
// ============================================================================

$runtimeJs

// ============================================================================
// Binding Map (extracted at compile time)
// ============================================================================

const _bindings = $bindingsJs;

// ============================================================================
// Compiled Bosatsu Code
// ============================================================================

$computeJs

// ============================================================================
// Initialize Application
// ============================================================================

function init() {
  // Link state objects to their binding keys for lookup
$stateLinkingCode

  // Get the VNode from the compiled 'main' or 'view' binding
  const vnode = typeof main !== 'undefined' ? main : (typeof view !== 'undefined' ? view : null);

  if (vnode) {
    const app = document.getElementById('app');
    app.appendChild(_renderVNode(vnode));
    console.log('Rendered VNode:', vnode);
  } else {
    console.error('No main or view binding found');
  }

  // Initialize element cache for bindings
  _initBindingCache();

  // Apply initial state values to DOM (for states that aren't 0/false)
  _applyInitialBindings();
}

// Apply initial state values to DOM elements
function _applyInitialBindings() {
  Object.entries(_state).forEach(([stateId, stateObj]) => {
    const bindingKey = _stateToBindingKey.get(stateObj);
    const bindings = bindingKey ? _bindings[bindingKey] : null;
    if (bindings && stateObj.value !== 0 && stateObj.value !== false) {
      bindings.forEach(binding => {
        _updateBinding(binding, stateObj.value);
      });
    }
  });
}

document.addEventListener('DOMContentLoaded', init);
${generateSourceMapComment(config.includeSourceMap, sourceFileName, sourceContent, computeJs)}
  </script>
</body>
</html>"""
  }

  /**
   * Generate an inline source map comment if enabled.
   *
   * The source map maps the compiled Bosatsu code section to the original .bosatsu file,
   * enabling browser DevTools to show the original source.
   */
  private def generateSourceMapComment(
      enabled: Boolean,
      sourceFileName: String,
      sourceContent: String,
      compiledJs: String
  ): String = {
    if (!enabled) return ""

    // Create a source map that maps the compiled JS to the original Bosatsu source
    val builder = new SourceMapGenerator.Builder("inline")
    val sourceIdx = builder.addSource(sourceFileName, Some(sourceContent))

    // Count lines in the HTML before the compiled code section
    // This is approximate - the compiled code starts after the "Compiled Bosatsu Code" comment
    val preambleLines = 30  // Runtime, state management, bindings header

    // Create mappings for each line of the compiled JS
    // This is a basic 1:1 mapping - more precise mapping would require tracking
    // positions through the compilation pipeline
    val sourceLines = sourceContent.split('\n')
    val compiledLines = compiledJs.split('\n')

    // Map compiled lines to source lines where possible
    var srcLine = 0
    compiledLines.zipWithIndex.foreach { case (_, genLineIdx) =>
      // Map generated line (relative to start of compiled section) to source line
      if (srcLine < sourceLines.length) {
        builder.addMapping(
          genLine = preambleLines + genLineIdx,
          genCol = 0,
          sourceIdx = sourceIdx,
          origLine = srcLine,
          origCol = 0
        )
        srcLine += 1
      }
    }

    val sourceMap = builder.build()
    "\n" + sourceMap.toInlineComment
  }

  /**
   * Generate the state management and rendering runtime.
   */
  private def generateRuntimeJs(): String = {
    """// State storage: { id: { id, value } }
const _state = {};

// Element cache for O(1) DOM lookups
const _elements = {};

// Handler registry
const _handlers = {};
let _handlerIdCounter = 0;

// Map state objects to their binding keys (WeakMap for proper GC)
const _stateToBindingKey = new WeakMap();

// Link a state object to its binding key (called from init)
function _linkStateToBinding(stateObj, bindingKey) {
  _stateToBindingKey.set(stateObj, bindingKey);
}

// Create a state object (called by compiled Bosatsu code via state(initial))
let _stateIdCounter = 0;
function _ui_create_state(initialValue) {
  const id = 'state_' + (_stateIdCounter++);
  const stateObj = { id: id, value: initialValue };
  _state[id] = stateObj;
  return stateObj;
}

// Read state value (called by compiled code)
function _ui_read(stateObj) {
  return stateObj.value;
}

// Write state and trigger binding updates (called by compiled code)
function _ui_write(stateObj, value) {
  const oldValue = stateObj.value;
  stateObj.value = value;

  // Find bindings using the binding key (Bosatsu variable name)
  const bindingKey = _stateToBindingKey.get(stateObj);
  const bindings = bindingKey ? _bindings[bindingKey] : null;
  if (bindings) {
    bindings.forEach(binding => {
      _updateBinding(binding, value);
    });
  }

  return []; // Return Unit (empty tuple)
}

// Register an event handler and return its ID
function _ui_register_handler(eventType, handler) {
  const handlerId = 'handler_' + (_handlerIdCounter++);
  _handlers[handlerId] = { type: eventType, fn: handler };
  return handlerId;
}

// Update a single DOM binding
function _updateBinding(binding, value) {
  let el = _elements[binding.elementId];
  if (!el) {
    el = document.getElementById(binding.elementId) ||
         document.querySelector('[data-bosatsu-id="' + binding.elementId + '"]');
    if (el) _elements[binding.elementId] = el;
  }
  if (!el) return;

  // Check if value is a Bosatsu Bool - [0] for False, [1] for True
  // Also handle raw integers 0/1 for numeric boolean pattern
  const isBosatsuBool = Array.isArray(value) && (value[0] === 0 || value[0] === 1) && value.length <= 2;
  const isIntBool = typeof value === 'number' && (value === 0 || value === 1);
  const boolValue = isBosatsuBool ? value[0] === 1 : (isIntBool ? value === 1 : null);

  let displayValue;
  if (binding.transform) {
    const transformResult = eval(binding.transform)(value);
    // Transform may return a Bosatsu string (array), convert to JS string
    displayValue = Array.isArray(transformResult)
      ? _bosatsuStringToJs(transformResult)
      : String(transformResult);
  } else if (isBosatsuBool || isIntBool) {
    displayValue = boolValue ? 'true' : 'false';
  } else {
    displayValue = String(value);
  }

  switch (binding.property) {
    case 'textContent':
      el.textContent = displayValue;
      break;
    case 'className':
      // For className with bool values (Bosatsu Bool or int 0/1), toggle a 'checked' class
      if (isBosatsuBool || isIntBool) {
        if (boolValue) {
          el.classList.add('checked');
        } else {
          el.classList.remove('checked');
        }
      } else {
        el.className = displayValue;
      }
      break;
    case 'value':
      el.value = displayValue;
      break;
    case 'checked':
      el.checked = !!value;
      break;
    case 'disabled':
      el.disabled = !!value;
      break;
    default:
      if (binding.property.startsWith('style.')) {
        const styleProp = binding.property.slice(6);
        el.style[styleProp] = displayValue;
      }
  }
}

// Initialize element cache for all bindings
function _initBindingCache() {
  Object.values(_bindings).flat().forEach(binding => {
    const el = document.getElementById(binding.elementId) ||
               document.querySelector('[data-bosatsu-id="' + binding.elementId + '"]');
    if (el) {
      _elements[binding.elementId] = el;
    }
  });

  // Set up event handlers
  Object.entries(_handlers).forEach(([handlerId, handler]) => {
    // Find elements with this handler ID
    const els = document.querySelectorAll('[data-on' + handler.type + '="' + handlerId + '"]');
    els.forEach(el => {
      el.addEventListener(handler.type, (e) => {
        // Call the handler - for click it receives Unit, for input it receives the value
        if (handler.type === 'click') {
          handler.fn([]);  // Unit = empty tuple
        } else if (handler.type === 'input' || handler.type === 'change') {
          // Convert JS string to Bosatsu string
          const bsString = _js_to_bosatsu_string(e.target.value);
          handler.fn(bsString);
        }
      });
    });
  });
}

// Render a VNode to a DOM element
function _renderVNode(vnode) {
  if (!vnode) return document.createTextNode('');

  const type = vnode.type;

  if (type === 'text') {
    // Text node: { type: "text", text: "content" }
    return document.createTextNode(vnode.text || '');
  }

  if (type === 'element') {
    // Element node: { type: "element", tag: "div", props: [...], children: [...] }
    // Tag may be a Bosatsu string (array), convert to JS string
    const tag = _bosatsuStringToJs(vnode.tag) || vnode.tag || 'div';
    const el = document.createElement(tag);

    // Process props (array of [key, value] tuples in Bosatsu format)
    if (vnode.props) {
      const props = _bosatsuListToArray(vnode.props);
      props.forEach(prop => {
        // prop is a Bosatsu tuple: [key, value]
        if (Array.isArray(prop) && prop.length >= 2) {
          const key = _bosatsuStringToJs(prop[0]) || prop[0];
          const value = _bosatsuStringToJs(prop[1]) || prop[1];

          if (key.startsWith('data-on')) {
            // Event handler attribute
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
      const children = _bosatsuListToArray(vnode.children);
      children.forEach(child => {
        el.appendChild(_renderVNode(child));
      });
    }

    return el;
  }

  if (type === 'fragment') {
    // Fragment: { type: "fragment", children: [...] }
    const frag = document.createDocumentFragment();
    if (vnode.children) {
      const children = _bosatsuListToArray(vnode.children);
      children.forEach(child => {
        frag.appendChild(_renderVNode(child));
      });
    }
    return frag;
  }

  // Unknown type - try to render as text
  return document.createTextNode(String(vnode));
}

// Convert Bosatsu list to JS array
// Bosatsu list: [1, head, [1, head2, [0]]] or [0] for empty
function _bosatsuListToArray(list) {
  const result = [];
  let current = list;
  while (Array.isArray(current) && current[0] === 1) {
    result.push(current[1]);
    current = current[2];
  }
  return result;
}

// Convert Bosatsu string to JS string
// Bosatsu string: [1, "c", [1, "h", [0]]] - linked list of characters
function _bosatsuStringToJs(str) {
  if (typeof str === 'string') return str;
  if (!Array.isArray(str)) return String(str);

  let result = '';
  let current = str;
  while (Array.isArray(current) && current[0] === 1) {
    result += current[1];
    current = current[2];
  }
  return result;
}

// Convert JS string to Bosatsu string (for event handlers)
function _js_to_bosatsu_string(s) {
  if (s.length === 0) return [0];
  let result = [0];
  for (let i = s.length - 1; i >= 0; i--) {
    result = [1, s[i], result];
  }
  return result;
}

// Convert JS array to Bosatsu list
function _js_to_bosatsu_list(arr) {
  let result = [0]; // Empty list
  for (let i = arr.length - 1; i >= 0; i--) {
    result = [1, arr[i], result]; // Cons(head, tail)
  }
  return result;
}

// ============================================================================
// Dynamic List State Management
// ============================================================================

// List state storage: { id: { id, items: [], templateBindings: {} } }
const _listState = {};
let _listStateIdCounter = 0;

// Map list state objects to their binding keys
const _listStateToBindingKey = new WeakMap();

// Link a list state object to its binding key
function _linkListStateToBinding(listStateObj, bindingKey) {
  _listStateToBindingKey.set(listStateObj, bindingKey);
}

// Create a list state object
function _ui_create_list_state(initialItems) {
  const id = 'list_state_' + (_listStateIdCounter++);
  // Convert Bosatsu list to JS array for internal storage
  const items = _bosatsuListToArray(initialItems);
  const listStateObj = {
    id: id,
    items: items,
    templateBindings: {} // Will be populated from _listBindingTemplates
  };
  _listState[id] = listStateObj;
  return listStateObj;
}

// Read list state - returns current items as Bosatsu list
function _ui_list_read(listStateObj) {
  return _js_to_bosatsu_list(listStateObj.items);
}

// Append item to list - registers bindings for new item
function _ui_list_append(listStateObj, item) {
  const index = listStateObj.items.length;
  listStateObj.items.push(item);

  // Register bindings for the new item
  const bindingKey = _listStateToBindingKey.get(listStateObj);
  if (bindingKey && _listBindingTemplates[bindingKey]) {
    _registerListItemBindings(listStateObj, bindingKey, index);
  }

  // Trigger list re-render (simple approach - re-render container)
  _renderListItems(listStateObj);

  return []; // Unit
}

// Remove item at index - cleans up bindings
function _ui_list_remove_at(listStateObj, index) {
  if (index >= 0 && index < listStateObj.items.length) {
    listStateObj.items.splice(index, 1);

    // Cleanup bindings for removed item and re-index remaining
    const bindingKey = _listStateToBindingKey.get(listStateObj);
    if (bindingKey) {
      _cleanupListItemBindings(bindingKey, index);
      _reindexListBindings(bindingKey, index, listStateObj.items.length);
    }

    // Trigger list re-render
    _renderListItems(listStateObj);
  }

  return []; // Unit
}

// Update item at index - triggers binding update
function _ui_list_update_at(listStateObj, index, item) {
  if (index >= 0 && index < listStateObj.items.length) {
    listStateObj.items[index] = item;

    // Trigger bindings for this item
    const bindingKey = _listStateToBindingKey.get(listStateObj);
    if (bindingKey) {
      _updateListItemBindings(bindingKey, index, item);
    }
  }

  return []; // Unit
}

// Template bindings for lists (populated at compile time)
// Format: { "listName": { "$.field": { property, selectorPattern, transform } } }
const _listBindingTemplates = {};

// Register bindings for a new list item at given index
function _registerListItemBindings(listStateObj, bindingKey, index) {
  const templates = _listBindingTemplates[bindingKey];
  if (!templates) return;

  Object.entries(templates).forEach(([fieldPattern, template]) => {
    // Create concrete binding key: "todos.0.completed" from "todos.$.completed"
    const concreteKey = bindingKey + '.' + index + fieldPattern.substring(1);
    const selector = template.selectorPattern.replace(/\$/g, String(index));

    if (!_bindings[concreteKey]) {
      _bindings[concreteKey] = [];
    }
    _bindings[concreteKey].push({
      elementId: selector,
      property: template.property,
      transform: template.transform,
      when: null
    });
  });
}

// Cleanup bindings for a removed list item
function _cleanupListItemBindings(bindingKey, index) {
  const prefix = bindingKey + '.' + index;
  Object.keys(_bindings).forEach(key => {
    if (key.startsWith(prefix)) {
      delete _bindings[key];
    }
  });
}

// Re-index bindings after a removal
function _reindexListBindings(bindingKey, removedIndex, newLength) {
  // This is a simplified approach - for complex cases we might need
  // to track the original indices and remap
  // For now, we rely on re-rendering to fix element IDs
}

// Update bindings for a specific list item
function _updateListItemBindings(bindingKey, index, item) {
  const prefix = bindingKey + '.' + index;
  Object.entries(_bindings).forEach(([key, bindings]) => {
    if (key.startsWith(prefix)) {
      bindings.forEach(binding => {
        // Extract the field name from the binding key
        const fieldPath = key.substring(prefix.length + 1);
        const value = _getFieldValue(item, fieldPath);
        _updateBinding(binding, value);
      });
    }
  });
}

// Get a field value from an item (supports nested paths like "name" or "address.city")
function _getFieldValue(item, fieldPath) {
  if (!fieldPath) return item;
  const parts = fieldPath.split('.');
  let current = item;
  for (const part of parts) {
    if (current == null) return null;
    // Bosatsu structs are arrays: [constructorIndex, field0, field1, ...]
    if (Array.isArray(current) && !isNaN(parseInt(part))) {
      current = current[parseInt(part) + 1]; // +1 because index 0 is constructor
    } else if (typeof current === 'object') {
      current = current[part];
    } else {
      return null;
    }
  }
  return current;
}

// Container elements for list rendering
const _listContainers = {};

// Render list items into their container
function _renderListItems(listStateObj) {
  const bindingKey = _listStateToBindingKey.get(listStateObj);
  const containerId = _listContainers[listStateObj.id];
  if (!containerId) return;

  const container = document.getElementById(containerId) ||
                    document.querySelector('[data-list-container="' + bindingKey + '"]');
  if (!container) return;

  // Get the item template (stored during initial render)
  const template = _listItemTemplates[bindingKey];
  if (!template) return;

  // Clear and re-render all items
  container.innerHTML = '';
  listStateObj.items.forEach((item, index) => {
    const itemEl = _renderListItem(template, item, index, bindingKey);
    container.appendChild(itemEl);

    // Cache elements and set up handlers
    _cacheListItemElements(itemEl, index, bindingKey);
  });

  // Re-initialize handlers for new elements
  _initBindingCache();
}

// Item templates for list rendering (populated from VNode analysis)
const _listItemTemplates = {};

// Render a single list item from template
function _renderListItem(template, item, index, listKey) {
  // Clone the template and fill in item data
  const vnode = _instantiateTemplate(template, item, index, listKey);
  return _renderVNode(vnode);
}

// Instantiate a template with item data
function _instantiateTemplate(template, item, index, listKey) {
  if (!template) return { type: 'text', text: String(item) };

  if (template.type === 'text') {
    // Check if text has a binding
    if (template.binding) {
      const value = _getFieldValue(item, template.binding);
      return { type: 'text', text: String(value) };
    }
    return template;
  }

  if (template.type === 'element') {
    // Clone props and substitute $INDEX
    const props = template.props ? _bosatsuListToArray(template.props).map(prop => {
      if (Array.isArray(prop) && prop.length >= 2) {
        const key = _bosatsuStringToJs(prop[0]) || prop[0];
        let value = _bosatsuStringToJs(prop[1]) || prop[1];
        value = String(value).replace(/\$INDEX/g, String(index));
        return [key, value];
      }
      return prop;
    }) : [];

    // Recursively instantiate children
    const children = template.children ?
      _bosatsuListToArray(template.children).map(child =>
        _instantiateTemplate(child, item, index, listKey)
      ) : [];

    return {
      type: 'element',
      tag: template.tag,
      props: _js_to_bosatsu_list(props.map(p => [p[0], p[1]])),
      children: _js_to_bosatsu_list(children)
    };
  }

  return template;
}

// Cache elements for a rendered list item
function _cacheListItemElements(itemEl, index, listKey) {
  // Find all elements with IDs or data-bosatsu-id and cache them
  const elementsWithId = itemEl.querySelectorAll('[id], [data-bosatsu-id]');
  elementsWithId.forEach(el => {
    const id = el.id || el.getAttribute('data-bosatsu-id');
    if (id) {
      _elements[id] = el;
    }
  });
}"""
  }

  /**
   * Generate CSS styles.
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
  padding: 20px;
}

#app {
  width: 100%;
  max-width: 600px;
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
  min-width: 80px;
  min-height: 80px;
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

input[type="text"], input[type="number"] {
  padding: 12px 16px;
  font-size: 16px;
  border: 2px solid #e0e0e0;
  border-radius: 8px;
  width: 100%;
  max-width: 300px;
}

input:focus {
  outline: none;
  border-color: $accentColor;
}

/* Todo List Styles */
.container {
  width: 100%;
  max-width: 600px;
}

.container .card {
  margin-bottom: 20px;
}

.subtitle {
  color: #9aa0d4;
  font-size: 14px;
  margin-bottom: 24px;
}

.todo-list {
  display: flex;
  flex-direction: column;
  gap: 8px;
  margin: 20px 0;
  text-align: left;
}

.todo-item {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 14px 16px;
  background: rgba(102, 126, 234, 0.05);
  border-radius: 12px;
  border: 1px solid rgba(102, 126, 234, 0.1);
  transition: background 0.2s;
}

.todo-item:hover {
  background: rgba(102, 126, 234, 0.1);
}

.todo-checkbox {
  width: 24px;
  height: 24px;
  border-radius: 6px;
  border: 2px solid #667eea;
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: center;
  transition: all 0.2s;
  flex-shrink: 0;
}

.todo-checkbox:hover {
  background: rgba(102, 126, 234, 0.2);
}

.todo-checkbox.checked {
  background: $accentColor;
  border-color: $accentColor;
}

.todo-checkbox.checked::after {
  content: '\u2713';
  color: white;
  font-weight: bold;
  font-size: 14px;
}

.todo-text {
  flex: 1;
  font-size: 15px;
}

.binding-info {
  background: rgba(0, 0, 0, 0.05);
  border-radius: 8px;
  padding: 12px;
  font-family: 'SF Mono', Consolas, monospace;
  font-size: 11px;
  color: #666;
  margin-top: 16px;
  text-align: left;
}

h3 {
  font-size: 14px;
  color: #666;
  margin-bottom: 12px;
}

p {
  font-size: 14px;
  color: #666;
  line-height: 1.6;
}"""
  }

  private def escapeHtml(s: String): String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
}

object UICommand {

  implicit val pathArgument: Argument[Path] = new Argument[Path] {
    def read(string: String): ValidatedNel[String, Path] =
      try {
        Validated.valid(Paths.get(string))
      } catch {
        case e: Exception => Validated.invalidNel(e.getMessage)
      }
    def defaultMetavar: String = "path"
  }

  val opts: Opts[UICommand] = {
    val input = Opts.argument[Path]("input")
    val output = Opts
      .option[Path]("output", "Output HTML file", "o")
      .withDefault(Paths.get("output.html"))
    val titleOpt = Opts.option[String]("title", "Page title", "t").orNone
    val themeOpt = Opts
      .option[String]("theme", "Theme: light or dark")
      .withDefault("light")
    val sourceMap = Opts.flag("source-map", "Include source map for debugging").orFalse

    (input, output, titleOpt, themeOpt, sourceMap).mapN {
      (i, o, t, th, sm) => UICommand(i, o, t, th, sm)
    }
  }

  val command: Command[UICommand] =
    Command("bosatsu-ui", "Generate reactive UI HTML from Bosatsu/UI code")(opts)

  def parse(args: List[String]): Either[Help, UICommand] =
    command.parse(args)
}
