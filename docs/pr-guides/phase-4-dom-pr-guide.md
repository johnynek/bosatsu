# Phase 4: Basic DOM Primitives - PR Guide

## Overview

Phase 4 introduces the foundational building blocks for BosatsuUI - a Virtual DOM representation (`VNode`) and code generation for native JavaScript DOM manipulation (`DOMCodegen`).

## Key Components

### 1. VNode.scala - Virtual DOM Types

Location: `core/src/main/scala/dev/bosatsu/ui/VNode.scala`

The Virtual DOM representation provides:

- **VNode sealed trait** - Base type for all virtual DOM nodes
- **VNode.Element** - HTML elements with tag, attributes, event handlers, and children
- **VNode.Text** - Plain text content
- **VNode.Component** - Reusable UI components with render functions
- **AttributeValue** - Type-safe attribute values (String, Int, Double, Bool, Null)
- **EventHandler** - Event handler wrapping Bosatsu expressions
- **DOMBindings** - Result of static analysis for state subscriptions

### 2. DOMCodegen.scala - DOM Code Generation

Location: `core/src/main/scala/dev/bosatsu/ui/DOMCodegen.scala`

Generates native JavaScript for DOM manipulation:

- **generate()** - Create DOM element from VNode
- **generateRenderFunction()** - Generate complete render function
- **generateModule()** - Generate ES module with exports
- **renderToString()** - Server-side rendering / testing

Generated code patterns:
```javascript
const el = document.createElement("div");
el.setAttribute("class", "container");
el.addEventListener("click", (event) => { ... });
el.appendChild(childEl);
```

### 3. DOMRuntime.scala - Scala.js DOM Runtime

Location: `core/.js/src/main/scala/dev/bosatsu/ui/DOMRuntime.scala`

A Scala.js runtime for actual DOM manipulation, using raw JS interop (`js.Dynamic`) to avoid external dependencies:

- **createElement/createTextNode** - Create DOM nodes
- **setAttribute/removeAttribute/setAttributes** - Attribute manipulation
- **addEventListener/removeEventListener** - Event handling
- **appendChild/removeChild/replaceChild/insertBefore** - Tree manipulation
- **querySelector/querySelectorAll/getElementById** - DOM queries
- **addClass/removeClass/toggleClass/hasClass** - CSS class manipulation
- **setStyle** - Inline style manipulation
- **render(VNode)** - Render a VNode tree to real DOM nodes
- **mount/mountById** - Mount a VNode tree to a container element

This file is Scala.js-specific (in `.js/src/main/scala/`) and uses `js.Dynamic` for all DOM operations, making it dependency-free while still providing type-safe Scala APIs.

### 4. Test Files

- **VNodeGen.scala** - ScalaCheck generators for VNode trees
- **VNodeTest.scala** - Property-based tests (22 tests)

## PR Breakdown

### PR 1: VNode Types (Foundation)

**Files:**
- `core/src/main/scala/dev/bosatsu/ui/VNode.scala`

**Changes:**
- VNode sealed trait and case classes
- AttributeValue types with rendering
- EventHandler case class
- DOMBindings case class
- Smart constructors (el, text, div, span, etc.)

**Tests:** Unit tests for type construction

**Reviewable in:** ~10 min

**Dependencies:** None

### PR 2: DOMCodegen (Core Implementation)

**Files:**
- `core/src/main/scala/dev/bosatsu/ui/DOMCodegen.scala`

**Changes:**
- generate() for VNode → JS statements
- generateRenderFunction() for complete render functions
- generateModule() for ES modules
- renderToString() for SSR/testing

**Tests:** Property tests for codegen correctness

**Reviewable in:** ~15 min

**Dependencies:** PR 1 (VNode types), Phase 3 (Code.scala)

### PR 3: DOMRuntime (Scala.js Platform-Specific)

**Files:**
- `core/.js/src/main/scala/dev/bosatsu/ui/DOMRuntime.scala`

**Changes:**
- Scala.js DOM manipulation runtime using `js.Dynamic`
- VNode rendering to actual DOM
- Mount functions for container elements
- Event handling with preventDefault/stopPropagation

**Tests:** Requires browser environment (JSDOM or real browser)

**Reviewable in:** ~10 min

**Dependencies:** PR 1 (VNode types)

### PR 4: VNode Generators & Tests

**Files:**
- `core/src/test/scala/dev/bosatsu/ui/VNodeGen.scala`
- `core/src/test/scala/dev/bosatsu/ui/VNodeTest.scala`

**Changes:**
- ScalaCheck generators for VNode trees
- 22 property-based tests covering:
  - VNode structure invariants
  - renderToString HTML validity
  - DOM codegen correctness

**Tests:** Self-contained test suite

**Reviewable in:** ~10 min

**Dependencies:** PRs 1 & 2

## Test Coverage

| Test Category | Count | Description |
|---------------|-------|-------------|
| Structure invariants | 4 | VNode depth, node count |
| renderToString | 4 | HTML output, escaping, attributes |
| DOM codegen | 5 | Statement generation, function creation |
| AttributeValue | 1 | Type rendering |
| Smart constructors | 4 | el, text, div, keyedEl |
| Edge cases | 4 | Void elements, nesting |

## Design Decisions

### 1. Sealed Trait for VNode

Using a sealed trait allows exhaustive pattern matching and ensures all VNode types are handled by codegen.

### 2. Separate AttributeValue Type

Type-safe attribute values prevent runtime errors and enable proper HTML escaping.

### 3. Direct DOM Manipulation vs Virtual DOM Diffing

DOMCodegen generates code that directly creates DOM elements rather than using a virtual DOM diffing algorithm. This is faster for initial renders and simpler to debug.

### 4. renderToString for Testing

The renderToString method enables testing without a browser environment (JSDOM not required for basic tests).

## Usage Examples

### Creating VNodes

```scala
import dev.bosatsu.ui.VNode._
import dev.bosatsu.ui.AttributeValue._

val button = el("button",
  Map("class" -> StringValue("btn"), "disabled" -> BoolValue(false)),
  text("Click me")
)

val list = ul(
  li(text("Item 1")),
  li(text("Item 2"))
)
```

### Generating JavaScript

```scala
import dev.bosatsu.ui.DOMCodegen

val statements = DOMCodegen.generate(button, "btn")
val html = DOMCodegen.renderToString(button)
val module = DOMCodegen.generateModule(list, "ListModule")
```

### Using DOMRuntime (Scala.js only)

```scala
import dev.bosatsu.ui.{DOMRuntime, VNode}

// Create a VNode tree
val app = VNode.div(
  VNode.h1(VNode.text("Hello, World!")),
  VNode.p(VNode.text("Welcome to BosatsuUI"))
)

// Mount to a container (requires browser environment)
DOMRuntime.mount(app, "#app")

// Or mount by ID
DOMRuntime.mountById(app, "app")
```

## Future Work (Phase 5+)

- Event handler execution (requires Bosatsu runtime integration)
- State management and reactive updates
- Virtual DOM diffing for efficient updates
- Component lifecycle hooks

## Migration Notes

This is a new module with no breaking changes to existing code.

## Verification

```bash
# Run UI tests (JVM - VNode, DOMCodegen)
sbt "coreJVM/testOnly dev.bosatsu.ui.*"

# Check JVM compilation
sbt "coreJVM/compile"

# Check JS compilation (includes DOMRuntime)
sbt "coreJS/compile"

# Run full JS test suite
sbt "coreJS/test"
```

## References

- Phase 3 JsGen (dependency): `core/src/main/scala/dev/bosatsu/codegen/js/Code.scala`
- Plan document: `docs/plans/2026-01-26-feat-bosatsu-provenance-tooling-plan.md` (Phase 4 section)
- BurritoScript UI reference: `src/ui/index.ts`
