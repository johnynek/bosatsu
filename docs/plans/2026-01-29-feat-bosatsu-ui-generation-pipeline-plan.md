---
title: "feat: BosatsuUI Generation Pipeline"
type: feat
date: 2026-01-29
---

# BosatsuUI Generation Pipeline

## Overview

Build a complete pipeline from `.bosatsu` UI source → working HTML with **direct DOM updates** (no virtual DOM diffing). Uses **static analysis on TypedExpr** to extract state→DOM bindings at compile time. Includes source maps for browser debugging back to original `.bosatsu` source, and performance benchmarks comparing to React/Elm.

## Problem Statement

Current `web_deploy/demos/ui/` demos (counter.html, todo-list.html) are **hand-written HTML/JS** - no corresponding `.bosatsu` source, no generation pipeline. The demos claim to show BosatsuUI's approach but aren't actually generated from Bosatsu code.

We have framework pieces (VNode, ReactiveState, DOMCodegen, UIAnalyzer) but:
1. They're not wired together into a complete pipeline
2. UIAnalyzer has stub implementations that don't actually extract bindings
3. No `Bosatsu/UI` module exists for developers to import
4. No source map generation for debugging

## Proposed Solution

Implement a TypedExpr-based binding extraction system that generates optimized JavaScript with O(1) direct DOM updates:

```
.bosatsu UI source
       ↓
┌─────────────────────────────────────┐
│  Bosatsu Compiler (TypedExpr AST)   │
└─────────────────────────────────────┘
       ↓
┌─────────────────────────────────────┐
│  UIAnalyzer (extract bindings)      │
│  state path → DOM selector+property │
└─────────────────────────────────────┘
       ↓
┌─────────────────────────────────────┐
│  JsGen + Source Map Generator       │
│  (JS code with VLQ-encoded maps)    │
└─────────────────────────────────────┘
       ↓
┌─────────────────────────────────────┐
│  SimulationGen / EmbedGenerator     │
│  (HTML + runtime + bindings)        │
└─────────────────────────────────────┘
       ↓
   Working HTML with source maps
```

## Technical Approach

### Architecture

**Core Data Structures:**

```scala
// core/src/main/scala/dev/bosatsu/ui/UIAnalyzer.scala
case class DOMBinding[A](
  selector: String,           // "#display" or "[data-bosatsu-id='el-0']"
  property: DOMProperty,      // TextContent, ClassName, Value, Style(prop)
  statePath: List[String],    // ["count"] or ["user", "name"]
  conditional: Boolean,       // Inside a match/if?
  transform: Option[String],  // "int_to_string" etc.
  sourceExpr: TypedExpr[A]    // Reference for source maps
)

case class EventBinding[A](
  selector: String,
  event: String,              // "click", "input", "change"
  handler: TypedExpr[A]       // Lambda expression
)

case class UIAnalysis[A](
  stateReads: List[List[String]],
  bindings: List[DOMBinding[A]],
  eventHandlers: List[EventBinding[A]]
)
```

**Source Map Integration:**

```scala
// core/src/main/scala/dev/bosatsu/codegen/js/SourceMapGenerator.scala
case class SourceMapping(
  generatedLine: Int,
  generatedColumn: Int,
  originalFile: String,
  originalLine: Int,
  originalColumn: Int,
  name: Option[String]
)

class SourceMapGenerator {
  def addMapping(mapping: SourceMapping): Unit
  def toJSON: String  // V3 format with VLQ encoding
  def toInlineComment: String  // data: URL for embedding
}
```

### Implementation Phases

#### Phase 1: Bosatsu/UI Package Definition

Create the UI primitives as a proper Bosatsu module with external implementations.

**Files:**
- `core/src/main/resources/bosatsu/ui.bosatsu` - Package definition
- `core/src/main/scala/dev/bosatsu/codegen/js/JsGen.scala` - UIExternal intrinsics (already exists, needs completion)

**Tasks:**
- [x] Define `Bosatsu/UI` package with type signatures
- [x] Implement `h(tag, props, children) -> VNode`
- [x] Implement `text(content) -> VNode`
- [x] Implement `fragment(children) -> VNode`
- [x] Implement `State[A]` external struct
- [x] Implement `read(state) -> A`
- [x] Implement `write(state, value) -> ()`
- [x] Implement event handlers: `on_click`, `on_input`, `on_change`
- [x] Add tests for package parsing and type checking

**Acceptance Criteria:**
- [x] `from Bosatsu/UI import h, text, State, read, write` compiles
- [ ] Type errors for wrong argument types
- [ ] Event handler lambdas type-check correctly

#### Phase 2: TypedExpr Binding Extractor

Replace UIAnalyzer stubs with real TypedExpr analysis using State monad pattern.

**Files:**
- `core/src/main/scala/dev/bosatsu/ui/UIAnalyzer.scala` - Complete implementation

**Tasks:**
- [ ] Implement `collectStateReads` to find `App(Global("read"), ...)` patterns
- [ ] Implement `traceValueFlow` to track where read values flow (into text(), h() props)
- [ ] Implement `extractSelector` to determine CSS selector for each binding
- [ ] Implement `extractDOMProperty` to determine which property (textContent, className, value)
- [ ] Implement `collectEventHandlers` to find on_click/on_input calls
- [ ] Track conditional context (inside match/if expressions)
- [ ] Handle transform functions (int_to_string, etc.)
- [ ] Add comprehensive tests for binding extraction

**Key Algorithm:**

```scala
def traceValueFlow[A](readExpr: TypedExpr[A], path: List[String], root: TypedExpr[A]): List[DOMBinding[A]] = {
  // Walk up from readExpr to find where it's used
  // 1. If reaches text() → TextContent binding
  // 2. If reaches h() props with key "class" → ClassName binding
  // 3. If reaches h() props with key "value" → Value binding
  // 4. Track parent elements to determine selector
}
```

**Acceptance Criteria:**
- [ ] `text(int_to_string(read(count)))` extracts binding with transform
- [ ] `h("span", [("id", "display")], [...])` extracts selector `#display`
- [ ] Conditional bindings flagged correctly
- [ ] Event handlers extracted with correct event type

#### Phase 3: Source Map Generator

Implement V3 source map generation that tracks positions through compilation.

**Files:**
- `core/src/main/scala/dev/bosatsu/codegen/js/SourceMapGenerator.scala` (new)
- `core/src/main/scala/dev/bosatsu/codegen/js/JsGen.scala` (modify to track positions)
- `core/src/main/scala/dev/bosatsu/codegen/js/Code.scala` (add source position to Expression)

**Tasks:**
- [ ] Implement VLQ Base64 encoding
- [ ] Implement SourceMapGenerator class with addMapping/toJSON
- [ ] Add source position field to Code.Expression
- [ ] Modify JsGen.exprToJs to propagate TypedExpr.tag region
- [ ] Modify Code.render to emit position info
- [ ] Generate mappings during code emission
- [ ] Support both external .map files and inline data: URLs
- [ ] Add tests for source map generation and browser loading

**VLQ Encoding:**

```scala
object VLQ {
  private val Base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  def encode(value: Int): String = {
    val vlq = if (value < 0) ((-value) << 1) + 1 else value << 1
    encodeUnsigned(vlq)
  }

  private def encodeUnsigned(vlq: Int): String = {
    var v = vlq
    val sb = new StringBuilder
    do {
      var digit = v & 31
      v >>>= 5
      if (v > 0) digit |= 32
      sb += Base64Chars(digit)
    } while (v > 0)
    sb.toString
  }
}
```

**Acceptance Criteria:**
- [ ] Generated source map validates against V3 spec
- [ ] Browser DevTools shows original .bosatsu source
- [ ] Breakpoints in DevTools work on .bosatsu lines
- [ ] Stack traces show .bosatsu file and line numbers

#### Phase 4: Counter Demo

Write real `demos/ui/counter.bosatsu` and wire through simulation-cli.

**Files:**
- `demos/ui/counter.bosatsu` (new)
- `simulation-cli/src/main/scala/dev/bosatsu/simulation/SimulationCommand.scala` (extend for UI mode)

**Tasks:**
- [ ] Write counter.bosatsu with h(), text(), read(), write(), on_click()
- [ ] Add `--ui` flag to simulation-cli to use UIAnalyzer
- [ ] Generate binding map in output JS
- [ ] Generate element cache initialization
- [ ] Generate direct DOM update functions
- [ ] Include source map in generated HTML
- [ ] Delete hand-written counter.html after generated version works
- [ ] Add Playwright tests for generated counter

**counter.bosatsu:**

```bosatsu
package Demo/Counter

from Bosatsu/UI import State, read, write, h, text, on_click

count: State[Int] = State(0)

increment = \() -> write(count, add(read(count), 1))
decrement = \() -> write(count, sub(read(count), 1))

view = h("div", [("class", "card")], [
  h("h1", [], [text("Counter")]),
  h("span", [("id", "display")], [text(int_to_string(read(count)))]),
  h("button", [on_click(decrement)], [text("-")]),
  h("button", [on_click(increment)], [text("+")])
])

main = view
```

**Acceptance Criteria:**
- [ ] `bosatsu-sim demos/ui/counter.bosatsu -o counter.html --ui` produces working HTML
- [ ] Clicking +/- updates display via direct DOM binding
- [ ] Browser DevTools shows counter.bosatsu source
- [ ] No hand-written JS in output (all generated)

#### Phase 5: Performance Benchmarks

Reproduce BurritoUI-style benchmarks comparing BosatsuUI to React and Elm.

**Files:**
- `demos/benchmarks/ui-performance/index.html` (new)
- `demos/benchmarks/ui-performance/benchmark.bosatsu` (new)
- `demos/benchmarks/ui-performance/benchmark-runtime.ts` (new)
- `tests/e2e/ui-benchmark.spec.ts` (new)

**Tasks:**
- [ ] Create benchmark harness HTML with run buttons and results table
- [ ] Implement BosatsuUI runtime measurement
- [ ] Implement React simulation (vDOM diff)
- [ ] Implement Elm simulation (pure + vDOM)
- [ ] Benchmark scenarios:
  - [ ] Single property update (10,000 iterations)
  - [ ] Batch updates (1,000 × 100)
  - [ ] List item update (1,000 on 1000-item list)
  - [ ] Targeted deep tree update (5,000 iterations)
- [ ] Measure: avgMs, minMs, maxMs, opsPerSec
- [ ] Calculate speedup ratios vs React/Elm
- [ ] Add to deploy workflow to publish benchmarks

**Acceptance Criteria:**
- [ ] Benchmark page loads and runs all scenarios
- [ ] Results show BosatsuUI faster than React/Elm for targeted updates
- [ ] Speedup ratios calculated and displayed
- [ ] Benchmarks reproducible (warmup, stable measurements)

#### Phase 6: Todo List Demo

Convert todo-list.html to real Bosatsu source with dynamic list support.

**Files:**
- `demos/ui/todo-list.bosatsu` (new)
- `core/src/main/scala/dev/bosatsu/ui/UIAnalyzer.scala` (extend for lists)

**Tasks:**
- [ ] Extend UIAnalyzer to handle list mapping (`todos.map(todo -> h(...))`)
- [ ] Implement keyed list reconciliation for efficient updates
- [ ] Write todo-list.bosatsu with add/remove/toggle functionality
- [ ] Generate HTML with list-aware binding updates
- [ ] Delete hand-written todo-list.html
- [ ] Add Playwright tests for todo operations

**Acceptance Criteria:**
- [ ] Adding todo updates only the new item binding
- [ ] Toggling todo updates only that item's className binding
- [ ] Removing todo cleans up its bindings
- [ ] List re-order uses keyed reconciliation

#### Phase 7: Deploy Pipeline Integration

Wire generated demos into GitHub Pages deploy workflow.

**Files:**
- `.github/workflows/deploy_web.yml`

**Tasks:**
- [ ] Add simulation-cli UI build steps for counter and todo-list
- [ ] Generate source maps alongside HTML
- [ ] Add benchmark page to deploy
- [ ] Remove old hand-written demo copies
- [ ] Verify deployed site has working demos

**Acceptance Criteria:**
- [ ] Deploy workflow generates all UI demos from .bosatsu source
- [ ] Source maps accessible in deployed site
- [ ] Benchmark page published and functional

## Success Metrics

| Metric | Target |
|--------|--------|
| Counter demo generated from .bosatsu | ✓ |
| Todo demo generated from .bosatsu | ✓ |
| Source maps work in Chrome/Firefox DevTools | ✓ |
| setState completes in <1ms for simple updates | ✓ |
| Single update faster than React vDOM diff | 10x+ |
| No hand-written JS in demo output | ✓ |
| Playwright tests pass for all demos | ✓ |

## Dependencies & Risks

**Dependencies:**
- Existing UIAnalyzer infrastructure (partially implemented)
- JsGen with UIExternal intrinsics (exists, needs extension)
- simulation-cli (working, needs UI mode)
- TypedExpr.tag containing region info (exists)

**Risks:**
| Risk | Mitigation |
|------|------------|
| TypedExpr analysis complexity | Start with simple counter, iterate |
| Source map browser compatibility | Test on Chrome, Firefox, Safari |
| List reconciliation performance | Profile and optimize keyed updates |
| External def type checking | Leverage existing Bosatsu/Predef patterns |

## References & Research

### Internal References
- Brainstorm: `docs/brainstorms/2026-01-29-bosatsu-ui-generation-pipeline-brainstorm.md`
- UIAnalyzer: `core/src/main/scala/dev/bosatsu/ui/UIAnalyzer.scala`
- JsGen UIExternal: `core/src/main/scala/dev/bosatsu/codegen/js/JsGen.scala:543-594`
- TypedExpr: `core/src/main/scala/dev/bosatsu/TypedExpr.scala`
- SimulationGen: `simulation-cli/src/main/scala/dev/bosatsu/simulation/SimulationGen.scala`

### External References
- Source Map V3 Spec: https://tc39.es/ecma426/
- BurritoUI Benchmarks: `/Users/steven/Documents/Code/portToBosatsu/burritoscript/demos/benchmarks/ui-performance/`

### Documented Learnings Applied
- Store AST references, not copies: `docs/solutions/design-patterns/store-ast-references-not-copies.md`
- State monad for AST analysis: `docs/solutions/design-patterns/state-monad-for-ast-analysis.md`
- Canvas selector fixes: `docs/solutions/runtime-errors/canvas-visualization-container-selector-fixes.md`

## Open Questions Resolved

1. **How to handle list rendering?** → Keyed children with O(1) binding lookup per key
2. **Conditional rendering?** → Track `conditional` flag, update when condition expression changes
3. **Nested state paths?** → `["user", "profile", "name"]` → immutable update with path granularity
4. **Event handler codegen?** → Closures capture state references, compile to JS arrow functions
5. **Source maps?** → V3 format with VLQ encoding, embed in HTML or serve as .map file
