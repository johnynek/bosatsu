---
title: BosatsuUI Generation Pipeline
date: 2026-01-29
status: accepted
---

# BosatsuUI Generation Pipeline

## What We're Building

A complete pipeline from `.bosatsu` UI source → working HTML with **direct DOM updates** (no virtual DOM diffing). Uses **static analysis on TypedExpr** to extract state→DOM bindings at compile time.

## The Problem

Current `web_deploy/demos/ui/` demos (counter.html, todo-list.html) are **hand-written HTML/JS** - no corresponding `.bosatsu` source, no generation pipeline.

We have framework pieces (VNode, ReactiveState, DOMCodegen, UIAnalyzer) but they're not wired together, and UIAnalyzer has stub implementations that don't actually extract bindings.

## Inspiration: BurritoUI

BurritoUI in `/Users/steven/Documents/Code/portToBosatsu/burritoscript` achieves **O(1) updates** without virtual DOM diffing:

```
COMPILE TIME: Source → Effect AST → Binding Map
   state.read(['count']) flows into text() child of h('span', {id: 'display'})
   Result: { statePath: ["count"], selector: "#display", property: "textContent" }

RUNTIME: setState(['count'], 42)
   bindingMap.get("count") → find cached element → apply directly
   No tree walk. No diff. O(1).
```

**How they track data flow:** Symbol injection at runtime - replace state reads with Symbols, run the code, see where Symbols end up in the VNode tree.

## Why Bosatsu Can Do Better

BurritoUI needs runtime evaluation because TypeScript doesn't expose data flow statically. **Bosatsu has TypedExpr** - the entire program as a typed AST where:

- Every subexpression has a type
- All bindings are explicit (Let, Lambda, Match)
- External calls are identifiable
- Data flow is traceable without running anything

**Pure static analysis beats symbol injection:**

```scala
// TypedExpr for: text(int_to_string(read(count)))
App(text,
  App(int_to_string,
    App(read, Var(count))))

// Static analysis walks this tree:
// 1. Find App(read, _) → marks as state read
// 2. Trace where that value flows → into text()
// 3. Find enclosing h() with id → extract selector
// No runtime. No symbols. Just tree walking.
```

## Chosen Approach: TypedExpr Binding Extraction

### Core Data Structures

```scala
// What we extract from TypedExpr
case class DOMBinding(
  selector: String,           // "#display" or "[data-bosatsu-id='el-0']"
  property: DOMProperty,      // TextContent, ClassName, Value, Style(prop), etc.
  statePath: List[String],    // ["count"] or ["user", "name"]
  conditional: Boolean,       // Inside a match/if?
  transform: Option[String]   // "int_to_string" etc.
)

case class UIAnalysis(
  stateReads: List[List[String]],  // All state paths read
  bindings: List[DOMBinding],       // State → DOM mappings
  eventHandlers: List[EventBinding] // onClick, onInput, etc.
)
```

### Extraction Algorithm

```scala
def extractBindings(expr: TypedExpr[A]): UIAnalysis = {
  // 1. Find all state.read calls and what paths they read
  val stateReads = findStateReads(expr)

  // 2. For each state read, trace where its value flows
  val bindings = stateReads.flatMap { case (readExpr, path) =>
    traceToDOM(readExpr, path, expr)
  }

  // 3. Find event handler registrations
  val handlers = findEventHandlers(expr)

  UIAnalysis(stateReads.map(_._2), bindings, handlers)
}

def traceToDOM(readExpr: TypedExpr, path: List[String], root: TypedExpr): List[DOMBinding] = {
  // Walk up from readExpr to find where it's used
  // If it reaches text() → TextContent binding
  // If it reaches h() props → Attribute/Style binding
  // Track transforms along the way (int_to_string, etc.)
}
```

### Key Patterns to Recognize

```bosatsu
# Pattern 1: Direct text binding
h("span", [("id", "display")], [text(int_to_string(read(count)))])
# → Binding(selector="#display", property=TextContent, path=["count"])

# Pattern 2: Attribute binding
h("div", [("class", read(theme))], [...])
# → Binding(selector=generated, property=ClassName, path=["theme"])

# Pattern 3: Conditional binding
match read(status):
  Loading: h("div", [], [text("Loading...")])
  Ready(data): h("div", [], [text(data)])
# → Binding with conditional=true, condition on status

# Pattern 4: Event handler
h("button", [on_click(\() -> write(count, add(read(count), 1)))], [...])
# → EventBinding(selector=..., event="click", handler=...)
```

### Generated Runtime

Instead of virtual DOM diffing, generate a runtime that:

1. **Mount:** Create DOM once, cache element references by selector
2. **setState:** Look up bindings for path, apply directly to cached elements

```javascript
// Generated from analysis
const _bindings = {
  "count": [
    { selector: "#display", property: "textContent", transform: String }
  ]
};

const _cache = new Map();  // selector → Element

function mount(root, initialState) {
  // Create DOM from VNode, populate cache
  root.appendChild(createDOM(view, _cache));
  _state = initialState;
}

function setState(path, value) {
  _state = setPath(_state, path, value);
  const key = JSON.stringify(path);
  for (const binding of _bindings[key] || []) {
    const el = _cache.get(binding.selector);
    applyBinding(el, binding.property, value, binding.transform);
  }
}
```

## Implementation Phases

### Phase 1: Bosatsu/UI Package

Define the primitives in Bosatsu:

```bosatsu
package Bosatsu/UI

export (State, read, write, VNode, h, text, on_click)

external struct State[A]
external def read(s: State[A]) -> A
external def write(s: State[A], value: A) -> ()

external struct VNode
external def h(tag: String, attrs: List[(String, String)], children: List[VNode]) -> VNode
external def text(content: String) -> VNode

external def on_click(handler: () -> ()) -> (String, String)
```

### Phase 2: TypedExpr Binding Extractor

Replace UIAnalyzer stubs with real TypedExpr analysis:

```scala
object BindingExtractor {
  def extract[A](expr: TypedExpr[A]): UIAnalysis = {
    val stateReads = collectStateReads(expr)
    val bindings = stateReads.flatMap { case (readExpr, path) =>
      traceValueFlow(readExpr, path, expr)
    }
    val handlers = collectEventHandlers(expr)
    UIAnalysis(stateReads.map(_._2), bindings, handlers)
  }

  private def collectStateReads[A](expr: TypedExpr[A]): List[(TypedExpr[A], List[String])] = {
    // Find App(Global("read"), ...) patterns
    // Extract the state path argument
  }

  private def traceValueFlow[A](
    readExpr: TypedExpr[A],
    path: List[String],
    root: TypedExpr[A]
  ): List[DOMBinding] = {
    // Walk the expression tree to see where readExpr's value flows
    // If it reaches text() → TextContent binding
    // If it reaches h() attrs → Attribute binding
  }
}
```

### Phase 3: Counter Demo

Write real `demo/counter.bosatsu`:

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

### Phase 4: Generation Pipeline

Extend simulation-cli:

```bash
bosatsu-ui demo/counter.bosatsu -o web_deploy/demos/ui/counter.html
```

Pipeline:
1. Parse → TypedExpr
2. BindingExtractor.extract() → UIAnalysis
3. Generate runtime JS with binding map
4. Generate initial HTML from VNode
5. Wrap in template

### Phase 5: Delete Fake Demos

Once generated demos work, delete hand-written versions.

## Key Decisions

1. **Static analysis on TypedExpr** - No runtime symbol injection like BurritoUI
2. **Direct DOM updates** - No virtual DOM diffing at runtime
3. **Binding map generated at compile time** - O(1) updates
4. **Element caching by selector** - No DOM queries at update time
5. **Start with counter** - Simplest case to prove the approach

## Comparison: React vs BurritoUI vs BosatsuUI

| Approach | Update Cost | Analysis | Diffing |
|----------|-------------|----------|---------|
| React | O(tree size) | None | Full VNode diff every update |
| BurritoUI | O(bindings for path) | Runtime symbols | None - direct updates |
| **BosatsuUI** | O(bindings for path) | **Static TypedExpr** | None - direct updates |

BosatsuUI matches BurritoUI's runtime performance but with **compile-time analysis** instead of runtime symbol tracking.

## Success Criteria

- [ ] `Bosatsu/UI` package compiles with external defs
- [ ] BindingExtractor correctly traces state reads to DOM properties
- [ ] `demo/counter.bosatsu` produces working `counter.html`
- [ ] Generated HTML updates only affected elements (verify in Playwright)
- [ ] No hand-written JS in generated output
- [ ] Performance: setState completes in <1ms for simple updates

## Open Questions

1. **How to handle list rendering?** - Keyed children need special treatment
2. **Conditional rendering** - How deep should condition tracking go?
3. **Nested state paths** - `["user", "profile", "name"]` - how to handle?
4. **Event handler codegen** - Closures in Bosatsu → JS functions

## Next Steps

1. `/workflows:plan` to break Phase 1-2 into tasks
2. Define Bosatsu/UI package
3. Implement BindingExtractor on TypedExpr
4. Write counter.bosatsu
5. Wire through simulation-cli
