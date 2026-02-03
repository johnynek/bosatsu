# Conditional Rendering via Static Analysis

**Date:** 2026-01-30
**Status:** Exploration

## What We're Building

Enhance BosatsuUI's static analysis to handle conditional rendering (sum types, lists, boolean conditions) with the same performance advantages as simple bindings—no VDOM diffing, O(1) updates.

## Why This Approach

### The Core Insight

Bosatsu has properties React lacks:
1. **No mutations** - state transitions are explicit, traceable
2. **No runaway recursion** - all recursion terminates
3. **Full type information** - we know the shape of every value at compile time
4. **Sum types with exhaustive matching** - every branch is statically known

**This means we can statically enumerate all possible UI states.**

React must diff at runtime because it doesn't know what the next render will produce. BosatsuUI can know at compile time.

### Current Limitation

The current analyzer marks bindings as `conditional: Boolean` but doesn't track:
- Which condition controls the binding
- What value the condition must have
- How conditions relate to each other

```scala
// Current: binary flag only
DOMBinding(elementId, property, statePath, conditional = true, ...)
```

### The Key Idea: Condition Hoisting

Instead of embedding conditions in DOM structure:
```bosatsu
match status:
  Loading: h("div", [], [text("Loading...")])
  Success(data): h("div", [], [text(data.name)])
```

Hoist conditions into the binding model:
```javascript
// Hoisted conditional binding
{
  elementId: "div-1",
  property: "textContent",
  conditions: [
    { when: "status.tag == Loading", value: "Loading..." },
    { when: "status.tag == Success", value: "status.Success.name" }
  ]
}
```

The binding IS the condition—the DOM structure becomes static.

## Binding Representations to Explore

### 1. Conditional Bindings

Each binding has explicit `when` clauses:

```javascript
{
  elementId: "bosatsu-0",
  property: "textContent",
  when: { path: "status", tag: "Success" },
  valuePath: ["status", "Success", "data", "name"]
}
```

**Pros:**
- Simple, flat structure
- Easy to evaluate: check condition, apply if true
- Matches current binding model closely

**Cons:**
- Doesn't capture relationships between conditions
- May redundantly check same condition for multiple bindings

### 2. Binding DAG (Directed Acyclic Graph)

Conditions are nodes, bindings are edges:

```
       [status.tag]
         /     \
    Loading   Success
       |         |
   bind-1    bind-2
```

**Pros:**
- Captures condition relationships
- Can topologically sort for efficient evaluation
- Natural for nested conditions

**Cons:**
- More complex data structure
- Harder to serialize for worker transfer

### 3. State Machine

Enumerate all (condition-set, bindings) pairs:

```javascript
const uiStates = {
  "status=Loading": {
    bindings: [{ el: "div-1", prop: "textContent", value: "Loading..." }],
    visibility: { "div-1": true, "div-2": false }
  },
  "status=Success": {
    bindings: [{ el: "div-1", prop: "textContent", valuePath: ["status", "Success", "name"] }],
    visibility: { "div-1": true, "div-2": false }
  }
}
```

**Pros:**
- O(1) state transition: look up new state, apply diff
- Perfect for finite sum types
- Can pre-compute transitions

**Cons:**
- State explosion with many independent conditions
- May not work well for lists (unbounded states)

## Worker-Based Architecture

Since bindings are pure functions of state, we can:

1. **Main thread:** Receives state change, posts to worker
2. **Worker:** Evaluates conditional bindings, computes DOM updates
3. **Worker:** Posts back `{elementId, property, value}[]`
4. **Main thread:** Applies updates (DOM writes only)

This keeps the main thread doing only DOM writes while condition evaluation runs in parallel.

```javascript
// Main thread
stateWorker.postMessage({ type: 'setState', path: ['status'], value: { tag: 'Success', data: {...} } });

// Worker
self.onmessage = (e) => {
  const updates = evaluateBindings(state, bindingGraph);
  self.postMessage({ type: 'domUpdates', updates });
};

// Main thread
worker.onmessage = (e) => {
  for (const { elementId, property, value } of e.data.updates) {
    document.getElementById(elementId)[property] = value;
  }
};
```

## Sum Type Analysis Deep Dive

Given Bosatsu's type system, analyzing a match expression:

```bosatsu
def render(status: Status) -> VNode:
  match status:
    Loading: h("div", [("class", "loading")], [text("Please wait...")])
    Error(msg): h("div", [("class", "error")], [text(msg)])
    Success(user): h("div", [("class", "success")], [
      h("span", [], [text(user.name)]),
      h("span", [], [text(int_to_string(user.age))])
    ])
```

**Static analysis can extract:**

1. **Discriminant path:** `status` (the value being matched)
2. **Variant tags:** `Loading`, `Error`, `Success`
3. **Per-variant bindings:**
   - `Loading`: static text, class="loading"
   - `Error`: `msg` binds to textContent, class="error"
   - `Success`: `user.name`, `user.age` bind to textContent, class="success"

4. **Structural differences:**
   - `Loading`/`Error`: 1 text node
   - `Success`: 2 span children

**Output representation:**

```javascript
{
  discriminant: ["status"],
  variants: {
    "Loading": {
      structure: { tag: "div", children: [{ tag: "text" }] },
      bindings: [
        { el: "bosatsu-0", prop: "className", value: "loading" },
        { el: "bosatsu-0-text", prop: "textContent", value: "Please wait..." }
      ]
    },
    "Error": {
      structure: { tag: "div", children: [{ tag: "text" }] },
      bindings: [
        { el: "bosatsu-0", prop: "className", value: "error" },
        { el: "bosatsu-0-text", prop: "textContent", valuePath: ["status", "Error", "msg"] }
      ]
    },
    "Success": {
      structure: { tag: "div", children: [{ tag: "span" }, { tag: "span" }] },
      bindings: [
        { el: "bosatsu-0", prop: "className", value: "success" },
        { el: "bosatsu-0-0-text", prop: "textContent", valuePath: ["status", "Success", "user", "name"] },
        { el: "bosatsu-0-1-text", prop: "textContent", valuePath: ["status", "Success", "user", "age"], transform: "int_to_string" }
      ]
    }
  }
}
```

## Key Decisions

1. **Start with sum types** - Bosatsu's strongest feature, exhaustive matching means we know all UI states
2. **Explore multiple representations** - Conditional bindings, DAG, and state machine each reveal different insights
3. **Condition hoisting** - Move conditions from DOM structure into binding metadata
4. **Consider worker offloading** - Pure bindings enable parallel evaluation

## Open Questions

1. **Structure differences:** When variants have different DOM structures (different children), do we:
   - Pre-render all and hide/show?
   - Swap DOM templates?
   - Use a minimal reconciler for structural changes?

2. **Nested conditions:** How do we handle match inside match?
   - Flatten to single state space?
   - Hierarchical conditions?

3. **List + conditions:** What happens with `map` that returns conditional VNodes?
   - Per-item condition state?
   - Keyed diffing with variant tracking?

4. **Performance tradeoffs:**
   - Pre-render all: larger initial DOM, fastest updates
   - Template swap: smaller DOM, slightly slower updates
   - Worker: parallel eval, but postMessage overhead

## Next Steps

1. **Experiment 1:** Modify UIAnalyzer to extract variant information from Match expressions
2. **Experiment 2:** Generate conditional bindings with `when` clauses
3. **Experiment 3:** Build a demo with sum type conditional rendering
4. **Experiment 4:** Compare the three binding representations on real examples
5. **Experiment 5:** Prototype worker-based binding evaluation

## References

- Current UIAnalyzer: `core/src/main/scala/dev/bosatsu/ui/UIAnalyzer.scala`
- TypedExpr Match node: `TypedExpr.Match(arg, branches, tag)`
- BranchCondition (unused): Already defined but not populated
- Matchless IR: Could provide more detailed control flow for future analysis
