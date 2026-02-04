# BosatsuUI: Complete the Production-Ready Framework

---
title: "feat: BosatsuUI Production-Ready"
type: feat
date: 2026-02-03
---

## Overview

Complete BosatsuUI as a production-ready frontend framework based on its established paradigm:

1. **Compile-time binding extraction** from typed AST (not runtime discovery)
2. **O(1) direct DOM updates** (no Virtual DOM diffing)
3. **State tree decoupled from DOM tree** (bindings map state paths → DOM selectors)
4. **Conditional updates via discriminant tracking** (not reconciliation)

This is not "another React." It's the BurritoUI paradigm implemented properly in a typed language.

## The Paradigm (What We're Building)

```
COMPILE TIME:
  .bosatsu source
       ↓
  TypedExpr AST (typed, all bindings explicit)
       ↓
  UIAnalyzer.analyze() extracts:
     - State reads: which state paths are read where
     - DOM bindings: state path → (selector, property, transform, condition)
     - Event handlers: selector → handler expression
       ↓
  JsGen compiles to JavaScript with binding map
       ↓
  Self-contained HTML with runtime

RUNTIME:
  setState(path, value)
       ↓
  bindingMap.get(path) → List[{selector, property, transform, when}]
       ↓
  For each binding:
     - Check condition (if conditional)
     - Apply transform
     - Update DOM property directly
       ↓
  O(1) update complete (no tree walk, no diff)
```

## Current State (What Works)

| Feature | Status | Location |
|---------|--------|----------|
| VNode types (Element, Text, Fragment) | ✅ Done | `ui/VNode.scala` |
| State primitives (state, read, write) | ✅ Done | `ui.bosatsu`, `JsGen.scala` |
| UIAnalyzer binding extraction | ✅ Done | `ui/UIAnalyzer.scala` |
| Text content bindings | ✅ Done | `UICommand.scala` |
| Attribute bindings (class, value) | ✅ Done | `UICommand.scala` |
| Event handlers (click, input, change) | ✅ Done | `UICommand.scala` |
| Conditional bindings (match/if) | ✅ Done | `UIAnalyzer.scala` |
| HTML generation pipeline | ✅ Done | `UICommand.scala` |
| Counter demo | ✅ Done | `demos/ui/counter.bosatsu` |
| Todo list demo (static) | ✅ Done | `demos/ui/todo-list.bosatsu` |
| Conditional demo | ✅ Done | `demos/ui/conditional.bosatsu` |

## What's Missing (To Complete the Promise)

### Phase 1: Dynamic Lists with Runtime Binding Registration

**The Problem:** Current demos have static item counts. A real todo app needs add/remove.

**The Solution:** Runtime binding registration that follows the same pattern:

```javascript
// Compile-time: generate the binding pattern
const _listBindingTemplate = {
  "todos.$.completed": {
    selectorPattern: "todo-$-checkbox",
    property: "className",
    transform: (v) => v ? "checked" : ""
  }
};

// Runtime: when item added
function _registerListItem(listPath, index) {
  const key = `${listPath}.${index}`;
  const selector = _listBindingTemplate[`${listPath}.$.completed`]
    .selectorPattern.replace('$', index);
  _bindings[key] = { selector, ... };
}

// Runtime: when item removed
function _unregisterListItem(listPath, index) {
  delete _bindings[`${listPath}.${index}`];
  // Remove DOM element
}
```

**Tasks:**
- [x] Define list state type in Bosatsu/UI: `ListState[A]`
- [x] Add list operations: `append`, `remove_at`, `update_at`
- [x] Extend UIAnalyzer to extract list binding templates (pattern with `$` placeholder)
- [x] Generate runtime binding registration code in UICommand
- [x] Create todo-dynamic.bosatsu demo with add/remove
- [x] Detect list_length as state read for computed value updates

**Files:**
- `core/src/main/resources/bosatsu/ui.bosatsu` - Add ListState type
- `core/src/main/scala/dev/bosatsu/ui/UIAnalyzer.scala` - List template extraction
- `simulation-cli/src/main/scala/dev/bosatsu/simulation/UICommand.scala` - Runtime generation
- `demos/ui/todo-dynamic.bosatsu` - New demo

### Phase 2: Two-Way Input Binding

**The Problem:** on_input handlers exist but string conversion is awkward.

**The Solution:** Proper input value binding with automatic string handling:

```bosatsu
# Current (awkward)
new_todo_text: State[String] = state("")

h("input", [
  ("id", "new-todo-input"),
  ("value", read(new_todo_text)),  # Bosatsu string → JS string
  on_input(\(value: String) -> write(new_todo_text, value))  # JS string → Bosatsu string
], [])
```

**Tasks:**
- [x] Verify string conversion works in JsGen (Bosatsu List[Char] ↔ JS String)
- [ ] Add `bind_value` helper that combines value attribute + on_input handler (deferred - current pattern works well)
- [x] Create form-input.bosatsu demo with text input, textarea
- [x] Ensure UIAnalyzer extracts input bindings correctly
- [x] Fix empty string handling (don't treat [0] as boolean False)

**Files:**
- `core/src/main/resources/bosatsu/ui.bosatsu` - Add bind_value helper
- `core/src/main/scala/dev/bosatsu/codegen/js/JsGen.scala` - String conversion
- `demos/ui/form-input.bosatsu` - New demo

### Phase 3: Style Bindings for Animations

**The Problem:** Benchmarks need style.transform bindings for particle animations.

**The Solution:** Extend binding extraction to handle style properties:

```bosatsu
# In Bosatsu
h("div", [
  ("id", "particle-0"),
  ("style", concat(["transform: translate(",
    double_to_string(read(x)), "px, ",
    double_to_string(read(y)), "px)"]))
], [])
```

**Tasks:**
- [x] Add style binding detection to UIAnalyzer
- [x] Extract style property name from attribute string
- [x] Generate style-specific update code (via `computeValue` functions)
- [x] Create particles.bosatsu demo using Math functions (sin, cos)
- [x] Fix TypedExprNormalization to preserve write() side effects (mayHaveSideEffects helper)
- [ ] Update drag-animation benchmark to use real compiled Bosatsu

**Infrastructure Fix Completed:**
TypedExprNormalization was eliminating `_ = write(state, value)` patterns, breaking UI state updates.
Fixed by adding `mayHaveSideEffects()` helper that detects external function calls (stored in TypeEnv.values).
Now Match elimination, Match lifting, and Let elimination all preserve side-effecting calls.

**Files:**
- `core/src/main/scala/dev/bosatsu/ui/UIAnalyzer.scala` - Style binding extraction
- `core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala` - Side-effect preservation (DONE)
- `simulation-cli/src/main/scala/dev/bosatsu/simulation/UICommand.scala` - Style updates
- `demos/ui/particles.bosatsu` - Animation demo (DONE)
- `demos/benchmarks/drag-animation/` - Real benchmark

### Phase 4: More Event Types

**The Problem:** Only click, input, change events are supported.

**The Solution:** Add keyboard and drag events following the same pattern:

```bosatsu
# Add to Bosatsu/UI
external def on_keydown(handler: String -> Unit) -> (String, String)
external def on_keyup(handler: String -> Unit) -> (String, String)
external def on_dragstart(handler: Unit -> Unit) -> (String, String)
external def on_dragover(handler: Unit -> Unit) -> (String, String)
external def on_drop(handler: Unit -> Unit) -> (String, String)
```

**Tasks:**
- [x] Add event types to ui.bosatsu (on_keydown, on_keyup, on_dragstart, on_dragover, on_drop)
- [x] Implement in JsGen UIExternal
- [x] Generate event handler registration in UICommand
- [x] Create keyboard-input.bosatsu demo
- [x] Create drag-drop.bosatsu demo

**Files:**
- `core/src/main/resources/bosatsu/ui.bosatsu` - Event types
- `core/src/main/scala/dev/bosatsu/codegen/js/JsGen.scala` - UIExternal
- `simulation-cli/src/main/scala/dev/bosatsu/simulation/UICommand.scala` - Handler registration
- `demos/ui/keyboard-input.bosatsu` - New demo
- `demos/ui/drag-drop.bosatsu` - New demo

### Phase 5: Nested State Paths

**The Problem:** State is flat. Real apps have structured state.

**The Solution:** Extend state path tracking through record field access:

```bosatsu
struct User(name: String, age: Int)
user: State[User] = state(User("Alice", 30))

# This should extract binding path ["user", "name"]
text(read(user).name)
```

**Tasks:**
- [ ] Extend UIAnalyzer to trace through struct field access
- [ ] Handle nested paths in binding map generation
- [ ] Generate nested state update code
- [ ] Create user-profile.bosatsu demo with nested state

**Files:**
- `core/src/main/scala/dev/bosatsu/ui/UIAnalyzer.scala` - Nested path extraction
- `simulation-cli/src/main/scala/dev/bosatsu/simulation/UICommand.scala` - Nested updates
- `demos/ui/user-profile.bosatsu` - New demo

### Phase 6: Error Boundaries (Simple)

**The Problem:** Runtime errors crash everything.

**The Solution:** Wrap generated code in try/catch with fallback UI:

```javascript
// Generated runtime
function _safeUpdate(binding, value) {
  try {
    _applyBinding(binding, value);
  } catch (e) {
    console.error("BosatsuUI binding error:", binding, e);
    // Optionally show error indicator on element
  }
}

function _safeHandler(handler) {
  return function(event) {
    try {
      handler(event);
    } catch (e) {
      console.error("BosatsuUI handler error:", e);
    }
  };
}
```

**Tasks:**
- [ ] Add try/catch wrapper to binding updates in generated runtime
- [ ] Add try/catch wrapper to event handlers
- [ ] Log errors with context (binding info, state path)
- [ ] Optional: show visual error indicator on failed elements

**Files:**
- `simulation-cli/src/main/scala/dev/bosatsu/simulation/UICommand.scala` - Error handling

### Phase 7: All Demos Are Real

**The Problem:** CLAUDE.md requires all demos be generated from .bosatsu source.

**The Solution:** Port remaining hand-written demos or remove them:

| Demo | Action | Complexity |
|------|--------|------------|
| `dashboard.html` | Port after nested state (Phase 5) | Medium |
| `spreadsheet.html` | Remove (needs 2D data structures) | High |
| `shopping-cart.html` | Port after dynamic lists (Phase 1) | Medium |
| `kanban-board.html` | Port after drag events (Phase 4) | Medium |
| `nested-comments.html` | Remove (needs recursive templates) | High |
| `drag-drop.html` | Port after drag events (Phase 4) | Low |

**Tasks:**
- [ ] Create `tests/e2e/demos-are-real.spec.ts` meta test
- [ ] Update `scripts/regenerate_demos.sh` to include UI demos
- [ ] Port feasible demos after prerequisite phases
- [ ] Remove demos that need features outside scope
- [ ] Ensure benchmarks use real compiled Bosatsu

**Files:**
- `tests/e2e/demos-are-real.spec.ts` - Meta test
- `scripts/regenerate_demos.sh` - Include UI demos
- Various demo files

## Implementation Order

```
Phase 1: Dynamic Lists ──────────────────────────────┐
                                                     │
Phase 2: Two-Way Input ──────────────────────────────┼── Enables real apps
                                                     │
Phase 3: Style Bindings ─────────────────────────────┘
         (unlocks benchmarks)

Phase 4: More Event Types ───────────────────────────┐
                                                     │
Phase 5: Nested State Paths ─────────────────────────┼── Enables complex apps
                                                     │
Phase 6: Error Boundaries ───────────────────────────┘

Phase 7: All Demos Real ─────────────────────────────── Verification
```

## Success Criteria

1. **Dynamic todo app works** - Users can add/remove items, state updates correctly
2. **Form input works** - Text input with two-way binding
3. **Animation benchmark uses real Bosatsu** - particles.bosatsu compiles to working animation
4. **All demos generated from source** - `demos-are-real.spec.ts` passes
5. **No crashes on errors** - Errors logged, app continues

## Non-Goals (Out of Scope)

These would distract from completing the core paradigm:

- SSR/hydration (nice-to-have, not core)
- TypeScript types (documentation concern)
- HMR/DevTools (developer experience, not functionality)
- Component system (current h() composition is sufficient)
- CSS-in-JS (use regular CSS)
- Routing (separate concern)
- Async/fetch (use Bosatsu/IO separately)

## References

- Brainstorm: `docs/brainstorms/2026-02-03-bosatsuui-real-demos-brainstorm.md`
- Pipeline brainstorm: `docs/brainstorms/2026-01-29-bosatsu-ui-generation-pipeline-brainstorm.md`
- CLAUDE.md: No fake demos rule
- UIAnalyzer: `core/src/main/scala/dev/bosatsu/ui/UIAnalyzer.scala`
- UICommand: `simulation-cli/src/main/scala/dev/bosatsu/simulation/UICommand.scala`
- JsGen UIExternal: `core/src/main/scala/dev/bosatsu/codegen/js/JsGen.scala`
