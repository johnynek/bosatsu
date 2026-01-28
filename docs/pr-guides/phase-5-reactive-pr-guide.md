# Phase 5: Simple Reactive Simulation - PR Guide

## Overview

Phase 5 introduces reactive state management for BosatsuUI, enabling interactive simulations where UI elements automatically update when state changes.

## Key Components

### 1. ReactiveState.scala - State Management

Location: `core/src/main/scala/dev/bosatsu/ui/ReactiveState.scala`

Provides a reactive state system:

- **StateValue[A]** - Trait for observable state values
- **MutableState[A]** - Writable state with change notification
- **ComputedState[A]** - Derived state that auto-updates when dependencies change
- **Subscription** - Cancellable observer pattern
- **StateStore** - Named state management container

### 2. StateBinding.scala - DOM Bindings

Location: `core/src/main/scala/dev/bosatsu/ui/StateBinding.scala`

Generates JavaScript code connecting state to DOM:

- **TextBinding** - Updates element text content when state changes
- **AttributeBinding** - Updates element attributes when state changes
- **InputBinding** - Reads user input into state
- **generateStateStore()** - Creates JS state management code
- **generateModule()** - Creates complete reactive JS module

### 3. Test Files

- **ReactiveStateGen.scala** - ScalaCheck generators for state and bindings
- **ReactiveStateTest.scala** - 15 property-based tests

### 4. Demo

- **tax-calculator.html** - Interactive tax calculator demonstrating reactive state

## PR Breakdown

### PR 1: ReactiveState (Core State Management)

**Files:**
- `core/src/main/scala/dev/bosatsu/ui/ReactiveState.scala`

**Changes:**
- StateValue trait with get/set/update/subscribe
- MutableState implementation with change notification
- ComputedState for derived values
- StateStore for named state management
- Subscription cancellation pattern

**Tests:** Property tests for state operations

**Reviewable in:** ~15 min

**Dependencies:** None

### PR 2: StateBinding (JS Code Generation)

**Files:**
- `core/src/main/scala/dev/bosatsu/ui/StateBinding.scala`

**Changes:**
- TextBinding, AttributeBinding, InputBinding case classes
- generateStateStore() for JS state management
- generateTextBinding(), generateInputBinding() for DOM updates
- generateModule() for complete reactive modules

**Tests:** Property tests for code generation

**Reviewable in:** ~15 min

**Dependencies:** PR 1, Phase 3 (Code.scala)

### PR 3: Tests & Demo

**Files:**
- `core/src/test/scala/dev/bosatsu/ui/ReactiveStateGen.scala`
- `core/src/test/scala/dev/bosatsu/ui/ReactiveStateTest.scala`
- `demo/tax-calculator.html`

**Changes:**
- ScalaCheck generators for state values and bindings
- 15 property-based tests
- Tax calculator demo showcasing reactive state

**Tests:** Self-contained test suite

**Reviewable in:** ~10 min

**Dependencies:** PRs 1 & 2

## Test Coverage

| Test Category | Count | Description |
|---------------|-------|-------------|
| MutableState | 6 | get, set, update, subscribe, cancel |
| ComputedState | 3 | initial value, dependency updates, immutability |
| StateStore | 2 | register/get, computed values |
| StateBinding | 4 | store code, text binding, input binding, module |

## Design Decisions

### 1. Pull-based vs Push-based Updates

Using a push-based model where state changes immediately notify subscribers. This is simpler and sufficient for UI use cases.

### 2. No Batching (Yet)

State changes trigger immediate updates. Batching can be added in Phase 6 if needed for performance.

### 3. Direct JS Generation

StateBinding generates native JavaScript rather than relying on a Scala.js runtime. This produces smaller bundle sizes and faster execution.

### 4. Computed State as Separate Type

ComputedState is a distinct type that throws on set/update, making the API explicit about read-only derived values.

## Usage Examples

### Creating Reactive State

```scala
import dev.bosatsu.ui.ReactiveState

// Create mutable state
val count = ReactiveState.state(0)

// Subscribe to changes
val sub = count.subscribe { value =>
  println(s"Count is now: $value")
}

// Update state (triggers subscriber)
count.set(5)
count.update(_ + 1)

// Cancel subscription
sub.cancel()
```

### Creating Computed State

```scala
val a = ReactiveState.state(10)
val b = ReactiveState.state(20)

val sum = ReactiveState.computed(
  () => a.get + b.get,
  a, b  // dependencies
)

a.set(15)  // sum automatically updates to 35
```

### Generating JS Bindings

```scala
import dev.bosatsu.ui.StateBinding

val module = StateBinding.generateModule(
  "Counter",
  Map("count" -> "0"),
  List(StateBinding.TextBinding("count", "count-display")),
  List(StateBinding.InputBinding("count-input", "count", "input", Some("parseInt")))
)

println(module.render(80))
```

## Generated JavaScript Pattern

The generated JS follows this pattern:

```javascript
const _state = { count: 0 };
const _listeners = { count: [] };

function _setState(name, value) {
  if (_state[name] !== value) {
    _state[name] = value;
    _listeners[name].forEach(fn => fn(value));
  }
}

function _subscribe(name, fn) {
  _listeners[name].push(fn);
  fn(_state[name]);  // Initial notification
}

// Bindings
_subscribe('count', (v) => {
  document.getElementById('count-display').textContent = v;
});

document.getElementById('count-input').addEventListener('input', (e) => {
  _setState('count', parseInt(e.target.value));
});
```

## Future Work (Phase 6+)

- State batching for performance
- Virtual DOM diffing for efficient updates
- "Why?" buttons with derivation chain display
- "What if?" assumption toggling

## Migration Notes

This is a new module with no breaking changes to existing code.

## Verification

```bash
# Run reactive state tests
sbt "coreJVM/testOnly dev.bosatsu.ui.ReactiveStateTest"

# Run all UI tests
sbt "coreJVM/testOnly dev.bosatsu.ui.*"

# Check compilation
sbt "coreJVM/compile"
```

## References

- Phase 4 VNode (dependency): `core/src/main/scala/dev/bosatsu/ui/VNode.scala`
- Phase 3 Code.scala (dependency): `core/src/main/scala/dev/bosatsu/codegen/js/Code.scala`
- Plan document: `docs/plans/bosatsu-provenance-tooling-plan.md` (Phase 5 section)
