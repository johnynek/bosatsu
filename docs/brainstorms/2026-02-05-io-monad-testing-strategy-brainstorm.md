# IO Monad Testing Strategy

**Date:** 2026-02-05
**Status:** Decided
**Context:** Sliders in bouncing-ball demo don't work. Event handlers generate IO thunks that are never executed. No tests exist for IO monad behavior.

## What We're Building

A testing strategy that covers **families of IO behaviors** across the full stack (unit + integration + E2E), ensuring that Bosatsu's IO monad - the ONLY non-pure part of the language - works correctly from type checking through code generation to browser execution.

## Why This Approach

The IO monad creates a universal contract: **describe an effect, then execute it**. Every IO operation follows this pattern. Testing at the behavior family level catches integration bugs (like the current slider issue) that component-level tests miss. We also add thin component-level unit tests for fast feedback.

## Architecture: Two Testing Axes

### Axis 1: Behavior Families (Integration)

Each family tests the full describe-then-execute contract:

| Family | Bosatsu Pattern | What Must Work |
|--------|----------------|----------------|
| **Single write** | `write(state, val)` | Generates executable JS, state updates, DOM reflects change |
| **flatMap sequencing** | `flatMap(write(a,x), _ -> write(b,y))` | Both writes execute in order |
| **IO in event handlers** | `on_click(u -> write(s, v))` | Click fires handler, IO executes, DOM updates |
| **IO in frame callbacks** | `on_frame(dt -> write(s, f(dt)))` | Frame loop runs, IO executes each frame |
| **IO with state reads** | `on_input(v -> write(s, g(read(t), v)))` | Read current state, compute, write new state |

### Axis 2: Component Layers (Unit)

Thin unit tests for fast feedback on each layer:

| Layer | Tests | Style |
|-------|-------|-------|
| **UIAnalyzer** | IO pattern extraction (write, flatMap, pure detection) | Synthetic TypedExpr nodes (existing make* helpers) |
| **JsGen** | IO intrinsic compilation (correct JS output) | Matchless IR nodes → JS string assertions |
| **Runtime** | IO thunk execution, state update propagation | Browser-level via Playwright |

## Key Decisions

1. **Both axes** - Behavior family integration tests AND component unit tests
2. **Synthetic + real** - Hand-built TypedExpr for fast unit tests, real .bosatsu files for integration
3. **All 5 IO families** - Comprehensive coverage, not just the broken path
4. **Fixtures location** - `simulation-cli/src/test/resources/` for .bosatsu files (matches existing pattern)
5. **E2E via existing Playwright infra** - Compile to `web_deploy/`, serve via test server

## The 5 Test Fixture Files (.bosatsu)

Small, focused .bosatsu files that each exercise one IO behavior family:

1. **`io_single_write.bosatsu`** - Button click writes to state, text displays new value
2. **`io_flatmap_sequence.bosatsu`** - Button click writes TWO states via flatMap, both displays update
3. **`io_event_handler.bosatsu`** - Input slider writes to state, display updates with new value
4. **`io_frame_callback.bosatsu`** - on_frame increments counter each frame, display updates
5. **`io_read_then_write.bosatsu`** - Input handler reads state A, computes, writes state B

Each file is minimal (~30-50 lines) and tests exactly one behavior.

## Test Execution Flow

```
.bosatsu fixture
    |
    v
[Scala unit tests] ---- UIAnalyzer: correct bindings extracted?
    |                     JsGen: correct JS generated?
    v
[Integration test] ---- Compile via simulationCli, verify JS output patterns
    |
    v
[E2E Playwright] ------ Open HTML, interact, verify DOM updates
```

## Open Questions

- Should the integration tests compile .bosatsu and inspect the JS output (string matching), or should they just verify the E2E behavior works?
- Do we need a test helper that compiles .bosatsu → JS in-memory for the Scala integration tests?
- Should frame callback tests have a timeout-based assertion (wait N ms, check counter > 0)?

## Current Bug

The slider handler generates:
```javascript
const on_gravity_change = value => (() => { ... })
```

The `(() => { ... })` is an IO thunk that never gets called. The event runtime needs to detect IO return values and execute them. This is Family 3 (IO in event handlers) and will be the first test written.
