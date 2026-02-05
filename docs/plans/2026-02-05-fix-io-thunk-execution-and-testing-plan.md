---
title: Fix IO Thunk Execution and IO Monad Testing Strategy
type: fix
date: 2026-02-05
---

# Fix IO Thunk Execution and IO Monad Testing Strategy

## Overview

Event handlers in generated BosatsuUI code don't work. Moving sliders, clicking buttons - nothing happens. The root cause is a chain of pipeline integration failures between JsGen (code generation) and UIGen (HTML/runtime generation). This plan fixes those bugs and adds comprehensive IO monad tests to prevent regressions.

## Problem Statement

The bouncing-ball demo compiles and renders, but all interactivity is broken:
- Sliders don't update physics parameters
- Reset button doesn't reset the ball
- The generated JS has multiple name/signature mismatches between layers

### Root Causes (5 Bugs)

| # | Bug | Location | Impact |
|---|-----|----------|--------|
| 1 | Handler bodies are placeholder comments, not compiled code | UIGen.scala:223-226 | Handlers do nothing |
| 2 | IO thunks returned from handlers are never executed | UIGen.scala:241 | Even if compiled, effects wouldn't run |
| 3 | `_ui_write` called by JsGen but never defined in runtime | JsGen.scala:647 vs UIGen.scala:267 | State writes crash |
| 4 | `_ui_register_handler` called with 2 args, `_registerHandler` defined with 3 | JsGen.scala:656 vs UIGen.scala:230 | Handler registration crashes |
| 5 | No element ID passed through JsGen handler registration | JsGen.scala:656+ | Can't attach handlers to DOM elements |

## Proposed Solution

### Phase 1: Fix the Runtime (Bugs 3-5)

Align the runtime function names and signatures so JsGen-generated code can call them.

**UIGen.scala changes:**

```javascript
// Define _ui_write to match what JsGen generates
function _ui_write(stateObj, value) {
  stateObj.value = value;
  _updateBindings(stateObj.id);
  return { value: undefined, trace: [] }; // IO[Unit] result
}

// Define _ui_register_handler to match JsGen's 2-arg call
// Returns a handler ID string for the prop tuple
function _ui_register_handler(eventType, handler) {
  const handlerId = '_handler_' + (++_handlerCounter);
  _eventHandlers[handlerId] = { type: eventType, fn: handler };
  return handlerId;
}
```

**Key design choice:** `_ui_write` returns an IO result object `{ value, trace }` so it works inside `flatMap` chains. The state object is passed directly (not an ID string) because JsGen passes the state object reference.

### Phase 2: Fix IO Thunk Execution (Bugs 1-2)

Update the event handler initialization to execute IO thunks returned by handlers.

**UIGen.scala - _initEventHandlers:**

```javascript
function _initEventHandlers() {
  document.querySelectorAll('[data-onclick], [data-oninput], ...').forEach(el => {
    // For each data-on* attribute, find the handler and wire it up
    for (const attr of el.attributes) {
      if (attr.name.startsWith('data-on')) {
        const eventType = attr.name.replace('data-on', '');
        const handlerId = attr.value;
        const handlerInfo = _eventHandlers[handlerId];
        if (handlerInfo) {
          el.addEventListener(handlerInfo.type, (e) => {
            // Get input value for input events
            const inputValue = e.target ? _js_to_bosatsu_string(e.target.value || '') : undefined;
            // Call the Bosatsu handler function
            const ioResult = eventType === 'input' || eventType === 'change'
              ? handlerInfo.fn(inputValue)
              : handlerInfo.fn(undefined); // Unit for click handlers
            // Execute the IO thunk if it's a function
            if (typeof ioResult === 'function') {
              ioResult(); // Execute the IO action
            }
          });
        }
      }
    }
  });
}
```

### Phase 3: Fix Frame Callback IO Execution

Ensure `_ui_register_frame_callback` also executes IO thunks:

```javascript
function _ui_register_frame_callback(updateFn) {
  let lastTime = null;
  function loop(timestamp) {
    if (lastTime === null) lastTime = timestamp;
    const dt = Math.min((timestamp - lastTime) / 1000, 0.033);
    lastTime = timestamp;
    // Call update function with delta time
    const ioResult = updateFn(dt);
    // Execute IO thunk
    if (typeof ioResult === 'function') ioResult();
    // Re-render canvas bindings
    _renderAllCanvases();
    requestAnimationFrame(loop);
  }
  requestAnimationFrame(loop);
}
```

### Phase 4: Write IO Behavior Family Tests

Five .bosatsu fixture files + corresponding Playwright specs:

#### 4.1 Test Fixtures (`simulation-cli/src/test/resources/`)

**`io_single_write.bosatsu`** (~25 lines)
- One state, one button, one text display
- Click button → `write(counter, 1)` → display shows "1"

**`io_flatmap_sequence.bosatsu`** (~35 lines)
- Two states (a, b), one button, two displays
- Click → `flatMap(write(a, "X"), _ -> write(b, "Y"))` → both displays update

**`io_event_handler.bosatsu`** (~30 lines)
- Input + display
- Type in input → `on_input(v -> write(display_state, v))` → display shows typed value

**`io_frame_callback.bosatsu`** (~30 lines)
- Counter state, display, on_frame
- `on_frame(dt -> write(counter, add(read(counter), 1)))` → counter increments

**`io_read_then_write.bosatsu`** (~35 lines)
- Two states (source, target), input, two displays
- Input writes to source, handler reads source + computes → writes target

#### 4.2 Playwright E2E Tests (`tests/e2e/io-behaviors.spec.ts`)

```typescript
test.describe('IO Behavior Family: Single Write', () => {
  test('click updates state and DOM', async ({ page }) => {
    await page.goto('/demos/test/io-single-write.html');
    await expect(page.locator('#display')).toHaveText('0');
    await page.click('#btn');
    await expect(page.locator('#display')).toHaveText('1');
  });
});

test.describe('IO Behavior Family: flatMap Sequencing', () => {
  test('both writes execute in order', async ({ page }) => {
    await page.goto('/demos/test/io-flatmap-sequence.html');
    await page.click('#btn');
    await expect(page.locator('#display-a')).toHaveText('X');
    await expect(page.locator('#display-b')).toHaveText('Y');
  });
});

test.describe('IO Behavior Family: Event Handler IO', () => {
  test('input handler executes IO and updates display', async ({ page }) => {
    await page.goto('/demos/test/io-event-handler.html');
    await page.fill('input[type="range"]', '75');
    await expect(page.locator('#display')).toHaveText('75');
  });
});

test.describe('IO Behavior Family: Frame Callback', () => {
  test('frame callback increments counter', async ({ page }) => {
    await page.goto('/demos/test/io-frame-callback.html');
    await page.waitForTimeout(500); // Wait for several frames
    const text = await page.locator('#counter').textContent();
    expect(parseInt(text || '0')).toBeGreaterThan(0);
  });
});

test.describe('IO Behavior Family: Read Then Write', () => {
  test('handler reads state A and writes computed result to state B', async ({ page }) => {
    await page.goto('/demos/test/io-read-then-write.html');
    await page.fill('#input', 'hello');
    await expect(page.locator('#display')).toHaveText('HELLO'); // computed
  });
});
```

### Phase 5: Write Component Unit Tests

#### 5.1 UIAnalyzer IO Tests (`UIAnalyzerTest.scala` additions)

```scala
test("extractStateWrites detects write in IO context") { ... }
test("extractStateWrites follows flatMap chain") { ... }
test("extractStateWrites skips pure (no writes)") { ... }
test("hasIOType detects IO return type") { ... }
test("on_frame detected in UI package") { ... }
```

#### 5.2 JsGen IO Tests (`JsGenTest.scala` additions)

```scala
test("UIExternal write generates _ui_write call") { ... }
test("IOExternal flatMap generates chained thunks") { ... }
test("IOExternal pure generates thunk with value") { ... }
test("UIExternal on_frame generates _ui_register_frame_callback") { ... }
```

### Phase 6: Verify Bouncing Ball Demo

After all fixes, regenerate and verify the bouncing-ball demo works end-to-end:
- Sliders update physics parameters
- Ball animates with gravity
- Reset button resets position
- Changing gravity mid-animation changes ball behavior

## Acceptance Criteria

### Functional

- [x] `write(state, value)` in event handlers actually updates state and DOM
- [x] `flatMap(io1, _ -> io2)` executes both IO actions in sequence
- [x] `on_frame` callback executes IO each frame
- [x] Sliders in bouncing-ball demo update physics in real time
- [x] Reset button resets ball position

### Testing

- [x] 5 .bosatsu fixture files compile without errors
- [x] 5 Playwright E2E tests pass (one per IO behavior family)
- [ ] UIAnalyzer unit tests pass for IO pattern extraction
- [x] JsGen unit tests pass for IO intrinsic compilation

### Non-Regression

- [x] Existing counter, todo, and loan calculator demos still work
- [x] Existing Scala test suite passes (`sbt test`) (2 pre-existing failures in ClangGenTest/PathModuleTest unrelated to IO)
- [x] Meta-coverage test still passes

## Implementation Order

```
Phase 1 (Runtime fixes)     ← Unblocks everything else
    ↓
Phase 2 (IO thunk execution) ← Handlers work
    ↓
Phase 3 (Frame callbacks)    ← Animation works
    ↓
Phase 4 (E2E test fixtures)  ← Lock down behavior
    ↓
Phase 5 (Unit tests)         ← Fast feedback layer
    ↓
Phase 6 (Bouncing ball)      ← Full demo verification
```

Phases 4 and 5 can run in parallel once Phases 1-3 are done.

## References

- Brainstorm: `docs/brainstorms/2026-02-05-io-monad-testing-strategy-brainstorm.md`
- UIGen.scala: `core/src/main/scala/dev/bosatsu/ui/UIGen.scala`
- JsGen.scala: `core/src/main/scala/dev/bosatsu/codegen/js/JsGen.scala`
- UIAnalyzer.scala: `core/src/main/scala/dev/bosatsu/ui/UIAnalyzer.scala`
- Bouncing ball demo: `demos/physics/bouncing-ball.bosatsu`
- Existing UIAnalyzer tests: `core/src/test/scala/dev/bosatsu/ui/UIAnalyzerTest.scala`
- Existing JsGen tests: `core/src/test/scala/dev/bosatsu/codegen/js/JsGenTest.scala`
