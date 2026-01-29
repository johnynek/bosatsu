---
title: "Canvas Visualization Container and Parser Fixes for Bosatsu Simulation CLI"
category: runtime-errors
tags:
  - bosatsu
  - canvas-visualization
  - simulation-cli
  - javascript
  - dom-selectors
  - parser-errors
  - embed-generator
  - playwright-testing
module: simulation-cli/canvas-visualization
symptom: |
  Canvas visualization failed to render in generated HTML demos. Multiple issues:
  1. JavaScript error: `document.querySelector('main')` returned null
  2. Input controls not appearing in the UI
  3. Bosatsu compilation failed with parse error on config files
root_cause: |
  1. Container selector mismatch: JavaScript assumed `<main>` element, but HTML template uses `<div class="applet-container">`
  2. Input container ID mismatch: Code used `"inputs"` but HTML has `id="controls"`
  3. Parser limitation: Bosatsu doesn't allow trailing comments inside struct constructors
date_solved: 2026-01-28
---

# Canvas Visualization Container and Parser Fixes

## Problem Description

When implementing Phase 10 (Canvas Visualization) for the Bosatsu simulation CLI, the generated HTML demos failed to render properly. Tests revealed multiple issues with DOM selectors and Bosatsu syntax.

### Symptoms

1. **Playwright tests failing**: Elements not found, controls not appearing
2. **Silent JavaScript failures**: `document.querySelector('main')` returning null
3. **Bosatsu parse errors**: Config files with trailing comments wouldn't compile

## Investigation Steps

### Step 1: Identifying the Container Selector Issue

The generated JavaScript in `SimulationGen.scala` was using:

```javascript
const main = document.querySelector('main');
```

But the HTML template in `EmbedGenerator.scala` generates:

```html
<div class="applet-container">
  <h1 class="applet-title">...</h1>
  <div class="controls-section" id="controls"></div>
</div>
```

There is no `<main>` element - the container is `.applet-container`.

### Step 2: Identifying the Input Container ID Issue

The `addConfiguredInput` function was called with:

```javascript
addConfiguredInput("principal", "Loan Principal ($)", ..., "inputs");
```

But the HTML has `id="controls"`, not `id="inputs"`.

### Step 3: Identifying the Parser Issue

Config files like this would fail to parse:

```bosatsu
config = SimConfig(
  "Loan Calculator",
  "Calculate payments",  # This comment causes parse error!
  [...]
)
```

## Root Cause Analysis

| Issue | Root Cause | Location |
|-------|-----------|----------|
| Container selector | JavaScript assumed `<main>` but template uses `.applet-container` | `SimulationGen.scala` |
| Input container ID | Code used `"inputs"` but template uses `"controls"` | `SimulationGen.scala` |
| Parser error | Bosatsu doesn't allow comments after struct constructor fields | `loan_viz.sim.bosatsu` |

## Working Solution

### Fix 1: Container Selector (SimulationGen.scala)

**Before:**
```javascript
const main = document.querySelector('main');
if (main) {
  // ...
}
```

**After:**
```javascript
const controlsContainer = document.getElementById('controls');
const appletContainer = document.querySelector('.applet-container');

const resultsDiv = document.createElement('div');
resultsDiv.id = 'results';
resultsDiv.className = 'results-section';
if (controlsContainer && appletContainer) {
  controlsContainer.after(resultsDiv);
}
```

### Fix 2: Input Container ID (SimulationGen.scala)

**Before:**
```scala
s"""  addConfiguredInput("$name", "$label", ..., "inputs");"""
```

**After:**
```scala
s"""  addConfiguredInput("$name", "$label", ..., "controls");"""
```

### Fix 3: Remove Trailing Comments from Struct Constructors

**Before (causes parse error):**
```bosatsu
config = SimConfig(
  "Loan Calculator",
  "Description",  # This breaks!
  "Package",
  "function"
)
```

**After (works):**
```bosatsu
# Comments go above the struct, not inside
config = SimConfig(
  "Loan Calculator",
  "Description",
  "Package",
  "function"
)
```

## Prevention Strategies

### 1. Match Template and Code Selectors

When generating JavaScript that interacts with HTML templates:
- Document the HTML structure in comments
- Use constants for element IDs/selectors
- Add assertions or early error detection

```scala
// EmbedGenerator creates: <div class="controls-section" id="controls">
val CONTROLS_CONTAINER_ID = "controls"
val APPLET_CONTAINER_CLASS = "applet-container"
```

### 2. Test DOM Interactions Early

Add Playwright tests that verify:
- Elements exist before interacting
- Selectors match the generated HTML

```typescript
test('canvas API functions are available', async ({ page }) => {
  const hasAPI = await page.evaluate(() => {
    return typeof (window as any)._clear === 'function' &&
           typeof (window as any)._rect === 'function';
  });
  expect(hasAPI).toBe(true);
});
```

### 3. Bosatsu Syntax Guidelines

**Do:**
```bosatsu
# Comments before definitions
config = SimConfig("name", "desc", [])
```

**Don't:**
```bosatsu
config = SimConfig(
  "name",  # Don't put comments here
  "desc"
)
```

## Files Modified

| File | Change |
|------|--------|
| `simulation-cli/.../SimulationGen.scala` | Fixed container selectors, added `_redrawVisualization()` |
| `simulation-cli/.../SimulationCommand.scala` | Added `--canvas` CLI flag |
| `core/.../EmbedGenerator.scala` | Integrated canvas CSS and API |
| `core/.../CanvasVisualization.scala` | NEW - Canvas drawing API |
| `demo/loan_viz.sim.bosatsu` | NEW - Demo config without trailing comments |
| `tests/e2e/demos.spec.ts` | Added 4 canvas visualization tests |

## Test Verification

All 38 Playwright tests pass after fixes:

```bash
npm test
# ✓ 38 tests passed
```

## Cross-References

- [Store AST References Not Copies](../design-patterns/store-ast-references-not-copies.md) - Related pattern for immutable structures
- [Phase 6 Simulation Applets PR Guide](../../pr-guides/phase-6-simulation-applets-pr-guide.md) - CLI architecture
- [Phase 5 Reactive PR Guide](../../pr-guides/phase-5-reactive-pr-guide.md) - DOM binding patterns

## Key Learnings

1. **Always verify HTML template structure** before writing JavaScript that queries the DOM
2. **Use Playwright tests** to catch selector mismatches early
3. **Bosatsu parser limitations**: Comments cannot appear after struct constructor fields
4. **Document selector conventions** in code comments to prevent drift
