---
title: "feat: AI Debugging Demo - Browser-Only ScalaJS"
type: feat
date: 2026-01-28
brainstorm: docs/brainstorms/2026-01-28-ai-debugging-demo-brainstorm.md
---

# AI Debugging Demo - Browser-Only ScalaJS

## Overview

Create a **fully client-side** interactive debugging demo for Bosatsu programs that runs entirely in the browser via ScalaJS. No server required. Users can explore provenance chains ("Why does this value equal X?"), toggle assumptions ("What if?"), and see results recompute in real-time.

## Problem Statement

The current `EmbedGenerator` creates self-contained HTML demos, but:
1. **Derivation dependencies are empty** - `deps: []` arrays not populated
2. **Recomputation is a placeholder** - `_recompute()` has TODO comment
3. **No provenance bridge** - `ProvenanceTrace` (daemon) and `Derivation[A]` (UI) are disconnected
4. **"Why?" explanations non-functional** - no dependency chain data

## Proposed Solution

Fix the existing embed generation pipeline to:
1. **Populate derivation dependencies** during Matchless IR analysis
2. **Implement real recomputation** using the dependency graph
3. **Bridge provenance to UI** by embedding `ProvenanceTrace` data
4. **Generate working "Why?" explanations** from actual derivation chains

### Architecture

```
.bosatsu source
     │
     ▼
┌─────────────────────────────────────────────────────────────────┐
│  Compile-Time (bosatsu-sim CLI or sbt)                          │
│                                                                 │
│  TypedExpr → Matchless.Expr → DerivationAnalyzer                │
│                                    │                            │
│                                    ▼                            │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ ProvenanceTrace                                          │   │
│  │  - nodes: Map[NodeId, TraceNode]                        │   │
│  │  - dependencies per node                                 │   │
│  │  - formulas for each computation                         │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                    │                            │
│                                    ▼                            │
│  JsGen.exprToJs() + EmbedGenerator.generateEmbed()              │
└─────────────────────────────────────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────────────────────────────────────┐
│  Self-Contained HTML File                                       │
│                                                                 │
│  <script>                                                       │
│    const _derivations = {                                       │
│      "total_paid": {                                            │
│        type: "computed",                                        │
│        deps: ["monthly_payment", "num_payments"],  // POPULATED │
│        formula: "monthly_payment * num_payments",               │
│        value: 540000                                            │
│      },                                                         │
│      ...                                                        │
│    };                                                           │
│                                                                 │
│    function _recompute() { /* REAL implementation */ }          │
│    function _explain(name) { /* Uses deps for "Why?" */ }       │
│  </script>                                                      │
│                                                                 │
│  <div class="applet-container">                                 │
│    <!-- Interactive UI with Why? buttons, What if? toggles --> │
│  </div>                                                         │
└─────────────────────────────────────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────────────────────────────────────┐
│  Browser Runtime (no server)                                    │
│                                                                 │
│  User clicks "Why?" → _explain() → Modal with derivation chain  │
│  User toggles value → _setAssumption() → _recompute() → UI      │
└─────────────────────────────────────────────────────────────────┘
```

## Technical Approach

### Phase 1: Fix Derivation Dependency Extraction ✅ COMPLETE

**Goal**: Ensure `DerivationAnalyzer` correctly extracts dependencies from Matchless IR.

**Files**:
- `simulation-cli/src/main/scala/dev/bosatsu/simulation/SimulationCommand.scala`

**Tasks**:
- [x] Verify `extractDependencies()` returns correct dependency sets
- [x] Ensure `exprToFormula()` produces human-readable formulas
- [x] Test with sample `.bosatsu` files to confirm accuracy

**Implementation Notes**:
- Formula extraction now walks the **TypedExpr AST** directly (not Matchless IR)
- Handles compiler-inlined struct constructor arguments
- Converts TypedExpr.App nodes to human-readable formulas (sub→-, times→×, div→÷)

**Verification**:
```scala
// Given: taxable = sub(income, deductions)
// extractDependencies should return: Set("income", "deductions")
// exprToFormula should return: "income - deductions"
```

### Phase 2: Populate Derivations in EmbedGenerator ✅ COMPLETE

**Goal**: Make `EmbedGenerator.generateJS()` emit derivations with actual dependencies.

**Files**:
- `simulation-cli/src/main/scala/dev/bosatsu/simulation/SimulationGen.scala`
- `simulation-cli/src/main/scala/dev/bosatsu/simulation/SimulationCommand.scala`

**Actual Output (verified)**:
```javascript
// Fixed output - deps POPULATED:
_derivations["tax_amount"] = {
  type: "computed",
  name: "tax_amount",
  formula: "((taxable_income × tax_rate) ÷ 100)",  // ← human-readable!
  valueType: "number",
  deps: ["taxable_income", "tax_rate"],  // ← populated!
  value: undefined
};
```

**Tasks**:
- [x] Pass `DerivationAnalyzer` results to `EmbedGenerator`
- [x] Generate `deps` array from `AnalyzedBinding.dependencies`
- [x] Generate `formula` string from `AnalyzedBinding.formula`
- [x] Ensure derivation order respects dependency graph (topological sort)

### Phase 3: Implement Real Recomputation ✅ COMPLETE

**Goal**: Replace placeholder `_recompute()` with working implementation.

**Files**:
- `simulation-cli/src/main/scala/dev/bosatsu/simulation/SimulationGen.scala`

**Solution Approach**:
1. ✅ **Function-based computation**: Call the compiled Bosatsu function directly
2. ✅ **Re-evaluate**: Function returns struct with all output fields
3. ✅ **Update** the `_state` object and `_derivations` values
4. ✅ **Trigger** UI update via display value updates

**Tasks**:
- [x] Generate computation using JsGen-compiled function
- [x] Generate `_recompute()` that calls the function with current inputs
- [x] Update display elements when values change
- [x] Update derivation values for "Why?" explanations

**Generated JS Structure**:
```javascript
// Computation functions (generated from Bosatsu)
const _computeFunctions = {
  taxable: () => _state.income - _state.deductions,
  tax: () => _state.taxable * _state.tax_rate,
  net: () => _state.income - _state.tax
};

// Topological order for recomputation
const _computeOrder = ["taxable", "tax", "net"];

function _recompute() {
  for (const name of _computeOrder) {
    if (_computeFunctions[name]) {
      const newValue = _computeFunctions[name]();
      _state[name] = newValue;
      _derivations[name].value = newValue;
      _updateDisplay(name, newValue);
    }
  }
}
```

### Phase 4: Bridge ProvenanceTrace to WhyExplainer ✅ COMPLETE

**Goal**: Generate "Why?" explanations from actual derivation data.

**Files**:
- `simulation-cli/src/main/scala/dev/bosatsu/simulation/SimulationGen.scala`

**Implementation**: Generated JS functions that build explanation from `_derivations`:

```javascript
// Recursively explain derivation
function _explainDerivation(d, depth) {
  const marginLeft = depth * 20;
  const cls = d.type;
  const header = _formatDerivationHeader(d);
  let result = '<div class="derivation ' + cls + '" style="margin-left: ' + marginLeft + 'px">' + header + '</div>';

  if (d.deps && d.deps.length > 0) {
    d.deps.forEach(depName => {
      const dep = _getDerivation(depName);
      if (dep) {
        result += _explainDerivation(dep, depth + 1);  // Recursive!
      }
    });
  }
  return result;
}
```

**Tasks**:
- [x] Generate `_explainDerivation()` function that recursively builds explanation tree
- [x] Add depth-based indentation for hierarchical display
- [x] Style explanation modal with CSS
- [x] Wire "Why?" button onclick to show modal with explanation
- [x] Show actual formulas extracted from TypedExpr AST

### Phase 5: Create Demo Example

**Goal**: Create a compelling `.bosatsu` demo file that showcases all features.

**File**: `demo/tax_simulation.bosatsu`

```bosatsu
package TaxDemo

from Bosatsu/Predef import Int, sub, times

# Assumptions (user can change these)
tax_rate = 25          # percent
income = 100000        # dollars
deductions = 15000     # dollars

# Computations (derived values)
taxable = sub(income, deductions)
tax_amount = div(times(taxable, tax_rate), 100)
net_income = sub(income, tax_amount)

# Result
result = net_income
```

**Tasks**:
- [x] Create `demo/tax_simulation.bosatsu`
- [x] Create `demo/tax_simulation.sim.bosatsu` (config file)
- [x] Generate HTML: `bosatsu-sim demo/tax_simulation.bosatsu demo/tax_simulation.sim.bosatsu -o demo/tax_demo.html`
- [x] Test in browser:
  - [x] Verify initial values display correctly
  - [x] Click "Why?" on `net_income` - shows full derivation chain with formulas
  - [x] Toggle `tax_rate` - recomputes and updates all dependent values
  - [x] Verify "Why?" explanations update after changes

### Phase 6: End-to-End Testing ✅ COMPLETE

**Goal**: Automated tests to ensure demo works correctly.

**Files**:
- `tests/e2e/demos.spec.ts` (added Tax Calculator Demo section - 13 tests)

**Tasks**:
- [x] Test: Initial render shows all values
- [x] Test: "Why?" button opens modal with explanation
- [x] Test: Explanation shows dependency chain (not empty) - with actual formulas!
- [x] Test: Changing assumption triggers recomputation
- [x] Test: Derived values update correctly
- [x] Test: "Why?" explanation updates after change
- [x] Test: Each output shows its unique formula in Why? modal

**Example Test**:
```typescript
test('Why button shows derivation chain', async ({ page }) => {
  await page.goto('file://demo/tax_demo.html');

  // Click "Why?" on net_income
  await page.click('[data-why="net_income"]');

  // Modal should appear with explanation
  const modal = page.locator('.why-modal');
  await expect(modal).toBeVisible();

  // Should show dependency on income and tax_amount
  await expect(modal).toContainText('income');
  await expect(modal).toContainText('tax_amount');
});

test('Changing assumption recomputes values', async ({ page }) => {
  await page.goto('file://demo/tax_demo.html');

  // Get initial net_income
  const initialNet = await page.locator('[data-value="net_income"]').textContent();

  // Change tax_rate from 25 to 30
  await page.fill('[data-input="tax_rate"]', '30');

  // net_income should change
  const newNet = await page.locator('[data-value="net_income"]').textContent();
  expect(newNet).not.toBe(initialNet);
});
```

## Acceptance Criteria

### Functional Requirements

- [x] Generated HTML works entirely in browser (no server)
- [x] "Why?" buttons show actual derivation chains (not empty) - with real formulas!
- [x] Sliders trigger real recomputation
- [x] Derived values update correctly when assumptions change
- [x] Explanations update to reflect new values after changes

### Non-Functional Requirements

- [x] HTML file < 100KB uncompressed (tax_demo.html is ~45KB)
- [x] Initial render < 500ms on modern browser
- [x] Recomputation < 100ms for typical demos
- [x] Works offline (no external dependencies)

### Quality Gates

- [x] All Playwright tests pass (51 total tests)
- [ ] Demo works in Chrome, Firefox, Safari (Chrome tested via Playwright)
- [x] No console errors during normal operation (test verifies this)
- [ ] Accessibility: keyboard navigation works

## Success Metrics

1. **Functional Demo**: Tax simulation demo works end-to-end
2. **AI Debugging Value**: "Why?" explanations useful for understanding values
3. **Interactive**: Users can explore "What if?" scenarios
4. **Self-Contained**: Single HTML file, no server needed

## Dependencies & Risks

### Dependencies

- `DerivationAnalyzer` must correctly extract dependencies (exists, needs verification)
- `JsGen` must preserve computation structure (verified working)
- `EmbedGenerator` framework exists (needs enhancement)

### Risks

| Risk | Mitigation |
|------|------------|
| Circular dependencies in derivations | Add cycle detection in `_explain()` |
| Large derivation graphs slow rendering | Lazy loading, collapsible tree |
| Formula strings not human-readable | Improve `exprToFormula()` formatting |

## File Summary

| File | Action | Purpose |
|------|--------|---------|
| `simulation-cli/.../DerivationAnalyzer.scala` | Verify | Dependency extraction |
| `core/.../EmbedGenerator.scala` | Modify | Populate deps, generate recompute |
| `core/.../WhyExplainer.scala` | Reference | CSS/styling patterns |
| `demo/tax_simulation.bosatsu` | Create | Example demo source |
| `demo/tax_demo.html` | Generate | Output demo file |
| `tests/e2e/ai-debugging-demo.spec.ts` | Create | E2E tests |

## References

### Internal

- Brainstorm: `docs/brainstorms/2026-01-28-ai-debugging-demo-brainstorm.md`
- Existing plan: `docs/plans/2026-01-26-feat-bosatsu-provenance-tooling-plan.md`
- DerivationAnalyzer: `simulation-cli/src/main/scala/dev/bosatsu/simulation/DerivationAnalyzer.scala`
- EmbedGenerator: `core/src/main/scala/dev/bosatsu/ui/EmbedGenerator.scala`
- WhyExplainer: `core/src/main/scala/dev/bosatsu/ui/WhyExplainer.scala`

### Institutional Learnings

- State monad pattern: `docs/solutions/design-patterns/state-monad-for-ast-analysis.md`
- Store AST references: `docs/solutions/design-patterns/store-ast-references-not-copies.md`
- DOM selector fixes: `docs/solutions/runtime-errors/canvas-visualization-container-selector-fixes.md`
