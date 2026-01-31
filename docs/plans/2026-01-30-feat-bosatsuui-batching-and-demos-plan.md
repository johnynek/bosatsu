---
title: "feat: BosatsuUI Configurable Batching and Performance Demos"
type: feat
date: 2026-01-30
---

# feat: BosatsuUI Configurable Batching and Performance Demos

## Overview

Add configurable batching controls to BosatsuUI and create demos that showcase its performance advantages over React. Benchmarks revealed that BosatsuUI with proper batching is **20x faster** than React 18 for throughput scenarios.

## Problem Statement / Motivation

Current benchmark shows React 18 "beating" BosatsuUI (4.6M vs 1.5M ops/sec), but this is misleading:

| Framework | setState calls | Actual DOM updates |
|-----------|---------------|-------------------|
| BosatsuUI (benchmark) | 10,000 | 10,000 |
| React 18 | 10,000 | 1 |

React's automatic batching does 10,000x fewer DOM writes. When BosatsuUI uses equivalent batching, it's **20x faster** than React because it skips VDOM diffing entirely.

**The real runtime already has batching** (`bosatsu-ui-runtime.js:196-231`), but:
1. The benchmark uses a simplified inline runtime without batching
2. No configurable controls for batch size/timing
3. No demos showcasing the performance advantage

## Proposed Solution

### 1. Configurable Batching API

Instead of binary batched/unbatched modes, provide **continuous control**:

```javascript
// Configure batching behavior
BosatsuUI.configure({
  // Number of updates before auto-flush (default: Infinity = microtask only)
  batchSize: Infinity,

  // Time-based flush delay in ms (default: 'microtask')
  // 0 = synchronous, 16 = frame-aligned, 'microtask' = default
  flushDelay: 'microtask',
});

// Manual flush for latency-sensitive scenarios
BosatsuUI.flush();

// Explicit batch scope
BosatsuUI.batch(() => {
  setState(['a'], 1);
  setState(['b'], 2);
  // Flushes once at end
});
```

### 2. Update Benchmark

Update `demos/benchmarks/ui-performance/index.html` to:
- Use the real `bosatsu-ui-runtime.js` instead of inline simplified version
- Show batching configuration options
- Display fair comparison with equivalent batching

### 3. Create Performance Demos

Three demos showcasing different strengths:

| Demo | Batching | Advantage |
|------|----------|-----------|
| Real-time Dashboard | Full batching | 20x throughput vs React |
| Drag-and-Drop Editor | `batchSize: 1` | Synchronous for position reads |
| Spreadsheet | Mixed | Batch inputs, sync formulas |

## Technical Considerations

### Architecture

The runtime already has the foundation:

```javascript
// Existing in bosatsu-ui-runtime.js
let _pendingPaths = new Map();  // Last-write-wins monoid
let _flushScheduled = false;
let _inBatch = false;

function _queuePathUpdate(path) {
  const key = path.join('.');
  _pendingPaths.set(key, path);  // Deduplicates automatically
  _scheduleFlush();
}
```

**Changes needed:**
1. Add `_config` object with `batchSize` and `flushDelay`
2. Modify `_scheduleFlush()` to respect configuration
3. Add `configure()` and `flush()` public APIs
4. Export via `window.BosatsuUI` namespace

### Performance Implications

| Config | Behavior | Use Case |
|--------|----------|----------|
| `batchSize: 1` | Flush every update | Drag-drop, animations |
| `batchSize: 10` | Flush every 10 | Balanced responsiveness |
| `batchSize: Infinity, flushDelay: 'microtask'` | Full batching | Dashboards, forms |
| `batchSize: Infinity, flushDelay: 16` | Frame-aligned | Smooth animations |

### Why BosatsuUI Beats React

| Step | React 18 | BosatsuUI |
|------|----------|-----------|
| Collect updates | O(n) state | O(n) state |
| Reconcile | **O(tree) VDOM diff** | **O(1) map merge** |
| Commit | O(changed) DOM | O(changed) DOM |

BosatsuUI skips VDOM diff because compile-time analysis already knows which DOM elements each state path affects.

## Acceptance Criteria

### Core Implementation

- [x] Add `_config` object to `bosatsu-ui-runtime.js` with `batchSize` and `flushDelay` options
- [x] Implement `configure(options)` function to update config
- [x] Modify `_scheduleFlush()` to check `batchSize` threshold
- [x] Implement time-based flushing with `flushDelay` option
- [x] Add public `flush()` function for manual flushing
- [x] Export `BosatsuUI` namespace on `window`

### Benchmark Updates

- [x] Update benchmark to import real `bosatsu-ui-runtime.js`
- [x] Add UI controls to configure batching during benchmark
- [x] Show "DOM writes" count for both frameworks
- [x] Update explanatory text to reflect fair comparison

### Demo: Real-time Dashboard

- [ ] Create `demos/ui/dashboard.bosatsu` with 50+ updating values
- [ ] Simulate WebSocket-style rapid updates
- [ ] Show throughput comparison vs React equivalent
- [ ] Display "updates/sec" and "DOM writes/sec" metrics

### Demo: Drag-and-Drop Editor

- [ ] Create `demos/ui/drag-drop.bosatsu` with draggable elements
- [ ] Use `batchSize: 1` for immediate position updates
- [ ] Each drag reads previous position from DOM
- [ ] Show latency advantage over React's flushSync

### Demo: Spreadsheet with Formulas

- [ ] Create `demos/ui/spreadsheet.bosatsu` with formula cells
- [ ] Batch user input changes
- [ ] Cascade formula recalculations with `flush()` between steps
- [ ] Demonstrate mixed batching strategies

### Testing

- [ ] Add Playwright tests for each new demo
- [ ] Update `benchmark-integrity.spec.ts` for new benchmark structure
- [ ] Add tests verifying batching behavior (count DOM writes)

## Success Metrics

1. **Benchmark shows BosatsuUI advantage**: Batched BosatsuUI >10x faster than React
2. **Fair comparison**: Both frameworks do equivalent DOM writes
3. **Configurable**: Users can tune batching for their use case
4. **Demos work**: All three demos load without errors, showcase advantages

## Dependencies & Risks

**Dependencies:**
- Existing `bosatsu-ui-runtime.js` foundation
- `simulationCli` for generating demo HTML

**Risks:**
- Breaking existing demos that expect immediate updates → Mitigate with `batchSize: 1` default for existing demos
- Benchmark results vary by browser/hardware → Document test environment

## Implementation Tasks

### Phase 1: Runtime Enhancement

```
core/src/main/resources/bosatsu-ui-runtime.js
```

- [x] Add `_config` object with defaults
- [x] Implement `configure()` function
- [x] Add batch size threshold check in `_queuePathUpdate()`
- [x] Implement `flushDelay` timing options
- [x] Add `flush()` public API
- [x] Create `window.BosatsuUI` namespace

### Phase 2: Benchmark Update

```
demos/benchmarks/ui-performance/index.html
```

- [x] Replace inline BosatsuRuntime with real runtime
- [x] Add configuration UI (sliders for batchSize, radio for flushDelay)
- [x] Track and display DOM write counts
- [x] Update result explanations

### Phase 3: Dashboard Demo

```
demos/ui/dashboard.bosatsu
demos/ui/dashboard.html (generated)
```

- [ ] Design dashboard layout (grid of values)
- [ ] Create Bosatsu state model for 50+ values
- [ ] Implement simulated rapid updates
- [ ] Add React comparison panel

### Phase 4: Drag-Drop Demo

```
demos/ui/drag-drop.bosatsu
demos/ui/drag-drop.html (generated)
```

- [ ] Create draggable element VNodes
- [ ] Implement mouse event handlers
- [ ] Use `batchSize: 1` configuration
- [ ] Show position reads working correctly

### Phase 5: Spreadsheet Demo

```
demos/ui/spreadsheet.bosatsu
demos/ui/spreadsheet.html (generated)
```

- [ ] Create cell grid structure
- [ ] Implement formula parsing (simple: SUM, references)
- [ ] Batch input, sync formula cascade
- [ ] Show mixed batching in action

### Phase 6: Testing & Documentation

- [ ] Add Playwright tests for all demos
- [ ] Update `meta-coverage.spec.ts` DEMO_REGISTRY
- [ ] Update landing page with new demos
- [ ] Document batching API in README or docs

## References & Research

### Internal References

- Runtime with existing batching: `core/src/main/resources/bosatsu-ui-runtime.js:196-231`
- UIGen for HTML generation: `core/src/main/scala/dev/bosatsu/ui/UIGen.scala`
- Existing demos: `demos/ui/counter.bosatsu`, `demos/ui/todo-list.bosatsu`
- Benchmark: `demos/benchmarks/ui-performance/index.html`

### Brainstorm Document

- `docs/brainstorms/2026-01-30-bosatsuui-performance-optimization-brainstorm.md`

### Key Benchmark Findings

From live profiling session:
- Batched BosatsuUI: 166M ops/sec (single element)
- React 18: 8M ops/sec
- **BosatsuUI 20x faster with equivalent batching**

Multi-element (100 elements):
- Batched BosatsuUI: 14.3M ops/sec
- React 18: 2.8M ops/sec
- **BosatsuUI 5x faster**
