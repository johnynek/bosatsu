# BosatsuUI Batching API

BosatsuUI provides configurable batching for DOM updates, allowing you to optimize for different use cases: high-throughput dashboards, latency-sensitive drag-drop, or mixed scenarios like spreadsheets.

## Configuration

```javascript
BosatsuUI.configure({
  // Number of updates before auto-flush (default: Infinity)
  batchSize: Infinity,

  // Time-based flush delay (default: 'microtask')
  // Options: 0 (synchronous), 16 (frame-aligned), 'microtask' (end of microtask queue)
  flushDelay: 'microtask'
});
```

## API Reference

### `BosatsuUI.configure(options)`

Configure batching behavior. Can be called at any time.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `batchSize` | `number` | `Infinity` | Flush after this many updates |
| `flushDelay` | `number \| 'microtask'` | `'microtask'` | Time delay before flush |

### `BosatsuUI.flush()`

Immediately apply all pending DOM updates. Use this for:
- Ensuring DOM is updated before reading positions
- Forcing updates between dependency levels (formulas)
- Testing/debugging

### `BosatsuUI.batch(fn)`

Execute a function with batching enabled. Updates are flushed once at the end.

```javascript
BosatsuUI.batch(() => {
  setState(['a'], 1);
  setState(['b'], 2);
  setState(['c'], 3);
  // Single DOM flush at end
});
```

### `BosatsuUI.getConfig()`

Returns the current batching configuration.

```javascript
const config = BosatsuUI.getConfig();
// { batchSize: Infinity, flushDelay: 'microtask' }
```

## Common Configurations

### High-Throughput (Dashboards)

Full batching for maximum throughput. Updates are deferred to microtask.

```javascript
BosatsuUI.configure({
  batchSize: Infinity,
  flushDelay: 'microtask'
});
```

**Best for:** Real-time dashboards, data visualizations, batch data processing

### Immediate Mode (Drag-Drop)

No batching. Every setState immediately updates DOM.

```javascript
BosatsuUI.configure({
  batchSize: 1,
  flushDelay: 0
});
```

**Best for:** Drag-and-drop, animations where you read DOM positions, interactive drawing

### Frame-Aligned (Smooth Animations)

Batch updates but flush on animation frame boundary.

```javascript
BosatsuUI.configure({
  batchSize: Infinity,
  flushDelay: 16
});
```

**Best for:** Animations, smooth scrolling, visual effects

### Mixed (Spreadsheets)

Full batching with manual flush for dependency chains.

```javascript
BosatsuUI.configure({
  batchSize: Infinity,
  flushDelay: 'microtask'
});

// User types in cell A1
setState(['cells', 'A1'], '100');

// Recalculate formulas in dependency order
BosatsuUI.flush();  // A1 is now in DOM
const a1 = parseFloat(getElement('A1').value);
setState(['cells', 'B1'], a1 * 2);

BosatsuUI.flush();  // B1 is now in DOM
const b1 = parseFloat(getElement('B1').value);
setState(['cells', 'C1'], b1 + 10);
```

**Best for:** Spreadsheets, form validation, computed fields

## Why Batching Matters

Without batching, 10,000 setState calls = 10,000 DOM updates.

With batching, 10,000 setState calls = 1 DOM update (last-write-wins for each path).

| Scenario | setState calls | DOM updates |
|----------|---------------|-------------|
| No batching | 10,000 | 10,000 |
| Full batching | 10,000 | 1 |

This is why BosatsuUI with batching is **20x faster** than React in throughput benchmarks - both batch similarly, but BosatsuUI skips VDOM diffing entirely.

## Demos

- [Real-time Dashboard](/demos/ui/dashboard.html) - Full batching mode
- [Drag and Drop](/demos/ui/drag-drop.html) - Immediate mode
- [Spreadsheet](/demos/ui/spreadsheet.html) - Mixed batching
- [Performance Benchmark](/demos/benchmarks/ui-performance/index.html) - Compare configurations
