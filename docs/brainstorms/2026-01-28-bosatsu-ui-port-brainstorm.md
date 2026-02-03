# BosatsuUI: Porting burritoUI's Static Analysis DOM Updates

**Date:** 2026-01-28

## What We're Building

A UI library for Bosatsu that uses **static analysis of TypedExpr** to generate targeted DOM updates without virtual DOM diffing.

### The Magic

When state changes, instead of re-rendering and diffing:
1. Static analysis extracts `statePath → DOMProperty` bindings at compile time
2. Runtime looks up bindings for changed path
3. Direct `element.property = value` updates

### Use Case

General-purpose UI apps (not just simulations), compiling to JavaScript for browser.

## Source: burritoUI Structure

Reference: `/Users/steven/Documents/Code/portToBosatsu/burritoscript/src/ui/`

| File | Purpose | Size |
|------|---------|------|
| `vnode.ts` | VNode types + constructors `h()`, `text()`, `fragment()` | 185 lines |
| `analyze.ts` | Walk Effect AST → extract `DOMBinding` | 390 lines |
| `dom.ts` | Create DOM from VNode, element cache, `applyBindingUpdate()` | 342 lines |
| `runtime.ts` | `UIRuntime`: mount, setState → apply bindings | 420 lines |
| `events.ts` | Action-based event handlers | ~200 lines |
| `render-to-string.ts` | SSR support | ~150 lines |

### Key Types from burritoUI

```typescript
// VNode types
type VNode = VElement | VText | VFragment | null
interface VElement { type: 'element', tag: string, props?: VProps, children?: VNode[] }
interface VText { type: 'text', text: string }

// Binding from static analysis
interface DOMBinding {
  selector: string      // CSS selector to find element
  property: DOMProperty // textContent, className, style, value, checked, disabled
  statePath: string[]   // Path into state object
  conditional: boolean
  transform?: string    // Optional value transform
}

// Analysis result
interface DOMAnalysis {
  stateReads: string[][]  // All state paths read
  bindings: DOMBinding[]  // State → DOM bindings
}
```

## Why This Approach (Direct Port)

1. **Proven architecture** - burritoUI works, we know the abstractions are sound
2. **File-by-file reference** - can port incrementally with clear mapping
3. **TypedExpr has MORE info** - types enable optimizations burritoUI can't do

### TypedExpr Advantages Over Effect AST

- **Type information** - know if a value is Int, String, Bool → optimize binding updates
- **Existing `freeVarsSet`** - dependency tracking already built in
- **Matchless IR** - could analyze at IR level for even more precision

## Key Decisions

1. **UI syntax:** Function calls like `h('div', props, children)` - matches burritoUI
2. **State access:** Use existing `Bosatsu/IO` effect monad with `IO.read(['path'])`
3. **Compilation target:** JavaScript/browser via JsGen
4. **Architecture:** Direct port of burritoUI's 6-file structure

## BosatsuUI File Mapping

| burritoUI | BosatsuUI | Location |
|-----------|-----------|----------|
| `vnode.ts` | `VNode.scala` | `core/src/main/scala/dev/bosatsu/ui/` (partially exists) |
| `analyze.ts` | `UIAnalyzer.scala` | NEW - walk TypedExpr for IO.read |
| `dom.ts` | Generated JS | Part of JsGen output |
| `runtime.ts` | `bosatsu-ui-runtime.js` | JS runtime shipped with apps |
| `events.ts` | `Events.scala` | Types + codegen for event handlers |
| `render-to-string.ts` | Later | SSR not needed initially |

## Open Questions

1. **IO.read syntax** - Does `Bosatsu/IO` already have path-based state read, or do we need to add it?
2. **Element IDs** - How to generate unique selectors? Use `data-bosatsu-id` like burrito?
3. **Event handler compilation** - How do action dispatches compile to JS?
4. **Type-based optimizations** - What can we do with types that burrito can't?

## Next Steps

1. Create stacked branch `feat/bosatsu-ui` from current `feat/ai-debugging-browser-demo`
2. Port `vnode.ts` → expand existing `VNode.scala`
3. Port `analyze.ts` → create `UIAnalyzer.scala` for TypedExpr
4. Generate JS runtime alongside compiled code
5. Build simple counter example end-to-end

## References

- burritoUI source: `/Users/steven/Documents/Code/portToBosatsu/burritoscript/src/ui/`
- Existing VNode: `core/src/main/scala/dev/bosatsu/ui/VNode.scala`
- TypedExpr: `core/src/main/scala/dev/bosatsu/TypedExpr.scala`
- Matchless IR: `core/src/main/scala/dev/bosatsu/Matchless.scala`
