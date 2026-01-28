# Brainstorm: Bosatsu/Numeric Module & Static Analysis Stack

**Date:** 2026-01-27
**Status:** Ready for planning

## What We're Building

A complete static analysis and simulation stack using TypedExpr:

1. **Bosatsu/Numeric module** - Double type with symbolic operators that bypass RingOpt
2. **TypedExpr-based provenance** - Dependency tracking using existing ProvenanceAnalyzer
3. **Simulation code generation** - Generate reactive JavaScript from TypedExpr analysis

### The Full Stack

```
.bosatsu file (uses Bosatsu/Numeric)
     │
     ▼
Parse → TypeCheck → TypedExpr
     │                  │
     │                  ├── ProvenanceAnalyzer → DerivationGraph
     │                  │         (existing)
     │                  │
     │                  └── TypedExpr.freeVarsSet → Dependencies
     │                            (existing)
     ▼
Matchless IR (no RingOpt for Numeric ops)
     │
     ▼
JsGen + NumericExternal → JavaScript
     │
     ▼
SimulationGen → Reactive HTML with "Why?" buttons
```

## Why This Approach

### Core Insight

TypedExpr already has analysis infrastructure:
- `TypedExpr.freeVarsSet` - extract dependencies
- `ProvenanceAnalyzer` - build derivation graphs
- `DerivationGraph` - track what depends on what

We don't need new analysis. We need:
1. A numeric module that **preserves operations** (no RingOpt expansion)
2. Code generation that **uses the existing analysis**

### Why Bosatsu/Numeric?

- `TypedExprNormalization` pattern-matches `Bosatsu/Predef::{add,times,sub}`
- A separate module `Bosatsu/Numeric` won't match - operations pass through unchanged
- No compiler changes needed - just a library

## Module API

```bosatsu
package Bosatsu/Numeric

export (
  Double,
  (+.), (-.), (*.), (/.),
  from_Int, to_Int,
  cmp_Double, eq_Double
)

external struct Double

external def (+.)(a: Double, b: Double) -> Double
external def (-.)(a: Double, b: Double) -> Double
external def (*.)(a: Double, b: Double) -> Double
external def (/.)(a: Double, b: Double) -> Double

external def from_Int(i: Int) -> Double
external def to_Int(d: Double) -> Int

external def cmp_Double(a: Double, b: Double) -> Comparison
external def eq_Double(a: Double, b: Double) -> Bool
```

## Static Analysis Integration

### Using Existing TypedExpr Analysis

```scala
// Already exists in TypedExpr.scala
def freeVarsSet[A](te: List[TypedExpr[A]]): Set[Bindable]

// Already exists in ProvenanceAnalyzer.scala
def analyze(expr: TypedExpr[A]): DerivationGraph

// SimulationCommand already uses this:
val deps = TypedExpr.freeVarsSet(List(expr)).intersect(knownBindings)
```

### Connecting to Code Generation

SimulationCommand.scala currently:
1. Type checks to get `TypedExpr`
2. Extracts dependencies using `freeVarsSet`
3. Compiles through Matchless → JsGen
4. Post-processes to remove package prefixes

With Numeric module:
1. Type checks - `*.` is `Bosatsu/Numeric::(*.)`
2. Dependencies extracted from TypedExpr (same)
3. Matchless sees `(*.)` unchanged (not expanded to additions)
4. JsGen/NumericExternal inlines as `*`

## Key Decisions

| Decision | Choice |
|----------|--------|
| Analysis layer | TypedExpr (existing ProvenanceAnalyzer) |
| Numeric type | Double with symbolic operators |
| RingOpt | Bypass by using separate module |
| Code generation | Extend JsGen with NumericExternal |

## Implementation Touchpoints

### New Files
- `core/src/main/resources/bosatsu/numeric.bosatsu` - module definition
- `core/src/main/scala/dev/bosatsu/Numeric.scala` - FFI implementations

### Modified Files
- `JsGen.scala` - add NumericExternal for `Bosatsu/Numeric` package
- `PythonGen.scala` - same pattern
- `Type.scala` - add DoubleType constant
- `SimulationCommand.scala` - update to use Numeric imports

### No Changes Needed
- `ProvenanceAnalyzer.scala` - already works on TypedExpr
- `DerivationGraph.scala` - already tracks dependencies
- `TypedExprNormalization.scala` - won't match Numeric ops
- `RingOpt.scala` - not involved

## Open Questions

1. **Double literals** - Should `3.14` parse as Double? Or require `from_Int`?
2. **Math functions** - `sqrt`, `sin` etc. in Numeric or separate Math module?
3. **Package loading** - How are non-Predef .bosatsu resources discovered?

## Success Criteria

1. `principal *. rate` compiles to `principal * rate` in JavaScript
2. TypedExpr analysis correctly identifies `principal` and `rate` as dependencies
3. "Why?" explanations show `monthly_payment = principal *. rate` (not expanded)
4. SimulationCommand works with Numeric module imports
