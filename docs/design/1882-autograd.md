---
issue: 1882
title: Autograd
status: proposed
base_branch: main
touch_paths:
  - docs/design/1882-autograd.md
  - test_workspace/Bosatsu/Num/Autograd.bosatsu
  - core/src/main/scala/dev/bosatsu/AutogradCheck.scala
  - core/src/main/scala/dev/bosatsu/AutogradLowering.scala
  - core/src/main/scala/dev/bosatsu/rankn/Type.scala
  - core/src/main/scala/dev/bosatsu/rankn/Infer.scala
  - core/src/main/scala/dev/bosatsu/TypedExpr.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/TypeRenderer.scala
  - core/src/test/scala/dev/bosatsu/rankn/RankNInferTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: L
generated_at: 2026-03-02
---

# Issue #1882 Design: Statically Checked Autograd

Issue: #1882 (https://github.com/johnynek/bosatsu/issues/1882)
Base branch: `main`

## Summary

Add a first-class differentiable-function type plus compiler intrinsics for `diff` and `grad`, with static rejection of non-differentiable function bodies and an explicit subtype relation so differentiable functions can be used as normal functions.

## Problem Statement

The issue asks for:

1. Compiler-supported gradients now that `Float64` and `Array[_]` exist.
2. Static checking so invalid gradient requests fail at compile time.
3. A type distinction between differentiable and ordinary functions, while still allowing differentiable functions where ordinary functions are expected.
4. A position on whether tensor shape types (type-level integers) are required.
5. Prior art and guidance.

Today Bosatsu has rank-n type inference, subtype-style subsumption checks, and rich typed-expression passes, but no concept of differentiability or gradient transformation.

## Goals

1. Introduce a user-visible differentiable-function type.
2. Support `diff` (construct a differentiable function) and `grad` (obtain gradient function) as compiler-checked intrinsics.
3. Add compile-time failure modes for unsupported/non-differentiable function bodies.
4. Allow implicit use of differentiable functions as ordinary functions via subtype/coercion.
5. Keep lowering output in existing core IR forms so evaluators/codegen do not need a new runtime IR node.

## Non-Goals (This Issue)

1. Full tensor algebra with compile-time shape checking.
2. Higher-order differentiation (`grad(grad(f))`) guarantees for all programs.
3. Universal support for all externals and all control-flow styles in gradientable code.
4. Numerically robust handling of every singularity point (for example division by zero, `log(0)`, undefined derivatives at branch boundaries).

## User-Facing Design

### 1. New Autograd package

Add `Bosatsu/Num/Autograd`:

1. `Diff[a]`: differentiable scalar-output function wrapper.
2. `diff`: intrinsic constructor.
3. `grad`: gradient projection.
4. `run_Diff`: ordinary function projection.

Proposed API surface:

```bosatsu
package Bosatsu/Num/Autograd

from Bosatsu/Predef import Float64

export (
  Diff,
  diff,
  grad,
  run_Diff,
)

struct Diff[a](run: a -> Float64, grad: a -> a)

# compiler intrinsic
external def diff[a](f: a -> Float64) -> Diff[a]

def run_Diff[a](d: Diff[a]) -> a -> Float64:
  match d:
    case Diff(run, _): run

def grad[a](d: Diff[a]) -> a -> a:
  match d:
    case Diff(_, g): g
```

### 2. Syntax

No new parser syntax is required in this design. Usage is normal function-call syntax:

```bosatsu
from Bosatsu/Num/Autograd import diff, grad

square = diff(x -> x * x)

dsquare = grad(square)

# `square` can be used where `Float64 -> Float64` is expected
ys = xs.map_Array(square)
```

This keeps grammar stable while giving explicit autograd intent.

## Type-System Design

### 1. Distinct differentiable-function type

`Diff[a]` is distinct from `a -> Float64`.

### 2. Subtype/coercion relation

Add compiler rule:

`Diff[a] <:< (a -> Float64)`

This is one-way only. Ordinary functions are not implicitly differentiable.

### 3. Intrinsic typing rules

1. `diff(e)` requires `e` to typecheck as `a -> Float64`.
2. `diff(e)` runs a differentiability checker on typed `e`.
3. If checker passes, result type is `Diff[a]`.
4. `grad(e)` requires `e: Diff[a]`; result type is `a -> a`.

### 4. Why `grad` has type `a -> a`

`grad` returns a tangent/cotangent value with the same product structure as the input domain.

Examples:

1. Scalar input:

```bosatsu
f = diff(x -> x * x)
# f: Diff[Float64]

g = grad(f)
# g: Float64 -> Float64
```

2. Tuple input:

```bosatsu
f2 = diff((x, y) -> x * y + y * y)
# f2: Diff[(Float64, Float64)]

g2 = grad(f2)
# g2: (Float64, Float64) -> (Float64, Float64)
# g2(x, y) = (d/dx, d/dy) = (y, x + 2*y)
```

3. Nested tuple input:

```bosatsu
f3 = diff(((x1, x2), y) -> x1 * y + x2)
# grad(f3): ((Float64, Float64), Float64) -> ((Float64, Float64), Float64)
```

The gradient shape mirrors the input shape.

### 5. Supported input type domain for this issue

Initial accepted `a` domain:

1. `Float64`
2. Tuples recursively composed of `Float64`

`Array[Float64]` is explicitly deferred in this issue (see rollout/follow-up). Calls that attempt `diff` on unsupported domains fail with dedicated autograd type errors.

## Compiler Architecture

### 1. Intrinsic detection in inference

In `Infer.typeCheckRho`/apply paths, detect calls to:

1. `Bosatsu/Num/Autograd::diff`
2. `Bosatsu/Num/Autograd::grad`

These are handled as intrinsics (not ordinary external runtime calls).

### 2. Subsumption/coercion changes in `Infer` (explicit)

This is a new coercion-producing subsumption case in `Infer.subsCheckRho2`.

Required additions:

1. Add a `Type.DiffT.unapply(t: Type): Option[Type]` helper in `rankn/Type.scala`.
2. In `subsCheckRho2`, add a case when inferred type is `Diff[b]` and expected type is function-like `a -> Float64`.
3. Implement it by reducing to existing function subsumption, then composing a runtime projection coercion:
   1. compute `runType = b -> Float64`,
   2. ask existing engine to prove `runType <:< (a -> Float64)`,
   3. return a coercion that rewrites value `d: Diff[b]` to `coerce_fn(run_Diff(d))`.

So the effective judgment is:

1. `Diff[b] <:< (a -> Float64)` iff `(b -> Float64) <:< (a -> Float64)`.
2. Existing function rule then enforces contravariance (`a <:< b`) and codomain equality.
3. Runtime insertion is exactly one call to `run_Diff` during coercion construction.

This keeps the new logic local to one subtype relation and reuses existing function-subsumption machinery.

### 3. Differentiability checker

Add `AutogradCheck` pass over typed lambda bodies used by `diff`.

Checker responsibilities:

1. Verify input type is in supported domain.
2. Verify result type is `Float64`.
3. Verify every active-path operation has a derivative rule.
4. Reject unsupported constructs with precise regions.

#### Composition with current package model (imports only expose exported names/types)

Because Bosatsu compiles one package at a time and imported implementations are unavailable, checker rules are:

1. Local functions/lambdas: allowed when checker can inspect their typed bodies in the same package.
2. Imported/global values with unknown body: allowed only if one of:
   1. name is in a builtin derivative registry (for example `Bosatsu/Num/Float64::sin`),
   2. value type is `Diff[t]` and it is used through `run_Diff` / `grad` interface.
3. Other imported opaque functions are rejected in `diff` bodies (`AutogradUnsupportedCall`) because no derivative witness is available.

This makes cross-package composition explicit and deterministic.

### 4. Branching policy (explicit)

For this issue, checker allows only branch conditions that are non-active:

1. Condition must not depend on differentiable inputs.
2. Both branches must be individually differentiable and return `Float64`.

Allowed pattern:

```bosatsu
def choose(use_sin: Bool) -> Diff[Float64]:
  if use_sin:
    diff(x -> sin(x))
  else:
    diff(x -> cos(x))
```

Also allowed:

```bosatsu
def choose2(use_sin: Bool) -> Diff[Float64]:
  diff(x -> if use_sin: sin(x) else: cos(x))
```

In `choose2`, `use_sin` is non-active (closed-over Bool), so branch choice is treated as control/configuration, not a differentiation variable.

Rejected in phase 1:

1. Branch condition depending on active differentiable input.
2. Input domains that include non-float leaves (for example `(Bool, Float64)`) for `diff`.

### 5. Lowering strategy

Lower `diff(f)` at compile time into a concrete `Diff(...)` struct value:

1. `run` field is the original function value.
2. `grad` field is compiler-generated gradient function produced from typed `f`.

Lowering output uses existing `TypedExpr` forms (`AnnotatedLambda`, `Let`, `App`, `Match`, literals, globals), so no new backend IR form is required.

### 6. Derivative rule registry

Add a central derivative-rule table keyed by fully-qualified value name (and arity), initially for:

1. `+`, `-`, `*`, `/` on `Float64`
2. Core `Bosatsu/Num/Float64` unary functions like `sin`, `cos`, `exp`, `log`, `sqrt`, etc.

If a called function is itself `Diff[a]` used via `run_Diff`, chain via its stored `grad` field.

## Error Model

Add autograd-specific compile errors in `PackageError`, e.g.:

1. `AutogradUnsupportedInputType`
2. `AutogradExpectedScalarOutput`
3. `AutogradUnsupportedCall`
4. `AutogradUnsupportedControlFlow`
5. `AutogradUnsupportedRecursion`

Diagnostics should include:

1. Source region.
2. Offending expression summary.
3. Actionable fix hint (rewrite as differentiable primitive, annotate with `diff`, or refactor).

## Can We Always Detect Invalid Gradients?

No, not fully.

This design guarantees static detection of structural non-differentiability in the supported language subset. It does not prove mathematical differentiability for all runtime values.

Examples not fully decidable statically in this model:

1. `x / y` when `y` may be `0.0`.
2. `log(x)` when `x <= 0`.
3. Piecewise functions with undefined derivative at boundary points.

Policy: reject what is structurally unsupported; allow mathematically partial operations and preserve runtime floating-point behavior.

## Arrays and Type-Level Integers

### Decision for this issue

1. Do not require type-level integers in this issue.
2. Do not block autograd on shape typing long-term.
3. Explicitly defer `Array[Float64]` support from phase 1.

### Why `Array[Float64]` is deferred

`Array[Float64]` is a primary use case, but it adds real complexity beyond tuple-of-float products:

1. Unknown size at compile time:
   1. gradient container must be allocated with runtime length,
   2. many rules need shape-equality runtime checks.
2. Reverse accumulation needs indexed "scatter add" behavior:
   1. repeated index reads/writes must sum cotangents,
   2. this is more complex than fixed-size tuple projection.
3. Existing optimizer/lowering paths are tuned for expression trees, not array-level linear algebra primitives.
4. Without shape types, many mistakes become runtime errors unless checker/lowering carries extra dynamic guards.

This is solvable, but it is large enough to deserve a focused follow-up.

### Product structs and recursive ADTs

Non-recursive product structs that are isomorphic to nested tuples are much easier.

Example:

```bosatsu
struct Pair(p: (Float64, Float64))
```

For this kind of type, tangent representation can be the same structure, so support can be added with tuple-like projection/packing rules.

Recursive ADTs are not phase-1 targets because current autograd subset rejects recursion (`loop`/`recur` and recursive defs in `diff` bodies). Supporting general recursive data + recursive control flow is separate work.

### Follow-up direction

`Array[Float64]` gradients can be added later with either:

1. shape-erased runtime checks, or
2. a future dimension-typed tensor layer using type-level naturals.

That follow-up should be separate from this issue.

## Prior Art

1. Swift's differentiable function design explicitly models differentiability as a separate function type and discusses function subtyping/runtime representation, which matches this issue's type-relationship requirement.
2. JAX's `grad`/`value_and_grad` APIs validate the practical model of transformation-based AD with scalar-output gradient entry points.
3. Conal Elliott's "The Simple Essence of Automatic Differentiation" and Pearlmutter/Siskind's reverse-mode functional AD work provide strong foundations for typed, compositional AD transformations.

Primary references:

1. Swift differentiable programming manifesto: https://raw.githubusercontent.com/swiftlang/swift/main/docs/DifferentiableProgramming.md
2. JAX autodiff docs: https://docs.jax.dev/en/latest/automatic-differentiation.html
3. Elliott 2018: https://arxiv.org/abs/1804.00746
4. Pearlmutter and Siskind (functional reverse-mode AD): https://doi.org/10.1145/1411204.1411222

## Implementation Plan

1. Add `Bosatsu/Num/Autograd` library module with `Diff`, `run_Diff`, and `grad` surface.
2. Add autograd type helpers in `rankn/Type.scala` (`Diff` extractor/helpers).
3. Add intrinsic detection for `diff` and `grad` in `rankn/Infer.scala` application/typechecking paths.
4. Add coercion-producing subsumption case in `subsCheckRho2` for `Diff[b] <:< a -> Float64`, including `run_Diff` insertion.
5. Implement `AutogradCheck` (new module) for structural differentiability validation.
6. Implement `AutogradLowering` (new module) to produce the generated `grad` closure used inside lowered `Diff` values.
7. Plumb autograd errors into `PackageError` and message rendering.
8. Wire checker/lowering into package inference pipeline via `Package.inferBodyUnopt`.
9. Add tests (parser/infer/error/eval/tooling) and language-guide updates.

## Testing Strategy

### Type and inference tests

1. `diff(x -> x * x)` has type `Diff[Float64]`.
2. `grad(diff(x -> x * x))` has type `Float64 -> Float64`.
3. Tuple examples infer `grad: (Float64, Float64) -> (Float64, Float64)`.
4. `Diff[a]` values are accepted where `a -> Float64` is expected, including contravariant checks on input type.
5. Ordinary functions are rejected where `Diff[a]` is required.

### Error tests

1. Unsupported call in a `diff` body yields `AutogradUnsupportedCall` with region.
2. Unsupported recursion/control-flow in `diff` body yields dedicated autograd error.
3. Unsupported input type yields `AutogradUnsupportedInputType`.
4. Branch condition depending on active value yields `AutogradUnsupportedControlFlow`.

### Evaluation and integration tests

1. Numeric spot checks for derivatives (`x*x`, `sin`, `exp`, compositions).
2. Bool-controlled non-active branch examples pass (`choose` and `choose2` style).
3. Ensure compiled codegen paths (Python/C where covered by existing suites) run lowered autograd output with no backend-specific IR changes.

## Acceptance Criteria

1. A `Bosatsu/Num/Autograd` module exists with `Diff`, `diff`, `grad`, `run_Diff` API.
2. `diff` is compiler-recognized and rejects non-differentiable bodies at compile time.
3. `grad` typechecks only for `Diff[a]` and returns `a -> a` (with tuple-structured examples covered by tests).
4. Compiler implements one-way subtype/coercion `Diff[a] <:< a -> Float64` via `Infer.subsCheckRho2` with `run_Diff` insertion.
5. `diff` lowering produces ordinary core typed expressions (no new backend IR node required).
6. Autograd checker composition across package boundaries is enforced: only local-inspectable functions, known derivative intrinsics, and `Diff`-typed imported values are allowed.
7. Autograd errors are represented in `PackageError` with region-aware messages.
8. Tests cover successful typing, coercion behavior, static failures, and runtime numeric sanity checks.
9. Attempting `diff` on unsupported domains (including `Array[Float64]` in this phase) fails with explicit diagnostics.

## Risks and Mitigations

1. Risk: Coercion complexity in inference may introduce subtle subsumption regressions.
   Mitigation: isolate the `Diff` subtype rule, add focused `RankNInferTest` coverage for unrelated function-subtyping scenarios.
2. Risk: Gradient lowering can capture/shadow incorrectly in nested lets/lambdas.
   Mitigation: reuse existing `TypedExpr` substitution/unshadow utilities and add stress tests for nested scopes.
3. Risk: User surprise from rejected control-flow cases.
   Mitigation: explicit diagnostics with rewrite guidance and clear documentation of active vs non-active branching policy.
4. Risk: Numerical discrepancies near singularities.
   Mitigation: document runtime math limits; keep semantics aligned with current `Float64` behavior.

## Rollout Notes

1. Land as a single feature PR with docs + tests.
2. No CLI flag in first release; feature is active when importing and calling `Bosatsu/Num/Autograd.diff`.
3. Keep `Array[Float64]` differentiation explicitly disabled in this issue to avoid ambiguous partial support.
4. Follow up with a second design for array/tensor gradients and shape-typing strategy.

## Follow-Up (Out of Scope)

1. Add `value_and_grad`, `jvp`, and `vjp` intrinsics once first-order `grad` is stable.
2. Expand differentiable domain to non-recursive product structs in addition to tuples.
3. Add `Array[Float64]` and selected tensor primitives.
4. Evaluate optional type-level natural numbers for shape-safe tensor APIs.
5. Consider richer smooth-control-flow support (piecewise differentiation policy).
