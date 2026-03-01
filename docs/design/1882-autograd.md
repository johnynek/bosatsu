---
issue: 1882
priority: 3
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
estimated_size: M
generated_at: 2026-03-01T07:36:25Z
---

# Issue #1882 Design: Statically Checked Autograd

_Issue: #1882 (https://github.com/johnynek/bosatsu/issues/1882)_

## Summary

Design doc content for adding a first-class differentiable-function type, intrinsic diff/grad typing/lowering, static autograd validation, subtype coercion to ordinary functions, and phased rollout notes.

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
generated_at: 2026-03-01
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

### 4. Supported input type domain for this issue

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

### 2. Differentiability checker

Add `AutogradCheck` pass over typed lambda bodies used by `diff`.

Checker responsibilities:

1. Verify input type is in supported domain.
2. Verify result type is `Float64`.
3. Verify every active-path operation has a derivative rule.
4. Reject unsupported constructs with precise regions.

Initial unsupported constructs (hard errors):

1. Recursion/`loop`/`recur` in diff bodies.
2. Calls to unknown external functions without registered derivative rules.
3. Pattern/control-flow whose branch condition depends on active differentiable values.
4. Non-scalar-output objectives.

### 3. Lowering strategy

Lower `diff(f)` at compile time into a concrete `Diff(...)` struct value:

1. `run` field is the original function value.
2. `grad` field is compiler-generated gradient function produced from typed `f`.

Lowering output uses existing `TypedExpr` forms (`AnnotatedLambda`, `Let`, `App`, `Match`, literals, globals), so no new backend IR form is required.

### 4. Derivative rule registry

Add a central derivative-rule table keyed by fully-qualified value name (and arity), initially for:

1. `+`, `-`, `*`, `/` on `Float64`
2. Core `Bosatsu/Num/Float64` unary functions like `sin`, `cos`, `exp`, `log`, `sqrt`, etc.

If a called function is itself `Diff[a]` used via `run_Diff`, chain via its stored `grad` field.

### 5. Subtype coercion implementation

Extend subsumption/coercion path so when `Diff[a]` is required where `a -> Float64` is expected, generated coercion rewrites the expression to `run_Diff(diffValue)`.

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
2. Do not block autograd on shape typing.

### Rationale

1. Gradient soundness for scalar-output scalar-domain functions does not require dimension-indexed types.
2. Bosatsu can add shape-aware tensor APIs later as an orthogonal improvement.

### Follow-up direction

`Array[Float64]` gradients can be added later with either:

1. shape-erased runtime checks, or
2. a future dimension-typed tensor layer using type-level naturals.

That follow-up should be separate from this issue.

## Prior Art

1. Swift's differentiable function design explicitly models differentiability as a separate function type and discusses function subtyping/runtime representation, which matches this issue's type-relationship requirement.
2. JAX's `grad`/`value_and_grad` APIs validate the practical model of transformation-based AD with scalar-output gradient entry points.
3. Conal Elliott's 'The Simple Essence of Automatic Differentiation' and Pearlmutter/Siskind's reverse-mode functional AD work provide strong foundations for typed, compositional AD transformations.

Primary references:

1. Swift differentiable programming manifesto: https://raw.githubusercontent.com/swiftlang/swift/main/docs/DifferentiableProgramming.md
2. JAX autodiff docs: https://docs.jax.dev/en/latest/automatic-differentiation.html
3. Elliott 2018: https://arxiv.org/abs/1804.00746
4. Pearlmutter and Siskind (functional reverse-mode AD): https://doi.org/10.1145/1411204.1411222

## Implementation Plan

1. Add `Bosatsu/Num/Autograd` library module with `Diff`, `run_Diff`, and `grad` surface.
2. Add autograd type helpers in `rankn/Type.scala` (`Diff` extractor/helpers).
3. Add intrinsic detection for `diff` and `grad` in `rankn/Infer.scala` application/typechecking paths.
4. Implement `AutogradCheck` (new module) for structural differentiability validation.
5. Implement `AutogradLowering` (new module) to produce the generated `grad` closure used inside lowered `Diff` values.
6. Add subsumption/coercion support for `Diff[a] <:< a -> Float64`.
7. Plumb autograd errors into `PackageError` and message rendering.
8. Wire checker/lowering into package inference pipeline via `Package.inferBodyUnopt`.
9. Add tests (parser/infer/error/eval/tooling) and language-guide updates.

## Testing Strategy

### Type and inference tests

1. `diff(x -> x * x)` has type `Diff[Float64]`.
2. `grad(diff(x -> x * x))` has type `Float64 -> Float64`.
3. `Diff[a]` values are accepted where `a -> Float64` is expected.
4. Ordinary functions are rejected where `Diff[a]` is required.

### Error tests

1. Unsupported call in a `diff` body yields `AutogradUnsupportedCall` with region.
2. Unsupported recursion/control-flow in `diff` body yields dedicated autograd error.
3. Unsupported input type yields `AutogradUnsupportedInputType`.

### Evaluation and integration tests

1. Numeric spot checks for derivatives (`x*x`, `sin`, `exp`, compositions).
2. Composition checks (`grad(diff(f.compose(g)))` patterns).
3. Ensure compiled codegen paths (Python/C where covered by existing suites) run lowered autograd output with no backend-specific IR changes.

## Acceptance Criteria

1. A `Bosatsu/Num/Autograd` module exists with `Diff`, `diff`, `grad`, `run_Diff` API.
2. `diff` is compiler-recognized and rejects non-differentiable bodies at compile time.
3. `grad` typechecks only for `Diff[a]` and returns `a -> a`.
4. Compiler implements one-way subtype/coercion `Diff[a] <:< a -> Float64`.
5. `diff` lowering produces ordinary core typed expressions (no new backend IR node required).
6. Autograd errors are represented in `PackageError` with region-aware messages.
7. Tests cover successful typing, coercion behavior, static failures, and runtime numeric sanity checks.
8. Language-guide docs describe autograd API, limits, and failure modes.
9. Attempting `diff` on unsupported domains (including `Array[Float64]` in this phase) fails with explicit diagnostics.

## Risks and Mitigations

1. Risk: Coercion complexity in inference may introduce subtle subsumption regressions.
   Mitigation: isolate `Diff` subtype rule, add focused `RankNInferTest` coverage for unrelated function-subtyping scenarios.
2. Risk: Gradient lowering can capture/shadow incorrectly in nested lets/lambdas.
   Mitigation: reuse existing `TypedExpr` substitution/unshadow utilities and add stress tests for nested scopes.
3. Risk: User surprise from rejected control-flow cases.
   Mitigation: explicit diagnostics with rewrite guidance and clear documentation of supported subset.
4. Risk: Numerical discrepancies near singularities.
   Mitigation: document runtime math limits; keep semantics aligned with current `Float64` behavior.

## Rollout Notes

1. Land as a single feature PR with docs + tests.
2. No CLI flag in first release; feature is active when importing and calling `Bosatsu/Num/Autograd.diff`.
3. Keep `Array[Float64]` differentiation explicitly disabled in this issue to avoid ambiguous partial support.
4. Follow up with a second design for array/tensor gradients and shape-typing strategy.

## Follow-Up (Out of Scope)

1. Add `value_and_grad`, `jvp`, and `vjp` intrinsics once first-order `grad` is stable.
2. Expand differentiable domain to `Array[Float64]` and selected tensor primitives.
3. Evaluate optional type-level natural numbers for shape-safe tensor APIs.
4. Consider richer smooth-control-flow support (piecewise differentiation policy).
