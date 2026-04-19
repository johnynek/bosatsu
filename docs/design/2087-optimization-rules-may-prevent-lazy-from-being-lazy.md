---
issue: 2087
priority: 3
touch_paths:
  - docs/design/2087-optimization-rules-may-prevent-lazy-from-being-lazy.md
  - core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala
  - core/src/test/scala/dev/bosatsu/TypedExprTest.scala
  - core/src/test/scala/dev/bosatsu/Issue2087Test.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-09T03:50:58Z
---

# Issue #2087 Design: Preserve Thunk Laziness Across Optimization

_Issue: #2087 (https://github.com/johnynek/bosatsu/issues/2087)_

## Summary

Proposes treating `Unit -> a` lambdas as suspension boundaries in `TypedExprNormalization` so lambda code motion and immutable-expression sharing cannot hoist delayed work out of lazy thunks.

---
issue: 2087
title: Optimization rules may prevent Lazy from being lazy
status: proposed
base_branch: main
touch_paths:
  - docs/design/2087-optimization-rules-may-prevent-lazy-from-being-lazy.md
  - core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala
  - core/src/test/scala/dev/bosatsu/TypedExprTest.scala
  - core/src/test/scala/dev/bosatsu/Issue2087Test.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-09
---

# Issue #2087 Design: optimization rules may prevent Lazy from being lazy

Issue: #2087 (https://github.com/johnynek/bosatsu/issues/2087)  
Base branch: `main`

## Summary

Treat `Unit -> a` lambdas as suspension boundaries in typed-expression normalization so optimizer code motion cannot precompute thunk bodies before they are forced.

## Problem statement

`lazy(() -> x)` is expected to delay construction of `x` until `get_Lazy` forces the thunk. Current normalization can move closed computations from inside lambdas to the lambda creation site. That is semantically visible when delayed work diverges or is never forced, and it defeats the purpose of `Lazy`.

## Current code review findings

1. `TypedExprNormalization.normalizeLetOpt` has a lambda rewrite that can float a non-simple `let` from inside a lambda to outside that lambda when the bound expression does not depend on lambda arguments.
2. The same lambda rewrite block can float a `match` scrutinee out of a lambda under similar dependency checks.
3. `TypedExprNormalization.shareImmutableValues` performs whole-expression sharing and can introduce outer `let` bindings for repeated immutable expressions that appear inside lambda bodies and do not depend on lambda arguments.

By inspection, these paths make issue #2087 likely today for thunk-shaped lambdas.

## Goals

1. Preserve delayed evaluation semantics for thunk-shaped lambdas used by `lazy` and similar APIs.
2. Avoid brittle symbol-name special-casing tied to `Bosatsu/Lazy.lazy`.
3. Keep existing optimization behavior for non-thunk lambdas.
4. Add regression tests that pin the no-hoist behavior for thunks.

## Non-goals

1. No runtime changes to `Bosatsu/Lazy`.
2. No broad optimizer shutdown for all lambdas.
3. No full strictness or effect analysis redesign in this issue.

## Design decision

Define a suspension boundary as an `AnnotatedLambda` with exactly one argument whose normalized type is `Unit`.

This is the key policy choice for #2087.

1. It is robust to refactors and aliases because it is type- and shape-based.
2. It aligns with Bosatsu `() -> a` usage as delayed computation.
3. It is narrow enough to avoid unnecessary regressions in other optimization cases.

## Proposed architecture changes

### 1) Add a shared suspension helper in `TypedExprNormalization`

Add a local helper that checks whether lambda args represent a suspension boundary:

`isSuspensionLambdaArgs(args) == true` iff args length is 1 and arg type `sameAs` `Type.UnitType` after normalization.

Use this helper consistently for all outward lambda code-motion checks.

### 2) Guard outward lambda rewrites in `normalizeLetOpt`

In the `AnnotatedLambda` case:

1. Keep current in-lambda simplifications.
2. Keep eta conversion.
3. Disable the outward `let` float rewrite when the lambda is a suspension boundary.
4. Disable the outward `match` float rewrite when the lambda is a suspension boundary.

This prevents eager evaluation from being moved to lambda creation time for `Unit -> a` thunks.

### 3) Prevent `shareImmutableValues` from crossing suspension boundaries

Update candidate collection and replacement state in `shareImmutableValues` to track whether traversal is inside a suspension boundary.

1. While inside suspension, do not add expressions to outer sharing candidates.
2. While inside suspension, do not replace with bindings created outside suspension.

This closes the second known path where thunk-internal work can be hoisted outward.

## Alternatives considered

1. Special-case `Bosatsu/Lazy.lazy` by global name.
Reason rejected: fragile under refactor, aliases, wrappers, and future thunk APIs.
2. Disable lambda outward code motion for all lambdas.
Reason rejected: larger optimization loss than required.
3. Introduce full strictness/effect-aware code motion.
Reason rejected: much larger scope and risk than this issue.

## Implementation plan

1. Add suspension boundary helper logic in `TypedExprNormalization.scala`.
2. Gate lambda outward `let` and `match` rewrites on non-suspension lambdas.
3. Add suspension-boundary tracking to `shareImmutableValues` collection and replacement passes.
4. Add focused normalization tests in `TypedExprTest.scala`.
5. Add an issue regression test in `Issue2087Test.scala` that verifies optimized `lazy(() -> ...)` shapes keep expensive work inside the thunk lambda.
6. Run targeted suites for normalization and lazy behavior.

## Testing strategy

1. Keep existing non-`Unit` lambda hoist tests passing.
2. Add a test showing a non-simple `let` inside `Unit -> a` lambda is not floated outside.
3. Add a test showing repeated closed expressions inside `Unit -> a` lambda are not shared by an outer `let`.
4. Add an issue-focused compile/normalize test using `Bosatsu/Lazy` that asserts delayed computation remains under the thunk lambda boundary.
5. Confirm existing lazy runtime tests remain green.

## Acceptance criteria

1. The design documents current optimizer paths that can break thunk laziness in `TypedExprNormalization`.
2. `TypedExprNormalization` has a single helper that identifies suspension lambdas by `Unit` argument type.
3. Outward lambda `let` float rewrites do not apply to suspension lambdas.
4. Outward lambda `match` float rewrites do not apply to suspension lambdas.
5. `shareImmutableValues` does not move expressions from inside suspension lambdas to outside those lambdas.
6. Existing non-suspension lambda optimization behavior remains validated by tests.
7. New regression tests for issue #2087 fail before the change and pass after it.
8. Optimized typed expressions for `lazy(() -> delayedExpr)` retain `delayedExpr` inside the lambda body.

## Risks and mitigations

1. Risk: reduced optimization opportunities for some legitimate `Unit -> a` callbacks.
Mitigation: scope only blocks outward motion across suspension boundaries and keeps non-suspension behavior unchanged.
2. Risk: a future optimization path reintroduces boundary crossing.
Mitigation: central helper plus issue-specific regression test.
3. Risk: brittle shape tests.
Mitigation: assert boundary invariants about where expensive expressions live, not exact pretty-printed output.

## Rollout notes

1. Land as one compiler-focused PR with tests and no feature flag.
2. No user migration is required.
3. Validate with targeted test runs before full CI.
4. If optimization regressions appear in practice, follow up with boundary-local sharing that does not cross suspension boundaries.
