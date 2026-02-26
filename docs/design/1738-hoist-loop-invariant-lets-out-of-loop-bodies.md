---
issue: 1738
priority: 3
touch_paths:
  - docs/design/1738-hoist-loop-invariant-lets-out-of-loop-bodies.md
  - core/src/main/scala/dev/bosatsu/Matchless.scala
  - core/src/test/scala/dev/bosatsu/MatchlessTests.scala
depends_on: []
estimated_size: M
generated_at: 2026-02-24T22:58:34Z
---

# Issue #1738 Design: Hoist Loop-Invariant Lets Out of Loop Bodies

_Issue: #1738 (https://github.com/johnynek/bosatsu/issues/1738)_

## Summary

Design doc content for #1738, proposing a Matchless-based loop-invariant let hoisting pass with safety/profitability heuristics, tests, acceptance criteria, risks, and rollout notes.

---
issue: 1738
priority: 2
touch_paths:
  - docs/design/1738-hoist-loop-invariant-lets-out-of-loop-bodies.md
  - core/src/main/scala/dev/bosatsu/Matchless.scala
  - core/src/test/scala/dev/bosatsu/MatchlessTests.scala
depends_on:
  - 1728
estimated_size: M
generated_at: 2026-02-24T00:00:00Z
---

# Issue #1738 Design: Hoist Loop-Invariant Lets Out of Loop Bodies

Issue: #1738 (https://github.com/johnynek/bosatsu/issues/1738)
Base branch: `main`
Status: proposed

## Summary

Implement a Matchless optimization that hoists loop-body `let` bindings out of canonical recursion `WhileExpr` loops when they are proven invariant, safe to move across mutable barriers, and expensive enough to justify hoisting.

## Context

`#1728` already lifts invariant loop arguments during typed normalization, but it does not move invariant `let` bindings that stay in the loop body.

Issue #1738 asks for this follow-up optimization. The issue thread adds a key constraint: implementation should likely live in Matchless, where we can optimize both typed-lowered loops and additional loops introduced during Matchless lowering.

## Goals

1. Hoist loop-body `let` bindings that are invariant across recur paths.
2. Preserve semantics in the presence of Matchless mutability (`LetMut`/`SetMut`/`LocalAnonMut`).
3. Apply clear profitability heuristics so cheap bindings are not hoisted.
4. Add positive and negative tests, including effects, escaping mutable refs, and shadowing guards.

## Non-goals

1. No broad global code-motion pass across arbitrary `WhileExpr` forms.
2. No change to `TypedExprNormalization` logic for this issue.
3. No attempt to hoist conditionally-executed lets that do not dominate the loop effect.
4. No rewrite of Matchless evaluation/runtime semantics.

## Current architecture

`Matchless.fromLet` lowers recursive typed loops into canonical Matchless loop scaffolding:

1. Mutable cells allocated by `letMutAll` for loop state.
2. Init assignments via `setAll`.
3. `Always(SetMut(cond, TrueExpr), WhileExpr(isTrueExpr(cond), effectExpr, resultMut))` for the tail-loop body.

Current post-lowering optimization in this path is `reuseConstructors`. There is no loop-invariant `let` hoist pass today.

## Proposed design

### 1) Add a dedicated pass in Matchless

Add a new private optimization pass in `Matchless.scala`:

- `hoistInvariantLoopLets(expr: Expr[A]): Expr[A]`

Update `fromLet` pipeline to run:

1. lower typed expr to Matchless expr
2. `hoistInvariantLoopLets`
3. `reuseConstructors`

### 2) Restrict to canonical recursion-loop shape

Only hoist in the canonical recursion-loop envelope:

- `Always(SetMut(condMut, TrueExpr), WhileExpr(isTrueExpr(condMut), effectExpr, resultMut))`

This keeps the pass in a shape where first iteration is guaranteed and avoids unsafe hoisting on generic search/backtracking loops.

### 3) Candidate extraction from dominating let-prefix

Inside eligible `WhileExpr.effectExpr`, collect only the contiguous leading immutable `Let` chain:

- `Let(b1, v1, Let(b2, v2, ... rest))`

Stop at the first non-hoistable binding.

Rationale: leading lets dominate all recur paths in the loop effect and are always executed on each iteration.

### 4) Safety predicate and mutable barrier checks

A candidate binding is hoistable only if all checks pass:

1. `value` is pure under Matchless semantics (`!hasSideEffect(value)`).
2. `value` has no mutable reads (`LocalAnonMut`) anywhere in the expression.
3. `value` contains no nested `WhileExpr` (redundant with side-effect check, but kept explicit for clarity).
4. Binding is used in the residual loop body.
5. Binder dependencies are preserved: if `v2` depends on `b1`, both are hoisted in original order.
6. Defensive shadowing guard: for `Right(name)` binders, loop condition must not reference `name`.

This is the explicit read/write barrier model requested in the issue discussion.

### 5) Profitability heuristic

Add a conservative heuristic to avoid noise:

1. Reject trivially cheap values (locals, literals, constructor constants/aliases).
2. Require a minimum expression weight (`exprWeight(value) >= HoistMinWeight`, initial value 4).

This follows issue guidance to hoist only when likely worthwhile.

### 6) Rewrite

For hoisted prefix `[(b1, v1), ..., (bk, vk)]` and residual effect `rest`:

- from: `WhileExpr(cond, Let(b1, v1, ... Let(bk, vk, rest)), result)`
- to: `Let(b1, v1, ... Let(bk, vk, WhileExpr(cond, rest, result)))`

Then rebuild the surrounding canonical `Always(SetMut(condMut, TrueExpr), ...)` envelope.

## Implementation plan

1. Add `hoistInvariantLoopLets` traversal and helper analyses to `core/src/main/scala/dev/bosatsu/Matchless.scala`.
2. Implement canonical-loop matcher for recursion loop scaffolding emitted by `buildLoopExpr`.
3. Implement candidate collection from leading loop-effect `Let` prefix.
4. Implement safety/profitability predicates and dependency-preserving rewrite.
5. Integrate pass into `fromLet` before `reuseConstructors`.
6. Add targeted tests in `core/src/test/scala/dev/bosatsu/MatchlessTests.scala`.

## Testing strategy

### Positive cases

1. Top-level invariant loop-body let (`z = f(100)`) hoists outside `WhileExpr`.
2. Multiple dependent invariant lets at loop top hoist together in order.
3. Hoisted value is outside loop effect (computed once, reused across iterations).

### Negative cases

1. Let inside `If` branch is not hoisted.
2. Let value reading `LocalAnonMut` is not hoisted.
3. Let value with side-effectful control (`WhileExpr` / `SetMut` path) is not hoisted.
4. Cheap alias/constant lets are not hoisted.
5. Defensive shadowing guard blocks unsafe hoist.
6. Lambda value that captures mutable state (escaping mutable ref) is not hoisted.

### Regression checks

1. Existing loop lowering tests remain green.
2. Existing `reuseConstructors` tests remain green with new pass ordering.

## Acceptance criteria

1. `Matchless.fromLet` runs a loop-invariant-let hoist pass before `reuseConstructors`.
2. Hoisting applies only to canonical recursion-loop scaffolding.
3. Only dominating leading loop-effect lets are considered.
4. Hoisted values satisfy purity and mutable-read-free checks.
5. Hoisted values satisfy profitability threshold; cheap bindings stay in-loop.
6. Rewrites preserve binder dependency order and avoid shadowing regressions.
7. New Matchless tests cover positive and negative scenarios above.
8. Existing Matchless/core suites pass without behavioral regression.

## Risks and mitigations

1. Risk: Unsound motion across mutable state.
Mitigation: strict mutable-read and side-effect gates; canonical-shape restriction.

2. Risk: Missed opportunities due conservative scope.
Mitigation: start narrow and safe; expand in follow-up once validated.

3. Risk: Heuristic threshold over/under-hoists.
Mitigation: keep threshold local and tunable; adjust using benchmark/test feedback.

4. Risk: Interaction with constructor CSE changes expected shape.
Mitigation: stable pass order and regression assertions for both passes.

## Rollout notes

1. Land pass + tests together in one PR.
2. Keep scope conservative and limited to canonical recursion loops.
3. If regressions appear, short-circuit pass entry while keeping tests and helpers for iteration.
4. Revisit broader loop-hoisting after stability in CI.

## Follow-up work (out of scope)

1. Extend beyond leading let-prefix to branch-merged invariants.
2. Support safe hoisting in additional `WhileExpr` families when zero-iteration behavior is preserved.
3. Reuse mutable barrier analysis for other Matchless code-motion optimizations.
