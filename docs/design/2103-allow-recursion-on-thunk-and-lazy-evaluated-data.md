---
issue: 2103
priority: 3
touch_paths:
  - docs/design/2103-allow-recursion-on-thunk-and-lazy-evaluated-data.md
  - core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala
  - core/src/test/scala/dev/bosatsu/TypedExprRecursionCheckTest.scala
  - docs/src/main/paradox/recursion.md
depends_on: []
estimated_size: M
generated_at: 2026-03-10T02:26:08Z
---

# Issue #2103 Design: Allow recursion on thunk and lazy evaluated data

_Issue: #2103 (https://github.com/johnynek/bosatsu/issues/2103)_

## Summary

Design doc content for adding two narrow structural-decrease recognizers in `TypedExprRecursionCheck` (`thunk()` force and trusted `Bosatsu/Lazy.get_Lazy` force) while keeping lexicographic and Int recursion logic unchanged.

---
issue: 2103
priority: 3
touch_paths:
  - docs/design/2103-allow-recursion-on-thunk-and-lazy-evaluated-data.md
  - core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala
  - core/src/test/scala/dev/bosatsu/TypedExprRecursionCheckTest.scala
  - docs/src/main/paradox/recursion.md
depends_on: []
estimated_size: S
generated_at: 2026-03-10T00:00:00Z
---

# Issue #2103 Design: Allow recursion on thunk and lazy evaluated data

_Issue: #2103 (https://github.com/johnynek/bosatsu/issues/2103)_

Status: proposed  
Base branch: `main`

## Summary

Extend `TypedExprRecursionCheck.classifyStructuralArg` with two additional, narrow recognizers that produce `Smaller`:

1. `thunk()` force on branch-proven smaller local thunk of type `() -> T`.
2. `get_Lazy(l)` force on branch-proven smaller local `l: Lazy[T]` when callee resolves to trusted `Bosatsu/Lazy.get_Lazy`.

No other recursion rules change: tuple lexicographic ordering, constructor-rank decreases, and SMT-backed `Int` checks remain as-is.

## Problem statement

Current structural recursion accepts:

1. local names proven smaller by branch patterns,
2. equal aliases/singletons,
3. constructor-rank decreases (issue #2025),
4. dedicated SMT logic for `Int`.

This still rejects safe recursive-argument forms when the decrease witness is already proven smaller but wrapped in force operations:

1. `f(th())` where `th` is a smaller local thunk producing the recur target type,
2. `f(get_Lazy(l))` where `l` is a smaller local `Lazy` of the recur target type.

Issue #2103 requests these cases only, without broad wrapper unwrapping.

## Goals

1. Accept recursive args `th()` as `Smaller` when `th` is a branch-proven smaller local thunk of type `() -> T`.
2. Accept recursive args `get_Lazy(l)` as `Smaller` when `l` is branch-proven smaller, `l: Lazy[T]`, and `get_Lazy` is the trusted global symbol.
3. Preserve existing lexicographic ordering logic unchanged.
4. Preserve existing `Int` recursion SMT logic unchanged.
5. Keep this as a narrow exception, not generalized wrapper unrolling.

## Non-goals

1. No generic acceptance of arbitrary `Unit`-typed calls.
2. No generic acceptance of arbitrary `Lazy[T] -> T` functions.
3. No parser/type-system/runtime changes.
4. No modifications to constructor-rank logic introduced by #2025.

## Safety rationale

### Thunk force (`th()`)

1. Bosatsu already treats `f()` as sugar for `f(())` with type `Unit -> a`.
2. Under Bosatsu’s pure/total assumptions, forcing a thunk is a projection from `() -> T` to `T`.
3. Rule applies only when thunk local is already in branch-proven smaller names, so no new witness is invented.
4. Restricting to canonical unit-call shape avoids broad reasoning over arbitrary `Unit` expressions.

### Lazy force (`get_Lazy(l)`)

1. `Bosatsu/Lazy` models delayed computation as thunk + force.
2. JVM/Python/C implementations force by calling thunk with unit and memoizing successful results.
3. Memoization changes cost behavior, not decrease semantics.
4. Rule applies only when `l` is already branch-proven smaller.
5. Trust boundary is strict: only resolved global `Bosatsu/Lazy.get_Lazy` qualifies.

### Safety boundary

This remains inside existing trusted-core/trusted-runtime assumptions and does not relax conservatism for wrapped recursive payloads (`List[T]`, `Option[T]`, etc.).

## Proposed behavior (formalized)

For non-`Int` recur target component type `T`, classify recursive arg as `Smaller` when existing rules match, or when either new rule matches:

1. Thunk-force rule:
   1. Arg has canonical force shape `th(())` (source `th()` lowers to this shape).
   2. `th` resolves to a local name `n`.
   3. `n` is in this branch’s `allowed` set for that target component.
   4. `th` has type `() -> T`.

2. Lazy-force rule:
   1. Arg has shape `get_Lazy(l)` with exactly one argument.
   2. Callee resolves to global `(PackageName.parts("Bosatsu", "Lazy"), Identifier.Name("get_Lazy"))`.
   3. `l` resolves to local name `n`.
   4. `n` is in this branch’s `allowed` set.
   5. `l` has type `Lazy[T]`.

If neither applies, continue current logic (singleton equality, constructor-rank, then `Other`) unchanged.

## Architecture and implementation plan

Primary file: `core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala`

1. Add trusted symbol/type constants for lazy recognition:
   1. `lazyPackageName = PackageName.parts("Bosatsu", "Lazy")`
   2. `getLazyName = Identifier.Name("get_Lazy")`
   3. `lazyTypeConst = Type.Const.Defined(lazyPackageName, TypeName("Lazy"))`

2. Add expression-shape helpers:
   1. canonical `Unit` literal detector for call argument,
   2. one-arg call decomposition helper,
   3. trusted-global callee check helper.

3. Add thunk-force recognizer helper:
   1. match `App(fn, [unit])`,
   2. require `localNameOf(fn)` in `allowed`,
   3. require `fn.getType` is `Unit -> targetType`.

4. Add lazy-force recognizer helper:
   1. match `App(Global(lazyPackageName, getLazyName), [arg0])`,
   2. require `localNameOf(arg0)` in `allowed`,
   3. require `arg0.getType` is `Lazy[targetType]`.

5. Integrate into `classifyStructuralArg` as additional `Smaller` checks on the structural path.
   1. Keep existing local/equal/singleton precedence unchanged.
   2. Keep constructor-rank fallback unchanged for non-force cases.

6. Keep lexicographic control flow (`recurAllowedByLexOrder`) unchanged.
7. Keep `Int` recursion (`classifyIntArg` and SMT obligations) unchanged.

## Detailed testing plan

Primary file: `core/src/test/scala/dev/bosatsu/TypedExprRecursionCheckTest.scala`

1. Positive thunk case: direct recursion with `case Foo(th): f(th())`.
2. Positive tuple lexicographic case: earlier target equal, later target decreases via `th()`.
3. Negative thunk cases:
   1. non-local force helper (`force(th)`) should not match,
   2. wrong callee type (not `() -> T`) should not match,
   3. non-canonical unit argument shape should not match.
4. Positive lazy case: `case Foo(l): f(get_Lazy(l))` with `l: Lazy[T]`.
5. Negative lazy case: local/user `get_Lazy` function must not trigger trusted rule.
6. Regression: existing structural, constructor-rank, tuple lexicographic, and `Int` recursion tests remain green.

Documentation update (if included in the PR): add a brief recursion guide note for these two narrow admissible force forms.

## Acceptance criteria

1. `th()` recursive args are accepted as `Smaller` only when thunk local is branch-proven smaller and typed `() -> T`.
2. `get_Lazy(l)` recursive args are accepted as `Smaller` only when:
   1. `l` is branch-proven smaller local,
   2. `l: Lazy[T]`,
   3. callee is trusted `Bosatsu/Lazy.get_Lazy`.
3. Local/user-defined `get_Lazy` does not trigger the rule.
4. Non-canonical thunk force shapes do not trigger the thunk rule.
5. Lexicographic semantics remain unchanged (first differing component must be `Smaller`).
6. Constructor-rank behavior remains unchanged.
7. `Int` recursion SMT behavior and diagnostics remain unchanged.
8. New issue-focused positive/negative tests are added and pass.
9. Existing recursion suites remain green.
10. Recursion docs reflect the new narrow exceptions if docs path is touched.

## Risks and mitigations

1. Risk: over-matching `Unit` expressions broadens acceptance.
   Mitigation: accept only canonical unit literal force shape.

2. Risk: trust-boundary bypass by same-name function.
   Mitigation: require exact resolved global symbol for `Bosatsu/Lazy.get_Lazy`.

3. Risk: generic type-shape false positives.
   Mitigation: strict type checks (`() -> T`, `Lazy[T]`) with conservative fallback to `Other`.

4. Risk: structural-classification ordering regressions.
   Mitigation: preserve existing order and add only narrow checks.

5. Risk: confusion about rejected wrapped payloads.
   Mitigation: document explicit non-goal that wrapper unrolling is out of scope.

## Rollout notes

1. Compile-time checker change only; runtime/codegen unchanged.
2. Land as a single PR with tests and docs updates.
3. No migration or feature flag required.
4. Rollback path is straightforward: remove the two new recognizers from `classifyStructuralArg`.

## Decision

Proceed with a narrow, explicit recursion-check extension for canonical thunk-force and trusted lazy-force on already-proven-smaller locals, while keeping all existing recursion frameworks unchanged.
