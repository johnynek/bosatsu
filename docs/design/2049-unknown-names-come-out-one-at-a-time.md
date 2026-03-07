---
issue: 2049
priority: 3
touch_paths:
  - docs/design/2049-unknown-names-come-out-one-at-a-time.md
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/rankn/NameCheck.scala
  - core/src/main/scala/dev/bosatsu/rankn/Infer.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/rankn/NameCheckTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-07T00:18:58Z
---

# Issue #2049 Design: unknown names come out one at a time

_Issue: #2049 (https://github.com/johnynek/bosatsu/issues/2049)_

## Summary

Design for adding a pre-inference NameCheck phase that aggregates all unknown value names in one run and still type-checks a maximal unaffected subdag for additional diagnostics.

---
issue: 2049
priority: 2
touch_paths:
  - docs/design/2049-unknown-names-come-out-one-at-a-time.md
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/rankn/NameCheck.scala
  - core/src/main/scala/dev/bosatsu/rankn/Infer.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/rankn/NameCheckTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-07T00:00:00Z
---

# Issue #2049 Design: unknown names come out one at a time

_Issue: #2049 (https://github.com/johnynek/bosatsu/issues/2049)_

Status: proposed  
Base branch: `main`

## Summary

Add a dedicated `NameCheck` phase before `Infer.typeCheckLets` that:

1. collects all unresolved value-name errors in a package in a single pass,
2. returns a maximal name-safe let subgraph for follow-on type checking,
3. preserves existing user-facing unknown-name formatting by reusing `Infer.Error.VarNotInScope` and `PackageError.TypeErrorIn`.

This fixes the current "one unknown at a time" workflow while still allowing type errors from unaffected lets to be discovered in the same compiler run.

## Problem statement

Today, unknown value names are discovered inside type inference (`Infer.lookupVarType`). In practice this creates serial feedback:

1. one unknown name is reported,
2. compilation stops before enough downstream work runs,
3. user fixes one name and reruns,
4. next unknown appears.

The root cause is architectural:

1. `Infer` is monadic and short-circuits on `Left` for sequential paths.
2. `typeCheckLets` in `Infer.scala` checks let groups in order, extending env only after a group succeeds.
3. A failing group blocks type checking of later groups, so many name errors are never reached in that run.

## Goals

1. Report all unresolved value names for a package in one compiler invocation.
2. Keep current unknown-name diagnostics style (suggestions, occurrence counts, use-before-def hinting).
3. Avoid cascade noise from dependent lets when an earlier let has a name error.
4. Type-check the largest unaffected subset so independent type errors can still surface in the same run.
5. Preserve existing semantics for successful programs.

## Non-goals

1. Rewriting the core inference algorithm from `Monad` to full validation-style accumulation.
2. Changing language semantics around declaration ordering or recursive definitions.
3. Solving constructor/type-name unknowns already handled in `SourceConverter`.
4. Producing a fully typed `Program` when any top-level lets in that package still have name errors.

## Proposed architecture

### 1) Add pre-inference `NameCheck` phase

Create `core/src/main/scala/dev/bosatsu/rankn/NameCheck.scala`.

Proposed surface API:

- `checkLets(pack, lets, initialScope): Ior[NonEmptyChain[Infer.Error.NameError], NameCheck.Result]`
- `NameCheck.Result` contains:
  - `typecheckLets`: lets eligible for inference,
  - `nameErrorLets`: bindables whose RHS has direct name errors,
  - optional debug metadata (dependency map / blocked set) for tests.

`initialScope` is the same global map shape used to initialize infer env (`Map[(Option[PackageName], Identifier), Type]`) so suggestion quality remains aligned with current behavior.

### 2) Name-checking traversal rules

Traverse each let RHS as `Expr[Declaration]`, tracking lexical binders.

Rules:

1. `Expr.Local(name, tag)`:
   - if `name` not lexically bound, emit `Infer.Error.VarNotInScope((None, name), scopeAtSite, tag.region)`.
2. `Expr.Global(pack, name, tag)`:
   - if `(Some(pack), name)` not available in current global scope, emit `VarNotInScope((Some(pack), name), scopeAtSite, tag.region)`.
3. `Lambda`, `Let`, and `Match` pattern names extend lexical scope as in existing binding semantics.
4. For local `let rec`, include binder while checking RHS, matching `RecursionKind.Recursive` behavior.

Important anti-cascade rule:

- Even if a let body has name errors, its binder is still added to the subsequent top-level name scope for name-check purposes.
- This prevents secondary "unknown name `<earlier binder>`" noise in later lets.

### 3) Build maximal name-safe subdag for type checking

After per-let name checking:

1. Build top-level dependency edges using the same signal already used in `Infer.typeCheckLets`:
   - `expr.globals` filtered to `Expr.Global(pack, bindable, _)` for current package.
2. Root blocked set = lets with direct name errors.
3. Expand blocked set transitively across reverse dependency edges (dependents of blocked lets).
4. `typecheckLets` = original lets minus blocked set, preserving source order.

Why source-order, not topological reorder:

1. keeps existing use-before-def behavior unchanged,
2. minimizes semantic drift,
3. still achieves "smallest correct subdag" objective from the issue.

### 4) Integrate in `Package.inferBodyUnopt`

In `core/src/main/scala/dev/bosatsu/Package.scala`, after `withFQN`/type-env setup and before `Infer.typeCheckLets`:

1. Run `NameCheck.checkLets`.
2. If `typecheckLets` non-empty, run `Infer.typeCheckLets` on that subset.
3. Combine errors:
   - fold all name errors into `Infer.Error` (`Combine` chain),
   - if subset type checking also fails, combine name and type errors.
4. Emit as existing `PackageError.TypeErrorIn(...)` so rendering path stays unchanged.
5. If any name errors exist, return `Left` (no typed program output for that package), even if subset inference succeeded.
6. If no name errors exist, retain current full pipeline unchanged (recursion check, shadowed binding check, totality check).

### 5) Error rendering strategy

No new user-facing error type is required for v1.

By emitting `Infer.Error.VarNotInScope` entries, existing `PackageError.TypeErrorIn` behavior is reused:

1. same "Unknown name `...`" format,
2. same nearest-name suggestions,
3. same occurrence aggregation,
4. same use-before-def rewrite when applicable.

## Detailed implementation plan

1. Add `NameCheck.scala` with:
   - expression walker,
   - scope model,
   - dependency graph extraction,
   - blocked-subgraph pruning,
   - `Ior` result construction.
2. Add utility in `NameCheck` for combining `NonEmptyChain[Infer.Error.Single]` into `Infer.Error` (local helper or small shared helper in `Infer.scala` if cleaner).
3. Wire `Package.inferBodyUnopt` to call `NameCheck` before inference.
4. Route `typeCheckLets` input to `nameCheckResult.typecheckLets`.
5. Merge name/type errors into a single `PackageError.TypeErrorIn` payload.
6. Ensure existing post-inference checks run only when full inference succeeded with no name errors.
7. Add tests for message content and subdag pruning behavior.
8. Update any test expectations impacted by richer multi-error output ordering.

## Testing plan

### `core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala`

Add regression tests:

1. Multiple distinct unknown names across multiple lets are reported in one run.
2. Repeated unknown name occurrences still aggregate counts.
3. Use-before-def message still appears with same guidance text.
4. Name errors and independent type errors can both appear in one run when type error is in unaffected subdag.
5. Dependent lets are not reported as additional unknown-name cascades when upstream binder is syntactically present but semantically blocked.

### `core/src/test/scala/dev/bosatsu/rankn/NameCheckTest.scala`

Add focused unit tests:

1. Lexical scoping in lambda/let/match is respected.
2. Recursive local let behavior matches `RecursionKind`.
3. Blocked-set transitive dependency pruning is correct.
4. Source order is preserved for `typecheckLets` output.

### Full-suite validation

1. Existing inference and diagnostics suites remain green.
2. No behavior change for packages without name errors.

## Acceptance criteria

1. A package with N unresolved value names reports all N in one compile run.
2. Unknown-name diagnostics keep current wording and suggestion quality.
3. Use-before-def diagnostics continue to be recognized and rendered as before.
4. Dependent cascade noise is reduced: downstream lets are blocked, not misreported as new unknown-name roots.
5. Independent type errors in unblocked lets can appear in the same run as name errors.
6. Packages with no name errors follow the previous inference path and produce identical typed results.
7. New regression tests for multi-name reporting and subdag pruning are present and passing.
8. Existing test suites pass without introducing nondeterministic error ordering.

## Risks and mitigations

1. Risk: NameCheck scoping diverges from infer scoping.
   Mitigation: Mirror existing lexical rules and add targeted tests for lambda/let/match and recursion.

2. Risk: Error ordering changes break brittle message assertions.
   Mitigation: Add deterministic ordering rules (source-order traversal) and update tests to assert key content, not incidental spacing/order where appropriate.

3. Risk: Additional compiler pass increases compile time.
   Mitigation: Keep NameCheck linear in AST size plus lightweight graph operations on top-level lets; avoid repeated traversals.

4. Risk: Over-pruning hides useful type errors.
   Mitigation: Only prune transitive dependents of lets with direct name errors; preserve maximal remaining source-ordered subdag.

## Rollout notes

1. Ship as a single PR with no feature flag (diagnostic improvement, no syntax/runtime change).
2. Expect snapshot/message test updates due increased error cardinality per run.
3. Validate against a real multi-error refactor scenario (like issue example) before merge.
4. If regressions appear in error quality, fallback path is to keep NameCheck collection but disable subset typecheck in the same PR as a minimal safe variant.
