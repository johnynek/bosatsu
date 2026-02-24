---
issue: 1769
priority: 3
touch_paths:
  - docs/design/1769-design-refactoring-totalitychecker-to-run-on-typedexpr-declaration.md
  - core/src/main/scala/dev/bosatsu/TotalityCheck.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/Inhabitedness.scala
  - core/src/test/scala/dev/bosatsu/TypedTotalityTest.scala
  - core/src/test/scala/dev/bosatsu/WellTypedGen.scala
  - core/src/test/scala/dev/bosatsu/TotalityTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/TestUtils.scala
depends_on: []
estimated_size: M
generated_at: 2026-02-24T00:32:34Z
---

# Issue 1769 Design: Refactor TotalityCheck to TypedExpr[Declaration]

_Issue: #1769 (https://github.com/johnynek/bosatsu/issues/1769)_

## Summary

Moves totality checking to typed expressions, uses scrutinee-type and inhabitedness data from #1729 to prune impossible coverage/unreachable branches conservatively, and adds a typed test strategy with WellTypedGen-backed generators.

---
issue: 1769
title: Design refactoring TotalityCheck to run on TypedExpr[Declaration]
status: proposed
---

# Design: Refactor TotalityCheck to run on TypedExpr[Declaration]

## Context

Today `TotalityCheck` runs on `Expr[Declaration]` in `Package.inferBodyUnopt` before rank-n inference. As a result it does not have the inferred scrutinee type at each match site, and it cannot use `Inhabitedness.check` / `Inhabitedness.checkMatch` from #1729 to prune impossible coverage.

`Infer.typeCheckPattern` already builds well-typed match patterns in `TypedExpr.Match`, so totality can run after inference on typed trees.

## Goals

1. Run totality on `TypedExpr[Declaration]`.
2. Use `arg.getType` at each match.
3. Treat branches proven `Uninhabited` by #1729 as unreachable.
4. Allow missing branches to be omitted when they are proven unmatchable.
5. Keep conservative behavior for `Unknown`.
6. Keep guard semantics unchanged.
7. Expand tests to use well-typed generated patterns and environments.

## Non-goals

1. Rewriting pattern set algebra (`SetOps`) in this issue.
2. Solving all `Unknown` inhabitedness cases.
3. Changing parser/source-converter pattern syntax.
4. Reworking diagnostics unrelated to totality.

## Answers to issue questions

### Can TotalityCheck be simplified once patterns are fully typed?

Yes.

- Constructor existence and constructor arity validation are already guaranteed by `Infer.typeCheckPattern`; totality no longer needs to be the primary owner of those checks.
- Totality can focus on coverage and reachability over typed patterns.
- Keep only structural lint that inference does not currently enforce (adjacent string globs, multi-splice list patterns), so user-facing diagnostics remain stable.

### Does #1729 have enough information to identify unmatchable branches?

Yes, for definitive cases.

- `Inhabitedness.check(scrutineeType, env)` identifies fully uninhabited scrutinee domains.
- `Inhabitedness.checkMatch(scrutineeType, pattern, env)` identifies definitely unmatchable branches.
- Any `Unknown` or validation failure must stay conservative and be treated as potentially matchable.

## Proposed architecture

### 1) Typed totality traversal

Refactor `TotalityCheck.checkExpr` to traverse `TypedExpr[A]` and handle `TypedExpr.Match`.
`ExprError` payloads should point at `TypedExpr.Match[A]` (or equivalent match metadata with `tag`, `branches`, and `arg`).

Traversal remains recursive over annotations, lambdas, apps, lets, loops, recur, guards, and branch bodies.

### 2) Match analysis algorithm

For each `TypedExpr.Match(arg, branches, tag)`:

1. Compute `scrutineeType = arg.getType`.
2. Compute `scrutineeState = Inhabitedness.check(scrutineeType, env)`.
3. For each branch pattern, compute `branchState = Inhabitedness.checkMatch(scrutineeType, pattern, env)`.
4. Classify branch state:
- `Uninhabited`: definitely impossible.
- `Inhabited`: definitely possible.
- `Unknown` or check error: conservatively possible.
5. Coverage set for missing-branch analysis:
- only unguarded branches
- excluding definitely impossible branches.
6. Missing set:
- if `scrutineeState == Uninhabited`, missing is empty (vacuously total).
- otherwise compute with existing set algebra: `missingBranches(_ :: Nil, coverage)`.
- then drop any missing pattern `p` where `checkMatch(scrutineeType, p, env) == Uninhabited`.
7. Unreachable set:
- branch is unreachable if it is definitely impossible, or
- it is shadowed by prior unguarded branches that are not definitely impossible.

This keeps current ordered semantics and adds typed impossibility pruning.

### 3) Validation split

- Keep structural pattern validation in totality for now (`InvalidStrPat`, `MultipleSplicesInPattern`).
- Move constructor/arity invalid-pattern reporting out of normal totality flow (inference already enforces it).
- If defensive handling is retained internally, it should not change user-visible behavior for well-typed programs.

### 4) Pipeline change in Package

In `Package.inferBodyUnopt`:

1. Keep pre-inference checks (`DefRecursionCheck`, `UnusedLetCheck`) as-is.
2. Run inference to obtain `TypedExpr[Declaration]`.
3. Run typed totality on inferred lets.
4. Map totality errors to `PackageError.TotalityCheckError`.
5. Preserve error accumulation semantics for independent checks; if inference fails, typed totality is skipped.

## Implementation plan

1. `core/src/main/scala/dev/bosatsu/TotalityCheck.scala`
- Add typed traversal and match analyzer.
- Update `ExprError`/`NonTotalMatch`/`UnreachableBranches` to typed match payloads.
- Integrate inhabitedness-based branch classification.
- Keep existing set-ops core (`difference`, `intersection`, `missingBranches`).

2. `core/src/main/scala/dev/bosatsu/Package.scala`
- Remove pre-inference call to untyped totality.
- Insert typed totality pass after successful inference.

3. `core/src/main/scala/dev/bosatsu/PackageError.scala`
- Update references from `Expr.Match` to `TypedExpr.Match` in totality error rendering.
- Keep message text stable.

4. `core/src/test/scala/dev/bosatsu/TypedTotalityTest.scala` (new)
- Add deterministic typed match regressions for impossible branches, guards, and unreachable detection.

5. `core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala`
- Update/extend tests so totality diagnostics still render correctly with typed match payloads.

6. `core/src/test/scala/dev/bosatsu/WellTypedGen.scala`
- Add helpers to produce totality samples:
  - generated `TypeEnv`
  - selected scrutinee type from that env
  - typed pattern sets/branches (including optional impossible branches).

7. `core/src/test/scala/dev/bosatsu/TotalityTest.scala`
- Keep set-algebra law tests.
- Add properties that consume well-typed samples instead of mixed untyped pattern domains.

8. `core/src/test/scala/dev/bosatsu/TestUtils.scala` (if needed)
- Add helper(s) to compile source and extract typed lets/matches for tests.

## Testing strategy

### Deterministic regression tests

1. Missing impossible constructor branch is not required for totality.
2. Explicit impossible branch is reported in `UnreachableBranches`.
3. Guarded branch does not count toward coverage (unchanged).
4. Uninhabited scrutinee type yields vacuous totality.
5. Existing string/list structural invalid-pattern errors are unchanged.

### Property tests with well-typed generation

Current `TotalityTest` random patterns are not guaranteed to be well-typed.
Add typed coverage generation using `WellTypedGen`:

1. Generate statements and derived `TypeEnv`.
2. Pick scrutinee types from generated env.
3. Generate branch patterns constrained by scrutinee type.
4. Classify patterns with inhabitedness and inject both reachable and impossible cases.
5. Check properties:
- adding reported missing branches makes coverage total;
- impossible branches are excluded from missing requirements;
- `Unknown` never causes pruning.

## Acceptance criteria

1. `TotalityCheck` runs on `TypedExpr[Declaration]` in package compilation.
2. Every match check uses `arg.getType`.
3. Branches proven `Uninhabited` are reported unreachable.
4. Missing branches proven `Uninhabited` are not required.
5. `Unknown` states remain conservative.
6. Guard behavior is unchanged.
7. Existing structural invalid-pattern diagnostics remain unchanged.
8. New typed totality deterministic tests pass.
9. New well-typed generator-based properties pass.
10. Existing totality/error-message/evaluation test suites continue to pass.

## Risks and mitigations

1. Risk: behavior changes when inference fails (typed totality cannot run).
- Mitigation: keep pre-inference checks independent and accumulated.

2. Risk: performance overhead from repeated `checkMatch`.
- Mitigation: memoize per-match `(scrutineeType, pattern.unbind)` state.

3. Risk: false pruning from uncertain inhabitedness.
- Mitigation: prune only on definitive `Uninhabited`; treat `Unknown` and check errors as matchable.

4. Risk: generator instability/flaky property tests.
- Mitigation: bound depth/branch count; keep deterministic regressions as primary correctness guard.

## Rollout notes

1. Phase 1: add typed totality API and tests while leaving old path available behind internal compatibility.
2. Phase 2: switch `Package` to typed totality pass.
3. Phase 3: delete obsolete untyped totality traversal.
4. Phase 4: optional follow-up to move remaining structural pattern lint fully into inference.

## Out-of-scope follow-ups

1. Rich per-pattern region tracking (#132).
2. Full `Unknown` reduction in inhabitedness.
3. Large rewrite of pattern set algebra.
