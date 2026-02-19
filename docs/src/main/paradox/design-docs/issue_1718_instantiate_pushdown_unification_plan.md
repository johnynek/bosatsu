# Issue 1718: Instantiate/Pushdown Unification Plan

## Context

Issue [#1718](https://github.com/johnynek/bosatsu/issues/1718) calls out two duplication points:

1. `TypedExpr.instantiateTo` vs `Type.instantiate`.
2. `pushDownCovariant` in both `Infer.instDataCon` and `TypedExpr.instantiateTo`.

The objective is the smallest refactor that reduces duplication while preserving:

1. existing typechecking behavior,
2. instantiation evidence,
3. soundness (no widen-only shortcuts where evidence exists).

This document is an implementation and testing plan only (no `src/main` changes in this PR).

## Current Behavior Snapshot

### Instantiation paths today

1. `Type.instantiate`:
1. structural matcher with explicit left vars, right existentials, and right-side env scope.
1. returns `Type.Instantiation(frees, subs, toFrees, toSubs)`.
1. used in inference codepaths that can validate kind-correct substitutions.

2. `TypedExpr.instantiateTo`:
1. local solver for `Generic` expression specialization to `Rho`.
1. contains local pushdown retry logic and expression-level wrapping/fallback.
1. falls back to annotation when it cannot solve.

### Pattern pushdown today

1. `Infer.instDataCon` has local `pushDownCovariant` to handle constructor-pattern sigma with `forall`.
2. `TypedExpr` has a similar local `pushDownCovariant` used by `instantiateTo`.

These are conceptually the same transform but implemented twice with slightly different shape.

## Coverage Baseline (Before This PR)

From `sbt ';clean;set ThisBuild / coverageEnabled := true;coreJVM/test;cli/test;coreJVM/coverageReport;cli/coverageReport'` on February 19, 2026:

1. `coreJVM`: statement `84.28%`, branch `81.50%`.
2. `cli`: statement `48.29%`, branch `39.53%`.

Relevant uncovered branches in `Infer`:

1. `typeCheckPattern` list-pattern `forall List` fast-path (`Infer.scala:2661`, `Infer.scala:2663`).
2. `instDataCon` local `pushDownCovariant` branches that re-wrap recursive `ForAll` results (`Infer.scala:2875`, `Infer.scala:2877`, `Infer.scala:2890`, `Infer.scala:2891`).

`TypedExpr.instantiateTo` and `Type.instantiate` are already at 100% method coverage, but call-site behavior still benefits from regression tests.

## Tests Added In This PR

Added to `RankNInferTest`:

1. `list patterns can consume forall-list scrutinees`:
1. targets `typeCheckPattern` `forall List` specialization fast-path.

2. `pattern instantiation pushes forall through mixed-variance constructor args`:
1. targets `instDataCon.pushDownCovariant` recursive `ForAll` wrapping branches.

3. `polymorphic match result can be instantiated at higher-order call sites`:
1. regression on polymorphic match results specialized at use sites (instantiate/coerce call graph behavior).

## Refactor Plan (Issue 1718)

### Phase 1: Unify covariant pushdown

1. Extract a shared type-level helper in `Type` (or closely related rankn module):
1. input: quantifier-bearing type plus a variance/kind lookup for applied heads.
1. output: transformed type with `forall` pushed through covariant positions conservatively.

2. Replace both current call sites with thin wrappers:
1. `Infer.instDataCon` wrapper only supplies constructor arg variance/kind context.
2. `TypedExpr.instantiateTo` wrapper supplies resolver from available kinds map.

3. Keep behavior conservative:
1. if variance info is missing, return input unchanged.
2. do not invent substitutions or quantifier elimination not already allowed.

### Phase 2: Delegate `TypedExpr.instantiateTo` to `Type.instantiate`

1. Rework `TypedExpr.instantiateTo` as orchestration only:
1. pre-step: split quantifiers from `gen.quantType`.
2. solver step: call `Type.instantiate`.
3. post-step: substitute expression types using returned `subs`.
4. quantifier rewrapping/pushGeneric/fallback remains local to `TypedExpr`.

2. Preserve evidence:
1. use `Instantiation.subs` as the primary witness for specialization.
2. preserve unsolved frees by rebuilding `Generic` wrappers when possible.
3. only use annotation fallback when solver cannot provide a coherent witness.

3. Keep existing fallback semantics:
1. if solve fails, keep `ann(gen, instTpe)` behavior.
2. avoid replacing evidence-driven coercions with widen-only coercions.

### Phase 3: Clean call graph and reduce duplication

1. Remove duplicated local solver code once all tests pass.
2. Remove duplicated local pushdown implementation.
3. Keep helper APIs small:
1. one shared pushdown helper,
2. one shared instantiate solver (`Type.instantiate`) consumed by both inference and typed-expression coercion.

## Testing Plan For Refactor

### Required gates

1. existing `coreJVM/test` and `cli/test` must remain green.
2. targeted coverage for issue-relevant methods must be 100% statement/branch:
1. `Type.instantiate`,
2. `TypedExpr.instantiateTo`,
3. shared pushdown helper (new),
4. `Infer.typeCheckPattern` list `forall List` branch,
5. constructor-pattern pushdown branches previously uncovered.

### Additional regression matrix

1. pattern checks with:
1. mixed variance constructors,
2. `forall` scrutinee types,
3. list patterns under `forall`.

2. specialization/coercion with:
1. polymorphic match branches,
2. higher-order call sites expecting monomorphic function types,
3. existential-heavy argument/application paths where apply fallback currently exists.

3. soundness checks:
1. keep existing ill-typed programs ill-typed (especially unsound variance/existential cases),
2. ensure no new widening-only acceptance appears where evidence should be required.

## Minimal-Change Sequencing

1. land shared pushdown helper + call-site rewires first (no instantiate unification yet).
2. land `instantiateTo` delegation to `Type.instantiate` second.
3. remove dead duplicated code last.
4. after each step: run focused test subset + full `coreJVM/test`.

This order minimizes blast radius while keeping bisectable behavioral changes.
