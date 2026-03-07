---
issue: 2049
priority: 2
touch_paths:
  - docs/design/2049-unknown-names-come-out-one-at-a-time.md
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/rankn/NameCheck.scala
  - core/src/main/scala/dev/bosatsu/PackageCustoms.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
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

Add a pre-inference Expr-DAG diagnostic stage that runs before `Infer.typeCheckLets` and returns an `Ior`:

1. Left: all value-name errors found across lets (`VarNotInScope`) plus partial import/export bindable errors.
2. Right: the maximal name-safe subdag of lets that can still be typechecked for additional diagnostics.

This addresses one-at-a-time unknown names and also allows import/export bindable checks to surface even when typechecking fails.

## Problem statement

Unknown names are currently discovered mostly in type inference (`Infer.lookupVarType`), which means compilation usually short-circuits before reporting more than one root issue in a package.

The same sequencing problem affects import/export diagnostics that depend on typed lets:

1. export validation currently runs from `PackageCustoms.assemble` using typed lets,
2. import usage checks run in `PackageCustoms` over typed expressions,
3. if typechecking fails early, these checks do not run, even when part of them is checkable from the untyped Expr DAG.

Result: users fix one unknown, rerun, then hit additional unknowns or import/export bindable issues in multiple cycles.

## Goals

1. Report all unresolved value names for a package in one run.
2. Report export/import bindable issues that can be computed from Expr DAG, even if typechecking fails.
3. Preserve current unknown-name formatting and suggestion behavior.
4. Keep typechecking on maximal unaffected lets to expose independent type errors in the same run.
5. Preserve semantics and outputs for successful packages.

## Non-goals

1. Rewriting inference internals from monadic short-circuiting to full validation accumulation.
2. Reordering declarations in a way that changes use-before-def behavior.
3. Replacing typed import/export checks that require inferred types (those stay post-typecheck).
4. Producing a fully typed `Program` when any top-level let in that package has unresolved names.

## Proposed architecture

### 1) Add `NameCheck` over untyped lets

Create `core/src/main/scala/dev/bosatsu/rankn/NameCheck.scala`.

Proposed API shape:

- `checkLets(pack, lets, initialScope): Ior[NonEmptyChain[Infer.Error.NameError], NameCheck.Result]`
- `NameCheck.Result` includes:
  - `typecheckLets`: source-ordered let subset safe for inference,
  - `nameErrorLets`: lets with direct name errors,
  - dependency metadata used by tests.

Traversal details:

1. Track lexical binders for `Lambda`, local `Let`, and `Match` patterns.
2. For unresolved `Expr.Local`/`Expr.Global`, emit `Infer.Error.VarNotInScope` with use-site region.
3. Keep anti-cascade behavior: add each top-level binder to subsequent top-level name scope even if its RHS had errors.

### 2) Build maximal typecheckable subdag

After name checking:

1. Build same-package dependency edges from `expr.globals`.
2. Mark lets with direct name errors as blocked roots.
3. Transitively block dependents.
4. Keep surviving lets in original source order as `typecheckLets`.

This preserves existing use-before-def semantics while still providing a "smallest correct subdag" for follow-on checks.

### 3) Add partial import/export bindable checks on Expr DAG

Extend `PackageCustoms` with an untyped precheck (name tentative: `checkExprDagBindables`) that runs before type inference.

Scope of this precheck:

1. Exports:
   - validate `ExportedName.Binding` targets exist among top-level let binders / externals visible in the package body.
   - emit `PackageError.UnknownExport` (or a lightweight equivalent) without requiring typed lets.
2. Imports:
   - compute imported bindable references used by Expr globals in lets,
   - report unused imported bindables detectable without types.

Typed portions remain in existing post-typecheck checks:

1. private type escape checks,
2. type-driven import usage/constructor-type ambiguity checks,
3. full export referant resolution.

### 4) Integrate in `Package.inferBodyUnopt`

In `core/src/main/scala/dev/bosatsu/Package.scala`:

1. Run `NameCheck.checkLets`.
2. Run `PackageCustoms.checkExprDagBindables` on the same untyped body/import/export context.
3. Typecheck only `nameCheckResult.typecheckLets`.
4. Combine diagnostics from:
   - NameCheck errors,
   - Expr-DAG import/export bindable errors,
   - typecheck errors from the surviving subdag.
5. If NameCheck has any errors, return `Left` (no typed program output), but include all combined diagnostics.
6. If no NameCheck errors, preserve existing recursion/shadow/totality and customs flow.

### 5) Error rendering strategy

Prefer reusing existing error render paths to minimize churn:

1. Name errors remain `Infer.Error.VarNotInScope` rendered via `PackageError.TypeErrorIn`.
2. Export/import bindable precheck should reuse current `PackageError` constructors where possible.
3. Maintain deterministic source-ordered emission for stable tests.

## Detailed implementation plan

1. Add `NameCheck.scala` with lexical walk + dependency pruning + `Ior` result.
2. Add helper to combine `NonEmptyChain[Infer.Error.Single]` into one `Infer.Error` tree.
3. Add `PackageCustoms.checkExprDagBindables` for untyped bindable-level import/export checks.
4. Update `Package.inferBodyUnopt` to run prechecks and aggregate their errors with inference errors.
5. Keep existing typed customs checks; run them when type inference succeeds.
6. If needed for export suggestions, adapt `PackageError.UnknownExport` to accept untyped candidate binder regions.
7. Add/extend tests for multi-unknown reporting and pre-typecheck import/export bindable diagnostics.

## Testing plan

### `core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala`

Add regression tests for:

1. multiple unknown names reported in one run,
2. unknown-name aggregation count behavior unchanged,
3. use-before-def message behavior unchanged,
4. export unknown bindable error appears even when a separate let has type/name errors,
5. unused imported bindable (Expr-detectable case) appears even when typecheck fails elsewhere.

### `core/src/test/scala/dev/bosatsu/rankn/NameCheckTest.scala`

Add unit tests for:

1. lexical scoping correctness,
2. recursive local let behavior,
3. dependency blocking closure,
4. source-order stability of surviving lets.

### Full-suite validation

1. Existing diagnostics/inference suites remain green.
2. Successful packages keep identical behavior.

## Acceptance criteria

1. A package with N unresolved names reports all N in one compile run.
2. Unknown-name wording/suggestions remain consistent with current formatting.
3. Export unknown bindables are reported even if typechecking fails earlier in the package.
4. Expr-detectable unused imported bindables are reported even if typechecking fails earlier.
5. Independent type errors from unblocked lets can appear in the same run as name/import/export bindable errors.
6. No semantic change for packages that previously compiled successfully.
7. Added regression tests pass, and existing suite remains green.

## Risks and mitigations

1. Risk: precheck logic drifts from infer/customs semantics.
   Mitigation: restrict precheck to bindable-only facts derivable from Expr DAG, keep typed checks unchanged.

2. Risk: duplicate diagnostics (same issue from precheck and typed check).
   Mitigation: gate typed customs checks when equivalent precheck errors already emitted, or dedupe by key.

3. Risk: ordering instability in combined errors.
   Mitigation: preserve source-order traversal and deterministic merge order.

4. Risk: additional pass cost.
   Mitigation: keep algorithms linear in expression size + simple dependency graph traversal.

## Rollout notes

1. Land as a single diagnostics-focused PR.
2. Expect snapshot changes where more errors appear per run.
3. Validate with a multi-error refactor case and a failing-typecheck + bad-export/import case before merge.
4. If error quality regresses, keep NameCheck aggregation and temporarily narrow precheck scope to exports-only as a fallback.
