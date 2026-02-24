---
issue: 1770
priority: 3
touch_paths:
  - docs/design/1770-design-post-typechecking-recursion-checker.md
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/RecursionCheck.scala
  - core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala
  - core/src/main/scala/dev/bosatsu/DefRecursionCheck.scala
  - core/src/test/scala/dev/bosatsu/LegacyDefRecursionCheck.scala
  - core/src/test/scala/dev/bosatsu/TypedExprRecursionCheckTest.scala
  - core/src/test/scala/dev/bosatsu/TypedExprRecursionParityTest.scala
  - core/src/test/scala/dev/bosatsu/DefRecursionCheckTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/WellTypedTests.scala
  - docs/src/main/paradox/recursion.md
depends_on: []
estimated_size: M
generated_at: 2026-02-24T00:36:05Z
---

# Design post-typechecking Recursion checker

_Issue: #1770 (https://github.com/johnynek/bosatsu/issues/1770)_

## Summary

Move recursion validation from syntax `Declaration` to post-typechecking `TypedExpr`, introduce a shared recursion error/strategy model, and migrate safely via legacy differential testing (including WellTypedGen) before enabling typed `Int` and future enum-order decrease strategies.

# Design post-typechecking Recursion checker

Status: proposed  
Date: 2026-02-24  
Issue: #1770 (https://github.com/johnynek/bosatsu/issues/1770)  
Base branch: `main`

## Problem statement
Today recursion legality is checked by `DefRecursionCheck` on `Declaration` before `Infer.typeCheckLets` in `Package.inferBodyUnopt`. That creates three core problems:
1. The checker has no resolved type information.
2. #1760 (`Int` recursion) is forced toward syntax-level strategy encoding.
3. The checker is tied to source syntax, while later compiler phases operate on `TypedExpr`.

Issue #1770 asks to move recursion checking post-typechecking on `TypedExpr`, discuss benefits and risks, and define a migration plan with parity against the legacy checker.

## Goals
1. Make a typed recursion checker authoritative in the production pipeline.
2. Preserve current behavior for structural and tuple-lexicographic recursion.
3. Keep recursion diagnostics source-anchored and close to current messages.
4. Keep strategy semantics strictly structural in this PR and document the typed path needed for #1760 follow-up.
5. Keep the current checker in test scope as a legacy oracle and verify parity on WellTypedGen programs.
6. Define a safe future path for broader notions of smaller such as enum-constructor ordering.

## Non-goals
1. Do not implement the full #1760 proof engine in this issue.
2. Do not change parser surface syntax in this issue.
3. Do not change codegen or runtime behavior in this issue.
4. Do not enable enum-ordinal decrease by default in this issue.

## Current pipeline
In `Package.inferBodyUnopt` today:
1. `SourceConverter.toProgram` builds `Expr[Declaration]` lets.
2. `DefRecursionCheck.checkStatement` runs on source `Statement`.
3. `TotalityCheck` and unused-let checks run on source expressions.
4. `Infer.typeCheckLets` produces typed lets (`TypedExpr[Declaration]`).
5. Lowering and normalization run later.

Recursion checking happens before type information is available.

## Proposed pipeline
1. Keep source conversion, totality, and unused-let checks as-is.
2. Remove production use of `DefRecursionCheck` from `Package.inferBodyUnopt`.
3. After successful `Infer.typeCheckLets`, run `TypedExprRecursionCheck` on typed lets.
4. Map typed recursion failures through `PackageError.RecursionError`.
5. Run `TypedExprLoopRecurLowering` and `TypedExprNormalization` only after recursion checking succeeds.

Net effect: recursion validity moves to post-typechecking and pre-lowering.

Ordering constraint (required): recursion checking must run before `TypedExprNormalization`.
Reason:
1. Normalization rewrites match and recursion shape, which can degrade source diagnostics if recursion checking runs afterwards.
2. Some rewrites rely on trusted recursion/termination properties; running typed recursion checks first keeps that trust boundary explicit.

## Architecture and modules
### 1. `RecursionCheck` shared model
Add `core/src/main/scala/dev/bosatsu/RecursionCheck.scala` with:
1. Checker-neutral `Error` ADT equivalent to current recursion errors.
2. Shared diagnostics text and region behavior.
3. Structural decrease relation as the only active strategy in this PR, with extension points documented for future typed strategies.

`PackageError.RecursionError` should depend on `RecursionCheck.Error`, not `DefRecursionCheck.RecursionError`.

### 2. `TypedExprRecursionCheck`
Add `core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala`.

Inputs:
1. Package name.
2. Full `TypeEnv[Kind.Arg]`.
3. Typed lets list: `List[(Bindable, RecursionKind, TypedExpr[Declaration])]`.

Output:
1. `ValidatedNec[RecursionCheck.Error, Unit]`.

The checker traverses recursive definitions in typed IR (top-level and nested recursive lets), mirroring current recursion invariants.

### 3. Legacy checker relocation for parity
Move current syntax checker logic to `core/src/test/scala/dev/bosatsu/LegacyDefRecursionCheck.scala` and keep it only for differential testing during migration.

## Typed checker algorithm
1. Find each recursive binding (`RecursionKind.Recursive`) and recover parameter groups from typed lambda shape.
2. While recovering lambda groups and traversing recur regions, peel wrappers explicitly:
   1. On `TypedExpr.Generic(q, in)`, push a quantification frame and record which `Type.Var.Bound` are universal (`q.forallList`) vs existential (`q.existList`) for the current traversal scope.
   2. On `TypedExpr.Annotation(in, tpe, qev)`, keep both inner expression and annotation/coercion metadata so scrutinee typing can be interpreted with the active quantification context.
   3. Enter and exit wrapper frames structurally (stack discipline) so each recur-site is checked with the exact in-scope universal/existential view.
3. Track checker state analogous to legacy checker (`TopLevel`, `InDef`, `InDefRecurred`, `InRecurBranch`) so existing invariants are preserved.
4. Detect recur-sites from typed matches by reading node tags: a typed `Match` is a recur-site when its tag is `Declaration.Match(RecursionKind.Recursive, target, ...)`.
5. Resolve recur target names to parameter positions and reject invalid, duplicate, or local targets with equivalent error semantics.
6. Use structural decrease classification for all recur-target components in this PR (parity mode).
7. For each branch, compute branch-local decrease evidence: single target uses `pattern.substructures`; tuple target uses component-wise evidence from tuple branch patterns, or empty evidence when shape does not align.
8. For each recursive self-call in a recur-branch, classify each target argument as `Equal`, `Smaller`, or `Other` using the structural relation, then apply lexicographic acceptance where the first non-equal component must be `Smaller`.
9. Validate nested recursive calls inside call arguments, matching current behavior.
10. Keep current constraints around illegal shadowing, unexpected `recur`, and `recur` with no recursive call.

## Why post-typechecking helps #1760
Typed checking still improves the #1760 path even with structural-only parity in this PR:
1. Scrutinee and target types are available in `TypedExpr`, so future strategy choice can be type-driven instead of syntax-driven.
2. The wrapper-peeling/quantification context above provides the place to reason about `Int`-typed recur targets without parser-level strategy encoding.
3. A follow-up can add `IntDecrease` checks using this typed infrastructure, without changing the migration objective here (behavior parity first).

## Future extension: enum-constructor ordering as smaller
Potential follow-up after parity:
1. For enum constructors `C0, C1, ..., Cn` in declaration order, define `rank(Ci) = i`.
2. In a branch that proves value is `Ci`, allow recursive call arguments proven to be constructor `Cj` with `j < i` as a decrease witness.
3. Combine this with existing structural and lexicographic rules and keep strictness.
4. Gate the feature initially.

Safety rationale: constructor rank over a finite enum is well-founded. Risk comes from over-approximating proof, so implementation must require typed proof of constructor identity before using rank.

## Differential testing plan
### 1. Unit tests for typed checker
Add `TypedExprRecursionCheckTest` with positive and negative cases mirroring legacy recursion tests.

### 2. Property parity against legacy checker
Add `TypedExprRecursionParityTest`:
1. Generate programs via `WellTypedGen` phase1 through phase4.
2. Run legacy checker on source statements.
3. Typecheck and run typed checker on resulting typed lets.
4. Assert pass or fail agreement.
5. Report mismatches with minimal repro output.

### 3. Curated regression corpus
Reuse recursion-heavy cases from `DefRecursionCheckTest` and key diagnostics from `ErrorMessageTest`, because WellTypedGen has limited recursion-shape coverage today.

## Acceptance criteria
1. `Package.scala` no longer runs `DefRecursionCheck` in production typechecking.
2. `TypedExprRecursionCheck` runs post-inference and blocks invalid recursion.
3. `PackageError.RecursionError` wraps shared `RecursionCheck.Error`.
4. Legacy checker code exists in test scope as `LegacyDefRecursionCheck` and is not required by production pipeline.
5. Differential parity tests pass for WellTypedGen phase1 through phase4 in CI.
6. Curated recursion regressions match existing accept or reject behavior for structural and tuple lexicographic recursion.
7. Strategy behavior remains structural-only for all targets in this PR (explicitly deferring `IntDecrease` and enum-ordinal strategies).
8. User docs note that recursion validation now runs post-typechecking and before normalization.

## Risks and mitigations
1. Risk: inference failures now prevent recursion diagnostics from being produced in the same run.
Mitigation: document ordering change and keep diagnostics quality high when recursion checker runs.

2. Risk: recur-site detection via `Declaration` tags in typed matches is brittle.
Mitigation: run checker immediately after inference; if fragility appears, add an explicit typed recur marker in a follow-up.

3. Risk: behavior drift from legacy checker.
Mitigation: mandatory differential tests (property plus curated corpus) before switching production path.

4. Risk: extra typed-tree traversal cost.
Mitigation: single linear traversal per recursive definition with cached target-position and normalized-type lookups.

5. Risk: enum-ordinal extension could admit unsound decreases if proof is too permissive.
Mitigation: keep feature disabled by default and require explicit typed constructor proof.

## Rollout plan
1. Stage 1: introduce `RecursionCheck` shared ADT and implement `TypedExprRecursionCheck` in test-only shadow mode.
2. Stage 2: add legacy versus typed parity tests and fix mismatches.
3. Stage 3: switch production pipeline in `Package.scala` to typed recursion checking.
4. Stage 4: move syntax checker implementation to test-only (`LegacyDefRecursionCheck`) and remove production dependency.
5. Stage 5: implement #1760 obligations using the `IntDecrease` strategy path.
6. Stage 6: evaluate optional enum-ordinal decrease behind a guarded flag.

## Decision
Proceed with post-typechecking recursion checking on `TypedExpr` as the authoritative path, enforce strict parity first, and use typed strategy hooks to cleanly unlock #1760 and later decrease-relation extensions.
