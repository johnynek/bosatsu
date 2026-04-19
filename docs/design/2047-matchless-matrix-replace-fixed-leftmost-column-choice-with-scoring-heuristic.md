issue: 2047
priority: 2
touch_paths:
  - docs/design/2047-matchless-matrix-replace-fixed-leftmost-column-choice-with-scoring-heuristic.md
  - core/src/main/scala/dev/bosatsu/Matchless.scala
  - core/src/test/scala/dev/bosatsu/MatchlessTests.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenTest.scala
depends_on: []
estimated_size: S
generated_at: 2026-03-07T00:00:00Z
---

# Issue #2047 Design: Matchless matrix column scoring heuristic

_Issue: #2047 (https://github.com/johnynek/bosatsu/issues/2047)_

Status: proposed  
Base branch: `main`

## Summary

Replace fixed `colIdx = 0` selection in Matchless matrix compilation with a deterministic, low-cost scoring heuristic. Use the same chooser in both:

1. `matchExprMatrixCheap` (real decision-tree generation).
2. `matrixFallbackMultiplicity` (ordered-fallback multiplicity estimator).

The heuristic favors columns that discriminate earlier, penalizes expensive high-arity expansions, and uses leftmost as a final tie-breaker. Semantics do not change; only decision-tree and generated-code shape change.

## Problem statement

Current behavior always chooses the leftmost remaining column in two places:

1. matrix compiler recursion (`compileRows`).
2. fallback multiplicity estimator (`compileRowsCount` inside `matrixFallbackMultiplicity`).

This can produce avoidable checks and projections in multi-column matches, especially when the leftmost column has wide constructors while another column has better discriminating power.

Because fallback multiplicity also mirrors leftmost choice, any future improvement in real compilation can drift from fallback estimation unless both paths share one policy.

## Goals

1. Remove fixed-leftmost column selection from matrix compilation.
2. Introduce a deterministic, cheap scoring heuristic with no lookahead search.
3. Reuse one chooser implementation in both compile and fallback multiplicity counting.
4. Preserve match semantics and predictable tie-breaking behavior.
5. Improve generated C shape for at least one representative multi-column match.

## Non-goals

1. Global optimal decision-tree search.
2. Exponential or deep lookahead heuristics.
3. Reordering source branch-priority semantics.
4. Changing non-orthogonal matcher behavior.

## Proposed architecture

### 1) Shared column-scoring helper in `Matchless.scala`

Introduce internal scorer helpers near matrix utilities:

1. `sigArity(sig: HeadSig): Int`.
2. `ColumnScore` data structure carrying `colIdx`, `distinctSigs`, `refutableRows`, `arityPenalty`, and `sigs`.
3. `chooseColumnByScore(rows: List[MatchRow]): (Int, List[HeadSig])`.

Scoring inputs per column:

1. `distinctSigs`: count of distinct refutable head signatures.
2. `refutableRows`: number of rows where the pattern at this column is not wildcard.
3. `arityPenalty`: cheap projection-expansion proxy derived from signature arities.

Deterministic comparator:

1. maximize `distinctSigs`.
2. maximize `refutableRows`.
3. minimize `arityPenalty`.
4. minimize `colIdx` (leftmost tie-break).

This keeps chooser cost linear in matrix size per node and deterministic.

### 2) Replace `colIdx = 0` in matrix compile path

In `matchExprMatrixCheap.compileRows`:

1. replace hardcoded `val colIdx = 0` with chooser result.
2. reuse chooser-provided `sigs` for case compilation order.
3. keep existing specialization, default-row handling, guard handling, and `mustMatch` logic unchanged.

### 3) Apply the same chooser in fallback multiplicity estimation

In `matrixFallbackMultiplicity.compileRowsCount`:

1. replace hardcoded `val colIdx = 0` with the shared chooser (applied on `rows.map(_.row)`).
2. reuse returned `sigs` for `compileCasesCount`.
3. keep tagged fallback counting semantics unchanged.

This keeps `shouldPreferOrderedTerminalFallback` aligned with real matrix compilation behavior.

### 4) Determinism and semantic safety

Determinism is preserved by integer scoring and explicit leftmost tie-break. Semantic behavior is preserved because branch order, guard handling, and row-specialization semantics remain unchanged; only test/projection placement in the decision tree changes.

## Detailed implementation plan

1. Add column-scoring helpers and score comparator in `Matchless.scala`.
2. Update matrix compile column selection to call the chooser.
3. Update fallback multiplicity column selection to call the same chooser.
4. Update comments that currently describe fixed-leftmost behavior.
5. Add issue-focused regression tests in `MatchlessTests.scala` and `ClangGenTest.scala`.
6. Run targeted suites, then full core tests.

## Testing plan

### A) Matrix-level regression (`core/src/test/scala/dev/bosatsu/MatchlessTests.scala`)

Add a focused multi-column test where:

1. one column has low discrimination with higher-arity expansion cost.
2. another column has higher discrimination.

Assert structural properties instead of full-tree snapshots, for example:

1. discriminating checks appear before expensive-field projections.
2. wide-column projections are introduced only on needed branch paths.
3. runtime behavior remains unchanged for representative inputs.

### B) Generated C regression (`core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenTest.scala`)

Add one representative program and assert code-shape improvements using robust checks:

1. reduced unconditional `get_struct_index` or `get_enum_index` usage at top decision level.
2. discriminating checks emitted before wide-arity projections.

Use regex/count/order assertions, not full rendered-file equality.

### C) Existing suite validation

1. Existing Matchless tests pass.
2. Existing C codegen tests pass.
3. No expected parser/typechecker behavior changes.

### D) Repository-wide generated C diff audit (`test_workspace`)

Add an explicit audit pass across all `test_workspace` cases:

1. Generate C output for all `test_workspace` programs at `base_sha` and `head_sha`.
2. Diff generated C outputs case-by-case.
3. Verify changes are neutral-to-positive in aggregate, with focus on reduced redundant checks/projections and no suspicious control-flow regressions.
4. Capture a short summary of observed diff patterns in the PR description.

## Acceptance criteria

1. Column selection in matrix compilation is no longer fixed-leftmost.
2. Fallback multiplicity counting uses the same chooser as real matrix compilation.
3. Heuristic remains deterministic and cheap (no exponential lookahead).
4. Existing match tests pass.
5. At least one representative multi-column case shows reduced checks or projections in generated C versus the prior leftmost shape.
6. Repository-wide generated C diffs for `test_workspace` are reviewed before/after and look positive in general.
7. Match semantics are unchanged; only decision-tree/code shape changes.

## Risks and mitigations

1. Risk: chooser divergence between compile and fallback paths.
   Mitigation: one shared chooser helper used in both sites.

2. Risk: heuristic weight balance regresses some patterns.
   Mitigation: keep comparator simple and tune only with targeted evidence.

3. Risk: subtle behavior regressions from changed tree shape.
   Mitigation: preserve row order and guard semantics; rely on broad existing tests plus issue-specific regressions.

4. Risk: generated-C tests become brittle.
   Mitigation: assert structural/count properties rather than full-output snapshots.

5. Risk: compile-time overhead on large matches.
   Mitigation: linear per-node scoring only; no recursive lookahead added.

## Rollout notes

1. Land as a single PR against `main`.
2. Include before/after evidence for one representative multi-column C case in the PR description.
3. Include a summary of the full `test_workspace` generated-C before/after diff review in the PR description.
4. No feature flag or migration needed.
5. If regressions appear, rollback is straightforward by temporarily restoring leftmost selection at chooser call sites.

## Decision

Proceed with a shared deterministic scoring heuristic for matrix column selection in both real compilation and fallback multiplicity counting.
