---
issue: 2185
priority: 3
touch_paths:
  - docs/design/2185-design-a-matchless-global-inlining-phase.md
  - core/src/main/scala/dev/bosatsu/Matchless.scala
  - core/src/main/scala/dev/bosatsu/MatchlessGlobalInlining.scala
  - core/src/main/scala/dev/bosatsu/MatchlessFromTypedExpr.scala
  - core/src/main/scala/dev/bosatsu/codegen/CompilationSource.scala
  - core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala
  - core/src/main/scala/dev/bosatsu/library/LibraryEvaluation.scala
  - core/src/test/scala/dev/bosatsu/MatchlessApplyArgsTest.scala
  - core/src/test/scala/dev/bosatsu/MatchlessTests.scala
  - core/src/test/scala/dev/bosatsu/MatchlessInterfaceTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenLibraryDepsTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-17T22:39:58Z
---

# Matchless Global Inlining Phase

_Issue: #2185 (https://github.com/johnynek/bosatsu/issues/2185)_

## Summary

Add a post-lowering Matchless rewrite that keeps raw lowering parallel, builds topo-ordered summaries for global definitions, and heuristically inlines eligible cross-package helper calls before code generation.

## Context

`TypedExprNormalization` already performs a small amount of heuristic inlining through `ResolveToLambda`, but that logic is limited to the typed scope that is currently available and stops at package boundaries. Once lowering reaches Matchless, every non-local reference is preserved as `Global(from, pack, name)`, and current consumers treat those globals as ordinary calls.

That leaves a gap for cross-package control-flow helpers. A helper defined in another package can often avoid eager work once its body is visible, especially when:

1. some parameters are not used on every path
2. a parameter is only used under a branch body
3. a parameter is a literal lambda, or resolves to a lambda, and inlining would expose immediate beta-reduction

The existing pipeline already has the right insertion point for this optimization. Raw Matchless lowering is pure and parallel, and both code generation and evaluation consume compiled Matchless output afterward. Unlike issue #2178, this issue does not need new source syntax or interface/protobuf metadata. The implementation can stay entirely inside the post-lowering compiler pipeline and only inline when implementation packages are actually available.

## Problem

Today, imported helper calls survive lowering as `App(Global(...), args)`. That has two consequences:

1. ordinary call shaping still makes the actual arguments eager before control flow in the callee body can discard or delay them
2. any package-local typedexpr inlining heuristics are lost once the call crosses a package boundary

Matchless is a better place to recover these wins because it is type-erased, structurally comparable, and already closer to the branching and closure shapes that matter for code generation. The missing piece is a global inlining phase that can resolve compiled Matchless definitions across package and library scopes without giving up the current parallel raw-lowering step.

## Goals

1. Add a Matchless post-pass that can inline eligible top-level globals across package and library boundaries when implementation bodies are available.
2. Preserve the existing parallel raw Matchless lowering step.
3. Apply conservative heuristics that prioritize branch-delayed arguments and lambda-argument beta-reduction opportunities.
4. Keep semantics unchanged, including name binding, dependency-scope resolution, and evaluation behavior.
5. Route codegen and evaluator consumers through the same optimized Matchless pipeline.

## Non-goals

1. No new `inline def` syntax, export metadata, or protobuf/interface changes. That remains the scope of #2178.
2. No forced inlining annotation or user-configurable pragma in this PR.
3. No special handling for recursive source forms that have not already lowered cleanly to Matchless, and no new whole-program dead-code elimination pass beyond the tree shaking that already exists.
4. No generalized global CSE for arbitrary values beyond small cleanup needed after inlining.
5. No inlining when only interfaces are present and implementation packages are unavailable.

## Alternatives Considered

1. Inline during `MatchlessFromTypedExpr.compile`.
Reason rejected: it couples raw lowering to dependency ordering and gives up the current fully parallel lowering step.
2. Keep all heuristic inlining in `TypedExprNormalization`.
Reason rejected: cross-package bodies are not available there, and Matchless can reason about structurally equal expressions without the same type constraints.
3. Wait for explicit `inline def` support.
Reason rejected: issue #2185 asks for heuristic wins that should apply to ordinary defs before any forced-inline surface feature lands.

## Proposed Architecture

### 1. Keep raw lowering separate from global inlining

`MatchlessFromTypedExpr.compile` should remain the raw package-to-Matchless lowering primitive. Add a new coordinator, tentatively `MatchlessGlobalInlining`, that takes:

1. raw compiled lets grouped by scope and package
2. `Toposort.Result[(K, PackageName)]` for dependency ordering
3. `depFor` so `Global(from, pack, name)` can be resolved to the correct scope key

The new phase returns the same `MatchlessFromTypedExpr.Compiled[K]` shape. Backends and the evaluator do not need a new IR. In codegen flows, it should run on the already tree-shaken namespace so discarded lets are never summarized or inlined.

### 2. Publish summaries for rewritten globals

Each rewritten top-level let should publish an `InlineSummary` keyed by `(scopeKey, packageName, bindable)`.

Likely summary fields:

1. recovered lambda form from `Matchless.recoverTopLevelLambda`, if the binding can be treated as a top-level lambda
2. arity and rewritten body
3. expression weight / size estimate for budget checks
4. boundary-safety flags describing whether the body, or any nested lambda/value it produces, captures a free `LocalAnonMut` from outside the candidate
5. per-parameter demand facts: unused, eager on the selector or guard path, deferrable into an `If` or `SwitchVariant` branch body, lambda-callee-only, and total use count

The key summary fact is whether an argument is deferrable. A parameter is deferrable when every demand for it occurs inside an `If` or `SwitchVariant` branch body rather than on the selector or guard path. `Bosatsu/Predef.and` and `Bosatsu/Predef.or` are the canonical examples: after inlining, `and(x, y)` can become `if x: y; else False` and `or(x, y)` can become `if x: True; else y`, so `y` is deferrable in both cases. Because Bosatsu is pure and total, that deferral does not change semantics, but it can strictly lower runtime cost by avoiding work on untaken branches.

V1 should only publish conservative candidates:

1. fully known top-level lambda arity
2. no captured free mutable anons cross the inline boundary
3. bodies that have already lowered recursion to a closed `WhileExpr` remain eligible and are judged by the same size and duplication heuristics
4. body size below a fixed budget after rewrite

### 3. Rewrite in topo order while keeping layer parallelism

Process packages by `topoSort.layers`.

For each `(scope, package)`:

1. derive a same-package dependency order for lets from raw `Global` references
2. rewrite lets in that local topo order so forward references inside the same package can inline once their summaries exist
3. preserve the original let order in the final compiled output to minimize diffs and avoid surprising downstream behavior
4. publish summaries immediately after each let is rewritten

Packages in the same topo layer can still rewrite in parallel because all dependency summaries from earlier layers are already finalized.

### 4. Add a dedicated Matchless inlining application path

Add `Matchless.inlineApplyArgs` next to the existing `applyArgs`.

`applyArgs` should remain the ordinary call helper and keep eager outer `Let` bindings for actual arguments.

`inlineApplyArgs` should instead:

1. alpha-rename binders in the callee body to avoid capture
2. substitute actual arguments directly into the lambda body instead of first binding every argument in outer `Let`s
3. push through `If`, `SwitchVariant`, `Always`, and safe aliasing `Let`s in the same spirit as the current `applyArgs` and `recoverTopLevelLambda` logic
4. preserve nested `Global` nodes with their original `from` scope key so any dependency lookup continues to follow the callee package's dependency graph, not the caller's

This is the mechanism that keeps branch-only arguments inside branch bodies and exposes lambda beta-reduction opportunities that are invisible in the raw global call.

### 5. Use a conservative benefit-vs-cost heuristic

Inline only fully saturated direct calls whose callee resolves to an `InlineSummary`.

Hard gates:

1. the summary does not expose a free mutable anon across the inline boundary
2. the recovered lambda arity exactly matches the call arity
3. the candidate body stays under a configurable size budget
4. no non-cheap actual argument would be duplicated more than a small threshold

Benefit signals:

1. a parameter is unused in the callee body
2. a non-cheap actual argument sits in a deferrable parameter position, meaning inlining can move that work under an `If` or `SwitchVariant` branch body instead of forcing it on every path
3. a parameter is only used as a direct callee, and the actual argument is a literal lambda or resolves to a lambda after local alias resolution
4. the candidate body is tiny and pure

Cost signals:

1. rewritten body weight
2. repeated parameter use count
3. actual-argument weight when that argument would be copied multiple times
4. introduction of additional control-flow or loop structure at the call site

Some candidates should be treated as unconditional wins rather than merely good heuristic scores. At minimum, `Bosatsu/Predef.and` and `Bosatsu/Predef.or` should always inline, because their entire value is short-circuit deferral of the second argument and their lowered bodies are tiny.

Inline when the benefit exceeds the cost and all hard gates pass. A small helper whose recursion has already lowered to a self-contained `WhileExpr` is still eligible under this policy. The initial constants should mirror the intent of `TypedExprNormalization.ResolveToLambda`, but stay separate so Matchless tuning does not silently perturb typedexpr behavior.

### 6. Reuse local cleanup after inlining

After rewriting a let body, rerun existing local Matchless cleanup that is already safe post-lowering, especially `reuseConstructors`. If `inlineApplyArgs` introduces trivial `Let` wrappers or immediate alias opportunities, clean those up before publishing the summary.

This likely requires a small refactor in `Matchless.scala` so the existing post-lowering cleanup becomes an explicit pipeline rather than a few maps hanging off the tail of `fromLet`. The raw lowering step, the existing local cleanup passes, and the new global inlining phase should be exposed as named stages so the sequencing is easy to reason about and extend.

Existing tree shaking and dead-code elimination should still run first and determine which top-level bindings reach this phase. V1 does not add another DCE step; it only rewrites the surviving bodies. Export selection remains unchanged.

### 7. Integrate at the common compilation entry points

Route the user-facing Matchless compilation paths through the new post-pass:

1. `CompilationSource.packageMapSrc.compiled`
2. `DecodedLibraryWithDeps` compiled namespace construction
3. `LibraryEvaluation` compiled scope cache

That keeps codegen and evaluator behavior aligned. For codegen, the post-pass should run after `CompilationNamespace.treeShake(...)` has selected the reachable graph. Interface-only contexts continue to use raw `Global` calls because no implementation body exists to inline.

## Detailed Implementation Plan

1. Refactor the current Matchless post-lowering cleanup into explicit named phases so raw lowering, local cleanup, and global inlining are not fused into one tail of `fromLet`.
2. Add shared Matchless analysis helpers in `Matchless.scala`: expression weight, parameter-demand summary, deferrable-argument detection, lambda-callee-only detection, and `inlineApplyArgs`.
3. Add `MatchlessGlobalInlining.scala` with `InlineSummary`, local dependency sorting, topo-layer rewrite orchestration, and call-site heuristic evaluation.
4. Treat `Bosatsu/Predef.and` and `Bosatsu/Predef.or` as mandatory inline wins, either through an explicit always-inline classification or an equivalent deterministic rule over tiny short-circuit boolean combinators.
5. Keep `MatchlessFromTypedExpr.compile` as the raw primitive and call it from the new coordinator.
6. Update `CompilationSource`, `DecodedLibraryWithDeps`, and `LibraryEvaluation` to use the optimized pipeline instead of exposing only raw compiled output, and ensure any existing tree-shake step feeds the inliner rather than the reverse.
7. Preserve deterministic output by using topo order plus stable tie-breaks based on original let position or bindable ordering.
8. Add structural tests for `inlineApplyArgs` and for heuristic inlining decisions.
9. Add cross-package and cross-library regressions that prove imported helpers inline when implementations are available and remain uninlined when only interfaces are available.
10. Review generated-code diffs on representative `test_workspace` programs to confirm that eager work decreases without unacceptable code-size growth.

## Testing Strategy

1. Extend `MatchlessApplyArgsTest.scala` with branch-sensitive cases that show `inlineApplyArgs` keeps expensive arguments under `If` or `SwitchVariant` branches instead of hoisting them into eager lets.
2. Extend `MatchlessTests.scala` with explicit `Bosatsu/Predef.and` and `Bosatsu/Predef.or` examples that always inline and defer the second argument into the appropriate branch body.
3. Extend `MatchlessTests.scala` with another branch-only-parameter example that should inline because a non-cheap actual argument sits in a deferrable position.
4. Extend `MatchlessTests.scala` with a lambda-argument example that becomes directly beta-reducible after inlining.
5. Extend `MatchlessTests.scala` with a helper that lowers to a small closed `WhileExpr` and remains eligible for inlining.
6. Extend `MatchlessTests.scala` with a rejected case where a large or duplicated argument should not inline.
7. Extend `MatchlessInterfaceTest.scala` with a cross-package example that proves imported implementations can inline while interface-only dependencies still compile through the old `Global` path.
8. Extend `ClangGenLibraryDepsTest.scala` with a cross-library helper whose generated code no longer materializes both branch arguments before the helper call shape.
9. Keep the broader Matchless, evaluator, and codegen suites green to validate semantic preservation.

## Acceptance Criteria

1. The compiler has a distinct Matchless global inlining phase that runs after raw lowering and before Matchless consumers.
2. Raw Matchless lowering remains parallel, and in codegen flows the post-pass runs after existing tree shaking/dead-code elimination on the already-selected graph.
3. Fully saturated calls to eligible globals, including helpers whose recursion has already lowered to a closed `WhileExpr`, can inline across packages and across library dependency scopes when implementation packages are available.
4. The heuristic records deferrable argument positions and uses them at call sites, so non-cheap actual arguments can be pushed under `If` or `SwitchVariant` branches when that lowers runtime cost.
5. `Matchless.inlineApplyArgs` avoids eager outer lets for inlined arguments and preserves capture and dependency-scope correctness.
6. Candidates that would expose free mutable anons across the boundary, exceed size budgets, or duplicate expensive arguments remain as ordinary global calls.
7. Codegen and evaluator compilation entry points use the same optimized Matchless pipeline.
8. Interface-only dependency flows require no schema changes and still compile without attempting unavailable global inlining.
9. `Bosatsu/Predef.and` and `Bosatsu/Predef.or` are always inlined, so their second argument can be deferred into the short-circuit branch.
10. New structural and cross-library tests fail before the change and pass after it.
11. Generated-code diff review on representative programs shows no unacceptable code-size blowup.

## Risks And Mitigations

1. Risk: code size grows quickly when a helper body is duplicated at many call sites.
Mitigation: keep budgets conservative, block multi-use expensive arguments, and score `WhileExpr` bodies by size instead of rejecting them blindly.
2. Risk: wrong name capture or wrong dependency scope after copying a callee body into a caller.
Mitigation: alpha-rename binders before substitution and preserve the callee body's original `from` scope on nested globals.
3. Risk: codegen and evaluator diverge if only one path uses the new pass.
Mitigation: introduce one shared coordinator and route all user-facing Matchless compilation paths through it.
4. Risk: compile time regresses because every call site performs heavy analysis.
Mitigation: summarize each candidate once, memoize summaries, and keep raw lowering unchanged and parallel.
5. Risk: the heuristic misses profitable cases or accepts marginal ones.
Mitigation: start narrow, pin the intended cases with tests, and tune budgets from observed generated-code diffs instead of widening the first version.

## Rollout Notes

1. Land this as a compiler-only PR with no language, interface, or serialization changes.
2. Start with conservative heuristic constants and document them near the optimizer so future tuning is explicit.
3. Validate on at least one cross-package helper example and a representative `test_workspace` generated-code diff before merge.
4. If regressions appear, rollback is straightforward because raw lowering stays intact and the post-pass can be bypassed or reduced to zero-budget behavior.
