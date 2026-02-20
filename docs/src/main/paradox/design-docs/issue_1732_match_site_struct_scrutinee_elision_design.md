# Issue 1732: Match-Site Struct/Tuple Scrutinee Elision in `Matchless`

Status: proposed  
Date: 2026-02-20  
Issue: <https://github.com/johnynek/bosatsu/issues/1732>

## Problem
Issue [#1732](https://github.com/johnynek/bosatsu/issues/1732) describes a common shape:

```bosatsu
match (f, g):
  case (..., ...):
```

Today this lowers to `App(MakeStruct(2), ...)` and later pattern matching reads with `GetStructElement(...)`, so we allocate a tuple even when matching can read fields directly.

## Decision Summary
1. Yes: we can do this entirely in the `Matchless` lowering layer.
2. Yes: this generalizes to all struct-like single-constructor values lowered as `MakeStruct(arity)`, not just tuples.
3. Yes: we can support both matching backends (matrix and ordered sequential if/else) with one shared scrutinee representation.

## Goals
1. Elide match-site `MakeStruct` allocation when scrutinee is syntactically `App(MakeStruct(arity), args)`.
2. Preserve strict evaluation behavior (evaluate `args` once, left-to-right, before branch selection).
3. Keep semantics identical across matrix and ordered backends.
4. Reuse existing totality/typing invariants (no changes in `TypedExpr`, parser, or typechecker).

## Non-goals
1. No source-language changes.
2. No changes to totality checking.
3. No lazy argument evaluation in phase 1.

## Implementation Scope (files)
Implementation (follow-up PR) is concentrated in:

1. `core/src/main/scala/dev/bosatsu/Matchless.scala`
2. `core/src/test/scala/dev/bosatsu/MatchlessTests.scala`

This design-doc PR adds:

1. `docs/src/main/paradox/design-docs/issue_1732_match_site_struct_scrutinee_elision_design.md`
2. `docs/src/main/paradox/design-docs/index.md`

## New Internal Abstractions and Functions
Add these inside `Matchless.fromLet` (near existing match helpers):

1. `case class InlinedStructRoot(fields: Vector[CheapExpr[B]])`
1. Represents a root scrutinee known to be a struct constructor application, already converted to cheap field expressions.

2. `def prepareInlinedStructRoot(arg: Expr[B]): F[Option[InlinedStructRoot]]`
1. Detects `arg` shape `App(MakeStruct(arity), args)` with matching arity.
2. Ensures strict single evaluation by introducing `LocalAnon` lets for non-cheap args.
3. Returns cheap fields preserving source left-to-right order.

3. `def structRootValue(root: InlinedStructRoot): Expr[B]`
1. Reconstructs `App(MakeStruct(arity), fields)` only when a branch binds the whole scrutinee (`Var`/`Named` at root).

4. `def structField(root: InlinedStructRoot, idx: Int, size: Int): CheapExpr[B]`
1. Returns `root.fields(idx)` with arity check.

5. `def bindOccurrenceValue(occ: CheapExpr[B], inlined: Option[InlinedStructRoot]): Expr[B]`
1. For normal occurrences, returns `occ`.
2. For root inlined struct occurrence, returns `structRootValue(...)`.

6. `def projectStructOccurrence(occ: CheapExpr[B], idx: Int, size: Int, inlined: Option[InlinedStructRoot]): CheapExpr[B]`
1. For normal occurrences, returns `GetStructElement(occ, idx, size)`.
2. For root inlined struct occurrence, returns `structField(...)`.

## Ordered (sequential) matcher changes
Modify ordered compilation path (`matchExprOrderedCheap` + `doesMatch`) to thread optional root inlined-struct context:

1. Extend `doesMatch` with an optional `rootInlined: Option[InlinedStructRoot]` parameter used only at the top call.
2. In `Pattern.PositionalStruct` + `DataRepr.Struct/NewType` path, replace direct `GetStructElement(arg, pos, size)` with `projectStructOccurrence(...)`.
3. In root `Pattern.Var` / `Pattern.Named` binding cases, bind via `bindOccurrenceValue(...)` so full-value binds still work.
4. Recursive `doesMatch` calls on subpatterns pass `None` (only the root may be inlined).

Result: sequential fallback gets the same no-allocation benefit as matrix compilation.

## Matrix matcher changes
Modify matrix compilation (`matchExprMatrixCheap`) so root-struct projections also go through shared projection helpers:

1. At entry, call `prepareInlinedStructRoot(arg)`.
2. If `None`, keep current behavior.
3. If `Some(root)`, compile with a root context flag and use:
1. `projectStructOccurrence(...)` in `buildCase` for `StructSig`.
2. `bindOccurrenceValue(...)` from `normalizeRow`/`peelPattern` when emitting alias binds for root occurrence.
4. Keep existing projection materialization (`materializeOccs`) for non-root projected occurrences.

This keeps matrix sharing logic unchanged while replacing root `GetStructElement` reads with direct root field values.

## Why this is Matchless-only
This optimization depends on lowered constructor forms (`MakeStruct` + projection nodes) and backend matcher internals (`doesMatch`, `buildCase`, `peelPattern`).

No changes are needed in:

1. `TypedExpr` shape/typechecking.
2. Totality checker.
3. Source converter/parser.

## Generalization Beyond Tuple
Tuples and user-defined single-constructor struct-like values already lower to `MakeStruct(arity)` in `Matchless`.

So detection on `App(MakeStruct(arity), args)` automatically covers:

1. tuple literals,
2. newtypes lowered as arity-1 struct,
3. any struct constructor lowered to `MakeStruct`.

No tuple-specific code path is required.

## Strictness and the "lazy fields" idea
Issue [#1732](https://github.com/johnynek/bosatsu/issues/1732) mentions possible lazy field evaluation via `LocalAnonMut`/`SetMut`.

Phase 1 intentionally stays strict:

1. Evaluate all scrutinee args exactly once before matching.
2. Elide only the container allocation (`MakeStruct` result object).

Optional phase 2 (separate issue/PR): lazy per-field memo cells, only if we prove it does not change observable semantics in Bosatsu.

## Test Plan
Add tests in `core/src/test/scala/dev/bosatsu/MatchlessTests.scala`:

1. `matrix match elides tuple allocation at match site`:
1. compile `match (f(), g()): ...` and assert no root `App(MakeStruct(2), ...)` allocation is required for projection-only branches.

2. `ordered matcher also elides tuple allocation for non-orthogonal suffix`:
1. force ordered fallback (e.g. list/string search suffix) and assert the same root elision.

3. `single-constructor struct behaves like tuple`:
1. hand-written typed struct constructor scrutinee `App(MakeStruct(n), ...)` gets identical projection elision.

4. `whole-root binding still works`:
1. include `case x as ...`/`case x` branch and verify root value reconstruction is only on branch path that binds it.

5. `strict evaluation preserved`:
1. arguments are still evaluated once and in order (no duplication, no skipping).

## Risks and Mitigations
1. Risk: regressions in root alias binding (`Pattern.Named` / `Pattern.Var`).
1. Mitigation: explicit whole-root binding tests in both matrix and ordered paths.

2. Risk: accidental fallback divergence between matrix and ordered backends.
1. Mitigation: mirror tests that force each backend.

3. Risk: introducing duplicate evaluations of non-cheap fields.
1. Mitigation: `prepareInlinedStructRoot` always memoizes non-cheap args before matching.

## Rollout
1. Land this design doc.
2. Implement in `Matchless.scala` behind existing matcher-selection flow.
3. Add regression tests and compare generated Matchless trees before/after for representative tuple/struct matches.
