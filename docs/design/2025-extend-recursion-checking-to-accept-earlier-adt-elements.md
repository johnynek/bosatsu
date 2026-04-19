---
issue: 2025
priority: 3
touch_paths:
  - docs/design/2025-extend-recursion-checking-to-accept-earlier-adt-elements.md
  - core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala
  - core/src/test/scala/dev/bosatsu/TypedExprRecursionCheckTest.scala
  - docs/src/main/paradox/recursion.md
depends_on: []
estimated_size: M
generated_at: 2026-03-06T16:21:42Z
---

# Issue #2025 Design: Extend recursion checking to accept earlier ADT elements

_Issue: #2025 (https://github.com/johnynek/bosatsu/issues/2025)_

## Summary

Design for adding a constructor-rank decrease rule to `TypedExprRecursionCheck` so recursive calls to earlier ADT constructors are accepted when sound, without changing parser/type-system behavior.

---
issue: 2025
priority: 3
touch_paths:
  - docs/design/2025-extend-recursion-checking-to-accept-earlier-adt-elements.md
  - core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala
  - core/src/test/scala/dev/bosatsu/TypedExprRecursionCheckTest.scala
  - docs/src/main/paradox/recursion.md
depends_on: []
estimated_size: M
generated_at: 2026-03-06T00:00:00Z
---

# Issue #2025 Design: Extend recursion checking to accept earlier ADT elements

_Issue: #2025 (https://github.com/johnynek/bosatsu/issues/2025)_

Status: proposed  
Base branch: `main`

## Summary

Keep the current typed recursion checker model (structural recursion, tuple lexicographic ordering, and SMT-backed `Int` obligations) and add one new sound decrease form:

A recur-target component of ADT type `T` is also `Smaller` when, in a branch that proves current value is constructor `Ccur`, the recursive call passes constructor `Cnext` of the same `T` with `index(Cnext) < index(Ccur)`, and any direct recursive payloads (`Fi == T`) are already-proven strictly smaller locals for that target component.

This makes `Some(_) -> None`, `NonEmptyList(_,_) -> EmptyList`, and similar constructor-rank decreases legal without adding new syntax, qualifiers, or proof terms.

## Problem statement

Current `TypedExprRecursionCheck` classifies non-`Int` recursion arguments primarily by:

1. branch-proven strict substructure names (`Smaller`),
2. equal-name/equal-alias (`Equal`),
3. singleton constructor equality (`Equal` for values like `[]` or `EmptyList`).

This rejects useful and sound recursion where a call moves to an earlier ADT constructor but does not pass a plain local substructure. Example: `Some(_) -> None`.

Issue #2025 asks for this additional admissible decrease while preserving current architecture and keeping soundness conservative.

## Goals

1. Preserve existing structural, lexicographic, and `Int` recursion behavior.
2. Accept recur calls that are constructor-rank decreasing on ADTs.
3. Automatically support zero-arg constructors under that rule.
4. Stay conservative by rejecting wrapped recursive payload fields in v1.
5. Avoid parser, surface syntax, and type-system changes.

## Non-goals (v1)

1. New recursion annotations/qualifiers or proof terms.
2. Accepting wrapped recursive payloads such as `List[T]`, `Option[T]`, `Pair[Int, T]`.
3. Reworking `Int` recursion logic or lexicographic driver semantics.

## Proposed rule (v1)

For a recur target component type `T` with constructors in source order `C0, C1, ..., Cn`:

A recursive call argument for that component is accepted as `Smaller` if either existing rules already accept it, or all conditions below hold:

1. The current branch definitely proves the current component is constructor `Ccur`.
2. The recursive argument expression is constructor application `Cnext(args...)` of the same ADT `T`.
3. `index(Cnext) < index(Ccur)` using `DefinedType.constructors` order.
4. For each instantiated field type `Fi` of `Cnext`:
   1. If `Fi` does not contain `T`: accept any expression.
   2. If `Fi` is exactly `T`: argument must be a local name in this branch’s strict-smaller set for that target component.
   3. If `Fi` contains `T` but `Fi != T`: reject in v1.
5. Lexicographic tuple handling is unchanged; this only contributes an additional per-component `Smaller` classification.

Notes:

1. Zero-arg variants satisfy condition 4 vacuously.
2. If constructor identity is not definitely known (wildcard/union/ambiguous branch), this rule does not apply.

## Architecture changes

### 1. Extend branch constructor evidence in recursion state

File: `core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala`

Current branch state tracks constructor evidence only for singleton equality checks. For #2025 we need definite current constructor evidence for all constructor shapes.

Planned change:

1. Track `currentCtorPerTarget: NonEmptyList[Option[KnownCtor]]` in `InRecurBranch` (or broaden existing constructor-field semantics to carry definite current constructor identity per target).
2. Build this from compiled branch patterns (via existing target-part splitting helpers), stripping `Named`/`Annotation` wrappers.
3. Mark constructor as known only for unambiguous constructor patterns (`Pattern.PositionalStruct(...)`, plus existing empty-list singleton normalization).

This keeps branch-proof local and explicit, matching current checker architecture.

### 2. Add constructor-application extraction for recursive args

File: `core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala`

Add helper to recognize recursive call argument shape:

1. `Global(pack, constructor)` (zero-arg constructor value), and
2. `App(Global(pack, constructor), args...)` (including nested app groups after wrapper stripping).

Helper returns constructor identity plus flattened argument expressions in call order. If extraction fails, constructor-rank rule is not applicable.

### 3. Add constructor rank and field-type instantiation lookup

File: `core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala`

Using `TypeEnv.getConstructor` and `DefinedType.constructors`:

1. Resolve owner `DefinedType` for both `Ccur` and `Cnext`.
2. Require both constructors belong to the same ADT as recur target type `T`.
3. Compute constructor index by source-order position in `DefinedType.constructors`.
4. Instantiate `Cnext` field types with recur-target type arguments:
   1. derive target root/args from `targetItemType(... )` + `Type.unapplyAll`,
   2. substitute ADT type params in constructor field types via `Type.substituteVar`.

If lookup or instantiation invariants fail, classification remains `Other` (conservative fallback).

### 4. Add target-type containment checks for field safety

File: `core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala`

Add helpers:

1. `containsTargetType(fieldType, targetType): Boolean` (recursive walk over `Type` using `sameAs` for exact-type matches).
2. `fieldPayloadAllowed(fieldType, fieldArgExpr, allowedSmallerNames, targetType): Boolean` implementing v1 constraints:
   1. no target occurrence => allowed,
   2. exact target type => only allowed-smaller local name,
   3. wrapped target occurrence => rejected.

This is the soundness gate that blocks container-mediated cycles in v1.

### 5. Integrate rule into structural argument classification

File: `core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala`

In `classifyStructuralArg`:

1. Keep existing outcomes first:
   1. local strict substructure => `Smaller`,
   2. same target/equal alias/singleton-equality => `Equal`.
2. Before returning `Other`, attempt constructor-rank classification using:
   1. branch current constructor evidence,
   2. recursive argument constructor app,
   3. rank comparison,
   4. field safety checks.
3. Return `Smaller` only when constructor-rank rule passes; otherwise keep `Other`.

`classifyIntArg` and lexicographic control flow remain unchanged except state plumbing for constructor evidence.

## Detailed implementation plan

1. Update branch-state data and state-transition plumbing:
   1. extend `InRecurBranch` constructor evidence field(s),
   2. update branch enter/exit and temporary-state helpers to carry the new data.
2. Add constructor extraction helpers:
   1. current-branch constructor extraction from typed pattern parts,
   2. recursive-arg constructor application extraction from typed expressions.
3. Add constructor metadata helpers:
   1. resolve constructor owner ADT + source-order index,
   2. instantiate constructor field types for current target type arguments.
4. Add recursive-type containment helpers:
   1. `containsTargetType`,
   2. exact-target vs wrapped-target field classifier.
5. Add `classifyConstructorRankArg` and invoke it in `classifyStructuralArg` before `Other`.
6. Keep `Int` and lexicographic driver logic unchanged; verify regression safety with existing tests.
7. Update recursion docs to describe constructor-rank decreases and v1 wrapped-payload limitation.

## Testing plan

Primary file: `core/src/test/scala/dev/bosatsu/TypedExprRecursionCheckTest.scala`

Add tests for issue #2025 cases:

1. Accept `Option`: `Some(_) -> None`.
2. Accept list-like ADT: `NonEmptyList(_,_) -> EmptyList`.
3. Accept mixed rank/structural path: `Even(prev) -> Odd(prev)` and `Odd(prev) -> prev`.
4. Reject wrapped recursive payload (`Fi` contains `T` but `Fi != T`), including `A(xs: List[T])`-style container construction.
5. Reject exact recursive payload when argument is not an allowed smaller local name (`A(B(x))` style when `Fi == T`).
6. Confirm wildcard/union branch without definite current constructor does not unlock constructor-rank rule.
7. Existing recursion suite remains green (structural + lexicographic + `Int`).

Optional hardening:

1. Add table-driven regressions for zero-arg constructor rank transitions across multiple ADTs.
2. Add negative case where constructor belongs to different ADT than the recur target type.

## Acceptance criteria

1. `TypedExprRecursionCheck` accepts constructor-rank decreases that satisfy the v1 rule.
2. Existing structural and lexicographic behavior is unchanged for previously accepted programs.
3. Existing `Int` recursion obligations and diagnostics are unchanged.
4. Zero-arg constructor-rank decreases are accepted without syntax changes.
5. Wrapped recursive payload fields (`Fi` contains `T` but `Fi != T`) are rejected.
6. Direct recursive payload fields (`Fi == T`) require branch-proven strictly-smaller local names.
7. Ambiguous branch constructor knowledge does not enable the new rule.
8. New issue-specific tests are added in `TypedExprRecursionCheckTest` and pass.
9. Existing recursion-related test suites remain green.
10. Recursion documentation is updated to reflect new admissible decreases and v1 limits.

## Risks and mitigations

1. Risk: Unsound acceptance if containment check misses nested target occurrences.
   Mitigation: full recursive `Type` traversal; on uncertainty/failure, classify as `Other`.

2. Risk: Incorrect rank or type instantiation for constructors with generics/existentials.
   Mitigation: enforce ADT/root/type-arity checks and conservative fallback when invariants do not hold.

3. Risk: Regression in existing singleton/equality behavior.
   Mitigation: keep existing path precedence and retain current singleton regression tests.

4. Risk: User confusion when some syntactically similar cases remain rejected.
   Mitigation: document v1 boundary clearly (wrapped recursive payloads intentionally rejected).

5. Risk: Added per-call classification overhead.
   Mitigation: keep helper logic linear and local; precompute constructor-index map from `TypeEnv` if profiling shows hot spots.

## Rollout notes

1. No parser or type-system migration is required.
2. This is compile-time checker behavior only; runtime/codegen unchanged.
3. Land as a single PR with tests and docs; no feature flag planned.
4. Rollback is simple if needed: remove constructor-rank classification call from `classifyStructuralArg`.
5. Update `docs/src/main/paradox/recursion.md` in the same PR to keep user docs aligned.

## Decision

Proceed with v1 constructor-rank decrease support in `TypedExprRecursionCheck`, preserving current structural/lexicographic/`Int` rules and enforcing conservative recursive-payload constraints for soundness.
