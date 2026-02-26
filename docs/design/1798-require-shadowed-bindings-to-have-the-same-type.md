---
issue: 1798
priority: 3
touch_paths:
  - docs/design/1798-require-shadowed-bindings-to-have-the-same-type.md
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/ShadowedBindingTypeCheck.scala
  - core/src/test/scala/dev/bosatsu/ShadowedBindingTypeCheckTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-02-25T18:11:09Z
---

# Issue #1798 Design Doc: Require Shadowed Bindings To Have The Same Type

_Issue: #1798 (https://github.com/johnynek/bosatsu/issues/1798)_

## Summary

Proposes a new post-typechecking lint pass over TypedExpr that enforces semantic type equality for shadowed binders, defines quantified-type handling via Type.sameAs, rejects widening-only shadowing, and integrates errors into PackageError with tests and rollout guidance.

---
issue: 1798
title: Require shadowed bindings to have the same type
status: proposed
base_branch: main
touch_paths:
  - docs/design/1798-require-shadowed-bindings-to-have-the-same-type.md
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/ShadowedBindingTypeCheck.scala
  - core/src/test/scala/dev/bosatsu/ShadowedBindingTypeCheckTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-02-25
---

# Issue #1798 Design: require shadowed bindings to have the same type

Issue: #1798 (https://github.com/johnynek/bosatsu/issues/1798)
Base branch: main

## Summary

Add a post-typechecking lint that rejects term-level shadowing when the new binding type is not the same type as the shadowed binding type. Equality is semantic via `Type.sameAs`, so alpha-renamed and reordered quantifiers compare equal. Widening-only shadowing is not allowed in this issue.

This is lint-only and does not change evaluation, lowering, or code generation.

## Context

Bosatsu allows local shadowing for immutable-style rebinding, and the language guide currently recommends not changing a name type when shadowing. Issue #1798 asks to enforce this after typechecking, and asks explicitly how quantified types should be handled and whether the policy should be `previous_type <:< current_type` or `=:=`.

The compiler already has post-typechecking typed checks (`TypedExprRecursionCheck`) in `Package.inferBodyUnopt`, so the same phase boundary is appropriate for this rule.

## Goals

1. Enforce same-type shadowing for term binders after successful inference.
2. Keep the feature lint-only with no runtime or codegen impact.
3. Handle quantified types deterministically and predictably.
4. Return clear, source-anchored diagnostics that show both types.

## Non-goals

1. No parser or syntax changes.
2. No warning mode or compile option flag in this issue.
3. No broad import-shadow policy redesign in this issue.
4. No widening-only acceptance mode in this issue.

## Decision: exact equality, not widening

Use `previous =:= current`, implemented as `previous.sameAs(current)`.

Rationale:

1. The issue asks for same type, not compatible type.
2. `Type.sameAs` already handles normalization, alpha-renaming, and quantifier-order insensitivity.
3. Allowing widening would require a separate subsumption check with kind validation and would be less obvious to users.
4. Exact equality gives a simple and readable language rule.

Examples:

1. Allowed: `forall a. a -> a` shadowed by `forall b. b -> b`.
2. Rejected: `forall a. a -> a` shadowed by `Int -> Int`.
3. Rejected: any case where only `previous <:< current` holds.

## Architecture

### 1) New typed lint module

Add `core/src/main/scala/dev/bosatsu/ShadowedBindingTypeCheck.scala` with:

1. `checkLets(pack, lets): ValidatedNec[ShadowedBindingTypeCheck.Error, Unit]`.
2. An error ADT carrying at least:
   1. binding name,
   2. previous type,
   3. current type,
   4. region of the new binding,
   5. optional site metadata for message quality.

Internally, the checker does one lexical traversal with an environment map:

`Map[Identifier.Bindable, BoundInfo]`

where `BoundInfo` holds current in-scope type and source region.

### 2) Binding sites included

The checker compares a new binder against outer scope for:

1. top-level typed let names (tracked in typed let order),
2. `TypedExpr.AnnotatedLambda` args,
3. `TypedExpr.Let` binders,
4. `TypedExpr.Loop` binders,
5. `TypedExpr.Match` pattern binders (including guard scope).

Type source per binder:

1. lambda arg: declared arg type,
2. let binder: `rhs.getType`,
3. loop binder: each loop arg expression `getType`,
4. pattern binders: `Pattern.envOf(branch.pattern, Map.empty)(identity)`.

Scope behavior mirrors existing semantics:

1. recursive let binder is in scope for rhs and body,
2. non-recursive let binder is in scope only for body,
3. pattern binders are in scope for guard and branch body,
4. loop binders are in scope for loop body only.

### 3) Quantified type handling

Comparison is:

`sameShadowType(prev, curr) = prev.sameAs(curr)`

This resolves quantified edge cases by construction:

1. alpha-renamed binders compare equal,
2. quantifier order differences compare equal when semantically equivalent,
3. vacuous quantifier normalization behavior stays consistent with existing type normalization,
4. no meta solving is needed because this runs after inference finalization.

### 4) Pipeline placement

In `Package.inferBodyUnopt`:

1. keep untyped checks (`UnusedLetCheck`, `TotalityCheck`) unchanged,
2. run `Infer.typeCheckLets`,
3. run `TypedExprRecursionCheck` (existing behavior),
4. run `ShadowedBindingTypeCheck`,
5. map errors into `PackageError.ShadowedBindingTypeError`,
6. proceed to lowering/normalization only if typed checks pass.

Keeping recursion first avoids duplicate or noisy shadow diagnostics for recursion-specific illegal-shadow cases.

### 5) Error plumbing

Add `PackageError.ShadowedBindingTypeError` in `core/src/main/scala/dev/bosatsu/PackageError.scala`.

Message requirements:

1. include shadowed name,
2. include previous type and current type,
3. include source context for the new binding region,
4. include concise fix hint: rename binding or keep same type.

## Implementation plan

1. Create `ShadowedBindingTypeCheck.scala` with checker state, traversal, and error ADT.
2. Implement binder extraction helpers for lambda, let, loop, and pattern sites.
3. Wire checker into `Package.inferBodyUnopt` after recursion check.
4. Add package-error rendering for new lint failures.
5. Add focused unit tests in `ShadowedBindingTypeCheckTest.scala`.
6. Add end-to-end failure/message coverage in `ErrorMessageTest.scala`.
7. Update language guide note that currently says this may be enforced in the future.

## Testing strategy

### Unit tests

1. let shadow with same type passes,
2. let shadow with different type fails,
3. lambda arg shadow same type passes,
4. lambda arg shadow different type fails,
5. loop binder shadow different type fails,
6. match pattern binder shadow different type fails,
7. recursive let scope behavior is correct,
8. quantified alpha-equivalent shadows pass,
9. quantified non-equivalent shadows fail.

### Integration tests

1. package compile fails with the new error for a true mismatch,
2. message includes name plus previous/current types,
3. message region points at the shadowing binding site,
4. existing recursion, type, and totality test suites remain green.

## Acceptance criteria

1. A new post-typechecking pass exists at `core/src/main/scala/dev/bosatsu/ShadowedBindingTypeCheck.scala`.
2. The pass is called from `Package.inferBodyUnopt` after successful type inference.
3. The pass checks shadowing for top-level typed lets, lambda args, lets, loop binders, and match pattern binders.
4. Shadow comparison uses semantic type equality via `Type.sameAs`.
5. Alpha-renamed and reordered quantified binders are accepted as equal.
6. Widening-only shadows are rejected.
7. `PackageError` has a dedicated case with user-facing diagnostics.
8. Lowering, normalization, and codegen behavior are unchanged.
9. New checker unit tests exist and pass.
10. `ErrorMessageTest` covers at least one end-to-end mismatch diagnostic.
11. Existing core compiler test suites remain green.
12. Language guide wording is updated to match enforced behavior.

## Risks and mitigations

1. Risk: source compatibility break for code that intentionally changes type across shadows.
   Mitigation: clear diagnostics and straightforward rename-based migration.
2. Risk: duplicate diagnostics with recursion shadow constraints.
   Mitigation: run recursion check first and only run this lint after recursion passes.
3. Risk: coarse regions for some binder forms, especially patterns.
   Mitigation: use current region model now; improve with pattern-region work in issue #132.
4. Risk: potential false negatives for shadowing categories not represented as local typed binders.
   Mitigation: keep this issue focused on typed term binders; follow up separately if import-level policy is desired.
5. Risk: performance overhead.
   Mitigation: single linear traversal per typed let and constant-time scope lookups.

## Rollout notes

1. Land checker, pipeline wiring, diagnostics, and tests in one PR.
2. Ship as hard error immediately in this change (no temporary flag).
3. Update docs in the same PR.
4. Monitor CI and downstream package failures right after merge; expected fixes are local renames.

## Follow-up

1. If needed later, introduce an optional widening policy mode behind explicit configuration.
2. Revisit import-shadow handling policy if language direction requires it.
3. Improve binder-level region precision when issue #132 is addressed.
