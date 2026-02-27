---
issue: 1818
priority: 3
touch_paths:
  - docs/design/1818-make-sure-synthetic-names-are-unambiguously-identifiable.md
  - core/src/main/scala/dev/bosatsu/Identifier.scala
  - core/src/main/scala/dev/bosatsu/Expr.scala
  - core/src/main/scala/dev/bosatsu/TypedExpr.scala
  - core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala
  - core/src/main/scala/dev/bosatsu/TypedExprLoopRecurLowering.scala
  - core/src/main/scala/dev/bosatsu/Matchless.scala
  - core/src/main/scala/dev/bosatsu/SourceConverter.scala
  - core/src/main/scala/dev/bosatsu/ProtoConverter.scala
  - core/src/test/scala/dev/bosatsu/IdentifierTest.scala
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/ProtoConverterTest.scala
  - core/src/test/scala/dev/bosatsu/MatchlessTests.scala
depends_on: []
estimated_size: M
generated_at: 2026-02-27T01:53:28Z
---

# Issue #1818 Design: Make sure synthetic names are unambiguously identifiable

_Issue: #1818 (https://github.com/johnynek/bosatsu/issues/1818)_

## Summary

Design doc proposing an explicit Synthetic identifier subtype, synthetic-aware/backward-compatible identifier identity semantics, centralized synthetic fresh-name iteration, migration of internal name-generation call sites, protobuf round-trip coverage, and compatibility/rollout guidance for issue #1818.

---
issue: 1818
title: Make sure synthetic names are unambiguously identifiable
status: proposed
base_branch: main
touch_paths:
  - docs/design/1818-make-sure-synthetic-names-are-unambiguously-identifiable.md
  - core/src/main/scala/dev/bosatsu/Identifier.scala
  - core/src/main/scala/dev/bosatsu/Expr.scala
  - core/src/main/scala/dev/bosatsu/TypedExpr.scala
  - core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala
  - core/src/main/scala/dev/bosatsu/TypedExprLoopRecurLowering.scala
  - core/src/main/scala/dev/bosatsu/Matchless.scala
  - core/src/main/scala/dev/bosatsu/SourceConverter.scala
  - core/src/main/scala/dev/bosatsu/ProtoConverter.scala
  - core/src/test/scala/dev/bosatsu/IdentifierTest.scala
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/ProtoConverterTest.scala
  - core/src/test/scala/dev/bosatsu/MatchlessTests.scala
depends_on: []
estimated_size: M
generated_at: 2026-02-27
---

# Issue #1818 Design: make sure synthetic names are unambiguously identifiable

Issue: #1818 (https://github.com/johnynek/bosatsu/issues/1818)
Base branch: main

## Summary

Add an explicit `Identifier.Synthetic` bindable kind, make synthetic identity unambiguous while preserving current non-synthetic equality behavior, centralize fresh synthetic-name generation under `Identifier.Bindable`, migrate compiler-internal fresh-name call sites to that API, and add protobuf round-trip tests proving synthetics and backticked underscore names remain distinct.

This keeps protobuf schema unchanged and preserves compatibility for valid existing artifacts. Legacy artifacts that already lost information due prior collisions are treated as best-effort decode.

## Problem statement

Today synthetic names are represented as `Identifier.Name("_...")` and distinguished by convention.

That creates three concrete problems:

1. Synthetic names are currently represented as `Name("_x")`, so they are not disjoint from user names that serialize to the same text (for example `` `_x` ``), and can collide in `Map`/`Set`/`SortedMap` paths where we need synthetic-vs-user distinction.
2. Internal fresh-name generation is inconsistent: some paths use `Expr.nameIterator` (plain names), some build synthetic names ad hoc, and the policy is not centralized.
3. Protobuf tests do not directly assert that underscore synthetics and backticked underscore names survive serialization/deserialization as distinct binders.

## Goals

1. Represent synthetic binders as a first-class subtype.
2. Make synthetic identifier identity unambiguous across equality, hashing, and ordering without breaking existing non-synthetic compatibility.
3. Standardize fresh compiler-internal naming on synthetic binders.
4. Preserve protobuf compatibility for valid existing serialized data.
5. Add targeted tests for parser and protobuf collision scenarios.

## Non-goals

1. No parser syntax change for user-authored identifiers.
2. No protobuf schema/version bump in this issue.
3. No attempt to recover semantic intent from already-corrupted legacy artifacts beyond deterministic best effort.
4. No broad rename of existing user-facing names or diagnostics format beyond what falls out of synthetic naming changes.

## Proposed architecture

### 1) Introduce explicit `Identifier.Synthetic`

In `Identifier.scala`:

1. Add `final case class Synthetic(asString: String) extends Bindable`.
2. Update `Identifier.synthetic(name)` to return `Synthetic` rather than `Name`.
3. Keep `sourceCodeRepr` for synthetic names in underscore form (`_foo`) so wire format remains stable.
4. Update `bindableWithSynthetic` and `parserWithSynthetic` so underscore-prefixed synthetic payloads decode as `Synthetic`.
5. Keep `Identifier.isSynthetic` compatible by recognizing both `Synthetic(_)` and legacy `Name` values that start with underscore.

Compatibility note:

Valid historical data that encoded synthetics as underscore tokens remains decodable. If older compilation paths already collapsed a backticked underscore name into bare underscore text, that artifact was already semantically inconsistent; decode remains deterministic but cannot perfectly recover original intent.

### 2) Make identifier identity synthetic-aware and backward compatible

`Identifier` currently compares by `asString` only. We should keep that behavior for non-synthetic identifiers, and only make synthetic names disjoint from everything else.

Proposed equality/ordering/hash contract:

1. `Synthetic(sa)` equals `Synthetic(sb)` iff `sa == sb`.
2. `Synthetic(_)` is never equal to any non-synthetic identifier.
3. For non-synthetic pairs (`Name`, `Backticked`, `Operator`, `Constructor`), preserve current behavior: equality/order/hash remain based on existing text identity (`asString`), so established equivalences (for example operator/backticked interop where currently relied upon) keep working.
4. Implement the same split policy in `Order[Identifier]` and `Hashable[Identifier]` so set/map behavior remains coherent.

Result: `Synthetic("_x")` and `Backticked("_x")` are always distinct keys, while pre-existing non-synthetic equality semantics remain compatible.

### 3) Centralize synthetic fresh-name iteration in `Identifier.Bindable`

Add a canonical synthetic iterator API in `Identifier.Bindable`.

1. `syntheticIterator`: yields synthetics in small deterministic order (`_a`, `_b`, ..., `_a0`, ...), mirroring current binder progression.
2. `freshSyntheticIterator(avoid)`: convenience wrapper equivalent to `syntheticIterator.filterNot(avoid)`.
3. Optional prefixed helper for places that currently use readable prefixes (for example Matchless), while still producing `Synthetic` values.

`Expr.nameIterator` should become either:

1. A thin alias to the new synthetic iterator (temporary compatibility path), or
2. Removed after direct call-site migration in the same PR.

Preferred in this issue: migrate direct call sites and keep a short-lived alias only if needed to reduce churn.

### 4) Migrate compiler-internal fresh-name call sites

Replace internal uses of non-synthetic generation with `Identifier.Bindable` synthetic iteration.

Primary paths:

1. `TypedExprLoopRecurLowering.scala` helper/lambda argument freshening.
2. `TypedExprNormalization.scala` CSE and closure-capture fresh names.
3. `TypedExpr.scala` eta-expansion helper names currently using `Type.allBinders` + `Name`.
4. `Matchless.scala` helper names generated through local `freshSyntheticNames`.
5. `SourceConverter.scala` synthetic iterator call sites that should use one shared API.

This enforces one policy: compiler-internal names are synthetic unless they intentionally model user names.

### 5) Protobuf round-trip contract

No schema change is required.

1. Encode path continues storing identifier text via `sourceCodeRepr`.
2. Decode path uses synthetic-aware parsing to reconstruct `Synthetic` versus `Backticked` correctly when representation is unambiguous.
3. Add explicit tests that include both `Synthetic("_x")` and `Backticked("_x")` in the same serialized payload and verify exact round-trip identity.

## Detailed implementation plan

1. Add `Identifier.Synthetic` and wire it into `Document`, parser helpers, and `Identifier.synthetic`.
2. Change identifier equality/hash/order/hashable to a hybrid policy: synthetic-vs-non-synthetic disjointness, non-synthetic compatibility preserved.
3. Add `Identifier.Bindable.syntheticIterator` and filtered fresh-name helper(s).
4. Update `Expr.nameIterator` to delegate to `Identifier.Bindable.syntheticIterator` or remove it after migration.
5. Migrate `TypedExprLoopRecurLowering` fresh-name sites.
6. Migrate `TypedExprNormalization` fresh-name sites.
7. Migrate `TypedExpr` eta-expansion fresh-name site.
8. Migrate `Matchless` fresh-name helper to shared API while preserving deterministic output behavior expected by tests.
9. Migrate `SourceConverter` synthetic iterator usage to the same API.
10. Verify `ProtoConverter` decode helpers continue using synthetic-aware parser entry points and adjust if needed for the new subtype.
11. Add `Identifier` unit tests for hybrid identity behavior (synthetic disjointness plus non-synthetic compatibility).
12. Extend parser/protobuf tests with explicit synthetic-vs-backticked underscore cases.
13. Run full core test suite, then update any deterministic-name assertions that intentionally check generated helper names.

## Testing strategy

### Identifier and parser tests

1. `Synthetic("_x") != Backticked("_x")` and both coexist in `Set`/`Map`/`SortedSet`.
2. `Identifier.isSynthetic` is true for `Synthetic` and legacy underscore `Name`.
3. `bindableWithSynthetic` parses `_x` as `Synthetic` and `` `_x` `` as `Backticked`.
4. `sourceCodeRepr` round-trips for all bindable kinds including `Synthetic`.
5. Existing non-synthetic equivalence behavior remains intact (for example forms currently considered equal by `asString` continue to compare equal).

### Protobuf tests

1. TypedExpr round-trip with both synthetic and backticked underscore binders preserves distinct identities.
2. Package/interface round-trip paths that include synthetic binders continue to decode successfully.
3. Legacy decode smoke check: previously serialized payloads without explicit synthetic subtype still decode.

### Regression tests for fresh-name policy

1. Lowering/normalization passes that allocate internal names produce only synthetics.
2. Existing invariants (no capture, no collisions) remain true.
3. Matchless recovery tests continue to pass with deterministic fresh-name generation.

## Acceptance criteria

1. `Identifier` has an explicit `Synthetic` bindable subtype.
2. `Identifier.synthetic(...)` returns `Synthetic`.
3. Identifier equality, hashCode, ordering, and hashable make `Synthetic` disjoint from non-synthetic identifiers while preserving existing non-synthetic `asString` compatibility behavior.
4. `_foo` synthetic and `` `_foo` `` backticked names are representable simultaneously without collection-key collisions.
5. A standard synthetic iterator exists in `Identifier.Bindable` and is used by compiler internal-name generation paths.
6. `Expr.nameIterator` no longer drives non-synthetic internal-name allocation.
7. TypedExpr normalization/lowering and Matchless fresh-name call sites use the standardized synthetic API.
8. Protobuf schema is unchanged.
9. Protobuf round-trip tests explicitly cover synthetic vs backticked underscore names and pass.
10. Existing serialized artifacts from valid historical outputs remain decodable.
11. All touched core tests pass.

## Risks and mitigations

1. Risk: equality/ordering implementation could accidentally change non-synthetic behavior.
Mitigation: encode the hybrid contract in tests (including non-synthetic compatibility checks) and run full suite to catch regressions.

2. Risk: fresh-name policy changes can churn deterministic test expectations.
Mitigation: preserve deterministic iterator order, keep optional prefixed helper where readability matters, and update only assertions tied to internal helper names.

3. Risk: subtle backward-compatibility edge cases in already-inconsistent legacy artifacts.
Mitigation: keep wire format unchanged, decode deterministically, and document that previously-collided artifacts cannot be perfectly reconstructed.

4. Risk: incomplete migration leaves mixed synthetic and non-synthetic internal naming.
Mitigation: grep-based migration checklist in the PR (`Expr.nameIterator`, `Type.allBinders.iterator` bindable sites) plus targeted tests.

## Rollout notes

1. Land this as a single PR with model change, call-site migrations, and tests together to avoid intermediate inconsistent behavior.
2. Keep protobuf schema stable; no migration tooling is required.
3. After merge, monitor for downstream test failures that assert exact internal names and update those tests to new synthetic outputs where appropriate.
4. Document the exact compatibility boundary in PR notes: only synthetic-vs-non-synthetic equality changes; non-synthetic behavior is intentionally preserved.
