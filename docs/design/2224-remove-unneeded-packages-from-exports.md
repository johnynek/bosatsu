---
issue: 2224
priority: 3
touch_paths:
  - docs/design/2224-remove-unneeded-packages-from-exports.md
  - test_workspace/Bosatsu/Json.bosatsu
  - test_workspace/core_alpha_conf.json
  - core/src/test/scala/dev/bosatsu/library/LibConfigTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-20T23:01:23Z
---

# Issue #2224 Design: Minimize `core_alpha` Exports

_Issue: #2224 (https://github.com/johnynek/bosatsu/issues/2224)_

## Summary

Reduce `core_alpha`'s public surface to compiler-known packages, command-known or intentionally public utility packages, packages with runtime-backed `external` APIs, and the small local dependency closure required by those packages. Refactor `Bosatsu/Json` to replace `Nat`-based fuel with `Int`-based recursion so `Bosatsu/Num/Nat` does not remain exported solely as an implementation dependency, then lock the smaller allowlist in `core_alpha_conf.json` with regression coverage.

## Context

`core_alpha` currently exports 23 packages from `test_workspace/core_alpha_conf.json`. Only a smaller subset is required by the compiler, by command-level semantics, or by runtime-backed package APIs:

- `Bosatsu/Lazy` and `Bosatsu/Eval` are referenced by trusted recursion/evaluation paths.
- `Bosatsu/Prog` is the canonical effect, main-entry, and prog-test package.
- `Bosatsu/Json` is recognized directly by JSON encoding/decoding code paths, including special handling for `Json`, `Optional`, and `Nullable`.
- `Bosatsu/IO/Std` is the standard I/O surface exposed on top of `Prog` and `IO/Core`.
- `Bosatsu/Collection/Array`, `Bosatsu/IO/Bytes`, `Bosatsu/IO/Core`, and `Bosatsu/Num/Float64` define runtime-backed `external` APIs.

Today `Bosatsu/Json` also imports `Bosatsu/Num/Nat` to model parser/render fuel. That dependency is an implementation detail rather than intended public surface, so the design should remove it by rewriting the local JSON helpers instead of exporting `Bosatsu/Num/Nat`.

Everything else in `test_workspace` can remain part of the built library as internal packages, but it does not need to remain in the exported interface set.

## Problem

The current export list is broader than the intended public contract. Every exported package expands:

- semver surface area,
- downstream dependency/import surface,
- public review and documentation burden.

The goal is to minimize exports without breaking compiler-known behavior, command-facing semantics, or the runtime-backed APIs that Bosatsu intentionally treats as part of core.

## Goals

1. Reduce `core_alpha` exports to the minimum stable public set.
2. Keep all compiler-known packages exported.
3. Keep packages that Bosatsu commands know about or that serve as the intended standard-library surface for core workflows.
4. Keep packages with runtime-backed `external` APIs exported.
5. Prefer small local refactors over exporting helper packages when a dependency exists only for implementation convenience.
6. Keep only the local package closure required by those exported packages after those refactors.
7. Lock the resulting allowlist in tests.
8. Treat the shrink as a breaking change and version it accordingly.

## Non-goals

1. Removing packages from `test_workspace` or from `all_packages`.
2. Auto-generating `exported_packages` at build time.
3. Changing compiler trust rules for `Bosatsu/Lazy`, `Bosatsu/Eval`, or `Bosatsu/Prog`.
4. Changing `Bosatsu/Json`'s public API or JSON behavior beyond swapping `Nat` fuel for equivalent `Int` fuel.
5. Reorganizing higher-level utility packages into a separate library in this change.

## Proposed Export Policy

Use an explicit allowlist, but derive it once from a simple rule:

1. Seed the allowlist with compiler-known packages, command-known or intentionally public utility packages, and packages that define `external` APIs.
2. If a seed package depends on another package only for an internal implementation technique, prefer a local refactor over exporting that dependency.
3. Add any remaining local package that appears in the exported-package dependency closure under existing `LibConfig.validatePacks` rules.
4. Keep the result explicit in `core_alpha_conf.json`; do not add new production code to infer it on every build.

### Seed packages

- `Bosatsu/Prog`
- `Bosatsu/Lazy`
- `Bosatsu/Eval`
- `Bosatsu/Json`
- `Bosatsu/IO/Std`
- `Bosatsu/Collection/Array`
- `Bosatsu/IO/Bytes`
- `Bosatsu/IO/Core`
- `Bosatsu/Num/Float64`

### `Bosatsu/Json` refactor

`Bosatsu/Json` should stay exported because the JSON commands know about its `Json`, `Optional`, and `Nullable` ADTs. The package should stop depending on `Bosatsu/Num/Nat` by rewriting its local fuel helpers to use `Int` countdown recursion instead:

- Replace helper parameters such as `fuel: Nat` with `budget: Int`.
- Replace `Zero`/`Succ` pattern matching with `cmp_Int(budget, 0) matches LT | EQ` and `budget.sub(1)`.
- Preserve the same initial bounds that already exist in the file, only expressed as `Int` values.
- Keep the exported `Bosatsu/Json` API and observable parse/render behavior unchanged.

### Closure additions

After the `Bosatsu/Json` refactor, the current source tree adds two more packages that must remain exported for the seed set to stay valid:

- `Bosatsu/Char`
  Reason: exported helpers in `Bosatsu/Collection/Array`, `Bosatsu/IO/Core`, and `Bosatsu/Json` call into `Bosatsu/Char`.
- `Bosatsu/IO/Error`
  Reason: `Bosatsu/IO/Core` and `Bosatsu/IO/Std` expose `Prog[IOError, ...]` APIs.

Notably absent from the closure:

- `Bosatsu/List`, `Bosatsu/Option`, and `Bosatsu/Bool`
  Reason: `List`, `Option`, `Bool`, `Char`, `Int`, `String`, and `Float64` types come from predef/type space; exporting helper packages is not required just because those types appear in signatures.
- `Bosatsu/Rand` and `Bosatsu/Testing/Properties`
  Reason: they are only used by test code inside packages such as `Bosatsu/Collection/Array`.
- `Bosatsu/Num/Nat`, `Bosatsu/Num/BinNat`, `Bosatsu/Num/Binary`, `Bosatsu/Dict`, `Bosatsu/Collection/Queue`, `Bosatsu/Collection/TreeList`, `Bosatsu/Nothing`
  Reason: after the `Bosatsu/Json` refactor, they are higher-level pure packages, not compiler-known, command-known, or runtime-backed surface required by the seed set.

### Target exported package set

`core_alpha` should export exactly:

- `Bosatsu/Char`
- `Bosatsu/Collection/Array`
- `Bosatsu/Eval`
- `Bosatsu/IO/Bytes`
- `Bosatsu/IO/Core`
- `Bosatsu/IO/Error`
- `Bosatsu/IO/Std`
- `Bosatsu/Json`
- `Bosatsu/Lazy`
- `Bosatsu/Num/Float64`
- `Bosatsu/Prog`

This reduces the public surface from 23 packages to 11 while preserving the compiler/runtime contract and the command-facing `Json` and standard-I/O APIs.

## Implementation Plan

1. Update `test_workspace/Bosatsu/Json.bosatsu`.
   - Replace `Nat`-based fuel helpers with `Int`-based countdown recursion.
   - Preserve the same guard behavior and exported API surface.
   - Remove the import of `Bosatsu/Num/Nat`.

2. Update `test_workspace/core_alpha_conf.json`.
   - Replace the current `exported_packages` list with the 11-package allowlist above.
   - Keep `all_packages` unchanged so non-exported packages still compile and ship as internal packages.
   - Bump `next_version` from `5.1.0` to the next major version because removing exported packages is an API break.

3. Add a regression test for the allowlist.
   - In `core/src/test/scala/dev/bosatsu/library/LibConfigTest.scala`, load and parse `test_workspace/core_alpha_conf.json`.
   - Assert that `exported_packages` exactly matches the documented allowlist.
   - Assert that `Bosatsu/Num/Nat` is not part of the retained export set.
   - Assert that the configured version bump is major relative to the previous released version.
   - Include an assertion comment or note that `Bosatsu/Json` is command-known, `Bosatsu/IO/Std` is intentionally retained public surface, and `Bosatsu/Num/Nat` is intentionally absent after the JSON refactor.

4. Add an interface-visibility integration test.
   - In `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala`, assemble/build a library using the trimmed export list.
   - Verify a downstream source can import retained packages such as `Bosatsu/Json`, `Bosatsu/IO/Std`, `Bosatsu/IO/Core`, or `Bosatsu/Prog`.
   - Verify a downstream source cannot import a removed package such as `Bosatsu/Num/Nat`, `Bosatsu/Dict`, or `Bosatsu/Collection/Queue` through the dependency interface.

5. Re-run targeted library tests.
   - `LibConfigTest` for config and semver coverage.
   - `ToolAndLibCommandTest` for dependency visibility coverage.
   - Existing `core_alpha`-oriented flows that exercise compiler-known/runtime packages, especially `Prog`, `Lazy`, `Eval`, array/bytes JSON support, `Bosatsu/Json` command behavior, `Bosatsu/IO/Std` evaluation flows, and any `lib check` or `lib build` path using `test_workspace/core_alpha_conf.json`.
   - Existing JSON parse/render tests in `test_workspace/Bosatsu/Json.bosatsu` to confirm the `Int`-based fuel rewrite does not change behavior.

No compiler or command implementation changes are expected unless the new tests expose an existing assumption that exported and internal packages are equivalent.

## Acceptance Criteria

- `test_workspace/Bosatsu/Json.bosatsu` no longer imports `Bosatsu/Num/Nat`; its bounded recursion uses `Int` countdown logic instead.
- `test_workspace/core_alpha_conf.json` exports exactly the 11-package allowlist documented above.
- `test_workspace/core_alpha_conf.json` keeps `all_packages` broad enough that non-exported packages still build as internal packages.
- The configured `next_version` is a major bump from the previous released `core_alpha` version.
- `LibConfig.validatePacks` and `lib check` succeed with the trimmed export list; no exported package depends on a now-private local package.
- A regression test locks the expected `core_alpha` export set.
- An integration test proves retained packages remain importable as dependency interfaces and at least one removed package, including `Bosatsu/Num/Nat`, no longer is.
- Existing compiler-known/runtime-backed behaviors for `Bosatsu/Prog`, `Bosatsu/Lazy`, `Bosatsu/Eval`, `Bosatsu/Collection/Array`, `Bosatsu/IO/Bytes`, `Bosatsu/IO/Core`, and `Bosatsu/Num/Float64` continue to pass their current tests.
- Existing JSON-command and standard-I/O behaviors for `Bosatsu/Json` and `Bosatsu/IO/Std` continue to pass their current tests.

## Risks

1. Downstream breakage for users importing removed packages such as `Bosatsu/Dict`, `Bosatsu/Collection/Queue`, or property-testing helpers.
   Mitigation: ship this only as a major version and call out the removed export list explicitly in release notes.

2. The `Bosatsu/Json` refactor could change parser/render termination behavior or error results.
   Mitigation: keep the same budget formulas, translate the `Nat` countdown mechanically to `Int` countdown, and rerun the existing `Bosatsu/Json` parse/render tests plus JSON command tests.

3. Accidentally omitting a package still required by exported-package bodies.
   Mitigation: rely on existing `LibConfig.validatePacks` checks and add the downstream import/export integration test.

4. Public docs or examples may still mention packages that are no longer exported.
   Mitigation: audit generated docs as part of release preparation, and if necessary switch core-alpha doc generation to exported-only/private-excluded mode in a follow-up.

5. Users may assume non-exported packages disappeared entirely.
   Mitigation: document that this change narrows dependency interfaces; packages remain internal to the built library unless a later issue removes or relocates them.

## Rollout Notes

- Release the change as the next major `core_alpha` artifact.
- Include a migration note listing removed exports and recommending a follow-up higher-level library if packages like `Bosatsu/Rand` or `Bosatsu/Testing/Properties` still need a supported public home.
- Verify published docs and examples do not present newly-private packages as part of the supported core surface.
- Keep this change narrowly scoped to export curation; moving removed packages into a separate stdlib-style library can happen independently later.
