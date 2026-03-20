---
issue: 2224
priority: 3
touch_paths:
  - docs/design/2224-remove-unneeded-packages-from-exports.md
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

Reduce `core_alpha`'s public surface to compiler-known packages, packages with runtime-backed `external` APIs, and the small local dependency closure required by those packages. Implement this as an explicit allowlist change in `core_alpha_conf.json`, treat it as a major-version break, and add regression coverage so the trimmed export set does not drift.

## Context

`core_alpha` currently exports 23 packages from `test_workspace/core_alpha_conf.json`. Only a smaller subset is required by the compiler or by runtime-backed package APIs:

- `Bosatsu/Lazy` and `Bosatsu/Eval` are referenced by trusted recursion/evaluation paths.
- `Bosatsu/Prog` is the canonical effect, main-entry, and prog-test package.
- `Bosatsu/Collection/Array`, `Bosatsu/IO/Bytes`, `Bosatsu/IO/Core`, and `Bosatsu/Num/Float64` define runtime-backed `external` APIs.

Everything else in `test_workspace` can remain part of the built library as internal packages, but it does not need to remain in the exported interface set.

## Problem

The current export list is broader than the intended public contract. Every exported package expands:

- semver surface area,
- downstream dependency/import surface,
- public review and documentation burden.

The goal is to minimize exports without breaking compiler-known behavior or the runtime-backed APIs that Bosatsu intentionally treats as part of core.

## Goals

1. Reduce `core_alpha` exports to the minimum stable public set.
2. Keep all compiler-known packages exported.
3. Keep packages with runtime-backed `external` APIs exported.
4. Keep only the local package closure required by those exported packages.
5. Lock the resulting allowlist in tests.
6. Treat the shrink as a breaking change and version it accordingly.

## Non-goals

1. Removing packages from `test_workspace` or from `all_packages`.
2. Auto-generating `exported_packages` at build time.
3. Changing compiler trust rules for `Bosatsu/Lazy`, `Bosatsu/Eval`, or `Bosatsu/Prog`.
4. Reorganizing higher-level utility packages into a separate library in this change.

## Proposed Export Policy

Use an explicit allowlist, but derive it once from a simple rule:

1. Seed the allowlist with compiler-known packages and packages that define `external` APIs.
2. Add any local package that appears in the exported-package dependency closure under existing `LibConfig.validatePacks` rules.
3. Keep the result explicit in `core_alpha_conf.json`; do not add new production code to infer it on every build.

### Seed packages

- `Bosatsu/Prog`
- `Bosatsu/Lazy`
- `Bosatsu/Eval`
- `Bosatsu/Collection/Array`
- `Bosatsu/IO/Bytes`
- `Bosatsu/IO/Core`
- `Bosatsu/Num/Float64`

### Closure additions

The current source tree adds two more packages that must remain exported for the seed set to stay valid:

- `Bosatsu/Char`
  Reason: exported helpers in `Bosatsu/Collection/Array` and `Bosatsu/IO/Core` call into `Bosatsu/Char`.
- `Bosatsu/IO/Error`
  Reason: `Bosatsu/IO/Core` exposes `Prog[IOError, ...]` APIs.

Notably absent from the closure:

- `Bosatsu/List`, `Bosatsu/Option`, and `Bosatsu/Bool`
  Reason: `List`, `Option`, `Bool`, `Char`, `Int`, `String`, and `Float64` types come from predef/type space; exporting helper packages is not required just because those types appear in signatures.
- `Bosatsu/Rand` and `Bosatsu/Testing/Properties`
  Reason: they are only used by test code inside packages such as `Bosatsu/Collection/Array`.
- `Bosatsu/Json`, `Bosatsu/IO/Std`, `Bosatsu/Num/Nat`, `Bosatsu/Num/BinNat`, `Bosatsu/Num/Binary`, `Bosatsu/Dict`, `Bosatsu/Collection/Queue`, `Bosatsu/Collection/TreeList`, `Bosatsu/Nothing`
  Reason: they are higher-level pure packages, not compiler-known/runtime-backed surface required by the seed set.

### Target exported package set

`core_alpha` should export exactly:

- `Bosatsu/Char`
- `Bosatsu/Collection/Array`
- `Bosatsu/Eval`
- `Bosatsu/IO/Bytes`
- `Bosatsu/IO/Core`
- `Bosatsu/IO/Error`
- `Bosatsu/Lazy`
- `Bosatsu/Num/Float64`
- `Bosatsu/Prog`

This reduces the public surface from 23 packages to 9 while preserving the compiler/runtime contract.

## Implementation Plan

1. Update `test_workspace/core_alpha_conf.json`.
   - Replace the current `exported_packages` list with the 9-package allowlist above.
   - Keep `all_packages` unchanged so non-exported packages still compile and ship as internal packages.
   - Bump `next_version` from `5.1.0` to the next major version because removing exported packages is an API break.

2. Add a regression test for the allowlist.
   - In `core/src/test/scala/dev/bosatsu/library/LibConfigTest.scala`, load and parse `test_workspace/core_alpha_conf.json`.
   - Assert that `exported_packages` exactly matches the documented allowlist.
   - Assert that the configured version bump is major relative to the previous released version.

3. Add an interface-visibility integration test.
   - In `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala`, assemble/build a library using the trimmed export list.
   - Verify a downstream source can import a retained package such as `Bosatsu/IO/Core` or `Bosatsu/Prog`.
   - Verify a downstream source cannot import a removed package such as `Bosatsu/Json` or `Bosatsu/IO/Std` through the dependency interface.

4. Re-run targeted library tests.
   - `LibConfigTest` for config and semver coverage.
   - `ToolAndLibCommandTest` for dependency visibility coverage.
   - Existing `core_alpha`-oriented flows that exercise compiler-known/runtime packages, especially `Prog`, `Lazy`, `Eval`, array/bytes JSON support, and any `lib check` or `lib build` path using `test_workspace/core_alpha_conf.json`.

No production Scala changes are expected unless the new tests expose an existing assumption that exported and internal packages are equivalent.

## Acceptance Criteria

- `test_workspace/core_alpha_conf.json` exports exactly the 9-package allowlist documented above.
- `test_workspace/core_alpha_conf.json` keeps `all_packages` broad enough that non-exported packages still build as internal packages.
- The configured `next_version` is a major bump from the previous released `core_alpha` version.
- `LibConfig.validatePacks` and `lib check` succeed with the trimmed export list; no exported package depends on a now-private local package.
- A regression test locks the expected `core_alpha` export set.
- An integration test proves retained packages remain importable as dependency interfaces and at least one removed package no longer is.
- Existing compiler-known/runtime-backed behaviors for `Bosatsu/Prog`, `Bosatsu/Lazy`, `Bosatsu/Eval`, `Bosatsu/Collection/Array`, `Bosatsu/IO/Bytes`, `Bosatsu/IO/Core`, and `Bosatsu/Num/Float64` continue to pass their current tests.

## Risks

1. Downstream breakage for users importing removed packages such as `Bosatsu/Json`, `Bosatsu/IO/Std`, or property-testing helpers.
   Mitigation: ship this only as a major version and call out the removed export list explicitly in release notes.

2. Accidentally omitting a package still required by exported-package bodies.
   Mitigation: rely on existing `LibConfig.validatePacks` checks and add the downstream import/export integration test.

3. Public docs or examples may still mention packages that are no longer exported.
   Mitigation: audit generated docs as part of release preparation, and if necessary switch core-alpha doc generation to exported-only/private-excluded mode in a follow-up.

4. Users may assume non-exported packages disappeared entirely.
   Mitigation: document that this change narrows dependency interfaces; packages remain internal to the built library unless a later issue removes or relocates them.

## Rollout Notes

- Release the change as the next major `core_alpha` artifact.
- Include a migration note listing removed exports and recommending a follow-up higher-level library if packages like `Bosatsu/Json`, `Bosatsu/Rand`, or `Bosatsu/Testing/Properties` still need a supported public home.
- Verify published docs and examples do not present newly-private packages as part of the supported core surface.
- Keep this change narrowly scoped to export curation; moving removed packages into a separate stdlib-style library can happen independently later.
