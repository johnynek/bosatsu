---
issue: 2119
priority: 3
touch_paths:
  - docs/design/2119-add-an-option-to-lib-test-to-select-particular-test-values.md
  - core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/main/scala/dev/bosatsu/codegen/CompilationNamespace.scala
  - core/src/main/scala/dev/bosatsu/codegen/CompilationSource.scala
  - core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-11T23:20:00Z
---

# Issue #2119 Design: add `--value` to `lib test` to select a particular test value

_Issue: #2119 (https://github.com/johnynek/bosatsu/issues/2119)_

## Summary

Add a new `lib test --value <package::value>` selector that runs exactly one test value, with parser-level exclusivity against `--filter` via Decline `orElse`.

To keep compilation semantics simple and unchanged, `--value` only accepts exported test values. If the selected value is not exported or is not a test value, `lib test` errors clearly.

## Problem statement

`lib test` currently only supports package-level narrowing via `--filter` regexes. That is useful but coarse during iteration, where users often want to run one named test value such as `Foo/Bar/Baz::quux`.

Issue #2119 requests:

1. A `--value` selector for `package::value`.
2. A failure when the selected value is not a valid test value.
3. Mutual exclusivity with `--filter` using Decline `orElse`.

## Goals

1. Add `--value <package::value>` to `lib test`.
2. Enforce `--value` XOR `--filter` in CLI parsing.
3. Fail fast with a clear message when the selected value is not exported as a test value.
4. Keep existing `--filter` and default `lib test` behavior unchanged.
5. Avoid changes to compilation/inlining semantics.

## Non-goals

1. No changes to `tool test` behavior or options.
2. No C runtime API changes.
3. No changes to test output formatting.
4. No support for multiple `--value` selectors in one invocation.

## Current behavior (relevant paths)

1. `lib test` options are parsed by `ClangTranspiler.Mode.testOpts`.
2. `Mode.Test.values` currently filters discovered package test entries.
3. `lib check` currently reuses a filter parser path from test options.

This is the key integration point for adding `--value` safely while preserving existing flows.

## Proposed architecture

### 1) Selection mode in `ClangTranspiler.Mode.Test`

Introduce explicit selector modes:

1. `ByFilter(regexes, predicate)` for the current package-regex behavior.
2. `ByValue(packageName, bindable)` for explicit single-value selection.

`Mode.Test` exposes a derived source package filter:

1. `ByFilter` keeps current behavior.
2. `ByValue` scopes local package compilation to the selected package.

### 2) CLI exclusivity via Decline `orElse`

Build parser branches as:

1. Value branch: `--value <package::value>`.
2. Filter branch: existing `--filter` behavior (or default all).
3. Compose as `valueBranch.orElse(filterBranch)`.

This enforces exclusive modes at parse time as requested.

### 3) Keep `lib check` stable

`lib check` should not gain `--value`. Split parser helpers in `ClangTranspiler.Mode`:

1. A filter-only helper for `lib check`.
2. Full selector parser (`--value` or `--filter`) for `lib test`.

This avoids accidental option leakage from `lib test` to `lib check`.

### 4) Export-gated value validation

For `ByValue(package::name)`, validation is:

1. Package exists in root-library scope.
2. `name` is exported by that package.
3. The exported `name` resolves to a top-level value typed as `Bosatsu/Predef::Test` or `Bosatsu/Prog::ProgTest`.

If any check fails, emit a dedicated `InvalidTestValueSelection` error.

Rationale: requiring export means we only depend on values guaranteed to survive existing compilation/inlining behavior. We do not need to broaden root retention rules.

### 5) Namespace lookup support

Add lookup support in compilation namespace plumbing so `ByValue` can validate and resolve one value by package/name without changing runtime APIs.

Likely additions:

1. A namespace helper to look up exported value metadata by package/value.
2. A namespace helper to resolve test-entry kind for the selected exported value.

Implement these in both package-map and decoded-library namespace implementations.

### 6) Error model

Add a dedicated CLI error for explicit selection failures, with clear messages for:

1. Unknown package.
2. Value is not exported.
3. Exported value exists but is not a test value.
4. Package has no exported test values.

Keep existing `NoTestsFound` and discovery errors unchanged for filter/default modes.

## Detailed implementation plan

1. Refactor `ClangTranspiler.Mode.Test` to carry selector mode (`ByFilter` or `ByValue`).
2. Add `--value` parsing (`package::value`, bindable-only) and compose with filter parser using `orElse`.
3. Add filter-only parser helper and update `lib check` to use it.
4. Update `Command.testCommand` to use derived source filter from the selector mode.
5. Extend `CompilationNamespace` with exported-value lookup support required by `ByValue` validation.
6. Implement namespace lookup support in `CompilationSource.scala` and `DecodedLibraryWithDeps.scala`.
7. Add helper logic in `Package.scala` for resolving top-level test-entry kind for a named bindable.
8. Update `Mode.Test.values`:
   1. Filter mode keeps existing discovery behavior.
   2. Value mode resolves a single exported test entry or errors.
9. Add `InvalidTestValueSelection` CLI exception and wire it into error reporting.
10. Add/adjust tests in `ToolAndLibCommandTest.scala` for selector exclusivity and export-gated validation.

## Testing strategy

### CLI and command tests (`ToolAndLibCommandTest.scala`)

1. `lib test --value Foo/Bar::quux --filter Foo/.*` fails parse (exclusive selectors).
2. `lib test --value <exported plain Test value>` selects one value and proceeds through existing execution path.
3. `lib test --value <exported ProgTest value>` selects one value and uses prog-test path.
4. `lib test --value <non-exported test value>` fails with explicit export requirement error.
5. `lib test --value <exported non-test value>` fails with not-a-test-value error.
6. `lib test --value <unknown package/value>` fails with clear selection error.
7. Existing `lib test --filter` behavior remains unchanged.
8. Existing `lib check --filter` behavior remains unchanged and does not accept `--value`.

## Acceptance criteria

1. `lib test --value Foo/Bar/Baz::quux` is accepted only when `quux` is exported and is a valid test value.
2. `lib test --value ...` runs exactly the selected value.
3. Non-exported values passed to `--value` fail with a clear error.
4. Exported but non-test values passed to `--value` fail with a clear error.
5. `--value` and `--filter` are mutually exclusive at parse time.
6. Existing `--filter` and default `lib test` flows behave as before.
7. `lib check` remains filter-only and unchanged.
8. No compilation root-retention behavior changes are required for this feature.

## Risks and mitigations

1. Risk: requiring export may force users to expose values they intended to keep private.
   Mitigation: document this as the contract for `--value`; recommend a test-focused package API surface.

2. Risk: parser refactor could unintentionally change `lib check` behavior.
   Mitigation: split parser helpers and add explicit regression tests for `lib check`.

3. Risk: users may confuse non-exported with non-test failures.
   Mitigation: provide separate, explicit error messages for "not exported" vs "not a test value".

## Rollout notes

1. Land as one PR with parser updates, selector wiring, and test coverage.
2. No migration is required for existing `lib test` or `lib check` usage.
3. Release notes should call out the new `--value` option and export requirement.
