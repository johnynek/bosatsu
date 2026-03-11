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
  - core/src/test/scala/dev/bosatsu/Issue1654Test.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-11T05:37:55Z
---

# Issue #2119 Design: add `--value` to `lib test` to select a particular test value

_Issue: #2119 (https://github.com/johnynek/bosatsu/issues/2119)_

## Summary

Design for adding a mutually-exclusive `--value <package::value>` selector to `lib test`, including CLI parsing changes, test-target resolution/validation, acceptance criteria, risks, and rollout notes.

---
issue: 2119
title: add an option to `lib test` to select particular test values
status: proposed
base_branch: main
touch_paths:
  - docs/design/2119-add-an-option-to-lib-test-to-select-particular-test-values.md
  - core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/main/scala/dev/bosatsu/codegen/CompilationNamespace.scala
  - core/src/main/scala/dev/bosatsu/codegen/CompilationSource.scala
  - core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/Issue1654Test.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-10
---

# Issue #2119 Design: add `--value` to `lib test` for selecting a single test value

Issue: #2119 (https://github.com/johnynek/bosatsu/issues/2119)  
Base branch: `main`  
Status: proposed

## Summary

Add a new `lib test --value <package::value>` selection mode so users can run a single top-level test value (plain `Test` or `ProgTest`) without running all discovered tests in a package.

`--value` must be mutually exclusive with `--filter`, enforced via Decline `orElse` composition. Invalid selections must fail with a clear CLI error.

## Problem statement

Current `lib test` selection behavior is package-level only:

1. CLI supports `--filter` regexes (package-level selection).
2. Runtime selection uses discovered package test entries (`Package.testEntry`), i.e. one selected test entry per package.
3. There is no way to target a single named test value such as `Foo/Bar/Baz::quux`.

Issue #2119 requests a targeted mode for iteration:

1. Add `--value Foo/Bar/Baz::quux`.
2. Ensure `quux` is validated as a test value.
3. Make `--value` and `--filter` exclusive using Decline `orElse`.

## Goals

1. Add `--value <package::value>` to `lib test`.
2. Enforce `--value` XOR `--filter` at parser level.
3. Validate that selected value is a test value (`Bosatsu/Predef::Test` or `Bosatsu/Prog::ProgTest`), otherwise error.
4. Keep existing `--filter` and default (no selector) behavior unchanged.
5. Keep C codegen/runtime execution path unchanged except for which test entries are selected.

## Non-goals

1. No change to `tool test` options in this issue.
2. No C runtime API changes (`bsts_test_run`, `bsts_test_run_prog`, summaries remain as-is).
3. No change to test output formatting.
4. No support for selecting multiple explicit values in one invocation.

## Current behavior (relevant code paths)

1. `lib test` parses options through `ClangTranspiler.Mode.testOpts`.
2. `Mode.Test.values` currently filters `ns.testEntries` by package regex, then renders/runs each selected package test entry.
3. `Command.testCommand` passes `test.filter` into `cc.build(...)` to scope local package typechecking.
4. `lib check` currently reuses `Mode.testOpts(...).map(_.filter)` to get package filter behavior.

This reuse is important because adding `--value` directly to `testOpts` without parser split would unintentionally leak option semantics into `lib check`.

## Proposed architecture

### 1) Introduce explicit test-selection mode in `ClangTranspiler.Mode.Test`

Add a selector ADT for test selection:

1. `ByFilter(regexes, predicate)` for current package regex behavior.
2. `ByValue(packageName, bindable)` for explicit named test selection.

`Mode.Test` carries this selector and exposes a derived `sourceFilter`:

1. `ByFilter` -> existing package predicate (or none for default all).
2. `ByValue` -> exact package predicate (`_ == packageName`) to keep local compilation scoped.

### 2) Parser design using Decline `orElse` (exclusive modes)

Use parser branches so `--value` and `--filter` cannot be combined:

1. Value branch: parse one required `package::value` into `ByValue`.
2. Filter branch: parse `--filter` regexes (or default all) into `ByFilter`.
3. Compose as `valueBranch.orElse(filterBranch)`.

This gives parser-level exclusivity and matches issue guidance.

### 3) Keep `lib check` stable by splitting filter parser from test parser

Because `lib check` currently consumes `.map(_.filter)` from `testOpts`, introduce a dedicated filter-only parser helper in `ClangTranspiler.Mode`:

1. `filterOpts` for package filtering only (used by `lib check`).
2. `testOpts` for full `lib test` behavior (`--value` or `--filter`, plus `--quiet`, execute flag).

This avoids accidental acceptance of `--value` in `lib check`.

### 4) Explicit value resolution and validation

Current namespace API only exposes discovered test entry map. For `--value`, we need exact bindable lookup among all top-level test-typed lets in a package.

Add namespace support:

1. `CompilationNamespace.testEntriesInPackage(pn): List[Package.TestEntry[Any]]`.
2. Implement in `CompilationSource.packageMapSrc` and `DecodedLibraryWithDeps` namespace impl.
3. Add helper in `Package.scala` to enumerate all top-level lets whose type is `Test` or `ProgTest`, preserving bindable/recursion/expr.

Selection behavior:

1. `ByFilter`: preserve existing `ns.testEntries` discovery + discovery errors.
2. `ByValue`: select exactly one matching bindable from `testEntriesInPackage`.
3. If no match, raise a dedicated CLI error that states selected value is not a test value and includes available test values in the package (if any).

### 5) Ensure optimizer does not drop valid `--value` targets

`Package.discardUnused` currently pins roots from `Package.testRootBindables`, which currently tracks discovered entry semantics. That can drop other valid test-typed lets before CLI selection is applied.

To make `--value` reliable, broaden root pinning for test values:

1. Update `Package.testRootBindables` to include all top-level test-typed bindables (`Test` and `ProgTest`).
2. Keep `Package.testEntry` discovery/error semantics unchanged (only retention changes).

This guarantees explicit test values remain available for selection in optimized compilation.

### 6) Error model

Add a dedicated CLI exception in `ClangTranspiler` for explicit-value selection failures (instead of overloading `NoTestsFound`):

1. Unknown package in root library.
2. Package exists but selected bindable is not a test value.
3. Package has no test values.

Keep existing errors unchanged for filter/default mode:

1. `InvalidTestDiscovery` for discovery-order violations.
2. `NoTestsFound` for regex filters that match no discovered package tests.

## Detailed implementation plan

1. In `ClangTranspiler.Mode`, add selector ADT and refactor `Mode.Test` to use it.
2. Add `--value` parser (argument type `package::value`, bindable-only) and compose with filter parser via `orElse`.
3. Add filter-only parser helper and switch `lib check` to that helper.
4. Update `Command.testCommand` to pass `test.sourceFilter` into `cc.build`.
5. Add `CompilationNamespace.testEntriesInPackage` API.
6. Implement namespace API in `CompilationSource.scala` and `DecodedLibraryWithDeps.scala` namespace implementation.
7. Add helper(s) in `Package.scala` to enumerate top-level test entries and lookup by bindable.
8. Update `Mode.Test.values` to branch by selector mode (`ByFilter` vs `ByValue`).
9. Add new `InvalidTestValueSelection` CLI exception with informative message and valid candidates.
10. Update `Package.testRootBindables` to keep all top-level test-typed bindables so explicit value targets survive optimization.
11. Add/extend tests for parser exclusivity, value selection success, and invalid selection errors.

## Testing strategy

### Parser and command behavior (`ToolAndLibCommandTest.scala`)

1. `lib test --value Foo/Bar::quux --filter Foo/.*` fails parse (exclusive modes).
2. `lib test --value <valid plain Test value>` selects that value and attempts execution path.
3. `lib test --value <valid ProgTest value>` uses prog-test execution path.
4. `lib test --value <non-test top-level value>` fails with clear invalid-selection message.
5. `lib test --value <unknown package/value>` fails with clear message and candidate test values (when package exists).
6. Existing `lib test --filter` tests continue to pass unchanged.
7. Existing `lib check --filter` behavior remains unchanged; `--value` is not accepted there.

### Core test-discovery helper coverage (`Issue1654Test.scala` or equivalent)

1. Verify helper that enumerates test entries includes plain `Test` and `ProgTest` lets.
2. Verify bindable lookup returns correct entry kind.
3. Verify `testRootBindables` retains all top-level test-typed bindables needed for explicit selection.

## Acceptance criteria

1. `lib test --value Foo/Bar/Baz::quux` is accepted when `quux` is a top-level test value.
2. `lib test --value ...` runs only the selected value (single test entry target).
3. If selected value is not a valid test value, command fails with a clear error.
4. `--value` and `--filter` are mutually exclusive at CLI parse level.
5. Existing `--filter` behavior and no-selector default behavior are unchanged.
6. `lib check` retains existing filter-only behavior and does not silently accept `--value`.
7. Both plain `Test` and `ProgTest` values are valid targets for `--value`.
8. Discovery-order error handling remains unchanged for filter/default modes.
9. Regression tests are added/updated to cover parser exclusivity, valid/invalid value selection, and filter compatibility.

## Risks and mitigations

1. Risk: explicit value targets could be optimized away before selection.
   Mitigation: broaden `Package.testRootBindables` retention to all top-level test-typed lets.

2. Risk: parser refactor could change `lib check` semantics unintentionally.
   Mitigation: split filter-only parser API and keep `lib check` on that path; add regression tests.

3. Risk: unclear error messages reduce usability.
   Mitigation: dedicated invalid-selection exception with package/value context and valid candidate list.

4. Risk: retaining all test-typed lets increases generated code size for packages containing many test values.
   Mitigation: scope change to test-typed lets only; keep tree-shaking from selected runtime root in transpiler to minimize emitted main/test harness impact.

## Rollout notes

1. Land as one PR containing parser changes, selection-resolution changes, and tests.
2. No migration required; existing commands continue to work.
3. Mention new `lib test --value` option in release notes/changelog.
4. Post-merge, monitor for parser regressions in `lib check` and for any code-size concerns from expanded test-value retention.
