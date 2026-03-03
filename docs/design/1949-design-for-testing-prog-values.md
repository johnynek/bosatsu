---
issue: 1949
priority: 3
touch_paths:
  - docs/design/1949-design-for-testing-prog-values.md
  - test_workspace/Prog.bosatsu
  - core/src/main/scala/dev/bosatsu/rankn/Type.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageMap.scala
  - core/src/main/scala/dev/bosatsu/Evaluation.scala
  - core/src/main/scala/dev/bosatsu/library/LibraryEvaluation.scala
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - core/src/main/scala/dev/bosatsu/tool_command/TestCommand.scala
  - core/src/main/scala/dev/bosatsu/codegen/CompilationNamespace.scala
  - core/src/main/scala/dev/bosatsu/codegen/CompilationSource.scala
  - core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala
  - core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala
  - core/src/main/scala/dev/bosatsu/codegen/clang/ClangGen.scala
  - c_runtime/bosatsu_ext_Bosatsu_l_Prog.h
  - c_runtime/bosatsu_ext_Bosatsu_l_Prog.c
  - c_runtime/bosatsu_runtime.h
  - c_runtime/bosatsu_runtime.c
  - core/src/test/scala/dev/bosatsu/Issue1654Test.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenLibraryDepsTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/ExternalNamespaceTest.scala
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-03-03T01:38:16Z
---

# Issue #1949 Design: testing `Prog` values with `ProgTest`

_Issue: #1949 (https://github.com/johnynek/bosatsu/issues/1949)_

## Summary

Design for adding `Bosatsu/Prog::ProgTest` as a test entrypoint, including discovery rules, evaluator and C-runtime execution paths, validation semantics, acceptance criteria, and rollout/risk planning.

---
issue: 1949
title: design for testing Prog values
status: proposed
base_branch: main
touch_paths:
  - docs/design/1949-design-for-testing-prog-values.md
  - test_workspace/Prog.bosatsu
  - core/src/main/scala/dev/bosatsu/rankn/Type.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageMap.scala
  - core/src/main/scala/dev/bosatsu/Evaluation.scala
  - core/src/main/scala/dev/bosatsu/library/LibraryEvaluation.scala
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - core/src/main/scala/dev/bosatsu/tool_command/TestCommand.scala
  - core/src/main/scala/dev/bosatsu/codegen/CompilationNamespace.scala
  - core/src/main/scala/dev/bosatsu/codegen/CompilationSource.scala
  - core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala
  - core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala
  - core/src/main/scala/dev/bosatsu/codegen/clang/ClangGen.scala
  - c_runtime/bosatsu_ext_Bosatsu_l_Prog.h
  - c_runtime/bosatsu_ext_Bosatsu_l_Prog.c
  - c_runtime/bosatsu_runtime.h
  - c_runtime/bosatsu_runtime.c
  - core/src/test/scala/dev/bosatsu/Issue1654Test.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenLibraryDepsTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/ExternalNamespaceTest.scala
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: L
generated_at: 2026-03-03
---

# Issue #1949 Design: testing `Prog` values with `ProgTest`

Issue: #1949 (https://github.com/johnynek/bosatsu/issues/1949)
Base branch: `main`

## Summary

Add a first-class test entrypoint type, `Bosatsu/Prog::ProgTest`, so Bosatsu test commands can execute effectful `Prog` computations and still report standard `Bosatsu/Predef::Test` output. Keep existing plain-`Test` discovery as fallback, and add a clear validation rule for mixed definitions.

## Problem statement

Current Bosatsu test discovery only runs the final top-level value of type `Bosatsu/Predef::Test` in each package. This prevents testing programs whose test setup or assertions require `Prog` effects (filesystem, env, process, IO, time, etc.) before producing a final `Test` tree.

The issue proposes a `ProgTest` wrapper, analogous to `Main`, with a function `List[String] -> Prog[..., Test]`. We need a design that is explicit about:

1. How packages are discovered as tests.
2. What happens when both `Test` and `ProgTest` exist.
3. How behavior stays consistent across evaluator (`tool test`) and C-runtime (`lib test`) paths.

## Goals

1. Add `ProgTest` to `Bosatsu/Prog` as a standard, exported test entrypoint.
2. Support running `ProgTest` in both `tool test` and `lib test`.
3. Preserve existing behavior for packages that only define plain `Test` values.
4. Define and enforce deterministic discovery/precedence rules when both `Test` and `ProgTest` are present.
5. Keep test output format compatible with existing `Test` reporting.

## Non-goals

1. Do not add new CLI test-argument flags in this issue; pass `[]` to `ProgTest` for now.
2. Do not redesign `Test` output rendering format.
3. Do not introduce a new testing DSL or new `Test` constructors.
4. Do not change `Main` execution semantics.

## Proposed API changes

In `Bosatsu/Prog` (`test_workspace/Prog.bosatsu`):

1. Export `ProgTest()`.
2. Add `struct ProgTest(test_fn: forall e. List[String] -> Prog[e, Test])`.

Resulting contract:

1. `ProgTest` values are top-level test entrypoints for effectful tests.
2. `test_fn` is invoked with `[]` by current runners.
3. `test_fn` must return a `Prog` whose success value is a `Test`.

## Discovery and precedence semantics

Define a shared discovery model in `Package` and reuse it everywhere.

For each package, examine top-level lets in source order:

1. Plain test candidate: let type `sameAs(Type.TestType)`.
2. Prog test candidate: let type `sameAs(Type.ProgTestType)` where `Type.ProgTestType` is `Bosatsu/Prog::ProgTest`.

Selection rules:

1. If no `ProgTest` exists, run the last plain `Test` (existing behavior).
2. If at least one `ProgTest` exists, run the last `ProgTest`.
3. If any plain `Test` appears after the selected `ProgTest`, discovery is an error for that package.
4. If neither exists, the package has no tests.

Rationale:

1. `ProgTest` presence opts a package into effectful test mode.
2. "Test after ProgTest" is treated as ambiguous/likely accidental and is surfaced early.
3. This matches the issue guidance while keeping migration simple.

## Architecture and implementation plan

### 1. Shared test-entry model in compiler core

Add a shared representation (in `Package.scala`) for selected test entrypoints and discovery errors, then expose it from `PackageMap`.

Planned additions:

1. A `TestEntry` kind (`PlainTest` or `ProgTest`) including selected bindable.
2. A package-scoped discovery error type for invalid mixed ordering.
3. `Package.testEntry(...)` (or equivalent) returning selected entry or error.
4. `PackageMap.testEntries(...)` (or equivalent) for multi-package consumers.

This removes duplicated ad-hoc selection logic and guarantees evaluator/codegen consistency.

### 2. Evaluator path (`tool test`)

In `Evaluation.scala` and `LibraryEvaluation.scala`, dispatch test evaluation by discovered entry kind:

1. `PlainTest`: existing path (`Test.fromValue` on evaluated let value).
2. `ProgTest`:
   - Evaluate selected let value.
   - Run helper `PredefImpl.runProgTest(value, args = Nil)`.
   - On success, convert resulting value using `Test.fromValue`.
   - On uncaught `Prog` error, synthesize a single failing `Test.Assertion(False, msg)`.

In `TestCommand.scala`:

1. If discovery returns package errors, fail command with a clear message listing package(s) and offending binding order.
2. Otherwise keep existing output contract (`Output.TestOutput`).

### 3. C-runtime path (`lib test`)

`lib test` currently compiles C and calls `bsts_test_run` for plain tests. Add parallel support for `ProgTest`.

Compiler/codegen changes:

1. Extend `CompilationNamespace` and its implementations (`CompilationSource`, `DecodedLibraryWithDeps`) to expose discovered test entries with kind, not only plain test bindables.
2. Update `ClangTranspiler.Mode.Test.values(...)` to use entry kind and to fail on discovery errors.
3. Update `ClangGen.renderTests(...)` to emit per-package runner calls:
   - plain test entry: `bsts_test_run(...)`
   - prog test entry: `bsts_test_run_prog(...)`

Runtime changes:

1. In `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c/.h`, add helper to execute `ProgTest` value and return either `Test` value or top-level error.
2. In `c_runtime/bosatsu_runtime.c/.h`, add `bsts_test_run_prog(...)` that:
   - Constructs/evaluates selected prog-test value.
   - Executes it with empty arg list.
   - Reuses existing test-report logic on success.
   - Produces one failed result with clear error text on uncaught error.

This keeps the existing `BSTS_Test_Result` summary model intact.

### 4. Documentation updates

Update `docs/src/main/paradox/language_guide.md` test section to document:

1. `ProgTest` entrypoint semantics.
2. Precedence over plain `Test`.
3. Validation error when plain `Test` appears after selected `ProgTest`.
4. Current argument behavior (`[]`).

## Detailed implementation steps

1. Add `ProgTest` struct/export to `test_workspace/Prog.bosatsu`.
2. Add `Type.ProgTestType` constant in `rankn/Type.scala`.
3. Introduce shared test discovery API + error model in `Package.scala`.
4. Plumb discovery through `PackageMap.scala`.
5. Update evaluator discovery usage in `Evaluation.scala` and `LibraryEvaluation.scala`.
6. Add `PredefImpl.runProgTest` helper in `Predef.scala`.
7. Update `tool_command/TestCommand.scala` to surface discovery errors.
8. Extend `CompilationNamespace` and both namespace builders to expose typed test entries.
9. Update `ClangTranspiler.scala` test selection and error handling.
10. Update `ClangGen.scala` to emit runner call by entry kind.
11. Add C runtime helper functions in `bosatsu_ext_Bosatsu_l_Prog.*` and `bosatsu_runtime.*`.
12. Add/adjust tests for discovery, evaluator execution, and clang/runtime wiring.
13. Update language guide test-discovery docs.

## Testing strategy

### Core discovery tests

1. Package with only plain tests still selects last plain `Test`.
2. Package with `ProgTest` and no trailing plain test selects `ProgTest`.
3. Package with plain `Test` after selected `ProgTest` yields discovery error.
4. Discovery handles vacuous polymorphism consistently with existing `sameAs` behavior.

### Evaluator command tests (`tool test`)

1. `ProgTest` returning passing `Test` reports success.
2. `ProgTest` returning failing `Test` reports failure details.
3. `ProgTest` raising uncaught error reports one synthetic failure.
4. Mixed-order validation error is surfaced as command failure.

### Clang/C path tests (`lib test`)

1. Generated C for plain tests still uses `bsts_test_run`.
2. Generated C for `ProgTest` uses `bsts_test_run_prog`.
3. Filter behavior still scopes tested packages.
4. Discovery error packages fail before execution with clear message.

### Regression tests

1. Existing non-`ProgTest` test suites remain green.
2. Existing `lib build`/`--main` flows remain unchanged.

## Acceptance criteria

1. `Bosatsu/Prog` exports `ProgTest()` with `test_fn: forall e. List[String] -> Prog[e, Test]`.
2. `tool test` executes `ProgTest` and reports resulting `Test` using existing output shape.
3. `lib test` executes `ProgTest` through C runtime and includes results in existing summary model.
4. Packages with no `ProgTest` keep current "last plain `Test`" behavior.
5. If a plain top-level `Test` appears after selected `ProgTest`, test discovery fails with a clear package-level error.
6. `ProgTest` currently receives empty args (`[]`) in both evaluator and C-runtime paths.
7. Uncaught `Prog` errors in `ProgTest` are reported as test failures (not silent pass/no-op).
8. Existing tests for plain `Test` behavior continue to pass.
9. Language guide documents the new discovery rules.

## Risks and mitigations

1. Risk: evaluator and C-runtime `ProgTest` semantics diverge.
   Mitigation: centralize selection rules in compiler core and add paired tests for both execution paths.

2. Risk: behavior change for packages that previously mixed plain tests and new prog tests.
   Mitigation: explicit validation error with actionable wording; document migration pattern.

3. Risk: poor diagnostics for polymorphic top-level `Prog` errors.
   Mitigation: standardize failure message format and include package name + brief rendered error value.

4. Risk: adding kinded test entries ripples through namespace/codegen APIs.
   Mitigation: keep changes mechanical, update namespace tests first, then clang tests.

## Rollout notes

1. Land as one additive PR (plus intentional validation behavior for mixed ordering).
2. No feature flag required.
3. After merge, release/update `core_alpha` so consumers get `Bosatsu/Prog::ProgTest`.
4. Announce migration guidance: if adopting `ProgTest`, keep it as the final test entrypoint and move later plain tests into its returned suite.
5. Follow-up issue can add explicit CLI test args and pass them to `ProgTest` instead of `[]`.
