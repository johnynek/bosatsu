# Code Plan #2249

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `implementation`
- Issue: `#2249` Kind inference should suppress dependent 'unknown type' errors after earlier type-definition failures
- Source design doc: `docs/design/2249-kind-inference-should-suppress-dependent-unknown-type-errors-after-earlier-type-definition-failures.md`
- Pending steps: `3`
- Completed steps: `0`
- Total steps: `3`

## Summary

Preserve the existing failed-dependency guard in alias-aware kind/shape inference and lock in its behavior with solver and package regression tests. Original failures must remain visible, dependent failures must be suppressed transitively across definitions and aliases, and independent declarations must still be checked. The expected implementation changes are confined to `KindFormulaTest.scala` and `ErrorMessageTest.scala`; no production rewrite is needed.

Plan trajectory: initial steps 3; review revisions 0; review-origin steps 0.

## Current State

The merged design is present on the base branch and agrees with issue #2249; the issue-tree manifest contains no additional ancestor or dependency requirements. `Package.inferBodyUnopt` calls the `ParsedTypeEnv` overload of `KindFormula.solveShapesAndKinds`. That overload restores declaration order with `orderedTypes.reverse`, then checks qualified dependencies against its failed set before dispatching either definitions or aliases to shape/kind solving. Actual failures retain diagnostics through `Ior.Both`, and skipped declarations enter the failed set so suppression propagates. Successful declarations are returned in source order, with external definitions preserved. `KindFormulaTest.makeTE` discards partial results on errors, so it cannot establish the required recovery contract. Existing tests cover successful aliases, invalid annotations, and related constructor cascades, but not the full accepted regression matrix. Repository guidelines require strict Scala compilation, focused tests, and coverage review, and prohibit running scalafmt. This planning turn made no edits and ran no tests.

## Problem

The issue reports misleading unknown-type cascades when a local type definition fails and later declarations reference it or its dependents. Those local types are known declarations with earlier failures, rather than genuinely unknown names. Although the current solver already implements the accepted suppression policy, dedicated coverage is needed to prevent regressions in failure recording, transitive propagation, definition/alias dispatch, partial results, and package diagnostics. Tests must preserve original and independent errors while proving that failed local declarations are not subsequently diagnosed as unknown.

## Steps

1. [ ] `step_1` Pin solver failure ownership and partial-result recovery

Add case-based regressions in `core/src/test/scala/dev/bosatsu/KindFormulaTest.scala` that call `KindFormula.solveShapesAndKinds(predefTypeEnv, parsedTypeEnv)` directly. Build inputs with `TestUtils.parsedTypeEnvOf`, and pattern-match `Ior.Both` to retain both diagnostics and the partial solved environment; fail explicitly on unexpected result variants. Keep `makeTE` unchanged for existing callers. Any small local assertion helper must retain concrete types and use ordinary ADT matching.

Cover four failure roots: a definition whose constructor references `Missing`; an alias with body `Missing`; a shape-valid definition with an incompatible variance annotation; and a shape-valid alias with an incompatible variance annotation. For the latter two, reuse the existing invariant `external type Cell[a]` pattern with `struct Bad[a: +*](value: Cell[a])` and `type Bad[a: +*] = Cell[a]`, respectively. Assert `KindFormula.Error.Unsatisfiable` for kind failures so the tests cannot pass by accidentally exercising shape failure. Add direct and transitive dependents, including failed definitions consumed by aliases and failed aliases consumed by definitions; instantiate parameterized roots with `Int`.

Interleave independent valid definitions and aliases before, between, and after the chain, plus an independent invalid declaration. Inspect `allDefinedTypes`, `typeAliases`, and `orderedTypes` to prove that only successful declarations survive in their existing order. Inspect error ownership through `typeDecl.toTypeConst` and match `FromShapeError(Shape.UnknownConst(...))` to verify the missing constant. Include a dependent that also references another missing constant: the existing policy skips the entire dependent, whereas a separate independent declaration referencing that missing constant must still report its error.

The enforcing boundary is the existing guarded ordered fold in `KindFormula.scala`: both statement variants pass through the guard and both record actual failures through `solvedStmt`. Preserve that production mechanism and public APIs. If a regression fails, diagnose fixture staging versus a defect at this boundary before changing production code; do not relax the required behavior. Compile with `sbt "coreJVM/test:compile"` and run `sbt "coreJVM/testOnly dev.bosatsu.KindFormulaTest"` while developing. Follow strict warning/explicit-null rules, import symbols normally, and do not run scalafmt.

#### Invariants

- Actual shape and kind failures retain their original diagnostic category and declaration ownership.
- A statement referencing an earlier failed local declaration produces no solver diagnostics and contributes no solved declaration.
- Definitions and aliases use the same failed-dependency policy, including propagation through skipped statements.
- Independent failures remain visible, and independent successes remain in the partial result in successful source order.
- A genuinely unknown constant remains an error when its declaration is not skipped for a failed dependency.

#### Property Tests

- None recorded.

#### Assertion Tests

- The Bad/UsesBad/third-dependent fixture reports Missing in Bad and no error owned by either dependent.
- Definition and alias variance fixtures produce Unsatisfiable errors owned by their roots, with no errors owned by dependents.
- Shape-failing aliases and failed definition-to-alias and alias-to-definition chains retain root errors and exclude failed/skipped statements from every solved collection.
- Interleaved valid definitions and aliases survive in source order, while independent invalid declarations retain their own errors.
- A skipped dependent's additional missing name adds no solver error; the same genuinely missing name in an independent declaration still produces UnknownConst.

2. [ ] `step_2` Check transitive suppression across bounded mixed chains

Extend `core/src/test/scala/dev/bosatsu/KindFormulaTest.scala` with a deterministic property-style test over the existing solver boundary. Enumerate one through four dependent links and every definition/alias choice for those links. Exercise both shape-failure and kind-failure roots, including the definition and alias root fixtures established in step_1. Each dependent must reference exactly its predecessor: emit a monomorphic struct containing the predecessor or a monomorphic alias of it. Apply parameterized kind-failure roots to `Int` in the first link; later links reference the preceding monomorphic name.

For each root, solve a baseline containing the root, its successful prerequisites, and an unrelated valid declaration. Then solve each generated chain with the same independent declarations. Assert that extending the failed chain introduces no additional solver errors, preserves root error categories and owners, and leaves no generated dependent in the solved definition, alias, or ordered-statement collections. Compare stable diagnostic identities and counts rather than solver-internal constraint IDs or rendered text. Verify that unrelated successes and their order remain unchanged. Include the generated chain description in assertion clues so failures are reproducible.

Use a small local enumeration in the existing suite; no new generator framework, production abstraction, graph traversal, or formal state-machine model is necessary for this finite fold. Run `sbt "coreJVM/test:compile"` and `sbt "coreJVM/testOnly dev.bosatsu.KindFormulaTest"` after adding the property.

#### Invariants

- Every skipped statement is treated as failed for all subsequent statements, regardless of definition/alias combinations.
- Adding dependent links cannot add solver errors or solved dependent declarations.
- Failure propagation does not stop checking unrelated declarations or change successful-result ordering.

#### Property Tests

- For every chain with one through four dependent links and every definition/alias link combination, across shape-invalid and kind-invalid definition/alias roots, diagnostics remain attributable only to the original root and successful results match the independent baseline.
- Every generated failed-chain constant is absent from all three solved declaration collections, while the unrelated valid declaration survives.

#### Assertion Tests

- None recorded.

3. [ ] `step_3` Verify suppression through package compilation and rendered diagnostics

Add two package regressions in `core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala` using its existing `compileErrors` helper. First compile the issue's `package Repro` example with `Bad` referencing `Missing`, `UsesBad` referencing `Bad`, and a third declaration referencing `UsesBad`. Retain the example's exports as appropriate. Collect `PackageError.KindInferenceError` values using ADT matching and inspect their `kindError` categories, owning declaration constants, and unknown constants. Require the root inference error for Missing in Bad, retain the source-conversion diagnostic for the genuinely missing name, and reject inference errors owned by either dependent.

Second compile a package with `type Bad = Int[Int]`, followed by an alias consuming Bad and a definition consuming that alias. Assert a shape error owned by Bad and no inference error owned by either dependent. This fixture must reach inference using known names, independently of missing-name recovery. Render diagnostics with the source map returned by `compileErrors` and `Colorize.None`; check the root message and ensure no inference message describes Bad or the skipped intermediate declaration as unknown. Do not assert the total package-error count or alter export diagnostics.

These tests exercise the complete in-scope path: `Package.inferBodyUnopt` → the `ParsedTypeEnv` solver overload → its guarded fold → shape/kind solving. The bounded solver branches are definitions and aliases. Keep the list overload, standalone Shape APIs, import lookup, source conversion behavior, export diagnostics, and rendering implementation unchanged. Existing suites cover their compatibility; no runtime, build, or schema changes are planned.

Compile and run `sbt "coreJVM/testOnly dev.bosatsu.KindFormulaTest dev.bosatsu.ErrorMessageTest"` after the final edits. Perform the design-required coverage check with `sbt "coverage; clean; coreJVM/test; cli/test; coverageReport"`, review exercised recovery paths against the repository's coverage expectations, and address failures within this slice. The ordinary core/CLI required-test gate is owned by MergeXO: it runs `scripts/test_basic.sh` with the configured 2400-second timeout after implementation. Do not add a step that runs or waits for that supervised gate. No CLI smoke, code-generation matrix, deployment, or post-merge verification is required.

#### Invariants

- Package compilation retains the original type-definition failure without reporting failed or transitively skipped local declarations as unknown.
- Missing-name source-conversion diagnostics remain visible, while a known-name shape failure independently demonstrates inference recovery.
- Structured error ownership establishes suppression; rendering checks confirm the user-visible result without coupling acceptance to unrelated package-error counts.
- Existing partial-result behavior, import lookup, external definitions, successful inference, rendering, and export diagnostics retain their current contracts.

#### Property Tests

- None recorded.

#### Assertion Tests

- The three-declaration missing-name package reports the root UnknownConst for Missing in Bad, retains its source-conversion error, and has no kind/shape errors owned by UsesBad or its dependent.
- The Int[Int] alias package reports a root shape failure and suppresses errors for its alias and definition dependents.
- Rendered inference diagnostics show the original failure and contain no unknown-type diagnostic for failed or skipped local constants.
