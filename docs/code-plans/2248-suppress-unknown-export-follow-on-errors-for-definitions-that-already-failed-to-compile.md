# Code Plan #2248

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `implementation`
- Issue: `#2248` Suppress 'unknown export' follow-on errors for definitions that already failed to compile
- Source design doc: `docs/design/2248-suppress-unknown-export-follow-on-errors-for-definitions-that-already-failed-to-compile.md`
- Pending steps: `0`
- Completed steps: `3`
- Total steps: `3`

## Summary

Replace misleading unknown-export diagnostics for declared but uncompiled definitions with `cannot export Module because its definition did not compile`. Extend the existing unresolved-export mapping in `PackageCustoms.assemble`, using retained source statements and export-kind matching. Preserve original diagnostics, genuinely unknown exports, successful export referants, and assembly failure behavior. The implementation spans three production Scala files and two existing test suites, with no inference or public-interface recovery changes.

Plan trajectory: initial steps 3; review revisions 0; review-origin steps 0.

## Current State

The merged design is present on the checkout and agrees with issue #2248. The issue-tree context contains no ancestor or dependency requirements. The working tree is clean; planning has made no edits or run tests.

`PackageCustoms.assemble` calls `ExportedName.buildExports` and currently maps every unresolved item to `PackageError.UnknownExport`. Its typed `Program` retains top-level source statements in `program.from`, including declarations removed from the recovered environment. `TypeStatement`, `TypeDefinitionStatement`, and `Statement.ValueStatement.names` already provide the required declaration distinctions. `UnknownExport` renders candidate suggestions, `CompilerApi.classifyError` exhaustively classifies diagnostics, and `PackageError.isPostponable` defaults unfamiliar errors to hard failures.

The complete relevant site inventory is assembly's unresolved-export mapping, its two production callers in `PackageMap` (ordinary inference and internal predef compilation), and the separate `checkExprDagBindables` unknown-binding check. That earlier check operates before type-recovery filtering and must retain its existing behavior. Existing `ErrorMessageTest` fixtures cover failed local types and aliases, transitive recovery, and missing exported bindings; `ToolAndLibCommandTest` supplies `runWithFiles`, diagnostic rendering assertions, and repository-level warning/lax command fixtures.

Read and follow `coding_style.md` and `pre_pr_review_guidance.md`: immutable APIs, idiomatic ADT matching, strict warnings, explicit-null compatibility, normal imports, and no scalafmt. This deterministic classification needs focused contract tests rather than a state-machine model.

## Problem

A declared type can fail compilation and disappear from the recovered type environment. Export assembly then calls it unknown, even though its source declaration is available and its compilation failure has already been reported. This misdirects debugging, including the issue's `export Module` followed by `struct Module(a: Missing)` example. The same information loss affects dependent declarations and local values excluded during recovery. The correction must distinguish these unresolved declarations from genuinely absent names without hiding original errors or allowing an incomplete package interface.

## Steps

1. [x] `step_1` Classify unresolved exports using retained local declarations

Implement the complete production change together with the minimal regression in the existing files.

In `core/src/main/scala/dev/bosatsu/PackageError.scala`, add immutable `UncompiledExport[A]` carrying `ex: ExportedName[A]` and `in: PackageName`, adjacent to `UnknownExport`. Use the existing source-map lookup and `ex.name.sourceCodeRepr` to render `in <source> cannot export <name> because its definition did not compile`, without spelling suggestions. Leave `isPostponable` unchanged so its default makes this a hard error. In `core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala`, add the exhaustive classifier case assigning this diagnostic one package error.

In `core/src/main/scala/dev/bosatsu/PackageCustoms.scala`, add a small private predicate using normal ADT matching over `program.from`: a type export matches a same-name `TypeStatement`; a constructor export matches a same-name `TypeDefinitionStatement`; a binding export matches a name in a top-level `Statement.ValueStatement.names`. Reuse the existing statement helpers where appropriate. Consult this predicate only inside `Validated.Invalid(unknowns)`. Map each unresolved item to `UncompiledExport` when it matches, otherwise preserve the existing `UnknownExport` construction with its candidate regions. Keep `buildExports`, the successful assembly branch, and the nonempty `Ior.Left` result unchanged. Add a short comment explaining that recovery can remove declarations while retaining their source statements.

In `core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala`, use `compileErrors` for the issue's exact minimal repro and inspect the entire diagnostic list. Also add a focused assembly-boundary test: construct a typed `Program` with empty recovered types/lets and a parsed local struct declaration retained in `from`; call `assemble` with empty imports, its type export, and no exposes declarations. Assert that unresolved export assembly returns `Ior.Left`, not a typed package. Keep any new test helper local to the suite and preserve type parameters.

Run `sbt "coreJVM/test:compile"` and `sbt "coreJVM/testOnly dev.bosatsu.ErrorMessageTest"`. Completion means the production change compiles under existing strict settings and the minimal regression and assembly failure test pass.

#### Invariants

- Successful export lookup takes precedence; source declarations are consulted only for items rejected by buildExports.
- Each unresolved export produces exactly one diagnostic, preserving a nonempty Ior.Left and preventing an incomplete typed package.
- Matching uses only top-level declarations from the current package and respects type, constructor, and binding export kinds.
- The replacement is a hard package error; genuinely missing exports retain existing UnknownExport rendering and candidates.
- Both production assembly callers inherit the change through the existing boundary; checkExprDagBindables and inference recovery remain unchanged.

#### Property Tests

- None recorded.

#### Assertion Tests

- For `package Repro; export Module; struct Module(a: Missing)` expressed with valid source newlines, assert that the original missing-type diagnostics remain with their original category and package/type ownership, exactly the expected UncompiledExport names Module in Repro, and no UnknownExport names Module anywhere in the diagnostic list.
- Render the minimal repro through its source map and assert the accurate replacement text and absence of spelling suggestions on UncompiledExport.
- At the assembly boundary, an unresolved export matching retained source returns Ior.Left containing UncompiledExport and no typed package.
- Assert PackageError.isPostponable is false for the new diagnostic.

#### Completion Notes

Added `UncompiledExport` as a hard package error and classified unresolved exports against same-kind declarations retained in `Program.from`; successful export construction and `Ior.Left` assembly failure behavior remain unchanged. The minimal repro retains its source-conversion and kind-inference diagnostics, reports the accurate export error, and the direct assembly test confirms no typed package is returned. `sbt "coreJVM/test:compile"` and `sbt "coreJVM/testOnly dev.bosatsu.ErrorMessageTest"` passed; all 93 suite tests passed.

2. [x] `step_2` Cover recovery and export-kind classification contracts

Extend `core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala` with a small deterministic fixture matrix using `compileErrors`; reuse the nearby failed-local-type and shape-failed-alias syntax. Keep fixtures and expected export classifications together so each case identifies its root failure, exported declaration names, and genuinely absent names. No new generator framework or production tracking mechanism is needed.

Cover failed structs and enums exported as types and with `T()`, failed aliases exported as types, and direct/transitive dependent types. Include local values removed by recovery through failed type annotations or constructors, plus a value depending on another removed value. Exercise the existing value-declaration forms—top-level definitions, binding patterns, and external definitions—using valid fixtures that reach assembly. Include both an unknown type such as `Missing` and a known-type shape failure such as `type Bad = Int[Int]`.

For each applicable fixture, vary whether an unrelated absent type export is present. Use a type export for this dimension because a missing binding can stop at the earlier expression-DAG check before assembly. Assert expected classifications against the complete structured list, including the exported source form and package, and verify the original root failure remains. Do not require a separate root diagnostic for every skipped dependent or change inference's existing suppression behavior.

Add narrow negative cases for `Alias()` when Alias is a source alias and for an enum branch exported as a type when only its enclosing enum is declared. Verify that nested names and imported declarations do not become local declaration matches; use focused boundary fixtures where earlier resolution would otherwise prevent reaching assembly. Retain the existing missing-exported-binding regression and add a concrete suggestion assertion with a nearby surviving binding and its source location. Add a successful control covering type, constructor, alias, and binding exports, asserting the expected referant kinds and names through normal compilation.

Run `sbt "coreJVM/testOnly dev.bosatsu.ErrorMessageTest"`. Completion means every supported classification branch and the export-kind exclusions have focused coverage, without weakening existing diagnostics.

#### Invariants

- Direct failures and declarations skipped transitively receive the same accurate classification when their requested export kind has a matching local declaration.
- Adding an unrelated absent type export preserves both root failures and declared-export replacements while retaining UnknownExport for the absent item.
- Aliases do not satisfy constructor exports, enum branch names do not become type declarations, and nested/imported names do not establish local declarations.
- The earlier missing-binding check continues reporting genuinely absent bindings even when name checking also fails.
- Successful exports retain their original referants and constructor expansion.

#### Property Tests

- Across the deterministic matrix of supported export forms, direct/transitive recovery, missing-name/known-type root failures, and optional absent type exports, compare complete expected UncompiledExport and UnknownExport classifications and assert preservation of the root diagnostics.
- For every UncompiledExport produced by the matrix, assert that it belongs to the fixture's package, retains the requested export kind, and is not postponable.
- Across successful control exports, assert that normal compilation retains the expected value, defined-type, alias, and constructor referants.

#### Assertion Tests

- A failed source alias exported with Alias() remains UnknownExport rather than UncompiledExport.
- A branch of a failed enum exported as a type remains UnknownExport unless an actual matching top-level type declaration exists.
- Focused boundary cases establish that imported and nested names do not count as local source declarations.
- A genuinely absent binding retains its existing suggestion text and candidate location; the existing missing-binding-plus-name-error regression remains passing.
- A known-type shape failure preserves the original root error ownership while exported dependents receive UncompiledExport.

#### Completion Notes

Expanded `ErrorMessageTest` with a deterministic matrix for failed structs, enums, aliases, direct and transitive dependents, and recovered values, with and without an absent type export. Added export-kind, imported/nested-name, external-declaration, missing-binding suggestion, and successful referant controls. `sbt "coreJVM/testOnly dev.bosatsu.ErrorMessageTest"` passed all 98 tests.

3. [x] `step_3` Verify CLI rendering and hard-error behavior

In `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala`, add a `runWithFiles` regression invoking `tool check --color none --input <fixture>` on the minimal Module repro. Match the normal CLI exception and inspect its diagnostic document. Assert that the root compilation diagnostic and the accurate replacement appear, the replacement is classified as a package error, and `unknown export Module` is absent.

Using the existing `baseLibFiles` repository fixture, run the corresponding source through repository-level `check --repo_root repo` with strict, `--warn`, and `--lax` modes. These flags belong to repository-level check. Assert failure in all modes and that the replacement remains in the hard-error diagnostic output; failure alone is insufficient because the root error is also fatal. Keep unrelated lint diagnostics out of these fixtures.

Run `sbt "coreJVM/test:compile"`, then `sbt "coreJVM/testOnly dev.bosatsu.ErrorMessageTest dev.bosatsu.ToolAndLibCommandTest"` and `./test_cli.sh`. Use `sbt "coverage; clean; coreJVM/test; cli/test; coverageReport"` for the repository coverage check, which also runs the broader Scala suites required by the guidelines. Review coverage against the goal of at least 95% overall and ideally complete changed-line coverage; address meaningful uncovered classification branches within these suites. Keep formatting and strict-warning fixes inline.

MergeXO independently runs the supervised `scripts/test_basic.sh` required-tests gate with its configured 2400-second timeout after implementation. This step neither schedules nor waits for that gate. All planned acceptance is verifiable before merge; no deployment, backend matrix, or post-merge artifact is required.

#### Invariants

- The replacement travels through the normal CLI diagnostic renderer and is counted as a package error.
- Warning and lax modes neither demote nor suppress UncompiledExport.
- Original root failures remain visible alongside the replacement, and misleading unknown-export text for Module disappears.
- All implementation and validation needed by this plan completes on the candidate branch before publication.

#### Property Tests

- Across strict, warning, and lax repository check modes, the same unresolved declared export remains a hard error with the accurate replacement text.

#### Assertion Tests

- The tool check minimal repro fails with both the original root diagnostic and a package-error replacement, without `unknown export Module`.
- Warning and lax invocations retain the replacement in hard-error output rather than merely failing because of the root error.
- The focused suites, CLI regression script, and repository coverage entry point pass after the change.

#### Completion Notes

Added CLI coverage for normal `tool check` rendering and repository `check` in strict, `--warn`, and `--lax` modes. `sbt "coreJVM/test:compile"` and the focused suites passed (270 tests). `bash ./test_cli.sh` passed, including 6,118 Bosatsu workspace tests; the literal `./test_cli.sh` invocation stopped because the environment lacks the external `time` command. `sbt "coverage; clean; coreJVM/test; cli/test; coverageReport"` passed: core coverage was 84.56% statements and 80.50% branches, below the 95% team goal. The changed export classification, replacement diagnostic, and package-error classification lines were covered.
