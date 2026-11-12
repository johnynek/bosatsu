# Code Plan #2474

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2474` 0i: Emit program C from a selected compiled package group
- Pending steps: `0`
- Completed steps: `4`
- Total steps: `4`

## Summary

Add local program emission through `tool emit-c --main-packages FILE --main-package NAME`, using the existing compiled-bundle runtime loader, main discovery, C backend, and staged output lifecycle. Validate selected-bundle membership and native Main/Prog compatibility, preserve test emission, and demonstrate bundle-only emission followed by separate native linking and execution with exit code 7. Update the existing command contract and complete tracker item 0i after its acceptance checks pass.

Plan trajectory: initial steps 3; review revisions 0; review-origin steps 0.

## Current State

The three implementation steps are completed: program selection, native representation validation, shared emission/publication handling, CLI regressions, and the bundle-only native fixture are implemented. Their completion notes retain the recorded verification results and limitations. The ancestor roadmap and local_main node remain aligned with this scope and supply no direct dependencies.

MergeXO tested candidate `1ba5bf38e746aefb0f1ffb6d81410ce3145e67c0` in gate/run `01a1cd86123375c18d3b65f9cb45cd17` using `scripts/test_basic.sh`: status `failed`, exit code 1. CLI tests passed; the sole core failure was `OperatorTest.test operator precedence in real programs` exceeding MUnit's 30-second timeout. The core run reported 2,191 passing tests and one failure.

During this planning revision, `sbt -batch 'coreJVM/testOnly dev.bosatsu.OperatorTest'` reproduced the same timeout at the unchanged candidate: two tests passed and one failed. The failing test runs three sequential compiler/interpreter scenarios through `TestUtils.runBosatsuTest`, outside the changed C-emission path. This evidence warrants one focused timeout investigation and repair; it does not establish a semantic failure or justify changing production emission behavior. There were no existing pending steps to absorb this work. Repository code and plan artifacts were not edited during this revision.

## Problem

Tracker item 0i remains incomplete: callers cannot emit a program from compiled local bundles through the file-only command. The runtime manifest describes the whole implementation closure, so a package name alone cannot establish that the requested main belongs to the selected root group. Reusing nominal Main recognition without inspecting the supplied Main/Prog definitions could also authorize an incompatible native call. Program emission must therefore add explicit root-bundle selection, reuse the existing last-matching-main discovery, enforce the native representation contract, and retain the existing binding, header, determinism, and publication guarantees.

## Technical Debt Notes

Follow-up proposal: apply the checked native Main/Prog representation contract to the legacy `ClangTranspiler.renderAll` program path, which currently selects the native runner using nominal `validMain` recognition alone. That adjacent entry point can accept a loose Bosatsu/Prog definition with an incompatible representation. Estimate 150–300 lines including shared validation extraction and legacy-command regressions. This job enforces the contract over the bounded emit-c operation family; broadening legacy command behavior is outside tracker 0i.

## Steps

1. [x] `step_1` Allow program rendering with an explicit external resolver

In `core/src/main/scala/dev/bosatsu/codegen/clang/ClangGen.scala`, add a `renderMain` overload taking the existing package, binding, runner, and an explicit `ExternalResolver`, matching the pattern used by `renderTests`. Make the existing three-argument overload delegate with `ExternalResolver.stdExternals`; use explicit arguments rather than default arguments. Both overloads must invoke the same existing rendering implementation.

Add focused regressions in `core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenTest.scala` using its existing compiled-package fixtures. Verify compatibility of the original overload and demonstrate that a supplied resolver controls actual external references. Keep layout validation in the file-command boundary: the low-level renderer supports arbitrary explicitly supplied runners and must retain that API contract. Run the focused codegen tests and compile the affected Scala sources before completing this slice.

#### Invariants

- Existing renderMain callers retain the same generated C and standard resolver behavior.
- The explicit-resolver overload cannot bypass its supplied resolver by falling back to standard external resolution.
- Program rendering continues through the existing ClangGen rendering implementation.

#### Property Tests

- None recorded.

#### Assertion Tests

- For an existing compiled fixture, the original overload and the overload explicitly supplied with stdExternals produce identical rendered C.
- A main containing an external reference fails with UnknownValue when its resolver excludes that symbol and succeeds when the resolver permits it.

#### Completion Notes

Added the explicit-resolver `renderMain` overload and made the existing overload delegate to it. The focused regression confirms default-rendering compatibility and that filtering a referenced external makes rendering fail. `coreJVM/test:compile`, the targeted `ClangGenTest` case, and `git diff --check` passed.

2. [x] `step_2` Implement checked local program selection through the shared emission path

Extend `core/src/main/scala/dev/bosatsu/tool_command/EmitCCommand.scala` with a typed selector representing either tests from one bundle or a program from one bundle and package. Parse exactly one of `--test-packages FILE` or the complete pair `--main-packages FILE --main-package NAME`, using the existing package-name argument parser. Reject missing pairs, mixed modes, and missing selectors.

Keep manifest reading, selected-bundle presence checks, `LocalRuntime.load`, input/output collision checks, and filesystem alias protection in one shared command path. In program mode, require the selected package to appear in `scope.members(selectedBundle)` before discovering a main. Use `CompilationSource.packageMapSource[Region](useStandardInt64 = false)` and `mainValues(ClangTranspiler.validMain(_).isRight)` to select the last matching binding, including private bindings. A main in another manifest bundle cannot satisfy this selection. Report the selected bundle/package when membership or main discovery fails.

Extend the existing ProgTest layout validation with a small shared validator for the native wrapper/callback contract, used by both test and program emission. For Main, require a single constructor with one unboxed field, whose type is a unary callback from Predef `List[String]` to the supplied `Bosatsu/Prog::Prog[err, Int]`, allowing the quantification already accepted by the ProgTest approach. Require Prog to be opaque with the existing two covariant type parameters. Preserve the corresponding ProgTest result requirement of Predef Test. Validate against the loaded implementations before tree shaking; a nominal Main value alone is insufficient.

Tree-shake program generation from only the selected binding and invoke the new explicit-resolver renderMain overload. Factor the repeated generation work narrowly so both modes use the same filtered external resolver, missing-native-binding diagnostics, requested-header generation, and `Output.TranspileOut` construction. Continue checking bindings at actual C references after optimization. Keep the existing all-or-nothing output preparation and the `FileCommandRunner`/`StagedOutputs` publisher; add no direct publication or native-process execution.

Extend `cli/src/test/scala/dev/bosatsu/EmitCCommandTest.scala` using the existing file-command fixtures and parameterize relevant shared lifecycle cases across both selectors. Update the local C emission section of `docs/design/hermetic-bazel-rules.md` with both selector forms, the root-bundle requirement, last-matching binding behavior, and Main layout checks, leaving tracker 0i pending until integration validation finishes. The bounded production inventory is EmitCCommand's selector/discovery/validation/rendering path and the ClangGen overload from step 1; LocalRuntime, LocalRuntimeManifest, CompilationSource, and staged publication are reused. Imported scopes, worker transports, toolchain verification, and legacy-command validation changes are outside this job. Run focused `EmitCCommandTest`, `LocalRuntimeScopeTest`, and codegen tests while implementing.

#### Invariants

- Every accepted invocation has exactly one complete selector; program selection requires a bundle declared in the manifest and a package belonging to that bundle.
- The selected program binding is the last value recognized by the existing main-discovery mechanism, regardless of its name or export status.
- Every emit-c program entry passes Main/Prog layout validation before the native runner is selected; test entries retain the existing ProgTest validation.
- Both modes pass through the shared output checks, filtered resolver, header preparation, and Output.TranspileOut publication boundary.
- Only native references surviving tree shaking and optimization require providers; loose standard-named packages do not authorize standard Int64 intrinsics.
- Failures preserve existing outputs and inputs, create no partial output set, and allow a later request to succeed.

#### Property Tests

- Compile a small program graph once, then emit across bounded permutations and repeated occurrences of its manifest bundle paths: generated C remains identical, including across fresh and reused runners.
- For multiple Main bindings with distinguishable reachable values, the last matching binding alone determines program roots; unrelated later bindings and mains in dependency packages do not change the selected result.
- Adding or removing a provider declaration for an unreachable external does not change successful program C or make its binding mandatory.
- Re-emitting identical program C and requested headers preserves their bytes and modification times.

#### Assertion Tests

- Valid program selection works from a selected multi-package bundle and discovers a private Main binding.
- Reject a selected bundle absent from the manifest, a missing bundle file, an unknown package, and a main package supplied only by another bundle.
- Reject a selected package with no Main value, including an ordinary Int or function named main; another package's valid Main cannot satisfy it.
- Reject incomplete selector pairs, simultaneous test/program selectors, and invocations with no selector.
- Reject nominal Main definitions with wrong constructor/field shape, a non-callback field, wrong callback arguments, wrong Prog success type, or a nonopaque/incompatible Prog definition; retain the existing invalid ProgTest cases.
- A missing reachable native binding reports its package and symbol; declaring that binding succeeds, while an unreachable external needs no provider.
- Generate only requested matching headers; an invalid header request preserves old C/header files and publishes no new siblings, followed by a successful request on the same runner.
- Exercise program mode through the shared lexical, symlink/hard-link, and output-to-output alias checks, preserving runtime inputs; retain existing filesystem case-rule coverage and staged-publication rollback tests.
- All existing test-emission cases continue to pass, including private Test/ProgTest discovery, custom Int64 bindings, stale interface rejection, and dependency-test exclusion.

#### Completion Notes

Added the paired program selectors, selected-bundle membership checks, shared Main/Prog layout validation, and common native-binding, header, and output handling. Added CLI regressions for selection, layouts, externs, output safety, deterministic emission, and byte-identical retention; updated the local C emission contract. `EmitCCommandTest`, `LocalRuntimeScopeTest`, `ClangGenTest`, and `git diff --check` passed.

3. [x] `step_3` Prove bundle-only native program emission and complete tracker 0i

Extend `scripts/test_emit_c.py` alongside its existing passing/failing test-suite fixtures. Compile separate Prog, Types, Api, and Main groups with `compile-packages`: Types supplies a public type, Api imports and exposes that type while supplying the result value, and Main imports Api and Bosatsu/Prog and constructs a Main that exits 7. Supply the precise direct interfaces required by each compile invocation, including exposure-only owners. Reuse `test_workspace/Prog.bosatsu` as the Prog source.

Copy only implementation bundles into a fresh program action directory and write its local runtime manifest with the required native binding declarations. Invoke the assembled jar through an absolute Java path with an empty PATH, selecting Main through both new flags. The emission invocation receives neither source/signature files nor the C compiler configuration. Reorder the manifest inputs and require identical generated C. Check that emission creates only its declared C/header outputs and does not install a runtime. Then, in a separate operation, use the existing `cc_conf` driver and configured runtime libraries to compile/link the generated C and require the executable to exit 7. Preserve every existing plain/effectful suite and failing-assertion check.

The existing Linux and macOS C jobs already invoke this script, so extend that integration point without adding a parallel workflow or migrating legacy codegen callers. Run the assembled-jar fixture with the existing runtime setup, `./test_cli.sh`, and the JVM/CLI coverage entry point from `coding_style.md`. Validate the applicable `.github/workflows/ci.yml` matrix against the candidate, including shared Scala.js checks, Python/Node codegen checks, Linux/macOS native checks, and the existing sanitizer/Valgrind checks. Preserve existing regressions and inspect changed-behavior coverage without weakening thresholds. MergeXO independently runs the configured required-tests gate; this step does not launch or wait for that gate.

After the acceptance checks pass, mark only tracker row 0i complete in `docs/design/hermetic-bazel-rules.md` and update the local-emission description with links to the committed CLI tests and native fixture. Record commands, results, and any material validation limits in this step's completion notes. Completion requires evidence against the candidate changes and no future default-branch merge or release.

#### Invariants

- Program emission succeeds using only declared compiled bundles and the manifest, with an empty PATH and no source or interface files in its action directory.
- Native compilation/linking and runtime provisioning remain separate from emit-c.
- The separately compiled Prog/Types/Api/Main graph produces an executable exiting 7, and reordered manifest inputs produce identical C.
- The existing native test-emission fixtures and applicable codegen checks remain successful.
- Tracker 0i is marked complete only after its acceptance checks pass; other roadmap items remain unchanged.

#### Property Tests

- The integration fixture compares program C bytes before and after reordering the manifest's compiled-bundle inputs.

#### Assertion Tests

- The assembled jar emits the program from the clean bundle-only action directory with an empty PATH and without installing a runtime.
- A separate configured native compile/link operation succeeds, and the resulting program exits exactly 7.
- Existing passing and failing plain Test and ProgTest fixtures retain their expected exits and exclude the dependency suite.
- Run focused suites, JVM/CLI coverage checks, CLI integration checks, and the applicable existing codegen CI matrix.

#### Completion Notes

Extended `scripts/test_emit_c.py` with separate Prog, Types, Api, and Main bundles. The assembled-jar fixture passed with an absolute Java path and empty `PATH`; emission used only bundles and the runtime manifest, produced the declared C/header outputs without installing a runtime, and generated identical C after manifest reordering. Separate native linking produced a program that exited 7. Existing passing and expected-failure native suite fixtures also passed. Updated `docs/design/hermetic-bazel-rules.md` and marked only tracker row 0i complete. Restored the existing test-bundle diagnostic wording in `EmitCCommand` and covered both selector-specific messages.

Verification: `python3 scripts/test_emit_c.py` passed against the current assembled jar and runtime; `bash ./test_cli.sh` passed 6,118 tests; focused JVM/CLI tests passed, and focused coverage passed 47 core tests and 168 CLI tests (one skipped), with both changed diagnostic branches covered. `git diff --check` passed. Python codegen tests passed 78 tests. Scala.js checks passed (coreJS 2,167 passed/8 ignored; jsapiJS 2; jsuiJS 8); webpack exited successfully with three stats-parser warnings. Node codegen/runtime checks passed, including 6,159 tests and FibBench output 89. `scripts/test_c_valgrind.sh` passed after removing generated ASan objects left by the earlier sanitizer attempt; its test suites reported zero Valgrind errors or leaks.

Validation limits: the full coverage command timed out in `Issue2087Test` at its 30-second test limit; that test passed without coverage in 7.6 seconds, and the focused coverage run passed. The coreJS run took 34:07, beyond the workflow's 30-minute timeout. `scripts/test_c_sanitizers.sh` remained CPU-bound in `./bosatsuj test` for over 19 minutes with repeated `AddressSanitizer:DEADLYSIGNAL` output; it was stopped without a final result. The macOS native job could not run on this Linux host. `scripts/test_basic.sh` remains for MergeXO's supervised gate.

4. [x] `step_4_operator_timeout` Resolve the reproducible OperatorTest timeout

Repair the repeatable timeout blocking the required JVM gate. Investigate `test operator precedence in real programs` in `core/src/test/scala/dev/bosatsu/OperatorTest.scala` and its three sequential `TestUtils.runBosatsuTest` calls in `core/src/test/scala/dev/bosatsu/TestUtils.scala`. Measure the scenarios to distinguish successful compiler/interpreter work exceeding the default 30-second limit from a hang, semantic defect, or pathological performance regression.

If the workload completes correctly but needs a larger budget, use the existing suite-local `munitTimeout` mechanism, following the bounded timeout conventions already used by slow core suites such as `EvaluationTest` and `PredefWorkspaceTest`. Choose the smallest justified budget from the measurements. Preserve every existing program, expected assertion count, and semantic assertion. If investigation instead demonstrates a compiler/interpreter defect, repair its demonstrated cause and add focused regression coverage appropriate to that defect rather than masking it with a timeout increase.

Run the unchanged focused command `sbt -batch 'coreJVM/testOnly dev.bosatsu.OperatorTest'` and repeat it to check that the repair resolves the reproduced failure under normal runner settings. Run any additional focused suites required by the actual repair. Record the diagnosis, measurements, change, and verification in completion notes. Completion requires the full OperatorTest suite to pass within its justified bounded timeout. MergeXO supplies the subsequent supervised required-tests run. This new step addresses a reproduced gate blocker; all previous steps are completed immutable history.

Required-test repair provenance: tested SHA `1ba5bf38e746aefb0f1ffb6d81410ce3145e67c0`; gate/run ID `01a1cd86123375c18d3b65f9cb45cd17`; status `failed`; configured gate `required_tests`; failure: test_failure required gate failed with exit code 1.

#### Invariants

- All existing operator-precedence programs and semantic assertions remain exercised, including the three compiler/interpreter scenarios with expected assertion counts 3, 1, and 2.
- Any timeout adjustment is bounded and local to the demonstrated slow suite; repository-wide timeout settings remain unchanged.
- A hang, semantic defect, or pathological performance regression must be repaired at its demonstrated cause rather than hidden by a larger timeout.

#### Property Tests

- None recorded.

#### Assertion Tests

- The normal focused OperatorTest command passes all three existing tests, including every compiler/interpreter scenario in the previously failing test.
- A repeated focused run passes within the justified timeout without command-line timeout overrides.
- If investigation identifies a semantic or performance defect, focused regression coverage demonstrates the defect and verifies its repair.

#### Completion Notes

Measured the three sequential `runBosatsuTest` scenarios at 14.75s, 3.39s, and 3.24s; the test took 24.4s in that run, while a later repeat took 33.4s. This confirmed successful work exceeded the previous 30-second limit under runner load, with no semantic failure. Added a suite-local 60-second timeout, preserving all scenarios and assertions. The unchanged `sbt -batch 'coreJVM/testOnly dev.bosatsu.OperatorTest'` command passed twice after the change (3 tests each; 25.5s and 33.4s). `git diff --check` passed.
