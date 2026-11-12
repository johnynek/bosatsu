# Code Plan #2474

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2474` 0i: Emit program C from a selected compiled package group
- Pending steps: `1`
- Completed steps: `6`
- Total steps: `7`

## Summary

Add local program emission through `tool emit-c --main-packages FILE --main-package NAME`, using the existing compiled-bundle runtime loader, main discovery, C backend, and staged output lifecycle. Validate selected-bundle membership and native Main/Prog compatibility, preserve test emission, and demonstrate bundle-only emission followed by separate native linking and execution with exit code 7. Update the existing command contract and complete tracker item 0i after its acceptance checks pass.

Plan trajectory: initial steps 3; review revisions 1; review-origin steps 2.

## Current State

All five existing steps are completed immutable history, and there are no pending steps to absorb the requested corrections. Program selection, Main/Prog layout checks, resolver reuse, and staged publication are implemented. The reviewed JVM/CLI required-tests gate passed after the shared startup-timeout repair.

Candidate inspection confirms a native declaration defect: ClangGen's shared main renderer calls its supplied runner without emitting its documented prototype. The installed Prog binding header supplies that declaration incidentally, but programs can have no reachable Prog externals, and generated Prog headers can shadow the installed header. One corrective step repairs this at the existing main-rendering boundary and covers both manifestations through native compilation with generated headers first.

Required acceptance validation is still incomplete. Step_3 records an interrupted sanitizer run and a failed full coverage run; successful replacements are not established by the passing basic gate. A second corrective step completes those checks and the applicable existing codegen validation against the repaired candidate before restoring tracker 0i's completion claim. Required macOS evidence needs a matching platform; if unavailable before publication, that obligation remains pending for execution replanning. Roadmap #2470 and its local_main node remain aligned, with no direct dependencies. The existing technical-debt proposal for legacy layout validation remains outside this repair.

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

5. [x] `step_5_core_startup_timeouts` Resolve the shared core JVM startup timeout failures

Investigate and repair the timeout family blocking the required JVM gate, rather than adding an independent workaround for each failed test. Authoritative provenance: tested SHA `d64de59648469fe7ddeeec5b73d2c31cf731f7f8`; gate/run ID `01a1cda3c26f7801935149b63df1a6c7`; command `scripts/test_basic.sh`; status `failed`, exit code 1. The failures were the 30-second MUnit timeouts in DefRecursionCheckTest.non recursive cases are allowed, CompileCacheTest.dependency interface changes invalidate the compile key, and PackageTest.simple package resolves. All previous steps are completed, so no pending work can absorb this correction.

Trace and measure the failed workloads through `core/src/test/scala/dev/bosatsu/DefRecursionCheckTest.scala`, `PackageTest.scala`, `tool/CompileCacheTest.scala`, and the existing Predef initialization in `TestUtils.scala` and `core/src/main/scala/dev/bosatsu/PackageMap.scala`. Examine JVM test scheduling in `build.sbt` and service ownership in `ParTest.scala`. In particular, determine whether TestUtils's eager Predef-derived values make its parsing helper wait for unrelated compiler initialization, and whether concurrent suites amplify initialization or CPU waits. Distinguish cold-start cost, contention, a hang, and a demonstrated compiler performance or semantic defect. Use fresh sbt processes for cold-start measurements and a focused comparison with serial JVM suite scheduling when useful; do not infer the cause solely from warm passing tests.

Apply the smallest correction supported by those measurements through the existing initialization or sbt test-scheduling mechanism. Prefer one repair at the demonstrated shared cause over repeated suite-specific timeout increases. If a genuinely slow successful workload still needs a larger timeout after addressing that cause, use the existing bounded MUnit timeout mechanism at the narrowest justified scope and record the measurements supporting it. Repair any demonstrated hang or semantic defect instead of hiding it with a larger budget. Reassess the code added by the completed OperatorTest step if the shared repair makes that workaround unnecessary; its historical plan step remains untouched. Preserve every existing test workload and semantic assertion, required-test selection, and the supervised gate's command and timeout.

Validate with `sbt -batch 'coreJVM/testOnly dev.bosatsu.DefRecursionCheckTest dev.bosatsu.tool.CompileCacheTest dev.bosatsu.PackageTest dev.bosatsu.OperatorTest'` under the committed runner settings, and repeat in a fresh process. Include additional focused regression or property coverage only for behavior actually changed. Completion requires a supported diagnosis and successful focused verification of all four suites; record cold-start measurements, any scheduling comparison, the correction, and results in completion notes. MergeXO performs the subsequent supervised required-tests run automatically.

Required-test repair provenance: tested SHA `d64de59648469fe7ddeeec5b73d2c31cf731f7f8`; gate/run ID `01a1cda3c26f7801935149b63df1a6c7`; status `failed`; configured gate `required_tests`; failure: test_failure required gate failed with exit code 1.

#### Invariants

- All existing recursion, package-resolution, compile-cache invalidation, and operator-precedence workloads and semantic assertions remain exercised.
- Timeouts remain bounded; skipped tests, suppressed failures, reduced gate selection, and increased supervised gate budgets cannot serve as the repair.
- Changes address the demonstrated startup or execution cause through existing mechanisms; production emission behavior changes only if investigation demonstrates a related defect.

#### Property Tests

- None recorded.

#### Assertion Tests

- The three previously failing cases complete successfully with their original semantic assertions under committed test settings.
- The combined DefRecursionCheckTest, CompileCacheTest, PackageTest, and OperatorTest focused command passes in two fresh sbt processes without diagnostic command-line scheduling or timeout overrides.
- If initialization or scheduling changes, focused verification exercises cold initialization and concurrent access relevant to the diagnosed cause, rather than relying only on warmed state.

#### Completion Notes

Diagnosed the gate timeouts as parallel-suite CPU contention, not a hang or semantic defect. In a fresh sbt process with parallel execution enabled, the focused baseline took 22.468s for DefRecursionCheckTest, 45.861s for OperatorTest, and 54.869s for CompileCacheTest; PackageTest.simple package resolves timed out at 44.076s. A fresh serial comparison passed all four suites (15.734s, 4.645s, 16.734s, and 25.012s respectively). Set coreJVM test parallelism to false, deferred TestUtils’ Predef-derived values so parser-only use avoids unrelated Predef compilation, and removed OperatorTest’s now-unneeded 60-second timeout. The required focused command passed in two fresh sbt processes: 64/64 tests each, with suite timings 3.599/25.184/20.295/4.636s and 3.539/26.647/17.025/3.749s. `git diff --check` passed.

6. [x] `step_6_main_runner_declaration` Declare the runner ABI at the shared main-rendering boundary

Invariant family: main-runner-declaration
Extend the existing main renderer in core/src/main/scala/dev/bosatsu/codegen/clang/ClangGen.scala. Steps step_1 and step_2 routed program emission through this renderer, but it emits a runner call without declaring the runner. Depending on an external binding to include the installed Prog header fails when no Bosatsu/Prog external survives, and generated Prog headers can shadow the installed header while omitting its runner declaration. Both manifestations prevent valid programs from compiling and violate the issue's native emission and header acceptance criteria. All existing steps are completed; no pending work can absorb this correction.

Use the existing Code.DeclareFn representation to emit a prototype for the supplied runner before the generated main: int runner(BValue main_value, int argc, char** argv). Put the declaration in the shared implementation reached by both renderMain overloads, preserving arbitrary runner identifiers and explicit-resolver behavior. This source-site repair makes runner declaration independent of reachable extern packages and header search order. Keep generated external headers limited to their existing binding contract; do not add a parallel runner-header mechanism or require an artificial Prog binding.

Extend core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenTest.scala with focused declaration regressions, retaining overload equivalence and resolver rejection coverage. Extend scripts/test_emit_c.py through its existing bundle-only emission and separate linking lifecycle: compile the existing pure-based program using its generated Prog header before runtime include directories, and add a valid program whose callback obtains Prog through another package's external with no reachable Bosatsu/Prog external. Supply that provider's native implementation and generated header. Both programs must compile with implicit function declarations treated as errors and exit 7. Preserve the separate Prog/Types/Api/Main groups, empty-PATH emission, deterministic manifest permutations, and every existing test-emission fixture. Remove the runtime-first include ordering that masked the defect. Run focused ClangGenTest and EmitCCommandTest coverage and the assembled-jar native fixture before completing this step.

#### Invariants

- Every runner call emitted by the shared main renderer has the documented ABI declared before its use.
- Runner declaration does not depend on external binding reachability or which Prog header the native compiler finds.
- Both renderMain overloads retain their supplied runner and resolver semantics.
- Emission remains file-only and continues through the existing staged publication lifecycle.

#### Property Tests

- Across fixtures with and without reachable Bosatsu/Prog externals, and both renderMain overloads, the supplied runner has a compatible prototype before its generated call.

#### Assertion Tests

- The pure-based bundle fixture compiles with generated Prog headers preceding runtime headers and exits 7.
- A Main using only another package's external to obtain Prog compiles with generated headers first, rejects implicit function declarations, and exits 7.
- Default and explicit standard-resolver rendering remain equivalent; excluding a required external still fails rendering.
- Existing passing and failing Test and ProgTest native fixtures retain their expected results.

#### Completion Notes

Added the supplied runner’s ABI prototype in the shared main renderer. The focused regression confirms the prototype precedes `main` for a custom runner with no Bosatsu/Prog external. `ClangGenTest` (33 tests) and `EmitCCommandTest` (28 tests) passed. The rebuilt assembled-jar fixture passed all existing native test-emission cases and both bundle-only program variants; each program compiled with implicit function declarations treated as errors and exited 7. The provider-backed C contained no Prog external include. `git diff --check` passed.

7. [ ] `step_7_complete_acceptance_validation` Complete required validation before restoring tracker completion

Invariant family: acceptance-validation
Complete the implementation validation required by issue #2474, roadmap #2470, and coding_style.md. Step step_3 recorded an interrupted sanitizer run with repeated AddressSanitizer:DEADLYSIGNAL output and a full coverage timeout, yet marked tracker 0i complete. Step step_5 subsequently repaired JVM scheduling and initialization, and the reviewed required-tests gate passed, but neither fact establishes successful sanitizer or full coverage checks. This is an outstanding acceptance obligation, not a request to rewrite completed history. There are no pending validation steps to revise.

Set only tracker row 0i in docs/design/hermetic-bazel-rules.md to incomplete while validation remains outstanding. After step_6_main_runner_declaration, run the full sbt coverage entry point from coding_style.md: sbt "coverage; clean; coreJVM/test; cli/test; coverageReport". Inspect changed-behavior coverage without weakening thresholds. Rebuild the candidate assembly after coverage cleanup and run the assembled-jar scripts/test_emit_c.py fixture with an explicitly selected jar and cc_conf, plus ./test_cli.sh. Run scripts/test_c_sanitizers.sh to successful completion and the applicable codegen matrix from .github/workflows/ci.yml, including Linux native, Valgrind, shared Scala.js, Python/Node, and macOS native checks. Use clean generated native build state between instrumentation configurations so sanitizer artifacts cannot contaminate ordinary or Valgrind runs. The shared renderer change warrants checking these existing consumers; do not introduce a new validation subsystem.

Investigate reproduced sanitizer failures or timeouts instead of treating an interrupted run as success. Distinguish a compiler/runtime defect from build contamination, scheduling, or an unavailable environment; repair demonstrated blockers through existing mechanisms and add focused coverage where executable behavior changes. Preserve test workloads, instrumentation, assertions, and repository thresholds. MergeXO independently reruns scripts/test_basic.sh; do not schedule or wait for that supervised gate here.

Evidence must come from repository commands or matching-platform execution of the repaired candidate before publication. Record the tested revision, commands, outcomes, and coverage assessment in this step's completion notes. The earlier Linux execution supplied no macOS result, and this Linux host cannot itself produce one: obtain matching-platform evidence before completing this step. If the required environment or evidence cannot be obtained within the pre-PR transaction, leave the step pending and report the lifecycle/environment conflict through the execution replan flow; do not substitute another platform, defer authoritative acceptance to debt, or depend on this candidate's future merge. Restore only tracker row 0i to complete after all required acceptance checks succeed.

#### Invariants

- Tracker 0i claims completion only after required acceptance and validation checks have succeeded against the repaired candidate.
- A passing basic gate, focused coverage, or interrupted sanitizer run cannot substitute for the outstanding full checks.
- Required validation retains its workloads, instrumentation, assertions, and coverage thresholds.
- Completed plan steps remain immutable; validation results are recorded in the new step.

#### Property Tests

- None recorded.

#### Assertion Tests

- The complete JVM/CLI coverage command succeeds and changed executable behavior is assessed against repository coverage expectations.
- The sanitizer script completes successfully without unresolved sanitizer failures.
- The repaired assembled-jar fixture, CLI integration checks, and applicable existing codegen matrix succeed on their required platforms.
- Tracker 0i remains incomplete when any required check lacks successful candidate evidence.
