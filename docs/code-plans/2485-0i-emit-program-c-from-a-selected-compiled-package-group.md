# Code Plan #2485

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2485` 0i: Emit program C from a selected compiled package group
- Pending steps: `3`
- Completed steps: `0`
- Total steps: `3`

## Summary

Extend the existing compiled-bundle `tool emit-c` command with program emission selected by `--main-packages FILE --main-package NAME`, mutually exclusive with `--test-packages FILE`. Reuse local runtime loading, main selection, tree shaking, declared external resolution and staged publication. Validate selected-bundle membership and native Main layout, and demonstrate that separately compiled Prog/Types/Api/Main bundles produce an executable exiting 7.

The boundary is local compiled-bundle emission and its executable fixtures/documentation. Imported-library scopes, toolchain authentication, persistent-worker transport and Bazel rules remain assigned to other roadmap nodes. Existing interactive transpiler callers retain their behavior.

Plan trajectory: initial steps 3; review revisions 0; review-origin steps 0.

## Current State

At initial authoring, `EmitCCommand.scala` supports only `--test-packages`. Its shared command path reads `LocalRuntimeManifest`, loads `LocalRuntime`, protects runtime inputs and C/header destinations against aliases, and returns `Output.TranspileOut` for the existing FileCommandRunner/StagedOutputs publication lifecycle. LocalRuntime already retains bundle membership, supplies compiled Predef once, validates implementation/interface requirements and validates declared external names.

Test rendering disables standard Int64 intrinsics for loose implementations, tree-shakes selected entries, filters native resolution through `scope.hasBinding`, validates requested headers and checks the ProgTest wrapper layout. `CompilationNamespace.mainValues` selects the last binding satisfying the supplied main-type predicate; `ClangTranspiler.validMain` recognizes `Bosatsu/Prog::Main`. `ClangGen.renderMain` currently always uses the standard external resolver, whereas `renderTests` accepts an explicit resolver. The native runner calls Main's unboxed callback with List[String] and consumes its external Prog result as an Int exit code.

`scripts/test_emit_c.py` already separates compiled-only emission under an empty PATH from native compilation and checks passing/failing private Test and ProgTest fixtures. The design's 0i tracker is incomplete and its command contract does not yet identify the program's selected root bundle. Roadmap #2482 and its program_c node agree with the requested extension and require no dependency artifacts.

## Problem

Tracker item 0i requires generating program C from a selected compiled package group without invoking a host compiler. The current command can emit only test runners. Selecting a main from every package in the runtime manifest would incorrectly accept a main belonging solely to a dependency group. Program emission also needs to reject invalid main types and incompatible loose definitions of the native Main wrapper, enforce declared reachable native bindings, and preserve the existing deterministic output, header, alias-protection and rollback guarantees.

## Steps

1. [ ] `step_1` Allow the existing main renderer to use a supplied external resolver

In `core/src/main/scala/dev/bosatsu/codegen/clang/ClangGen.scala`, add an explicit-resolver overload of `renderMain`, following the existing `renderTests` overload pattern. Route it through the existing private `render` method so direct external references and first-class external function values use the supplied resolver. Keep the current three-argument overload and make it delegate with `ExternalResolver.stdExternals`; do not introduce default arguments or change existing transpiler callers.

Add focused regressions in `core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenTest.scala`. Build small typed fixtures using the suite's existing helpers and exercise the actual main-rendering path. Check explicit standard resolution, allowed external bindings and rejected reachable external bindings, including a first-class function reference. This slice is complete when the new overload is available for local program emission and existing main-rendering expectations still pass. Iterate with `coreJVM/test:compile` and focused ClangGen tests; also run the focused suite on coreJS because this API is shared.

#### Invariants

- Existing renderMain callers retain standard external-resolution behavior.
- The explicit-resolver overload uses the same rendering traversal and error reporting as the existing renderer; it cannot silently fall back to standard resolution for a denied reference.
- The change preserves namespace type parameters and introduces no casts, runtime type dispatch or parallel rendering mechanism.

#### Property Tests

- None recorded.

#### Assertion Tests

- The legacy overload and explicit standard-resolver overload produce identical C for the same namespace and main.
- Allowing a referenced external renders its C symbol and include; denying it returns UnknownValue.
- A referenced external passed as a first-class function obeys the supplied resolver.

2. [ ] `step_2` Implement checked program selection through the shared local emission boundary

Extend `core/src/main/scala/dev/bosatsu/tool_command/EmitCCommand.scala` with a private checked emission-mode representation: tests select one bundle, while programs select one bundle and PackageName. Parse exactly one complete mode: `--test-packages FILE` or the pair `--main-packages FILE --main-package NAME`. Reject missing, incomplete and conflicting selectors before loading inputs or producing outputs.

Keep one common command path for manifest reading, selected-bundle presence checks, LocalRuntime loading, runtime-input collection and existing output alias checks. Require the program bundle to occur in the manifest using the platform's existing path ordering, then require NAME to occur in `scope.members(selected)`. A package elsewhere in `scope.packages` cannot satisfy root membership. Preserve LocalRuntime's existing loading and validation; no new manifest format or second bundle loader is needed.

For program rendering, construct the namespace with `CompilationSource.packageMapSource[Region](useStandardInt64 = false)`. Use `mainValues(ClangTranspiler.validMain(_).isRight)` for the selected package, preserving the existing last-valid-binding semantics, and obtain the native runner identifier through `validMain`. Report a useful error for a missing package or a package with no valid Main binding. Before authorizing the runner, validate the supplied Bosatsu/Prog definitions: Main has one constructor with one field; that field is a one-argument callback from List[String] returning external Prog[err, Int], allowing the quantifier placement used by `test_workspace/Prog.bosatsu`; Prog is opaque with the existing two covariant type parameters. Extend the current ProgTest layout check with a small shared check for these common requirements and the wrapper/result type, preserving its accepted behavior.

Tree-shake from only the selected main binding. Use the new renderMain overload with the same `stdExternals.filter(scope.hasBinding)` policy as test emission. Share the existing missing-native-binding diagnostic mapping and requested-header generation between the two local modes so neither branch bypasses declaration checks. Return C and requested headers together in `Output.TranspileOut`; FileCommandRunner/StagedOutputs remains the owning publication transaction. Keep mode-specific discovery separate without introducing a general emission framework.

Extend `cli/src/test/scala/dev/bosatsu/EmitCCommandTest.scala` through FileCommandRunner. Reuse compiled fixtures across bounded input permutations instead of repeatedly type-checking them. Cover the selectors, root membership, main selection/type, native layout, reachable bindings, loose Int64 behavior, deterministic output and output failure/recovery cases below. Exercise program input/output aliases through the same protections already tested for test mode. For an actual publication failure, request a legitimate generated header whose destination is a directory, with prior C/header siblings present; verify preservation and a successful retry after fixing the destination. Retain the existing StagedOutputs injected later-rename rollback tests as coverage of partial-commit restoration rather than adding emitter-specific publication hooks.

The complete production-site inventory for the new local contract is `EmitCCommand.opts`, its existing test renderer and new program renderer, and the explicit-resolver ClangGen main-rendering overload. Both command branches must converge on the shared load/output checks and return their whole output set through the existing publisher. Run focused EmitCCommand, ClangGen and StagedOutputs suites while implementing.

#### Invariants

- The parsed mode is either one test bundle or one program bundle/package pair; incomplete and mixed modes never reach emission.
- A program main must belong to the explicitly selected manifest bundle, even when a valid same-named selection exists elsewhere in the runtime graph.
- Main binding selection follows existing last-valid-binding semantics, and the native runner is used only after checking its required wrapper and Prog representation.
- Both local modes load the same validated graph with effective Predef once, disable standard Int64 assumptions for loose implementations and resolve reachable externals through declared bindings.
- Only requested valid headers are emitted, and C/header outputs cannot alias each other or declared runtime inputs.
- Every output set passes through FileCommandRunner/StagedOutputs; emission or publication failure preserves previous outputs and permits a subsequent successful request.

#### Property Tests

- For a fixed compiled program graph, enumerate manifest bundle permutations and repeated declared paths; generated C and requested headers remain byte-identical, and an unchanged rebuild preserves output mtime.
- Vary declarations for an unreachable native binding while keeping the selected main unchanged: C remains unchanged; making that binding reachable requires its declaration.
- Across table-driven selector, layout, binding and destination failures, compare prior output bytes before and after the failed request and then verify recovery through the same runner.

#### Assertion Tests

- Emit a valid private Main from a multi-package root bundle; choose the last valid Main binding and ignore a trailing unrelated test/value.
- Reject a selected bundle absent from the manifest, a missing package, a valid main found only in a dependency bundle, an empty package and a package containing only invalid main types.
- Reject no selector, either incomplete program selector and every combination mixing program selectors with --test-packages.
- Accept the real Prog/Main definitions and reject incompatible constructor count, field count, callback arguments/result, concrete Prog representation and incompatible Prog kinds; retain ProgTest checks.
- Reject missing reachable direct and first-class native bindings, accept declared providers, and reject an invalid generated-header request without publishing siblings.
- A loose Int64-named binding used with a literal remains a native call and requires its declared provider.
- Program C excludes unreachable dependency assertions and root test code, while the existing selected-test behavior remains passing.
- C and header paths cannot replace the manifest or a bundle through direct paths, symlinks or hard links; existing filesystem case-alias coverage remains passing.
- A real output publication failure preserves previous C/header siblings, leaves no new output files and is followed by a successful retry.

3. [ ] `step_3` Qualify compiled-only program emission and document the command contract

Extend `scripts/test_emit_c.py` using its existing producer/action split and `compile_group` helper. Compile the source-built Bosatsu/Prog, Demo/Types, Demo/Api and Demo/Main groups separately, with Main using Api and Api exposing Types so the fixture exercises implementation edges and interface exposure requirements. Main must construct a valid Main callback returning a Prog whose result is 7. Copy only implementation bundles into the emission action directory and write its local runtime manifest with the native symbols actually supplied by the fixture/runtime. Invoke `emit-c --main-packages main.pkg --main-package Demo/Main` under the existing empty-PATH environment using the resolved Java executable. Keep sources, signature files, compiler configuration and installed runtime outside that action directory.

Compile and link the emitted program in a separate subprocess using only the explicitly supplied `--cc-conf` configuration and requested generated headers, then assert exit status 7. Retain all three existing passing/failing private Test and ProgTest cases and their dependency-suite exclusion checks. The emitter itself must receive no C compiler/runtime-installation options and must create no inferred configuration or runtime installation.

Update `docs/design/hermetic-bazel-rules.md`: extend the local-emission section to include 0i, document the mutually exclusive selectors, show a program command, explain selected-bundle membership and existing main-binding selection, describe the native Main layout guard, and retain the external/header/publication guarantees for both modes. Mark tracker 0i complete with the implementation reference, following the neighboring tracker rows' implementing-PR convention. Do not mark later imported-scope or qualification items complete.

Validate the candidate with `sbt "coreJVM/test; cli/test; coreJS/test; cli/assembly"` and `./test_cli.sh`. Use the existing coverage entry point, `sbt "coverage; clean; coreJVM/test; cli/test; coverageReport"`, to inspect changed executable branches and close meaningful rejection/recovery coverage gaps; rebuild the assembly afterward if needed. Resolve the resulting candidate assembly to one concrete jar path. Prepare the vendored runtime and cc_conf using the Linux testC setup in `.github/workflows/ci.yml`, outside the empty-PATH emission action. Run `python3 scripts/test_emit_c.py --jar <resolved-candidate-jar> --cc-conf <resolved-cc-conf>` and `python3 scripts/test_bazel_worker.py --jar <resolved-candidate-jar>`. Follow roadmap #2482's named Linux-worker validation scope; macOS evidence remains PR CI's responsibility. The committed harness and documentation are the durable deliverables; no scratch evidence file or later merge checkpoint is required.

#### Invariants

- The native program is emitted from declared compiled inputs alone, with no source/signature/configuration discovery or host-tool lookup during emission.
- Native compilation/linking and vendored-runtime preparation occur outside the emission operation and use explicit caller inputs.
- The all-source program exits 7, and existing private Test/ProgTest passing and failing fixtures continue to produce their expected outcomes.
- Documentation accurately describes both implemented local modes and leaves later roadmap work incomplete.

#### Property Tests

- None recorded.

#### Assertion Tests

- The assembled candidate emits the separately compiled Prog/Types/Api/Main program under an empty PATH and the separately linked executable exits 7.
- The action directory contains only declared compiled inputs, its manifest and declared emission outputs; no source, signature or inferred runtime/configuration files are needed.
- The existing native test cases still yield exits 0, 1 and 1 and never run dependency assertions.
- The named JVM, CLI, Scala.js, CLI regression and candidate-jar worker checks pass; changed rejection and publication/recovery branches receive focused coverage.
