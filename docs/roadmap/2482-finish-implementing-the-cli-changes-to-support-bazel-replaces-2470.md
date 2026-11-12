# Roadmap #2482

> Generated from roadmap graph JSON.
> Edit the `.graph.json` file, not this `.md` file.
> MergeXO regenerates this file from the graph; manual edits are overwritten.

## Overview

Complete the eight remaining stage-0 tracker items in `docs/design/hermetic-bazel-rules.md` through seven implementation children, combining 0l and 0o. There are no proposed scope reductions requiring approval. Stage 1–6 Bazel rules/Gazelle and the design's optional producer/history features remain outside the source issue's requested boundary. Completed 0j remains intact. This is a fresh roadmap for #2482, with no dependency on abandoned #2470 checkpoint branches or its validation requirements.

Investigation used main/origin/main at 5b3fec0b27714053d7d0a25880eeb253664f494f, matching the checkout. The existing mechanisms to extend are the request-scoped FileCommandRunner/StagedOutputs lifecycle, BazelWorker.Operation registry and @params dispatcher, PackageSignatures/CompilerApi selection and compilation, GroupGraph validation, LocalRuntime compiled-bundle loading, and CompilationSource/CompilationNamespace with the existing versioned library backend. Extend these rather than introducing parallel command execution, verification or code-generation systems.

The main design choices are explicit program root-bundle selection, one shared digest implementation, edge authentication before library decoder deduplication, and preservation of per-member bindings from interface selection through native emission. The current local runtime manifest lists all reachable bundles without identifying the program's selected group; program_c therefore proposes --main-packages FILE alongside --main-package NAME. The decoder currently skips its loader on cached name/version edges, so inspect_imports verifies every incoming descriptor before deduplication. Current signature planning and bundle requirements are unscoped; import_scopes repairs this operation family once and hands mixed_c the same binding representation. These changes serve existing tracker acceptance criteria. Maintainers can amend these recommended implementation choices before execution; no initial approval gate or design-only investigation child is needed.

Shared properties: file actions use declared inputs, operate offline and publish only declared completed outputs through the existing staged lifecycle. Ordinary failures preserve prior outputs and cannot leak work into a later worker request. One-shot and persistent dispatch share the operation registry; worker compilation has no filesystem inference cache. Each member sees its own import/exposure closure, with at most one release per library name visible to that member; separate private selections remain legal. Every incoming library descriptor is authenticated against its selected bytes, including reused identities. Signature-only consumer inputs exclude full implementation digests. Both C emission modes retain scoped identities, effective Predef, interface-layout safeguards and explicit native bindings, and leave C compilation/linking to a separate caller. Native qualification compares requested components against one caller-pinned manifest, including every reachable reference to the selected core.

Sequencing and preflight: the first ready frontier is program_c, persistent_worker and toolchain_verify. Each has all turn-one inputs on main and ships its behavior, tests and documentation together. toolchain_verify owns SHA-256 support so inspection does not invent a second implementation. The second frontier is inspect_imports, receiving that implemented digest/verifier state directly; its remaining inputs already exist or are fixtures it can create. Then import_scopes receives inspection, selected-core verification and persistent-worker states directly; mixed_c receives all five foundations it uses; qualify_cli receives all six implementations directly. All dependencies require implemented because downstream needs merged executable behavior. Every child is a small_job producing an implemented state across multiple existing surfaces, with no single promised artifact path. No worker needs a guessed doc path, transitive artifact handoff, credential, deployment or human decision to begin. The graph was checked as acyclic with these five successive frontiers.

Validation is Linux-worker scoped. `scripts/container_setup.sh` provisions Java/sbt, Node/npm, Python, native tools, ASan/UBSan support and Valgrind, including the GCC ASan launcher added by #2479; the relevant tools are available in this checkout's worker. Each child names its worker checks and targets complete coverage of changed executable behavior. Native tests reuse the vendored-runtime setup in the Linux testC job and the existing candidate-jar/native harnesses. No child requires the complete CI matrix, an agent macOS run, remote-cache infrastructure, a future release event or production publishing credentials. macOS checks remain PR CI's responsibility and never block PR publication. Final qualification exercises the actual staged jar/core/archive tuple locally before wiring its production into release/CI.

The largest risks are incorrect scope rebinding across same-named private releases, accepting a second unauthenticated diamond edge, native-symbol collisions, and qualifying files different from those distributed. The dedicated scoped compilation/native fixtures and exact-artifact release handoff address those risks. Authoring also downloaded the released core pinned by `test_workspace/core_alpha_conf.json`, verified its BLAKE3 and decoded it; it has no dependency descriptors, so private-version and diamond evidence must come from constructed pinned serialized fixtures alongside that real legacy artifact.

### Key Goals

- Complete tracker items 0i, 0k, 0l, 0m, 0n, 0o, 0p and 0q in at most seven children while preserving completed 0j.
- Produce equivalent declared outputs and diagnostics through standalone, one-shot and persistent JVM execution, including failure recovery and edits at reused paths.
- Authenticate every imported edge and preserve per-member private-version bindings through strict interface checking, serialization, tree shaking and working native programs/tests.
- Verify and locally qualify the exact compiler/core/runtime tuple distributed by release/CI, while retaining compiler-only and matched-source operation.
- Run named Linux worker checks with complete changed-behavior coverage as the target; leave macOS checks to PR CI without blocking PR publication.

## Metadata

- Roadmap issue: `#2482`
- Graph version: `1`
- Node count: `7`

## Dependency Overview

1. `persistent_worker` (`small_job`): none
2. `program_c` (`small_job`): none
3. `toolchain_verify` (`small_job`): none
4. `inspect_imports` (`small_job`): `toolchain_verify` (`implemented`)
5. `import_scopes` (`small_job`): `inspect_imports` (`implemented`), `persistent_worker` (`implemented`), `toolchain_verify` (`implemented`)
6. `mixed_c` (`small_job`): `import_scopes` (`implemented`), `inspect_imports` (`implemented`), `persistent_worker` (`implemented`), `program_c` (`implemented`), `toolchain_verify` (`implemented`)
7. `qualify_cli` (`small_job`): `import_scopes` (`implemented`), `inspect_imports` (`implemented`), `mixed_c` (`implemented`), `persistent_worker` (`implemented`), `program_c` (`implemented`), `toolchain_verify` (`implemented`)

## Nodes

### `persistent_worker`

- Kind: `small_job`
- Title: 0k: Add the singleplex persistent JSON JVM worker
- Depends on: none

#### Body

Implement tracker item 0k without replacing the completed one-shot dispatcher or adding an inference cache.

Turn-one inputs: current default-branch code and the persistent-worker contract in `docs/design/hermetic-bazel-rules.md`; no upstream child state is required. `cli/src/main/scala/dev/bosatsu/PathModule.scala` currently accepts exactly one @parameter-file argument. `cli/src/main/scala/dev/bosatsu/BazelWorker.scala` supplies parameter decoding and the shared Operation registry. `cli/src/main/scala/dev/bosatsu/FileCommandRunner.scala` already owns a reusable compiler executor and request-local platforms, streams, supervised work and staged output publication. `cli/src/main/scala/dev/bosatsu/StagedOutputs.scala` supplies the existing publication lifecycle. `scripts/test_bazel_worker.py` currently expects persistent startup to fail, so that expectation must change in this same slice.

Add --persistent_worker startup and a sequential UTF-8 newline-delimited JSON loop. Dispatch received arguments directly through BazelWorker.Operation and FileCommandRunner; do not re-expand @arguments or maintain a second operation allowlist. Reuse one runner resource across requests and capture action output and diagnostics in the response. Match requestId, including its protobuf default of zero; supply the documented defaults, tolerate unknown input fields, and handle base64 input digests as opaque tokens. Recompute from current files on every request: missing digests and edits at the same path cannot reuse stale inferred state. Do not expose filesystem inference-cache options.

Establish the transport lifecycle in the existing request boundary: stdout contains only one complete, immediately flushed JSON response per supported action request, after outputs are closed/published and request-owned work and scratch cleanup have finished. Ordinary command or publication failure yields a nonzero response and leaves the process usable. Clean EOF ends successfully; malformed/truncated protocol input without a trustworthy request ends nonzero with transport diagnostics on stderr. Reject unsupported nonempty sandboxDir rather than changing the process working directory. Follow the documented v1 treatment of unsupported controls, and do not advertise cancellation or multiplexing. Reuse the existing bounded executor and immutable compiler resources; adding a cache or another execution engine would add correctness obligations without serving 0k.

Acceptance and Linux worker checks: extend `cli/src/test/scala/dev/bosatsu/BazelWorkerTest.scala`, `cli/src/test/scala/dev/bosatsu/FileCommandRunnerTest.scala` and `scripts/test_bazel_worker.py`. Drive an actual assembled JVM with good/bad/good requests, changed bytes at the same path, missing/empty digests, omitted and explicit IDs, unknown fields, spaces/non-ASCII paths, escaped multiline diagnostics, invalid commands, publication failure followed by success, clean EOF, malformed/truncated framing and restart. Repeat the strict-interface X/Sibling/Y and exposure cases from `cli/src/test/scala/dev/bosatsu/StrictInterfacesCommandTest.scala` through the transport. Compare status, diagnostic content and output bytes with direct and @params invocations; assert no stray stdout, scratch leakage or stale outputs. Check a response arrives before closing stdin so buffering cannot masquerade as success.

Run `sbt "cli/test; cli/assembly"`, `python3 scripts/test_bazel_worker.py --jar` with the resolved candidate assembly, and `./scripts/test_launcher_passthrough.sh`. Target complete coverage of new protocol and lifecycle branches. No Bazel rule installation, macOS run or full CI matrix is needed for this stage-0 transport check.

Completion: mark tracker 0k complete and document the actual v1 behavior in the existing design. Downstream receives implemented persistent/direct/one-shot parity and the reusable candidate-jar protocol harness. Future inspection and verification operations must join the same registry after their implementations land.

### `program_c`

- Kind: `small_job`
- Title: 0i: Emit program C from a selected compiled package group
- Depends on: none

#### Body

Implement tracker item 0i in `docs/design/hermetic-bazel-rules.md` as a bounded extension of the existing compiled-bundle emitter.

Turn-one inputs: the current default-branch implementation and design; no upstream child state is required. Verified foundations are `core/src/main/scala/dev/bosatsu/tool_command/EmitCCommand.scala`, `core/src/main/scala/dev/bosatsu/tool/LocalRuntime.scala`, `core/src/main/scala/dev/bosatsu/tool/LocalRuntimeManifest.scala`, `core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala`, and `core/src/main/scala/dev/bosatsu/codegen/clang/ClangGen.scala`. LocalRuntime already records bundle membership, validates implementation requirements, supplies implicit Predef and checks external declarations. EmitCCommand currently requires --test-packages and protects inputs against aliased C/header destinations. ClangTranspiler.validMain recognizes Bosatsu/Prog::Main; ClangGen.renderMain currently uses standard external resolution directly, whereas test rendering accepts a filtered resolver.

Recommended approach: add --main-package and reuse local runtime loading, tree shaking, C rendering and staged publication. Resolve the design's otherwise unspecified root-group membership explicitly: introduce a proposed --main-packages FILE option paired with --main-package NAME, analogous to --test-packages FILE. The selected bundle must occur in the runtime manifest and contain the named package. Require exactly one emission mode: the program pair or the existing test selector. Document this small command-contract clarification in the existing design. Selecting a name from the union of every reachable bundle cannot enforce the required rejection of a main outside its root group; introducing a second runtime-manifest format solely for that selection is unnecessary.

Validate the main binding and the native Main wrapper layout before using the runtime runner. `test_workspace/Prog.bosatsu` defines Main's callback layout, and `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c` implements bsts_Bosatsu_Prog_run_main. Follow the existing ProgTest layout-checking approach where applicable. Extend renderMain's existing rendering path to honor the same declared external-binding restrictions as tests. Preserve custom loose Int64 semantics, effective Predef exactly once, deterministic enumeration, generated-header checks, input/output alias protection and rollback. Neither emission mode may discover or invoke a host compiler or install a runtime.

Acceptance and Linux worker checks: extend `cli/src/test/scala/dev/bosatsu/EmitCCommandTest.scala` and the relevant core C-backend tests. Cover valid and invalid mains, absent bundle/package, a valid main located only in a dependency bundle, conflicting/incomplete selectors, incompatible native layout, reachable missing externs, deterministic C and failed publication preserving previous outputs. Extend `scripts/test_emit_c.py` to compile the all-source Prog/Types/Api/Main bundles separately, emit using only declared compiled inputs with an empty PATH, then separately compile/link and observe exit 7. Retain its passing/failing private Test and ProgTest cases. Run `sbt "coreJVM/test; cli/test; coreJS/test; cli/assembly"`, the extended native harness with its explicit --jar and --cc-conf inputs from the existing Linux testC vendored-runtime setup in `.github/workflows/ci.yml`, and `python3 scripts/test_bazel_worker.py --jar` with the resolved candidate assembly. Target complete coverage of changed behavior, including rejection and rollback branches; macOS evidence belongs to PR CI.

Completion: update tracker 0i and its command documentation with the implementing PR. Downstream receives this child's implemented state: both local emission modes, the explicit program root-bundle contract, and their executable/native regression fixtures. No single artifact path is promised.

### `toolchain_verify`

- Kind: `small_job`
- Title: 0p: Verify declared compiler, core and runtime components
- Depends on: none

#### Body

Implement tracker item 0p and the shared SHA-256 support needed by subsequent import verification.

Turn-one inputs: the current default branch and the detached-manifest contract in `docs/design/hermetic-bazel-rules.md`; no upstream child state is required. Verified extension points are `core/src/main/scala/dev/bosatsu/hashing/Algo.scala`, `core/src/main/scala/dev/bosatsu/tool/BuildManifest.scala`, `core/src/main/scala/dev/bosatsu/tool_command/ToolCommand.scala`, `core/src/main/scala/dev/bosatsu/PlatformIO.scala`, `cli/src/main/scala/dev/bosatsu/FileCommandRunner.scala` and `cli/src/main/scala/dev/bosatsu/BazelWorker.scala`. Algo currently parses only BLAKE3. `build.sbt` generates BuildInfo.cRuntimeArchiveHash from BOSATSU_C_RUNTIME_HASH; `core/src/main/scala/dev/bosatsu/cruntime/Command.scala` treats an empty value as unstamped and rejects a malformed advertised value. BuildInfo is generated, not a checked-in Scala source.

Add the designed file-only tool verify-toolchain operation with --manifest, --compiler-jar, --out and requested --core-library or --core-source inputs and --runtime-archive. Extend the existing digest abstraction/parser with SHA-256 while preserving BLAKE3 parsing, hashes and CAS behavior. Use a JVM/Scala.js-compatible implementation and test both platforms; avoid a separate digest syntax or verification subsystem for this command. Reuse strict UTF-8/versioned JSON parsing and staged publication. Register the implemented command in the ordinary file runner and shared worker registry.

The manifest is a caller-trusted input: the launcher must authenticate its pin and the jar before execution. State that precondition explicitly; successful self-inspection cannot authenticate an untrusted running jar. Verify the supplied compiler bytes against the selected manifest and only read additional components requested by the target. Binary core verification checks full SHA-256 and embedded library name/version. Source mode checks listed package names and exact source bytes from the snapshot used for validation; reject modified/unlisted replacements and incompatible binary/source choices. For native verification, hash the compressed runtime archive and additionally enforce the running candidate's advertised runtime hash when present. An empty development stamp is allowed with an explicitly pinned manifest; malformed or conflicting nonempty values fail. Compiler-only verification must work with unused core/runtime files absent.

Properties established: a stamp is created or replaced only after all requested checks succeed; failure preserves previously published outputs. Separate SHA-valid substitutes cannot override the selected manifest's component digests. Checks remain offline and invoke no compiler, runtime installer or resolver. Full manifest and implementation digests remain outside ordinary signature-only compile inputs. Release generation and publication are deferred to qualify_cli, where the final artifacts exist.

Acceptance and Linux worker checks: add focused command/digest tests covering known SHA-256 vectors, incremental hashing, existing BLAKE3 compatibility, malformed manifest/UTF-8/hash values, requested-component reads, missing files, same-name/version altered core, incorrect embedded core identity, changed source bytes/package name, and each component replaced with an independently valid but mismatched pin. Exercise absent, valid, malformed and conflicting embedded runtime hashes; test no new/replaced success stamp on failure and one-shot worker parity. Candidate-jar tests must launch the exact jar supplied as --compiler-jar. Run `sbt "coreJVM/test; cli/test; coreJS/test; cliJSJS/test; cli/assembly"` and the candidate-jar verification cases added to `scripts/test_bazel_worker.py`. Use local fixtures for mismatch cases; production release credentials are unnecessary. Target complete coverage of changed executable behavior; macOS checks remain PR CI's responsibility.

Completion: update tracker 0p and document the concrete manifest/command policy. Downstream receives implemented shared SHA-256/BLAKE3 verification, the requested-component verifier, and its documented manifest contract.

### `inspect_imports`

- Kind: `small_job`
- Title: 0l and 0o: Inspect pinned library files and authenticate every dependency edge
- Depends on: `toolchain_verify` (`implemented`)

#### Body

Implement tracker items 0l and 0o together using one declared-file verification and inspection path.

Turn-one handoff: toolchain_verify implemented supplies shared SHA-256/BLAKE3 support and the component-verification conventions. Existing inputs are `core/src/main/scala/dev/bosatsu/library/DecodedLibrary.scala`, `core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala`, `core/src/main/scala/dev/bosatsu/PackageSignatures.scala`, `core/src/main/scala/dev/bosatsu/ProtoConverter.scala`, `core/src/main/scala/dev/bosatsu/tool/BuildManifest.scala`, `proto/src/main/protobuf/bosatsu/TypedAst.proto`, and `docs/design/hermetic-bazel-rules.md`.

Authoring verified the recurring defect: DecodedLibrary.decodeWithDeps caches by name/version and can return a cached dependency without invoking its loader for a second referring edge. Wrapping only that loader cannot authenticate a diamond's second descriptor. Extend the existing declared-file library decoding mechanism with an edge-verification pass before decoder deduplication. Retain referring identity, public/private/legacy unused-transitive-public visibility, expected hashes and witness paths. Verify each incoming descriptor against the same byte snapshot subsequently parsed, even on identity reuse. Coalesce equal identities with equal bytes; reject different bytes claiming the same identity. Preserve separate versions as separate scopes. All in-scope full inspection and later mixed decoding must consume this verified graph rather than bypassing it through an unchecked decoder call.

Implement tool inspect-library and --descriptors-only. Descriptor-only inspection must authenticate one pinned file and its expected identity before exposing its descriptors; it must not require already-downloaded child files or typed dependency decoding, since the repository-host resolver needs those descriptors to discover children. Full inspection takes an explicit declared dependency-file context and fails if it is incomplete. Use a small versioned JSON contract for that context and the inspection metadata, documented and tested in the existing design. Keep signature projections independently requestable. Emit canonical exported signatures, typed per-package exposure/provider ownership, membership, descriptors and a scoped implementation view referencing authenticated artifacts. Keep full artifact identity metadata separate from signature bytes so a private implementation edit can preserve the public projection.

File acquisition belongs to the caller. Inspect neither fetches URIs nor updates a lockfile. The resolver-facing workflow repeatedly verifies a child against the authenticated parent descriptor before trusting that child's descriptors or deriving its SHA-256. Verify supported recorded digests consistently, retain legacy BLAKE3 support and diagnose conflicting pins. Share this root/edge verification between descriptor and full modes instead of implementing two readers. Register the completed pure operation through ToolCommand, FileCommandRunner and BazelWorker.Operation.

Legacy evidence available now: `test_workspace/core_alpha_conf.json` pins core_alpha 9.0.1 at its v0.0.69 release URI. During authoring, downloading and decoding those 937080 bytes confirmed BLAKE3 098f5eff00611599b54b5e6bce8b7696c8fdf89f4e99f8bfbbec521764ed8927 and SHA-256 b6e7d87fcb786084dd90da490fe9514678fee7f6920fc17e0abdf28210a11dff. That artifact has no dependency descriptors. Use it for real legacy compatibility, and construct pinned serialized Parser, diamond and private-version fixtures using the patterns in `core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenLibraryDepsTest.scala`; do not assume the legacy core alone exercises dependency traversal. Stage external fixture acquisition before offline command tests and retain its pin in the test setup.

Acceptance and Linux worker checks: test real legacy projection, missing dependencies, unindexed typed exposures, private-body projection stability, deterministic reordered inputs, two private versions with referring edges, equal-identity aliases, conflicting bytes, and a second diamond edge with a bad digest after a valid first edge. Enumerate the Parser fixture closure file by file, verify a BLAKE3-only child before deriving SHA-256, and reject tampering before exposing child descriptors. Check failed inspection preserves all previous outputs and direct/@params parity. Run `sbt "coreJVM/test; cli/test; coreJS/test; cliJSJS/test; cli/assembly"` plus the extended candidate-jar inspection cases in `scripts/test_bazel_worker.py`. Target complete changed-behavior coverage; macOS evidence remains with PR CI.

Completion: mark 0l and 0o complete together. Downstream receives implemented authenticated root/edge traversal, scoped inspection/projection contracts and reusable pinned graph fixtures.

### `import_scopes`

- Kind: `small_job`
- Title: 0m: Preserve per-member imported bindings through validation and compilation
- Depends on: `inspect_imports` (`implemented`), `persistent_worker` (`implemented`), `toolchain_verify` (`implemented`)

#### Body

Implement tracker item 0m across the existing graph, signature-selection and compilation boundary.

Turn-one handoffs: inspect_imports implemented supplies authenticated scoped graphs, exported projections, exposure/provider ownership and graph fixtures; toolchain_verify implemented supplies the selected-core manifest and digest-verification contract; persistent_worker implemented supplies warm/restarted protocol execution and its candidate-jar harness. These are all direct inputs because this child uses each state in implementation or acceptance.

Verified current limitations: `core/src/main/scala/dev/bosatsu/tool/GroupGraph.scala` and `core/src/main/scala/dev/bosatsu/tool_command/ValidateGraphCommand.scala` currently model local groups. `core/src/main/scala/dev/bosatsu/PackageSignatures.scala` indexes providers by unqualified package name across a batch. `core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala` builds source requirements, selects interfaces and stages source/foreign-interface decoding. `core/src/main/scala/dev/bosatsu/tool_command/CompilePackagesCommand.scala` passes no filesystem cache. `core/src/main/scala/dev/bosatsu/tool/Output.scala` writes package-wide unscoped build requirements, defined in `proto/src/main/protobuf/bosatsu/TypedAst.proto`. One unqualified inference environment cannot represent incompatible definitions with the same nominal package/type names.

Extend these mechanisms with the designed --scope-map FILE contract for compile-packages. Bind each referring local member or imported scope and required package to its provider scope and one declared interface file. Derive and validate bindings against real dependency edges and the authenticated inspection index. Resolve each member's imports and selected APIs' exposures to a fixed point before taking the union of files for strict-unused checking. Do not pull implementation-only dependencies or unrelated exports of a wide signature into the visible environment. Partition inference and decoding where bindings differ, sharing environments only when their bindings agree; preserve source-to-source batch dependencies and source-supplied exposed owners.

Use one scoped binding representation through selection, inference, output metadata and later typed-bundle decoding. Extend serialized requirements only as needed to identify the referring member and selected owner; preserve old local bundle/signature compatibility and existing layout-digest safeguards. Provider switches must be revalidated even when signature bytes match. Keep full implementation hashes in artifact verification/composition rather than ordinary consumer signature keys; private body edits behind a wrapper must preserve the outer consumer's signature inputs. No worker inference cache is introduced.

Extend structural checks to scoped library boundaries and the selected toolchain core. A single member's visible closure may select only one release per library name, even when versions export different package names or byte-identical signatures. Different members may have disjoint legal selections. Retain both witness paths for conflicts. Authenticate all reachable descriptor edges, including private and legacy edges, while using only selected exposures for type visibility. Coalesce identical core references and reject any transitive core identity/digest incompatible with the selected manifest, including private paths. Preserve local membership-only checks, one full-root traversal, diamond deduplication and linear manifest-visit behavior; do not make each group rescan its full closure.

Acceptance and Linux worker checks: extend `core/src/test/scala/dev/bosatsu/PackageSignaturesTest.scala`, `core/src/test/scala/dev/bosatsu/GroupGraphTest.scala`, `cli/src/test/scala/dev/bosatsu/CompilePackagesCommandTest.scala`, `cli/src/test/scala/dev/bosatsu/StrictInterfacesCommandTest.scala` and `cli/src/test/scala/dev/bosatsu/ValidateGraphCommandTest.scala`. Cover legal private versions, disjoint members in one batch, reversed argument/label order, visible conflicts with identical signatures and with different package names, missing/ambiguous bindings, source/import exposure chains, extra interfaces, shared diamonds and private-path core conflicts. Round-trip member bindings and requirements for downstream decoding. Warm a worker with success, replace a visible binding at the same path with a conflicting release, and require the same diagnostic as cold/restarted execution. Repeat existing local graph visit-count and membership-cache-input regressions. Run `sbt "coreJVM/test; cli/test; coreJS/test; cliJSJS/test; cli/assembly"` and the scoped compilation/validation cases added to `scripts/test_bazel_worker.py`. Target complete coverage of changed behavior; leave macOS checks to PR CI.

Completion: update tracker 0m and the existing design's scoped contracts. Downstream receives implemented caller-relative bindings, scoped typed-bundle metadata, selected-core composition validation and executable warm/cold fixtures.

### `mixed_c`

- Kind: `small_job`
- Title: 0n: Emit and run programs and tests with scoped imported implementations
- Depends on: `import_scopes` (`implemented`), `inspect_imports` (`implemented`), `persistent_worker` (`implemented`), `program_c` (`implemented`), `toolchain_verify` (`implemented`)

#### Body

Implement tracker item 0n for both program and test emission, preserving the validated scopes through the existing backend.

Turn-one handoffs: program_c implemented supplies both local emission selectors and root-group enforcement; inspect_imports implemented supplies authenticated implementation graphs and pinned fixtures; import_scopes implemented supplies per-member bindings and serialized requirements; toolchain_verify implemented supplies component/core-source qualification; persistent_worker implemented supplies the protocol and parity harness. Each is a direct dependency because this child consumes that behavior or contract itself.

Verified extension points are `core/src/main/scala/dev/bosatsu/tool/LocalRuntime.scala`, `core/src/main/scala/dev/bosatsu/tool/LocalRuntimeManifest.scala`, `core/src/main/scala/dev/bosatsu/tool_command/EmitCCommand.scala`, `core/src/main/scala/dev/bosatsu/codegen/CompilationSource.scala`, `core/src/main/scala/dev/bosatsu/codegen/CompilationNamespace.scala`, and `core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala`. The package-map backend has one Unit scope; the library backend already uses name/version keys and version-qualified non-root C identifiers. `core/src/main/scala/dev/bosatsu/tool_command/RuntimeCommandSupport.scala` flattens its older file-runtime path, which cannot preserve this mixed graph. `core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenLibraryDepsTest.scala` already constructs incompatible private Shared/Api versions and asserts distinct emitted symbols, but that test alone does not link/run a mixed CLI graph.

Extend the existing runtime loader and CompilationSource/CompilationNamespace path with a loose local root plus authenticated library scopes. Retain unique loose package names while giving members with different bindings distinct resolution contexts. Decode every typed member against the bindings recorded by compilation; verify scoped interface/layout requirements before lowering. Route main selection and discovered private test roots through their member contexts. Preserve library name/version keys through Matchless lowering, global inlining, tree shaking and C identifiers. Missing or ambiguous scoped references must fail before emission rather than falling back to a root scope. Do not synthesize a published library around loose groups or flatten private releases into one package map.

Extend the runtime-manifest contract compatibly and document its actual versioned schema in the existing design. Reuse inspect_imports' verification boundary for artifact bytes and every incoming edge. For selected standard core/runtime implementations, consume the component and graph checks established by the prerequisites; carry matched source origin through composition. Keep the existing local schema usable without library files. Both modes must retain Predef once, selected-group membership, private test discovery/order, stale-layout checks, declared header/output alias protection and staged rollback.

Native external bindings need scoped implementation identity as well as ABI. Reuse the existing C external resolver and naming conventions. Share an unversioned symbol only when releases select the same verified native implementation with a compatible ABI. Distinct native implementations need distinct symbols; reject bindings that cannot be separated before linking, even if signatures match. Keep custom loose Int64 calls unless verified standard implementation identity authorizes intrinsics. An external declaration or manifest symbol list alone supplies no native object code. Emission remains file-only and never invokes the host compiler.

Acceptance and Linux worker checks: extend `scripts/test_emit_c.py`, `cli/src/test/scala/dev/bosatsu/EmitCCommandTest.scala`, `core/src/test/scala/dev/bosatsu/LocalRuntimeScopeTest.scala`, `core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenLibraryDepsTest.scala` and `core/src/test/scala/dev/bosatsu/codegen/clang/ExternalNamespaceTest.scala`. From declared pinned files, emit, separately link and run a mixed native program and private Test/ProgTest. Link/run both incompatible private releases in one executable and check both results; repeat for separately bound members in one group and reversed input order. Reject a single member's visible version conflict, stale scoped requirements, incompatible core and inseparable native bindings. Cover shared-native-implementation success and distinct-implementation separation. Retain all-source exit-7 and dependency-test-exclusion regressions. Compare direct, @params and persistent output bytes/diagnostics, including success after failure and changed files at reused paths.

Run `sbt "coreJVM/test; cli/test; coreJS/test; cliJSJS/test; cli/assembly"`, the extended `scripts/test_emit_c.py` using the Linux testC vendored-runtime setup, and the extended `scripts/test_bazel_worker.py` against the candidate jar. Run `./scripts/test_c_sanitizers.sh` and `./scripts/test_c_valgrind.sh`, including coverage of the new emitted native fixtures under their respective instrumentation. These are worker-supported Linux checks; macOS remains PR CI's responsibility. Target complete changed-behavior coverage.

Completion: update tracker 0n. Downstream receives implemented mixed program/test emission and native executable fixtures covering scoped private versions, core compatibility and external binding identity.

### `qualify_cli`

- Kind: `small_job`
- Title: 0q: Qualify the exact release tuple and migrate file-build callers
- Depends on: `import_scopes` (`implemented`), `inspect_imports` (`implemented`), `mixed_c` (`implemented`), `persistent_worker` (`implemented`), `program_c` (`implemented`), `toolchain_verify` (`implemented`)

#### Body

Implement tracker item 0q after the complete stage-0 CLI is available. This child owns release-manifest production, exact-artifact qualification and the remaining repository caller integration.

Turn-one handoffs: program_c implemented supplies local program emission and root selection; persistent_worker implemented supplies the candidate-jar transport; toolchain_verify implemented supplies manifest/component verification; inspect_imports implemented supplies legacy and authenticated import inspection; import_scopes implemented supplies scoped compile/graph validation; mixed_c implemented supplies both scoped emission modes and native fixtures. Receive all six satisfied states directly, rather than reconstructing contracts through transitive ancestry.

Authoring verified `.github/workflows/release.yml`: prepare archives c_runtime and calculates its BLAKE3 before build_cli embeds BOSATSU_C_RUNTIME_HASH; the release job downloads the jar/runtime, separately assembles again for library publication, stages libraries and writes SHA256SUMS/B3SUMS. No detached compatibility manifest is currently generated. `scripts/publish_bosatsu_libs.sh` uses the checkout's bosatsuj launcher; `core/src/main/scala/dev/bosatsu/library/Command.scala` provides publish --dry-run, which emits library files without changing library configs or CAS. `bosatsu_libs.json` selects core_alpha. These mechanisms can produce the qualification artifacts without inventing a producer subsystem or executing an actual release.

Extend the existing release/CI flow to archive the runtime once, stamp and stage the candidate jar, build core with that exact staged jar, then generate the detached manifest from the final jar, full core bytes, compressed runtime archive and supported source-replacement snapshots. Do not silently select a newly assembled jar or recreate the archive after hashing. Derive embedded core identity from the produced artifact. Compute and hand off the manifest's SHA-256 outside the manifest, and include the final manifest in release checksum/assets generation. Reuse toolchain_verify for validation. Keep a jar-only handoff with its pin available independently of core/runtime; full handoff supplies the pinned manifest and all three local files. Publish no full toolchain tuple until its qualification step has passed. Workflow code is the deliverable; workers do not create a release, use publishing credentials or require a live release event.

Exercise the exact staged tuple with a native program using Bosatsu/Prog and a ProgTest calling a core external. Also retain the matched-source all-local exit-7 case. Read only declared files during verification/emission; acquisition and runtime construction occur in preparation. Run file-command invocations in a clean directory with an empty PATH and read-only home, an explicit JVM, and a writable private JNA extraction directory supplied through the documented -Djna.tmpdir launch property. Include a changed-existing-output rebuild as well as a first build so the JNA publication path is exercised. Compare successful/failing requests and outputs across standalone, one-shot and persistent execution, including inspection and verification operations added after 0k.

Cut over build-only artifact-generation callers to compile-packages and emit-c where they are superseded, including the relevant Linux CI, sanitizer and Valgrind paths in `.github/workflows/ci.yml`, `scripts/test_c_sanitizers.sh` and `scripts/test_c_valgrind.sh`. Preserve their runtime, assertion-output, ProgTest, native build/test and instrumentation scenarios. Keep interactive tool check/transpile behavior and callers that still need it, including Python transpilation; removing those commands is unnecessary for this consumer milestone. Update `test_cli.sh`, existing candidate-jar/native harnesses and `docs/design/hermetic-bazel-rules.md` together. Avoid a wholesale rewrite of unrelated workflow jobs.

Acceptance and Linux worker checks: run `sbt "coreJVM/test; cli/test; coreJS/test; cliJSJS/test; cli/assembly; cliJSJS/fullOptJS"`, `./test_cli.sh`, the expanded `scripts/test_bazel_worker.py` and `scripts/test_emit_c.py`, and the migrated Linux testC runtime/generated-C checks from `.github/workflows/ci.yml`. Run `./scripts/test_c_sanitizers.sh` and `./scripts/test_c_valgrind.sh` with the new compiled-bundle cases included. Run the workflow's testPY generation/unittest commands and testWithNode CLI smoke commands to detect shared-code/CLI regressions. Exercise manifest production and qualification locally with staged files and dry-run core generation, round-trip every digest/identity, and substitute each artifact to check rejection before native link. Verify source replacement tampering and compiler-only operation with core/runtime absent. Use the repository's existing `coverage`, coreJVM/cli tests and `coverageReport` tasks to inspect changed executable paths and target 100% coverage of their behavior; no repository-wide 100% threshold is introduced.

The worker runs these Linux checks only. Hosted macOS checks remain with PR CI and never gate publication of this child's PR. Existing unrelated CI/platform work is outside this change. Qualification evidence consists of local candidate-jar transcripts, manifests/digests and native fixture results for the exact tested files; successful future hosted release runs are not a prerequisite for merging workflow changes.

Completion: update tracker 0q with its implementing PR, confirm every requested incomplete 0-prefixed item is now complete, leave completed 0j intact, and document compiler-only/full handoffs in the existing design. No actual Bazel rules, Gazelle implementation or optional producer/history/publication features are required by issue #2482.
