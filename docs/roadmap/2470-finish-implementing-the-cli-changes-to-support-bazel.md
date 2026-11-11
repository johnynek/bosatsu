# Roadmap #2470

> Generated from roadmap graph JSON.
> Edit the `.graph.json` file, not this `.md` file.
> MergeXO regenerates this file from the graph; manual edits are overwritten.

## Overview

Implement all remaining numbered Stage 0 items, 0i–0q, from docs/design/hermetic-bazel-rules.md for source issue johnynek/bosatsu#2470. Authoring inspected main at e1f20fb985bb98e38966f9af6ccbf7f2b8d520cb; the checkout and main resolve to that revision. The tracker and executable code agree that 0a–0h have landed. No implementation or tests were run as part of this roadmap-authoring turn. There are no proposed scope reductions requiring approval. Product Bazel rules/toolchains/Gazelle in Stages 1–6, optional select-interfaces, and the optional producer/history/publication phase are outside this issue's requested remaining 0-prefixed work.

Extend the existing FileCommandRunner/RequestPlatform/StagedOutputs lifecycle, canonical PackageSignatures and codec machinery, GroupGraph validator, Algo hash abstraction, and CompilationSource/CompilationNamespace C backend. Combine 0l and 0o because descriptor bootstrap and full inspection must share artifact verification; keep their distinct completeness guarantees explicit. Preserve existing interactive commands while qualifying the new file surface. No preliminary design/probe document or implementation-approval gate is needed: the repository design supplies the direction and authoring inspection resolves the immediate choices. Maintainers may amend the proposed root-bundle flag clarification or other documented choices before execution.

The central invariants are: worker requests use only registered file operations and declared inputs, observe current input contents, and finish request-owned work/publication/cleanup before responding; every followed library descriptor is verified against the selected original artifact bytes even on a reused identity; compilation follows each member's imports and exposure closure in its own provider context; private implementations do not enter consumer signatures; native generation preserves caller-relative library identities and checked interface/layout requirements; selected core/runtime compatibility and source origins are checked before producing a qualified native executable. Standard-named externs alone cannot authorize standard native semantics. Existing output rollback, alias protection, deterministic outputs, and byte-identical output retention remain applicable.

The largest risk is loss of scope identity across the current batch-wide package-name maps in PackageSignatures, CompilerApi, PackageMap, and ProtoConverter, followed by unversioned external C symbols. Address that recurring operation family in one scoped-compilation child and one mixed-emission child. Avoid a new cross-request cache, serialized synthetic source library, or replacement compiler. Record source provenance only where the source-replacement compatibility requirement needs it, keeping it out of public signatures. Release qualification must use the actual staged jar/core/archive, not an independently rebuilt assembly.

Preflight simulated ready frontiers are (1) local_main, worker_args, import_inspection and (2) worker_json, toolchain_check. The first frontier extends existing executable boundaries with its own tests and exposes no release configuration dependent on unmerged foundations. The second receives implemented dispatcher or verifier state directly and remains independently merge-safe. Later frontiers are scoped_compile, mixed_emit, then qualify_cli. Release manifest generation, distribution handoffs, and end-to-end rollout documentation land only in the last executable slice. All edges require implemented states, explicitly named in consumer briefs; no worker needs a transitive artifact handoff. There are no doc-producing nodes or promised standalone artifact paths.

Workers can create synthetic pinned library/graph fixtures and assembled jars themselves. Compatibility setup can obtain the predecessor release pinned in test_workspace/core_alpha_conf.json, then pass it as a declared file to offline commands. Existing CI/runtime setup supplies native test prerequisites; the final child adds the bounded Bazel transport fixture and its provisioning. Tests and CI logs establish repository behavior and qualification of an exact tuple; completion requires neither a deployment, live publication, new credentials, nor external attestation.

Follow the verified coding_style.md and build.sbt contracts: immutable public APIs, typed validated boundaries, capability-driven equality/hash/order/rendering for generic values, strict Scala warnings/explicit nulls, and existing formatting. Target 100% coverage of changed executable behavior, including errors and recovery. Use focused tests while iterating, then sbt "coreJVM/test; cli/test" and ./test_cli.sh for CLI changes. Use sbt "coverage; clean; coreJVM/test; cli/test; coverageReport" to inspect changed-line coverage without weakening repository thresholds. Shared core changes also require relevant Scala.js checks; backend/runtime changes require the applicable full matrix in .github/workflows/ci.yml, including native sanitizer/Valgrind coverage. Each child updates only the design contracts/tracker rows it actually completes.

### Key Goals

- Complete every remaining tracker item 0i–0q with tests and implementation provenance.
- Generate program and private-test C from local or mixed compiled inputs while preserving scoped identities, native binding checks, and deterministic declared outputs.
- Run the same safe file operations through standalone, one-shot parameter-file, and persistent JSON entry points with matching results and reliable failure recovery.
- Authenticate imported artifact edges and selected compiler/core/runtime/source components without network access inside file commands or workers.
- Generate and qualify the detached manifest against the exact distributable tuple, including real jar/Bazel transport tests and native program/ProgTest execution.
- Preserve existing CLI compatibility and lifecycle guarantees while targeting 100% coverage of changed executable behavior.

## Metadata

- Roadmap issue: `#2470`
- Graph version: `1`
- Node count: `8`

## Dependency Overview

1. `import_inspection` (`small_job`): none
2. `local_main` (`small_job`): none
3. `toolchain_check` (`small_job`): `import_inspection` (`implemented`)
4. `scoped_compile` (`small_job`): `import_inspection` (`implemented`), `toolchain_check` (`implemented`)
5. `mixed_emit` (`small_job`): `import_inspection` (`implemented`), `local_main` (`implemented`), `scoped_compile` (`implemented`), `toolchain_check` (`implemented`)
6. `worker_args` (`small_job`): none
7. `worker_json` (`small_job`): `worker_args` (`implemented`)
8. `qualify_cli` (`small_job`): `import_inspection` (`implemented`), `local_main` (`implemented`), `mixed_emit` (`implemented`), `scoped_compile` (`implemented`), `toolchain_check` (`implemented`), `worker_args` (`implemented`), `worker_json` (`implemented`)

## Nodes

### `import_inspection`

- Kind: `small_job`
- Title: 0l and 0o: Inspect pinned libraries through one declared-file verifier
- Depends on: none

#### Body

Complete tracker items 0l and 0o together. Implement tool inspect-library with independently requestable consumer projections and --descriptors-only inspection. Both modes must use the same artifact verification mechanism; acquisition remains outside the compiler command.

Turn-one inputs already exist on main: core/src/main/scala/dev/bosatsu/hashing/Algo.scala, core/src/main/scala/dev/bosatsu/library/DecodedLibrary.scala, core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala, core/src/main/scala/dev/bosatsu/PackageSignatures.scala, core/src/main/scala/dev/bosatsu/tool/CommandSupport.scala, core/src/main/scala/dev/bosatsu/tool_command/ExtractIfaceCommand.scala, core/src/main/scala/dev/bosatsu/tool/BuildManifest.scala, cli/src/main/scala/dev/bosatsu/IOPlatformIO.scala, proto/src/main/protobuf/bosatsu/TypedAst.proto, and test_workspace/core_alpha_conf.json. There are no upstream dependencies. Downstream workers receive the implemented verifier, inspection formats, canonical projections, and verified scoped identities/edges.

Verified facts: Algo.parseIdent accepts only BLAKE3 today. IOPlatformIO.readLibrary hashes the original bytes. DecodedLibrary.decodeWithDeps keys its cache by name/version and can reuse a child without invoking its loader for a second incoming descriptor. Public, private, and unused-transitive-public dependencies exist in the wire format. ExtractIfaceCommand only removes implementations and retains the library envelope, private descriptors, history, and other metadata. PackageSignatures already supplies canonical public signatures and legacy exposure discovery. No serialized library fixture is committed; core_alpha_conf.json supplies an existing released predecessor coordinate and BLAKE3 pin for compatibility setup.

Extend Algo's existing hash abstraction/parser to support SHA-256 while retaining BLAKE3 and cross-platform behavior. Verify a root against an external expected digest and identity before trusting its metadata. Full inspection takes an explicit declared-file dependency mapping, verifies every incoming descriptor against the selected child's original bytes and embedded name/version, and only then semantically decodes the graph. Coalesce identical verified name/version identities, reject different bytes at one identity, and preserve distinct private versions. Verify all descriptor edges even when decoding reuses a child. Use immutable checked graph values to carry this result into projection and downstream consumers rather than treating an unchecked decoder cache hit as authentication.

Descriptor-only mode verifies the supplied artifact and returns its identity and outbound expected hashes, URIs, visibility, and referring scope without requiring unavailable children or decoding their interfaces. It must distinguish those declared edges from an already verified closure. This is the bootstrap interface a repository-host resolver can invoke while acquiring children. Full inspection requires the declared verified closure. Neither mode follows URIs or reads a global CAS.

Emit deterministic exported signatures/exposures, owner/provider scope identities, scoped implementation metadata, membership, and an external descriptor as independently requested outputs. Consumer signatures use the existing canonical signature reader/encoder and exclude implementation, release, and history metadata. Document their distinction from the legacy extract-iface library envelope. Derive exposure information for old releases from typed exports in their verified dependency context. Preserve existing legacy command compatibility. Register inspection as a safe file operation using the worker registration mechanism available when this child lands.

Acceptance: known-vector and incremental SHA-256/BLAKE3 tests; a pinned existing release supplied as a declared local fixture; legacy exposure recovery; incomplete context; bad root and child hashes; wrong identities; diamonds including a conflicting second incoming digest; legal private versions; same-identity conflicting bytes; and deterministic ordering. A resolver test driver must enumerate a pinned fixture closure, authenticate a BLAKE3-only child before deriving its SHA-256, and reject a tampered child. Private-body edits must preserve projected signatures. Exercise staged publication and reused requests. Setup may fetch the pinned release; command execution must be file-only. Mark both rows complete after their checks pass.

### `local_main`

- Kind: `small_job`
- Title: 0i: Emit program C from a selected compiled package group
- Depends on: none

#### Body

Complete tracker item 0i in docs/design/hermetic-bazel-rules.md. Produce a working program-emission alternative to the existing test-emission path, using compiled bundles and explicit file inputs.

Turn-one inputs already exist on main: core/src/main/scala/dev/bosatsu/tool_command/EmitCCommand.scala, core/src/main/scala/dev/bosatsu/tool/LocalRuntime.scala, core/src/main/scala/dev/bosatsu/tool/LocalRuntimeManifest.scala, core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala, core/src/main/scala/dev/bosatsu/codegen/clang/ClangGen.scala, cli/src/test/scala/dev/bosatsu/EmitCCommandTest.scala, scripts/test_emit_c.py, and test_workspace/Prog.bosatsu. There are no upstream dependencies. The outcome handed to downstream workers is the implemented local program selector and its tested emission behavior.

Verified facts: emit-c currently requires --test-packages and supports only the local runtime schema. LocalRuntime already supplies implicit Predef, bundle membership, implementation/interface consistency checks, and native binding declarations. ClangTranspiler.validMain recognizes Bosatsu/Prog::Main, and CompilationSource selects the last matching binding. ClangGen.renderMain currently always uses the standard external resolver, whereas renderTests accepts an explicit resolver.

Extend these mechanisms. Add --main-package NAME and require an explicit root bundle for program mode. The recommended clarification is a new --main-packages FILE option paired with --main-package, mutually exclusive with --test-packages. The current manifest lists the whole reachable closure and cannot identify the selected root group by itself. Explicit bundle selection makes the required rejection of a dependency's main checkable without redesigning the runtime manifest. Update the command contract in the existing design document; maintainers can amend this spelling before execution.

Reuse main discovery and C generation, adding the resolver overload needed to apply the same reachable-native-binding checks as test emission. Check the supplied Main/Prog representation against the native runner's calling convention, following the existing ProgTest validation approach; nominal recognition alone must not authorize an incompatible native call. Share output/header handling and preserve output alias protection, staged publication, deterministic enumeration, and byte-identical output retention. Neither emission mode invokes a C compiler or installs a runtime.

Acceptance: test valid selection, absent root bundles, mains outside the selected bundle, missing or invalid main values, incompatible Main layouts, conflicting selectors, missing reachable externs, unreachable externs, headers, and failure without partial publication. Extend scripts/test_emit_c.py to compile separate Prog/Types/Api/Main groups, emit from bundles alone in a clean directory with an empty PATH, then separately link and run a program exiting 7. Reordered manifest inputs must generate identical C. Preserve all existing test-emission cases. Run focused tests, the JVM/CLI checks, and applicable codegen CI checks described in the overview. Mark 0i complete only after its acceptance checks pass.

### `toolchain_check`

- Kind: `small_job`
- Title: 0p: Verify selected compiler, core, runtime, and source components
- Depends on: `import_inspection` (`implemented`)

#### Body

Complete tracker item 0p by implementing tool verify-toolchain against the detached release manifest defined in docs/design/hermetic-bazel-rules.md.

Turn-one handoff: direct dependency import_inspection must be implemented. It supplies shared SHA-256/BLAKE3 verification and checked library identity handling. Existing inputs are core/src/main/scala/dev/bosatsu/cruntime/Command.scala, core/src/main/scala/dev/bosatsu/tool/BuildManifest.scala, core/src/main/scala/dev/bosatsu/tool_command/ToolCommand.scala, cli/src/main/scala/dev/bosatsu/FileCommandRunner.scala, build.sbt, test_workspace/Prog.bosatsu, and the compiler/core/runtime manifest section of the design. Downstream workers receive the implemented component verifier and its source-identity and compatibility rules.

Verified facts: build.sbt embeds BOSATSU_C_RUNTIME_HASH as BuildInfo.cRuntimeArchiveHash. The runtime command accepts an empty development value and rejects malformed nonempty advertised hashes. The detached manifest and verify-toolchain command do not yet exist. The design separates this release manifest from the per-program runtime graph and requires the caller to authenticate the manifest and compiler jar before launching the verifier.

Implement --manifest, --compiler-jar, --out, and the requested --core-library or repeatable --core-source inputs, plus --runtime-archive for native verification. Parse the documented versioned schema and check supplied components against that manifest's expected SHA-256, irrespective of independently valid alternative pins. Check a core binary's embedded library name/version. For source replacements, hash and parse one source snapshot, require a listed package/digest, and return reusable checked source identity facts. Preserve this same-snapshot rule when other commands use the verification logic.

Check the compressed runtime archive against both the selected manifest and any nonempty jar-advertised archive hash. Permit an unstamped development jar with an explicitly trusted manifest; reject malformed or conflicting advertised hashes. Verify only components requested by the target: jar-only use must not open core/runtime inputs, and core-only verification must not require the runtime. A request for unavailable core/native qualification must identify its missing manifest/component. Write the existing small staged success-output form only after all requested checks pass. Register the command as a safe file operation.

Keep the trust boundary explicit: this command checks supplied files against its supplied manifest; it does not authenticate its own launcher or invent a trust root. Repository resolution/CI must authenticate the manifest pin and launch the corresponding compiler. Keep release generation and publication integration in qualify_cli, after this executable verifier exists.

Acceptance: matched tuples; separately SHA-valid substituted jar/core/runtime; same-version core with different bytes; wrong embedded core name/version; listed and modified/unlisted source replacements; malformed schema/digests; empty, valid, malformed, and conflicting embedded runtime hashes; requested-component-only reads; and no published success result on failure. Assert file/digest/identity diagnostics. Cover same-path replacements in reused requests and JVM/JS compatibility of shared hashing/schema code. Update 0p after focused and shared validation passes.

### `scoped_compile`

- Kind: `small_job`
- Title: 0m: Preserve per-member imported scopes through validation and compilation
- Depends on: `import_inspection` (`implemented`), `toolchain_check` (`implemented`)

#### Body

Complete tracker item 0m. Extend structural validation and compile-packages to caller-relative library bindings, including legal disjoint private versions within one source batch.

Turn-one handoff: import_inspection supplies the implemented verified graph, canonical signatures, and owner/provider edge metadata; toolchain_check supplies the implemented selected-core and source-identity rules. Both are direct implemented dependencies. Existing inputs include core/src/main/scala/dev/bosatsu/tool/GroupGraph.scala, core/src/main/scala/dev/bosatsu/tool_command/ValidateGraphCommand.scala, core/src/main/scala/dev/bosatsu/tool_command/CompilePackagesCommand.scala, core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala, core/src/main/scala/dev/bosatsu/PackageSignatures.scala, core/src/main/scala/dev/bosatsu/PackageMap.scala, core/src/main/scala/dev/bosatsu/ProtoConverter.scala, core/src/main/scala/dev/bosatsu/cache/CompileCache.scala, proto/src/main/protobuf/bosatsu/TypedAst.proto, and cli/src/test/scala/dev/bosatsu/StrictInterfacesCommandTest.scala. Downstream workers receive the implemented scoped topology/binding formats, compilation behavior, and preserved bundle interpretation metadata.

Verified root cause: PackageSignatures.plan selects providers by package name across the whole batch; CompilerApi combines the resulting providers; PackageMap builds one externalByName map; ProtoConverter groups dependency interfaces by package name. These sites cannot represent two member-specific selections of Shared/Api. A flag or preprocessing projection alone cannot repair that loss of identity. Extend this operation family once, carrying caller context through selection, inference, serialization, and decoding instead of flattening versions or renaming public Bosatsu packages.

Add the documented --scope-map FILE to compile-packages, binding each referring member/imported scope and package to a provider scope and declared interface file. Preserve the current unscoped local path. Resolve each member's imports and selected packages' exposure closure before taking the union of used files. Track selected releases by library name with witness paths, rejecting two visible releases even when they supply different package names or byte-identical signatures. Keep implementation-only private edges outside a member's type environment. Partition inference/decoding contexts where bindings differ while retaining source-to-source scheduling and exposed source-owner support. Do not split the group into unrelated invocations that lose existing batch semantics.

Carry the bindings needed to interpret each typed member beside its implementation bundle. Record source-origin digests from the snapshots actually compiled when needed to qualify standard source replacements; keep these facts outside public signatures. Revalidate provider identity independently of signature-byte equality. Preserve the existing interface-based cache contract: selected public facts and their interpretation determine a consumer's compilation context; private implementation digests do not. Bazel worker inference caching remains disabled.

Extend GroupGraph with explicit imported scopes and real library boundaries. Preserve local membership projections, declared-node authorization, missing/surplus/cycle diagnostics, diamond deduplication, and linear structural scans. Reject loose/imported implementation collisions without treating distinct private scopes as loose duplicates. Apply the selected toolchain core identity/digest check over every verified dependency edge, including private paths; coalesce matching core references and diagnose conflicting ones with their path. Accept only checked standard-source origins at composition and reject duplicate core package providers.

Acceptance: shared diamonds; private versions in separate wrappers and independently bound members in the same batch; swapped argument/label order; scope-map missing/ambiguous/wrong-file bindings; direct and exposure-only visibility conflicts with both paths; identical-signature provider switches; source-owner exposures; and strict whole-file X/Sibling/Y behavior. Round-trip member bindings and source-origin metadata through bundles. Check cold/reused-request agreement, private-body signature stability, matched/conflicting transitive core, and N/2N chain visit counts. Extend the existing signature, graph, compile-command, codec, and cache tests. No mixed C emitter is required here. Update 0m after its tests pass.

### `mixed_emit`

- Kind: `small_job`
- Title: 0n: Emit program and test C with caller-relative imported implementations
- Depends on: `import_inspection` (`implemented`), `local_main` (`implemented`), `scoped_compile` (`implemented`), `toolchain_check` (`implemented`)

#### Body

Complete tracker item 0n by extending both emit-c modes to mixed loose-package and verified-library graphs.

Turn-one handoff consists of four direct implemented dependencies: local_main supplies program selection and local emission; import_inspection supplies verified scoped library inputs and identities; toolchain_check supplies native component/source compatibility checks; scoped_compile supplies per-member bindings, scoped validation, and bundle interpretation/source-origin metadata. Existing inputs include core/src/main/scala/dev/bosatsu/tool/LocalRuntime.scala, core/src/main/scala/dev/bosatsu/tool/LocalRuntimeManifest.scala, core/src/main/scala/dev/bosatsu/tool_command/EmitCCommand.scala, core/src/main/scala/dev/bosatsu/codegen/CompilationSource.scala, core/src/main/scala/dev/bosatsu/codegen/CompilationNamespace.scala, core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala, core/src/main/scala/dev/bosatsu/tool_command/RuntimeCommandSupport.scala, core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenLibraryDepsTest.scala, core/src/test/scala/dev/bosatsu/codegen/clang/ExternalNamespaceTest.scala, and scripts/test_emit_c.py. Downstream workers receive the implemented mixed native pipeline for both selectors.

Verified facts: LocalRuntime currently decodes all loose bundles together and rejects imported schema fields. RuntimeCommandSupport flattens library implementations into a PackageMap. DecodedLibraryWithDeps already has a CompilationSource keyed by library name/version and performs scoped inlining/tree shaking. Its non-root global identifiers carry that identity, but its depFor fallback is unsuitable as validation of an ambiguous/missing mixed binding. ExternalNamespaceTest explicitly establishes the current unversioned native external naming convention.

Extend the runtime manifest and existing CompilationSource/CompilationNamespace boundary to represent an in-memory loose root with per-member resolution contexts plus verified imported scopes. Retain name/version keys for imported code and unique loose package ownership. Decode local typed members using their saved bindings, resolve each foreign reference in its caller's context, and preserve that context through optimization, global inlining, root selection, and tree shaking. Do not manufacture a serialized source library or route through the flattening runtime helper. Reject missing/ambiguous bindings before rendering.

Preserve all local-mode checks: implicit Predef exactly once, interface/layout requirements against the correct scoped implementation, private test discovery restricted to the selected bundle, valid native Main/ProgTest layouts, output/input alias protection, declared headers, and staged output publication. Qualify used standard core providers against the selected toolchain, including recorded source origins, and reject duplicate or conflicting core implementations before native linking.

Make native binding resolution scope-aware. One unversioned C symbol can be shared only for the same verified native implementation with compatible ABI. Different implementations require explicit distinguishable bindings/symbols; diagnose an inseparable collision before linking, including equal-signature collisions. Do not enable standard Int64 intrinsics solely because a package/symbol has the standard name. Preserve custom bindings and authorize standard semantics only from checked provenance.

Acceptance: emit, separately link, and run a program and private Test/ProgTest suite using pinned library files. Link and run both private releases of a package/symbol with differing signatures and assert both results. Repeat with separately bound members in one group and reordered inputs. Test equal-signature distinct-native-implementation collisions, explicit distinct bindings, shared verified bindings, missing bindings, wrong core, duplicate implementations, stale requirements, and source-origin mismatch. A private library body change must relink while preserving an outer consumer's public signature; a visible conflict must fail before C/native linking. Keep all-source empty-library behavior and deterministic C. Run the relevant backend, CLI, native, sanitizer, and Valgrind checks. Update 0n after acceptance passes.

### `worker_args`

- Kind: `small_job`
- Title: 0j: Dispatch one-shot Bazel parameter files through the JVM file runner
- Depends on: none

#### Body

Complete tracker item 0j by implementing java -jar <candidate> tool bazel-worker @params using the existing in-process file-command engine.

Turn-one inputs already exist on main: cli/src/main/scala/dev/bosatsu/Main.scala, cli/src/main/scala/dev/bosatsu/PathModule.scala, cli/src/main/scala/dev/bosatsu/FileCommandRunner.scala, cli/src/main/scala/dev/bosatsu/StagedOutputs.scala, core/src/main/scala/dev/bosatsu/tool_command/ToolCommand.scala, cli/src/test/scala/dev/bosatsu/FileCommandRunnerTest.scala, bosatsuj, bosatsu, and scripts/test_launcher_passthrough.sh. There are no upstream dependencies. Downstream workers receive the implemented one-shot dispatcher, its operation classification, and its parameter-file contract.

Verified facts: PathModule already routes supported standalone file commands through FileCommandRunner. One runner resource owns a compiler execution pool; each invocation owns streams, its RequestPlatform, a Supervisor, and staged outputs. Its existing supports list is broader than the proposed worker surface: it includes legacy commands such as check and assemble. CommonOpts exposes an optional filesystem inference cache on legacy commands. Both JVM launcher paths forward arguments intact. No bazel-worker dispatcher currently exists.

Add the JVM entry point at the CLI routing boundary and call FileCommandRunner directly after expanding exactly one UTF-8 multiline parameter file. The first line is the operation and subsequent lines are individual arguments. Preserve spaces, Unicode, empty argument values, and LF/CRLF semantics without shell tokenization, recursive @file expansion, or subprocess dispatch. Diagnose unreadable or malformed input through the ordinary exit-status/diagnostic path.

Keep one worker-operation classification shared by the one-shot and later persistent transport. Initially register the implemented build operations headers, compile-packages, validate-graph, and emit-c. The inspection and verification children must register their operations through this same mechanism when they land. Do not expose publishing, fetching, arbitrary process execution, legacy native transpilation, or filesystem-cache options through the worker dispatcher. The ordinary interactive CLI retains its existing commands.

The enforcing structure is the worker operation classification followed by the existing FileCommandRunner request lifecycle. Every accepted worker operation must pass through that lifecycle; a new transport must not publish outputs itself. Extend the runner only where needed, preserving rollback, symlink handling, access preservation, scratch cleanup, and completion of request-owned work.

Acceptance: invoke an assembled candidate jar with parameter files containing spaces and non-ASCII paths and compare output bytes, diagnostics, and status with direct invocation. Cover line endings, empty arguments, invalid operations/options, missing files, and a failed write followed by a successful request. Check launcher passthrough of the literal @params argument. Test rejection of online/process-executing operations and cache options. Do not implement the persistent protocol in this child. Run focused tests, ./test_cli.sh, and the shared validation commands. Update the existing design tracker for 0j when complete.

### `worker_json`

- Kind: `small_job`
- Title: 0k: Add the singleplex persistent JSON worker
- Depends on: `worker_args` (`implemented`)

#### Body

Complete tracker item 0k by adding --persistent_worker to the one-shot worker entry point.

Turn-one handoff: direct dependency worker_args must be implemented. It supplies the merged parameter-file dispatcher, shared worker-operation classification, and FileCommandRunner integration. Existing inputs include cli/src/main/scala/dev/bosatsu/FileCommandRunner.scala, cli/src/main/scala/dev/bosatsu/IOPlatformIO.scala, cli/src/main/scala/dev/bosatsu/StagedOutputs.scala, core/src/main/scala/dev/bosatsu/Json.scala, and the Persistent JVM worker section of docs/design/hermetic-bazel-rules.md. Downstream workers receive the implemented transport and its tested request/response and cleanup behavior.

Use UTF-8 newline-delimited JSON with protobuf JSON field names/defaults, unknown-field tolerance, opaque base64 input digests, and request ID echoing. Process requests serially using one runner resource. Dispatch the received arguments directly; Bazel has already expanded the action's parameter file. Capture action diagnostics in output, reserve stdout for one response per ordinary request, and flush each complete response immediately. Clean EOF succeeds. An invalid command in a valid envelope is a request failure and the next request remains usable. Malformed/truncated transport input without a trustworthy request is a process failure reported on stderr. Reject nonempty sandboxDir under the v1 layout. Do not advertise cancellation or multiplexing; handle unsupported control messages according to the documented singleplex policy without inventing successful cancellation responses.

Reuse the JIT, immutable Predef/compiler resources, and bounded execution service. Keep decoded files, errors, pending outputs, and scratch state request-local. Do not add a cross-request content cache. A missing/empty digest therefore causes fresh reading and cannot establish reuse. Keep filesystem inference caching unavailable through this entry point.

Preserve the declared-input contract for worker reads. Apply request input authorization at the JVM file-read boundary used by the registered operation family. Authoring inspection found separate IOPlatformIO readUtf8, readBytes, read, and readHashed implementations; overriding only readBytes would leave binary reads outside enforcement. Inventory and cover those existing read paths, including manifest-listed bundles and dependency libraries, while allowing output publication and its filesystem identity checks. This boundary must not silently read an undeclared input left by another request. Avoid a separate compiler or generalized filesystem subsystem.

A response is sent only after the runner has closed/published successful outputs or completed failure cleanup and joined request-owned work. Preserve byte-identical output behavior and recovery after failed publication.

Acceptance: drive the assembled jar over real stdin/stdout with good/bad/good requests, changed bytes at the same paths, missing digests, omitted IDs, unknown fields, Unicode/spaces, multiline diagnostics, and process restart. Compare statuses, diagnostics, and artifacts with one-shot execution. Cover invalid JSON/types/base64, truncated input, clean EOF, unexpected sandboxDir, unsupported controls, undeclared text and binary inputs, publication failures, scratch cleanup, and absence of stray stdout. Repeat the existing strict-interface X/Sibling/Y matrix over the transport. Use focused JVM tests and subprocess tests, then shared CLI validation. Update 0k only when complete.

### `qualify_cli`

- Kind: `small_job`
- Title: 0q: Qualify the candidate jar and generate the distributable toolchain manifest
- Depends on: `import_inspection` (`implemented`), `local_main` (`implemented`), `mixed_emit` (`implemented`), `scoped_compile` (`implemented`), `toolchain_check` (`implemented`), `worker_args` (`implemented`), `worker_json` (`implemented`)

#### Body

Complete tracker item 0q and the Stage 0 CLI acceptance boundary. Integrate reproducible qualification and detached manifest generation into existing CI/release preparation after the executable foundations have landed.

Turn-one handoff requires every listed dependency directly: local_main supplies local program behavior; worker_args and worker_json supply both actual worker entry points; import_inspection supplies file-only authenticated import inspection; toolchain_check supplies component verification; scoped_compile supplies member bindings and core/source composition rules; mixed_emit supplies both mixed native modes. Each dependency must be implemented. Existing inputs include .github/workflows/ci.yml, .github/workflows/release.yml, scripts/test_emit_c.py, scripts/test_c_sanitizers.sh, scripts/test_c_valgrind.sh, scripts/c_runtime_ci_env.py, scripts/publish_bosatsu_libs.sh, scripts/test_launcher_passthrough.sh, test_cli.sh, bosatsuj, bosatsu_libs.json, test_workspace/core_alpha_conf.json, c_runtime/deps.json, and build.sbt. This node's output is the implemented qualification/release-preparation state; it does not require a real release or credentials to complete.

Verified facts: release.yml already archives c_runtime once in prepare and embeds its BLAKE3 in build_cli. The release job separately rebuilds an assembly before scripts/publish_bosatsu_libs.sh generates core artifacts; that script invokes bosatsuj, which selects a local assembly by modification time. The workflow stages jar/runtime/library assets and writes checksum lists, but no detached tested-tuple manifest. CI has Linux/macOS native paths, a file-emission fixture, sanitizer/Valgrind scripts, and library-publish dry-run setup. Several low-level native tests still invoke tool transpile.

Make the exact candidate deploy jar an explicit input to core construction and qualification rather than relying on a later assembly or newest-file selection. Generate the documented versioned manifest only after the final jar, core library, and compressed runtime archive exist. Derive SHA-256 and embedded core identity from those bytes, and core_sources entries from the matching compiled source snapshots. Preserve the existing single-archive embedded-hash ordering. Verify the generated manifest against the staged files and record its SHA-256. Integrate staging/checksum inclusion into the existing release workflow after qualification; running this child must not publish a release or require publishing authority.

Exercise the assembled jar through standalone file commands, one-shot @params, and persistent JSON requests from clean directories. Add an external test workspace with minimal Bazel actions invoking these interfaces and the design's Bazel 9.2.0 pin. This is a transport/declared-input qualification harness, not implementation of the Stage 1–6 product rules or Gazelle. Provision tools/dependencies before offline execution. Compare sandboxed one-shot and worker strategies in separate output bases or with fresh inputs so both execute; inspect execution records, output digests, diagnostics, and results. Test empty home/read-only home, empty PATH during emission, request-local JNA extraction, same-path edits, strict interfaces, restarts, and offline reads after setup.

Against the exact staged tuple, separately compile/link and run a program using Bosatsu/Prog that exits 7 and a private ProgTest calling a core external. Exercise binary-core and manifest-matched source replacement, an empty imported graph, mixed imports, and the two-private-release fixture. Verify wrong components/core origins fail before linking. The qualification driver and CI produce the evidence for that exact revision and environment; no external provider or universal-future guarantee is claimed.

Recommended cutover is additive compatibility: retain interactive check/transpile and existing library workflows. Move the low-level C fixture callers being replaced in ci.yml, test_c_sanitizers.sh, and test_c_valgrind.sh to compile-packages plus emit-c and an explicit native compile/link driver, preserving each existing regression and instrumentation assertion. Retain tests whose purpose is legacy command compatibility. Update test_cli.sh and launcher coverage for the new CLI. Do not introduce producer commands or retire unrelated APIs.

Acceptance: manifest round-trip and substituted-component negatives; compiler-only handoff of jar plus SHA with core/runtime absent; full handoff of manifest, its SHA, and three local artifacts; actual jar/worker/Bazel parity; Linux and macOS native qualification; sanitizer/Valgrind regression preservation; full applicable CI and changed-behavior coverage review. Update the existing design's contracts and tracker so 0i–0q are accurately complete with implementation provenance. Stages 1–6 and optional producer/projection work remain explicitly pending.
