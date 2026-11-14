# Code Plan #2493

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2493` 0l and 0o: Inspect pinned library files and authenticate every dependency edge
- Pending steps: `2`
- Completed steps: `4`
- Total steps: `6`

## Summary

Implement `tool inspect-library` and `--descriptors-only` over caller-declared, pinned files. Authenticate roots and every dependency descriptor before decoder deduplication, preserve library scopes and referring edges, and produce independently requested canonical signatures and inspection metadata through the existing file-command lifecycle. Deliver reusable serialized graph fixtures and Linux candidate-jar coverage, then mark tracker items 0l and 0o complete together.

The enforcing boundary is a privately constructed authenticated snapshot/graph consumed by inspection and its reusable scoped decoder. Extend `DecodedLibrary`, `DecodedLibraryWithDeps`, `PackageSignatures`, `BuildManifest`, and `FileCommandRunner` rather than introducing another compiler, downloader, or publication system. The required production, fixture, and test family may exceed the 1,000-line guideline; planner judgment permits that necessary work because edge authentication, scoped decoding, resolver discovery, and their acceptance tests form one inseparable outcome. Avoid a compiler-wide migration.

Plan trajectory: initial steps 5; review revisions 0; review-origin steps 0.

## Current State

At initial authoring, HEAD is `86f8b7db7273062d0b43881080bf1806499dc2c1`. Roadmap #2482 and its `inspect_imports` node agree with issue #2493. The direct dependency, toolchain verification from #2486/#2490, is implemented: portable SHA-256 and BLAKE3 are available through `Algo`, and `BuildManifest` provides versioned JSON parsing with UTF-8 and duplicate-field diagnostics.

`PlatformIO` already exposes portable byte reads and file identity operations. `DecodedLibrary.decodeWithDeps` caches by name/version and skips its dependency loader on cache hits. `DecodedLibraryWithDeps.decodeAll` performs additional loader-driven decoding and also caches dependencies. Consequently, wrapping those loaders cannot authenticate every incoming descriptor. `ProtoConverter.packagesFromProto` can decode a library against supplied dependency interfaces, and the existing C-backend fixture demonstrates decoding separate private versions in their own scopes. `PackageSignatures.encode` supplies the canonical public signature format and typed interfaces expose their package dependencies.

`ToolCommand`, `FileCommandRunner`, and `BazelWorker.Operation` provide shared command dispatch; the JVM runner publishes complete output sets through `StagedOutputs`. There is no inspect-library command or versioned inspection/dependency-context contract. The existing design still marks 0l and 0o incomplete. The released core pinned by `test_workspace/core_alpha_conf.json` supplies real legacy compatibility evidence, but its lack of dependency descriptors means constructed serialized graphs are also required.

## Problem

Bazel's repository-host resolver and later compiler actions need a file-only way to authenticate imported libraries and inspect their descriptors, public interfaces, ownership, and implementations. The current decoder can reuse a dependency identity without checking a second referring descriptor, allowing a bad diamond edge to escape loader-based verification. Flattening imported package providers also loses the distinction between legal private releases.

Descriptor discovery must authenticate a pinned file before exposing its children, without requiring those children to exist already. Full inspection must require the complete declared dependency context, verify every incoming edge against the bytes actually parsed, reject conflicting bytes for one identity, and preserve separate version scopes. Public projections must remain independent of full implementation digests so implementation-only edits can preserve consumer signature inputs.

## Technical Debt Notes

The existing signature planner and local runtime remain unscoped across imported providers. Their migration belongs to roadmap items 0m and 0n, which consume this issue's authenticated graph and ownership contracts; it must not be folded into inspection. Expect those downstream compilation/selection/native-binding changes and tests to exceed 1,000 lines cumulatively and require their own planner judgment. Existing interactive CAS/history/evaluation loader APIs also remain outside this declared-file inspection boundary; a separate follow-up could migrate that operation family to authenticated snapshots, estimated at 400–800 lines including compatibility tests. Neither exclusion permits an unchecked path within full inspection or its reusable verified-graph decoder.

## Steps

1. [x] `step_1` Add the shared pinned-file boundary and reusable serialized fixtures

Add an authenticated snapshot abstraction in `core/src/main/scala/dev/bosatsu/library/VerifiedLibraryGraph.scala`, with construction restricted to its verification functions. Accept a declared path, expected library name, exact version, and nonempty external hash list. Read the file once through `PlatformIO.readBytes`, compute supported digests using `Algo`, verify the supplied pins, and parse `proto.Library` from that same byte array. Check the embedded name/version before returning a snapshot. Keep arrays method-local or defensively encapsulated; expose immutable identity, digests, parsed protobuf, and path provenance. Embedded self-descriptor metadata is not an authentication source.

Use the same verifier for roots and dependency edges. Require at least one supported pin; check every recorded supported pin rather than accepting the first match. Reject malformed supported hashes and conflicting pins, including one valid algorithm accompanied by an invalid digest for another supported algorithm. Preserve unsupported hash entries in descriptor output, but never count them as authentication. Keep BLAKE3-only authentication valid. Include the path and expected/actual facts in failures. Reuse `Algo.parseIdent` and existing descriptor JSON conventions rather than adding another digest implementation.

Add a version-1 dependency-context reader in `core/src/main/scala/dev/bosatsu/tool/LibraryInspection.scala` using `BuildManifest`. Its shape is `{"schema_version":1,"libraries":[{"path":"deps/shared.bosatsu_lib","library_name":"shared","version":"1.0.0"}]}`. Paths use the normal platform argument parser and request working directory. Entries declare candidate files for an identity; authentication comes from incoming descriptors. Preserve multiple paths declaring one identity so graph verification can distinguish aliases from conflicting bytes. Validate schema, fields, identity syntax, and repeated JSON keys without fetching files or interpreting URIs.

Create `core/src/test/scala/dev/bosatsu/library/LibraryInspectionFixtures.scala` with portable typed-library builders based on `ClangGenLibraryDepsTest`: serialize real compiled packages and hash the resulting bytes. Cover a Parser root with public, private, and legacy unused-transitive-public children, a diamond, and wrappers selecting two incompatible versions of Shared/Api. Materialize the small reusable serialized fixture set and pins under committed `test_workspace/inspect_library/`, with a README recording the source snippets and regeneration command. A small JVM test utility under `cli/src/test/scala/dev/bosatsu/` may call the shared builders to regenerate these files. These files remain in the PR and are consumed by later candidate-jar tests.

#### Invariants

- Only externally pinned bytes with matching embedded identity can become an authenticated snapshot; descriptor-only and full inspection share this constructor.
- Hashing and protobuf parsing consume one byte snapshot, with no second filesystem read between verification and parsing.
- All supported expected digests must agree with the snapshot; an unsupported-only or empty pin set cannot authenticate it.
- Public APIs remain immutable, and generic paths use the platform's ordering and rendering contracts.

#### Property Tests

- For generated valid serialized libraries, correct SHA-256, BLAKE3, or combined pins authenticate the same identity; changing bytes without changing the pins fails.
- Permuting or repeating equal supported pins preserves the result; adding any conflicting supported pin causes failure.
- Exercise the verifier/context reader with paths whose JVM equals, hashCode, and toString throw, extending ParametricPathCommandsTest or its existing adapter.

#### Assertion Tests

- Test wrong name/version, missing version, malformed protobuf, missing file, empty pins, unsupported-only pins, malformed supported hashes, and mixed-algorithm conflicts.
- Use an instrumented platform that would return different bytes on a second read to prove snapshot verification and parsing use one read.
- Test version-1 context parsing, invalid UTF-8, duplicate fields, unsupported schema versions, invalid paths/identities, and preserved equal-identity aliases.

#### Completion Notes

Added `VerifiedLibrarySnapshot.verify` in `VerifiedLibraryGraph.scala`. It reads once, hashes and parses the same private byte snapshot, validates every supported external pin, and checks the embedded name and exact version. The focused JVM suite passed (5 tests), and `coreJS/compile` passed.

2. [x] `step_1_complete_context_and_fixtures` Complete the dependency context reader and reusable serialized fixtures

Continuation of parent `step_1` in the shared pinned-file boundary and reusable serialized fixture family. Complete the remaining step_1 deliverables by adding a version-1 declared dependency-context reader in `core/src/main/scala/dev/bosatsu/tool/LibraryInspection.scala` using `BuildManifest`. Validate schema, fields, identity syntax, repeated JSON keys, and paths through the platform argument parser; preserve multiple declared paths for one identity and perform no file acquisition.

Add typed-library fixture builders in `core/src/test/scala/dev/bosatsu/library/LibraryInspectionFixtures.scala`, based on `ClangGenLibraryDepsTest`, and commit their serialized bytes and pins under `test_workspace/inspect_library/` with a README containing source snippets and a regeneration command. Cover the Parser closure with public, private, and legacy unused-transitive-public children, a diamond, and wrappers selecting two incompatible Shared/Api versions. These fixtures remain in the PR for step_5 candidate-jar tests.

This serves the accepted 0l/0o workflow: descriptor-only inspection must expose authenticated descriptors for resolver discovery, while full inspection requires an explicit complete dependency context. Step_2 consumes this context contract and the fixtures; keeping them in a separate upstream step gives later graph, projection, and candidate-jar work stable inputs instead of making each step invent them. Add focused context and fixture tests, including an instrumented platform assertion that verification reads a pinned file once.

#### Invariants

- Dependency-context paths are parsed by the platform and never fetched or interpreted as URIs.
- Multiple declared paths for one identity remain available for later alias and conflicting-byte checks.
- Committed serialized fixtures remain reusable by the later candidate-jar step.

#### Property Tests

- Permuting context entries preserves the decoded declarations, including all aliases for an identity.
- Valid serialized fixture bytes authenticate against their recorded supported pins; changing bytes without changing pins fails.

#### Assertion Tests

- Reject unsupported schema versions, unknown or repeated fields, invalid identities, invalid paths, and malformed JSON while preserving useful diagnostics.
- Verify the committed Parser closure, diamond, and both private versions from their serialized bytes and pins.
- Use an instrumented platform to assert the pinned-file verifier reads once and parses the authenticated snapshot.

#### Completion Notes

Added the version-1 dependency context reader with strict field, identity, repeated-key, and platform path validation while preserving aliases; documented its JSON contract. Added a typed serialized Parser graph with public/private and legacy unused-transitive-public edges, a diamond, and wrappers selecting two private Shared versions. Committed pinned artifacts and a regeneration README. JVM focused suites passed 29 tests; Scala.js focused suites passed 12 tests, and Scala.js test compilation passed after manifest validation was consolidated in BuildManifest. Coverage includes pin tampering and an instrumented single-read assertion.

3. [x] `step_2` Verify every graph edge before scoped decoding

Extend `VerifiedLibraryGraph.scala` to build an authenticated graph from a verified root and the explicit dependency-file context. Inventory the complete edge family as `publicDependencies`, `privateDependencies`, and `unusedTransitivePublicDependencies`; inspect all three on every reachable authenticated artifact. Each retained edge records the referring identity, visibility, target identity, original expected hashes/mirror URIs, selected file provenance, and a deterministic witness path.

For every incoming descriptor, locate its declared candidate files and compare its pins with their snapshots before consulting the decoded-node cache. A previously authenticated target still needs the new descriptor checked. Compare all declared aliases for a reachable identity: coalesce equal bytes and reject different bytes claiming the same name/version, reporting both files and referring paths. Keep exact versions as separate keys. Reject missing children, missing descriptor versions/pins, identity mismatches, and cycles with useful paths. Read each selected path once per request and use an explicit worklist or existing stack-safe graph facilities. Context entries do not authorize filesystem discovery or URI fetching.

Add a graph-consuming decoding entry point beside `DecodedLibrary`/`DecodedLibraryWithDeps`. Decode authenticated nodes in dependency order using `DecodedLibrary.decode`/`ProtoConverter.packagesFromProto` with that node's own dependency interfaces, then assemble `DecodedLibraryWithDeps` values from those already decoded nodes. Preserve legacy resolution dependencies needed for decoding, while retaining their distinct visibility in graph metadata. Coalesce a repeated provider identity before supplying interfaces; never flatten interfaces from unrelated private scopes into one global environment. Report missing or ambiguous providers in the referring scope.

The complete in-scope decoding inventory is full inspection, every full-mode projection, and the reusable graph-to-decoded-library entry point handed to downstream mixed decoding. These paths must take the verified graph and must not call either loader-based `decodeWithDeps` or loader-based `decodeAll`. Preserve `LibConfig.validateReservedPackages` and the existing ProtoConverter Int64 validation. Existing interactive CAS/history/evaluation callers retain their current APIs; migrating that separate operation family is outside this issue.

#### Invariants

- Every reachable public, private, and legacy resolution descriptor is checked before identity deduplication can suppress work.
- A graph can contain one byte value per exact library identity and multiple versions of a library in separate scopes.
- Full inspection cannot bypass authentication through an unchecked loader-based decoder.
- Typed decoding sees each library's own dependency context and preserves existing reserved-package and Int64 contracts.

#### Property Tests

- Generate small pinned DAGs and compare the verified edge set with all three descriptor lists from reachable nodes; permuting context and descriptor enumeration preserves canonical graph output.
- For any incoming edge in a generated DAG, replacing its expected digest causes rejection even when another edge has already authenticated its target.
- Adding equal-byte aliases preserves graph semantics; adding different bytes under a reachable existing identity always fails.

#### Assertion Tests

- Add the ordered diamond regression: authenticate the shared child through the first branch, then reject the second branch's bad digest and identify that branch.
- Test missing dependencies in each visibility class, equal-identity aliases, conflicting-byte aliases, cycles, and a supported-hash conflict on a reused identity.
- Decode the two-private-version fixture and assert each wrapper binds Shared/Api to its selected release; reject an ambiguous provider inside one referring scope.
- Retain focused reserved Bosatsu namespace and malformed Int64 regressions through the new graph decoder.

#### Completion Notes

Implemented authenticated traversal of public, private, and legacy resolution edges, including per-path snapshot reuse, alias checks, and cycle detection. Added graph-only dependency-order decoding with per-library provider scopes. Parser fixture tests cover the second diamond edge, private versions, aliases, missing and ambiguous providers, descriptor pins and versions, cycles, reserved packages, and malformed Int64 data. The focused JVM suites passed (32 tests), and `coreJS/compile` passed.

4. [x] `step_3` Derive canonical signatures and scoped inspection projections

Implement inspection projections in `core/src/main/scala/dev/bosatsu/tool/LibraryInspection.scala`, consuming the authenticated snapshot for descriptor-only results and the verified graph/scoped decoder for full results. Reuse `PackageSignatures.encode` for the root's exported signature; retain its existing protobuf format and canonicalization. Do not encode artifact hashes, file paths, mirror URIs, implementation metadata, or unrelated private scopes in signature bytes.

Define small version-1 JSON outputs with explicit field names and deterministic ordering. The descriptor projection contains the inspected identity, computed verified SHA-256/BLAKE3, embedded descriptor metadata, and dependency entries carrying target identity, visibility, expected hashes, and URIs. The exposure projection contains records keyed by referring library identity and exported package, with the package's owner and each exposed package's provider identity and witness path. The membership projection lists each graph scope's internal and exported package names. The implementation projection identifies the root and lists authenticated artifact references, their full digests/declared paths, and scoped dependency edges; it references original implementation artifacts rather than creating a second implementation codec. Use name plus exact version as the structured scope identity.

Derive exposure edges from decoded typed exports, including interfaces without `signatureDependencies`; do not treat a missing serialized exposure index as an empty exposure set. Resolve providers relative to the referring library through its authenticated dependency/public exposure relationships, including legacy resolution records as appropriate. Keep private implementation dependencies in the implementation graph without turning their unrelated packages into public exposure dependencies. Represent the compiler-provided implicit Predef explicitly when necessary instead of inventing a downloaded provider. Diagnose missing or ambiguous ownership with referring paths. Preserve existing decoder validation of serialized indexes and definitions.

Allow each projection to be requested independently. Full-mode signature-only inspection still authenticates its required declared graph, but produces no unrequested files. Descriptor-only projection must not invoke typed decoding or attempt to load child files. Document the concrete input/output examples and ordering rules in `docs/design/hermetic-bazel-rules.md` alongside the existing command contracts; retain this documentation for downstream nodes.

#### Invariants

- Exposure metadata reflects typed exports and caller-relative provider identities, including legacy unindexed interfaces.
- Public signature bytes contain public interface facts and remain separate from full artifact identity metadata.
- Private versions retain distinct scoped ownership even when package names or signature bytes coincide.
- Descriptor-only inspection exposes children only after authenticating their parent and does not require the children.

#### Property Tests

- Generate implementation-only edits with unchanged exported interfaces and assert identical signature bytes while artifact digests and implementation references change.
- Reorder equivalent exported declarations, graph inputs, and context entries and assert byte-identical canonical projections.
- For every emitted exposure edge, assert that its provider owns the referenced package in the referring scope's verified resolution context.

#### Assertion Tests

- Test an unindexed typed value/type exposure and a re-export, proving provider ownership is recovered from typed data.
- Check that two private releases appear with their distinct referring edges, while a wrapper's private dependency does not leak into an outer public projection.
- Test each projection alone, empty exported-interface sets, and signatures that round-trip through existing signature readers.
- Inspect a valid pinned parent with all children absent in descriptor-only mode; require full mode to reject that same incomplete context.

#### Completion Notes

Implemented canonical signature, descriptor, typed exposure/provider, membership, and scoped implementation projections over the authenticated snapshot and graph. Added deterministic and legacy unindexed-exposure fixtures, private implementation stability coverage, and version-1 output documentation. The focused JVM inspection/signature suites passed (34 tests); `coreJS/test:compile` and `git diff --check` passed.

5. [ ] `step_4` Register the inspection command with staged publication

Add `core/src/main/scala/dev/bosatsu/tool_command/InspectLibraryCommand.scala`. Require `--input FILE`, `--expected-name NAME`, `--expected-version VERSION`, and repeated `--expected-hash ALGO:HEX`. Full mode requires `--dependency-context FILE`, using an empty libraries array for a leaf, and accepts independently optional `--sig-out`, `--exposures-out`, `--membership-out`, `--implementation-out`, and `--descriptor-out`; require at least one output. `--descriptors-only` requires only the pinned root and `--descriptor-out`, and rejects full-only outputs/context instead of opening child files. Resolve all paths with the existing platform parser and request directory.

Finish authentication, decoding, projection construction, and serialization before reporting outputs. Reuse `Output.Many`, existing interface-bundle reporting, and JSON/document reporting; add only the small output variant needed if the existing interface reporting cannot write already encoded canonical signatures directly. Register the command in `ToolCommand`, `FileCommandRunner.supports`, and `BazelWorker.Operation.registered`. Both one-shot and persistent dispatch must use this same operation.

Protect inputs and distinct requested outputs using the existing file-identity checks. Extract the generic output/input alias checks currently private to `EmitCCommand` into a small shared helper used by EmitC and inspection, preserving its behavior instead of copying the algorithm. Include the root, dependency-context manifest, and selected dependency files in inspection's protected input set. Reject duplicate/aliased outputs and outputs aliasing inputs, including filesystem aliases already handled by the existing helper.

Return all requested outputs to the existing JVM `FileCommandRunner`/`StagedOutputs` transaction. Ordinary verification, decoding, serialization, staging, and publication failures must preserve previous outputs and leave absent outputs absent. Add JVM command tests using `FileCommandFixtures`, shared in-memory command tests where appropriate, and a focused Scala.js filesystem command test in `cliJS/src/test/scala/dev/bosatsu/Fs2PlatformIOTest.scala`. Do not add another transaction manager, network path, or request cache.

#### Invariants

- Direct, parameter-file, and persistent worker execution use the same inspection operation and current request files.
- Inspection reads only declared files and neither fetches URIs nor updates lockfiles.
- All JVM outputs publish as one completed set through StagedOutputs, with existing rollback and cleanup guarantees.
- Requested outputs cannot overwrite declared inputs or alias one another; generic path handling remains parametric.

#### Property Tests

- For each supported output subset, compare emitted bytes with the corresponding files from an all-projections invocation.
- Run equivalent direct and worker invocations against reordered contexts and assert equivalent status, diagnostics, and requested output bytes.

#### Assertion Tests

- Test command help/registration, required pins, invalid mode combinations, no-output requests, and malformed context diagnostics.
- Seed every requested output with sentinels; test authentication failure, incomplete context, typed decoding failure, and a blocked later output destination, preserving all previous outputs and cleaning staging files.
- Cover duplicate outputs and input/output aliases using existing lexical, symlink, hardlink, and pending-path test patterns; rerun the existing EmitC alias tests after helper extraction.
- Test good/bad/good requests and changed bytes at the same path without restarting the runner.
- Exercise descriptor and full inspection through Scala.js file IO with successful output and tamper rejection.

6. [ ] `step_5` Qualify resolver traversal and the candidate jar, then complete both trackers

Extend `scripts/test_bazel_worker.py` with candidate-jar inspection cases using the committed fixtures in `test_workspace/inspect_library/`. Add an explicit prepared-fixture directory argument and a small preparation script, `scripts/prepare_inspect_library_fixtures.py`, that stages those fixtures and acquires the real legacy core before offline acceptance begins. Retain the release URI and immutable pins in the setup: core_alpha 9.0.1 from v0.0.69, 937080 bytes, SHA-256 `b6e7d87fcb786084dd90da490fe9514678fee7f6920fc17e0abdf28210a11dff`, and BLAKE3 `098f5eff00611599b54b5e6bce8b7696c8fdf89f4e99f8bfbbec521764ed8927`. Verify the acquisition SHA-256 before staging it; assert its BLAKE3 through inspection. The preparation script may acquire files, but command tests must run entirely from prepared local inputs and must fail clearly rather than skip required legacy evidence when setup is absent.

Drive the resolver-facing workflow file by file: inspect a pinned Parser root in descriptor-only mode; for each returned descriptor, provide only that selected child and its authenticated parent-specified identity/hashes; accept the child's descriptors and derived SHA-256 only after successful inspection. Exercise a BLAKE3-only child, tampering before descriptor exposure, every closure file, the diamond's second bad edge, aliases, conflicting bytes, and both private versions with referring edges. Then supply the complete context for full projections. Test the real legacy core's nonempty exported signatures and typed exposure projection separately; its empty dependency lists are not traversal evidence.

Compare direct, `@params`, and persistent requests for output bytes, exit status, and diagnostic content. Include signature-only invocation, deterministic input reordering, failed inspection preserving all outputs, and good/bad/good persistent recovery after changing a dependency at a reused path. Use the existing harness's snapshot and scratch checks, and run actions without acquisition tools or network access.

Run the issue's Linux checks: `sbt "coreJVM/test; cli/test; coreJS/test; cliJSJS/test; cli/assembly"`, the extended `python3 scripts/test_bazel_worker.py --jar <candidate-assembly> --inspection-fixtures <prepared-directory>`, and `./test_cli.sh` as required by `coding_style.md` for CLI changes. Use focused suites during earlier steps and target complete changed-behavior coverage. MergeXO independently owns the configured `scripts/test_basic.sh` gate; do not add a step that runs or waits for it. macOS validation remains with PR CI.

Finish `docs/design/hermetic-bazel-rules.md` with runnable descriptor-discovery/full-inspection examples, the version-1 schemas, hash policy, scope/ownership rules, and fixture preparation instructions. Mark 0l and 0o complete together with the implementation reference. Keep the committed fixtures and their README as the downstream handoff; no step requires this candidate's future merge.

#### Invariants

- Resolver traversal trusts a child's descriptors and newly derived SHA-256 only after verifying that child against its authenticated parent's descriptor.
- Real legacy compatibility and constructed dependency traversal are both exercised; neither substitutes for the other.
- Candidate-jar checks execute the branch's implementation entirely from prepared files and preserve worker failure recovery.
- Completion delivers implemented authentication, projection contracts, and reusable fixtures before PR publication.

#### Property Tests

- Repeated resolver traversal with equivalent file/context ordering produces identical authenticated repository inputs, scoped edges, and package indexes.
- The candidate jar's direct, one-shot, and persistent projections match for the same pinned fixture graph.

#### Assertion Tests

- Verify the released core's identity, both known digests, canonical signature projection, typed exposure metadata, and empty dependency descriptors.
- Enumerate the constructed Parser closure file by file, authenticate the BLAKE3-only child before recording its SHA-256, and reject a tampered child before publishing descriptors.
- Run candidate-jar diamond, private-version, alias/conflict, incomplete-context, output-preservation, and reused-path recovery regressions.
- Complete the named JVM, Scala.js, CLI, assembly, and candidate-worker checks without requiring macOS evidence.
