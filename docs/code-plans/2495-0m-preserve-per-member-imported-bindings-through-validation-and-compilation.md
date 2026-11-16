# Code Plan #2495

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2495` 0m: Preserve per-member imported bindings through validation and compilation
- Pending steps: `5`
- Completed steps: `1`
- Total steps: `6`

## Summary

Preserve caller-relative provider bindings from graph validation through interface selection, source inference, compiled requirements and typed-bundle decoding. A member may see only one exact release of each library name; separate members may select different releases. Authenticate the complete implementation graph and selected core separately from the public interfaces used for compilation.

Extend the existing VerifiedLibraryGraph, LibraryInspection, GroupGraph, PackageSignatures, CompilerApi, PackageMap source DAG, ProtoConverter and staged file-command lifecycle. Keep nominal types unchanged and supply each member with its own checked environment. The bounded migration includes their command, serialization and file-reader call sites, focused tests and the candidate-worker harness. Estimated cumulative churn is 2,000–3,500 lines, exceeding the 1,000-line guideline because graph identity, inference context and persisted decoding context must agree to deliver the requested behavior safely. Retaining the existing algorithms and codecs is the chosen simplification; independent replacements would increase the migration.

Mixed native emission, runtime-manifest expansion, Bazel/Gazelle implementation, producer/history features and the optional select-interfaces command remain outside this job. This change supplies the scoped decoding contract needed by tracker 0n.

Plan trajectory: initial steps 6; review revisions 0; review-origin steps 0.

## Current State

Initial planning inspected main at 620bbfd3ed0da74435ee5a083b534c6e8ec9fd06. The direct dependencies are implemented: inspection authenticates snapshots and every descriptor edge, exposes scoped ownership and provides serialized graph fixtures; toolchain verification defines the selected core identity/digest contract; the persistent worker supplies request-local execution and candidate-jar parity checks. The issue, root #2482, immediate roadmap node and retained human context are aligned. Retained human history has disclosed coverage gaps and supplies no scope amendment for this issue.

GroupGraph currently validates loose local groups with a stack-safe traversal, membership-only local checks and diamond deduplication. PackageSignatures selects reachable roots of wide signatures and stages source-supplied foreign owners, but its providers, origins and traversal state use unqualified package names. CompilerApi.compileWithSignatures carries one provider map across stages. PackageMap already infers sources through a memoized dependency DAG and builds per-package exposure closures, but resolves external interfaces through batch-wide name maps. CompilePackagesCommand supplies no filesystem inference cache. Output.PackageGroup writes package-wide InterfaceRequirement entries without referring-member or provider identity. ProtoConverter and LocalRuntime decode through unqualified package maps; file readers can currently discard bundle-level requirements. No select-interfaces command exists.

## Problem

Tracker 0m requires local groups and imported libraries to retain the provider selected relative to each referring member or imported scope. The current batch-wide package lookup cannot represent incompatible private releases with the same nominal package/type names, and package-only conflict checks miss two releases exporting different names or identical signatures. Unscoped requirements also lose the context needed to decode a compiled member against its original providers.

The change must resolve only each member's imports and selected API exposures, preserve source dependencies and source-supplied owners, reject visible release conflicts with both witness paths, and check every reachable descriptor and core reference without admitting private implementation dependencies into type visibility or ordinary consumer signature inputs. These guarantees must hold after serialization and across warm, cold and restarted worker execution.

## Steps

1. [x] `step_1` Define the shared scoped binding and public-index contract

Add one immutable internal representation, in core/src/main/scala/dev/bosatsu/PackageBindings.scala, for loose/source members, exact imported library identities, scoped package owners and checked bindings. A binding key identifies a referring local member or imported scope plus a required package; its target identifies the provider scope, package and original declared interface file. Keep file paths, labels and witness routes as provenance rather than nominal type identity. Use existing Name/Version identity semantics, including exact version identity, and explicit Order/Eq/Show instances where needed.

Refactor LibraryInspection's existing exposure/provider calculation to return typed public-index records before rendering its existing JSON projections. Reuse those records in binding validation; preserve the existing inspection JSON contract and independently requestable projections. Do not create a second provider-search algorithm.

Implement the versioned --scope-map JSON reader using BuildManifest. Its contract contains local member identities and dependency edges, imported scope identities with exported-package/exposure ownership from inspection, and requested bindings with referring identity, required package, provider identity and interface path. Public projection references and paths are explicit declared inputs; interface targets must belong to the --direct-interface list. Derive witness routes from the supplied topology rather than trusting caller-written routes. Reject duplicate binding keys, unknown members/scopes, invalid package/version identities, undeclared target files and inconsistent owner declarations. Validate the imported records against inspection's public index and the real dependency topology; a scope-map claim is not an artifact-authentication capability. Full artifact authentication belongs to step_2, and compilation consumes the resulting declared public facts without reopening implementation files.

Provide an explicit legacy/unscoped mode for existing callers. Do not infer imported identity from interface bytes or diagnostic labels. This representation is the common input for the subsequent graph, selection, inference and codec changes.

#### Invariants

- A checked binding has exactly one referring identity and one selected owner; equal signature bytes do not merge different library identities.
- Imported ownership and exposure edges use the existing inspection provider-resolution mechanism and real dependency edges.
- Public binding inputs exclude full implementation digests, private implementation contents and unrelated exported roots.
- Legacy local calls remain representable without a scope map.

#### Property Tests

- Generate bounded binding maps and permute declaration order: parsing and canonicalization preserve the same checked bindings and deterministic diagnostics.
- Round-trip scope-map public identities and bindings, including two versions exporting the same package and two files sharing one diagnostic label.

#### Assertion Tests

- Add focused reader tests for duplicate keys, unknown referring members/providers, invalid exact versions, undeclared interface paths and forged exposure/provider edges.
- Extend LibraryInspectionProjectionTest to confirm that introducing typed projection records preserves existing projection bytes and private-body stability.

#### Completion Notes

Added the shared scoped binding types and versioned scope-map reader, then refactored LibraryInspection to produce typed public-index records while preserving its existing JSON projection. Validation checks declared interface paths, public-index facts, source-derived package edges, provider ownership, and witness routes; the in-memory index also retains inspected identities for libraries with no exported packages. `sbt "coreJVM/test:compile"` passed, and `sbt "coreJVM/testOnly dev.bosatsu.tool.PackageBindingsTest dev.bosatsu.tool.LibraryInspectionProjectionTest"` passed all 13 tests.

2. [ ] `step_2` Validate imported topology and selected-core composition

Extend core/src/main/scala/dev/bosatsu/tool/GroupGraph.scala and tool_command/ValidateGraphCommand.scala with a distinct library topology node. Reuse LibraryInspection's declared pinned-artifact/context records and its membership/exposure projections instead of treating imported members as loose groups. Add declared scope-map and selected-toolchain-manifest inputs for mixed validation. Preserve the existing version-1 local group and group-members contracts and --local behavior; membership-only inputs must never certify transitive topology.

Extend VerifiedLibraryGraph's existing verifier with a shared multi-root operation so one requested local root can compose several imported roots. Keep the current single-root API as an adapter. Snapshot each distinct artifact path once per request, authenticate every incoming descriptor against those snapshots, and retain all public, private and unused-transitive-public edges before identity deduplication. Shared imported roots and diamonds must use that common request state rather than invoking a complete verifier for each local group.

Use the checked binding/public-index representation to compute each member's visible release selections by following only selected package exposures. Reject two exact releases of the same library name in one visible closure, even when package names differ or signatures match, and report both actual routes. Permit separate members and private implementation scopes to select different releases. Retain loose membership collisions, scoped package ambiguity and local/import implementation ownership checks without putting unrelated private package names into one global namespace. Apply source-origin collision checks only where the existing inspection contract supplies source claims; do not invent origin data for legacy releases.

Reuse ToolchainManifest's core identity and digest rules at this composition boundary. A mixed validation must declare its selected manifest. Compare every reachable reference to that core name, including private and legacy paths, with the selected exact identity and authenticated full-file digest. Coalesce matching references; reject incompatible versions or bytes with a dependency route. Do not verify or read unrequested compiler/runtime components. Keep full hashes in this validation/artifact input family rather than adding them to public scope-map fingerprints.

Continue publishing only the existing success stamp through Output and staged publication. All traversal state is request-local, and no source, URI or undeclared manifest is discovered during validation.

#### Invariants

- Every reachable incoming descriptor is authenticated before decoder identity reuse, including private and legacy edges.
- Visible release uniqueness is checked per member and keyed by library name, with two retained witness paths on conflict.
- Every reachable core reference matches the selected manifest's exact identity and artifact digest.
- A full root reads each topology/artifact path once; local checks retain their membership-only boundary and linear aggregate manifest visits.

#### Property Tests

- Generate bounded mixed DAGs and compare visible release selections with a simple per-member reference traversal; adding a private non-exposed dependency cannot change a consumer's visible closure.
- Permute roots, edges and labels and require equivalent validation results and stable conflict routes.
- Extend visit-count checks over shared diamonds and N/2N chains, asserting one read per distinct full-graph input and preserving the existing aggregate-local linear bound.

#### Assertion Tests

- Extend GroupGraphTest and ValidateGraphCommandTest for legal private versions, disjoint member selections, same-signature conflicts, different-package conflicts, missing/ambiguous bindings and local/import collisions.
- Extend VerifiedLibraryGraphTest for shared multi-root diamonds, second-edge bad pins, private and legacy-edge authentication, and conflicting aliases.
- Cover matching repeated core references, wrong core version, same-version wrong bytes, and private-path core conflicts with path diagnostics.
- Repeat local membership-cache-input, membership-only full-scan rejection, undeclared/surplus inputs and failed-stamp publication regressions.

3. [ ] `step_3` Carry member bindings through selection and source inference

Add optional --scope-map FILE to CompilePackagesCommand using Decline option validation, then thread the checked representation through CompilerApi.compileWithSignatures and PackageSignatures.plan. Keep CheckCommand's existing unscoped behavior through an explicit legacy adapter. The compile-packages path continues to pass no filesystem cache.

Replace PackageSignatures' batch-wide provider/origin/traversal keys with scoped keys. For each source member, seed requirements from parsed imports and declared exposures, then follow selected packages' exposure edges to a fixed point in the referring context. Check those requirements against the scope map and inspection public index. Build the union of original physical interface files only after resolving all member closures, and use that union for strict-unused checking. An unselected sibling in a wide file and an implementation-only dependency must not contribute types or consume another interface file. Retain legacy exposure recovery, checked kind/constructor ownership, implicit Predef, cycle diagnostics and source-supplied exposed owners.

Extend PackageMap's existing resolveAllSourceUnits/resolvePackages and inferAllSourceUnits source-DAG path to obtain external interfaces and exposure nodes in the current member's checked context. Replace the global externalByName lookup for scoped calls; share decoded environments only when the selected bindings agree. Preserve the existing memoized source dependency DAG and compile each source once. A source dependency contributes its public interface and exposed-owner bindings, not its private closure. Stage foreign signatures that refer back to a source until that owner is compiled, and reject an inconsistent rebinding of an exposed owner before inference. Keep Package.inferBodyUnopt and nominal Type.Const unchanged.

Carry a deterministic fingerprint of selected public facts and owner bindings into the existing inference-key input in cache/CompileCache.scala, updating its bounded callers and tests. Use the member's checked interfaces for cache decoding and validate provider choices before any lookup. Keep paths, labels and full artifact hashes out of that fingerprint. Preserve legacy key behavior where practical; if the serialized key shape changes, advance the existing cache schema instead of introducing another cache. Return compiled members together with their checked per-member selected requirements for serialization in step_4.

#### Invariants

- No scoped inference or signature decoding lookup can fall back to a batch-wide provider map.
- Each member sees precisely its import/exposure closure; strict-unused checks operate on the union of the members' original files.
- Source-to-source dependencies remain in the existing DAG, and foreign re-exports bind to the actual source owner's public declarations.
- Provider changes are revalidated and affect the binding fingerprint even when public interface bytes match; private body changes behind a wrapper do not affect an outer consumer's public inputs.
- compile-packages introduces no worker inference cache.

#### Property Tests

- Extend PackageSignaturesTest with bounded scoped graphs: selection equals a reference fixed point, remains invariant under input permutations, and is unchanged by unselected siblings or private edges.
- Check that separately compiling independent member closures agrees with compiling those members together, including incompatible definitions with identical nominal names.
- Test fingerprint invariants: provenance/private-body edits preserve it, while a selected owner switch changes it even for byte-identical interfaces.

#### Assertion Tests

- Extend CompilePackagesCommandTest and StrictInterfacesCommandTest for two disjoint members using incompatible Shared/Api releases, in both source/interface orders.
- Cover one source member importing another whose private binding differs, a public exposure that creates a conflict, and source-to-import-to-source exposure chains.
- Cover missing and ambiguous bindings, exposed-owner rebinding, extra interfaces, repeated labels, wide signatures, legacy re-exports and implicit Predef.
- Retain duplicate-source, stale constructor/alias/kind, deep traversal, strict diagnostics and existing interactive cache regressions.

4. [ ] `step_4` Persist member requirements and decode with their original owners

Extend proto/src/main/protobuf/bosatsu/TypedAst.proto additively so BuildRequirements can record requirements by referring member and selected scoped owner, including the bindings required to interpret selected interfaces. Preserve existing field numbers and the legacy interfaces list. Keep canonical public Interfaces unchanged. For scoped output, record member/package identities, provider scope and canonical public-interface BLAKE3; exclude physical paths, labels, witness formatting and full implementation hashes. Use an explicit presence/version distinction so empty scoped requirements cannot be mistaken for absent legacy metadata.

Update CompilerApi.CompiledGroup, Output.PackageGroup and Output's serializer to consume the checked requirements produced by step_3 without reconstructing owners from package names. Emit one deterministic member requirement set, including exposure-only external owners. Source dependencies retain their member context even when their requirement is satisfied inside the same bundle. Serialize both outputs fully within the existing request publication lifecycle.

Extend ProtoConverter with a bundle-aware scoped decode entry point that takes the shared checked binding representation and owner-indexed provider interfaces. Decode each member and each foreign definition using its recorded context; return member/context pairs rather than flattening incompatible providers into one PackageMap. Reuse existing package/interface decoding, imported-public-view validation, constructor/alias checks and Int64 declaration checks. Reject missing, duplicate, contradictory or stale scoped requirements before returning decoded members, including an owner switch with matching interface bytes. Recompute selected exposure closure against the actual supplied providers so omitted exposure-only requirements cannot pass.

Retain the existing packagesFromProto API for legacy/local callers. Adapt LocalRuntime's requirement checker to the common requirement representation while preserving its existing local stale-layout and legacy safety checks. Supply the scoped decoding API for tracker 0n without adding mixed C emission or extending the runtime manifest in this step.

#### Invariants

- Serialization preserves the referring member and selected owner needed for every foreign lookup.
- Scoped decoding requires an explicit matching context and never silently rebinds by unqualified package name.
- Existing local bundles, legacy signatures and layout-digest safeguards retain their supported behavior.
- Public signatures and ordinary consumer requirements contain no implementation hashes or diagnostic provenance.

#### Property Tests

- Round-trip generated bounded member requirements and scoped bundles, preserving selected owners and decoded public/type layouts across input permutations.
- Change only a wrapper's private implementation and verify unchanged outer signature/requirement inputs; change a selected constructor family and require stale-layout rejection.

#### Assertion Tests

- Extend ProtoConverterTest, PackageSignaturesTest, CompilePackagesCommandTest and LocalRuntimeScopeTest for member binding round-trips and two incompatible same-named owners decoded in separate contexts.
- Reject missing/duplicate member records, omitted exposure-only owners, wrong provider identity with equal signature bytes, changed public layouts and mismatched source-supplied owners.
- Read existing unscoped requirements and older bundles/signatures, retaining the current rejection of unverifiable legacy external layouts.
- Preserve private Test/ProgTest discovery order and existing source/decoded declaration validation.

5. [ ] `step_5` Prevent bundle readers from discarding scoped context

Close the bounded file-reader bypass created by the new bundle metadata. Audit and update cli/src/main/scala/dev/bosatsu/IOPlatformIO.scala readInterfacesAndPackages/readPackages, cliJS/src/main/scala/dev/bosatsu/Fs2PlatformIO.scala readPackages, MemoryMain's package reader, PlatformIO's applicable bundle helpers and LocalRuntime.load. These are the repository-controlled readers that currently extract proto.Packages.packages before decoding.

Route whole compile-produced bundles through the common requirement parser from step_4. Readers with an explicit scoped provider context use the scoped decoder. Existing unscoped readers may continue decoding legacy/local bundles, but must reject a scoped bundle requiring unavailable imported context with a clear diagnostic instead of dropping metadata and rebinding through package names. Keep the legacy raw-package codec usable for existing library/cache internals that already supply their owning context; do not migrate unrelated publication or backend APIs.

Add focused coverage through the actual JVM, Scala.js and in-memory reader boundaries. Completion means the listed readers cannot silently flatten a scoped compile output, while existing local read/emit paths remain usable. This is the required compatibility guard until tracker 0n connects the scoped decoder to mixed runtime loading.

#### Invariants

- Every listed whole-bundle reader either preserves scoped requirements through decoding or explicitly rejects a missing scoped context.
- Legacy local readers retain supported local behavior and cannot mistake scoped metadata for absent requirements.
- No mixed native-emission behavior is introduced as part of this reader migration.

#### Property Tests

- None recorded.

#### Assertion Tests

- Exercise a scoped bundle through JVM, Scala.js, MemoryMain and LocalRuntime readers and require explicit context handling or the same missing-context diagnostic.
- Exercise ordinary local compile output and legacy bundle fixtures through those readers to retain compatibility.
- Verify malformed or stale requirement failures preserve previous declared outputs through FileCommandRunner.

6. [ ] `step_6` Add candidate-worker scope fixtures and document the implemented contract

Extend the existing LibraryInspectionFixtures and test_workspace/inspect_library fixture family only where additional serialized graphs are needed for 0m. Reuse the existing preparation script and pinned artifacts; command tests remain offline. Add scoped compilation and validation scenarios to scripts/test_bazel_worker.py using the assembled candidate jar and existing direct, @params and persistent-worker helpers.

Cover legal wrapper-private versions, incompatible disjoint members in one batch, a shared diamond, visible release conflicts with equal signatures and different exported package names, and private-path core conflicts. Warm a worker with a successful request, replace the visible binding at the same path with a conflicting release while preserving interface bytes where applicable, and compare status and diagnostics with standalone, cold and restarted execution. Follow failure with success and check previous outputs remain intact. Reverse source, interface and label order and compare output bytes and diagnostics. Round-trip a produced scoped bundle through the new decoder test boundary rather than claiming mixed C emission is implemented.

Run focused suites while implementing, then the issue's Linux acceptance commands: sbt "coreJVM/test; cli/test; coreJS/test; cliJSJS/test; cli/assembly", ./test_cli.sh, and python3 scripts/test_bazel_worker.py with --jar pointing to that candidate assembly and --inspection-fixtures pointing to prepared declared fixtures. Inspect changed executable coverage with the repository's existing coverage tasks and target complete changed-behavior coverage. MergeXO owns the separate scripts/test_basic.sh gate; no step waits for that supervised report. macOS validation remains PR CI's responsibility.

Update docs/design/hermetic-bazel-rules.md in this same slice: document the actual scope-map schema and trust boundary, mixed graph/library-node inputs, selected-core composition checks, member requirement encoding, legacy behavior, unsupported-context reader diagnostics and candidate-worker invocation. Mark tracker 0m implemented with links to code/tests and identify the scoped binding/decoder API handed to 0n. Keep select-interfaces described as an optional optimization and leave 0n/0q incomplete. Keep fixture documentation and executable cases in the repository; no separate scratch evidence artifact is needed.

#### Invariants

- Warm, cold, restarted and standalone execution use the same request-local validation and produce equivalent results after files change at reused paths.
- Failed requests preserve prior outputs and do not contaminate a later request.
- Candidate acceptance exercises the exact assembled jar and prepared offline inputs.
- Documentation claims only implemented compilation, validation and decoding behavior.

#### Property Tests

- Use the harness's reordered-input matrix to assert deterministic successful output bytes and equivalent conflict diagnostics across execution modes.

#### Assertion Tests

- Add the specified warm-success/visible-conflict/restart/recovery sequence for both scoped compilation and graph validation.
- Run the required named Scala suites, cross-platform suites, candidate assembly, CLI checks and extended Linux worker harness.
- Repeat existing local graph visit-count and membership-cache-input regressions alongside the new scoped cases.
