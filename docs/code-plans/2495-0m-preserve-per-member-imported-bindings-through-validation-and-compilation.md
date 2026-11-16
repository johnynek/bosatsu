# Code Plan #2495

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2495` 0m: Preserve per-member imported bindings through validation and compilation
- Pending steps: `5`
- Completed steps: `7`
- Total steps: `12`

## Summary

Preserve caller-relative provider bindings from graph validation through interface selection, source inference, compiled requirements and typed-bundle decoding. A member may see only one exact release of each library name; separate members may select different releases. Authenticate the complete implementation graph and selected core separately from public compilation inputs.

Extend VerifiedLibraryGraph, LibraryInspection, GroupGraph, PackageSignatures, CompilerApi, the PackageMap source DAG, CompileCache, ProtoConverter and the staged file-command lifecycle. The recurring defect is loss of the referring context at nominal package-name lookups. Repair that family once: retain checked owners through selection and per-member source execution, fingerprint their public interpretation context, and persist it for decoding. Reuse validation's authentication loader and existing dependency-context/toolchain-manifest formats. Publish the scoped command only together with scoped requirements.

Planner cost judgment: the completed selector accounts for 623 lines of production/test churn; completed production/test work across this issue totals 2,994 lines relative to its inspected base. The selector/inference/command continuation is expected to bring that subset to roughly 1,600–2,500 lines, and the complete operation-family migration is now estimated at 5,000–7,000 lines including serialization, reader guards, tests and worker fixtures. These are cumulative estimates, not separate allowances reset by step ids. Explicitly authorize exceeding the 1,000-line guideline for the named migration: authentication, context-aware inference, provider-sensitive keys and persisted decoding context are directly coupled acceptance requirements. Stopping at selection or applying isolated lookup patches cannot compile disjoint incompatible providers safely. Keep the migration bounded to the listed mechanisms and their required call sites; reuse algorithms, codecs and fixtures rather than introducing parallel systems.

Mixed native emission, runtime-manifest expansion, Bazel/Gazelle implementation, producer/history features and the optional select-interfaces command remain outside this job. The existing final worker/documentation step retains Linux acceptance and the tracker 0n handoff. All required evidence is produced by repository tests and the candidate-jar harness before publication; MergeXO owns the separate required-tests gate.

Plan trajectory: initial steps 6; review revisions 0; review-origin steps 0.

## Current State

Replanning inspected checkpoint 9b4e1bf5a9d78f164618d9bde3affc29bbe75d87 against the initial base 620bbfd3ed0da74435ee5a083b534c6e8ec9fd06. The issue, root #2482, immediate node and implemented dependency handoffs align. Retained human history has disclosed coverage gaps and supplies no scope amendment. The implementation planning context reports no edits or tests in the paused continuation; this planning revision also changes no implementation files.

Completed history defines PackageBindings and the inspection public-index contract, authenticates imported topology and selected-core composition, and connects mixed graph validation. ValidateGraphCommand currently owns a private imported-artifact/signature verifier that compilation must reuse. PackageSignatures.planScoped selects each member's checked external roots, source dependencies and public exposure closure, detects visible release conflicts and computes the strict-unused file union. Its completed tests and verification remain recorded in the immutable completion notes.

Compilation is still unscoped: CompilePackagesCommand has no scope-map option, CompilerApi.compileWithSignatures carries a batch-wide Providers map, PackageMap.resolvePackages uses batch-wide interface lookup and source-name precedence, and inferAllSourceUnits uses externalByName. The existing memoized source DAG and per-source cache/inference body are the extension points. CompileCache keys lack selected owner identity. CompilePackagesCommand currently supplies no filesystem inference cache, which must remain true.

Output.PackageGroup still emits package-wide unscoped requirements. ProtoConverter and LocalRuntime require the planned member-context extension, and the listed platform readers can discard whole-bundle requirements. Remaining work is ordered as shared authenticated loading, binding-sensitive cache inputs, contextual source-DAG inference, command plus requirement publication, scoped decoding, reader guards, then the retained candidate-worker fixtures and design/tracker update. No selected-step work is marked complete because the paused continuation made no implementation changes.

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

2. [x] `step_2` Validate imported topology and selected-core composition

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

#### Completion Notes

Added `VerifiedLibraryGraph.verifyRoots`, with the existing single-root API delegating to it. Shared roots and diamonds use one path-keyed snapshot table; the two-root test checks each artifact is read once and root reordering preserves edge routes. Added `ToolchainManifest.validateCoreReferences` to check every reachable core reference against the selected exact version and authenticated SHA-256, with private-route tests for matching, wrong-version, and wrong-digest cases. `sbt "coreJVM/testOnly dev.bosatsu.library.VerifiedLibraryGraphTest"` passed all 13 tests.

3. [x] `step_2_mixed_validation` Wire mixed topology and core validation

This is the first continuation of the original `step_2` imported-topology and selected-core family. Its completed slice provides `VerifiedLibraryGraph.verifyRoots` and `ToolchainManifest.validateCoreReferences`; this step completes the remaining command-level composition required by tracker 0m. The parent slice changed 282 production and test lines. Estimate 500–650 additional lines for this continuation, keeping the family below the 1,000-line threshold.

Extend `core/src/main/scala/dev/bosatsu/tool/GroupGraph.scala` to represent local groups and imported library scopes as distinct topology nodes. Preserve the version-1 `group`, `group-members`, and `--local` behavior; membership-only nodes must not certify transitive topology. Extend `core/src/main/scala/dev/bosatsu/tool_command/ValidateGraphCommand.scala` to accept a checked `--scope-map`, the existing `LibraryInspection.DependencyContext` format, and a selected `ToolchainManifest` for mixed validation. Require these inputs together for mixed validation, and keep every artifact and manifest read within the caller-declared inputs.

Use `PackageBindings` and `LibraryInspection` to validate imported identities and public exposure records against the authenticated graph. Compose all imported roots in one `VerifiedLibraryGraph.verifyRoots` call, and run `ToolchainManifest.validateCoreReferences` on the complete graph so private and legacy edges are checked too. Derive per-member visible releases by following only the selected package bindings and their exposures. Reject two exact releases of one library name in a member's visible closure with both witness routes; permit disjoint members and private implementation scopes to select different releases. Retain local membership collisions and local/import ownership checks without flattening unrelated private package names into one namespace. Keep full artifact digests in this validation path rather than public binding fingerprints.

Publish only the existing success stamp through the current output lifecycle. Preserve the existing local manifest visit-count and membership-cache-input behavior. Finish when mixed validation uses the shared verifier and selected manifest, rejects invalid visible selections and core references with routes, and the legacy local command behavior remains covered.

#### Invariants

- Version-1 local topology and membership-only scan behavior remain unchanged; membership-only inputs never certify transitive topology.
- Imported libraries remain exact-identity scopes, and private package names are not merged into one global namespace.
- Every imported descriptor edge is authenticated, and every reachable selected-core reference matches the manifest's exact identity and full-file digest.
- Mixed validation reads only declared inputs and publishes the success stamp only after validation succeeds.

#### Property Tests

- Permute root, node, and edge declaration order and require equivalent validation results and stable conflict routes.
- Generate bounded mixed DAGs and verify per-member visible releases follow only selected bindings and exposures; adding an unexposed private dependency does not alter a member's visible closure.
- Extend shared-diamond and N/2N visit-count checks to assert one read per distinct full-graph input while preserving the aggregate local-scan bound.

#### Assertion Tests

- Extend `GroupGraphTest` and `ValidateGraphCommandTest` for mixed local/import composition, legal private versions, disjoint members, same-signature and different-package release conflicts, missing or ambiguous bindings, and both conflict routes.
- Cover a shared imported diamond, undeclared and surplus inputs, and local membership-only and visit-count regressions; failures preserve the prior validation stamp.
- Cover matching repeated core references, wrong core version, same-version wrong digest, and private or legacy-path core conflicts with dependency routes.

#### Completion Notes

Extended GroupGraph and validate-graph to combine local groups with authenticated imported scopes, check each member’s visible release closure, verify imported signatures and selected-core references, and preserve the existing local validation behavior. Added coverage for private releases, visible release conflicts with both witness routes, partial mixed configuration, and private-path core conflicts. `coreJVM/test:compile` and `cli/test:compile` passed; focused GroupGraph, PackageBindings, VerifiedLibraryGraph, ToolchainManifest, and ValidateGraphCommand suites passed (54 tests total). `git diff --check` passed.

4. [x] `step_3` Carry member bindings through selection and source inference

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

#### Completion Notes

Added `PackageSignatures.planScoped`, which selects interfaces by checked per-member owners, follows selected public exposures to a fixed point, retains source dependency references, checks visible release conflicts, and unions selected physical files for strict-unused checks. Tests cover disjoint members selecting byte-identical releases, exposure chains, wide-file siblings, private source imports, source exposures, and visible release conflicts. `sbt "coreJVM/testOnly dev.bosatsu.PackageSignaturesTest"` passed all 19 tests; `PackageBindingsTest` passed all 9 tests in the combined focused run; `git diff --check` passed. CLI and inference wiring remains pending in the inserted continuation.

5. [x] `step_3_authenticated_inputs` Share authenticated imported-input loading

The scoped selector is implemented, but compilation cannot authenticate a scope map using its caller-supplied public index alone. The necessary authentication already exists in the private verifyImports method of core/src/main/scala/dev/bosatsu/tool_command/ValidateGraphCommand.scala. Extract that implementation into core/src/main/scala/dev/bosatsu/tool/LibraryInspection.scala and make validate-graph use it. This replaces the command-private implementation rather than adding a second verifier.

The shared loader takes a parsed PackageBindings.ScopeMap, the existing declared dependency context and selected ToolchainManifest. Preserve root pin verification, authentication of every reachable descriptor edge before deduplication, scoped library decoding, inspected public-index generation, private-path core validation and comparison of each imported signature with its authenticated projection. Return an immutable result containing the verified graph, public index and the parsed signature bundles that were actually checked. Subsequent compilation must consume those bundles rather than reopen their paths after verification. Retain the empty-import case and existing diagnostic provenance.

Keep source-edge validation outside this loader: graph validation supplies its topology checks, while compilation will supply actual source-header dependencies. Do not manufacture authentication by passing scopeMap.publicIndex as the inspected index. Reuse existing dependency-context, manifest and public-index formats; no new trust file or verification subsystem is needed.

Extend existing LibraryInspectionProjectionTest, PackageBindingsTest and ValidateGraphCommandTest coverage as appropriate. Run the focused changed suites and compile checks. Completion means validate-graph uses the shared loader with unchanged supported behavior, and the next compilation step can obtain authenticated signature inputs through that same boundary.

#### Invariants

- Artifact authentication and selected-core checks retain every reachable private, public and legacy descriptor edge.
- A caller-supplied public index is compared with authenticated inspection results and never serves as authentication by itself.
- Compilation can reuse the exact parsed signature bundles checked by the loader.
- Local graph traversal, membership-only validation and diamond visit behavior remain intact.

#### Property Tests

- Permuting declared artifacts and imported scopes preserves the authenticated public result and equivalent rejection diagnostics.

#### Assertion Tests

- Retain successful shared-diamond and legal-private-version validation.
- Reject substituted signature contents, forged public ownership, invalid pins and private-path core conflicts through the shared loader.
- Retain local-only validation and partial mixed-option rejection in ValidateGraphCommandTest.

#### Completion Notes

Moved imported artifact verification into `LibraryInspection.loadImportedInputs`, which returns the verified graph, regenerated public index, and the parsed interface bundles checked against authenticated projections. `validate-graph` now uses this shared boundary. Added coverage for returned bundles, substituted signatures, forged public-index ownership, invalid artifact pins, and private-path core conflicts. `sbt "coreJVM/test:compile"` passed; focused core tests passed (14 tests), focused `ValidateGraphCommandTest` passed (10 tests), and `git diff --check` passed.

6. [x] `step_3_binding_cache` Include selected bindings in existing cache identity

The existing CompileCache key hashes interfaces by nominal package name and cannot distinguish a selected-provider switch when signature bytes are equal. Extend core/src/main/scala/dev/bosatsu/cache/CompileCache.scala and FsKey.scala with an explicit optional binding fingerprint. Update GenerateKeyInput construction and destructuring sites, using an explicit absent value for legacy callers; do not introduce default arguments or a second cache.

Define the fingerprint from a deterministic, path-free projection of the selected PackageBindings binding keys and PackageOwner identities. Include the referring context and owner bindings needed to interpret the member's selected public interfaces, including exposure-only owners. Existing dependency-interface hashes continue to represent the public facts; avoid serializing those facts twice merely to calculate the binding fingerprint. Exclude physical paths, labels, witnesses, full library artifact digests and unselected bindings. Use canonical identity encodings and the repository's ordering/typeclass contracts.

Add the fingerprint to FsKey hashing and advance the existing cache schema because the key payload changes. Keep cached-package decoding supplied with the member-specific dependency interfaces already carried by FsKey. Update the existing PackageMap key call with the legacy absent value; scoped inference will supply its checked fingerprint in the next step.

Extend core/src/test/scala/dev/bosatsu/tool/CompileCacheTest.scala with focused identity and filesystem-cache regressions. Run that suite and coreJVM/test:compile. This step introduces no CLI cache option and no worker inference cache.

#### Invariants

- A changed selected provider changes scoped cache identity even when public signature bytes match.
- Unselected providers, private implementation hashes and diagnostic provenance do not enter ordinary consumer keys.
- Legacy callers explicitly select legacy key inputs and retain supported cache behavior.
- Provider validation remains required before scoped cache lookup; a fingerprint does not authenticate a selection.

#### Property Tests

- Binding permutations and changes to paths, labels or witnesses preserve the fingerprint.
- Changing a selected owner changes the fingerprint; changing only an unselected binding preserves it.

#### Assertion Tests

- Equal public interfaces under two different selected releases produce distinct scoped keys.
- An unchanged checked context can hit the existing filesystem cache and decode using its supplied interfaces.
- Retain existing cache corruption, public-layout invalidation and legacy cache regressions.

#### Completion Notes

Added a deterministic, path-free fingerprint of selected binding keys and owners to the existing filesystem cache key, advanced its schema from 5 to 6, and kept the legacy PackageMap caller explicitly unscoped with `None`. Tests cover ordering and provenance invariance, selected-owner changes, distinct keys for equal dependency-interface hashes, and warm decoding with the same context. `sbt "coreJVM/testOnly dev.bosatsu.tool.CompileCacheTest" "coreJVM/test:compile"` passed (12 tests); `git diff --check` passed.

7. [x] `step_3_member_dag` Resolve and infer sources in their checked contexts

The root defect appears at CompilerApi.compileWithSignatures's batch-wide Providers map, PackageMap.resolvePackages's interfaceMap/source-name precedence, and inferAllSourceUnits's externalByName. Extend the existing source DAG so these lookups use each member's checked selection. Do not patch only externalByName: an earlier resolver could already have selected the wrong external interface or an unrelated same-named source.

Add an explicit scoped path to core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala using PackageBindings.BindingMode and PackageSignatures.planScoped. Preserve the legacy adapter used by CheckCommand. Scoped preparation consumes checked bindings and the authenticated signature bundles from LibraryInspection. Add source-header dependency derivation using actual source imports and their declared provider requests; check local targets against actual members and imported ownership against authenticated inspection. Compare these derived edges through PackageBindings.validateAgainstInspection rather than supplying scopeMap.dependencies as its own evidence. Resolve exposure-only requirements through the public closure instead of promoting them to direct source imports.

Implement the scoped execution through the existing memoized source DAG in core/src/main/scala/dev/bosatsu/PackageMap.scala:

1. Use MemberSelection.sourceDependencies to schedule required source owners, including owners needed by a foreign API that refers back to a source. Diagnose dependency cycles before recursive inference. Each source is inferred once.
2. After those owners finish, obtain only their canonical public interfaces and declared exposed-owner closures. Keep their private imports in their own contexts.
3. Materialize this member's selected foreign entries in dependency order, seeded with the required source public providers and effective Predef. Extend/reuse PackageSignatures.Root.bind and existing signature decoding, kind/layout validation and cycle handling; do not copy their validation into another decoder.
4. Resolve each import according to its checked owner. A source elsewhere in the batch must not override a checked library provider merely because its package name matches. Reject inconsistent exposed-owner rebinding before inference or cache lookup.
5. Feed the resulting member-specific public environment and binding fingerprint into the existing per-source inference/cache body. Extract that body only as needed to share it with legacy execution. Do not repeatedly invoke whole-batch compilation for separate closures or add a second inference engine.

Keep all owner-qualified identity until entering a single member's validated nominal environment. Sharing is optional and permitted only for equivalent selected bindings; no environment-interning subsystem is required. Preserve effective Predef, existing diagnostics, source-DAG stack safety and lint behavior for legacy callers.

Extend CompilerApi.CompiledGroup with an explicit legacy/scoped result distinction. Scoped results carry each compiled member beside its checked selected owner bindings and public dependency interfaces, including source and exposure-only dependencies. Reuse PackageBindings identities and records; do not flatten these into one dependency list or add protobuf encoding here. This is the input consumed by step_4_scoped_publication.

Add internal compiler/source-DAG regressions using the existing PackageMap, PackageSignatures and compiler test helpers. Run focused suites and coreJVM/test:compile. Command exposure and persisted output are deliberately implemented together in step_4_scoped_publication, so this step cannot publish a scoped compilation with legacy requirements.

#### Invariants

- Every scoped import-resolution, foreign-decoding and inference lookup uses the referring member's checked owners.
- Each source is inferred once through the existing memoized DAG, including source owners required by foreign exposures.
- Consumers receive source public APIs and declared exposures without private imports.
- Validation and owner-consistency checks precede cache lookup.
- Scoped compiled results retain per-member requirements without a batch-wide external-provider union.

#### Property Tests

- For bounded acyclic fixtures, batching disjoint members yields the same per-member inferred public results as compiling their individual checked closures, invariant under source/interface permutations.
- Editing a source dependency's private imports leaves a consumer's selected environment and binding fingerprint unchanged when its public API and exposed owners remain unchanged.

#### Assertion Tests

- Infer two members selecting incompatible definitions with the same nominal package/type names.
- Honor a checked library owner when an unrelated same-named source is present in the batch.
- Cover source-to-source imports, source-exposed imported owners and a foreign interface that requires a source owner before decoding.
- Reject conflicting exposed owners and dependency cycles without repeated source inference.
- Retain legacy CheckCommand, effective Predef, constructor/layout and compiler-cache behavior.

#### Completion Notes

Added `CompilerApi.prepareScopedCompilation` to derive dependency edges from actual source headers, validate them against the authenticated public index, and plan from authenticated signature snapshots. Added `PackageMap.resolvePackagesScoped` to honor each member’s selected source or interface owner and fail closed when a binding is missing. Regression coverage includes forged declared edges, missing bindings, distinct member interfaces despite a same-named source, and legacy resolver stack safety. The focused suites passed (24 tests); `sbt "coreJVM/test:compile"` and `git diff --check` passed. Member-DAG inference, foreign-interface binding after source dependencies, and scoped `CompiledGroup` results remain pending.

8. [ ] `step_3_member_dag_execution` Infer members through the checked source DAG

Parent step: `step_3_member_dag`; family: per-member scoped source inference for issue #2495. The parent added `CompilerApi.prepareScopedCompilation` for source-header-derived validation and `PackageMap.resolvePackagesScoped` for exact per-member import resolution. Complete the remaining execution layer before `step_4_scoped_publication` consumes its results.

Integrate the scoped plan into `CompilerApi` and the existing memoized source DAG in `PackageMap`. Schedule each member’s `MemberSelection.sourceDependencies`, including source owners needed by selected foreign APIs; diagnose cycles before recursive inference and infer each source once. After source dependencies finish, expose only their canonical public interfaces and declared exposure closures, keeping private imports in each source’s own context.

Materialize selected foreign signature entries in dependency order using `PackageSignatures.Root.bind` and its existing decoding, kind/layout validation, and cycle handling. Seed binding with required source public providers and effective Predef. Resolve imports through `PackageMap.resolvePackagesScoped` using the checked selection so local sources and unrelated interfaces cannot override a selected owner. Reject inconsistent exposed-owner rebinding before inference or cache lookup. Feed each member’s public environment and the existing selected-binding fingerprint into the current inference/cache body; preserve the legacy path without introducing another inference engine or whole-batch recompilation per member.

Extend `CompilerApi.CompiledGroup` with an explicit legacy/scoped distinction. Scoped results must retain each compiled member alongside its checked owner bindings and public dependency interfaces, including source and exposure-only dependencies. Reuse `PackageBindings` records; leave protobuf encoding to `step_4_scoped_publication`.

Add source-DAG regressions for incompatible same-named providers in one batch, a conflicting same-named local source, source-to-source and source-exposure dependencies, a foreign API referring back to a source, cycles, and one-time source inference. Retain effective Predef, diagnostics, lint, cache, and legacy caller behavior. Run the focused compiler suites and `sbt "coreJVM/test:compile"`. Completion means scoped compilation produces per-member inferred results with their original checked contexts, ready for publication.

#### Invariants

- Every scoped import, foreign-interface binding, and inference lookup uses the referring member’s checked owners.
- Each source is inferred once through the existing memoized DAG, and dependency cycles are diagnosed before recursive inference.
- Consumers receive source public APIs and declared exposures without inheriting private imports.
- Owner consistency is checked before inference or cache lookup; legacy compilation retains its supported behavior.

#### Property Tests

- For bounded acyclic fixtures, batching members with disjoint selections yields the same per-member inferred results as compiling their checked closures individually, invariant under source and interface order.
- Changing a source dependency’s private imports does not change a consumer’s selected public environment when the source API and declared exposures are unchanged.

#### Assertion Tests

- Compile two members selecting incompatible definitions with the same nominal package and type names in one batch.
- Verify a checked library owner wins over a same-named local source, while a checked source owner uses the local source DAG result.
- Cover source-supplied exposure chains and a selected foreign API that refers back to a source owner.
- Reject source dependency cycles and conflicting exposed-owner rebinding before inference or cache lookup.
- Retain effective Predef, legacy CheckCommand, cache, layout, and lint regressions.

9. [ ] `step_4_scoped_publication` Publish scoped compilation with member requirements

Expose the scoped compiler only together with its persisted context. This completes the command and serialization portion formerly spread across step_3_inference and step_4, preventing a successful scoped command from emitting requirements that lose its selected owners.

In core/src/main/scala/dev/bosatsu/tool_command/CompilePackagesCommand.scala add optional --scope-map FILE. Reuse validate-graph's existing --dependency-context and --toolchain-manifest input formats for authenticated imports; require the three scoped options together using Decline mapValidated. Retain the current unscoped invocation and keep CheckCommand on the explicit legacy adapter. Read PackageBindings using the original declared direct-interface paths, use the shared authenticated loader and source-derived validation from the preceding steps, then invoke the scoped CompilerApi path. Preserve request-local execution and pass no filesystem inference cache. Include the new declared inputs in applicable output-path checks.

Strict-unused checking uses the union of selected original files after every member's import/public-exposure fixed point. Preserve original file/label attribution; do not admit unrelated wide-signature exports or implementation-only dependencies into inference. Use the authenticated parsed signature snapshots rather than rereading checked imported files.

Extend proto/src/main/protobuf/bosatsu/TypedAst.proto additively so BuildRequirements records requirements by referring member and selected owner, including bindings needed to interpret foreign interfaces and source-supplied owners. Preserve existing field numbers and the legacy interfaces list. Include explicit scoped presence/version information so empty scoped requirements cannot look like absent legacy metadata. Keep canonical public Interfaces unchanged.

Update Output.PackageGroup and its serializer to consume the scoped CompiledGroup records directly. Serialize deterministic member identities, binding keys, selected owner identities and canonical public-interface BLAKE3 requirements, including exposure-only owners. Source dependencies retain their member context even when supplied within the same bundle. Exclude paths, labels, witnesses and implementation hashes. Preserve legacy output encoding for unscoped compilation. Serialize both outputs completely through the existing staged publication lifecycle.

Provide the common requirement parser used by subsequent decoding and file-reader work, rejecting malformed versions, duplicate member records and contradictory records. It must preserve the legacy/scoped distinction without requiring a new file format outside TypedAst.proto.

Extend CompilePackagesCommandTest, StrictInterfacesCommandTest and appropriate requirement-codec tests. Test command authentication failures, scoped compilation, output records and publication recovery now; bundle decoding belongs to step_4_scoped_decoding. Run focused suites and core/CLI compile checks.

#### Invariants

- Every successful scoped command emits the member context required to interpret its compiled output.
- Selected owners are taken from checked compilation results rather than reconstructed from package names.
- Public signature and requirement inputs exclude implementation hashes and diagnostic provenance.
- Unscoped compilation remains supported, and compile-packages introduces no inference-cache option.
- Failed authentication, compilation or publication preserves previous declared outputs.

#### Property Tests

- Permuting source, interface, binding and label inputs preserves successful signature and implementation output bytes.
- Round-trip bounded requirement records while preserving the legacy/scoped distinction and selected owner identities.

#### Assertion Tests

- Compile disjoint incompatible providers and source/import exposure chains through the actual command.
- Reject missing/ambiguous bindings, forged inspection metadata, substituted signatures and mismatched source dependencies before inference.
- Report an extra original interface file under strict checking, including its label.
- Persist exposure-only owners and source-supplied owners, including an explicitly empty scoped requirement set.
- Retain unscoped command, CheckCommand and failed-output-publication regressions.

10. [ ] `step_4_scoped_decoding` Decode bundles with their recorded member owners

Use the requirement representation and parser from step_4_scoped_publication to extend core/src/main/scala/dev/bosatsu/ProtoConverter.scala with a bundle-aware scoped decode entry point. It accepts the shared checked binding representation and owner-indexed supplied interfaces. Decode each member and foreign definition with its recorded context, returning member/context pairs rather than flattening incompatible external providers into one PackageMap.

Reuse existing package/interface decoding, imported-public-view checks, constructor and alias validation, layout digests and Int64 declaration checks. Source requirements satisfied within the bundle must still select the recorded source member and its context. Recompute the selected exposure closure against actual supplied providers so omitted exposure-only requirements cannot pass. Reject missing contexts, missing/duplicate member records, contradictory bindings, stale public hashes and provider switches even when signature bytes match. Do not make a package-name fallback available on the scoped branch.

Retain packagesFromProto for legacy/local callers and its existing supported compatibility behavior, including rejection of unverifiable legacy external layouts. Adapt core/src/main/scala/dev/bosatsu/tool/LocalRuntime.scala's requirement checker to the common representation while preserving local stale-layout safeguards. Supply the scoped decoder contract for tracker 0n; mixed C emission and runtime-manifest expansion remain outside this step.

Extend ProtoConverterTest, PackageSignaturesTest, CompilePackagesCommandTest and LocalRuntimeScopeTest where each boundary is exercised. Run focused suites. Completion means a command-produced scoped bundle can round-trip through this decoder with two incompatible same-named external owners in separate member contexts, and stale or rebound contexts fail before decoded members are returned.

#### Invariants

- Scoped decoding uses the recorded referring context and exact selected owner for every foreign lookup.
- Scoped bundles never silently rebind through an unqualified package map.
- Supported legacy/local bundles, signatures and layout safeguards retain their behavior.
- The decoder handoff does not claim mixed native emission is implemented.

#### Property Tests

- Round-trip bounded scoped bundles under input permutations, preserving member owners and decoded public/type layouts.
- Private implementation edits behind an unchanged wrapper preserve outer public requirements; changed selected layouts cause stale-layout rejection.

#### Assertion Tests

- Decode a command-produced bundle containing members with incompatible same-named external types.
- Reject an equal-signature owner switch, omitted exposure-only owner, wrong source-supplied owner and malformed member records.
- Read existing unscoped bundles and signature fixtures with their existing compatibility checks.
- Retain local stale-layout, private Test/ProgTest discovery and Int64 declaration regressions.

11. [ ] `step_5_bundle_reader_context` Preserve scoped metadata at whole-bundle readers

Close the bounded reader bypass using the common requirement parser from step_4_scoped_publication and scoped decoder from step_4_scoped_decoding. Update cli/src/main/scala/dev/bosatsu/IOPlatformIO.scala readInterfacesAndPackages/readPackages, cliJS/src/main/scala/dev/bosatsu/Fs2PlatformIO.scala readPackages, MemoryMain's package reader, applicable PlatformIO bundle helpers and LocalRuntime.load. These whole-bundle readers currently extract proto.Packages.packages before decoding and can discard requirements.

Pass whole compile-produced bundles through the common parser. Readers with an explicit scoped provider context use the scoped decoder. Readers without that context continue supporting legacy/local bundles and reject scoped requirements needing unavailable imported context with a clear diagnostic. Do not reject a self-contained local context merely because scoped metadata is present. Preserve the raw-package codec for library/cache internals that already supply their owning context; do not migrate unrelated backend or publication APIs.

Cover the listed JVM, Scala.js, in-memory and LocalRuntime boundaries through their actual reader APIs. Verify ordinary local read/emit compatibility and failure recovery through FileCommandRunner. Run focused platform suites. Completion means none of the listed whole-bundle readers can discard scoped metadata and silently rebind a compile output. Tracker 0n will connect the decoder to mixed runtime loading.

#### Invariants

- Every listed whole-bundle reader preserves scoped requirements or explicitly rejects unavailable context.
- Legacy/local and self-contained local scoped contexts retain supported behavior.
- Raw codecs remain available only where the caller already supplies the owning decode context.

#### Property Tests

- None recorded.

#### Assertion Tests

- Read an imported scoped bundle through JVM, Scala.js, MemoryMain and LocalRuntime boundaries and require explicit context handling or a missing-context diagnostic.
- Retain ordinary local compile output, self-contained local scoped output and legacy fixture compatibility.
- Malformed or stale requirements fail without replacing prior declared outputs.

12. [ ] `step_6` Add candidate-worker scope fixtures and document the implemented contract

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
