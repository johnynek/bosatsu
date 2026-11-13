# Code Plan #2486

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2486` 0p: Verify declared compiler, core and runtime components
- Pending steps: `0`
- Completed steps: `4`
- Total steps: `4`

## Summary

Implement file-only `tool verify-toolchain` against the caller-trusted detached manifest, extending the existing digest abstraction with portable SHA-256. Verify the compiler and only the requested core/source/runtime files, then publish a success stamp through the existing file-command lifecycle. Include cross-platform digest/verifier tests, JVM command and exact-candidate-jar worker tests, and the tracker 0p documentation update.

The bounded operation family is the new command through `ToolCommand`, ordinary JVM dispatch through `FileCommandRunner`, and one-shot dispatch through `BazelWorker.Operation`. All paths use the same requested-component verifier before producing an `Output`. Import-graph authentication, native composition, persistent-worker transport, Bazel rules, and release manifest production remain assigned to the other roadmap nodes. MergeXO's supervised `scripts/test_basic.sh` gate remains a publication prerequisite.

Plan trajectory: initial steps 4; review revisions 0; review-origin steps 0.

## Current State

The checkout provides the extension points described by issue #2486 and roadmap #2482, with no dependency handoff required. `hashing/Algo.scala` supports incremental BLAKE3 and parses only `blake3:<hex>` identifiers. Existing library and compiler CAS behavior uses BLAKE3 explicitly. `tool/BuildManifest.scala` provides strict UTF-8 decoding, duplicate-object-field detection, schema-version validation, and file/JSON-location diagnostics. The detached compiler/core/runtime schema is documented in `docs/design/hermetic-bazel-rules.md`, but tracker 0p is incomplete and no verification command exists.

`PlatformIO.readBytes` supports reading one snapshot for hashing and parsing. Existing protobuf library and version helpers expose embedded core identity, and `PackageResolver.headerParserIgnoreRest` supplies package-header parsing. `ToolCommand`, `FileCommandRunner.supports`, and `BazelWorker.Operation` are the bounded registration sites. The JVM runner already publishes returned outputs through `StagedOutputs`. `build.sbt` generates `BuildInfo.cRuntimeArchiveHash` from `BOSATSU_C_RUNTIME_HASH`; the runtime installer already distinguishes an empty stamp from a malformed nonempty stamp. BuildInfo is generated source.

## Problem

Build actions cannot yet establish that their supplied compiler, core library or supported core sources, and compressed runtime archive belong to the combination selected by one trusted manifest. Independent artifact pins and matching core name/version are insufficient: changed bytes must fail against the selected manifest, and an advertised runtime hash must additionally agree with the requested archive. Verification must remain offline, tolerate absent unused components, and preserve existing success outputs on failure. Shared SHA-256 support is also required so this command and subsequent import verification use the existing digest mechanism.

## Technical Debt Notes

Roadmap follow-up: `qualify_cli` owns generating and pinning detached manifests from final staged release artifacts, qualifying the distributed compiler/core/runtime tuple, and wiring release/CI publication. Import verification and scoped native composition own authentication of reachable core references and propagation of verified source origins into composed graphs. This change supplies the shared digest and requested-component verification contracts those nodes need; it introduces no dependency on their future merges.

## Steps

1. [x] `step_1` Extend the shared digest abstraction with portable SHA-256

Add a pinned `sha` dependency in `project/Dependencies.scala` and the shared core dependencies in `build.sbt`: `"pt.kcry" %%% "sha" % "2.0.2"`. Both Scala 3 JVM and Scala.js artifacts are published. Its [upstream API](https://github.com/kcrypt/scala-sha) supplies synchronous incremental hashing compatible with the current `Algo` interface.

In `core/src/main/scala/dev/bosatsu/hashing/Algo.scala`, add a SHA-256 algorithm marker and instance using `pt.kcry.sha.Sha2_256`. Implement one-shot hashing, fresh incremental hashers, offset/length updates, and finalization to 64 lowercase hexadecimal characters. Keep mutable backend state confined to each hasher. Extend `parseIdent` to accept `sha256:<hex>` using the existing parser construction, retaining BLAKE3 behavior. Expose or reuse the same digest parsing when reading bare manifest `sha256` fields; do not create another hash representation or parser grammar.

Add shared tests in `core/src/test/scala/dev/bosatsu/hashing/AlgoTest.scala` and a JVM reference comparison in the CLI test sources. Leave BLAKE3-selected library hashing, signature hashes, compile-cache keys and CAS paths unchanged. Run the focused digest suites on JVM and Scala.js and the existing compile-cache regression suite.

#### Invariants

- Both algorithms use the existing Algo, HashValue and WithAlgo interfaces and identifier syntax.
- SHA-256 results are independent of update partitioning and include exactly the bytes selected by each offset/length update.
- Each new hasher has independent state; existing BLAKE3 hashes, identifiers and CAS behavior remain compatible.

#### Property Tests

- For generated byte arrays and chunk partitions, one-shot and incremental SHA-256 agree, including empty updates and updates from slices of padded arrays.
- Round-trip generated SHA-256 and BLAKE3 identifiers through the shared parser and renderer.
- Compare generated JVM SHA-256 results with java.security.MessageDigest, with boundary lengths around SHA-256 padding and block sizes.

#### Assertion Tests

- Check known empty, abc, multi-block and long-input SHA-256 vectors on coreJVM and coreJS.
- Pin existing BLAKE3 vectors and reject unsupported algorithms, wrong-length hex, invalid characters and trailing input.
- Run existing CompileCacheTest coverage to detect changes to BLAKE3-selected cache behavior.

#### Completion Notes

Added the pinned SHA-256 dependency, incremental and one-shot support in `Algo`, `sha256:<hex>` and shared bare-hex parsing, plus cross-platform vectors, parser, partitioning, and JVM reference tests. Made existing generic library-hash calls explicitly select BLAKE3. The focused JVM and Scala.js digest suites, JVM reference suite, and `CompileCacheTest` passed.

2. [x] `step_2` Implement the typed manifest and requested-component verifier

Add `core/src/main/scala/dev/bosatsu/tool/ToolchainManifest.scala` containing the version-1 detached-manifest model, reader and requested-component verification entry point. Extend `BuildManifest.scala` only where sharing its strict byte-to-UTF-8 decoding avoids duplication. Use `BuildManifest.obj`, `version1` and `read` for the documented `compiler`, `core`, `c_runtime` and optional `core_sources` fields. Parse component SHA-256 values through the shared algorithm parser into typed SHA-256 values; parse core versions and source package names with existing repository parsers. Reject ambiguous duplicate fields, including duplicate source-package keys. URI fields are metadata and must never trigger fetching.

Represent core selection as no core, one binary core, or a nonempty source list. The verification entry point accepts the supplied compiler path, core selection, optional runtime path, platform capabilities and an explicit advertised runtime-hash string. Production command wiring will supply `BuildInfo.cRuntimeArchiveHash`; the explicit parameter permits testing the policy without altering generated source or adding a CLI override.

Always hash the supplied compiler bytes against the selected manifest. For binary core, read one byte snapshot, verify its full SHA-256, parse `proto.Library` from those same bytes, and compare its embedded name and required version using existing library helpers. Do not resolve its dependencies. For each requested source, read bytes once, compute SHA-256, strictly decode that snapshot, and parse its declared package with `PackageResolver.headerParserIgnoreRest`. Require a matching `core_sources` entry and reject changed bytes, unlisted names and duplicate package providers. Reject combined binary/source selection. Verification does not type-check source bodies.

When a runtime is requested, verify the compressed archive bytes against manifest SHA-256. An empty advertised hash permits development builds with the trusted manifest. Parse every nonempty advertised hash with `Algo.parseIdent` and verify it against the same archive snapshot using its selected algorithm; malformed or conflicting values fail. When runtime is not requested, do not read an archive or evaluate the embedded runtime-hash policy. Finish all checks before returning success.

Add shared tests using the existing `MemoryMain` platform and small read-counting/failing adapters where needed. Run them on coreJVM and coreJS.

#### Invariants

- The selected manifest supplies every expected component SHA-256; artifact metadata and separate valid pins cannot override it.
- Each component's hash and identity checks use the same bytes; source names and source hashes cannot come from separate reads.
- Only explicitly requested component files are read, and verification performs no compilation, resolution, process execution or installation.
- A requested runtime must satisfy both manifest SHA-256 and any nonempty advertised runtime digest.
- Generic paths are compared and rendered through PlatformIO capabilities.

#### Property Tests

- Generate valid manifests and requested-component subsets; instrument reads to prove unrequested components are never accessed and requested identity checks reuse their byte snapshots.
- Replace each requested component with different bytes and its independently correct digest while retaining the selected manifest; verification must fail.
- For generated source snapshots and package names, acceptance requires both the listed package identity and the selected digest; changing either fails.
- Exercise the verifier with the throwing-equality/rendering path adapter pattern from ParametricPathCommandsTest.

#### Assertion Tests

- Cover invalid UTF-8, malformed JSON, missing required fields, unsupported versions, duplicate fields/package keys, malformed SHA-256, invalid versions and invalid source package names.
- Accept compiler-only, binary-core, supported-source and native combinations; cover missing requested files and absent unused files.
- Reject same-name/version core with changed bytes, and separately reject wrong embedded name, wrong or missing version and malformed library bytes even when the manifest digest matches those fixture bytes.
- Reject changed source bytes, renamed or unlisted packages, duplicate source providers and binary/source conflicts; use a platform that changes bytes on a second read to guard snapshot reuse.
- Cover empty, matching BLAKE3, matching SHA-256, malformed and conflicting advertised runtime hashes, including a manifest-matching archive that conflicts only with the advertised hash.

#### Completion Notes

Added the typed version-1 manifest reader and requested-component verifier, including strict UTF-8 and duplicate-field checks, SHA-256 pin validation, core identity/version checks, and advertised runtime-hash verification. `coreJVM/testOnly dev.bosatsu.ToolchainManifestTest` and `coreJS/testOnly dev.bosatsu.ToolchainManifestTest` both passed (7 tests each).

3. [x] `step_3` Expose verification through the existing file-command lifecycle

Add `core/src/main/scala/dev/bosatsu/tool_command/VerifyToolchainCommand.scala`. Require `--manifest FILE`, `--compiler-jar FILE` and `--out STAMP`; accept optional `--core-library FILE`, repeatable `--core-source FILE`, and optional `--runtime-archive FILE`. Reject simultaneous binary and source core inputs. Read the manifest and invoke the verifier from step 2, passing the running build's `BuildInfo.cRuntimeArchiveHash`. Produce `Output.Basic` with an `ok` stamp only after the entire verification effect succeeds; do not write directly during validation.

Register the command in `ToolCommand.opts`, `FileCommandRunner.supports`, and `BazelWorker.Operation` and its registered list. These are the complete current dispatch sites: `PathModule` already routes supported ordinary commands through the file runner, and one-shot worker dispatch already uses the shared registry. Extend these mechanisms without adding a separate runner or worker allowlist.

Add `cli/src/test/scala/dev/bosatsu/VerifyToolchainCommandTest.scala` using `FileCommandFixtures`, and extend relevant runner/registry tests. Exercise failures with no prior stamp and with a pre-existing sentinel stamp. Include a failed output-publication case followed by success through the same runner, relying on existing `StagedOutputs` rollback and cleanup. Add a Node command smoke test in `cliJS/src/test/scala/dev/bosatsu/Fs2PlatformIOTest.scala` for portable command execution and validation failure preserving an existing stamp. Run focused command, registry, runner and Node tests as this slice lands.

#### Invariants

- Ordinary and worker requests reach the same parser, verifier and output-producing effect.
- No success Output exists before all requested checks succeed.
- JVM publication uses the existing request-owned StagedOutputs lifecycle; validation or publication failure preserves prior outputs and permits later requests.
- The embedded runtime hash comes from generated BuildInfo in production, with no user option that can override it.

#### Property Tests

- For generated component-mismatch cases, compare ordinary runner and worker status, diagnostics and stamp bytes; every failure leaves an absent stamp absent and a previous stamp unchanged.

#### Assertion Tests

- Test required flags, repeatable sources, incompatible selectors, useful component/path diagnostics and silent successful file output.
- Verify registry discovery and ordinary/direct worker parity for compiler-only, source and native requests.
- Run good/bad/good requests with files edited at reused paths to prove verification does not reuse stale results.
- Force publication failure and verify preservation of existing files, cleanup, and a subsequent successful request.
- Execute the command under Node with valid fixtures and a mismatch that preserves the previous stamp.

#### Completion Notes

Added `tool verify-toolchain` using the shared manifest verifier and generated runtime hash, and registered it with the ordinary file runner and one-shot worker. Added JVM command, runner, worker-parity and Node coverage for source selection, success stamps, failure preservation and publication retry. Focused JVM/Node suites passed; `./test_cli.sh` passed, including the candidate worker checks and final eval assertion.

4. [x] `step_4` Qualify exact candidate jars and document the implemented policy

Extend `scripts/test_bazel_worker.py` with local toolchain fixtures and direct-versus-parameter-file verification cases. Compute expected fixture SHA-256 independently with Python hashlib and generate small serialized core fixtures through the existing library/protobuf facilities. Use a local compressed archive and exact source fixtures; no release downloads or publishing credentials are needed.

For every candidate verification invocation, launch the exact jar path supplied as `--compiler-jar`, including negative jar-substitution cases. A runnable substitute can be a copy of the candidate with an additional harmless ZIP entry, giving it a different independently valid SHA-256 while retaining executable behavior. Verify it fails against the original selected manifest. Cover matched compiler-only operation with core/runtime absent, matched binary/source/native requests, independently pinned component substitutions, incorrect embedded core identity and modified source replacements. Compare exit status, diagnostics and output bytes across direct and one-shot worker execution. Extend the harness's failure checks to preserve and compare an already published sentinel stamp as well as checking absent outputs.

Keep advertised-runtime policy coverage from step 2 parameterized over all stamp states, and ensure the candidate harness executes the production BuildInfo wiring. For native success fixtures, use an unstamped development candidate or the actual archive matching a stamped candidate; never silently bypass a nonempty candidate stamp.

Update `docs/design/hermetic-bazel-rules.md` with the exact version-1 schema, bare SHA-256 field format, CLI selectors, requested-component behavior, source snapshot/name rules, binary/source exclusion, and empty/malformed/conflicting embedded-runtime policy. State that the launcher authenticates the manifest pin and jar before execution: self-inspection cannot authenticate an untrusted running jar. Explain that verification is offline and that the full manifest/core digest remains outside ordinary signature-only compile inputs. Mark tracker 0p complete with its implemented tests. Keep release generation/publication assigned to `qualify_cli`.

Run the issue-required Linux checks: `sbt "coreJVM/test; cli/test; coreJS/test; cliJSJS/test; cli/assembly"`, `python3 scripts/test_bazel_worker.py --jar <resolved-candidate-assembly>`, and `./test_cli.sh` as required for CLI changes by `coding_style.md`. Target complete coverage of changed executable branches, addressing gaps within these slices. macOS validation remains PR CI's responsibility.

#### Invariants

- Candidate-jar evidence runs the same artifact whose bytes are supplied for compiler verification.
- A valid substitute's own pin never supersedes the selected manifest's expected digest.
- The documented trust precondition does not claim that an untrusted executable can authenticate itself.
- Tracker completion describes locally implemented and tested behavior and requires no future merge or release event.

#### Property Tests

- None recorded.

#### Assertion Tests

- Run exact-candidate direct/one-shot success and mismatch cases, checking identical statuses, diagnostics and success stamps.
- Reject a runnable compiler substitute, altered same-identity core, manifest-pinned core with incorrect embedded identity, altered runtime and modified source replacement.
- Check compiler-only execution with unused files absent and failure preservation for both absent and existing stamps.
- Complete the named JVM, Scala.js, Node, assembly, candidate-worker and CLI checks.

#### Completion Notes

Added candidate-jar fixtures and direct/one-shot worker parity cases for matched compiler-only, binary-core, source-core, and native-runtime requests, plus mismatched compiler/core/runtime, core identity, and source replacements. Verified failures preserve existing stamps. Documented the version-1 manifest and implemented command policy, marked tracker 0p complete, and updated the CLI help-order assertion for the registered command. The requested sbt sequence passed: core JVM 2,227 passed/0 failed, CLI 182 passed/0 failed, coreJS 2,197 passed/0 failed, cliJSJS 3 passed/0 failed, and assembly built. The candidate-jar harness, `./test_cli.sh`, and `git diff --check` also passed.
