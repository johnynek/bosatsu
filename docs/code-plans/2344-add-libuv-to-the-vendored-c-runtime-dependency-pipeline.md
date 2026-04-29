# Code Plan #2344

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2344` Add libuv to the vendored C runtime dependency pipeline
- Source design doc: `docs/design/2342-document-the-libuv-c-runtime-integration-contract.md`
- Pending steps: `4`
- Completed steps: `0`
- Total steps: `4`

## Summary

Add libuv as a second vendored C runtime dependency, using the merged integration contract's exact version, source archive, hash, source subdirectory, and `libuv-cmake-static` recipe. The final change should let the existing vendored dependency pipeline fetch, build, cache, record, and link libuv through `c_runtime/deps.json`, `CDeps`, and `VendoredDeps`, while preserving current bdwgc behavior and `GC_THREADS` runtime requirements.

## Current State

The checkpoint worktree is effectively pre-implementation for issue #2344. `c_runtime/deps.json` still contains only the bdwgc dependency, `VendoredDeps.runRecipe` still handles only `bdwgc-cmake-static`, and the branch does not contain the planned `runLibuvRecipe` or libuv metadata/link-flag handling. Existing cruntime-focused tests and the required `scripts/test_basic.sh` gate were run successfully on this baseline state, but those results only validate the current pre-libuv branch and do not verify the intended implementation.

## Problem

The previous canonical plan marked the manifest, recipe execution, and metadata/link-flag implementation steps as completed, but implementation evidence shows those changes are absent from the current branch. Treating the remaining work as verification-only would leave issue #2344 unresolved. The plan must therefore return the implementation steps to pending and keep the final verification gate pending until it runs against a branch that actually contains libuv vendoring support. This job remains limited to vendored dependency pipeline behavior and must not start the later libuv event-loop or IO runtime migration work described in the reference document.

## Steps

1. [ ] `step-1` Update Manifest And Parsing Coverage

Update `c_runtime/deps.json` to add the direct `libuv` dependency after `bdwgc` using version `1.52.1`, URI `https://dist.libuv.org/dist/v1.52.1/libuv-v1.52.1.tar.gz`, hash `blake3:433979d1027ec72d546e1e4440e193a9d587f1378a8405299d6f219d23c215b7`, source subdirectory `libuv-v1.52.1`, recipe `libuv-cmake-static`, and no build-time dependency on bdwgc. Keep the manifest schema and existing bdwgc entry intact. Extend focused manifest tests so the checked-in manifest parses and the libuv entry's contract values are pinned.

#### Invariants

- The manifest remains valid `CDeps.Manifest` JSON with `schema_version = 1`.
- The existing bdwgc entry remains threadsafe and continues to request the `bdwgc-cmake-static` recipe with `threadsafe=true`.
- The libuv manifest entry uses the official dist.libuv.org source tarball contract from the reference document, not a GitHub-generated archive or `libuv-v1.52.1-dist.tar.gz`.
- Vendored dependency ordering remains deterministic and topological even though libuv has no direct build dependency on bdwgc.

#### Property Tests

- Add focused property coverage around `CDeps.orderedDependencies` showing independent bdwgc and libuv entries produce stable `bdwgc`, `libuv` order regardless of their manifest input order.

#### Assertion Tests

- Add a case-based test that reads and parses checked-in `c_runtime/deps.json` and asserts the pinned bdwgc and libuv dependency contract values, including libuv version, URI, hash, `source_subdir`, recipe, and absence of dependency edges.
- Update the existing dependency ordering example so libuv no longer incorrectly depends on bdwgc while the sorted topological output remains deterministic.

2. [ ] `step-2` Implement Libuv Recipe Execution

Extend `VendoredDeps.runRecipe` to dispatch `CDeps.LibuvCmakeStatic` to a new libuv CMake recipe helper. Follow the existing bdwgc two-phase shape: configure with `cmake -S <sourceRoot> -B <buildDir> ...`, then build/install with `cmake --build <buildDir> --target install`. Add a small testable helper for libuv configure arguments rather than burying the recipe switches inside the effectful build method. Pass inherited `CFLAGS` through as `-DCMAKE_C_FLAGS=<value>` when present, but do not add bdwgc's Darwin-only `NO_DESC_CATCH_EXCEPTION_RAISE` flag or any Bosatsu `GC_THREADS` flag to the libuv build.

#### Invariants

- Debug profile maps to `-DCMAKE_BUILD_TYPE=Debug`; all other profiles map to `Release`, matching the existing recipe convention.
- The configure args include `-DCMAKE_INSTALL_PREFIX=<prefix>`, `-DLIBUV_BUILD_SHARED=OFF`, `-DBUILD_TESTING=OFF`, `-DLIBUV_BUILD_TESTS=OFF`, and `-DLIBUV_BUILD_BENCH=OFF`.
- Only tracked compiler environment that is already represented in `BuildContext.relevant_env`, specifically `CFLAGS`, is propagated into the CMake configure args for this recipe.
- Unsupported recipe names still fail with the existing clear `unsupported vendored dependency recipe` error.

#### Property Tests

- Add property coverage that generated non-empty safe `CFLAGS` token lists are preserved exactly once in the libuv `-DCMAKE_C_FLAGS=` configure argument and no GC-specific tokens are injected.

#### Assertion Tests

- Add case-based tests for libuv debug and release configure arguments, including all required CMake switches.
- Add a case-based test that no `-DGC_THREADS` or `-DNO_DESC_CATCH_EXCEPTION_RAISE` token appears in libuv configure args unless it was explicitly present in inherited `CFLAGS`.

3. [ ] `step-3` Generalize Metadata Link Flags

Generalize pkg-config parsing/filtering so metadata can collect transitive system link flags for both bdwgc and libuv without leaking each dependency's own static library token into `system_link_flags`. Keep `BuildInputs.linkFlags` behavior aligned with the intended contract: concrete cached static archive paths appear before system flags. Update `systemLinkFlagsFor` to read libuv's installed `lib/pkgconfig/libuv-static.pc`, filter `-L...` path flags and libuv self-link spellings such as `-luv`, `-l:libuv.a`, and path-like `libuv.a` tokens, and preserve platform/system flags reported by libuv. Preserve bdwgc metadata behavior, including empty runtime requirements for libuv and `-DGC_THREADS` requirements only for threadsafe bdwgc.

#### Invariants

- `VendoredDeps.staticLibFileName` continues returning `libuv.a` for `libuv-cmake-static` and `libgc.a` for bdwgc.
- Libuv metadata records exactly the final cached `<prefix>/include` include directory and `<prefix>/lib/libuv.a` static archive path, with empty runtime requirements.
- Bdwgc metadata continues filtering its own self-library and continues producing `GC_THREADS` runtime/generated-code cppflags when `threadsafe=true`.
- Pkg-config parsing preserves field order, expands variables as it does today, removes `-L` flags, removes only configured self-library tokens, and keeps transitive platform flags such as `-pthread`, `-ldl`, `-lrt`, `-lsocket`, or platform equivalents.
- `BuildInputs.linkFlags` emits static archive paths before collected system flags and de-duplicates without changing the dependency-resolution contract.

#### Property Tests

- Add dependency-aware property coverage for pkg-config parsing: generated system flags are preserved in distinct order while generated `-L...` flags and configured self-library tokens are removed.
- Add focused invariant coverage for `BuildInputs.linkFlags` showing resolved static library paths precede collected system flags.

#### Assertion Tests

- Add a representative `libuv-static.pc` case asserting that `-L${libdir}`, `-luv`, `-l:libuv.a`, and path-like self-library tokens are filtered while system flags remain.
- Keep bdwgc pkg-config regression coverage asserting `-lgc` is filtered and `-pthread`/`-ldl` style flags remain.
- Add case-based coverage for `runtimeRequirementsFor` showing libuv has empty runtime requirements while bdwgc keeps both `-DGC_THREADS` entries.

4. [ ] `step-4` Run Required Verification Gate

After the implementation steps are present in the branch, run focused Scala tests while iterating, then run the configured repository gate `scripts/test_basic.sh` before the branch is considered PR-ready. The required-tests timeout for this repo version is 2400 seconds, so the implementation should avoid adding slow network or native-build work to unit tests; native recipe execution should remain covered by argument/metadata parsing tests unless an existing hermetic harness is available. In this environment, focused `sbt` commands should set `protoJVM / Compile / sbtprotoc.ProtocPlugin.autoImport.PB.protocExecutable` to `file("$(command -v protoc)")`, matching `scripts/test_basic.sh`, to avoid protobuf generation hangs seen during the paused round.

#### Invariants

- The branch cannot be submitted until `scripts/test_basic.sh` passes after the libuv vendoring implementation is present.
- Focused tests should not require downloading or compiling libuv from the network; unit tests should exercise manifest parsing, recipe args, static-lib naming, metadata construction/filtering, and link-flag contracts hermetically.
- The implementation remains limited to the vendored dependency pipeline and does not start runtime event-loop, generated C, or IO/Core migration work.

#### Property Tests

- Run the focused property suites in `core/src/test/scala/dev/bosatsu/cruntime/CDepsTest.scala` and `core/src/test/scala/dev/bosatsu/cruntime/VendoredDepsTest.scala` as part of local iteration.

#### Assertion Tests

- Run `sbt -batch "set protoJVM / Compile / sbtprotoc.ProtocPlugin.autoImport.PB.protocExecutable := file(\"$(command -v protoc)\")" "coreJVM/testOnly dev.bosatsu.cruntime.CDepsTest dev.bosatsu.cruntime.VendoredDepsTest -- --log=failure"` or the repo-equivalent focused test command during implementation.
- Run `scripts/test_basic.sh` as the final required verification gate within the configured 2400 second timeout.
