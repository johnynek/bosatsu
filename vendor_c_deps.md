# Vendor C Dependencies Plan

## Summary

Bosatsu should stop relying on host-installed `bdw-gc` today, and should be able to add `libuv` soon, without requiring either library to be globally installed on the local machine. The right model is:

1. each Bosatsu runtime version selects a dependency manifest;
2. each dependency source archive is downloaded and hash-verified into `.bosatsuc`;
3. each dependency is built into a Bosatsu-managed local prefix under `.bosatsuc`;
4. built third-party artifacts are reused across Bosatsu compiler/runtime bumps whenever the dependency source identity and build context are unchanged;
5. Bosatsu-generated C is linked against the local static artifacts, not a globally installed dynamic library.

This document proposes an implementation path that starts with vendored `libgc`, preserves the current runtime install flow as much as possible, and leaves a clean path for adding `libuv`.

## Current state

Today the Bosatsu C runtime already has some useful building blocks:

1. `bosatsu c-runtime install` can download a hash-pinned runtime archive into `.bosatsuc`, unpack it, and build locally.
2. the install step writes a `cc_conf.json` file consumed later by C compilation/linking.
3. Bosatsu already stores per-repo compiler/runtime state under `.bosatsuc`.

The main problem is that `bdw-gc` is still discovered from the host machine:

1. via `pkg-config bdw-gc`, or
2. via hard-coded Homebrew/MacPorts/system fallbacks, or
3. via `-lgc` expecting a system install.

That means the Bosatsu runtime is still coupled to whatever the machine has installed. The same problem would recur for `libuv`.

## Goals

1. Remove the requirement that `bdw-gc` or `libuv` be globally installed.
2. Reuse previously built third-party dependencies across Bosatsu version bumps.
3. Keep Bosatsu version selection in control of which dependency sources are acceptable.
4. Support macOS and Linux in the initial implementation.
5. Keep the design valid for Windows, even if Windows is not fully shipped in phase 1.
6. Link Bosatsu-generated executables against Bosatsu-managed static artifacts under `.bosatsuc`.
7. Preserve reproducibility by pinning source archives by hash.
8. Preserve room for dependency-specific build requirements and future transitive dependencies.

## Non-goals

1. This plan does not attempt to make the final Bosatsu executable fully static against the system C runtime.
2. This plan does not require Bosatsu to bundle prebuilt binaries for every platform.
3. This plan does not require phase 1 to ship `libuv`; phase 1 only needs to avoid painting us into a corner.
4. This plan does not require immediate end-to-end Windows support for the Bosatsu runtime itself.

## Design principles

### 1) Bosatsu version selects dependency manifest; it does not key third-party build reuse

Bosatsu version should determine which dependency descriptors are valid, but the reuse key for built `libgc.a` or `libuv.a` should not include the Bosatsu compiler/runtime version.

If Bosatsu `X` and Bosatsu `Y` both depend on the same `libgc` release tarball with the same hash, and the local build context is the same, we should reuse the same previously built artifact.

### 2) Separate source identity from build identity

We need two levels of cache keys:

1. source identity:
   - dependency name
   - dependency version
   - verified source archive hash
2. build identity:
   - source identity
   - target OS
   - target architecture
   - toolchain family and version
   - relevant recipe options
   - build profile
   - recipe version
   - transitive dependency build identities, if any

This is the minimum needed to safely reuse artifacts without conflating incompatible builds.

### 3) Bosatsu owns the local install prefix and final linkage

Every vendored dependency build should install into a Bosatsu-managed prefix under `.bosatsuc`, and `cc_conf.json` should then point at those local include and library paths directly.

We should not depend on:

1. host `pkg-config`;
2. host-wide library search paths;
3. global `brew`/`apt`/`vcpkg` state;
4. dynamic lookup of `libgc` or `libuv` at executable run time.

### 4) Keep dependency recipes explicit

Different C libraries have different build systems and different platform quirks. Bosatsu should represent that with explicit dependency recipes rather than pretending every library is built the same way.

That recipe layer is also where we record:

1. how to configure the library;
2. which artifacts to expect after install;
3. which additional system link flags remain necessary;
4. which compile defines Bosatsu runtime code must honor when using that library.

## Dependency manifest model

The Bosatsu runtime archive should contain a checked-in dependency manifest, for example `c_runtime/deps.json` or `c_runtime/deps.toml`.

At minimum each dependency entry should carry the conceptual tuple already discussed:

1. `name`
2. `version`
3. `uri`
4. `hash`

The actual serialized model should be slightly richer so Bosatsu can evolve cleanly. A good shape is:

```json
{
  "schema_version": 1,
  "recipe_version": 1,
  "dependencies": [
    {
      "name": "bdwgc",
      "version": "8.2.8",
      "uris": [
        "https://github.com/bdwgc/bdwgc/releases/download/v8.2.8/gc-8.2.8.tar.gz"
      ],
      "hash": "blake3:...",
      "source_subdir": "gc-8.2.8",
      "recipe": "bdwgc-cmake-static",
      "options": {
        "threadsafe": true
      }
    }
  ]
}
```

Notes:

1. `uris` is preferable to a single `uri` so we can add mirrors later without changing the model.
2. `recipe_version` is a Bosatsu-owned invalidation knob. If our build recipe changes, old cached builds should stop matching even if the source archive is unchanged.
3. `recipe` identifies Bosatsu-owned logic for configuring/building/installing that dependency.
4. `options` lets the manifest pin Bosatsu-relevant features, such as “thread support must be enabled”.

### Where the manifest lives

The manifest should live inside the Bosatsu C runtime source tree and therefore inside the runtime archive that `bosatsu c-runtime install` downloads or consumes locally.

That means:

1. the Bosatsu release process produces a runtime archive that contains:
   - the runtime source files
   - the Bosatsu runtime `Makefile` or later build files
   - the dependency manifest
2. `bosatsu c-runtime install` unpacks the archive;
3. Bosatsu reads the manifest from the unpacked runtime tree before building Bosatsu runtime artifacts.

This is the right placement because:

1. the manifest is part of the Bosatsu runtime definition for that release;
2. different Bosatsu runtime versions can pin different dependency sets or options;
3. the dependency cache can still be shared across Bosatsu versions because the build reuse key is not the Bosatsu version itself.

In practice, Bosatsu version selects the manifest, but previously built dependencies are reused if the manifest entries resolve to the same source identity and build identity.

## Cache layout under `.bosatsuc`

The cache should distinguish:

1. downloaded archives;
2. extracted source trees;
3. built/install prefixes;
4. runtime-version-specific Bosatsu artifacts.

Proposed layout:

```text
.bosatsuc/
  c_deps/
    archives/
      <name>/
        <version>/
          <hash>/
            source.tar.gz
    sources/
      <name>/
        <version>/
          <hash>/
            <source tree>
    builds/
      <build-key>/
        prefix/
          include/
          lib/
        metadata.json
        build.log
        success
    locks/
      <build-key>.lock
  <runtime-id>/
    include/
    lib/
    cc_conf.json
```

Where:

1. `c_deps/` is shared across Bosatsu runtime/compiler versions;
2. `<runtime-id>/` remains Bosatsu-version-specific and contains Bosatsu’s own runtime artifacts such as `bosatsu_platform.a` and the generated `cc_conf.json`.

This split is important because the current installer deletes the versioned runtime directory on reinstall. Third-party dep caches should live outside that per-runtime directory so they survive Bosatsu bumps and reinstalls.

## Build identity and reuse rules

### Reuse key

A dependency build should be reused if and only if all of the following match:

1. dependency name;
2. dependency version;
3. dependency source archive hash;
4. Bosatsu recipe identifier;
5. Bosatsu recipe version;
6. target OS;
7. target architecture;
8. toolchain family;
9. toolchain version fingerprint;
10. build profile;
11. relevant dependency build options;
12. transitive dependency build keys, if any.

### Toolchain fingerprint

To be conservative, the toolchain fingerprint should include enough information to avoid ABI mismatches. For phase 1 that can be:

1. resolved compiler path;
2. compiler `--version` output;
3. archiver identity if we invoke a non-default `ar`/`lib`;
4. environment knobs that materially affect output.

This means a Bosatsu version bump will not invalidate the cache, but an Xcode/Clang upgrade or GCC family switch usually will.

### Build profile

Dependency builds should keep `release` and `debug` separate. Even if the API/ABI is nominally compatible, debug-oriented builds often carry different flags, assertions, and symbol/debug settings.

## Build metadata contract

Each successful dependency build should write a Bosatsu-owned `metadata.json` next to the installed prefix. Bosatsu should consume this metadata rather than rediscovering the library from the host.

Suggested shape:

```json
{
  "schema_version": 1,
  "name": "bdwgc",
  "version": "8.2.8",
  "build_key": "...",
  "target": {
    "os": "macos",
    "arch": "arm64",
    "toolchain_family": "clang",
    "toolchain_version": "Apple clang 16.0.0 ..."
  },
  "prefix": "/abs/path/.bosatsuc/c_deps/builds/<build-key>/prefix",
  "include_dirs": [
    "/abs/path/.bosatsuc/c_deps/builds/<build-key>/prefix/include"
  ],
  "static_libs": [
    "/abs/path/.bosatsuc/c_deps/builds/<build-key>/prefix/lib/libgc.a"
  ],
  "system_link_flags": [
    "-pthread"
  ],
  "runtime_requirements": {
    "bosatsu_runtime_cppflags": [],
    "generated_c_cppflags": []
  }
}
```

Important separation:

1. `static_libs` are the vendored artifacts;
2. `system_link_flags` are non-vendored OS/toolchain libraries or frameworks still required at final link;
3. `runtime_requirements` are requirements Bosatsu runtime code must honor when using that dependency.

This separation becomes important for `libuv`, because even if `libuv.a` is vendored, some platform system libraries/frameworks may still need to be linked.

## Platform support

### Common platform abstraction

Bosatsu should model platform support at three layers:

1. archive extraction;
2. dependency recipe execution;
3. final runtime compile/link configuration.

Phase 1 can still use the existing shell-heavy flow on macOS/Linux, but the dependency subsystem should not hard-code Unix assumptions into its API. In particular:

1. dependency recipes should be keyed by platform and toolchain family;
2. archive extraction should not permanently assume `tar` and `unzip` are present;
3. recipe execution should not permanently assume GNU `make`;
4. final metadata should be platform-neutral even if the phase 1 implementation is Unix-only.

### macOS

macOS phase 1 requirements:

1. support Apple Clang builds;
2. build per-architecture artifacts rather than universal libraries by default;
3. prefer vendored static archives under `.bosatsuc`;
4. avoid relying on Homebrew installation paths for `bdw-gc` or `libuv`.

Implications:

1. `arm64` and `x86_64` should be separate build keys;
2. any remaining system frameworks needed by a vendored dependency should be recorded in Bosatsu metadata as final link flags;
3. the design should not depend on `brew` at all once vendoring is in place.

### Linux

Linux phase 1 requirements:

1. support common GCC/Clang toolchains;
2. keep vendored static archives under `.bosatsuc`;
3. allow final link metadata to include required system libraries such as threading or dynamic loader flags when applicable.

Implications:

1. build keys should include compiler family/version and target architecture;
2. the recipe layer should be allowed to record extra system flags instead of assuming the static archive is fully self-contained;
3. no `apt` or distro package should be required for vendored `libgc` or vendored `libuv`, except general build tools.

### Windows

Windows is an explicit design target, but not a phase 1 deliverable. The design should make it possible without a second architectural rewrite.

Requirements:

1. do not force Unix-only extraction/build APIs into the dependency subsystem;
2. support multiple toolchain families in principle:
   - MSVC
   - clang-cl
   - MinGW
3. keep toolchain family in the build key;
4. allow dependency metadata to emit `.lib` paths and Windows system libraries instead of `.a` plus Unix-style flags.

Important caveat:

1. the current Bosatsu runtime install path shells out to `make`, `tar`, and `unzip`;
2. that is not a reliable long-term Windows foundation;
3. phase 1 can keep that for macOS/Linux, but the vendored dependency subsystem should be designed so Bosatsu runtime build itself can later move to a CMake-driven or Bosatsu-owned build path.

## Library-specific requirements

### `libgc` requirements

Bosatsu’s current runtime already depends on Boehm GC, so `libgc` is the first dependency to vendor.

Per upstream `bdwgc` documentation:

1. CMake is the recommended build path;
2. the collector is built with thread support enabled by default unless explicitly disabled;
3. on Windows/MSVC, `libatomic_ops` may still be needed because compiler intrinsics support differs there.

Bosatsu requirements for vendored `libgc`:

1. build a static library;
2. ensure thread support is not disabled;
3. record the installed headers and static archive in dep metadata;
4. record any required system link flags in dep metadata;
5. do not rediscover `libgc` via host `pkg-config` during runtime install.

### `libgc` and threading

There are two separate concerns:

1. how `libgc` itself is built;
2. how Bosatsu runtime code is compiled when it uses `gc.h`.

For phase 1:

1. current Bosatsu runtime can keep its existing behavior, because it is not yet using `libuv`;
2. vendored `libgc` should still be built with thread-safe support enabled, since that is the correct base for the upcoming `libuv` backend;
3. phase 1 does not need to define `GC_THREADS` everywhere immediately if the current runtime backend does not require it.

For the future `libuv` backend:

1. Bosatsu runtime translation units that include `gc.h` and participate in multithreaded execution should define `GC_THREADS`;
2. Bosatsu-generated C that includes `gc.h` should also receive the relevant compile define in the libuv-enabled runtime configuration;
3. `GC_allow_register_threads()` and explicit worker thread registration/unregistration belong to Bosatsu runtime code, not to the vendored dependency recipe.

### `libgc` transitive dependencies

The vendored dependency system must be able to model transitive dependencies because `bdwgc` can require `libatomic_ops` on some toolchains, especially Windows/MSVC.

We should therefore not hard-code an assumption that every vendored dependency is leaf-only.

Phase 1 may start with the common macOS/Linux case where `libgc` builds without a separate vendored `libatomic_ops`, but the recipe/data model should allow:

1. `bdwgc` depending on `libatomic_ops`;
2. the `bdwgc` build key depending on the `libatomic_ops` build key.

### `libuv` requirements

`libuv` is not yet a Bosatsu runtime dependency, but the vendoring design should support it cleanly.

Per upstream `libuv` documentation:

1. it has a global threadpool;
2. `uv_queue_work()` runs work callbacks on threadpool threads;
3. CMake is the supported build system on Windows and a supported build system on Unix-like platforms.

Bosatsu vendoring requirements for `libuv`:

1. build a static library under `.bosatsuc`;
2. disable or skip tests/benchmarks/docs in vendored builds where appropriate;
3. record installed headers, static library path, and any remaining system link flags in dep metadata;
4. avoid host discovery through system `pkg-config`.

### `libuv` and `libgc` interaction

The main `libuv`/`libgc` integration constraint is not a `libuv` build flag. It is a Bosatsu runtime contract:

1. `libuv` worker threads are third-party-created threads from `libgc`’s perspective;
2. any such worker thread that touches GC-managed heap values must be explicitly registered with Boehm;
3. those threads must be unregistered before returning to `libuv`;
4. Bosatsu runtime code using the libuv backend should compile with `GC_THREADS`.

That means the design must distinguish:

1. dependency build metadata for `libuv`;
2. dependency build metadata for `libgc`;
3. runtime backend requirements for “Bosatsu with libuv”.

We should not encode `GC_THREADS` as a blanket `libgc` client requirement for every backend. It is specifically a requirement of Bosatsu’s future multithreaded/libuv runtime mode.

## Proposed integration with the current Bosatsu runtime install flow

We can integrate vendored dependencies incrementally without rewriting the whole pipeline first.

### Phase 1 integration shape

1. `bosatsu c-runtime install` downloads and unpacks the Bosatsu runtime archive as today.
2. before invoking `make install`, Bosatsu reads the vendored dependency manifest from the runtime source tree.
3. Bosatsu ensures all declared dependencies are present in `.bosatsuc/c_deps/...`.
4. Bosatsu aggregates dependency metadata into final include and link flags.
5. Bosatsu invokes the Bosatsu runtime build with those exact flags.
6. `install.py` writes `cc_conf.json` using the passed-in local include/library paths instead of rediscovering `libgc` from the host.

This preserves most of the current flow while removing host dependency discovery.

### Runtime build inputs

The Bosatsu runtime build step should receive:

1. dependency include flags derived from vendored metadata;
2. dependency library paths derived from vendored metadata;
3. dependency-required system link flags derived from vendored metadata;
4. runtime backend-specific compile defines such as future `GC_THREADS`.

### `cc_conf.json`

The easiest incremental step is to keep the current `CcConf` schema and just change its contents.

That means:

1. `iflags` should point at `.bosatsuc/<runtime-id>/include` and vendored dependency include prefixes;
2. `libs` should start with Bosatsu’s own `bosatsu_platform.a`, then vendored dependency archives in dependency order, then required system link flags;
3. no later C compile step should need to rediscover `libgc` or `libuv`.

### Bosatsu runtime install script changes

`c_runtime/install.py` should become dumber, not smarter.

Instead of trying to rediscover `bdw-gc` itself, it should:

1. accept the exact compile/link inputs passed by the Bosatsu installer;
2. copy Bosatsu runtime headers and archive into `.bosatsuc/<runtime-id>/`;
3. write those exact paths/flags to `cc_conf.json`;
4. avoid host `pkg-config` fallback logic for official vendored installs.

We may keep a developer-only fallback path temporarily, but the supported install path should stop depending on host discovery.

## Build recipes

### Bosatsu-owned recipe abstraction

Bosatsu should add explicit recipe implementations, for example:

1. `bdwgc-cmake-static`
2. `libuv-cmake-static`

Each recipe is responsible for:

1. taking a verified source tree and a target build context;
2. configuring/building/installing into a temp prefix;
3. validating expected outputs;
4. writing Bosatsu metadata;
5. atomically moving the successful prefix/metadata into the shared build cache.

### How dependency builds are invoked

Dependency build invocation should be specified in two layers:

1. the manifest chooses a Bosatsu-owned recipe and recipe options;
2. Bosatsu code implements that recipe and executes the actual commands.

The manifest should not embed arbitrary shell commands such as raw `./configure`, `cmake`, or `make` command lines.

Instead, the manifest should say things like:

1. build dependency `bdwgc` with recipe `bdwgc-cmake-static`;
2. enable thread-safe collector support;
3. use these source hashes and these accepted source URIs.

Then Bosatsu-owned installer code translates that recipe into the concrete build steps for the current platform/toolchain.

For example:

1. the `bdwgc-cmake-static` recipe might invoke CMake configure/build/install steps;
2. the `libuv-cmake-static` recipe might invoke a different set of CMake flags and validation rules;
3. later Windows-specific variants can reuse the same recipe identity but branch internally by toolchain family if needed.

This split keeps the manifest:

1. declarative;
2. stable across platforms;
3. safe to evolve without shipping arbitrary command execution in release metadata.

It also keeps platform-specific command construction in Bosatsu code where it can be tested and versioned.

### Where the recipe execution belongs

The dependency build orchestration should not primarily live in `c_runtime/install.py`.

Recommended responsibility split:

1. Bosatsu installer code in Scala:
   - reads the dependency manifest
   - computes build keys
   - fetches and verifies archives
   - checks and populates the shared dependency cache
   - executes dependency recipes
   - gathers dependency metadata
   - invokes the Bosatsu runtime build with the resulting include/link inputs
2. `c_runtime/install.py`:
   - copies Bosatsu runtime outputs into `.bosatsuc/<runtime-id>/`
   - writes `cc_conf.json` from the exact arguments it is given
   - does not rediscover or build third-party dependencies in the supported path

So the intended flow is:

1. `bosatsu c-runtime install` unpacks the Bosatsu runtime archive;
2. Scala-side installer code reads `c_runtime/deps.json`;
3. Scala-side installer code ensures each dependency build exists in `.bosatsuc/c_deps/...`;
4. Scala-side installer code invokes the Bosatsu runtime build using those vendored prefixes;
5. `install.py` records the final Bosatsu runtime install and the final `cc_conf.json`.

This keeps `install.py` narrow and avoids pushing dependency resolution logic into a script that currently only packages the final runtime outputs.

### Atomicity and locking

Dependency builds should be safe under concurrent Bosatsu invocations.

Recommended rules:

1. build into a temp directory outside the final cache key path;
2. acquire a per-build-key lock before publishing;
3. publish by atomic rename/move into the final `<build-key>/` directory;
4. reuse a cached build only if `metadata.json` and a success marker are both present and valid.

## Implementation plan

### Phase 1: manifest, metadata, and cache-key foundation

Status: completed on 2026-04-04.

Deliverables:

1. add a vendored dependency manifest to `c_runtime/`;
2. add Bosatsu-side manifest parsing, metadata encoding, and build-key computation under `dev.bosatsu.cruntime`;
3. seed the manifest with vendored `bdwgc`;
4. add unit tests for manifest parsing, build-key stability, and metadata round-tripping.

Delivered in this phase:

1. `c_runtime/deps.json` now exists and is part of the shipped runtime tree;
2. `core/src/main/scala/dev/bosatsu/cruntime/CDeps.scala` now owns the manifest schema, metadata schema, toolchain normalization helpers, and build-key hashing;
3. `core/src/test/scala/dev/bosatsu/cruntime/CDepsTest.scala` covers the pure data-contract pieces.

### Phase 2: vendored `libgc` install integration on macOS/Linux

Concrete file areas likely touched:

1. `core/src/main/scala/dev/bosatsu/cruntime/Command.scala`
2. new `core/src/main/scala/dev/bosatsu/cruntime/...` support files for manifest parsing, build keys, recipes, and metadata
3. `c_runtime/Makefile`
4. `c_runtime/install.py`
5. new `c_runtime/deps.json` or `c_runtime/deps.toml`
6. tests for installer/cache behavior

Recommended sequence:

1. implement archive fetch and source extraction reuse;
2. implement cache layout under `.bosatsuc/c_deps/`;
3. implement the `bdwgc-cmake-static` recipe and metadata output;
4. plumb vendored metadata into the runtime build and `cc_conf.json`;
5. delete or bypass host `bdw-gc` discovery in the supported installer path;
6. add install-path tests.

### Phase 3: multi-dependency and `libuv`-ready recipe plumbing

Deliverables:

1. extend dependency resolution to multiple dependencies and dependency order;
2. generalize the recipe layer so `libuv-cmake-static` is a straightforward next entry;
3. preserve runtime-backend-specific requirements so a future libuv-enabled runtime can emit `GC_THREADS` and other backend-specific compile flags cleanly;
4. allow dependency metadata to carry remaining system link flags.

This phase does not require the libuv backend itself to land at the same time, but it should make adding it straightforward.

### Follow-up work outside this issue

The dependency vendoring issue stops at shared-cache third-party dependency management. The following remain follow-up items after this issue lands:

1. adding the actual `libuv` dependency entry and recipe implementation;
2. building the libuv-backed Bosatsu runtime backend itself;
3. replacing Unix-specific runtime build assumptions for full Windows runtime support.

## Testing strategy

### Unit and metadata tests

1. manifest parsing tests;
2. build-key stability tests;
3. cache hit/miss tests showing Bosatsu version bumps do not invalidate vendored dep reuse;
4. metadata serialization/deserialization tests;
5. invalid cache entry handling tests.

### Installer tests

1. first install downloads, builds, and publishes vendored `libgc`;
2. second install with same manifest/toolchain/profile reuses cached `libgc`;
3. changing Bosatsu runtime version but not dependency source/build context still reuses cached `libgc`;
4. changing dependency hash causes a rebuild;
5. changing recipe version causes a rebuild;
6. changing toolchain fingerprint causes a rebuild.

### Linkage tests

1. verify `cc_conf.json` points at `.bosatsuc` paths rather than host `pkg-config` output;
2. verify a generated executable links successfully without host-installed `libgc`;
3. once `libuv` is added, verify equivalent behavior for vendored `libuv`.

### Platform tests

1. macOS CI path for vendored `libgc`;
2. Linux CI path for vendored `libgc`;
3. when Windows work starts, add toolchain-specific CI separately rather than assuming Unix tests are enough.

## Acceptance criteria

1. `bosatsu c-runtime install` no longer requires host-installed `bdw-gc`.
2. `cc_conf.json` no longer records host-discovered `bdw-gc` include/library paths for the supported path.
3. third-party dependency builds are reused across Bosatsu compiler/runtime version bumps when the dependency source identity and build context match.
4. a changed dependency source hash or build context causes a rebuild.
5. the dependency subsystem can represent multiple dependencies, dependency order, and future transitive dependencies.
6. the design cleanly accommodates future vendored `libuv`.
7. the design leaves room for Windows without requiring another architectural rewrite.

## Risks and mitigations

1. Risk: we accidentally key the shared dependency cache too loosely and reuse incompatible artifacts.
   Mitigation: include toolchain family/version, target arch, build profile, recipe version, and dependency options in the build key.

2. Risk: vendored static archives still need non-vendored system link flags and builds fail late.
   Mitigation: make system link flags an explicit part of dep metadata instead of assuming the archive is fully self-contained.

3. Risk: `libgc` recipe needs `libatomic_ops` on some platforms/toolchains.
   Mitigation: model transitive dependencies in the recipe/data model from the start, even if phase 1 common Unix builds do not need them.

4. Risk: current installer remains Unix-specific.
   Mitigation: keep phase 1 scoped to macOS/Linux, but define the dependency subsystem in a platform-neutral way and plan explicit Windows enablement later.

5. Risk: recipe logic drifts from upstream-supported build modes.
   Mitigation: use upstream-recommended build systems where available, especially CMake for both `bdwgc` and `libuv`.

## Recommended first implementation cut

The first practical cut should be:

1. add `c_runtime/deps.json` with a single vendored `bdwgc` entry;
2. add `.bosatsuc/c_deps/` shared cache layout;
3. implement `bdwgc-cmake-static` recipe for macOS/Linux;
4. change `bosatsu c-runtime install` to build/reuse vendored `libgc`;
5. change `install.py` and `cc_conf.json` generation to use the vendored prefix directly;
6. keep the rest of the Bosatsu C compile/link pipeline unchanged.

That gives immediate value for the current dependency, solves the repeated-rebuild problem across Bosatsu bumps, and leaves the door open for `libuv` without forcing a second redesign.
