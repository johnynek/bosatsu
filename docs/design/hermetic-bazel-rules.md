# Hermetic Bazel rules for Bosatsu

Ship the monorepo consumer path first. A repository can build a native executable entirely from `bosatsu_packages` targets or mix those groups with pinned OSS `bosatsu_import` targets. Its local packages need no library name, version, release history or publishing destination.

## Delivery boundary

| Initial monorepo rules | Optional producer phase |
| --- | --- |
| `bosatsu_packages`, `bosatsu_binary`, `bosatsu_test` and `bosatsu_import`. Include compiler/JRE and native toolchains, strict interface checking, worker support, and Gazelle for groups, imports, mains and test labels. | Add source `bosatsu_library` to aggregate groups, choose exports and optionally publish versioned artifacts. Preserve the release/history design below as a separate phase. |
| Source groups provide signatures, typed implementations and external symbol requirements. Imported libraries provide exported signatures, a verified implementation graph, scoped identities and native bindings. | A future source library provides the same downstream interface as an import. Only the producer needs full-library assembly, the local library version marker, predecessor checks and upload support. |

The native build must succeed after prefetch with an empty home, network-disabled actions and only declared tools/inputs. The packages-only fixture must also work with an empty set of library imports and no library lock. An importing monorepo needs a lock for those imports. Package compilation needs the pinned deploy jar and an execution-platform JRE. Native tests and programs additionally need the matching runtime archive and target C toolchain. Both artifacts support catalog pins and local files. The module examples below use placeholders for pins. [Local toolchain inputs](#local-toolchain-inputs) · [PR and rule delivery order](#delivery-order-and-acceptance-tests)

## Packages-only BUILD example

```starlark
# MODULE.bazel
bazel_dep(name = "rules_bosatsu", version = "<rules release>")
bosatsu = use_extension("@rules_bosatsu//:extensions.bzl", "bosatsu")
bosatsu.toolchain(version = "<compiler release>")
use_repo(bosatsu, "bosatsu_toolchains")
register_toolchains("@bosatsu_toolchains//:compiler")
```

The registration above suffices for `bazel build //app:api`. For `//app:demo`, also register `@bosatsu_toolchains//:runtime` and the pinned target C toolchain/sysroot described below. In this fixture, `Prog.bosatsu` supplies the ordinary source package `Bosatsu/Prog`. The binary toolchain supplies the runtime implementation of its standard C externals. `Demo/Api` imports `Demo/Types` and declares `exposes Demo/Types`. `Demo/Main` imports `Demo/Api` and `Bosatsu/Prog`.

```starlark
# app/BUILD.bazel
load("@rules_bosatsu//:defs.bzl", "bosatsu_binary", "bosatsu_packages")

bosatsu_packages(name = "prog", srcs = ["Prog.bosatsu"])
bosatsu_packages(name = "types", srcs = ["Types.bosatsu"])

bosatsu_packages(
    name = "api",
    srcs = ["Api.bosatsu"],
    deps = [":types"],
)

bosatsu_packages(
    name = "main",
    srcs = ["Main.bosatsu"],
    deps = [":api", ":prog", ":types"],
)

bosatsu_binary(
    name = "demo",
    packages = ":main",
    main_package = "Demo/Main",
)
```

`bazel build //app:demo` should produce the executable and `bazel run //app:demo` should run it. The example uses one file in each group to show the edges. `srcs` can contain several packages. To use a published standard library, change the `:main` deps to `[":api", "@core_alpha//:lib", ":types"]`. The same binary rule handles the mixed graph. Neither path assembles a local `.bosatsu_lib`.

### Adding the first import

Keep the direct import declarations in one checked-in file, `bosatsu-imports.json`. The `name` field is the Bazel repository name. The library name, version, URIs and SHA-256 identify the published bytes. For the mixed example the file contains one root.

```json
{
  "schema_version": 1,
  "roots": [{
    "name": "core_alpha",
    "library_name": "core_alpha",
    "version": "9.0.1",
    "uris": ["https://github.com/johnynek/bosatsu/releases/download/v0.0.69/core_alpha-v9.0.1.bosatsu_lib"],
    "sha256": "<verified SHA-256 of the release>"
  }]
}
```

With the compiler registered, run this from the checkout to create the first lock. Run the same command after editing a root pin.

```sh
bazel run @bosatsu_toolchains//:lock-libraries -- --imports bosatsu-imports.json --out bosatsu-deps.lock.json
```

The compiler repository supplies this launcher with the pinned jar and JRE. It resolves relative paths against `BUILD_WORKSPACE_DIRECTORY` and invokes `tool lock-libraries`. The updater reads the roots manifest directly, validates the downloaded closure and atomically replaces the lock after success. It can run with an absent or stale lock. Commit both files.

Configure the separate import extension for builds. Export the two file labels from the root BUILD package. The extension derives the repository names from the roots manifest. `use_repo` brings a name into scope.

```starlark
libraries = use_extension("@rules_bosatsu//:extensions.bzl", "bosatsu_import")
libraries.manifest(
    roots = "//:bosatsu-imports.json",
    lock = "//:bosatsu-deps.lock.json",
)
use_repo(libraries, "core_alpha")
```

An import build checks a canonical digest of the declared roots recorded in the lock and reports the update command if the lock is absent or stale. It never updates the checkout. Keep the compiler extension and the updater's dependencies independent of the import extension so the update command also works in a checkout with import configuration already present. The example release URI and Bosatsu digest come from the existing project config. [Core library config](../../test_workspace/core_alpha_conf.json) · [Bazel module extensions](https://bazel.build/external/extension)

| Target | Contract |
| --- | --- |
| `bosatsu_packages` | Compile a nonempty `srcs` list into one `.bosatsu_package` and one `.bosatsig`. Headers supply package names. `deps` must exactly supply the group's imports and required exposed interfaces. Bazel enables `--error-unused-interfaces`. Predef is implicit. The rule requires only the JVM compiler toolchain. |
| `bosatsu_binary` | `packages` names a root group. `main_package` selects a Bosatsu package in that group with a valid `Bosatsu/Prog::Main`. Link local implementations and, if present, verified imported library implementations. |
| `bosatsu_test` | Required `packages` is a single label providing `BosatsuPackagesInfo`. Run the suites in that group's compiled implementation. If none of its packages contains a test entry, building the runner fails. |
| `bosatsu_import` | A module extension downloads a pinned full `.bosatsu_lib` and exposes `@name//:lib`. A BUILD rule supports an existing download label or vendored file. Verify name/version/digests, index the exported packages and their exposures, and produce compile and codegen views of the same artifact. |
| `bosatsu_library` (later) | `packages` lists root groups. Follow group edges up to library boundaries. `exports` selects package names. Optional `version`, `uris` and `previous` configure the same target for release and publication. |

### Native binary action graph

1. Compile each reachable group from sources and the precise interface inputs below. Emit the typed implementations and signatures. All local groups belong to one binary root scope. Use the existing `CompilationSource` for a typed `PackageMap` in the all-local case. Resolve the selected root main, materialize the effective Predef implementation once, and tree-shake reachable values. [Package-map backend](../../core/src/main/scala/dev/bosatsu/codegen/CompilationSource.scala#L44) · [Main selection](../../core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala#L756) · [Prog source](../../test_workspace/Prog.bosatsu)
2. For a mixed graph, keep the loose local scope and add verified library scopes keyed by name/version. Preserve private dependencies and library-specific name resolution when generating C. The current file-based runtime helper flattens library implementations into a package map. The new emitter needs a local root plus imported scopes; it must not flatten two private versions into the same namespace. The root is an in-memory compilation scope and needs no serialized library or fake version. [Current file path](../../core/src/main/scala/dev/bosatsu/tool_command/RuntimeCommandSupport.scala) · [Library namespace](../../core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala)
3. Have the pinned jar emit C on the execution platform. Have Bazel's target C toolchain compile/link that C with the Bosatsu runtime, source-built GC and libuv, and any declared native extern providers. No C compiler discovery, network access or `.bosatsuc` install is part of these actions. The native inputs and acceptance gates apply equally when the imported library graph is empty.

**Compiler evidence.** A temporary fixture used the current JVM CLI to compile `Bosatsu/Prog`, `Demo/Types`, `Demo/Api` and `Demo/Main` separately with `tool check`. Main imported values from Api and compiled with the Prog, Api and Types signature files. Passing just the four resulting implementation bundles to today's C transpiler failed during topological sorting. Supplying the compiler's built-in compiled Predef alongside them produced C that linked and exited with the expected code, 7. Transpiling the four sources together also produced an executable with exit code 7. Neither path used a `.bosatsu_lib`. The smoke test used host Clang and installed static GC/uv archives. The planned Bazel path must materialize Predef automatically and repeat the test with pinned tools and source-built archives.

## Testing private functions in the same package

Declare the group and its test as two ordinary Bazel targets. `bosatsu_packages` compiles the sources. `bosatsu_test` takes one group label and builds a native runner from the group's typed values. Only the test rule needs a test executable, test data and attributes such as `size` and `timeout`. [Bazel executable and test rules](https://bazel.build/extending/rules#executable_rules_and_test_rules)

```starlark
# math/BUILD.bazel
load("@rules_bosatsu//:defs.bzl", "bosatsu_packages", "bosatsu_test")

bosatsu_packages(
    name = "math",
    srcs = ["Math.bosatsu"],
)

bosatsu_test(
    name = "math_test",
    packages = ":math",
)
```

```bosatsu
package Demo/Math

export answer

answer = 42

def twice(n: Int) -> Int:
  add(n, n)

checks = Assertion(eq_Int(twice(3), 6), "twice a private value")
```

Consumers depend on `//math:math`. Run `bazel test //math:math_test` for the suite, or `bazel test //...` to select it along with other tests. The assertion lives inside `Demo/Math`, where it can call `twice`. Only `answer` appears in the exported interface. The test rule runs the compiled `checks` binding.

1. **Compile once per configuration.** The group action type-checks all sources and writes the implementation bundle and public signature. Both binary and test C emission read that same bundle. Preserve private test entries, their discovery order and reachable helpers through serialization and optimization. Other groups depend on the signature. Starting from the program main lets binary C emission drop unreachable test code.
2. **Select suites from the direct group.** Proposed `tool emit-c --test-packages math.bosatsu_package --runtime-manifest graph.json --out runner.c` uses the named bundle to identify the exact local packages to inspect. The manifest supplies Predef, reachable implementations, scoped imports and native binding information just as for a binary. Discover at most one entry per package using `Package.testEntry`. With plain `Test` values, select the last one, which can be a `TestSuite`. If there are `ProgTest` values, select the last one and report a plain `Test` that follows it. Skip individual packages without entries. If none of the selected packages has an entry, fail C emission with a diagnostic listing the searched packages. A test in a dependency cannot satisfy that check. Dependencies and imported libraries supply implementation code. Tests defined in a dependency group run under that group’s own test label. [Package discovery](../../core/src/main/scala/dev/bosatsu/Package.scala#L224) · [Native test selection and tree shaking](../../core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala#L830)
3. **Let Bazel run the native harness.** Root C emission at the discovered test bindings, including private ones. The existing CLI's by-value selector requires an export. Use package discovery here. Compile and link the generated C with the same pinned target toolchain, runtime and externs as a binary. Return the executable and declared data runfiles from a Starlark test rule. `bazel build //math:math_test` builds the runner. `bazel test //math:math_test` executes it on a compatible test platform and reports assertion failures through its exit status. [C selection modes](../../core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala#L105) · [Bazel test execution contract](https://bazel.build/reference/test-encyclopedia)

**Dependency boundary.** An import used solely by an in-package assertion is still a direct dependency of the group being type-checked. It satisfies strict deps and belongs in the group's `deps`. A production group therefore cannot import a Bazel `testonly` helper target through this path. Runtime test data belongs on the test rule’s `data` attribute. A separate test-only Bosatsu group is useful for black-box suites that import the public API. It can depend on test-only helpers. No language visibility change or second compilation is needed for the in-package case.

**Checked against the compiler.** A temporary fixture compiled the source above with today's `tool check`. Native test C was generated using only the implementation bundle and compiled Predef. With host Clang and installed static GC/libuv, the executable found the private suite and passed with exit 0. Changing the expectation to 5 reported the assertion and exited 1. The implementation bytes changed while the signatures were byte-identical and exported only `answer`. The Bazel test rule and pinned native toolchain remain proposed work.

## Dependencies and composition

A group may depend on another group or an imported library. The interface inputs are a package-level graph over Bosatsu names, projected to real Bazel label edges after resolution. Grouping files or putting several packages in one imported library never makes all of that provider's private dependencies compile inputs. Runtime code generation gets the full reachable implementation graph separately. The later source-library rule can use the same edges. [Bazel aspects](https://bazel.build/extending/aspects)

### Strict deps and exposes

Bosatsu checks `exposes` against the foreign packages visible in a package's typed exported API. An omitted declaration means an empty set. The build uses this information when compiling a **consumer**, even when its source imports no symbol directly from an exposed package. [Source exposure check](../../core/src/main/scala/dev/bosatsu/PackageCustoms.scala#L211) · [Typed exposure calculation](../../core/src/main/scala/dev/bosatsu/Package.scala#L62)

1. For all source files in the group, seed the required package names with their explicit imports and declared exposures. Follow the `exposes` of **each required package** to a fixed point. Traverse package names before subtracting group-local packages and implicit Predef, choosing scoped providers and deduplicating by label. Ignore implementation-only imports of dependencies. Use verified typed exposure metadata for compiled dependencies and the headers for local sources. The compiler checks those headers against the typed exports.
2. Under `--error-unused-interfaces`, require exactly the files supplying that set. A file is used when it provides at least one required package, whether reached by an import or an exposure. A multi-package file need only supply one required package. Still apply the ordinary unused-import check to source imports. Report a missing interface with the path that required it and an extra interface with its file and label. Check the original declarations before projecting wide interfaces. These rules apply identically in one-shot and worker modes. [Source-import checking](../../core/src/main/scala/dev/bosatsu/PackageCustoms.scala)
3. If A imports X, X exposes Y, and X privately imports P, Gazelle gives A the providers of **X and Y**. Omitting Y is an error (`A imports X; X exposes Y; missing //types:y`). Supplying P is an unused-dependency error unless another required path reaches it. If Y exposes Z, add Z too. If X and Y share a provider, pass its file once. A's source still needs a `from Y import …` only to use names from Y directly. If A exposes nothing, a later consumer importing A alone has no edge to X or Y merely because A used them internally.

The ordinary `compile-packages` path must handle a wide signature without a projection action. First index each root's package name, checked exposure names and serialized entry. Starting from the source requirements, select the reachable package entries before decoding referants or resolving foreign definitions. For a file containing X and Sibling, if only Sibling exposes Y, a consumer of X can compile with that file alone. Supplying Y is an unused-interface error. A consumer of Sibling must supply Y. Share this selection logic with the optional `select-interfaces` action. Its benefit is a smaller downstream action key. [Current whole-file decoder](../../core/src/main/scala/dev/bosatsu/ProtoConverter.scala#L2219)

### Composition checks

**Check the graph being composed.** In the initial rules, check duplicate local names, ambiguity among visible imports, and collisions between local implementations and imported ones. Distinct private library versions keep their own namespaces. The future producer must additionally reject two distinct libraries embedding the same source when they become reachable together. A predecessor and its old dependencies form a separate comparison graph.

| Paths from one root | Result |
| --- | --- |
| `app → A → Common` and `app → B → Common` | Valid when both paths resolve to the same library. Visit `Common` once. |
| `app → A → //shared:json` and `app → B → //shared:json`, where both libraries package that group | Reject at `app`. Name the source package and print one path through each supplying library. |
| `app → //shared:json` as a loose group and `app → A` where `A` embeds that source | Reject at `app`. The local root and `A` would each supply an implementation. |

Use the source file identity to detect the same package wrapped by two different groups, including private members. Use Bosatsu package names to detect duplicate definitions in a library and ambiguous imports wherever both names are visible. Independent private helpers with the same name remain valid in separate library scopes. Compare shared-source claims across different library names. Two published versions of the same named library may contain successive revisions of that source when Bosatsu permits the versions in separate private scopes. Implicit Predef is supplied by the compiler. [Library validation](../../core/src/main/scala/dev/bosatsu/library/LibConfig.scala) · [Existing path diagnostic test](../../core/src/test/scala/dev/bosatsu/library/LibConfigTest.scala) · [Library namespaces](../../core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala)

### Transitive sets

- `BosatsuPackagesInfo` carries group and source labels, signatures and implementations, groups reachable without crossing a library edge, and the external libraries consumed along those edges.
- An imported `BosatsuLibraryInfo` carries exported signatures, their per-package exposure index, verified implementation files, scoped dependency metadata, and native `CcInfo`. A local `BosatsuPackagesInfo` records the same name/exposure data per package and its loose implementation closure. The `bosatsu_binary` consumer accepts either provider in the dependency graph. The optional source library will produce the library provider and record source membership claims.
- Carry structurally shared `depset`s of small per-node manifests. Each records its local members and immediate edges. For a validation root, visit each distinct library once, check all claims in a map, and retain parent links for diagnostics. Deduplicate aliases and diamonds by library identity and verify the full artifact hash before reusing a library. Different bytes claiming the same name/version fail. One requested root costs O(nodes + edges + memberships) without enumerating every path. On an N-group chain, full scans at every intermediate group would visit N(N+1)/2 manifests. Distinct composition roots each pay for their own reachable graph. Avoid flattening the entire closure in Starlark at each group. [Bazel depsets](https://bazel.build/extending/depsets)
- Check available label facts during analysis. Emit small topology manifests from declared group edges, package headers and verified import metadata. Group-local checks read only the group and direct-edge facts. Register one full scan at a binary, test or full library assembly. A group exposes the same scan on demand with `bazel build //app:api --output_groups=bosatsu_graph_check`. Consumers take the group’s providers and do not request that output group. Keep full-byte identity and release checks at full assembly, import verification and binary composition. The rule contract below describes how to request validation without adding implementation digests to a type-check action.
- Published libraries identify exported names and dependency descriptors. To read old releases, compute each exported interface’s exposed packages from its typed exports after loading its verified dependency context. The import updater writes this per-package index in the lock for Gazelle and the import action verifies it against the artifact. A private package origin may be unavailable in an old release. Portable source-origin metadata on new releases belongs to the deferred producer. [Library format](../../proto/src/main/protobuf/bosatsu/TypedAst.proto) · [Interface derivation](../../core/src/main/scala/dev/bosatsu/Package.scala#L382)

## Interface artifacts and rebuilds

| Artifact | Contents | Consumed by |
| --- | --- | --- |
| `api.bosatsu_package` | Typed implementations of all packages in the local group, including discovered private tests. | Binary and test C emission and optional future library assembly. |
| `api.bosatsig` | Local group package interfaces; each root advertises its checked exposed package names. | Other groups and optional library-interface projection. |
| Imported `.bosatsu_ifacelib` | Exactly the exported package interfaces of the verified library and their exposure index. | Groups requiring those roots by import or exposure. |
| Imported full `.bosatsu_lib` | Published implementations, full descriptors and history. | Import verification and extraction of scoped runtime inputs. |

Keep `Packages` and the repeated root interfaces in `Interfaces`. Emit a deterministic exposure index readable per root before semantic decoding, in the interface encoding or a companion metadata file. References to foreign types, constructors or aliases are resolved against the explicitly supplied interface for their package. A file for X cannot satisfy the required Y provider merely by embedding facts about Y. Preserve required local type/ABI information and canonical ordering. Audit opaque types so decoding an exported interface does not demand an implementation-only package omitted by the checked `exposes`. Handle representation details used only by the native backend in the implementation view. [Wire formats](../../proto/src/main/protobuf/bosatsu/TypedAst.proto) · [JVM reader](../../cli/src/main/scala/dev/bosatsu/IOPlatformIO.scala#L259) · [Interface serialization](../../core/src/main/scala/dev/bosatsu/ProtoConverter.scala#L1765)

The import projection reads verified dependency files when necessary to decode legacy libraries. Derive the exposure index from typed exported referants; the compiler drops the source-only `exposes` declaration when creating an interface. The existing `tool extract-iface` clears only implementations and retains VCS identity, private dependency descriptors and history. Project just the exported signatures and exposure names for consumers. A future source-library writer can produce byte-identical interfaces with the same encoder. [Typed interface](../../core/src/main/scala/dev/bosatsu/Package.scala#L382) · [Current extractor](../../core/src/main/scala/dev/bosatsu/tool_command/ExtractIfaceCommand.scala)

A compile action takes only files required by imports and the exposed-interface closure. A private body change in P can change the final binary without rechecking A. A public type change in exposed Y legitimately invalidates A even if X's own signature bytes are unchanged. An unrelated export in a wide Y interface can be filtered by the projection below. No runtime bundles, release hashes or predecessor metadata enter the compile key. [Bazel action inputs and caching](https://bazel.build/remote/caching)

## Bazel rule usability and providers

| Issue exposed by the review | Rule contract |
| --- | --- |
| Language exports and Bazel visibility serve different consumers. | Give each cross-directory library a Bazel `visibility` declaration, as in the Common example. `exports` controls the Bosatsu names visible through that library. Generate public `@name//:lib` targets for intentionally exposed imported libraries. Forward standard Bazel attributes, including `visibility`, `tags` and platform constraints, through macros. [Bazel visibility](https://bazel.build/concepts/visibility) |
| A binary is runnable; an imported library exposes several artifacts. | Return the target executable and its runfiles for the binary. Consumers choose signature/implementation fields from the providers. Offer explicit interface and full output groups for imports. Only a future publishable source library needs a publish launcher and deployment runfiles. [DefaultInfo](https://bazel.build/rules/lib/providers/DefaultInfo) · [Runfiles](https://bazel.build/concepts/runfiles) |
| Validations can accidentally force the full closure to build. | Use Bazel’s transitive `_validation` output group only for local checks. Keep the full group scan in the explicit `bosatsu_graph_check` output group. At each executable or full library root, consume its one structural check result in the emission or assembly action. Keep that result and all implementation digests outside package type-check inputs. Import verification and full assembly still check byte identity. [Bazel validation actions](https://bazel.build/extending/rules#validation_actions) |
| The source of an input and the machine a tool runs on can differ. | Accept group and imported-library providers on `deps`. Use the pinned JVM compiler on the execution platform and the registered C toolchain/sysroot on the target platform. Propagate native requirements as wrapped `CcInfo`. [Toolchains](https://bazel.build/extending/toolchains) · [C++ integration](https://bazel.build/configure/integrate-cpp) |
| A first-party library and an imported library must expose identical downstream APIs. | Project imports to the same compile and C-generation artifacts as source libraries. Carry native bindings explicitly when an import needs user-provided externals. Report missing bindings at binary assembly with the originating package and symbol. The consumer example in CI must register the pinned native toolchains and build without a host compiler fallback. |
| Tests live beside the functions they exercise. | Ship the explicit `bosatsu_test` rule in the initial suite. Use the package label for dependencies and the test label or a wildcard for `bazel test`. [In-package testing](#testing-private-functions-in-the-same-package) |

Expose `library_name` with the Bazel target name as its default so a version-specific label can keep a stable Bosatsu identity. Diagnose competing logical libraries with both labels. Pin `.bazelversion` and the module/toolchain versions in the external consumer fixture. Start CI with the fixture's Bazel 9.2.0 and qualify other versions explicitly.

Use real label edges for dependencies. Package names come from source headers and verified imported interfaces. Starlark cannot discover them by opening generated files during analysis. Use each group’s own `srcs`; Gazelle generates labels across BUILD directories. In the producer phase, `previous` is a separate edge outside the current dependency graph. [Bazel rules and phases](https://bazel.build/extending/rules) · [Depsets](https://bazel.build/extending/depsets)

## Action inputs and cache invalidation

A Bazel target dependency makes another target's providers available. Each action chooses the particular files its result needs. Passing a library's entire `DefaultInfo.files`, executable runfiles or descriptor to a type-check action defeats interface caching. Use `FilesToRunProvider` for executable tools and distinct fields for Bosatsu data. Pin the compiler jar, JRE and options; changing a tool is a legitimate invalidation. Use deterministic argument ordering, execroot-relative paths and a fixed environment. Keep wall clocks, inferred Git revisions, credentials, host home directories and global infer caches out of build actions. Disable volatile stamping by default. If a release embeds an explicit VCS ID, confine that input to assembly. Use stable output names for interface files independent of release version. Changes to labels, toolchains, configurations and repository paths can still change the action key. [Bazel remote caching](https://bazel.build/remote/caching)

| Action | Inputs that should affect its key | Outputs and expected invalidation |
| --- | --- | --- |
| Header scan and interface selection | Sources for the scan. For selection, stable header imports/exposures and one interface per declared provider. | Validate the required closure before projecting. Emit one narrowed interface per used label; a body edit can leave header data unchanged. |
| Group compilation | Sources, the normal or narrowed interfaces required by imports and exposures, their label map, compiler and semantic options. | Typed package bundle and group signature with exposure metadata. Byte-identical inputs preserve dependent cache hits. |
| Imported interface projection | The pinned full library and necessary verified dependency data for decoding. Later the source-library projection takes member signatures and export names. | Stable exported interfaces and per-root exposures. Metadata-only changes may rerun projection while leaving its output identical. |
| Full library assembly (producer phase only) | Member implementations, dependency descriptors/required full artifacts, release attributes and predecessor/comparison inputs. | Full `.bosatsu_lib` and descriptor. Absent from the local monorepo binary path. |
| Binary C emission | Root and reachable group implementations, library-scoped implementation bundles, symbol-resolution graph, scope identities, selected main, and codegen options. | Deterministic C. Source-built libraries can expose their existing implementation bundles. For imports, extract the equivalent runtime view from the verified full file. Serialize only the membership, resolution, version and code that affect C emission. Keep history, mirror URIs, docs URLs and predecessor descriptors out of this action. A version change can legitimately change generated identifiers. [Library namespaces](../../core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala) |
| Native compile and link | Generated C, actual headers, runtime/extern implementations and archives, target C toolchain and link options. | Native binary. With the jar, JRE, signature inputs and Bosatsu configuration fixed, changing an isolated native flag or runtime build input preserves group compile hits. Changing the jar bytes legitimately invalidates those actions, including when the only changed jar field is the embedded runtime digest. |
| Test C emission and execution | Emission uses the selected root bundle and binary implementation graph. Bazel's test action uses the linked harness, runfiles and test configuration. | A private assertion edit recompiles its group and can regenerate C. Identical signatures keep downstream type checks cached. A production emitter reading the changed bundle can rerun. Identical generated production C preserves downstream native cache hits when all other inputs match. Test data alone can invalidate the test result without recompiling the group. |

**Optional cache narrowing.** The compiler always selects required roots before semantic decoding. For large groups or imports, `tool select-interfaces --error-unused-interfaces` resolves the same import/exposure closure against the declared files and checks it before filtering. Emit one projection per required label, preserving attributed package names and the type facts needed along exposure paths. An exposed-only provider counts as used. Keep all its required API facts whenever finer slicing cannot be proved sound. Skip the extra action for small groups when it costs more than it saves.

**How to verify it.** Use `aquery` to inspect the declared actions/inputs and `--execution_log_json_file=<path>` or the compact log to check which spawns actually ran. Use a clean output base with a populated disk or remote cache to test cache portability separately from incremental builds in the same output base. Test a no-op; a private body edit; an unrelated export edit; an imported type change; a URI/history-only edit; a native flag edit with a byte-identical compiler jar; and a rebuilt compiler jar that embeds a different runtime digest. Compare generated bytes and execute the resulting binary when behavior should change. [Bazel action query](https://bazel.build/query/aquery) · [Execution logging](https://bazel.build/reference/command-line-reference#flag--execution_log_json_file)

A temporary **Bazel 9.2.0** two-action fixture used an actual sandboxed JSON worker, a producer with separate signature/implementation outputs, and a consumer that read the signature alone. The cold build executed both worker actions; a body edit executed only the producer; a signature edit executed both; a no-op executed neither. These measurements establish the Bazel mechanism. Bosatsu's interface encoder, runtime projection, cross-platform toolchains and remote cache behavior still need the end-to-end tests below.

## Downloads and the C runtime

1. **Pin the compiler and runtime together.** The release workflow already publishes `bosatsu.jar`, `bosatsu-c-runtime-<commit>.tar.gz`, `SHA256SUMS`, and `B3SUMS`. Copy verified SHA-256 pins into the catalog shipped with the rules. Resolve the artifacts from that catalog or from explicit local files as described below. Run the verified assembly using a pinned execution-platform Java runtime. The release builds the jar with Java 17. The JVM solver uses the Chicory/WASI implementation of Z3. Include a recursion-checking fixture on both execution OSes to exercise the packaged solver. [Release workflow](../../.github/workflows/release.yml) · [Compiler dependencies](../../project/Dependencies.scala) · [JVM solver](https://github.com/johnynek/scalawasiz3/tree/v0.0.13)
2. **Build the C dependencies from their pinned sources.** The runtime manifest already selects thread-safe BDWGC 8.2.8 and libuv 1.52.1 and gives BLAKE3 hashes. In the rules' catalog, index each upstream SHA-256 by dependency name and the manifest's BLAKE3 content hash. Verify both digests against the same source archive in CI. This lookup works with an unreleased local compiler/runtime pair. When a local runtime selects new upstream bytes, add the corresponding checksum entry before building it. Use Bazel repository downloads with hash verification, then target-configured build actions for the static archives. Start with pinned `rules_foreign_cc`, CMake and Ninja to reuse the existing upstream configurations. Carry through the BDWGC thread settings, consumer `GC_THREADS` define, macOS `NO_DESC_CATCH_EXCEPTION_RAISE`, libuv static build, and target-specific system link inputs. [Dependency manifest](../../c_runtime/deps.json) · [Current recipes](../../core/src/main/scala/dev/bosatsu/cruntime/VendoredDeps.scala) · [Bazel download rules](https://bazel.build/rules/lib/repo/http) · [Rules ForeignCc](https://bazel-contrib.github.io/rules_foreign_cc/)
3. **Build the Bosatsu runtime as a target `cc_library`.** Explicitly list the runtime, external implementations, public and internal headers from the matching archive. Both generated headers are tracked and can be consumed directly. Propagate includes and link requirements via `CcInfo`. A source group can type-check external declarations before their native implementations are configured. At the native milestone, groups or libraries supplying external implementations can opt into `native_deps` and propagate their `CcInfo` to the final link. Explicitly adding a C target can require native toolchain analysis. Its files stay outside the Bosatsu compile action. The existing Makefile and installer discover host tools, paths, and platform state. Port their flags and source membership into Bazel targets. [Runtime Makefile](../../c_runtime/Makefile) · [C backend external symbols](../../core/src/main/scala/dev/bosatsu/codegen/clang/ClangGen.scala)
4. **Pin everything that executes or supplies headers.** Register the exec JRE, C compiler, assembler, archiver, linker, target sysroot/SDK, and the tools used by the foreign build actions, including their shell utilities where applicable. Drive platform selection from Bazel constraints. Disable host discovery and configure-time execution of target programs when cross-compiling. Verify in a network-disabled execution environment with no ambient Homebrew, pkg-config, `.bosatsuc`, Git, or system GC/uv. Start with Linux, then certify macOS with a declared, pinned SDK or a digest-pinned execution image supplying it. A locally discovered Xcode SDK would leave macOS hermeticity unproven. Static GC and libuv can still link against the target OS's declared system ABI. [Bazel toolchains](https://bazel.build/extending/toolchains) · [rules_cc toolchain defaults](https://github.com/bazelbuild/rules_cc#hermetic-toolchains)

For library imports, derive the lock from the checked-in roots manifest using the first-import workflow above. Record the schema version and canonical roots digest with the descriptor graph. Record name, version, full-byte hashes, URIs, dependency edges, and **per exported package** its provider and exposed package names. The updater verifies each artifact’s pins. Normal builds consume the checked lock and fetch only pinned artifacts. Materialize per-import metadata, leaving the whole lock outside compile action keys. An all-source monorepo does not need a Bosatsu library lock. A mixed build needs the imported implementation closure for linking, including private versions, while type checking sees only the interfaces required by the package-level import/exposure graph. Reading a release requires no local release/history writer. [Library format](../../proto/src/main/protobuf/bosatsu/TypedAst.proto) · [Dependency decoder](../../core/src/main/scala/dev/bosatsu/library/DecodedLibrary.scala) · [Bazel module extensions](https://bazel.build/external/extension)

### Local toolchain inputs

A compiler-only workspace can select an unreleased jar with this MODULE declaration. The pinned JRE comes from the compiler toolchain.

```starlark
bosatsu.toolchain(
    compiler_jar = "//ci/artifacts:bosatsu.jar",
    compiler_sha256 = "<64 hex digits from the jar build>",
)
use_repo(bosatsu, "bosatsu_toolchains")
register_toolchains("@bosatsu_toolchains//:compiler")
```

When enabling native targets, add `c_runtime_archive = "//ci/artifacts:bosatsu-c-runtime.tar.gz"` and `c_runtime_sha256 = "<64 hex digits from the archive build>"` to the same tag, register `@bosatsu_toolchains//:runtime`, and register a pinned target C toolchain. With a catalog `version`, the catalog supplies each artifact's default URL and pin when that artifact is needed. A local override for a catalog release must match its pin. Without a catalog version, require an explicit pin for each supplied file. The jar alone is a complete compiler configuration. A native target additionally requires a configured runtime.

Labels identify individual source files, for example files declared with `exports_files`. For files outside the workspace, use `compiler_jar_path = "/srv/bosatsu/bosatsu.jar"` or `c_runtime_archive_path = "/srv/bosatsu/bosatsu-c-runtime.tar.gz"` with the corresponding SHA-256. Reject a label and a path together for one artifact. Files must exist when repository resolution starts, so CI builds them in an earlier step or job.

Keep compiler registration and its artifact verification independent of the repositories for the Bosatsu runtime, GC, libuv and target C tools. A catalog may describe all of them. Requesting the compiler or building a plain source group must never trigger native repository downloads or native toolchain resolution.

**Verification.** For remote artifacts use the repository downloader with the expected SHA-256. For a local label or path, resolve and `watch` the selected source file in a repository rule. Invoke an independently pinned repository-host helper through `repository_ctx.execute`. Ship that helper with the rules for each supported host so it is available before analysis, including on a provisioned offline machine. It streams the file once, copies and hashes the same bytes into a temporary file in the generated repository, compares the expected SHA-256, and renames the verified snapshot into place only on success. A missing file or mismatch aborts resolution, reports the path and, on mismatch, both digests. Extract only the verified snapshot. This path uses neither a downloader nor a cached copy of the artifact, even with a warm repository cache. [Local file watching and execution](https://bazel.build/rules/lib/builtins/repository_ctx#execute) · [Bazel 9.2.0 download-disable check](https://github.com/bazelbuild/bazel/blob/9.2.0/src/main/java/com/google/devtools/build/lib/bazel/repository/downloader/DownloadManager.java#L303)

The runtime input is the release-format tarball containing `c_runtime/`, including headers, sources and `deps.json`. Hash the compressed file, then unpack the verified snapshot. Build the runtime, GC and libuv for the target with the same rules in both modes. Give the jar and runtime separate canonical labels and stable exec paths. The compiler action takes the verified jar and exec JRE. Native actions take the verified runtime sources and target toolchain. Absolute host paths and the method of acquisition stay out of action arguments and generated manifests, including worker startup flags. Remote executors receive declared inputs from Bazel.

**Offline fixture.** With Bazel 9.2.0, a disposable repository rule watched an external binary file and invoked a pre-provisioned copy-and-hash helper under `--repository_disable_download`. The first query succeeded with a fresh output base and an empty cache for the selected artifact. Changing the file at the same path made the next query fail with the expected and actual SHA-256 digests. The toolchain acceptance tests repeat this for the real deploy jar and runtime tarball.

**Native compatibility.** SHA-256 pins identify the two chosen files independently. At native setup, run the verified jar’s proposed `tool verify-runtime --archive FILE --out STAMP` against the exact compressed snapshot. If the jar advertises `BuildInfo.cRuntimeArchiveHash`, compute and compare that algorithm’s digest (BLAKE3 for current releases). Fail with expected and actual digests on a mismatch or malformed advertised value. For a local build with an empty embedded hash, the explicitly pinned jar and runtime are the developer-selected pair. Consume the success stamp at native composition before producing a runnable test or program. Jar-only package compilation does not read the archive or stamp. [Existing runtime hash reader](../../core/src/main/scala/dev/bosatsu/cruntime/Command.scala#L107)

**GitHub CI handoff.** The compile-only job can stage the candidate deploy jar and its SHA-256 alone. For the later native job, archive `c_runtime/` once, compute its BLAKE3, pass `BOSATSU_C_RUNTIME_HASH=blake3:<digest>` to `sbt cli/assembly`, and stage the resulting assembly as `bosatsu.jar`. Compute SHA-256 for both completed files in that producer job and pass the two pins and the exact files to the Bazel consumer job. Generate its fixture MODULE declaration using those pins. For production, record reviewed release pins and point the paths at the provisioned copies. Repacking the tree can change the compressed bytes. A rebuilt release jar embedding the new archive digest changes the compiler action key. [Release workflow](../../.github/workflows/release.yml) · [Embedded runtime hash](../../build.sbt#L261)

## Compiler implementation checklist

Make `bosatsu tool` the file-oriented interface to the released jar used by Bazel and Make. These commands and flags are proposed. Preserve the existing engine and migrate its repository callers and tests when replacing low-level verbs. Build actions take named files, write only declared outputs, and run offline. Networked lock updates and deployment run outside the worker. [Current tool dispatch](../../core/src/main/scala/dev/bosatsu/tool_command/ToolCommand.scala) · [CI callers](../../.github/workflows/ci.yml)

### Commands and files

The consumer milestone needs `headers`, `compile-packages`, `inspect-library`, `validate-graph`, `verify-runtime`, `emit-c`, lock support for imports, and the worker dispatcher. `select-interfaces` is a cache optimization. Source library projection/assembly, history lookup and deployment can ship later. Keep the canonical export encoder, library provider and verified artifact descriptor reusable by both phases.

| Command or entry point | Required behavior |
| --- | --- |
| `tool headers --src FILE ... --out FILE` | Batch-read source headers using Bosatsu's parser. Emit a versioned, sorted list of path, declared name, imports and `exposes` for Gazelle and interface selection. Explicit files support sources generated by other Bazel rules. [DepsCommand](../../core/src/main/scala/dev/bosatsu/tool_command/DepsCommand.scala) · [PackageResolver](../../core/src/main/scala/dev/bosatsu/tool/PackageResolver.scala) |
| `tool select-interfaces` | Take `--headers`, repeated `--direct-interface`, a declared `--projection-map` of file/label/output, and `--error-unused-interfaces`. Follow per-package exposures to closure before projecting. Emit one narrowed file per required label. Report extra providers and missing exposure paths. |
| `tool compile-packages` | Take repeated `--src` and `--direct-interface` files, optional `--interface-labels FILE`, and `--error-unused-interfaces`. Validate the exact import/exposure closure across all sources. Index names/exposures in raw bundles and decode only reachable entries. Attribute each selected package to its declared file and label. Compile in emit mode and write `--packages-out` and `--sig-out`. Predef is implicit. [Current check command](../../core/src/main/scala/dev/bosatsu/tool_command/CheckCommand.scala) |
| `tool library-interface` (producer phase) | Project selected exported package names from local member signatures to the canonical interface format used by imports. |
| `tool inspect-library` | Verify the pinned release and derive exported signatures, the per-root exposure index, a scoped implementation view, membership and the descriptor. Compute exposure metadata from decoded typed exports for existing OSS artifacts. Keep projections independently requestable. |
| `tool validate-graph` | Take `--root`, repeated `--node` manifests, and `--out`. Distinguish direct visibility, loose membership, library boundaries and the separate history graph. Report the two actual paths for a conflict. Support a small declared validation output. Share this validator with full assembly and binary composition where byte identity matters. |
| `tool assemble-library` (producer phase) | Use `--manifest`, `--out` and `--descriptor-out`. Read group implementations, expected public interface, direct public/private dependencies, resolution-only inputs, metadata and optional predecessor via an explicitly declared assembly manifest. Validate local/explicit versions, stamped dependency hashes, URI closure and SemVer rules; compute history using `nextHistory`. Emit the full artifact and its external descriptor. [AssembleCommand](../../core/src/main/scala/dev/bosatsu/tool_command/AssembleCommand.scala) · [LibConfig](../../core/src/main/scala/dev/bosatsu/library/LibConfig.scala) |
| `tool verify-runtime` | Take `--archive FILE --out STAMP` at native composition. Read the jar’s advertised archive hash, verify the compressed archive when a hash is present, and write the success stamp only after validation. |
| `tool emit-c` | Require `--runtime-manifest`, `--out` and exactly one root selector. `--main-package NAME` selects the program. `--test-packages FILE` selects discovered private or public entries from that one local implementation bundle in the manifest. Reuse its decoded packages, preserving scope and entry order. Supply effective Predef, scoped dependencies and declared generated headers. Emit C and leave native compilation, linking and test execution to Bazel. [RuntimeCommandSupport](../../core/src/main/scala/dev/bosatsu/tool_command/RuntimeCommandSupport.scala) · [ClangTranspiler](../../core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala) |
| `tool history` (producer/history phase) | Start at a verified artifact and resolve a requested version over authenticated links. Provide a declared-file resolver for hermetic use and separate it from interactive URI fetching. Distinguish unavailable bytes, a bad digest and a version outside the lineage. |
| `tool bazel-worker` | Add one-shot param-file and persistent JSON modes as specified below. Dispatch the allowed file operations in process. Supply a request-local diagnostic sink, file context, exit code and scratch lifetime. |
| `tool lock-libraries`; later `tool publish-artifacts` | `lock-libraries --imports FILE --out FILE` resolves remote descriptors and atomically writes the verified closure, roots digest and export/exposure index from the authoritative roots file, even when the lock is absent or stale. In the producer phase, `publish-artifacts --manifest FILE` verifies the prepared closure and deploys in dependency order. Neither command runs in a worker. |

### Changes beneath the CLI

1. **Package and import interfaces.** Implement deterministic signatures and per-package exposure metadata, including a projection for legacy imported libraries. Decode required foreign definitions from the declared providers. Preserve semantic constructor order, aliases, kinds and ABI facts. Avoid embedding unrelated transitive API or release data. [ProtoConverter](../../core/src/main/scala/dev/bosatsu/ProtoConverter.scala)
2. **Resolution and namespaces.** Preserve file paths/labels when following source imports and interface exposure chains. Reject missing/unused interface files before flattening the type environment. Root local groups in one scope, include Predef, and preserve name/version scopes for imported dependencies. Resolve implementations for native program and test emission. [JVM file reader](../../cli/src/main/scala/dev/bosatsu/IOPlatformIO.scala) · [Dependency decoder](../../core/src/main/scala/dev/bosatsu/library/DecodedLibrary.scala)
3. **Phase boundary.** Import verification, declared-file decoding, native externs and the mixed-root C emitter are consumer requirements. Local version sentinels, producer SemVer/URI checks, new historical checkpoints, portable origin metadata on newly published artifacts and upload coordination belong to the optional producer phase.
4. **Process and IO.** Refactor `Main`/`PathModule` command execution so a request returns an exit status and diagnostics rather than writing directly to the process streams. Share a bounded parallel service and immutable compiler resources across worker requests. Keep paths, errors, temporary files and partial outputs request-local. Stage failed one-shot/worker outputs consistently. Under Make, replace interface outputs only when their bytes change. [JVM entry point](../../cli/src/main/scala/dev/bosatsu/Main.scala) · [IOPlatformIO](../../cli/src/main/scala/dev/bosatsu/IOPlatformIO.scala)
5. **Independent integration tests.** Exercise the released jar through its actual file CLI and worker entry point from an external Bazel workspace, including sandboxed one-shot and worker strategies. Exercise the same successful file invocation without Bazel from a clean directory for Make. Verify old published libraries as inputs. Migrate affected CI, sanitizer and Valgrind scripts when changing `tool check` and `tool transpile`. [Current CI](../../.github/workflows/ci.yml) · [Valgrind caller](../../scripts/test_c_valgrind.sh)

## Persistent JVM worker

### Commands and action registration

The execution toolchain supplies a launcher for the pinned JRE and jar, including their runfiles. Its effective command is `java -jar bosatsu.jar`, forwarding argv intact. Implement these three invocations in the same released jar:

```sh
# Standalone or Make: explicit file command, ordinary diagnostics and exit status.
java -jar bosatsu.jar tool compile-packages --error-unused-interfaces --src app/Types.bosatsu \
  --packages-out out/types.bosatsu_package --sig-out out/types.bosatsig

# One-shot action: expand one UTF-8 multiline params file, then call that command.
java -jar bosatsu.jar tool bazel-worker @compile.params

# Worker startup: Bazel appends this switch; read WorkRequests until EOF.
java -jar bosatsu.jar tool bazel-worker --persistent_worker
```

For this invocation the contents of `compile.params` are as follows.

```text
compile-packages
--error-unused-interfaces
--src
app/Types.bosatsu
--packages-out
out/types.bosatsu_package
--sig-out
out/types.bosatsig
```

The first entry is the operation and the remaining entries are its file/option arguments, one per line. Keep spaces within an argument, handle line endings without shell splitting, and ensure the JVM launcher passes the `@file` through to Bosatsu. In worker mode Bazel expands the final flagfile and places those **arguments** in each request. Dispatch the received vector directly. Action-specific paths and flags go in the params file and stay out of startup arguments. The worker supports only file-oriented operations; a request cannot invoke publishing, arbitrary process execution, or an online resolver.

For example, the Starlark action below receives a source list and a list of interface files from the group's direct `deps` (or one narrowed file for each such dep). `label_map` names the declaring Bazel labels for these files and `tc.driver` is the toolchain's `FilesToRunProvider`.

```starlark
args = ctx.actions.args()
args.add("compile-packages")
args.add("--error-unused-interfaces")
args.add("--interface-labels", label_map)
args.add_all(srcs, before_each = "--src")
args.add_all(direct_ifaces, before_each = "--direct-interface")
args.add("--packages-out", impl)
args.add("--sig-out", sig)
args.set_param_file_format("multiline")
args.use_param_file("@%s", use_always = True)
ctx.actions.run(
    executable = tc.driver,
    toolchain = "@rules_bosatsu//:compiler_toolchain_type",
    inputs = depset(srcs + direct_ifaces + [label_map]),
    outputs = [impl, sig],
    arguments = ["tool", "bazel-worker", args],
    mnemonic = "BosatsuCompile",
    execution_requirements = {
        "supports-workers": "1",
        "requires-worker-protocol": "json",
    },
    use_default_shell_env = False,
)
```

Declare that toolchain type on the rule. Start singleplex and enable workers first on expensive group compiles. Other pure commands can use the same dispatcher. Share a `worker-key-mnemonic` for compatible actions only if measurement shows that separate mnemonic pools would fragment warm JVMs. Bound worker count, heap and compiler parallelism together against the execution machine. The ordinary action still works on a remote executor that starts a fresh process. [Creating workers](https://bazel.build/remote/creating) · [Persistent worker keys and sandboxing](https://bazel.build/remote/persistent) · [Args](https://bazel.build/rules/lib/builtins/Args#use_param_file) · [Action toolchains](https://bazel.build/rules/lib/builtins/actions#run)

### Wire protocol

Use UTF-8 **newline-delimited JSON** on stdin/stdout. Each physical line contains one JSON object, with no length prefix. Decode protobuf JSON field names, tolerate unknown input fields and supply protobuf defaults when missing. Stdout is exclusively for protocol responses. Capture each action's diagnostics into its response and flush the newline immediately. Stderr is available for worker-lifetime logging. [Worker protocol definitions](https://github.com/bazelbuild/bazel/blob/9.2.0/src/main/protobuf/worker_protocol.proto) · [Bazel's JSON requirements](https://bazel.build/remote/creating) · [Protobuf JSON encoding](https://protobuf.dev/programming-guides/json/)

| Incoming `WorkRequest` field | Handling |
| --- | --- |
| `arguments` | Ordered string array, default `[]`. The first argument is the operation. Reuse the ordinary option parser for the rest. |
| `inputs` | Array of objects with `path` and optional `digest`. Paths identify the request's allowed inputs and may include the worker or params file. `digest` is a protobuf bytes field encoded in base64. Treat decoded bytes as an opaque cache token. Default/empty digest gives no evidence for reuse. |
| `requestId` | Integer, default `0`. Zero is singleplex. Match the incoming ID in the response. An ID is independent of the action cache key. |
| `verbosity` | Integer, default `0`. Direct optional worker debugging to stderr. Preserve action diagnostics at all levels. |
| `cancel` | Boolean, default `false`. A cancellation control message addresses an existing request by ID. V1 does not advertise cancellation. |
| `sandboxDir` | String, default empty for singleplex. Use the request working directory for v1; reject an unexpected nonempty value rather than reading outside the supported layout. Future multiplex sandbox support must resolve **both** inputs and outputs under the supplied request directory without changing the process-wide working directory. |

Example request and response, shown on their respective streams. The request's omitted ID defaults to zero. The short digest illustrates an opaque byte token.

```json
{"arguments":["compile-packages","--error-unused-interfaces","--src","app/Types.bosatsu","--packages-out","out/types.bosatsu_package","--sig-out","out/types.bosatsig"],"inputs":[{"path":"app/Types.bosatsu","digest":"AQID"}]}
```

```json
{"requestId":0,"exitCode":0,"output":""}
```

If type checking fails, return one response for the same request. The newline in this diagnostic is escaped on the wire. The JVM remains available for the next build action.

```json
{"requestId":0,"exitCode":1,"output":"app/Types.bosatsu:4: unknown name 'token'\n"}
```

A `WorkResponse` has integer `exitCode`, string `output`, integer `requestId` and boolean `wasCancelled`, default `false`. On success return only after all declared outputs are complete and closed. On ordinary failure, discard partial results, finish all request-owned work, then report failure. Handle an invalid command inside a valid envelope as an action failure. On clean EOF finish the loop. On malformed/truncated protocol input with no trustworthy request, log the transport error to stderr and exit nonzero so Bazel can retire the broken process.

Cancellation and multiplexing are later capabilities. Only when implemented and tested should the rule advertise their execution requirements. A later cancel message such as `{"requestId":7,"cancel":true}` may end the original request with `{"requestId":7,"wasCancelled":true}`. Exactly one response belongs to each original non-cancel request. A cancel control message gets no second response; a late cancellation for completed work is ignored. Do not leave asynchronous writes running after any response.

### Cache safety and acceptance

Reuse the JIT, classloader, immutable Predef and bounded executor first. Disable the filesystem inference cache for Bazel requests. A future bounded in-memory cache must include compiler/codec identity, compilation and lint options, file contents, and the dependency-interface context used to decode them. Keying by path, library name/version, or a request ID alone yields stale results after an edit. With a missing input digest, recompute from the declared file or bypass the cache. Use a request-local scratch directory under the action working directory and clean it before responding. Keep diagnostics and action outputs identical in fresh, warm and restarted processes. [Bosatsu compile cache keys](../../core/src/main/scala/dev/bosatsu/cache/CompileCache.scala)

```sh
bazel build --strategy=BosatsuCompile=worker,sandboxed --worker_sandboxing //app:demo
bazel build --strategy=BosatsuCompile=sandboxed //app:demo
bazel aquery 'mnemonic("BosatsuCompile", deps(//app:demo))'
```

For parity tests, use separate output bases with no prepopulated action cache or force fresh inputs so both strategies execute. Send multiple requests to one JVM: good, invalid, then good with changed bytes at the same path. Test missing digests, omitted ID and unknown fields, spaces/non-ASCII paths, multiline diagnostics, EOF, worker restart and no stray stdout. Check actual `SpawnExec` records, output digests and the program result. Measure worker speed only on action misses. Bazel action-cache hits dispatch no work request. The transport fixture above confirmed that Bazel 9.2.0 expanded the param file into `arguments` and omitted the singleplex zero ID. Bosatsu worker compatibility must still be verified using the released jar.

## Gazelle

Ship a Go extension in a custom `gazelle_binary`. Packages-only mode needs a source-root/grouping policy and declared binary mains. Default to grouping files by Bazel directory, retain hand-chosen groups, and allow singleton groups. Obtain package names, imports and per-package `exposes` from the pinned batch header reader. Index source package names to group labels and imported export names to their library labels and verified exposure sets from the lock. Expose the required external providers through real, consumer-visible labels. For example, let the Bzlmod extension generate an import index repo, bring it into scope with `use_repo`, and give each exposed foreign package a stable alias to its pinned library target. Gazelle can then emit a legal label even when Y arrived via imported X's lock entry. Gazelle operates before analysis and reads source headers and checked lock data. [Extending Gazelle](https://github.com/bazel-contrib/bazel-gazelle/blob/master/extend.md) · [Header parser](../../core/src/main/scala/dev/bosatsu/tool/PackageResolver.scala#L75)

For each group compute the exact required package names using the same import/exposure algorithm as the compiler. Follow each named API's exposures recursively with a visited set, including exposures crossing a source/import boundary. Resolve names before collapsing by provider label. Remove group-local names and Predef from the emitted deps, deduplicate labels, and write real dependency edges. Record a witness path for missing providers and diagnose ambiguous names with explicit override support. Do not walk ordinary imports of other packages or all exports of a provider. If grouping induces a Bazel target cycle, report the underlying imports/exposures and split or regroup. Re-running Gazelle removes stale generated edges and preserves intentional manual fields and `# keep`; strict checking still flags unused retained labels.

Preserve hand-authored `bosatsu_test` targets. A package-local directive such as `# gazelle:bosatsu_test math` can generate `bosatsu_test(name = "math_test", packages = ":math")` for an explicitly selected group. Recognize an existing test for the group before generating another one, and diagnose a conflicting target at the chosen label. Test bindings can have arbitrary names and inferred types. The typed compiler discovers them when building the runner.

When a source library producer is added, extend this same index with its exported packages. Generate membership and library visibility from explicit policy and typed exposures. No group owner field or library root declaration is needed for packages-only generation.

## Delivery order and acceptance tests

Merge the consumer-facing `tool` changes first. The subsequent Bazel milestones are independently usable in the order **compiler jar and JRE → packages → C runtime and target C toolchain → test → binary → import**. Package compilation can ship as soon as its file commands and compiler toolchain work. Gazelle can grow alongside each rule.

### Implementation tracker

Stage 0 lands the compiler changes as isolated PRs. Each has a check that can run without a Bosatsu Bazel rule. Stages 1–6 use the acceptance tests detailed below. [Command contracts](#commands-and-files) · [Worker protocol](#persistent-jvm-worker)

Change **Complete?** to **Yes** when the implementation has landed and its acceptance check passes. Link the implementing PR in the row. If the design changes, update the row and the corresponding design section in the same PR.

| Step | Complete? | Change and prerequisites | Acceptance check |
| --- | --- | --- | --- |
| 0a. Request-scoped execution | No | Factor a file-command runner out of JVM `Main` and `PathModule`. Return status and diagnostics to the caller, isolate paths and scratch files, and stage declared outputs on success. | Run good, invalid and good requests in the same JVM and verify diagnostics, outputs and exit statuses. A failed request must leave reusable process state clean. |
| 0b. Source headers | No | Add `tool headers` over explicit source files using the language parser. Emit stable package names, imports and `exposes`. | Reorder input files and change a private body. The header bytes stay identical. Capture malformed headers with file-level diagnostics. |
| 0c. Canonical package interfaces | No | Define the deterministic per-package signature and exposure encoding and decoder. Resolve foreign definitions from explicitly supplied interfaces. Preserve required type and ABI information, including opaque types. | Round-trip multi-package signatures, foreign types, constructors and aliases. With narrowing disabled, index X and Sibling and resolve only the selected root’s foreign references. Private-body edits preserve signature bytes and required export edits change them. |
| 0d. Batched file compilation | No | On 0a and 0c, add `tool compile-packages` with explicit sources/interfaces and named bundle and signature outputs. Preserve private bindings needed for test discovery and ordering. | Compile two groups from files, using only the first group's signature to type-check the second. Decode both outputs, check private tests survive and ensure implicit Predef is handled once per compilation. |
| 0e. Strict direct interfaces | No | On 0b–0d, add the shared import/exposure resolver, per-file label provenance and `--error-unused-interfaces`. Apply the check before combining or narrowing files. | Require Y when X exposes Y, reject private P and cover recursive exposures. With X and Sibling in one raw signature and only Sibling exposing Y: A importing X succeeds without Y, rejects an extra Y, and importing Sibling diagnoses missing Y until it is supplied. Run one-shot and worker fixtures. |
| 0f. Structural graph checks | No | Add `tool validate-graph` for explicit root and node manifests. Track group identities, reachable names and parent edges. Start with local group graphs. | Verify local checks read only local/direct facts. For chains of N and 2N groups, count manifests visited across validators for one requested root and require linear growth. Visit a shared diamond node once and report both paths for a collision. |
| 0g. Local native compilation scope | No | On 0d, decode the runtime manifest and compiled local bundles, supply effective Predef exactly once, and resolve native extern requirements. Feed the existing C backend a reusable compilation scope. | Decode separately compiled groups with an empty library graph in a clean directory. Resolve ordinary and Predef references, and diagnose duplicate/missing implementations without a local install. |
| 0h. Test C emission | No | On 0g, add `tool emit-c --test-packages`. Discover test roots in the one selected bundle, tree-shake from those bindings and emit declared C outputs. | Generate C from compiled inputs only. Run a native fixture for private `Test` and `ProgTest`, passing and failing assertions, empty selection, and a dependency whose own suite must be excluded. |
| 0i. Program C emission | No | On 0g, add the alternative `tool emit-c --main-package` selector, validate the root group and main type, and emit declared C outputs. | From the all-source demo bundles, generate C and link a program that exits 7. Reject a main outside the selected group or with an invalid type. Neither emission mode invokes a host C compiler. |
| 0j. One-shot worker arguments | No | On 0a and 0d, add the `tool bazel-worker @params` dispatcher for file operations. Keep filenames and multiline parameter-file semantics consistent with the standalone CLI. | Run the candidate jar on a params file containing paths with spaces and non-ASCII characters. Compare output bytes, diagnostics and status with the direct invocation. |
| 0k. Persistent JSON worker | No | On 0j, implement `--persistent_worker`, singleplex line framing, per-request diagnostics, flush and EOF handling. Reuse the JIT and immutable compiler resources. | Drive the jar on stdin/stdout with good/bad/good requests, changed bytes at the same path, omitted IDs, missing digests and a process restart. Compare with one-shot results. |
| 0l. Inspect verified imports | No | On 0c, add `tool inspect-library` over declared files. Verify artifact digests and name/version, project exported interfaces and exposure metadata, and emit the scoped implementation view and descriptor. | Read an existing released library with verified dependencies. Check the interface projection is stable across implementation-only changes. Reject a bad hash or incomplete declared dependency context. |
| 0m. Validate imported scopes | No | On 0f and 0l, extend the structural graph checks to local roots with scoped library dependencies. | Resolve a valid diamond once, retain two legal private versions, and reject conflicting visible packages with both paths. |
| 0n. Emit C for mixed graphs | No | On 0h, 0i, 0l and 0m, teach both C emission modes to resolve imported implementations and native bindings within their library scopes. | Generate a working program and test using a pinned library. Exercise same-named private packages in different scopes and a missing extern diagnostic. |
| 0o. Lock external libraries | No | On 0l, add the explicit online `tool lock-libraries` command. Traverse and verify dependency descriptors and write a deterministic lock with per-package providers and exposures. Normal build commands read declared files offline. | Generate a lock from the roots manifest with no prior lock, regenerate byte-identically, change a root pin and refresh it. A bad pin or missing link leaves the previous lock intact. Inspect the verified closure offline. |
| 0p. Runtime archive compatibility | No | Add the native-only `tool verify-runtime` command and success stamp. | Accept a jar and matching archive. Reject two individually SHA-pinned files when the jar’s embedded BLAKE3 disagrees. Cover a jar with no embedded hash, a malformed hash, and no stamp on failure. |
| 0q. CLI cutover and artifact qualification | No | After the preceding PRs, exercise the candidate deploy jar through the public CLI and worker entry points. Migrate CI, sanitizer and Valgrind callers in the same PR that retires the superseded command paths. | From a clean fixture directory run the Make-style file commands, native program and test fixtures, and the worker transcript using the same candidate jar. Pass the jar to stage 1 CI and the matching archive to stage 3 native CI. |
| Optional after 0e. Narrow interfaces | No | Add `tool select-interfaces` as a separate cache optimization. The packages rule can ship with whole direct signatures. | Compare raw and narrowed builds on the X/Sibling/Y fixture. Both enforce the same strict deps on the original declarations. Measure action misses after unrelated edits to a wide provider. |
| 1. JVM compiler toolchain | No | Register the pinned jar and exec JRE. | Test catalog and local jar selection. With downloads disabled and an empty artifact cache, resolve a valid local jar, then reject replacement at the same path under the old SHA-256. [Stage 1](#1-register-the-jvm-compiler-toolchain) |
| 2a. Package compilation | No | Build `bosatsu_packages` using the compiler toolchain. | Produce the bundle and signature. Compile from an unnarrowed multi-package signature using only the required roots. On a single-root chain build, check local validations and the explicit group-scan output group. [Stage 2](#2-compile-with-bosatsu_packages) |
| 2b. Package caching and Gazelle | No | Enforce strict dependencies and generate local BUILD rules. | Check cache hits after private edits and verify Gazelle generates the same BUILD files on its second run. [Stage 2](#2-compile-with-bosatsu_packages) |
| 3. Native runtime and toolchain | No | Build the Bosatsu runtime, GC and libuv with pinned target tools. | Resolve the local archive offline, detect a mismatched jar/archive pair, and check cache behavior both with a fixed jar and with a rebuilt stamped jar. Link and run the native fixture. [Stage 3](#3-add-the-c-runtime-and-target-c-toolchain) |
| 4. In-package tests | No | Add `bosatsu_test` and test label generation. | Run a private suite and diagnose a group with no test entries. Verify one mandatory root scan across a chain of groups for the test executable. [Stage 4](#4-run-tests-with-bosatsu_test) |
| 5. Native programs | No | Add `bosatsu_binary` and main declarations for Gazelle. | Run the all-source demo and confirm a binary and test sharing a group compile that group once. [Stage 5](#5-build-programs-with-bosatsu_binary) |
| 6a. Verified imports | No | Implement the repository and BUILD forms of `bosatsu_import`. | Bootstrap from no lock and refresh a stale lock using the documented command. Reject a stale import build, bad hash or package collision; resolve a valid diamond once. [Stage 6](#6-consume-external-code-with-bosatsu_import) |
| 6b. Mixed consumer and Gazelle | No | Connect imports to local groups and Gazelle’s dependency resolution. | Build and test the mixed fixture, check exposed dependencies and repeat generation. [Stage 6](#6-consume-external-code-with-bosatsu_import) |

The producer-only commands `library-interface`, `assemble-library`, `history` and `publish-artifacts` belong to the optional producer phase below. Existing published libraries remain inputs to the consumer qualification.

### 1. Register the JVM compiler toolchain

Add the Bzlmod catalog, local jar label/path support, SHA-256 verification, pinned exec JRE and worker-capable launcher. CI resolves the candidate jar built by the previous job. Run the offline fixture with the catalog dependencies pre-provisioned and only a local jar for Bosatsu. Exercise source labels and absolute paths, missing bytes, a wrong pin and an in-place replacement with a warm cache. Identical catalog and local jar bytes under the same rules/configuration should yield identical compiler inputs. A user registering only `@bosatsu_toolchains//:compiler` needs no native runtime archive or target C toolchain.

### 2. Compile with bosatsu_packages

- **2a. Group providers and actions.** Add the batched compile, strict direct interface edges, structural validations and deterministic provider/output groups. Build `//app:api` in a fixture whose BUILD file loads only `bosatsu_packages` and whose MODULE registers only the compiler. Verify the `.bosatsig` and `.bosatsu_package` outputs, then compile a consumer using only the signature. Inspect repository fetches and `aquery` to confirm the runtime archive, GC, libuv and C toolchain were unnecessary. Include a group with an external declaration. On the X/Sibling/Y fixture, build once with whole direct signatures and once with optional projections when available. Assert the same pass/fail matrix from 0e. An all-source workspace needs no import lock. Use a worker and a sandboxed one-shot action from clean output bases and compare outputs.
- **2b. Cache and Gazelle.** Verify cold, no-op, private-body and exposed-type edits with execution logs. For A importing X that exposes Y and privately uses P, accept exactly X and Y as A's compile providers. Exercise intra-group imports and a provider with multiple packages. Inspect executed validation actions on N- and 2N-group chains and a diamond. A plain group build runs local checks. Request `--output_groups=bosatsu_graph_check` on the tip and require exactly one full scan visiting each reachable manifest once. Count all validation input visits, including direct edges in local checks. The total for either chain scales linearly. Land Gazelle's local grouping, header index and exposure resolution as its own PR. Regenerate twice and compare BUILD files.

**Compiler evidence.** In a temporary directory with an empty `PATH`, a locally built deploy jar run by an explicit JVM produced both artifacts with today's `tool check`. A second package compiled using the first package's signature as its only dependency input. This exercises the JVM file path. The Bazel acceptance check above is still to be implemented. [Compiler output handling](../../core/src/main/scala/dev/bosatsu/tool/Output.scala#L88)

### 3. Add the C runtime and target C toolchain

Add runtime registration and verified local/catalog archive support. Build the Bosatsu runtime, static GC and libuv from pinned sources using the target C toolchain and sysroot. Carry runtime and external link inputs through `CcInfo`. Resolve the local archive from a fresh repository with downloads disabled, replace it in place under the old pin, and check mixed catalog/local sources. With valid SHA-256 pins for both files, pair a jar stamped for archive A with archive B. The native build must fail before producing an executable while a jar-only group build still succeeds. Compile, link and run a small native fixture that exercises GC and libuv. Prefetch, build on a network-disabled executor with an empty home, and inspect declared inputs. Change only a runtime C define with the jar, JRE and Bosatsu configuration fixed. The affected native actions rerun and group compilation stays cached. Then rebuild the jar with a different embedded runtime digest and its matching archive. Confirm the changed compiler tool bytes cause the group compile action to miss the prior cache key. Qualify Linux first and macOS separately. Cross-builds must not execute target programs during compilation.

### 4. Run tests with bosatsu_test

Add the explicit test rule taking one group label. Generate the harness from that compiled bundle, compile/link with the stage 3 runtime and C toolchain and let Bazel run it. Run targeted and wildcard tests for private helpers, plain `Test` and `ProgTest`, invalid ordering and an entirely testless selected group whose dependency has tests. Check assertion failures and runfiles. For a single test root on the chain fixture, verify one full structural scan and linear total manifest visits. A private assertion edit keeps downstream group type checks cached. A data-only edit reruns the test without compiling Bosatsu. Add Gazelle's explicit test-target generation and preservation checks.

### 5. Build programs with bosatsu_binary

Add the binary rule with one root group and one main package. In the all-source fixture `bazel build //app:demo` produces the executable and `bazel run //app:demo` exits 7. Build a program and test sharing a group in the same target configuration and confirm one `BosatsuCompile` action for that group with `aquery`. Inspect C emission and target compile/link actions, and verify one full structural scan for the requested binary. Compare output hashes across clean roots and check remote cache reuse, including equivalent locally staged and catalog artifacts. Add Gazelle's explicit main declarations.

### 6. Consume external code with bosatsu_import

- **6a. Verified repository and BUILD rule.** Expose pinned remote, vendored or previously downloaded library bytes, their exported signatures, exposed package index, scoped implementations and native bindings. From a clean checkout with no lock, run the first-import command, commit its deterministic output and build the example. Change the root to another valid pin and verify that a normal build rejects the stale lock. Run the same updater with the import extension already configured, rebuild, and regenerate again with no diff. A failed update preserves the previous file. Cover bad hashes, absent providers, duplicate packages, a shared diamond and legal private-version coexistence.
- **6b. Mixed consumer and Gazelle.** Replace local `Bosatsu/Prog` with a pinned OSS import in the same fixture. Build and run the executable and tests. Generate BUILD deps for local → imported and imported → imported exposures, including required providers that arrive via the lock. Regenerate twice, exercise impl-only and public-API edits, and confirm that downstream compile actions see only the required interfaces.

**Consumer code complete** when the stage 6 mixed fixture and the stages 1–5 all-source fixtures pass with the pinned toolchains, workers, cache checks and Gazelle. The user-facing rules now cover compiling, testing, running and consuming published code. Publishing a local `bosatsu_library` can be a separate project.

## Optional producer phase for bosatsu_library and publishing

This phase can be deferred in its entirety. A default build of `//orders:orders` requests its interface and full library. Consumer compilation selects only interface provider fields. The same `public_deps` and `private_deps` edges drive validation, release assembly and publication. A pinned `bosatsu_import` counts as versioned when its verified artifact has a real version.

| Library declaration | Behavior |
| --- | --- |
| No `version` | Use the reserved local version `0.0.0-bosatsu.local`, with empty history. The target can build, link, and depend on local or versioned libraries. It cannot publish. Reject release URIs or a predecessor supplied without a version. |
| Explicit `version` | Require a real version on **every library in the current transitive dependency graph**, including all private branches and recorded transitive public dependencies. Fail the versioned build at the first local marker, showing the dependency path. Versioned builds without distribution URIs are allowed for local use. |
| Explicit `version` and nonempty `uris` | Build the publishable bytes. Require a pinned `previous` release of this library, or `initial = True` for the first release. Each dependency descriptor must also have verified full-file hashes and immutable retrieval URIs throughout the closure. `bazel run` on this target publishes the built bytes. |

`uris` lists canonical download locations for the full artifact. A versioned build can use `previous` to validate a successor before choosing distribution URIs. `initial` and `previous` are mutually exclusive. The old dependency graph is a separate input for comparing APIs, and its libraries do not become current dependencies. Gazelle preserves hand-authored version, predecessor and URI fields. Propagate declared version/URI eligibility and a path to a failing descendant in providers so analysis can reject invalid configurations early. Verify imported declarations against the bytes in their import actions. Neither check requires adding dependency descriptors to the group compile command.

Write the local marker explicitly in the library version field. An edge to a local library records that version plus the digest of its full bytes. For a given name the entire current graph may contain one distinct local artifact, including a diamond that reaches it twice. Reject competing local contents or a local and released instance of the same name. For entirely versioned graphs use the existing Bosatsu public/private version rules. `DecodedLibrary.versionOf` requires a version, C namespaces use `(Name, Version)`, and `versionOrZero` already maps missing data to ordinary `0.0.0` in some paths. A reserved prerelease keeps that normal release number available. Reject the reserved spelling and its build-metadata variants as explicit versions or predecessors. Check historical release usage before finalizing the spelling. [Decoder](../../core/src/main/scala/dev/bosatsu/library/DecodedLibrary.scala) · [Version fallbacks](../../core/src/main/scala/dev/bosatsu/library/Library.scala) · [Version type](../../core/src/main/scala/dev/bosatsu/library/Version.scala)

### Releasing Orders with Common

Add the two predecessor imports and Parser to the same roots manifest introduced above, regenerate the lock with the same update command, and expose the three repositories with `use_repo(libraries, "common_prev", "orders_prev", "parser")`. The following entries go in the manifest’s `roots` array.

```json
[
  {"name": "common_prev", "library_name": "common", "version": "2.0.0", "uris": ["https://releases.example.org/common-v2.0.0.bosatsu_lib"], "sha256": "<SHA-256 of Common 2.0.0>"},
  {"name": "orders_prev", "library_name": "orders", "version": "1.3.0", "uris": ["https://releases.example.org/orders-v1.3.0.bosatsu_lib"], "sha256": "<SHA-256 of Orders 1.3.0>"},
  {"name": "parser", "library_name": "parser", "version": "3.0.1", "uris": ["https://releases.example.org/parser-v3.0.1.bosatsu_lib"], "sha256": "<SHA-256 of Parser 3.0.1>"}
]
```

To prepare a release, edit the existing library declarations. In this example Common and Orders have distinct versions and pinned predecessors. Parser is already a versioned import. The following declarations illustrate a future publishing repository with local Common and Orders groups.

```starlark
# common/BUILD.bazel
load("@rules_bosatsu//:defs.bzl", "bosatsu_library")

bosatsu_library(
    name = "common",
    packages = [":common_sources"],
    exports = ["Common/Types"],
    visibility = ["//orders:__pkg__"],
    version = "2.1.0",
    previous = "@common_prev//:lib",
    uris = ["https://releases.example.org/common-v2.1.0.bosatsu_lib"],
)
```

```starlark
# orders/BUILD.bazel
load("@rules_bosatsu//:defs.bzl", "bosatsu_library")

bosatsu_library(
    name = "orders",
    packages = [":domain"],
    exports = ["Orders/Model"],
    public_deps = ["//common:common"],
    private_deps = ["@parser//:lib"],
    version = "1.4.0",
    previous = "@orders_prev//:lib",
    uris = ["https://releases.example.org/orders-v1.4.0.bosatsu_lib"],
)
```

`bazel build //orders:orders` first finalizes Common 2.1.0, including its own history and dependency descriptors. It hashes the final bytes and emits an external descriptor with the library name, version, SHA-256, BLAKE3 and URIs. Orders reads that descriptor, verifies it against Common's bytes, embeds it in the public dependency list, and does the same for Parser in the private dependency list. If `Orders → Common → Codec` reaches an unversioned Codec, the versioned build fails even when Codec is private. If Common 2.1.0 is already published, Orders can depend directly on its pinned import. Assembly happens offline after repository fetching. [Bazel action inputs](https://bazel.build/remote/caching)

### Contents of the released protobuf

| Reference in Orders 1.4.0 | Data embedded |
| --- | --- |
| Own release | Name, version and canonical distribution URIs. Compute the hash of the final bytes afterward and put it in the external release descriptor and consuming libraries. |
| Orders history | Authenticated links to selected earlier Orders releases. The immediate predecessor is always a link. Each link records version, full-file SHA-256 and BLAKE3, and retrieval URIs for newly written releases. Traversing those links reaches earlier releases. |
| Public, private and recorded unused transitive public dependencies | Each dependency entry contains library name, selected version, SHA-256, BLAKE3 and URIs of the **full released dependency**. Preserve visibility and the existing resolution semantics for multiple private versions. Each verified child in turn describes its own dependencies. |

### History construction and lookup

The current writer already computes `thisHistory = prevHistory.nextHistory(prevDesc, nextVersion)`. It reads `prevHistory` from the verified predecessor, or starts from empty history for the first release. It constructs `prevDesc` afresh with the predecessor's version and BLAKE3 and omits the URIs. Preserve that recurrence. Pass the verified descriptor with its hashes and URIs so a consumer can fetch the next hop. The named major, minor, patch and prerelease fields are links into prior artifacts. `others` is documented as additional navigation links. [Assembly](../../core/src/main/scala/dev/bosatsu/library/LibConfig.scala#L594) · [Transition](../../core/src/main/scala/dev/bosatsu/library/LibConfig.scala#L1125) · [History wire format](../../proto/src/main/protobuf/bosatsu/TypedAst.proto#L422)

**Verified behavior.** Every branch places the immediate predecessor in a named slot, so induction gives a path to every published ancestor on the same lineage. Some transitions carry older SemVer-level links across later releases. For instance, on the sequence `1.0.0, 1.0.1, 1.0.2, 1.1.0, 1.1.1, 1.1.2`, version `1.1.2` can jump directly to `1.0.2`. Its `others` also repeats `1.0.2` twice. In a probe of the existing JVM method on 65 consecutive patches from `1.0.0` to `1.0.64`, all earlier releases were reachable and the oldest took 64 links from the newest. For 65 consecutive major releases the maximum was also 64, and for 65 consecutive prereleases it was 32. SemVer links can skip entire runs, but the current worst case is linear. No historical-version lookup routine was found in the current compiler. The CAS fetch traversal follows library dependencies. [CAS traversal](../../core/src/main/scala/dev/bosatsu/library/Command.scala#L3240)

Add a lookup helper that uses Bosatsu's version ordering. At each step choose the oldest linked version at or above the requested version and strictly below the current one. Verify the fetched bytes against the link and check library name and version. Report an unpublished version if a valid chain passes the requested version or ends. Report a missing artifact or hash mismatch separately. A pinned resolver can supply locations for older links with missing URIs. Reuse verified cached bytes.

**Proposed bound for new histories.** Keep the immediate predecessor and named SemVer links. Use `others` for an ordered set of release-depth checkpoints. At depth `d` relative to the start of the new layout, repeatedly clear the lowest set bit and keep the descriptor at each resulting depth. For depth 13 the checkpoints are 12, 8 and 0. These descriptors can all be selected from the prior release's checkpoints plus the verified predecessor. Pass the already-known release depth to the transition. Mark the new layout and its base depth explicitly so legacy `others` entries keep their original meaning. On migration, seed the checkpoints at the verified legacy tip and retain ordinary traversal before that tip.

This uses O(log N) checkpoint descriptors and O(log N) historical artifact hops for releases within the new layout. If the current and target depths first differ at bit `k`, a checkpoint can take the reader to the start of the current half, and the immediate predecessor crosses into the target half. At most two hops resolve that bit. A separate in-memory model checked all 131,328 earlier-version pairs over depths 0 through 512, verified that each next checkpoint was available from the predecessor, and observed at most 17 hops. These are proposed lookup guarantees. The existing writer has the linear cases above.

Versioned assembly still checks predecessor identity, legal version adjacency, exported API compatibility and changes to public dependencies. Preserve full hashes and URIs for every newly recorded dependency or history edge, coalesce matching links during lookup, and reject conflicting digests for the same version and algorithm. A complete list of historical releases in every artifact is unnecessary.

### Verification and publication

- **Verify bytes starting from a trusted pin.** The importing project's lock pins the root artifact. Verify its downloaded bytes and expected name/version. For each dependency or historical edge followed, obtain the named artifact from a recorded URI, verify its hashes, and check its name and version. A mirror is usable if it returns the same bytes. This authenticates the serialized packages contained in each full library. The root's own digest lives outside the bytes it covers. Adding `sha256:<hex>` to Bosatsu's descriptor hash reader/verifier requires extending the current BLAKE3-only hash parser. Keep accepting existing BLAKE3 releases and verify both algorithms for new release descriptors. [Bosatsu hash reader](../../core/src/main/scala/dev/bosatsu/hashing/Algo.scala) · [Bazel download integrity](https://bazel.build/rules/lib/repo/http)
- **Publish in dependency order.** The runnable library or CI consumes the built artifacts and their descriptors. Upload Common, verify its bytes at the advertised URI, then expose Orders. For pre-existing dependencies, verify availability and digest before exposing the parent. Use create-only or conditional operations for immutable coordinates. A retry accepts already published identical bytes and rejects conflicting bytes. If Orders fails after Common is uploaded, reuse Common's verified release. Publish the root descriptor or release index after its closure is available. The existing GitHub release asset workflow is a natural first transport. [Release workflow](../../.github/workflows/release.yml)
- **Run the library target.** With the declarations above, `bazel run //orders:orders` can deploy the prepared closure. It receives the exact versioned build outputs as runfiles. A target lacking publish metadata reports which fields are missing before attempting network access. Record the distributed descriptors in consumers' locks and use their pinned artifacts as predecessors for the next releases.
