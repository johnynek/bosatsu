# Hermetic Bazel rules for Bosatsu

Ship the monorepo consumer path first. A repository can build a native executable entirely from `bosatsu_packages` targets or mix those groups with pinned OSS `bosatsu_import` targets. Its local packages need no library name, version, release history or publishing destination.

## Delivery boundary

| Initial monorepo rules | Optional producer phase |
| --- | --- |
| `bosatsu_packages`, `bosatsu_binary`, `bosatsu_test` and `bosatsu_import`. Include compiler/JRE and native toolchains, strict interface checking, worker support, and Gazelle for groups, imports, mains and test labels. | Add source `bosatsu_library` to aggregate groups, choose exports and optionally publish versioned artifacts. Preserve the release/history design below as a separate phase. |
| Source groups provide signatures, typed implementations and external symbol requirements. Imported libraries provide exported signatures, a verified implementation graph, scoped identities and native bindings. | A future source library provides the same downstream interface as an import. Only the producer needs full-library assembly, the local library version marker, predecessor checks and upload support. |

The native build must succeed after prefetch with an empty home, network-disabled actions and only declared inputs. A group using implicit Predef needs the pinned deploy jar and an execution-platform JRE. Groups importing standard packages use the matched core interface. Native targets add the matched C runtime and target toolchain. A monorepo can build from source groups with zero third-party imports. The module examples use placeholders for release pins. [Local toolchain inputs](#local-toolchain-inputs) · [Delivery plan](#delivery-order-and-acceptance-tests)

## Building a monorepo binary

```starlark
# MODULE.bazel
bazel_dep(name = "rules_bosatsu", version = "<rules release>")
bosatsu = use_extension("@rules_bosatsu//:extensions.bzl", "bosatsu")
bosatsu.toolchain(version = "<compiler release>")
use_repo(bosatsu, "bosatsu_toolchains")
register_toolchains("@bosatsu_toolchains//:compiler")
```

This registration suffices for `bazel build //app:api`. For `//app:demo`, also register `@bosatsu_toolchains//:runtime` and the pinned target C toolchain/sysroot. `Demo/Api` imports and exposes `Demo/Types`. `Demo/Main` imports `Demo/Api` and `Bosatsu/Prog`. The toolchain exports the matched core library as a normal dependency label.

```starlark
# app/BUILD.bazel
load("@rules_bosatsu//:defs.bzl", "bosatsu_binary", "bosatsu_packages")

bosatsu_packages(name = "types", srcs = ["Types.bosatsu"])

bosatsu_packages(
    name = "api",
    srcs = ["Api.bosatsu"],
    deps = [":types"],
)

bosatsu_packages(
    name = "main",
    srcs = ["Main.bosatsu"],
    deps = [":api", ":types", "@bosatsu_toolchains//:core"],
)

bosatsu_binary(
    name = "demo",
    packages = ":main",
    main_package = "Demo/Main",
)
```

Run `bazel build //app:demo` to build the executable and `bazel run //app:demo` to run it. Each group can hold several sources. The `:types` edge follows the exposure from `:api`. The `:core` edge supplies the interface for `Bosatsu/Prog`. Both edges count toward strict dependency checking.

For the all-source fixture, declare `bosatsu_packages(name = "prog", srcs = ["Prog.bosatsu"])` and replace the core edge on `:main` with `:prog`. Use the exact `Bosatsu/Prog` source identified by the toolchain manifest. This exercises native emission from loose bundles with an empty library graph and preserves the same compiler/runtime compatibility check. [Toolchain manifest](#compiler-core-and-runtime-manifest)

### Adding a third-party import

Suppose `Demo/Main` also imports `Parser/Text` from a third-party Parser release. Declare the root in `MODULE.bazel`. The URL and digest below are illustrative. For acceptance, serve the serialized Parser fixture at a local mirror and compute its pin from those bytes.

```starlark
libraries = use_extension("@rules_bosatsu//:extensions.bzl", "bosatsu_imports")
libraries.bosatsu_import(
    name = "parser",
    version = "3.0.1",
    uris = ["https://releases.example.org/parser-v3.0.1.bosatsu_lib"],
    sha256 = "<SHA-256 of Parser 3.0.1>",
)
use_repo(libraries, "parser")
```

Add `"@parser//:lib"` to the `deps` of `//app:main`, retaining its existing edges. `name` sets the Bazel repository name and defaults the expected Bosatsu library name. An optional `library_name` handles aliases. A normal `bazel build //app:demo` resolves the new tag. Change the version, URI and pin in that same tag to upgrade. Direct import declarations live entirely in MODULE.

During Bzlmod resolution the extension verifies the root SHA-256, checks the embedded name and version, and follows the authenticated descriptors of all reachable dependencies. For a legacy edge carrying only BLAKE3, verify the parent-specified digest on the downloaded child before trusting its descriptors or recording its SHA-256. Produce deterministic repository specifications with verified artifact pins, scoped edges and the inputs for a Gazelle package/exposure index. Use a pinned repository-host reader available before build analysis to inspect these files. Module evaluation cannot invoke a configured Bazel build target. Network access belongs to resolution and prefetch. Compiler actions consume declared files offline.

Use Bazel's ordinary `MODULE.bazel.lock` to record extension inputs and generated repository specifications. Let the default update mode populate it on the first build. Commit the lock with MODULE changes. CI can use `--lockfile_mode=error` to reject stale resolution. Keep the extension eligible for lockfile recording and test reuse after prefetch. Gazelle consumes the generated index through a declared input. [Bazel module extensions](https://bazel.build/external/extension) · [Bazel lockfile](https://bazel.build/external/lockfile)

| Target | Contract |
| --- | --- |
| `bosatsu_packages` | Compile a nonempty `srcs` list into one `.bosatsu_package` and one `.bosatsig`. Headers supply package names. `deps` must exactly supply the group's imports and required exposed interfaces. Bazel enables `--error-unused-interfaces`. Predef is implicit. The rule requires only the JVM compiler toolchain. |
| `bosatsu_binary` | `packages` names a root group. `main_package` selects a Bosatsu package in that group with a valid `Bosatsu/Prog::Main`. Link local implementations and, if present, verified imported library implementations. |
| `bosatsu_test` | Required `packages` is a single label providing `BosatsuPackagesInfo`. Run the suites in that group's compiled implementation. If none of its packages contains a test entry, building the runner fails. |
| `bosatsu_import` | A module extension downloads a pinned full `.bosatsu_lib` and exposes `@name//:lib`. A BUILD rule supports an existing download label or vendored file. Verify name/version/digests, index the exported packages and their exposures, and produce compile and codegen views of the same artifact. |
| `bosatsu_library` (later) | `packages` lists root groups. Follow group edges up to library boundaries. `exports` selects package names. Optional `version`, `uris` and `previous` configure the same target for release and publication. |

### Native binary action graph

1. Compile each reachable group from sources and the precise interface inputs below. Emit the typed implementations and signatures. All local groups belong to one binary root scope. Use the existing `CompilationSource` for a typed `PackageMap` in the all-local case. Resolve the selected root main, materialize the effective Predef implementation once, and tree-shake reachable values. [Package-map backend](../../core/src/main/scala/dev/bosatsu/codegen/CompilationSource.scala#L44) · [Main selection](../../core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala#L756) · [Prog source](../../test_workspace/Prog.bosatsu)
2. For a mixed graph, keep the loose local scope and add verified library scopes keyed by name/version. Preserve private dependencies and library-specific name resolution when generating C. The current file-based runtime helper flattens library implementations into a package map. The new emitter needs a local root plus imported scopes; it must not flatten two private versions into the same namespace. Represent the root in memory and retain its member-specific bindings as described in [Private releases and package visibility](#private-releases-and-package-visibility). [Current file path](../../core/src/main/scala/dev/bosatsu/tool_command/RuntimeCommandSupport.scala) · [Library namespace](../../core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala)
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

1. For each source member, seed its required packages with its explicit imports and declared exposures. In a mixed graph, resolve in that member’s scope before taking the union of required files for the action. Follow the `exposes` of **each required package** to a fixed point. Traverse package names before subtracting group-local packages and implicit Predef, choosing scoped providers and deduplicating by label. Ignore implementation-only imports of dependencies. Use verified typed exposure metadata for compiled dependencies and the headers for local sources. The compiler checks those headers against the typed exports.
2. Under `--error-unused-interfaces`, require exactly the files supplying that set. A file is used when it provides at least one required package, whether reached by an import or an exposure. A multi-package file need only supply one required package. Still apply the ordinary unused-import check to source imports. Report a missing interface with the path that required it and an extra interface with its file and label. Check the original declarations before projecting wide interfaces. These rules apply identically in one-shot and worker modes. [Source-import checking](../../core/src/main/scala/dev/bosatsu/PackageCustoms.scala)
3. If A imports X, X exposes Y, and X privately imports P, Gazelle gives A the providers of **X and Y**. Omitting Y is an error (`A imports X; X exposes Y; missing //types:y`). Supplying P is an unused-dependency error unless another required path reaches it. If Y exposes Z, add Z too. If X and Y share a provider, pass its file once. A's source still needs a `from Y import …` only to use names from Y directly. If A exposes nothing, a later consumer importing A alone has no edge to X or Y merely because A used them internally.

The ordinary `compile-packages` path must handle a wide signature without a projection action. First index each root's package name, checked exposure names and serialized entry. Starting from the source requirements, select the reachable package entries before decoding referants or resolving foreign definitions. For a file containing X and Sibling, if only Sibling exposes Y, a consumer of X can compile with that file alone. Supplying Y is an unused-interface error. A consumer of Sibling must supply Y. Share this selection logic with the optional `select-interfaces` action. Its benefit is a smaller downstream action key. [Current whole-file decoder](../../core/src/main/scala/dev/bosatsu/ProtoConverter.scala#L2219)

A signature may expose a package supplied as source in the same invocation. Compile that source before decoding the re-export against its public types. Keep source-to-source dependencies in the same batch. The compiler uses the resulting public interfaces for inference and cache lookup. It stores a package in the compiler cache only after inference succeeds without diagnostics.

### Composition checks

**Check the graph being composed.** In the initial rules, check duplicate local names, ambiguity among visible imports, and collisions between local implementations and imported ones. Distinct private library versions keep their own namespaces. The future producer must additionally reject two distinct libraries embedding the same source when they become reachable together. A predecessor and its old dependencies form a separate comparison graph.

| Paths from one root | Result |
| --- | --- |
| `app → A → Common` and `app → B → Common` | Valid when both paths resolve to the same library. Visit `Common` once. |
| `app → A → //shared:json` and `app → B → //shared:json`, where both libraries package that group | Reject at `app`. Name the source package and print one path through each supplying library. |
| `app → //shared:json` as a loose group and `app → A` where `A` embeds that source | Reject at `app`. The local root and `A` would each supply an implementation. |

Use the source file identity to detect the same package wrapped by two different groups, including private members. Use Bosatsu package names to detect duplicate definitions in a library and ambiguous imports wherever both names are visible. Independent private helpers with the same name remain valid in separate library scopes. Compare shared-source claims across different library names. Two published versions of the same named library may contain successive revisions of that source when Bosatsu permits the versions in separate private scopes. Implicit Predef is supplied by the compiler. [Library validation](../../core/src/main/scala/dev/bosatsu/library/LibConfig.scala) · [Existing path diagnostic test](../../core/src/test/scala/dev/bosatsu/library/LibConfigTest.scala) · [Library namespaces](../../core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala)

### Private releases and package visibility

A program can link two verified releases of the same Bosatsu library. For example, Left privately imports `shared@1` and Right privately imports `shared@2`. Both releases define `Shared/Api`. If Left and Right each expose only an `Int`, App can import both wrappers. If their public APIs expose their respective versions of `shared`, App sees two versions and compilation must fail. Check the library identity even if the two APIs happen to expose *different* Bosatsu package names from the two versions. The existing `LibConfig.validatePacks` collision check groups its public closure by package name. The import validator also needs a per-consumer check keyed by library name. [Library validation](../../core/src/main/scala/dev/bosatsu/library/LibConfig.scala)

- **Repository resolution (0o, 6a).** Key verified artifacts by Bosatsu name and exact version, check the full digest on every edge, and preserve the referring artifact and the edge’s visibility. Check an incoming descriptor against the verified bytes even on a cache hit. The current library decoder can reuse a name/version without invoking its loader for the second edge. Verify the descriptors before passing a graph to that decoder. Coalesce equal identities with equal verified bytes. Reject different bytes claiming the same name/version. Multiple MODULE roots can use distinct repository names such as `shared_v1` and `shared_v2` with the same `library_name`. Their transitive dependencies can also select both versions without either being a MODULE root. The toolchain’s pinned core retains its existing single-identity check. [Library decoder](../../core/src/main/scala/dev/bosatsu/library/DecodedLibrary.scala)
- **Compilation and Gazelle (0l, 0m, 6b).** Expose each imported package’s owning library identity and bind each imported or exposed package relative to its referring package. For each source member, follow its direct imports and their public exposures and maintain a map from Bosatsu library name to the selected release. Keep a witness path for each selection. Inspect public, private and legacy unused-transitive-public descriptor edges for artifact verification. Only the exposures of the selected package bring further interfaces into a member’s type environment. Two distinct releases in one member’s visible closure are an error, including when their projected signature bytes match. Private implementation edges stay in the implementation graph. Gazelle needs a caller-relative package index and an explicit binding when a direct source import has multiple possible providers. Emit real Bazel dependency labels after resolving names.
- **Batches and caches.** Two source members of one group can have disjoint private selections. The action’s declared files are the union of the members’ required files. The per-member binding map must survive selection, inference, cache lookup and package decoding. Carry it beside each local bundle to mixed C emission so that decoding a typed member binds its foreign names in the same context as compilation. In 0m add a declared `--scope-map FILE` to `compile-packages` and the optional projection. Each entry binds a referring member or imported scope and package name to a provider scope and one declared interface file. Derive these bindings from real dependency edges and the verified import index. Partition inference environments when members select different providers for the same package name. The existing nominal type representation identifies a type by package and name, so a single unqualified environment cannot distinguish the two definitions. Today’s v1 file/label map gives diagnostics, and the local resolver indexes providers by package name across the batch. Passing every version’s interface through that unscoped lookup would reject valid private versions. Fingerprint the selected public facts and the bindings needed to interpret them. Keep full implementation digests in artifact verification and linking. Changes behind a wrapper’s private boundary can relink the binary while leaving App’s type check cached. A provider switch visible to the member must revalidate its binding even when the public signatures have identical bytes.
- **Native emission (0n).** Carry both library scopes to C generation. Resolve each imported package in its caller’s scope. Give loose members with different bindings distinct resolution contexts while retaining unique loose package names. Route main and test entries through their member contexts. The library backend already keys compiled code by `(Name, Version)` and includes it in generated non-root global identifiers. The mixed emitter must diagnose missing/ambiguous bindings before emission and must preserve these keys through inlining and tree shaking. Standard native externs use unversioned C symbols. Share one symbol only when both releases select the same verified native implementation with a compatible ABI. Distinct implementations need distinct symbols even if their function signatures match. Diagnose a collision before linking if their native bindings cannot be separated. [Library codegen scope](../../core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala) · [Existing external convention](../../core/src/test/scala/dev/bosatsu/codegen/clang/ExternalNamespaceTest.scala)

The compiler-level fixture type-checks two incompatible `Shared/Api.pick` signatures, round-trips the release protos against each wrapper’s own dependencies, and renders both version-qualified symbols in one C translation unit. A host-native smoke test of that translation unit returned the pair `(1, 2)`. The mixed Bazel acceptance fixture must **link and run** the two implementations from pinned files, then expose both versions to one source and reject it with both dependency paths. Repeat with independently bound members in the same group and swap the argument/label order. Warm the worker with a successful binding, switch one visible edge to a conflicting release with byte-identical signatures, and require the same error as a cold compile. A private body edit should still let the outer consumer reuse its signature inputs. [C backend fixture](../../core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenLibraryDepsTest.scala)

### Transitive sets

- `BosatsuPackagesInfo` carries group and source labels, signatures and implementations, groups reachable without crossing a library edge, and the external libraries consumed along those edges.
- An imported `BosatsuLibraryInfo` carries exported signatures, their per-package exposure index, verified implementation files, scoped dependency metadata, and native `CcInfo`. A local `BosatsuPackagesInfo` records the same name/exposure data per package and its loose implementation closure. The `bosatsu_binary` consumer accepts either provider in the dependency graph. The optional source library will produce the library provider and record source membership claims.
- Carry structurally shared `depset`s of small per-node manifests. Each records its local members and immediate edges. For a validation root, visit each distinct library once, check all claims in a map, and retain parent links for diagnostics. Deduplicate aliases and diamonds by library identity and verify the full artifact hash before reusing a library. Different bytes claiming the same name/version fail. One requested root costs O(nodes + edges + memberships) without enumerating every path. On an N-group chain, full scans at every intermediate group would visit N(N+1)/2 manifests. Distinct composition roots each pay for their own reachable graph. Avoid flattening the entire closure in Starlark at each group. [Bazel depsets](https://bazel.build/extending/depsets)
- Check available label facts during analysis. Emit small topology manifests from declared group edges, package headers and verified import metadata. Group-local checks read the root’s direct edges and the membership projections of its direct groups. Register one full scan at a binary, test or full library assembly. A group exposes the same scan on demand with `bazel build //app:api --output_groups=bosatsu_graph_check`. Consumers take the group’s providers and do not request that output group. Keep full-byte identity and release checks at full assembly, import verification and binary composition. The rule contract below describes how to request validation without adding implementation digests to a type-check action.
- Published libraries identify exported names and dependency descriptors. To read old releases, compute each exported interface’s exposed packages from its typed exports after loading its verified dependency context. The extension generates the per-package index for Gazelle from verified artifacts. The import action checks it against the same artifacts. A private package origin may be unavailable in an old release. Portable source-origin metadata on new releases belongs to the deferred producer. [Library format](../../proto/src/main/protobuf/bosatsu/TypedAst.proto) · [Interface derivation](../../core/src/main/scala/dev/bosatsu/Package.scala#L382)

## Interface artifacts and rebuilds

| Artifact | Contents | Consumed by |
| --- | --- | --- |
| `api.bosatsu_package` | Typed implementations of all packages in the local group, including discovered private tests. | Binary and test C emission and optional future library assembly. |
| `api.bosatsig` | Local group package interfaces; each root advertises its checked exposed package names. | Other groups and optional library-interface projection. |
| Imported `.bosatsu_ifacelib` | Exactly the exported package interfaces of the verified library and their exposure index. | Groups requiring those roots by import or exposure. |
| Imported full `.bosatsu_lib` | Published implementations, full descriptors and history. | Import verification and extraction of scoped runtime inputs. |

Keep `Packages` and the repeated root interfaces in `Interfaces`. Emit a deterministic exposure index readable per root before semantic decoding, in the interface encoding or a companion metadata file. References to foreign types, constructors or aliases are resolved against the explicitly supplied interface for their package. A file for X cannot satisfy the required Y provider merely by embedding facts about Y. Preserve required local type/ABI information and canonical ordering. Audit opaque types so decoding an exported interface does not demand an implementation-only package omitted by the checked `exposes`. Handle representation details used only by the native backend in the implementation view. [Wire formats](../../proto/src/main/protobuf/bosatsu/TypedAst.proto) · [JVM reader](../../cli/src/main/scala/dev/bosatsu/IOPlatformIO.scala#L259) · [Interface serialization](../../core/src/main/scala/dev/bosatsu/ProtoConverter.scala#L1765)

In the group signature, each `Interface` carries a present `signatureDependencies` message, including an empty message for a leaf. Its `exposes` names come from the typed exports and are checked again when that root is decoded. Package roots and export declarations have canonical order. A type exported without constructors retains its type parameters and kinds. An exported constructor family retains declaration order, field and default-helper types, and existential parameters. The implementation bundle retains the full definitions. Its imports record whether to reconstruct the canonical public interface. Older implementations omit that field and keep their full import view.

`tool compile-packages` also reads unindexed signatures written by `tool check`. For a selected legacy root, it derives dependencies from locally owned exports and the identities of re-exported definitions. It follows those owners before decoding the complete root. Legacy local-table pointers to foreign definitions bind to the explicitly supplied owner. Both formats require an explicit provider for each external package in that closure. For library inputs, `tool check` validates the dependency descriptors and passes the raw exported interfaces to the same selection step. The compiler checks public type schemes against the resolved owners’ kinds before consulting the consumer cache. Names written in source still follow the ordinary import rules. With `--error-unused-interfaces`, the resolver rejects each declared file that supplied no reachable external package. The optional label map annotates the original files.

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

Expose `library_name` with the Bazel target name as its default so a version-specific label can keep a stable Bosatsu identity. Report both labels when two releases of a logical library are visible to the same package, or two artifacts claim one name/version with different bytes. Pin `.bazelversion` and the module/toolchain versions in the external consumer fixture. Start CI with the fixture's Bazel 9.2.0 and qualify other versions explicitly.

Use real label edges for dependencies. Package names come from source headers and verified imported interfaces. Starlark cannot discover them by opening generated files during analysis. Use each group’s own `srcs`; Gazelle generates labels across BUILD directories. In the producer phase, `previous` is a separate edge outside the current dependency graph. [Bazel rules and phases](https://bazel.build/extending/rules) · [Depsets](https://bazel.build/extending/depsets)

## Action inputs and cache invalidation

A Bazel target dependency makes another target's providers available. Each action chooses the particular files its result needs. Passing a library's entire `DefaultInfo.files`, executable runfiles or descriptor to a type-check action defeats interface caching. Use `FilesToRunProvider` for executable tools and distinct fields for Bosatsu data. Pin the compiler jar, JRE and options; changing a tool is a legitimate invalidation. Use deterministic argument ordering, execroot-relative paths and a fixed environment. Keep wall clocks, inferred Git revisions, credentials, host home directories and global infer caches out of build actions. Disable volatile stamping by default. If a release embeds an explicit VCS ID, confine that input to assembly. Use stable output names for interface files independent of release version. Changes to labels, toolchains, configurations and repository paths can still change the action key. [Bazel remote caching](https://bazel.build/remote/caching)

| Action | Inputs that should affect its key | Outputs and expected invalidation |
| --- | --- | --- |
| Header scan and interface selection | Sources for the scan. For selection, stable header imports/exposures and one interface per declared provider. | Validate the required closure before projecting. Emit one narrowed interface per used label; a body edit can leave header data unchanged. |
| Group compilation | Sources, the normal or narrowed interfaces required by imports and exposures, their label map, caller-relative scope bindings for mixed graphs, compiler and semantic options. | Typed package bundle and group signature with exposure metadata. Byte-identical inputs preserve dependent cache hits. |
| Imported interface projection | The pinned full library and necessary verified dependency data for decoding. Later the source-library projection takes member signatures and export names. | Stable exported interfaces and per-root exposures. Metadata-only changes may rerun projection while leaving its output identical. |
| Full library assembly (producer phase only) | Member implementations, dependency descriptors/required full artifacts, release attributes and predecessor/comparison inputs. | Full `.bosatsu_lib` and descriptor. Absent from the local monorepo binary path. |
| Binary C emission | Root and reachable group implementations, library-scoped implementation bundles, symbol-resolution graph, scope identities, selected main, and codegen options. | Deterministic C. Source-built libraries can expose their existing implementation bundles. For imports, extract the equivalent runtime view from the verified full file. Serialize only the membership, resolution, version and code that affect C emission. Keep history, mirror URIs, docs URLs and predecessor descriptors out of this action. A version change can legitimately change generated identifiers. [Library namespaces](../../core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala) |
| Native compile and link | Generated C, actual headers, runtime/extern implementations and archives, target C toolchain and link options. | Native binary. With the jar, JRE, signature inputs and Bosatsu configuration fixed, changing an isolated native flag or runtime build input preserves group compile hits. Changing the jar bytes legitimately invalidates those actions, including when the only changed jar field is the embedded runtime digest. |
| Test C emission and execution | Emission uses the selected root bundle and binary implementation graph. Bazel's test action uses the linked harness, runfiles and test configuration. | A private assertion edit recompiles its group and can regenerate C. Identical signatures keep downstream type checks cached. A production emitter reading the changed bundle can rerun. Identical generated production C preserves downstream native cache hits when all other inputs match. Test data alone can invalidate the test result without recompiling the group. |

**Optional cache narrowing.** The compiler always selects required roots before semantic decoding. For large groups or imports, `tool select-interfaces --error-unused-interfaces` resolves the same import/exposure closure against the declared files and checks it before filtering. Emit one projection per required label, preserving attributed package names and the type facts needed along exposure paths. An exposed-only provider counts as used. Keep all its required API facts whenever finer slicing cannot be proved sound. Skip the extra action for small groups when it costs more than it saves.

**How to verify it.** Use `aquery` to inspect the declared actions/inputs and `--execution_log_json_file=<path>` or the compact log to check which spawns actually ran. Use a clean output base with a populated disk or remote cache to test cache portability separately from incremental builds in the same output base. Test a no-op; a private body edit; an unrelated export edit; an imported type change; a URI/history-only edit; a native flag edit with a byte-identical compiler jar; and a rebuilt compiler jar that embeds a different runtime digest. Compare generated bytes and execute the resulting binary when behavior should change. [Bazel action query](https://bazel.build/query/aquery) · [Execution logging](https://bazel.build/reference/command-line-reference#flag--execution_log_json_file)

A temporary **Bazel 9.2.0** two-action fixture used an actual sandboxed JSON worker, a producer with separate signature/implementation outputs, and a consumer that read the signature alone. The cold build executed both worker actions; a body edit executed only the producer; a signature edit executed both; a no-op executed neither. A second sandboxed fixture exercised the Bosatsu jar’s strict compiler and graph validator. A no-op executed zero of four actions. Editing a private body and a direct child’s outgoing edges executed only the producer compile and full graph check. Editing the public type and group membership also executed the consumer compile and local check. The release toolchain, mixed-version import actions and remote cache still need the end-to-end tests below.

## Downloads and the C runtime

1. **Select the release toolchain.** The existing release workflow produces `bosatsu.jar`, core library bytes, the runtime source archive and checksums. Add the pinned compatibility manifest specified below. Launch the verified jar with the pinned execution-platform JRE. The release builds the jar with Java 17 and packages the Chicory/WASI implementation of Z3. Include a recursion-checking fixture on both execution OSes. [Release workflow](../../.github/workflows/release.yml) · [Compiler dependencies](../../project/Dependencies.scala) · [JVM solver](https://github.com/johnynek/scalawasiz3/tree/v0.0.13)
2. **Build the C dependencies from their pinned sources.** The archive’s `c_runtime/deps.json` already selects thread-safe BDWGC 8.2.8 and libuv 1.52.1 and gives BLAKE3 hashes. In the rules' catalog, index each upstream SHA-256 by dependency name and the manifest's BLAKE3 content hash. Verify both digests against the same source archive in CI. This lookup works with an unreleased local compiler/runtime pair. When a local runtime selects new upstream bytes, add the corresponding checksum entry before building it. Use Bazel repository downloads with hash verification, then target-configured build actions for the static archives. Start with pinned `rules_foreign_cc`, CMake and Ninja to reuse the existing upstream configurations. Carry through the BDWGC thread settings, consumer `GC_THREADS` define, macOS `NO_DESC_CATCH_EXCEPTION_RAISE`, libuv static build, and target-specific system link inputs. [Dependency manifest](../../c_runtime/deps.json) · [Current recipes](../../core/src/main/scala/dev/bosatsu/cruntime/VendoredDeps.scala) · [Bazel download rules](https://bazel.build/rules/lib/repo/http) · [Rules ForeignCc](https://bazel-contrib.github.io/rules_foreign_cc/)
3. **Build the Bosatsu runtime as a target `cc_library`.** Explicitly list the runtime, external implementations, public and internal headers from the matching archive. Both generated headers are tracked and can be consumed directly. Propagate includes and link requirements via `CcInfo`. A source group can type-check external declarations before their native implementations are configured. At the native milestone, groups or libraries supplying external implementations can opt into `native_deps` and propagate their `CcInfo` to the final link. Explicitly adding a C target can require native toolchain analysis. Its files stay outside the Bosatsu compile action. The existing Makefile and installer discover host tools, paths, and platform state. Port their flags and source membership into Bazel targets. [Runtime Makefile](../../c_runtime/Makefile) · [C backend external symbols](../../core/src/main/scala/dev/bosatsu/codegen/clang/ClangGen.scala)
4. **Pin everything that executes or supplies headers.** Register the exec JRE, C compiler, assembler, archiver, linker, target sysroot/SDK, and the tools used by the foreign build actions, including their shell utilities where applicable. Drive platform selection from Bazel constraints. Disable host discovery and configure-time execution of target programs when cross-compiling. Verify in a network-disabled execution environment with no ambient Homebrew, pkg-config, `.bosatsuc`, Git, or system GC/uv. Start with Linux, then certify macOS with a declared, pinned SDK or a digest-pinned execution image supplying it. A locally discovered Xcode SDK would leave macOS hermeticity unproven. Static GC and libuv can still link against the target OS's declared system ABI. [Bazel toolchains](https://bazel.build/extending/toolchains) · [rules_cc toolchain defaults](https://github.com/bazelbuild/rules_cc#hermetic-toolchains)

Third-party imports use the Bzlmod tag and resolution procedure above. Materialize per-import metadata and semantic interfaces separately so the complete lockfile and download locations stay outside compile keys. Preserve private version scopes during native linking. [Library format](../../proto/src/main/protobuf/bosatsu/TypedAst.proto) · [Dependency decoder](../../core/src/main/scala/dev/bosatsu/library/DecodedLibrary.scala)

### Compiler, core and runtime manifest

A toolchain release names **one tested combination** of the deploy jar, the core `.bosatsu_lib` and the compressed C runtime source archive. The proposed release pipeline writes a detached manifest after producing all three. The rules catalog maps the release to the manifest URI and its reviewed SHA-256. An unreleased CI build can supply a local manifest and its trusted SHA-256. Verify the manifest first and verify each artifact against its entry before use. An individual local pin must agree with the selected manifest. The core version is its own library version and can differ from the compiler release. This release manifest describes distributable bytes. The per-target `--runtime-manifest` passed to C emission describes the resolved program or test graph.

```json
{
  "schema_version": 1,
  "compiler": {"uri": "<deploy jar URI>", "sha256": "<jar digest>"},
  "core": {
    "library_name": "core_alpha",
    "version": "<core library version>",
    "uri": "<core .bosatsu_lib URI>",
    "sha256": "<full core library digest>"
  },
  "c_runtime": {"uri": "<runtime archive URI>", "sha256": "<compressed archive digest>"},
  "core_sources": {"Bosatsu/Prog": "<SHA-256 of the exact Prog.bosatsu release source>"}
}
```

`core_sources` is optional. When the release supports building standard packages from source, list the digest of every supported replacement under its Bosatsu package name. Check the source bytes and declared name using the same snapshot the compiler reads. Propagate the verified origin to native composition. Reject unlisted or modified replacements and two providers of the same core package in one linked scope. This keeps the all-source mode available for matched sources.

Reserve the selected core library identity for the toolchain. Diagnose a third-party root tag claiming that identity and point the user to the core label. The stable label `@bosatsu_toolchains//:core` provides the verified core library's exported interfaces and scoped implementations. A group importing a core package gets the interface through its direct `deps`. A third-party dependency referring to core must resolve to this manifest’s library identity and full digest, including on private paths. Coalesce identical references at link time and reject a conflicting core with a dependency path. A core version string or an independently valid replacement SHA-256 alone cannot establish compatibility. The compiler recognizes `Bosatsu/Prog::Main` and `ProgTest`, and the C runtime implements that package’s external declarations. [Prog source](../../test_workspace/Prog.bosatsu) · [C runtime implementations](../../c_runtime/Makefile) · [Compiler main selection](../../core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala#L756)

Fetch and validate only the parts a target consumes. A Predef-only group can use a jar and JRE alone. A core consumer verifies the jar and core, and type-checks against the projected signature without fetching the C runtime. Native composition verifies the jar, runtime and any core artifact or matched core sources it uses before linking. Keep the complete release manifest and full core digest outside ordinary package compile keys. Identical projected signature bytes preserve downstream cache hits when the jar and other compiler inputs are fixed.

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

For a full local toolchain, add the manifest and local artifact overrides to the same tag. Supply the files from a previous CI step. Their paths can differ across machines.

```starlark
bosatsu.toolchain(
    release_manifest = "//ci/artifacts:bosatsu-toolchain.json",
    manifest_sha256 = "<SHA-256 from the producing job>",
    compiler_jar = "//ci/artifacts:bosatsu.jar",
    core_library = "//ci/artifacts:core_alpha.bosatsu_lib",
    c_runtime_archive = "//ci/artifacts:bosatsu-c-runtime.tar.gz",
)
```

The full declaration replaces the jar-only tag. The pinned manifest supplies all three expected artifact digests. Register `@bosatsu_toolchains//:runtime` and the target C toolchain when building native targets. For a catalog release, `version` selects the manifest and supplies default download locations. Any explicit artifact pin must equal the manifest entry. The standalone jar-only form remains sufficient for groups using implicit Predef. A request for core or native code with that form must identify the missing release manifest.

Labels identify individual source files declared, for example, with `exports_files`. For provisioned files outside the workspace use `release_manifest_path`, `compiler_jar_path`, `core_library_path` or `c_runtime_archive_path` with an absolute path. For each input choose a label or a path. Preserve the same digest checks for both forms. The files must exist when repository resolution starts.

Give the manifest, jar, core and runtime separate repository inputs with stable labels. Compiler-only registration can read the manifest when supplied and select the jar entry without fetching core, runtime, GC, libuv or target C tools. Materialize the core projection only when requested. This separation also applies to local paths.

**Verification.** For remote artifacts use the repository downloader with the expected SHA-256. For a local label or path, resolve and `watch` the selected source file in a repository rule. Invoke an independently pinned repository-host helper through `repository_ctx.execute`. Ship that helper with the rules for each supported host so it is available before analysis, including on a provisioned offline machine. It streams the file once, copies and hashes the same bytes into a temporary file in the generated repository, compares the expected SHA-256, and renames the verified snapshot into place only on success. A missing file or mismatch aborts resolution, reports the path and, on mismatch, both digests. Extract only the verified snapshot. This path uses neither a downloader nor a cached copy of the artifact, even with a warm repository cache. [Local file watching and execution](https://bazel.build/rules/lib/builtins/repository_ctx#execute) · [Bazel 9.2.0 download-disable check](https://github.com/bazelbuild/bazel/blob/9.2.0/src/main/java/com/google/devtools/build/lib/bazel/repository/downloader/DownloadManager.java#L303)

The runtime input is the release-format tarball containing `c_runtime/`, including headers, sources and `deps.json`. Hash the compressed file, then unpack the verified snapshot. Build the runtime, GC and libuv for the target with the same rules in both modes. Give the jar, core library and runtime separate canonical labels and stable exec paths. The compiler action uses the verified jar, exec JRE and required interface files. Native actions use the verified runtime sources and target toolchain. Absolute host paths and the method of acquisition stay out of action arguments and generated manifests, including worker startup flags. Remote executors receive declared inputs from Bazel.

**Offline fixture.** With Bazel 9.2.0, a disposable repository rule watched an external binary file and invoked a pre-provisioned copy-and-hash helper under `--repository_disable_download`. The first query succeeded with a fresh output base and an empty cache for the selected artifact. Changing the file at the same path made the next query fail with the expected and actual SHA-256 digests. The toolchain acceptance tests repeat this for the manifest, jar, core library and runtime tarball.

**Native compatibility.** Consume a success stamp from `tool verify-toolchain` before producing a native executable. Verify the jar and selected core/runtime inputs against one trusted manifest. For binary core, also check the library’s internal name and version. Compare every reachable reference to core with the manifest, even if a transitive library declares it private. For matched source mode, verify the manifest’s source entries in place of the core binary. When the jar advertises `BuildInfo.cRuntimeArchiveHash`, verify that additional digest against the compressed archive and reject a malformed or conflicting value. An unstamped development jar can use its explicitly pinned CI manifest. Identify the artifact, expected and actual identity or digest, and dependency path on failure. Jar-only builds do not depend on the native stamp. [Existing runtime hash reader](../../core/src/main/scala/dev/bosatsu/cruntime/Command.scala#L107)

**GitHub CI handoff.** The compile-only job can stage just the deploy jar and its SHA-256. For a complete toolchain, archive `c_runtime/` once, embed its BLAKE3 with `BOSATSU_C_RUNTIME_HASH` when assembling the jar, and compile the core library with that jar. Generate the detached manifest from the finished files and the matching source snapshot. Exercise the exact combination with a native test and program that call core externs, then publish the manifest with the qualified artifacts or pass its SHA-256 and local files to the Bazel CI job. The compiler build can finish before the core digest exists. [Release workflow](../../.github/workflows/release.yml) · [Embedded runtime hash](../../build.sbt#L261)

## Compiler implementation checklist

Make `bosatsu tool` the file-oriented interface to the released jar used by Bazel and Make. These commands and flags are proposed. Preserve the existing engine and migrate its repository callers and tests when replacing low-level verbs. Build actions take named files, write only declared outputs, and run offline. Bzlmod dependency resolution and deployment run outside the worker. [Current tool dispatch](../../core/src/main/scala/dev/bosatsu/tool_command/ToolCommand.scala) · [CI callers](../../.github/workflows/ci.yml)

### Commands and files

The consumer milestone needs `headers`, `compile-packages`, `inspect-library` (including descriptor inspection for Bzlmod), `validate-graph`, `verify-toolchain`, `emit-c` and the worker dispatcher. `select-interfaces` is a cache optimization. Source library projection/assembly, history lookup and deployment can ship later.

| Command or entry point | Required behavior |
| --- | --- |
| `tool headers --src FILE ... --out FILE` | Batch-read source headers using Bosatsu's parser. Emit a versioned, sorted list of path, declared name, imports and `exposes` for Gazelle and interface selection. Explicit files support sources generated by other Bazel rules. [DepsCommand](../../core/src/main/scala/dev/bosatsu/tool_command/DepsCommand.scala) · [PackageResolver](../../core/src/main/scala/dev/bosatsu/tool/PackageResolver.scala) |
| `tool select-interfaces` | Take `--headers`, repeated `--direct-interface`, a declared `--projection-map` of file/label/output, and `--error-unused-interfaces`. Follow per-package exposures to closure before projecting. Emit one narrowed file per required label. Report extra providers and missing exposure paths. |
| `tool compile-packages` | Take repeated `--src` and `--direct-interface` files, optional `--interface-labels FILE`, and `--error-unused-interfaces`. Validate each source member’s import/exposure closure, then check the union of selected files. Mixed graphs add `--scope-map FILE` in 0m. Index names/exposures in raw bundles and decode only reachable entries. Attribute each selected package to its declared file and label. Compile in emit mode and write `--packages-out` and `--sig-out`. Predef is implicit. [Current check command](../../core/src/main/scala/dev/bosatsu/tool_command/CheckCommand.scala) |
| `tool library-interface` (producer phase) | Project selected exported package names from local member signatures to the canonical interface format used by imports. |
| `tool inspect-library` | Verify the pinned release and derive exported signatures, the per-root exposure index, a scoped implementation view, membership and the descriptor. Compute exposure metadata from decoded typed exports for existing OSS artifacts. Keep projections independently requestable. |
| `tool validate-graph` | Take `--root`, repeated `--node` manifests, and `--out`. Distinguish direct visibility, loose membership, library boundaries and the separate history graph. Report the two actual paths for a conflict. Support a small declared validation output. Share this validator with full assembly and binary composition where byte identity matters. |
| `tool assemble-library` (producer phase) | Use `--manifest`, `--out` and `--descriptor-out`. Read group implementations, expected public interface, direct public/private dependencies, resolution-only inputs, metadata and optional predecessor via an explicitly declared assembly manifest. Validate local/explicit versions, stamped dependency hashes, URI closure and SemVer rules; compute history using `nextHistory`. Emit the full artifact and its external descriptor. [AssembleCommand](../../core/src/main/scala/dev/bosatsu/tool_command/AssembleCommand.scala) · [LibConfig](../../core/src/main/scala/dev/bosatsu/library/LibConfig.scala) |
| `tool verify-toolchain` | Take `--manifest FILE --compiler-jar FILE --out STAMP` and the inputs requested by the target: `--core-library FILE` or checked `--core-source FILE` entries, and `--runtime-archive FILE` for native builds. Verify component SHA-256, core name/version or source identity, and any advertised jar/runtime digest. Write the stamp only on success. The caller must verify the manifest pin and jar bytes before launching this tool. |
| `tool emit-c` | Require `--runtime-manifest`, `--out` and exactly one root selector. `--main-package NAME` selects the program. `--test-packages FILE` selects discovered private or public entries from that one local implementation bundle in the manifest. Reuse its decoded packages, preserving scope and entry order. Supply effective Predef, scoped dependencies and declared generated headers. Emit C and leave native compilation, linking and test execution to Bazel. [RuntimeCommandSupport](../../core/src/main/scala/dev/bosatsu/tool_command/RuntimeCommandSupport.scala) · [ClangTranspiler](../../core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala) |
| `tool history` (producer/history phase) | Start at a verified artifact and resolve a requested version over authenticated links. Provide a declared-file resolver for hermetic use and separate it from interactive URI fetching. Distinguish unavailable bytes, a bad digest and a version outside the lineage. |
| `tool bazel-worker` | Add one-shot param-file and persistent JSON modes as specified below. Dispatch the allowed file operations in process. Supply a request-local diagnostic sink, file context, exit code and scratch lifetime. |
| `tool inspect-library --descriptors-only` | Read a verified file and return its identity and dependency descriptors, including hashes, mirror URIs and visibility, for the extension's resolver. The full inspection mode derives exported interfaces and exposures after the verified dependency context is available. Both operations take files. Bazel handles acquisition and lockfile updates. |
| `tool publish-artifacts` (producer phase) | `--manifest FILE` verifies the prepared closure and deploys in dependency order outside the worker. |

### File contracts for steps 0e and 0f

All manifests in this section use UTF-8 JSON. Invalid byte sequences fail validation before paths or labels are compared.

`tool compile-packages --interface-labels FILE` reads a version 1 JSON manifest mapping signature paths to Bazel labels.

```json
{"schema_version":1,"interfaces":[{"path":"gen/types.bosatsig","label":"//types:api"}]}
```

When present, the map must contain exactly one entry for every distinct `--direct-interface` path. Paths use the same argument parser and request working directory as the CLI. Repeating a CLI input denotes the same file. Separate files can carry the same label and still count separately for strict checking. An empty bundle, an implicit Predef, or a name supplied solely by a local source cannot consume a file. A file providing even one exposure-only package counts. The mapping contributes diagnostic provenance and adds no files to the action. Run with `--error-unused-interfaces` to require exactness, with or without labels. Both flags leave the package and signature output format unchanged.

For local groups, `tool validate-graph --root ROOT.json --node DEP.json ... --out STAMP` reads version 1 node manifests. The root argument is a node file. A leaf has an empty `deps` list. This root declares one member and one dependency.

```json
{
  "schema_version": 1,
  "kind": "group",
  "label": "//app:main",
  "members": [{"package": "Demo/Main", "source": "app/Main.bosatsu"}],
  "deps": [{"label": "//lib:api", "manifest": "gen/lib/api.node.json"}]
}
```

Emit the canonical group label from the provider, including through Bazel aliases. Use one stable source artifact identity per member throughout the graph (for Bazel, the file’s exec path, including its generated-output or external repository prefix). The CLI compares identities without opening sources. Dependency manifest paths are execroot/request-relative in the same way as the command-line paths, including when the parent manifest lives in a subdirectory. Supply each dependency manifest explicitly with `--node`. The validator checks the expected group label at each edge. A repeated file in a diamond is read once. Distinct files claiming the same group identity are an error.

For local validation, add `--local`. Generate a local root whose edges point to direct groups’ membership files and pass only those files with `--node`. A membership file carries the group’s canonical label and its source/package pairs.

```json
{"schema_version":1,"kind":"group-members","label":"//lib:api","members":[{"package":"Demo/Api","source":"lib/Api.bosatsu"}]}
```

Emit the local root, full root and membership file from the same group provider and parsed source facts. Each root lists the same direct labels. The local root points to their membership files and the full root points to their topology files. A direct group’s outgoing-edge edit can now leave every input to its parent’s local check byte-identical. Local checks also accept full direct nodes for callers using a single manifest per group. A membership file can only be a direct input of a local scan. The root of either scan and every node in a full scan require topology. Imported libraries have their own scope and cannot supply loose membership projections.

For a full scan pass all reachable topology files and omit `--local`. Missing edges, undeclared or surplus manifests, membership collisions and full-graph cycles fail with diagnostics. A successful run writes the same small `ok` stamp for every valid topology. The JVM file runner publishes the stamp only on success. Neither operation reads implementations, and library nodes get a separate scoped contract in 0m.

### Local C emission (steps 0g and 0h)

The per-action runtime manifest describes the compiled inputs to one C emission. Use one request-relative path per reachable loose group and include the selected test bundle itself. The version 1 local scope has the following form.

```json
{
  "schema_version": 1,
  "kind": "local",
  "packages": ["gen/shared.bosatsu_package", "gen/math.bosatsu_package"],
  "externals": [{"package": "Demo/Native", "symbols": ["native_id"]}]
}
```

```sh
bosatsu tool emit-c --runtime-manifest gen/runtime.json \
  --test-packages gen/math.bosatsu_package --out gen/runner.c \
  --extern-header-out gen/headers/bosatsu_ext_Demo_l_Native.h
```

- **Implementations.** The compiler decodes the bundles together so references can cross file boundaries. It reads each distinct declared path once and adds the pinned jar’s compiled Predef. The implementation bundle from `compile-packages` records the BLAKE3 of each selected external package’s canonical public interface, including exposure-only owners. The emitter verifies those requirements against the supplied implementations before generating C. Adding an enum case can invalidate an old exhaustive match even if every imported value keeps the same nominal type. A body edit, opaque field edit or edit to an unselected sibling can preserve the requirements. Paths, labels, implicit Predef and implementation hashes stay out of this metadata. The public signature format stays the same. Empty bundles, duplicate package owners, a bundled Predef and missing implementations are errors. A shared group reached through a diamond has one implementation. Pass an empty `externals` array for a graph needing only built-in Predef bindings.
- **Historical bundles.** When the bundle predates interface digests, the emitter checks recorded value types and the exported identities of constructors, types and aliases. A legacy bundle can still use values whose types are built from Predef or its own packages. If the caller uses a nominal type from a separately supplied bundle, recompile it with `compile-packages` so the emitter can verify the layout against which its patterns and constructors were compiled. Older public signature files are valid inputs to that rebuild.
- **Root selection.** The selected path identifies the packages to search within the manifest. Discover private and public test entries before tree shaking. A package without a suite is skipped. An empty selection reports the bundle and the searched package names. An invalid test order in a selected package fails emission. For a selected `ProgTest`, the native harness needs an unboxed one-field wrapper around a callback from `List[String]` to the runtime’s external `Prog[err, Test]`. Emission checks those definitions in the supplied implementation. Other bundles supply implementations and cannot contribute tests to the runner.
- **Native bindings.** A provider lists the Bosatsu package and external binding names it supplies under the existing C naming and calling convention. The compiler checks that each symbol has an external declaration in the actual bundle. A reference which still needs an external C symbol after tree shaking and optimization requires a binding. Predef uses the runtime paired with the jar. Other packages, including a source-built `Bosatsu/Prog`, declare the symbols they supply. The `Bosatsu/` namespace is reserved for the compiler and the core library, and the compiler gives some of its bindings built-in meaning by name. Matchless lowering and C generation always apply the `Bosatsu/Num/Int64` intrinsics: `eq_Int64` and `cmp_Int64` become the builtin compare, and `int_to_Int64` and `int_low_bits_to_Int64` applied to a literal become constants. A native binding therefore cannot change their semantics. Every `Bosatsu/Num/Int64` entering a compilation, whether type-checked from source or decoded from a bundle, must declare `external type Int64` and give each intrinsic it defines the core library's external declaration and type. Compilation and emission reject one that does not. Library builds go further and reject every `Bosatsu/` package outside the core library. The release compatibility and core-source identity checks belong to toolchain qualification.
- **Headers and link inputs.** Repeat `--extern-header-out` to generate particular declaration headers. Their basenames must match the C backend’s package header names. The C and header destinations must name distinct files on the action filesystem, including when symlinks or case folding give two paths the same destination. They must also be distinct from the runtime inputs. A provider can supply its own matching header. The emitter reads the symbol list. Bazel passes the corresponding headers, C objects, runtime, GC and libuv to its target compile and link actions. A manifest declaration by itself supplies no native implementation.
- **Action keys.** Declare the runtime manifest, compiler and every listed implementation bundle as C emission inputs. Sort the manifest contents in the producing rule. Equivalent bundle enumeration orders generate identical C. An implementation-only edit can change C with a byte-identical group signature. Host compiler flags, installation paths and archive locations belong to the native compile/link actions. A sandboxed Bazel fixture ran zero actions on a no-op. A helper body edit reran its package compile and C emission while the consuming test group hit its compile cache. A private test edit reran the test group compile and C emission. Local emission accepts only the local scope schema. Steps 0l–0n add verified imported scopes and caller-relative binding resolution.

The native integration fixture compiles `Bosatsu/Prog`, a user external, a helper group and the selected test group in separate compiler invocations. It gives emission only the bundles and manifest in a fresh directory, links the resulting C with the CI runtime, and checks the exits of passing and failing plain and effectful suites. A dependency supplies a deliberately failing test. [Native fixture](../../scripts/test_emit_c.py) · [CLI tests](../../cli/src/test/scala/dev/bosatsu/EmitCCommandTest.scala)

### Changes beneath the CLI

1. **Package and import interfaces.** Implement deterministic signatures and per-package exposure metadata, including a projection for legacy imported libraries. Decode required foreign definitions from the declared providers. Preserve semantic constructor order, aliases, kinds and ABI facts. Avoid embedding unrelated transitive API or release data. [ProtoConverter](../../core/src/main/scala/dev/bosatsu/ProtoConverter.scala)
2. **Resolution and namespaces.** Preserve file paths/labels when following source imports and interface exposure chains. Reject missing/unused interface files before flattening the type environment. Check ownership of loose package names together, include Predef, and retain caller-relative import bindings and versioned library scopes. Resolve implementations for native program and test emission. [JVM file reader](../../cli/src/main/scala/dev/bosatsu/IOPlatformIO.scala) · [Dependency decoder](../../core/src/main/scala/dev/bosatsu/library/DecodedLibrary.scala)
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

The one-shot dispatcher reads exactly one `@file` argument as strict UTF-8. It splits on LF and CRLF; a CR immediately before LF is part of the delimiter, while other characters remain in the argument. A terminal line ending ends the last line and does not add another argument. An empty argument is an empty line between line endings, including an empty final argument encoded by a blank line followed by the terminal line ending. No shell quoting or tokenization is applied, and an argument beginning with `@` is not expanded recursively. Unreadable files and malformed UTF-8 use the ordinary CLI diagnostic and nonzero status path.

The shared worker-operation classification currently registers `headers`, `compile-packages`, `validate-graph`, and `emit-c`. The persistent transport must use this same classification when it is added; later inspection and verification commands register through `BazelWorker.Operation` after their implementations land. The dispatcher rejects legacy, publishing, fetching, and process-executing operations, and it does not expose filesystem inference cache options. The ordinary interactive CLI retains its existing commands.

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

The JVM file runner loads the jar’s bundled JNA bridge when replacing a changed, existing POSIX output. Point `-Djna.tmpdir` at a writable, private directory under the worker’s scratch root when launching the JVM. Use the action scratch directory for a one-shot invocation. This keeps native library extraction local to that process. Building into an empty output directory and retaining byte-identical outputs both bypass the bridge. Exercise a local rebuild with an empty `PATH` and a read-only home during toolchain qualification.

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

Ship a Go extension in a custom `gazelle_binary`. Index local names, imports and exposures with the pinned batch header reader. Default to grouping by Bazel directory, preserve hand-chosen groups and allow singletons. The toolchain index maps standard packages to `@bosatsu_toolchains//:core` or their verified source replacements. The import extension generates a repository containing a package/exposure index and consumer-visible aliases to the resolved third-party libraries. Bring the generated index into scope with `use_repo` and supply it as a declared runfile to Gazelle. This gives an exposed foreign package a usable label even when it arrived transitively. [Extending Gazelle](https://github.com/bazel-contrib/bazel-gazelle/blob/master/extend.md) · [Header parser](../../core/src/main/scala/dev/bosatsu/tool/PackageResolver.scala#L75)

For verified imports, resolve each member’s package/exposure closure in its own binding context before taking the union of labels for a group. For each group compute the exact required package names using the same import/exposure algorithm as the compiler. Follow each named API's exposures recursively with a visited set, including exposures crossing a source/import boundary. Resolve names before collapsing by provider label. Remove group-local names and Predef from the emitted deps, deduplicate labels, and write real dependency edges. Record a witness path for missing providers and diagnose ambiguous names with explicit override support. Do not walk ordinary imports of other packages or all exports of a provider. If grouping induces a Bazel target cycle, report the underlying imports/exposures and split or regroup. Re-running Gazelle removes stale generated edges and preserves intentional manual fields and `# keep`; strict checking still flags unused retained labels.

Preserve hand-authored `bosatsu_test` targets. A package-local directive such as `# gazelle:bosatsu_test math` can generate `bosatsu_test(name = "math_test", packages = ":math")` for an explicitly selected group. Recognize an existing test for the group before generating another one, and diagnose a conflicting target at the chosen label. Test bindings can have arbitrary names and inferred types. The typed compiler discovers them when building the runner.

When a source library producer is added, extend this same index with its exported packages. Generate membership and library visibility from explicit policy and typed exposures. No group owner field or library root declaration is needed for packages-only generation.

## Delivery order and acceptance tests

Merge the consumer-facing `tool` changes first. The subsequent Bazel milestones are independently usable in the order **compiler jar and JRE → packages → C runtime and target C toolchain → test → binary → import**. Package compilation can ship as soon as its file commands and compiler toolchain work. Gazelle can grow alongside each rule.

### Implementation tracker

Stage 0 lands the compiler and release-workflow changes as isolated PRs. Each has a check that can run without a Bosatsu Bazel rule. Stages 1–6 use the acceptance tests detailed below. [Command contracts](#commands-and-files) · [Worker protocol](#persistent-jvm-worker)

Change **Complete?** to **Yes** when the implementation has landed and its acceptance check passes. Link the implementing PR in the row. If the design changes, update the row and the corresponding design section in the same PR.

| Step | Complete? | Change and prerequisites | Acceptance check |
| --- | --- | --- | --- |
| 0a. Request-scoped execution | Yes | Factor a file-command runner out of JVM `Main` and `PathModule`. Return status and diagnostics to the caller, isolate paths and scratch files, and stage declared outputs on success. | Run good, invalid and good requests in the same JVM and verify diagnostics, outputs and exit statuses. Check a dangling output link on first build and rebuild, rollback of a write-only output, preservation of group access, and a bad output path followed by a successful request. [PR #2463](https://github.com/johnynek/bosatsu/pull/2463) · [Runner tests](../../cli/src/test/scala/dev/bosatsu/FileCommandRunnerTest.scala) · [Output publication tests](../../cli/src/test/scala/dev/bosatsu/StagedOutputsTest.scala) |
| 0b. Source headers | Yes | Add `tool headers` over explicit source files using the language parser. Emit stable package names, imports and `exposes`. | Reorder input files and change a private body. The header bytes stay identical. Capture malformed or misplaced declarations with file-level diagnostics and preserve the last valid index. [PR #2463](https://github.com/johnynek/bosatsu/pull/2463) · [Header command tests](../../cli/src/test/scala/dev/bosatsu/HeadersCommandTest.scala) |
| 0c. Canonical package interfaces | Yes | Define the deterministic per-package signature and exposure encoding and decoder. Resolve foreign definitions from explicitly supplied interfaces. Preserve required type and ABI information, including opaque types. | Round-trip multi-package signatures, foreign types, constructors and aliases. With narrowing disabled, index X and Sibling and resolve only the selected root’s foreign references. Private-body edits preserve signature bytes and required export edits change them. Check warm/cold agreement with stale embedded legacy aliases and read a 3,000-package re-export chain through both readers. [PR #2464](https://github.com/johnynek/bosatsu/pull/2464) · [Signature tests](../../core/src/test/scala/dev/bosatsu/PackageSignaturesTest.scala) |
| 0d. Batched file compilation | Yes | On 0a and 0c, add `tool compile-packages` with explicit sources/interfaces and named bundle and signature outputs. Preserve private bindings needed for test discovery and ordering. | Compile two groups from files, using only the first group's signature to type-check the second. Decode both outputs, check private tests survive and ensure implicit Predef is handled once per compilation. Supply an exposed owner as source and as a signature, then remove its required type and check that both builds fail. [PR #2464](https://github.com/johnynek/bosatsu/pull/2464) · [Group command tests](../../cli/src/test/scala/dev/bosatsu/CompilePackagesCommandTest.scala) |
| 0e. Strict direct interfaces | Yes | On 0b–0d, add the shared import/exposure resolver, per-file label provenance and `--error-unused-interfaces`. Apply the check before combining or narrowing files. | Require Y when X exposes Y, reject private P and cover recursive exposures. With X and Sibling in one raw signature and only Sibling exposing Y: A importing X succeeds without Y, rejects an extra Y, and importing Sibling diagnoses missing Y until it is supplied. Exercise fresh and reused file-command requests. Repeat the matrix over the worker transport in 0j–0k. [Strict interface fixtures](../../cli/src/test/scala/dev/bosatsu/StrictInterfacesCommandTest.scala) |
| 0f. Structural graph checks | Yes | Add `tool validate-graph` for explicit root and node manifests. Track group identities, reachable names and parent edges. Start with local group graphs. | Verify local checks read only direct memberships and stay cached when only a direct child’s outgoing edges change. Reject a membership-only file in a full scan. For chains of N and 2N groups, count manifests visited across validators for one requested root and require linear growth. Visit a shared diamond node once and report both paths for a collision. |
| 0g. Local native compilation scope | Yes | On 0d, decode the runtime manifest and compiled local bundles, supply effective Predef exactly once, and resolve native extern requirements. Feed the existing C backend a reusable compilation scope. | Decode separately compiled groups with an empty library graph in a clean directory. Resolve ordinary and Predef references. Reject duplicate or missing implementations, stale imports and changed exposed-owner layouts. Check that private edits preserve caller bytes. [Scope tests](../../core/src/test/scala/dev/bosatsu/LocalRuntimeScopeTest.scala) |
| 0h. Test C emission | Yes | On 0g, add `tool emit-c --test-packages`. Discover test roots in the one selected bundle, tree-shake from those bindings and emit declared C outputs. | Generate C from compiled inputs only. Run a native fixture for private `Test` and `ProgTest`, passing and failing assertions, an empty selection and a dependency whose suite must be excluded. Reject incompatible `ProgTest` layouts, missing reachable native bindings and a mismatched `Bosatsu/Num/Int64`. Fold literal Int64 conversions from the core source and protect declared inputs from aliased outputs. [Native fixture](../../scripts/test_emit_c.py) |
| 0i. Program C emission | No | On 0g, add the alternative `tool emit-c --main-package` selector, validate the root group and main type, and emit declared C outputs. | From the all-source demo bundles, generate C and link a program that exits 7. Reject a main outside the selected group or with an invalid type. Neither emission mode invokes a host C compiler. |
| 0j. One-shot worker arguments | Yes | On 0a and 0d, add the `tool bazel-worker @params` dispatcher for file operations. Keep filenames and multiline parameter-file semantics consistent with the standalone CLI. | Run the candidate jar on a params file containing paths with spaces and non-ASCII characters. Compare output bytes, diagnostics and status with the direct invocation. [Issue #2475](https://github.com/johnynek/bosatsu/issues/2475) · [Candidate-jar harness](../../scripts/test_bazel_worker.py) · [Launcher passthrough test](../../scripts/test_launcher_passthrough.sh) · [Worker unit tests](../../cli/src/test/scala/dev/bosatsu/BazelWorkerTest.scala) |
| 0k. Persistent JSON worker | No | On 0j, implement `--persistent_worker`, singleplex line framing, per-request diagnostics, flush and EOF handling. Reuse the JIT and immutable compiler resources. | Drive the jar on stdin/stdout with good/bad/good requests, changed bytes at the same path, omitted IDs, missing digests and a process restart. Compare with one-shot results. |
| 0l. Inspect verified imports | No | On 0c, add `tool inspect-library` over declared files. Verify artifact digests and name/version. Project each exported interface with its owner and provider identities for its exposure edges. Emit the scoped implementation view and descriptor. | Read an existing released library with verified dependencies. Recover scoped exposure edges for an unindexed interface. Check the interface projection is stable across implementation-only changes. Reject a bad hash or incomplete declared dependency context, including a second diamond edge claiming another digest for an already verified name/version. |
| 0m. Validate imported scopes | No | On 0f and 0l, extend structural validation, signature selection and batched compilation to per-member import bindings and the selected toolchain core. | Accept a shared diamond and legal private versions, including disjoint member closures in one batch. Reject two versions of one library visible to one package and show both paths, including when the versions contribute different package names or identical signature bytes. Coalesce identical core references. Reject an incompatible transitive core, including on a private path, and report the path. |
| 0n. Emit C for mixed graphs | No | On 0h, 0i, 0l and 0m, teach both C emission modes to resolve imported implementations and native bindings within their library scopes. | Generate a working program and test using a pinned library. Link and run two private releases exporting the same package and symbol with different signatures. Check both results and diagnose incompatible native bindings. |
| 0o. Inspect import descriptors | No | On 0l, expose file-only descriptor inspection to the repository-host resolver, including legacy hashes and scoped edges. | Enumerate both versions in a private-release fixture with their referring edges. From a pinned Parser artifact enumerate its closure, verify child hashes before trusting child descriptors, derive SHA-256 for a BLAKE3-only edge, and reject a tampered child. Repeated inspection yields the same repository inputs and package index. |
| 0p. Compiler/core/runtime compatibility | No | Add `tool verify-toolchain` against a detached manifest. Verify only requested components, with an additional archive check for jars advertising a runtime hash. | Accept the matched tuple. Substitute a separately SHA-valid jar, core or runtime in turn and reject each. Include core with the same name/version and different bytes, incorrect embedded core name/version, and an edited core source replacement. Test missing and malformed embedded runtime hashes under the documented policy and no success stamp on failure. |
| 0q. CLI cutover and artifact qualification | No | Exercise the candidate deploy jar through its file CLI and worker entry points. Migrate CI, sanitizer and Valgrind callers when retiring superseded commands. Have release/CI generate and pin the manifest after building the jar, core and runtime archive. | Round-trip the manifest against the staged files. Run the same native program using `Bosatsu/Prog` and a `ProgTest` using a core external against the exact tuple that will be distributed. Hand off the jar alone for compiler-only CI or the manifest and three local files for full CI. |
| Optional after 0e. Narrow interfaces | No | Add `tool select-interfaces` as a separate cache optimization. The packages rule can ship with whole direct signatures. | Compare raw and narrowed builds on the X/Sibling/Y fixture. Both enforce the same strict deps on the original declarations. Measure action misses after unrelated edits to a wide provider. |
| 1. JVM compiler toolchain | No | Register the pinned jar and exec JRE. Support release-manifest selection and the local jar-only form. | Resolve a valid local jar with downloads disabled and an empty artifact cache. Reject an altered manifest or substituted jar. Run the compiler directly on a Predef-only group with core and runtime files absent. [Stage 1](#1-register-the-jvm-compiler-toolchain) |
| 2a. Package compilation | No | Build `bosatsu_packages` and expose `@bosatsu_toolchains//:core` through the standard library provider. | Produce both artifacts with the compiler alone. Compile a group importing `Bosatsu/Prog` against the matched core interface with the runtime unavailable. Reject a same-version core with the wrong digest and an unused core dep. Exercise the wide-signature and chain checks. [Stage 2](#2-compile-with-bosatsu_packages) |
| 2b. Package caching and Gazelle | No | Enforce strict dependencies and generate local BUILD rules. | Check cache hits after private edits and verify Gazelle generates the same BUILD files on its second run. [Stage 2](#2-compile-with-bosatsu_packages) |
| 3. Native runtime and toolchain | No | Build the runtime, GC and libuv with pinned target tools. Gate native composition on the verified manifest components. | Resolve local and catalog inputs. Fail before link for a swapped core or archive, a jar/runtime BLAKE3 disagreement or a conflicting core carried by an import. Cover matched core source mode, fixed-jar cache hits and rebuilt-jar invalidation. [Stage 3](#3-add-the-c-runtime-and-target-c-toolchain) |
| 4. In-package tests | No | Add `bosatsu_test` and test label generation. | Run a private suite and diagnose a group with no test entries. Verify one mandatory root scan across a chain of groups for the test executable. [Stage 4](#4-run-tests-with-bosatsu_test) |
| 5. Native programs | No | Add `bosatsu_binary` and main declarations for Gazelle. | Run the all-source demo and confirm a binary and test sharing a group compile that group once. [Stage 5](#5-build-programs-with-bosatsu_binary) |
| 6a. Verified imports | No | Implement the MODULE tag, resolver and vendored BUILD form of `bosatsu_import`. Preserve separately pinned releases under distinct labels. | First build with one third-party tag resolves and populates `MODULE.bazel.lock`. Changing the tag is rejected in locked CI and refreshes the lock on a normal build. Resolve a private diamond with two versions from the same Bosatsu library. Reject bad root/child hashes and conflicting bytes at one name/version. Verify offline reuse after prefetch. [Stage 6](#6-consume-external-code-with-bosatsu_import) |
| 6b. Mixed consumer and Gazelle | No | Connect third-party libraries to source groups, the toolchain core and Gazelle. | Build and test a local group importing Parser and the matched core. Reject an incompatible core reached through Parser. Generate the same BUILD deps twice. Link and run both private releases, reject a visible version conflict, and preserve downstream type-check hits for an implementation-only edit. [Stage 6](#6-consume-external-code-with-bosatsu_import) |

The producer-only commands `library-interface`, `assemble-library`, `history` and `publish-artifacts` belong to the optional producer phase below. Existing published libraries remain inputs to the consumer qualification.

### 1. Register the JVM compiler toolchain

Pin the release manifest in the catalog and register its verified compiler jar, exec JRE and worker-capable launcher. Accept local label/path overrides and a jar-only declaration. Exercise missing files, manifest tampering, in-place replacement with a warm cache and a separately SHA-valid jar from a different release. CI must build a Predef-only group without resolving core, the runtime or target tools. Equivalent verified compiler bytes at stable paths yield the same declared inputs.

### 2. Compile with bosatsu_packages

- **2a. Group providers and actions.** Add the batched compile, strict direct interface edges, structural validations and deterministic provider/output groups. Build `//app:api` in a fixture whose BUILD file loads only `bosatsu_packages` and whose MODULE registers only the compiler. Verify the `.bosatsig` and `.bosatsu_package` outputs, then compile a consumer using only the signature. Inspect repository fetches and `aquery` to confirm the runtime archive, GC, libuv and C toolchain were unnecessary. Include a group with an external declaration. On the X/Sibling/Y fixture, build once with whole direct signatures and once with optional projections when available. Assert the same pass/fail matrix from 0e. Build a second group against the declared toolchain core using `Bosatsu/Prog`. Supply only the verified core interface to its compile action. Confirm the runtime is never fetched, substituting another core with the same name/version is rejected before use, and an unused core edge is an error. Use a worker and a sandboxed one-shot action from clean output bases and compare outputs.
- **2b. Cache and Gazelle.** Verify cold, no-op, private-body and exposed-type edits with execution logs. For A importing X that exposes Y and privately uses P, accept exactly X and Y as A's compile providers. Exercise intra-group imports and a provider with multiple packages. Edit a direct child’s outgoing edges and verify the parent’s local validation stays cached while a requested full scan re-executes. Inspect executed validation actions on N- and 2N-group chains and a diamond. A plain group build runs local checks. Request `--output_groups=bosatsu_graph_check` on the tip and require exactly one full scan visiting each reachable manifest once. Count all validation input visits, including direct edges in local checks. The total for either chain scales linearly. Land Gazelle's local grouping, header index and exposure resolution as its own PR. Regenerate twice and compare BUILD files.

**Compiler evidence.** In a temporary directory with an empty `PATH`, a locally built deploy jar run by an explicit JVM produced both artifacts with today's `tool check`. A second package compiled using the first package's signature as its only dependency input. This exercises the JVM file path. The Bazel acceptance check above is still to be implemented. [Compiler output handling](../../core/src/main/scala/dev/bosatsu/tool/Output.scala#L88)

### 3. Add the C runtime and target C toolchain

Register the runtime, compile source-built static GC and libuv, and link with the target C toolchain/sysroot. Exercise local and catalog manifest inputs with downloads disabled after prefetch. Substitute each of the three components with a different independently pinned file. The expected manifest digest for that component must win. Also test a malformed or mismatched jar-advertised archive hash and a core source file with a changed digest. Verify the wrong core or runtime is rejected before link and that a Predef-only package build still works when those unused files are unavailable. Exercise GC allocation and libuv in a native fixture with an empty home. Change only a native define and verify group compiles stay cached. Then rebuild the jar with a new embedded archive digest and its matching manifest and verify a compiler action miss. Qualify Linux first and macOS separately. Cross-builds must not execute target programs during compilation.

### 4. Run tests with bosatsu_test

Add the explicit test rule taking one group label. Generate the harness from that compiled bundle, compile/link with the stage 3 runtime and C toolchain and let Bazel run it. Run targeted and wildcard tests for private helpers, plain `Test` and `ProgTest`, invalid ordering and an entirely testless selected group whose dependency has tests. Run a private `ProgTest` calling `Bosatsu/Prog` externals from the matched core and runtime. Check assertion failures and runfiles. For a single test root on the chain fixture, verify one full structural scan and linear total manifest visits. A private assertion edit keeps downstream group type checks cached. A data-only edit reruns the test without compiling Bosatsu. Add Gazelle's explicit test-target generation and preservation checks.

### 5. Build programs with bosatsu_binary

Add the binary rule with one root group and one main package. Build and run `//app:demo` with the toolchain core, then with its manifest-verified `Bosatsu/Prog` source replacement and an empty library graph. Both executables must exit 7. Reject a duplicate provider or modified replacement before linking. Build a program and test sharing a group in the same target configuration and confirm one `BosatsuCompile` action for that group with `aquery`. Inspect C emission and target compile/link actions, and verify one full structural scan for the requested binary. Compare output hashes across clean roots and check remote cache reuse, including equivalent locally staged and catalog artifacts. Add Gazelle's explicit main declarations.

### 6. Consume external code with bosatsu_import

- **6a. Verified repository and BUILD rule.** Implement the separate `bosatsu_imports` extension and the pre-fetched/vendored BUILD rule. In a fresh workspace serve the fixture Parser release, declare its URI, version and SHA-256 directly in MODULE, then run `bazel build //app:demo`. Check the generated repositories, authenticated transitive pins and `MODULE.bazel.lock`. Change the tag, verify `--lockfile_mode=error` detects stale resolution, and let an ordinary build update it. A second build produces no lock diff. Prefetch and rebuild in a network-disabled executor. Cover bad hashes, missing providers, duplicate packages, legal private versions and a shared diamond.
- **6b. Mixed consumer and Gazelle.** Make `Demo/Main` import `Parser/Text` as well as the toolchain’s `Bosatsu/Prog`. Exercise source-to-import and import-to-import exposures and Gazelle’s generated index. Verify that Parser’s reference to the matched core coalesces with the explicit core edge, then replace that reference with another core release on a private path and require a diagnostic before native linking. Run the two-private-version fixture and its visible-conflict variant from [Private releases and package visibility](#private-releases-and-package-visibility). Run tests and the program, regenerate BUILD files twice, and check cache behavior on implementation-only and public-API edits.

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

Use the same `libraries` module extension and Parser tag as in the third-party example. Add the two predecessors directly in `MODULE.bazel`. Their repository names distinguish them from the source libraries under development.

```starlark
libraries.bosatsu_import(
    name = "common_prev",
    library_name = "common",
    version = "2.0.0",
    uris = ["https://releases.example.org/common-v2.0.0.bosatsu_lib"],
    sha256 = "<SHA-256 of Common 2.0.0>",
)
libraries.bosatsu_import(
    name = "orders_prev",
    library_name = "orders",
    version = "1.3.0",
    uris = ["https://releases.example.org/orders-v1.3.0.bosatsu_lib"],
    sha256 = "<SHA-256 of Orders 1.3.0>",
)
use_repo(libraries, "common_prev", "orders_prev")
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

- **Verify bytes starting from a trusted pin.** The importing project's MODULE tag pins the root artifact. Verify its downloaded bytes and expected name/version. For each dependency or historical edge followed, obtain the named artifact from a recorded URI, verify its hashes, and check its name and version. A mirror is usable if it returns the same bytes. This authenticates the serialized packages contained in each full library. The root's own digest lives outside the bytes it covers. Adding `sha256:<hex>` to Bosatsu's descriptor hash reader/verifier requires extending the current BLAKE3-only hash parser. Keep accepting existing BLAKE3 releases and verify both algorithms for new release descriptors. [Bosatsu hash reader](../../core/src/main/scala/dev/bosatsu/hashing/Algo.scala) · [Bazel download integrity](https://bazel.build/rules/lib/repo/http)
- **Publish in dependency order.** The runnable library or CI consumes the built artifacts and their descriptors. Upload Common, verify its bytes at the advertised URI, then expose Orders. For pre-existing dependencies, verify availability and digest before exposing the parent. Use create-only or conditional operations for immutable coordinates. A retry accepts already published identical bytes and rejects conflicting bytes. If Orders fails after Common is uploaded, reuse Common's verified release. Publish the root descriptor or release index after its closure is available. The existing GitHub release asset workflow is a natural first transport. [Release workflow](../../.github/workflows/release.yml)
- **Run the library target.** With the declarations above, `bazel run //orders:orders` can deploy the prepared closure. It receives the exact versioned build outputs as runfiles. A target lacking publish metadata reports which fields are missing before attempting network access. Consumers can declare the published coordinates in MODULE and use their pinned artifacts as predecessors for the next releases.
