---
issue: 2342
priority: 3
touch_paths:
  - docs/design/2342-document-the-libuv-c-runtime-integration-contract.md
  - c_runtime/deps.json
  - core/src/main/scala/dev/bosatsu/cruntime/CDeps.scala
  - core/src/main/scala/dev/bosatsu/cruntime/VendoredDeps.scala
  - core/src/test/scala/dev/bosatsu/cruntime/CDepsTest.scala
  - core/src/test/scala/dev/bosatsu/cruntime/VendoredDepsTest.scala
  - c_runtime/Makefile
  - c_runtime/bosatsu_ext_Bosatsu_l_Prog.c
  - c_runtime/bosatsu_ext_Bosatsu_l_Prog.h
  - c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c
  - core/src/main/scala/dev/bosatsu/codegen/clang/ClangGen.scala
  - core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala
  - scripts/test_basic.sh
depends_on: []
estimated_size: M
generated_at: 2026-04-28T18:24:29Z
---

# Document the libuv C Runtime Integration Contract

_Issue: #2342 (https://github.com/johnynek/bosatsu/issues/2342)_

## Summary

Adds a durable reference contract for the libuv-backed C runtime work, including the accepted dependency pin, static CMake recipe, metadata/linking expectations, GC/thread safety rules, event-loop ownership, Prog suspension architecture, IO/Core migration order, testing expectations, and rollout risks.

## Context

Bosatsu's C backend currently builds and links a small platform runtime, generated C, and vendored native dependencies through `c_runtime/deps.json`, `CDeps`, and `VendoredDeps`. The current vendored dependency is Boehm GC 8.2.8 with `threadsafe=true`; that path already contributes `-DGC_THREADS` to both runtime and generated C compile flags. `CDeps` already reserves the recipe name `libuv-cmake-static`, and existing tests already exercise dependency ordering and `libuv.a` static-library naming, but `VendoredDeps.runRecipe` does not yet implement the libuv recipe.

The current Prog runtime is synchronous. `Bosatsu/Prog` values use variants `Pure`, `Raise`, `FlatMap`, `Recover`, `ApplyFix`, and `Effect`; `Effect(arg, f)` is interpreted by calling `f(arg)` immediately inside the same execution loop. `bsts_Bosatsu_Prog_run_main` and `bsts_Bosatsu_Prog_run_test` allocate argv/test input and then call the same private `bsts_Bosatsu_Prog_run` loop. Generated C `main` and generated test runners call `GC_init()`, `init_statics()`, register `atexit(free_statics)`, and then call the runtime runner.

The libuv integration must preserve that public Bosatsu surface while changing the C runtime execution engine under Prog effects.

## Dependency Pin

Add libuv as a second vendored dependency in `c_runtime/deps.json` after `bdwgc`:

- `name`: `libuv`
- `version`: `1.52.1`
- `uris`: `https://dist.libuv.org/dist/v1.52.1/libuv-v1.52.1.tar.gz`
- `hash`: `blake3:433979d1027ec72d546e1e4440e193a9d587f1378a8405299d6f219d23c215b7`
- `source_subdir`: `libuv-v1.52.1`
- `recipe`: `libuv-cmake-static`
- `dependencies`: omit or use `[]`; libuv does not depend on `bdwgc` at build time.

The hash above is for the official upstream source tarball at `dist.libuv.org`, not for a GitHub auto-generated archive. Do not use `libuv-v1.52.1-dist.tar.gz` for this manifest entry because its top-level packaging differs from the source tarball contract above.

## Vendored Build Contract

`CDeps.LibuvCmakeStatic` remains the canonical recipe identifier. `VendoredDeps.staticLibFileName` already maps that recipe to `libuv.a`; downstream work should keep that behavior and add recipe execution plus metadata, not introduce a parallel naming scheme.

The static CMake recipe must run the same two-step shape as bdwgc: configure with `cmake -S <sourceRoot> -B <buildDir> ...`, then `cmake --build <buildDir> --target install`. Required configure arguments are:

- `-DCMAKE_BUILD_TYPE=Debug` when profile is `debug`, otherwise `Release`.
- `-DCMAKE_INSTALL_PREFIX=<prefix>`.
- `-DLIBUV_BUILD_SHARED=OFF` so the installed artifact is the static `uv_a` target.
- `-DBUILD_TESTING=OFF`.
- `-DLIBUV_BUILD_TESTS=OFF` and `-DLIBUV_BUILD_BENCH=OFF` to avoid depending on libuv test/bench target defaults.

The recipe should propagate tracked compiler environment in the same spirit as bdwgc. If `CFLAGS` is present in `BuildContext.relevant_env`, pass it through as `-DCMAKE_C_FLAGS=<value>` without adding Bosatsu-specific GC flags to libuv. `GC_THREADS` is a Bosatsu runtime/generated-code requirement, not a libuv build option.

Metadata expectations:

- `include_dirs` contains only the installed `<prefix>/include` path recorded under the final cache directory.
- `static_libs` contains only the installed `<prefix>/lib/libuv.a` path recorded under the final cache directory.
- `runtime_requirements` for libuv is empty for both `bosatsu_runtime_cppflags` and `generated_c_cppflags`.
- `system_link_flags` must contain only transitive system flags required to link the static libuv archive on the current target. Parse `<prefix>/lib/pkgconfig/libuv-static.pc` after install and filter out `-L...` plus the self library token (`-l:libuv.a`, `-luv`, or an absolute/path spelling if it appears). Preserve platform flags such as `-pthread`, `-ldl`, `-lrt`, `-lkstat`, `-lsendfile`, `-lsocket`, `-lws2_32`, `-lpsapi`, `-liphlpapi`, `-luserenv`, etc. when libuv reports them.
- The pkg-config filtering helper must not hardcode bdwgc-specific exclusions such as only `-lgc`. Generalize the parser to accept the dependency's own self-link tokens, or an explicit exclusion set derived from metadata, so future vendored static libraries can reuse the same logic without leaking their self library into `system_link_flags`.
- `BuildInputs.linkFlags` must continue to emit concrete static archive paths before system flags. Do not replace metadata with pkg-config invocation at generated-program link time.

Any change to libuv recipe arguments, source pin, metadata filtering, or build-key inputs should either bump `recipe_version` or be intentionally proven compatible with existing cache entries.

## Runtime Architecture Contract

The runtime should introduce an internal execution context that owns the event loop and the currently suspended Prog continuation. This structure is a C runtime implementation detail and must not become a Bosatsu public API. A suitable shape is:

- `uv_loop_t *loop` or embedded loop storage plus an ownership bit.
- The current Prog argument being evaluated.
- The current continuation stack, preserving the existing `done`, `fmstep`, and `recstep` semantics.
- The final `BSTS_Prog_Test_Result` once evaluation completes.
- A completion flag and an error path for runtime faults.
- Per-effect request records that keep all Bosatsu `BValue` inputs, callbacks, and result/error values reachable by the GC until the libuv completion callback has resumed the Prog.

The execution loop should become resumable. It must be able to step synchronously until it reaches an effect that chooses to suspend, return control to `uv_run`, and then continue from a libuv callback by setting the next Prog argument and re-entering the stepper. The owned loop must be driven with `uv_run(loop, UV_RUN_DEFAULT)` so execution continues until there are no active or referenced handles and no pending requests. Synchronous effects may continue to return their next `Prog` immediately during migration.

The key invariant is that `FlatMap` and `Recover` behavior is unchanged by suspension boundaries. A suspended effect resumes as if the old synchronous `Effect(arg, f)` call had returned a `Prog` value at that exact point in the interpreter, including error recovery, skipped flat-maps after raises, and stack-safe left-associated flat-maps.

## Event Loop Ownership

Generated `main` must continue to initialize GC and statics before calling the Prog runner. The runtime runner owns a fresh default-independent loop for the duration of a main or test run unless an explicit internal test helper supplies one. Do not use libuv's global default loop for generated programs; it makes tests and repeated invocations share handles across runs.

Required ownership behavior:

- `bsts_Bosatsu_Prog_run_main` creates/initializes one loop, runs the main Prog to completion, closes all runtime-owned handles, calls `uv_loop_close`, and returns the Bosatsu exit code or `1` for uncaught top-level errors.
- `bsts_Bosatsu_Prog_run_test` creates/initializes one loop per Prog test invocation and returns `BSTS_Prog_Test_Result` with the same success/error semantics as today.
- Generated test binaries may run many tests in one process; no test may inherit pending handles, pending callbacks, cached loop data, or close failures from a previous Prog test.
- If `uv_loop_close` reports `UV_EBUSY`, the runtime must drain/close runtime-owned handles before treating the run as complete. Use `uv_walk` to find remaining runtime-owned handles, call `uv_close` where needed, then run the loop again with `UV_RUN_DEFAULT` until close callbacks complete before retrying `uv_loop_close`. Silent leaks are not acceptable.
- Runtime code may add private C helper functions for loop injection in narrow C tests, but no Bosatsu API, CLI flag, operator knob, package setting, or user-visible runtime option should be added for loop selection.

## GC And Threading Contract

The existing bdwgc `threadsafe=true` dependency is mandatory for libuv integration. `-DGC_THREADS` must remain present for both runtime C and generated C whenever vendored bdwgc is used, and the non-vendored Makefile path must also compile the runtime with `GC_THREADS` when libuv worker threads are enabled.

`GC_INIT()` must still run once on the main thread before any Bosatsu allocation or libuv work is scheduled. Generated `main` and generated tests already call `GC_init()` before entering runtime code; downstream changes should normalize spelling if needed, but must not move initialization later than `init_statics()` or first `BValue` construction.

Libuv callbacks that run on the loop thread may allocate Bosatsu values normally after `GC_INIT`. Libuv worker callbacks that run on libuv's worker pool must either avoid touching GC-managed Bosatsu objects entirely or register/unregister the worker thread with Boehm before any GC allocation, object traversal, or `BValue` dereference. The safer default for this migration is:

- `uv_work_cb` may perform only OS calls and copy primitive/native data into request-owned C buffers.
- `uv_after_work_cb`, which runs on the loop thread, converts native results into Bosatsu `BValue`s and resumes the Prog.
- Any exception to this rule must document the Boehm thread registration mechanism used on every supported target and must have a stress test that allocates and collects while work is in flight.

All request records that hold `BValue`s across suspension must remain reachable from a GC-scanned root until the after-callback has consumed them. It is not enough for libuv's malloc-allocated request or handle to store the only pointer in its `data` field because Boehm will not scan libuv-owned memory for Bosatsu pointers. Keep active request records in a GC-scanned runtime-owned collection, explicitly register the memory range with `GC_add_roots`, or use an equivalent root-registration mechanism with a matching removal path. Stack-local `BValue` fields are not safe across `uv_queue_work` boundaries.

## IO/Core Migration Order

Migrate effects incrementally while preserving current external Bosatsu behavior and error variants.

1. Establish libuv as a vendored dependency and link dependency, with tests proving metadata and compile/link flags.
2. Introduce the internal Prog runtime context and loop ownership while keeping existing effects synchronous. This should be behaviorally neutral.
3. Migrate `sleep` first using `uv_timer_t`; it is the smallest real suspension point and verifies the resume path without file-handle complexity.
4. Migrate filesystem metadata and directory/path operations where libuv has direct `uv_fs_*` support: `stat`, `mkdir`, `remove`, `rename`, `list_dir`, `create_temp_dir` where practical. Preserve the existing `Bosatsu/IO/Error` mapping.
5. Migrate file open/read/write/flush/close/copy operations and handle lifetime. This step should settle the runtime-owned handle representation because it has the highest leak and close-order risk.
6. Revisit environment, clock, spawn, and wait. `now_wall`, `now_mono`, and `get_env` may remain synchronous if libuv does not improve the contract. `spawn` and `wait` are currently unsupported; making them work is separate feature work unless the roadmap explicitly scopes it.

Do not add new Bosatsu public APIs, syntax, CLI flags, operators, or package-level knobs as part of this migration. The change is a runtime implementation upgrade for existing Prog and IO/Core effects.

## Behavioral Invariants

After the change:

- A Prog with no asynchronous effects produces the same result, uncaught error, and stack behavior as the current synchronous interpreter.
- Inserting asynchronous suspension at an effect boundary is observationally equivalent to the old immediate `call_fn1(effect_fn, arg)` behavior, except for intended non-blocking scheduling.
- `Recover` handles errors raised after an asynchronous effect exactly as it handles errors raised before one.
- `FlatMap` callbacks run in program order for sequential composition; a later continuation does not run before the effect it depends on completes.
- Runtime-owned libuv handles cannot outlive their Prog run or test run.
- Repeated Prog tests in the same process are isolated at loop and handle level.
- Static archive paths and system link flags are deterministic for the same manifest, build context, and recipe version.
- Vendored dependency ordering remains topological and deterministic regardless of manifest input order.
- Existing IO/Core error variants and context strings remain stable unless a case is explicitly accepted in review as a libuv-specific improvement.

## Test Strategy

Property-check style tests are the right fit for stable algebraic and normalization properties:

- `CDeps.buildKey` changes when libuv recipe inputs, source hash, dependency names, transitive build keys, relevant environment, or `recipe_version` change, and remains stable under JSON option/object insertion-order changes.
- `CDeps.orderedDependencies` remains topological and deterministic for generated acyclic dependency graphs that include `libuv` and `bdwgc`, and rejects generated missing-dependency/cycle cases.
- `VendoredDeps` pkg-config parsing/filtering preserves arbitrary generated system flags while removing self-library and `-L` path flags.
- Prog interpreter equivalence: generated small Prog trees with `Pure`, `Raise`, `FlatMap`, `Recover`, synchronous effects, and test-only suspended effects produce the same final result/error as the old synchronous model.
- Suspension placement does not change `FlatMap`/`Recover` semantics for generated Prog trees.

Narrow case-based tests are still the right fit for platform and recipe facts:

- `c_runtime/deps.json` parses and contains exactly the accepted libuv version, URI, hash, `source_subdir`, and recipe.
- `VendoredDeps.staticLibFileName` returns `libuv.a` for `libuv-cmake-static`.
- The libuv CMake configure arguments contain the exact required switches for debug and release.
- Installed metadata points at `<prefix>/include` and `<prefix>/lib/libuv.a` and records expected system flags from a representative `libuv-static.pc`.
- Generated `main` and generated tests call GC/statics initialization before invoking Prog runners.
- A C runtime test runs two Prog tests with separate loops and verifies no pending handles leak between them.
- IO/Core migration cases cover representative success and error paths for each migrated effect, especially interrupted sleep, invalid paths, closed handles, read/write permissions, and uncaught Prog errors.

Required command coverage before merge of downstream implementation:

- `scripts/test_basic.sh`
- The focused JVM test suites for `dev.bosatsu.cruntime.CDepsTest`, `dev.bosatsu.cruntime.VendoredDepsTest`, and relevant clang/codegen tests.
- `make -C c_runtime clean all VENDORED_DEPS=0` where system bdwgc/libuv are installed, or the repo's equivalent non-vendored C runtime build.
- A vendored C backend build path that exercises `c_runtime/deps.json` and links a generated Prog main with both bdwgc and libuv archives.

## Risks And Rollout Notes

The highest risk is GC/thread unsafety from libuv worker callbacks. Keep `BValue` conversion and allocation on the loop thread unless thread registration is deliberately implemented and tested.

The second risk is handle leakage. Every async operation needs a clear owner, close path, and callback path for both success and failure. Loop close failures should fail tests loudly.

Static libuv linking is platform-sensitive. Trust installed `libuv-static.pc` for system libraries, but filter out paths and self-library tokens so Bosatsu metadata remains based on concrete cached archives plus portable system flags.

Roll out in narrow PRs: dependency recipe first, runtime context second, then one IO/Core migration group at a time. Keep unsupported process APIs unsupported until explicitly scoped. Avoid user-facing knobs during rollout; the default C backend behavior should simply become libuv-backed once the runtime is ready.
