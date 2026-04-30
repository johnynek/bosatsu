# Roadmap #2340

> Generated from roadmap graph JSON.
> Edit the `.graph.json` file, not this `.md` file.
> Regenerate with `python roadmap_json_to_md.py <path/to.graph.json> [--output <path/to.md>]`.

## Overview

Add basic libuv support to the C backend without introducing new Bosatsu surface area. The completed roadmap should leave the C runtime building libuv as a vendored dependency alongside threadsafe bdwgc, running `Bosatsu/Prog::Main` and `ProgTest` values through a `uv_loop_t`, and preserving existing Main/Test behavior while moving current C IO externals onto libuv-backed runtime primitives where practical.

### Key Goals

- Vendored C runtime installation builds and links static bdwgc with `GC_THREADS` and static libuv from `c_runtime/deps.json` on supported targets.
- Generated C mains and Prog tests continue to pass through `bsts_Bosatsu_Prog_run_main` and `bsts_Bosatsu_Prog_run_test`, now executing inside a managed libuv loop.
- Existing Bosatsu IO/Core and IO/Std tests keep passing with no new public Bosatsu functions, constructors, or CLI surface required for users.
- C backend validation covers Scala unit tests plus generated C runtime paths exercised by `scripts/test_c_sanitizers.sh` and, where available, `scripts/test_c_valgrind.sh`.

## Metadata

- Roadmap issue: `#2340`
- Graph version: `2`
- Node count: `8`

## Dependency Overview

1. `design` (`reference_doc`): none
2. `vendored_libuv` (`small_job`): `design` (`planned`)
3. `loop_core` (`small_job`): `design` (`planned`), `vendored_libuv` (`implemented`)
4. `async_resume` (`small_job`): `design` (`planned`), `loop_core` (`implemented`), `vendored_libuv` (`implemented`)
5. `io_phase1` (`small_job`): `async_resume` (`implemented`), `design` (`planned`), `loop_core` (`implemented`)
6. `io_files` (`small_job`): `async_resume` (`implemented`), `design` (`planned`), `io_phase1` (`implemented`)
7. `process_stdio` (`small_job`): `async_resume` (`implemented`), `design` (`planned`), `io_files` (`implemented`)
8. `validation_with_vendored` (`small_job`): `async_resume` (`implemented`), `design` (`planned`), `io_files` (`implemented`), `io_phase1` (`implemented`), `loop_core` (`implemented`), `process_stdio` (`implemented`), `vendored_libuv` (`implemented`)

## Nodes

### `design`

- Kind: `reference_doc`
- Title: Document the libuv C runtime integration contract
- Depends on: none

#### Body

Create a reviewed reference document for the libuv C backend work. The document must be based on the current default branch files `c_runtime/deps.json`, `core/src/main/scala/dev/bosatsu/cruntime/CDeps.scala`, `core/src/main/scala/dev/bosatsu/cruntime/VendoredDeps.scala`, `c_runtime/Makefile`, `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c`, `c_runtime/bosatsu_ext_Bosatsu_l_Prog.h`, `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c`, `core/src/main/scala/dev/bosatsu/codegen/clang/ClangGen.scala`, and `core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala`.

The artifact should define the concrete implementation contract for: the libuv version/source/hash to add to `c_runtime/deps.json`; the static CMake recipe and metadata/link flag expectations; how `GC_THREADS` and `GC_INIT`/thread registration safety will be preserved with libuv worker threads; the `uv_loop_t` ownership model for generated `main` and test execution; the runtime data structure needed to suspend a Prog stack around asynchronous effects and resume it on completion; and the migration order for current IO/Core effects. It must explicitly call out which work should not add new Bosatsu public APIs or CLI/operator knobs.

Completion outcome: a merged durable reference document that downstream workers can use as their first-turn artifact for exact libuv version/hash, runtime architecture, safety constraints, migration order, and required test commands.

### `vendored_libuv`

- Kind: `small_job`
- Title: Add libuv to the vendored C runtime dependency pipeline
- Depends on: `design` (`planned`)

#### Body

Use the merged reference document from `design` as the direct input for the libuv version, source archive URI, hash, static build recipe, expected metadata, and bdwgc/libuv safety constraints.

Implement libuv vendoring in the existing C dependency pipeline. Update the manifest and Scala/runtime dependency code so `c_runtime/deps.json` includes libuv, the existing `libuv-cmake-static` recipe constant in `core/src/main/scala/dev/bosatsu/cruntime/CDeps.scala` is handled by `core/src/main/scala/dev/bosatsu/cruntime/VendoredDeps.scala`, static metadata records `libuv.a`, and needed system link flags are collected without requiring a host-installed libuv. Preserve existing bdwgc behavior, including `threadsafe=true` and runtime `GC_THREADS` requirements.

Also update focused tests in `core/src/test/scala/dev/bosatsu/cruntime/CDepsTest.scala` and `core/src/test/scala/dev/bosatsu/cruntime/VendoredDepsTest.scala` for manifest parsing, dependency ordering, static library naming, libuv CMake args, and link flag metadata.

Completion outcome: default-branch implementation can build and cache libuv through the same vendored dependency path as bdwgc, with unit tests covering the new recipe behavior.

### `loop_core`

- Kind: `small_job`
- Title: Introduce a libuv-owned Prog runtime loop skeleton
- Depends on: `design` (`planned`), `vendored_libuv` (`implemented`)

#### Body

Use the merged reference document from `design` and the implemented vendored libuv support from `vendored_libuv` as direct inputs. The worker may rely on libuv being available through the vendored C runtime install/link path and on the reference document's loop ownership and GC safety contract.

Refactor the C Prog runner in `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c` and `c_runtime/bosatsu_ext_Bosatsu_l_Prog.h` so `bsts_Bosatsu_Prog_run_main` and `bsts_Bosatsu_Prog_run_test` execute through a managed `uv_loop_t` while preserving the current synchronous behavior for Pure, Raise, FlatMap, Recover, ApplyFix, and existing Effect functions. Generated C main/test code in `core/src/main/scala/dev/bosatsu/codegen/clang/ClangGen.scala` should continue to call the existing public runner entry points; avoid changing the Bosatsu language/library surface.

This slice should add the minimal runtime state structure needed to carry the current Prog argument, continuation stack, completion status, and loop pointer, but it does not need to migrate file IO effects yet. It should include focused C runtime tests or generated C tests that prove existing Main and ProgTest programs still exit and report results correctly through the new loop path.

Completion outcome: the C backend links libuv and runs current Prog mains/tests through a libuv loop without behavior regressions.

### `async_resume`

- Kind: `small_job`
- Title: Add suspend-and-resume support for libuv-backed Prog effects
- Depends on: `design` (`planned`), `loop_core` (`implemented`), `vendored_libuv` (`implemented`)

#### Body

Use the merged reference document from `design`, the implemented vendored libuv support from `vendored_libuv`, and the implemented libuv Prog loop skeleton from `loop_core` as direct inputs. The worker may rely on `bsts_Bosatsu_Prog_run_main` and `bsts_Bosatsu_Prog_run_test` already owning a `uv_loop_t` and preserving synchronous Prog behavior.

Extend the C Prog runtime so an effect can suspend interpretation with its current continuation stack, attach that suspended continuation to a libuv request or work item, and resume interpretation from the libuv completion callback. Keep the existing synchronous Effect path working for effects that do not suspend. Ensure GC-managed values reachable from suspended stacks, effect arguments, callbacks, and results stay reachable for the full async lifetime under bdwgc with `GC_THREADS`.

Add targeted tests in `c_runtime/test.c` or generated C test coverage that exercise a synthetic libuv-backed effect completing asynchronously, including success, error/recover, and flat_map continuation cases. Do not migrate IO/Core functions in this slice except for test-only or private runtime hooks needed to prove the resume mechanism.

Completion outcome: the C Prog interpreter has a reviewed, tested internal suspend/resume mechanism suitable for subsequent libuv IO migrations.

### `io_phase1`

- Kind: `small_job`
- Title: Migrate time, sleep, and environment C IO effects onto libuv runtime plumbing
- Depends on: `async_resume` (`implemented`), `design` (`planned`), `loop_core` (`implemented`)

#### Body

Use the merged reference document from `design`, the implemented libuv Prog loop skeleton from `loop_core`, and the implemented suspend/resume support from `async_resume` as direct inputs. The worker may rely on libuv being vendored and linked, and on async effects being able to suspend and resume a Prog continuation safely.

Update the low-risk IO/Core externals in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` first: `now_wall`, `now_mono`, `sleep`, and `get_env`. Use libuv equivalents where libuv provides them, and preserve the existing Bosatsu return values and `IOError` mapping. `sleep` should be implemented through the libuv loop rather than blocking with `nanosleep`; wall/monotonic time and environment behavior should remain compatible with current tests.

Add or adjust tests in `test_workspace/Bosatsu/IO/Core.bosatsu`, generated C test coverage, or C runtime tests so these functions are exercised through the C backend. Include a regression that a delayed `sleep` continuation resumes and then runs subsequent `flat_map` work.

Completion outcome: basic non-file IO effects are libuv-backed where applicable and prove the async continuation machinery in normal Bosatsu programs.

### `io_files`

- Kind: `small_job`
- Title: Migrate file and directory C IO effects to libuv-backed operations
- Depends on: `async_resume` (`implemented`), `design` (`planned`), `io_phase1` (`implemented`)

#### Body

Use the merged reference document from `design`, the implemented suspend/resume support from `async_resume`, and the implemented time/sleep/environment migration from `io_phase1` as direct inputs. The worker may rely on the reference document's migration matrix and on existing async continuation behavior having been exercised by normal IO/Core tests.

Migrate the file and directory operations in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` toward libuv-backed implementations while preserving the existing Bosatsu API and behavior. Cover handle creation/closing, read/write UTF-8 and bytes, read_all_bytes, copy_bytes, open_file, create_temp_file, create_temp_dir, list_dir, stat, mkdir, mkdir_with_mode, remove, rename, and flush as scoped by the reference document. Keep error conversion aligned with the existing `IOError` variants in `test_workspace/Bosatsu/IO/Error.bosatsu`.

Where libuv does not provide an exact semantic match, keep compatibility with existing behavior and document the local fallback in code comments only where needed. Add focused C backend tests for successful file round trips, EOF, invalid UTF-8, directory listing/stat/mkdir/remove/rename, mode handling where supported, and error mapping for common failures.

Completion outcome: current C IO/Core file and directory behavior is implemented through libuv-backed runtime primitives or explicitly justified compatibility fallbacks, with existing Bosatsu tests still passing.

### `process_stdio`

- Kind: `small_job`
- Title: Implement libuv-backed process spawn and wait for C IO/Core
- Depends on: `async_resume` (`implemented`), `design` (`planned`), `io_files` (`implemented`)

#### Body

Use the merged reference document from `design`, the implemented suspend/resume support from `async_resume`, and the implemented file/directory IO migration from `io_files` as direct inputs. The worker may rely on libuv-backed handles and continuation resumption being available in the C runtime.

Replace the current unsupported C implementations of `spawn` and `wait` in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` with libuv-backed process support for the existing `SpawnResult`, `Stdio`, and `StdioConfig` shapes defined in `test_workspace/Bosatsu/IO/Core.bosatsu`. Support inherited, null, piped, and existing-handle stdio according to the reference document, preserving the existing Bosatsu API.

Add C backend tests for spawning a simple command, waiting for its exit code, piping stdout/stderr where feasible, writing to child stdin where feasible, and error mapping for missing commands or invalid handles. Keep platform-specific assumptions guarded so the existing sanitizer and valgrind scripts can run on the current CI targets.

Completion outcome: C backend process execution is no longer the known unsupported gap in IO/Core and is integrated with the libuv loop and handle model.

### `validation_with_vendored`

- Kind: `small_job`
- Title: Harden C backend libuv coverage and CI validation
- Depends on: `async_resume` (`implemented`), `design` (`planned`), `io_files` (`implemented`), `io_phase1` (`implemented`), `loop_core` (`implemented`), `process_stdio` (`implemented`), `vendored_libuv` (`implemented`)

#### Body

Use the merged reference document from `design`, the implemented vendored libuv dependency pipeline from `vendored_libuv`, and the implemented runtime, IO, and process work from `loop_core`, `async_resume`, `io_phase1`, `io_files`, and `process_stdio` as direct inputs. The worker may rely on the full libuv-backed C runtime behavior being present on the branch and on the vendored libuv install/link metadata being present in the C runtime dependency pipeline.

Tighten validation for the completed libuv integration. Update `scripts/test_c_sanitizers.sh`, `scripts/test_c_valgrind.sh`, `scripts/c_runtime_ci_env.py`, or adjacent tests only as needed to ensure vendored libuv link flags, `GC_THREADS`, and generated C binaries are exercised. Add regression coverage for Main and Test compatibility, async continuation after IO completion, recovered async errors, file IO, process wait, and GC/thread safety assumptions that can be tested locally.

Run and document the practical verification set: relevant Scala unit tests for `dev.bosatsu.cruntime` and `dev.bosatsu.codegen.clang`, `scripts/test_c_sanitizers.sh`, and `scripts/test_c_valgrind.sh` when valgrind is installed. If valgrind is unavailable in the worker environment, leave the script updated and report that limitation in the PR.

Completion outcome: the roadmap closes with high-confidence tests and scripts covering libuv vendoring, runtime loop execution, async resume behavior, IO compatibility, and GC/libuv safety.
