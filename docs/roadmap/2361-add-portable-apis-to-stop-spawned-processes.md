# Roadmap #2361

> Generated from roadmap graph JSON.
> Edit the `.graph.json` file, not this `.md` file.
> Regenerate with `python roadmap_json_to_md.py <path/to.graph.json> [--output <path/to.md>]`.

## Overview

Add portable direct-child process control to Bosatsu without exposing raw platform-specific signals or process-tree behavior. When the roadmap is complete, `Bosatsu/IO/Core` should expose semantic stop/status APIs, JVM/Python/C-libuv runtimes should implement them with stable idempotent exit observation, common cleanup should be available as a Bosatsu-level helper, and the behavior should be covered by focused backend and library tests.

### Key Goals

- Direct spawned processes can be stopped through portable `terminate` and `kill` APIs that return `StopSent` or `AlreadyExited` without consuming the eventual exit result.
- `wait`, `poll`, and `wait_timeout` observe one stable recorded exit code; timeout polling does not consume the final process result.
- C/libuv process control operates on the live `uv_process_t` handle and keeps it valid until libuv observes process exit, avoiding pid-reuse and zombie-process hazards.
- A managed cleanup helper brackets spawned process stdio, terminates/escalates direct children on early/error exit, and always waits/reaps before returning.
- Focused tests cover JVM, Python, and C runtime behavior for stop idempotence, stable waiting, timeout non-consumption, stdio ownership, and managed cleanup.

## Metadata

- Roadmap issue: `#2361`
- Graph version: `1`
- Node count: `7`

## Dependency Overview

1. `contract` (`reference_doc`): none
2. `wait_state` (`small_job`): `contract` (`planned`)
3. `low_level_api` (`small_job`): `contract` (`planned`), `wait_state` (`implemented`)
4. `helper_contract` (`reference_doc`): `contract` (`planned`), `low_level_api` (`implemented`)
5. `helper_impl` (`small_job`): `helper_contract` (`planned`), `low_level_api` (`implemented`)
6. `coverage` (`small_job`): `contract` (`planned`), `helper_impl` (`implemented`), `low_level_api` (`implemented`)
7. `docs` (`small_job`): `contract` (`planned`), `coverage` (`implemented`), `helper_impl` (`implemented`), `low_level_api` (`implemented`)

## Nodes

### `contract`

- Kind: `reference_doc`
- Title: Specify the portable process stop and status contract
- Depends on: none

#### Body

Create a reviewed durable reference document for issue #2361's process-control semantics. Base it on the source issue and the current default-branch files `test_workspace/Bosatsu/IO/Core.bosatsu`, `core/src/main/scala/dev/bosatsu/Predef.scala`, `test_workspace/ProgExt.py`, `test_workspace/Prog.bosatsu_externals`, `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c`, `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.h`, `c_runtime/test.c`, and the existing process discussion in `docs/src/main/paradox/design-docs/prog_concurrency_design.md`.

The artifact must pin the public semantic contract for `StopResult`, `terminate`, `kill`, `poll`, `wait_timeout`, and the existing `wait`; define direct-child-only behavior; reject raw public signal APIs and process-tree/group semantics; describe backend mappings for JVM `Process.destroy`/`destroyForcibly`, Python `Popen.terminate`/`kill`, and C/libuv `uv_process_t`-based termination; specify exit-code normalization and idempotence; and list the test commands/workspaces downstream workers should use. It should also state the intended managed cleanup helper behavior without requiring that helper to land in the same implementation slice.

Completion outcome: a merged reference document that downstream workers receive by exact MergeXO artifact path and can use as the reviewed source of truth for process stop/status semantics, backend mappings, non-goals, and acceptance tests.

### `wait_state`

- Kind: `small_job`
- Title: Prepare process state for idempotent wait and status queries
- Depends on: `contract` (`planned`)

#### Body

Direct inputs: receive the merged reference document from `contract` as a `planned` dependency artifact. Use that artifact as the reviewed contract for direct-child process lifetime, stable exit-code recording, and timeout non-consumption.

Update existing process runtime state so the current `wait(p)` behavior is idempotent and status-cache-ready before new public stop/status functions are added. Keep the public Bosatsu API unchanged in this slice. Scope the implementation to the existing process wrappers in `core/src/main/scala/dev/bosatsu/Predef.scala`, `test_workspace/ProgExt.py`, and `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c`/`.h`.

For JVM and Python, ensure the `Process` runtime object records one final exit code and repeated `wait` calls return that same value. For C/libuv, ensure the process object:

- Records the exit status once.
- Supports repeated waits after exit.
- Does not close or invalidate the `uv_process_t` before libuv observes process exit.
- Does not rely on a cached pid for future control operations.

Preserve existing spawn, stdio, and wait public behavior.

Add focused regression tests in the existing test locations, including `c_runtime/test.c` and the relevant Scala/Python evaluation tests, for repeated wait after zero and nonzero exits. Avoid introducing `terminate`, `kill`, `poll`, or `wait_timeout` in this slice.

Completion outcome: existing process waiting has a reviewed, tested stable-exit-state foundation that downstream workers may rely on as implemented state.

### `low_level_api`

- Kind: `small_job`
- Title: Implement portable terminate, kill, poll, and wait_timeout APIs
- Depends on: `contract` (`planned`), `wait_state` (`implemented`)

#### Body

Direct inputs: receive the merged reference document from `contract` as a `planned` dependency artifact, and receive the implemented stable process state from `wait_state` as a satisfied `implemented` dependency state. Use both directly; do not reconstruct these requirements from roadmap history.

Add the low-level public API to `test_workspace/Bosatsu/IO/Core.bosatsu`: `StopResult` with `StopSent` and `AlreadyExited`, plus external `terminate`, `kill`, `poll`, and `wait_timeout` functions. Export the new type and functions consistently with existing `spawn` and `wait` exports.

Implement the corresponding runtime externals for JVM in `core/src/main/scala/dev/bosatsu/Predef.scala`, Python in `test_workspace/ProgExt.py` and `test_workspace/Prog.bosatsu_externals`, and C/libuv in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` and `.h`. JVM should map to `Process.destroy()` and `destroyForcibly()`. Python should map to `Popen.terminate()` and `Popen.kill()`. C/libuv should operate on the `uv_process_t` handle and use the strongest reviewed portable mapping from the reference document; do not expose raw signal names or pid-based public behavior.

Low-level behavior must satisfy these contracts:

- `terminate` and `kill` return `AlreadyExited` for an already-recorded exit.
- `terminate` and `kill` return `StopSent` when a stop request is sent.
- `poll` returns `Some(exit_code)` only after a recorded exit.
- `poll` returns `None` while the process is still running.
- `wait_timeout` returns `None` on timeout without consuming the final result.
- Subsequent `wait` calls still return the stable exit code after `wait_timeout` times out.
- Stop operations do not automatically close returned stdin/stdout/stderr handles.

Add focused tests for each backend where the repository already exercises process externals. Include idempotent stop on an already-exited child, stop followed by wait, kill escalation for a long-running child, poll before/after exit, and wait_timeout timeout followed by successful wait. Keep platform-sensitive commands guarded or factored through existing test helpers.

Completion outcome: the portable low-level stop/status API is publicly available and implemented across JVM, Python, and C/libuv with tests passing.

### `helper_contract`

- Kind: `reference_doc`
- Title: Specify the managed with_process helper contract
- Depends on: `contract` (`planned`), `low_level_api` (`implemented`)

#### Body

Direct inputs: receive the merged reference document from `contract` as a `planned` dependency artifact, and receive the implemented low-level process stop/status API from `low_level_api` as a satisfied `implemented` dependency state.

Create a reviewed durable reference document for the Bosatsu-level managed process helper. The artifact should choose the exact public helper shape that fits existing Bosatsu error-polymorphism and library style. This includes:

- The module where the helper should live.
- The argument order around `spawn`, `StdioConfig`, and grace `Duration`.
- The signature of the `use` block: `SpawnResult -> Prog[IOError, a]`.
- How errors from `use`, stdio close, `terminate`, `kill`, and final `wait` should compose.

The contract must require that returned stdio pipe handles are closed during cleanup, that a still-running direct child is terminated and then force-killed after the grace duration, that cleanup always waits/reaps the child before returning, and that low-level APIs remain available for callers needing custom drain or close order. It must explicitly keep process-tree termination and implicit closure from low-level `terminate`/`kill` out of scope.

Completion outcome: a merged reference document that downstream workers receive by exact MergeXO artifact path and can use as the reviewed source for the helper signature, cleanup ordering, and error behavior.

### `helper_impl`

- Kind: `small_job`
- Title: Implement managed process cleanup helper
- Depends on: `helper_contract` (`planned`), `low_level_api` (`implemented`)

#### Body

Direct inputs: receive the merged helper contract from `helper_contract` as a `planned` dependency artifact, and receive the implemented low-level process stop/status APIs from `low_level_api` as a satisfied `implemented` dependency state. The worker should use the helper contract for the exact signature and cleanup rules.

Implement the managed process helper in the Bosatsu library surface selected by the helper contract, likely starting from `test_workspace/Bosatsu/IO/Core.bosatsu` unless the reviewed contract chooses a different existing module. The helper should wrap `spawn`, pass `SpawnResult` to `use`, close any returned pipe handles on exit, check whether the process is still alive, terminate and then force-kill after the grace duration when needed, and always wait for the direct child before returning from cleanup.

Add Bosatsu-level tests covering normal completion, user-code error or early failure, already-exited children, close behavior for returned pipe handles, and escalation from terminate to kill when the child ignores or outlives graceful termination. Keep the helper built from the public low-level APIs rather than backend-specific runtime hooks.

Completion outcome: common process cleanup is available as a reviewed Bosatsu-level helper with deterministic stdio close and process reap behavior.

### `coverage`

- Kind: `small_job`
- Title: Add cross-backend process stop and cleanup regression coverage
- Depends on: `contract` (`planned`), `helper_impl` (`implemented`), `low_level_api` (`implemented`)

#### Body

Direct inputs: receive the merged process stop/status contract from `contract` as a `planned` dependency artifact, the implemented low-level API from `low_level_api` as a satisfied `implemented` dependency state, and the implemented managed cleanup helper from `helper_impl` as a satisfied `implemented` dependency state.

Add or strengthen end-to-end regression coverage so issue #2361's acceptance criteria are exercised across the supported runtime paths rather than only in isolated unit tests. Use existing verified test locations and commands: `c_runtime/test.c` with `make -C c_runtime test_out`, Scala/JVM tests under `core/src/test/scala` or `cli/src/test/scala`, Python generation/evaluation tests under the existing Python test flow, and the repository gate `scripts/test_basic.sh`.

Cover direct process termination and force-kill, idempotent stop after exit, stable wait after stop, poll before and after exit, wait_timeout timeout followed by final wait, no implicit stdio closure from low-level stop functions, and helper-managed stdio close/reap behavior. Keep long-running child commands bounded, portable, and guarded for macOS/Linux/Windows where necessary. Prefer small helper programs or existing runtime-neutral test fixtures over shell-specific assumptions when possible.

Completion outcome: the process stop/status and cleanup behavior has durable cross-backend regression coverage with the focused C runtime target and repository basic test gate passing.

### `docs`

- Kind: `small_job`
- Title: Document portable process stop APIs and cleanup helper
- Depends on: `contract` (`planned`), `coverage` (`implemented`), `helper_impl` (`implemented`), `low_level_api` (`implemented`)
- Promised artifact path: `docs/src/main/paradox/design-docs/minimal_prog_io_tools_design.md`

#### Body

Direct inputs: receive the merged process stop/status contract from `contract` as a `planned` dependency artifact, the implemented low-level API from `low_level_api` as a satisfied `implemented` dependency state, the implemented managed cleanup helper from `helper_impl` as a satisfied `implemented` dependency state, and the completed regression coverage from `coverage` as a satisfied `implemented` dependency state.

Update the repository's user/developer documentation to describe the new portable direct-child process APIs and the managed cleanup helper. At minimum, update `docs/src/main/paradox/design-docs/minimal_prog_io_tools_design.md` so the documented `IO/Core` process surface matches the shipped API. This includes documenting:

- `StopResult`, `terminate`, `kill`, `poll`, `wait_timeout`, and stable `wait`.
- The direct-child-only scope.
- The absence of a raw signal API or process-tree guarantee.
- Explicit low-level stdio ownership.

Add a concise helper example using `with_process` and a grace `Duration` if the final helper contract exposes that shape.

Do not introduce operator-facing knobs, platform-specific public branches, or process-tree semantics in the docs. The documentation should be consistent with the implemented signatures and tests, not merely the initial issue proposal.

Completion outcome: checked-in documentation matches the implemented portable process stop/status and cleanup API, with no stale claim that process termination is out of scope.
