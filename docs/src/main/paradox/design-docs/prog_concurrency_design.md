# Prog Concurrency API Design

Status: proposed  
Date: 2026-03-21

## Problem
`Bosatsu/Prog` currently has a synchronous execution model on every backend in this repository:

1. JVM evaluator: `run_prog` steps one `Prog` value inline.
2. Python runtime: `ProgExt.run` steps one `Prog` value inline.
3. C runtime: `bsts_Bosatsu_Prog_run` steps one `Prog` value inline.

That is enough for sequencing effects, but it leaves two missing pieces:

1. We cannot start a child `Prog` and later join or cancel it.
2. We cannot move expensive pure work off the main thread, IO thread, or future event-loop thread.

For a future libuv-backed C runtime, the second point is required: CPU-heavy work must not run on the event loop. For JVM and Python, the same API should still make sense even if the concrete scheduler is different.

## Decision Summary
1. Add `JoinHandle` and `JoinResult` to `Bosatsu/Prog`.
2. Add `start`, `join`, `cancel`, and `compute` to `Bosatsu/Prog`.
3. Make `join` return the child outcome as a value, not through the caller's `Prog` error channel.
4. Define cancellation as cooperative and best-effort across all backends.
5. Treat `compute` as the standard "shift CPU work off the main scheduler" primitive.
6. Do not add `race`, `par_map`, `poll`, masking, or finalizers in this first API.

## Proposed API

```bosatsu
package Bosatsu/Prog

export (
  ...,
  JoinHandle,
  JoinResult(),
  start,
  join,
  cancel,
  compute,
)

external struct JoinHandle[err: +*, a: +*]

enum JoinResult[err: +*, a: +*]:
  Succeeded(value: a)
  Errored(error: err)
  Canceled

external def start[err, a](prog: Prog[err, a]) -> forall f. Prog[f, JoinHandle[err, a]]
external def join[err, a](handle: JoinHandle[err, a]) -> forall f. Prog[f, JoinResult[err, a]]
external def cancel[err, a](handle: JoinHandle[err, a]) -> forall f. Prog[f, Unit]

# Run a pure, potentially expensive computation on the backend's compute pool
# or worker threads rather than the main scheduler thread.
external def compute[a](thunk: Unit -> a) -> forall err. Prog[err, a]
```

## Why `join` Returns `JoinResult[err, a]`
The sketched type

```bosatsu
join(h: JoinHandle[e, a]) -> Prog[e, JoinResult[a]]
```

mixes two different concerns:

1. did `join` itself fail?
2. what happened in the child fiber?

For this API, the important failure is the child outcome, not a second effect-level error channel for `join`. Returning `JoinResult[err, a]` makes the contract simpler:

1. `join` itself is total in the typed `Prog` error channel.
2. child success, child error, and cancellation all live in one value.
3. repeated `join` calls can deterministically return the same terminal value.

This is the closest Bosatsu analogue to cats-effect's `Outcome`, but simplified because Bosatsu does not yet have finalizers or a need to return `Prog` inside the success case.

## Semantics

### `start`
1. `start(p)` is still lazy like every other `Prog` constructor; nothing happens until the returned `Prog` is executed.
2. Once executed, the backend creates a child task and returns a `JoinHandle` immediately.
3. The child may already be running, queued, or even completed by the time the parent receives the handle. That race is allowed.
4. `JoinHandle` is opaque and may be shared freely.

### `join`
1. `join(h)` waits until `h` reaches a terminal state.
2. The terminal states are `Succeeded(a)`, `Errored(e)`, and `Canceled`.
3. `join` is idempotent: every successful call returns the same terminal `JoinResult`.
4. Multiple fibers may join the same handle.

### `cancel`
1. `cancel(h)` is idempotent.
2. If the child is already complete, `cancel` is a no-op.
3. Otherwise, `cancel` records a cancellation request.
4. Backends should stop scheduling the child at the next cancellation checkpoint.
5. If a backend can interrupt a blocked operation, it should do so. If it cannot, work may continue in the background and its result is discarded.
6. If cancellation wins the race against completion, `join(h)` returns `Canceled`.

### Cancellation checkpoints
Cancellation is not preemptive in the middle of arbitrary pure code. The shared contract is:

1. before entering a backend effect, the runtime may observe cancellation
2. after resuming from a backend wait, the runtime may observe cancellation
3. `compute` is a cancellation boundary
4. long pure Bosatsu loops are not required to notice cancellation until they reach an effect boundary

This is the only semantics that works across JVM threads, Python threads, and libuv worker tasks without unsafe thread killing.

### `compute`
1. `compute(thunk)` runs `thunk(())` away from the main scheduler thread.
2. `compute` is for pure, CPU-bound work.
3. If the caller wants background compute, use `start(compute(thunk))`.
4. `compute` does not promise parallel speedup on Python because of the GIL, but it still gives a portable "not on the main/event-loop thread" guarantee.
5. On libuv, `compute` is the standard way to keep the event loop responsive while doing CPU work.
6. This "pure only" restriction is not optional. Bosatsu functions are pure and do not throw exceptions, so a value of type `Unit -> a` has no typed failure mode. If a later API is needed for shifting blocking effectful work, that should be a separate combinator over `Prog`, not a broader meaning for `compute`.

## Example Usage

```bosatsu
from Bosatsu/Prog import await, compute, join, start, JoinResult()

background_hash =
  handle <- compute(() -> expensive_hash(input_bytes)).start()
  join(handle)
```

The result is a `Prog` returning a `JoinResult`, because child completion, child error, and cancellation are all explicit values. Higher combinators such as `race` or `par_map2` can be layered on top once the base semantics are stable.

## Shared Runtime Shape
Backends should implement `JoinHandle` with a small shared state machine:

1. `Running(cancel_requested, waiters...)`
2. `Succeeded(a)`
3. `Errored(e)`
4. `Canceled`

`waiters` here means backend-owned continuations, condition variables, promises, or event-loop callbacks. The Bosatsu surface does not expose this representation.

No new public `Prog` tag is required for the API itself. On JVM and Python, the new operations can still be exposed as ordinary `Prog` externals. A libuv runtime will likely need richer internal scheduler machinery, but that can stay behind the same Bosatsu API.

## Shared `Bosatsu/IO/Core` Process Contract
The existing `Bosatsu/IO/Core` API already commits us to a specific process model:

```bosatsu
spawn(cmd: String, args: List[String], stdio: StdioConfig) -> Prog[IOError, SpawnResult]
wait(p: Process) -> Prog[IOError, Int]
```

To keep all backends aligned, this design commits to the following shared semantics:

1. `spawn` uses argv semantics, not shell semantics. `cmd` is the program to execute and `args` are literal argument strings.
2. `spawn` inherits the current working directory and environment of the host runtime unless a future Bosatsu API adds explicit `cwd` or `env` fields.
3. `spawn` raises `IOError` only if process creation or requested stdio setup fails. A child process exiting non-zero is observed only through `wait`.
4. `SpawnResult.stdin`, `SpawnResult.stdout`, and `SpawnResult.stderr` are `Some(handle)` exactly when the corresponding `Stdio` entry was `Pipe`. For `Inherit`, `Null`, and `UseHandle`, the returned field is `None`.
5. `UseHandle(handle)` validates direction at runtime: `stdin` requires a readable handle; `stdout` and `stderr` require writable handles. Closed or invalid handles raise `IOError`.
6. `wait(p)` is idempotent and may be called multiple times or concurrently. Every successful call returns the same cached exit code.
7. `wait(p)` does not implicitly close or drain pipe handles returned from `spawn`. If the caller requested `Pipe`, those returned handles remain ordinary Bosatsu handles with their own lifetime.
8. Canceling a Bosatsu fiber blocked in `wait(p)` never kills the external process. It only cancels that Bosatsu wait.
9. If a backend has to emulate `UseHandle` with internal copy tasks rather than native OS-level stdio inheritance, `wait(p)` must not complete until those backend-owned bridge tasks have also settled. Otherwise `wait` could return before the requested redirection is actually complete.
10. `wait(p)` returns a single `Int`, so this API intentionally collapses normal exit and signal termination into one value. On POSIX backends that expose signal termination separately or as negative codes, Bosatsu should normalize signal termination to `128 + signal_number`.
11. If Bosatsu later needs to distinguish ordinary exit from signal termination, that should be a new structured `ExitStatus` API rather than a silent change to `wait`.

## Backend Notes

### Scala (`MatchlessToValue` + cats-effect)
1. The Scala-side implementation should live in `MatchlessToValue`, not in the current synchronous `PredefImpl.run_prog` loop.
2. Assume the interpreter runs in a cats-effect `F[_]`, with the needed capabilities supplied by cats-effect typeclasses rather than JVM-specific thread primitives.
3. `start` should use cats-effect fiber spawning over the interpreted child `Prog`, and `JoinHandle` should wrap that fiber or a small cats-effect state object built from `Ref`/`Deferred`.
4. `join` should wait on the cats-effect fiber and translate its terminal state into Bosatsu `JoinResult`.
5. `cancel` should delegate to cats-effect fiber cancellation, which already has the right cooperative semantics for this API.
6. `compute` should also be interpreted inside `MatchlessToValue` on top of cats-effect. On JVM and Scala Native that can run on the cats-effect scheduler in the normal way. On Scala.js, cats-effect still gives portable fiber concurrency and cancellation, but it does not by itself create a background CPU thread; if we need literal off-main-thread CPU execution there, that is a later Web Worker integration problem.
7. The current Scala evaluator stack is not concurrency-safe as written. `MatchlessToValue` has mutable interpreter state, and `tool/Output.scala` explicitly notes that `MatchlessToValue` is not currently thread-safe. A real implementation needs to make interpreter state instance-local and safe for multiple in-flight fibers before we rely on concurrent `start`/`join` within one process.
8. Shared Scala process support should not be hard-wired directly to `java.lang.ProcessBuilder`, because Scala.js does not have that API.
9. `ProgRuntime[F]` should instead carry a process backend, with distinct implementations for JVM, `bosatsu_node` under Node.js, and browser-oriented Scala.js.
10. Browser Scala.js should fail `spawn` and `wait` with `Unsupported`.
11. `bosatsu_node` should provide a Node-specific implementation over Node `child_process`, not pretend that generic browser Scala.js can run OS processes.
12. `fs2.io.process.ProcessBuilder` is still relevant as an existing cross-JVM/Node API already used in this repository, but Bosatsu should not make its process semantics depend directly on fs2's `Resource` lifecycle or pipe-only process model.

### Python
1. `start` and `compute` can use `threading.Thread` or a small executor.
2. `join` can wait on a `threading.Condition` or `Event`.
3. `cancel` is cooperative only; Python cannot safely kill an arbitrary thread.
4. `compute` still matters even with the GIL because it keeps the caller's main thread free and works correctly for native code that releases the GIL.
5. `spawn` should continue to use `subprocess.Popen` with `shell=False`.
6. `Stdio.Inherit` maps to `None`, `Pipe` to `subprocess.PIPE`, `Null` to `subprocess.DEVNULL`, and `UseHandle` to the existing Python file object carried by the Bosatsu handle.
7. The Python `Process` runtime object should cache exit code and own one shared waiter result so repeated or concurrent `wait` calls observe the same completion rather than racing independent `Popen.wait()` calls.
8. On POSIX, Python reports signal termination as a negative return code. Bosatsu should normalize that to `128 + signal_number` before returning from `wait`.
9. Canceling a Bosatsu fiber blocked in `wait(process)` must not kill the process.

### C with libuv
1. The current C runtime in this repository is not libuv-based today. It uses a synchronous `Prog` loop, `FILE*`-backed handles, blocking `fread`/`fwrite`, `fopen`, and `nanosleep`, and it still reports `spawn` and `wait` as unsupported.
2. A real libuv backend should not wrap the existing blocking C runtime in worker threads. If we depend on libuv, the C runtime should use libuv directly for the `Bosatsu/Prog` scheduler and for the `Bosatsu/IO/Core` primitives.
3. That means the C backend needs a genuine event-loop-driven fiber scheduler, a suspend/resume effect ABI, and a new opaque `Handle` representation built on `uv_file`, `uv_stream_t`, `uv_process_t`, `uv_timer_t`, `uv_fs_t`, and `uv_work_t`.
4. The detailed plan is specified in the next section.

## Detailed Scala/MatchlessToValue Design

### Core split: pure evaluation stays pure, `Prog` execution becomes `F`
The important Scala design choice is to keep two layers distinct:

1. `MatchlessToValue` expression evaluation remains pure and produces `Value`.
2. Executing a `Bosatsu/Prog::Prog[...]` value becomes effectful and runs in cats-effect `F[_]`.

That means we do not try to make the whole Bosatsu evaluator asynchronous. We only make the `Prog` runner asynchronous.

### Recommended API shape
Keep the current pure API and add an effectful `Prog` runner adjacent to it.

Recommended shape:

```scala
object MatchlessToValue {
  def traverse[F[_]: Functor, A](
      me: F[Expr[A]]
  )(resolve: (A, PackageName, Identifier) => Eval[Value]): F[Eval[Value]]

  def runProgF[F[_]: cats.effect.kernel.Async](
      prog: Value,
      runtime: ProgRuntime[F]
  ): F[Either[Value, Value]]

  def runProgMainF[F[_]: cats.effect.kernel.Async](
      main: Value,
      args: List[String],
      runtime: ProgRuntime[F]
  ): F[ProgRunResult]

  def runProgTestF[F[_]: cats.effect.kernel.Async](
      progTest: Value,
      args: List[String],
      runtime: ProgRuntime[F]
  ): F[Either[Value, Value]]
}
```

`ProgRuntime[F]` should own:

1. stdin/stdout/stderr abstractions
2. file/process/time externals
3. fiber spawning and cancellation support
4. any state needed for `Var`, `JoinHandle`, and `compute`

### Why this split matters on Scala.js
On Scala.js we cannot synchronously block waiting for `IO`.

So the rule should be:

1. core evaluation code never calls `Await.result`
2. shared Scala core never calls `unsafeRunSync`
3. `runProgF` returns `F[...]`
4. only the outermost Scala.js host boundary converts that `F` to `Future`

In other words, Scala.js support is not "special await support inside MatchlessToValue". It is "keep the whole `Prog` execution path inside `IO`, then convert to `Future` only where the JavaScript host needs it".

### What happens at the Scala.js boundary
The Scala.js adapter should:

1. choose concrete `IO` for `F`
2. run command/evaluation logic in `IO`
3. call `unsafeToFuture()` only at the topmost JS integration boundary

That boundary may be:

1. a UI event handler
2. a Node-facing API
3. a JS-exported method

Every layer below that boundary stays in `IO`, not `Future`.

### Current code paths that need to change
The current shared Scala code assumes a synchronous `Prog` runner in a few places.

#### `Output`
Right now:

1. `Output.EvaluationResult` stores `Eval[Value]`
2. `Output.RunMainResult` stores `Eval[PredefImpl.ProgRunResult]`
3. `Output.TestOutput` stores `Option[Eval[Test]]`

That shape can mostly stay if effectful `Prog` execution happens before constructing the output.

#### `EvalCommand`
Right now `EvalCommand.runOutput` is pure and uses:

1. `result.value.map(...)`
2. `PredefImpl.runProgMainWithSystemStdin(...)`

For the MatchlessToValue + cats-effect path, `runOutput` should become effectful in the surrounding command `F`:

1. evaluate the main value purely as today
2. if `--run` is requested, call `MatchlessToValue.runProgMainF`
3. once the `F[ProgRunResult]` completes, build an ordinary `Output.RunMainResult(Eval.now(result))`

That keeps `Output` simple while moving the actual program execution into `F`.

#### `TestCommand`
The same pattern applies to `ProgTest`:

1. pure evaluation still discovers and builds the `ProgTest` value
2. command execution calls `runProgTestF` inside `F`
3. once the test value is available, build ordinary `Test` output data

#### `LibraryEvaluation` and `Evaluation`
These types should keep their pure value-evaluation responsibilities.

Add effectful helpers rather than making the whole type effectful:

1. `evaluateMainValue` stays pure
2. new `runMainF(...)` helpers execute returned `Main` values in `F`
3. new `runProgTestF(...)` helpers execute returned `ProgTest` values in `F`

This keeps the pure compiler/evaluator pipeline reusable and isolates concurrency to the `Prog` boundary.

### `start`, `join`, and `cancel` in cats-effect
Inside the Scala runtime, the implementation should directly use cats-effect fibers.

Recommended mapping:

1. `start` -> `Spawn[F].start(runProgF(child, runtime))`
2. `join` -> wait on the cats-effect fiber and map its result to `JoinResult`
3. `cancel` -> `fiber.cancel`

If extra bookkeeping is needed for Bosatsu handles or repeated joins, wrap the cats-effect fiber in a small runtime object built from `Ref[F, State]` and `Deferred[F, JoinResult[...]]`.

### `compute` on Scala.js
This is the one place where Scala.js is materially different.

On JVM and Scala Native:

1. `compute` can run on the cats-effect runtime in the normal way
2. the runtime can use its normal scheduler/fiber machinery

On Scala.js:

1. `start`, `join`, and `cancel` still work as fiber concurrency
2. `compute` cannot promise a real background CPU thread in shared code
3. the shared implementation should therefore be documented as fairness-only on Scala.js: it yields back to the event loop and then evaluates the pure thunk
4. if Bosatsu later needs actual off-main-thread compute on Scala.js, that is a separate Web Worker integration, not something `MatchlessToValue` can solve by itself

So the portable contract is:

1. on JVM/Native, `compute` may move work to another runtime thread
2. on Scala.js, `compute` preserves async/fiber structure but does not create real parallel CPU execution

### Process support in `MatchlessToValue`, `bosatsu_node`, and browser Scala.js
`Bosatsu/IO/Core.spawn` and `wait` need a concrete effectful runtime design too, and this is the one place where JVM and Scala.js cannot share a single host API.

The important design rule is:

1. shared `MatchlessToValue` code depends only on a process capability inside `ProgRuntime[F]`
2. JVM provides one implementation of that capability
3. `bosatsu_node` provides a Node-specific Scala.js implementation
4. browser-oriented Scala.js provides an unsupported implementation

That avoids baking `java.lang.ProcessBuilder` assumptions in to shared Scala code.

#### Why not just use `java.lang.ProcessBuilder` everywhere
Because Scala.js does not have it.

That means the old JVM-only `Predef` strategy cannot be the shared design for process support once `MatchlessToValue` becomes the primary runtime.

#### What fs2 gives us
This repository already depends on `fs2-io` on both JVM and Scala.js, and `cliJS` already uses `fs2.io.process.ProcessBuilder` under `bosatsu_node`.

That matters because fs2 demonstrates:

1. there is already a shared Scala API for launching child processes on both JVM and Scala.js
2. on Scala.js, that fs2 implementation is Node-specific and delegates to Node `child_process.spawn`
3. so a working Node-backed process layer for `bosatsu_node` is realistic

But fs2 is not a drop-in Bosatsu process runtime contract.

Why not:

1. fs2 `ProcessBuilder` models command, args, cwd, env, and pipe-based stdio only; it does not directly model Bosatsu `StdioConfig`
2. fs2 `spawn` returns a `Resource`, and releasing that resource kills the child if it is still running
3. fs2 `Process` stream docs explicitly warn that canceling in-progress stdin/stdout/stderr work may kill the process
4. on Scala.js, fs2 process support is a Node runtime story, not a browser story

So the right design is:

1. use fs2 as precedent and optional implementation substrate where it fits
2. keep Bosatsu's own `Process`/`Handle` semantics as the source of truth
3. make the Node Scala.js runtime explicit rather than pretending all Scala.js targets are equal

#### Preferred platform split

##### JVM
On JVM, the `ProgRuntime[F]` process backend should continue to use `java.lang.ProcessBuilder` plus cats-effect-managed state.

Recommended JVM runtime object:

1. the underlying `java.lang.Process`
2. a cached `Deferred[F, Int]` or equivalent shared completion cell for the normalized exit code
3. optional bridge fibers for `UseHandle(stdin)`, `UseHandle(stdout)`, and `UseHandle(stderr)`
4. a shared completion result for those bridge fibers so repeated `wait` calls do not rerun cleanup

JVM `spawn`:

1. build a `ProcessBuilder(cmd :: args)` with no shell wrapper
2. do not override cwd or environment, so the existing Bosatsu API keeps inheriting both
3. map `Inherit`, `Null`, and `Pipe` directly to `ProcessBuilder.Redirect.INHERIT`, `Redirect.DISCARD`, and `Redirect.PIPE`
4. for `Pipe`, return Bosatsu `Handle` values around the child streams exactly as the current JVM evaluator does
5. for `UseHandle`, configure that child stream as `Redirect.PIPE`, then start a cats-effect bridge fiber
6. `stdin = UseHandle(h)` copies from the supplied readable Bosatsu handle in to the child stdin stream, then closes child stdin on EOF
7. `stdout = UseHandle(h)` or `stderr = UseHandle(h)` copies from the child stream to the supplied writable Bosatsu handle
8. if bridge setup fails, fail `spawn` with `IOError`
9. only literal `Pipe` returns `Some(handle)` in `SpawnResult`

JVM `wait`:

1. if exit code is already cached, return it immediately
2. otherwise wait on `process.onExit` inside `F` rather than blocking a thread in `waitFor`
3. normalize the resulting exit code as needed for the shared Bosatsu contract
4. if the process used `UseHandle` bridges, wait for those bridge fibers to settle before returning success from `wait`
5. if a bridge fiber failed with an IO-level error, surface that as `Prog[IOError, Int]` failure from `wait`
6. canceling the Bosatsu wait fiber does not destroy the process

##### `bosatsu_node`
For `bosatsu_node`, the Scala.js runtime should provide a separate Node-specific process backend.

Recommended Node runtime substrate:

1. use a small Scala.js facade over Node `child_process.spawn`
2. use Node stream objects directly for child stdin/stdout/stderr
3. keep the Bosatsu runtime object in Scala, just as on JVM, but back it with Node handles instead of Java process objects

This is better than trying to pretend browser Scala.js can participate, and it is also a better semantic fit than relying on raw fs2 `Resource` lifecycle as the Bosatsu process object.

Node `spawn` should:

1. call Node `child_process.spawn(cmd, args, options)` with no shell wrapper
2. map Bosatsu `Stdio` directly to Node stdio entries where possible:
3. `Inherit` -> `inherit`
4. `Null` -> `ignore`
5. `Pipe` -> `pipe`
6. `UseHandle(handle)` -> pass the underlying Node stream or file descriptor when the Bosatsu handle wraps one
7. if a Bosatsu handle cannot be natively handed to Node for `UseHandle`, fall back to the same bridge-fiber strategy used on JVM
8. return `Some(handle)` only for literal `Pipe`
9. cache both normal exit code and signal termination information from the Node child-process events

Node `wait` should:

1. if exit has already been observed, return the cached normalized code immediately
2. otherwise wait on a shared `Deferred[F, Int]` completed from Node child-process exit events
3. if Node reports normal exit, return that exit code
4. if Node reports signal termination, map the signal name through `os.constants.signals` and return `128 + signal_number`
5. if Node reports an unknown signal name that cannot be mapped, fail `wait` with `IOError`
6. if `UseHandle` fell back to bridge fibers, wait for those bridge fibers before completing `wait`
7. canceling the Bosatsu wait fiber does not call `child.kill()`

This gives `bosatsu_node` real process support without requiring impossible browser APIs.

##### Browser Scala.js
For browser-oriented Scala.js runtimes:

1. `spawn` should fail with `IOError.Unsupported`
2. `wait` should fail with `IOError.Unsupported`
3. this should be wired in as an explicit browser runtime choice, not discovered accidentally at runtime after trying to import Node modules

So the policy is:

1. generic shared Scala.js process support: unsupported
2. `bosatsu_node`: supported through a Node-specific backend
3. JVM: supported through the JVM backend

### No internal `Future` in shared core
The shared Scala implementation should not switch to `Future` internally just because Scala.js ends in a `Future`.

Reasons:

1. the rest of the tooling already composes in `F`
2. cats-effect gives cancellation and fiber semantics that `Future` does not
3. converting to `Future` too early would throw away the exact semantics we are trying to add for `start`, `join`, and `cancel`

So the design should be:

1. shared core: `Eval` for pure value construction, `F` for `Prog` execution
2. Scala.js shell: `IO` at the boundary, then `unsafeToFuture()`

### Concrete design choice
To minimize later ambiguity, this design commits to the following for Scala:

1. keep `MatchlessToValue.traverse` pure
2. add effectful `runProgF`/`runProgMainF`/`runProgTestF` helpers
3. do not block anywhere in shared Scala code
4. do not introduce `Future` into shared evaluator code
5. on Scala.js, convert `IO` to `Future` only at the outermost host boundary
6. accept that shared `compute` on Scala.js is fairness-only unless a later Web Worker backend exists
7. make process support a runtime capability with three explicit cases: JVM supported, `bosatsu_node` supported, browser Scala.js unsupported

## Detailed Python Runtime Design

### Process support in `ProgExt.py`
The current Python runtime already has the right overall shape for `spawn` and `wait`: it uses `subprocess.Popen`, it returns Bosatsu handles for `Pipe`, and it caches the exit code on the process object.

The design should keep that shape and tighten the concurrency details.

#### `spawn`
Recommended Python `spawn` behavior:

1. continue to call `subprocess.Popen([cmd, *args], shell=False, stdin=..., stdout=..., stderr=...)`
2. keep inheriting cwd and environment, because the Bosatsu API does not yet expose either
3. map `Inherit` to `None`, `Pipe` to `subprocess.PIPE`, `Null` to `subprocess.DEVNULL`, and `UseHandle` to the existing Python stream object stored in the Bosatsu handle
4. validate readable vs writable directions exactly as the current implementation already does
5. return `Some(handle)` only for streams configured as `Pipe`
6. keep the current UTF-8-oriented process pipe behavior; the Python runtime's byte helpers already peel through `.buffer` when present, so one Bosatsu handle can still support both UTF-8 and byte operations

#### `wait`
For correctness once Bosatsu adds concurrency, the Python `Process` object should carry a little more shared state than it does today:

1. the underlying `subprocess.Popen`
2. cached normalized exit code
3. a lock
4. an `Event` or equivalent shared completion signal
5. a flag indicating whether a background waiter thread has already been started

Recommended Python `wait` behavior:

1. if the exit code is already cached, return it immediately
2. otherwise, ensure exactly one daemon waiter thread is started for that process
3. that waiter thread calls `Popen.wait()`, normalizes the result, stores it, and signals the shared completion event
4. all Bosatsu `wait(process)` calls block on that one shared completion event rather than issuing their own competing `Popen.wait()` calls
5. on POSIX, if `Popen.wait()` reports a negative return code, normalize it to `128 + signal_number`
6. canceling a Bosatsu fiber blocked in `wait(process)` does not kill the external process

This keeps the Python runtime simple, preserves the current `subprocess`-based design, and gives repeated/concurrent `wait` calls a precise shared result model.

## Detailed C/libuv Runtime Design

### Why the current C runtime must change
Today the C runtime is centered on `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c`, where:

1. `bsts_Bosatsu_Prog_run` is a single synchronous stack machine.
2. `ProgTagEffect` calls a C callback that must return the next `Prog` immediately.
3. `Bosatsu/IO/Core` uses blocking stdio and POSIX-style calls in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c`.

That model cannot support a real libuv backend:

1. `uv_run()` is the central libuv loop and is not reentrant.
2. libuv handles are long-lived and requests are short-lived objects that complete later in callbacks.
3. file system requests, timers, process exit, stream reads, and `uv_queue_work()` all complete asynchronously.

So the libuv backend cannot keep the current "effect callback returns the next `Prog` now" contract internally. It needs a scheduler that can suspend a fiber and later resume it from a libuv callback.

### High-level runtime shape
Keep the public Bosatsu API unchanged and replace the C runtime internals with:

1. `BSTS_Prog_Runtime`
   - owns one `uv_loop_t`
   - owns the scheduler queue of ready fibers
   - owns one `uv_idle_t` used to drain the ready queue
   - tracks the root fiber result
   - tracks outstanding child fibers and open runtime-owned handles
2. `BSTS_Prog_Fiber`
   - current `Prog` node being evaluated
   - continuation stack for `flat_map` and `recover`
   - state: `Ready`, `Running`, `Suspended`, `Done`
   - cancellation flag
   - pointer to optional join state
3. `BSTS_Prog_Join`
   - terminal result slot: `Succeeded`, `Errored`, `Canceled`, or still running
   - intrusive list of fibers waiting in `join`
   - ownership count so the runtime can free it after all waiters and handles are gone
4. `BSTS_Prog_Request`
   - heap object used as the baton for each in-flight libuv request
   - stores the owning fiber
   - stores request-kind-specific payload such as buffers, paths, offsets, or write data
   - stores cleanup and cancellation hooks

### Root execution flow
`bsts_Bosatsu_Prog_run_main` and `bsts_Bosatsu_Prog_run_test` should become:

1. allocate a fresh `uv_loop_t` with `uv_loop_init`
2. initialize a `BSTS_Prog_Runtime` around that loop
3. initialize a scheduler `uv_idle_t`
4. create the root fiber from the Bosatsu `Main` or `ProgTest`
5. enqueue the root fiber on the ready queue
6. start the idle handle so ready fibers are drained
7. call `uv_run(loop, UV_RUN_DEFAULT)`
8. once the root fiber is terminal, cancel any detached child fibers that are still running
9. continue draining close callbacks until `uv_loop_alive(loop)` is false
10. call `uv_loop_close`

This matches libuv's requirement that the loop can only be closed after all handles and requests are closed.

### Scheduler design
The runtime should use a cooperative fiber scheduler on top of the single libuv loop thread.

#### Ready queue
1. Fibers that can continue immediately are pushed onto a ready queue.
2. A single `uv_idle_t` callback drains that queue.
3. When the queue becomes empty, stop the idle handle.
4. When the queue transitions from empty to non-empty, start the idle handle again.

Using `uv_idle_t` is important because active idle handles force libuv to poll with zero timeout instead of blocking for I/O. That lets pure Bosatsu fibers continue to make progress without reentering `uv_run()` from inside callbacks.

#### Step budget
Each time a fiber is scheduled, it should run only up to a fixed step budget, for example 1024 `Prog` nodes, before being re-enqueued.

This prevents one long pure Bosatsu loop from monopolizing the event loop thread and starving timers, process exit callbacks, stream reads, or completed filesystem requests.

#### No direct recursive resume from callbacks
libuv callbacks should never directly run an entire fiber to completion. They should:

1. write the fiber's next `Prog` value
2. mark the fiber `Ready`
3. enqueue it
4. return

The idle scheduler callback is the only place that drains ready fibers. This avoids callback nesting and respects the fact that `uv_run()` is not reentrant.

### Internal effect ABI
The current C runtime treats `ProgTagEffect` as:

1. payload argument
2. pure callback `BValue -> BValue`

That is too weak for libuv because an effect may complete later. The public `Prog` tag can stay the same, but the internal C ABI of the effect payload should change to a start/resume protocol.

Recommended internal shape:

```c
typedef enum {
  BSTS_EFFECT_IMMEDIATE,
  BSTS_EFFECT_PENDING
} BSTS_Effect_Result_Tag;

typedef struct {
  BSTS_Effect_Result_Tag tag;
  BValue next_prog; /* valid only for IMMEDIATE */
} BSTS_Effect_Result;

typedef BSTS_Effect_Result (*BSTS_Effect_Start)(
  BSTS_Prog_Runtime *runtime,
  BSTS_Prog_Fiber *fiber,
  BValue arg,
  void *effect_data
);
```

Then:

1. `bsts_prog_effect1/2/3/4` create `ProgTagEffect` values holding the effect argument and an opaque effect descriptor.
2. When the fiber stepper sees `ProgTagEffect`, it calls the descriptor's `start(...)`.
3. If the result is `BSTS_EFFECT_IMMEDIATE`, evaluation continues in the same fiber activation with `next_prog`.
4. If the result is `BSTS_EFFECT_PENDING`, the fiber becomes `Suspended` and will only resume from a later libuv callback.

The effect descriptor should also carry:

1. a cancel hook
2. a cleanup hook
3. an effect kind tag for debugging

### Fiber semantics in the libuv runtime
The synchronous `Pure`, `Raise`, `FlatMap`, `Recover`, and `ApplyFix` logic can remain the same as today.

Only the `Effect` case changes:

1. synchronous effects such as `pure`, `raise_error`, `observe`, `new_var`, and most `Var` operations complete immediately
2. asynchronous effects such as `sleep`, `compute`, stream reads, stream writes, file reads, file writes, `spawn`, and `wait` suspend the current fiber
3. the completion callback later installs either `prog_pure(value)` or `prog_raise_error(err)` as the fiber's next `Prog` and re-enqueues the fiber

This lets existing Bosatsu code continue to think of `Prog` as one monad, while the C runtime turns that monad into a libuv-managed continuation machine.

### JoinHandle implementation in C
`start`, `join`, and `cancel` should be implemented directly in the fiber scheduler.

#### C `start`
1. Create a child `BSTS_Prog_Fiber`.
2. Create a `BSTS_Prog_Join` state object in `Running`.
3. Point the child at that join state.
4. Enqueue the child.
5. Return the opaque `JoinHandle`.

#### C `join`
1. If the join state is terminal, return that `JoinResult` immediately.
2. Otherwise, add the current fiber to the join waiters list and suspend it.
3. When the child reaches a terminal state, all join waiters are re-enqueued.

#### C `cancel`
1. Set the child's cancellation flag.
2. If the child is currently suspended in a request that supports cancellation, invoke its cancel hook.
3. If the child is ready but not running, leave it in the queue and let the next scheduler checkpoint observe cancellation.
4. If the child is already terminal, do nothing.

### Cancellation policy for pending libuv work
Cancellation must be effect-specific.

#### Canceling `compute`
1. Use `uv_queue_work`.
2. If `uv_cancel()` succeeds before work starts, the after-work callback reports `UV_ECANCELED` and the fiber becomes `Canceled`.
3. If work has already started, cancellation becomes best-effort only. The worker thread runs to completion, but the after-work callback checks the fiber cancellation flag and discards the computed value.

#### Filesystem requests
1. Requests such as `uv_fs_open`, `uv_fs_read`, `uv_fs_write`, `uv_fs_close`, `uv_fs_scandir`, `uv_fs_lstat`, and `uv_fs_rename` should be issued asynchronously with callbacks.
2. If `uv_cancel()` succeeds, the callback will eventually run with `UV_ECANCELED`.
3. If the request is already executing, cancellation becomes best-effort only and the callback result is discarded if the fiber is already canceled.

#### Stream reads and writes
1. `uv_write_t` requests are canceled by `uv_close()` of the underlying handle; libuv reports the write callback with `UV_ECANCELED`.
2. A pending stream read is canceled by removing the Bosatsu read waiter from the handle's pending-read queue. If the handle has no pending reads and no buffered demand, `uv_read_stop()` may be called.
3. Canceling a Bosatsu fiber that is waiting on a stream read does not close the stream itself.

#### Canceling `sleep`
1. Implement `sleep` with a one-shot `uv_timer_t`.
2. Canceling `sleep` stops the timer and closes the timer handle.
3. The fiber then resumes as `Canceled`.

#### Canceling `wait(process)`
1. Canceling a fiber blocked in `Bosatsu/IO/Core::wait` only cancels the Bosatsu wait.
2. It does not kill the external process.
3. If we later want process termination, that should be a separate process API, not part of fiber cancellation.

### C `Handle` representation
The current C runtime stores:

```c
typedef struct {
  BSTS_Handle_Kind kind;
  FILE *file;
  int readable;
  int writable;
  int close_on_close;
  int closed;
} BSTS_Core_Handle;
```

That must be replaced in the libuv backend with an internal union:

```c
typedef enum {
  BSTS_HANDLE_FILE,
  BSTS_HANDLE_STREAM
} BSTS_Handle_Kind;

typedef struct BSTS_Core_Handle BSTS_Core_Handle;

struct BSTS_Core_Handle {
  BSTS_Handle_Kind kind;
  int readable;
  int writable;
  int closed;
  int closing;
  int eof;
  int close_on_close;

  union {
    struct {
      uv_file fd;
      int64_t offset;
      int append_mode;
    } file;
    struct {
      uv_stream_t *stream;
      ByteQueue read_buffer;
      PendingRead *reads_head;
      PendingRead *reads_tail;
      PendingWrite *writes_head;
      PendingWrite *writes_tail;
      size_t buffered_bytes;
      int read_started;
      int backpressured;
    } stream;
  } as;
};
```

The Bosatsu `Handle` surface stays opaque. Only the runtime representation changes.

### Regular files vs streams
The libuv backend should not force everything through one abstraction internally.

#### Regular-file handles
Use `uv_file` plus `uv_fs_*` requests:

1. `open_file` -> `uv_fs_open`
2. `read_bytes` and `read_utf8` -> `uv_fs_read`
3. `write_bytes` and `write_utf8` -> `uv_fs_write`
4. `close` -> `uv_fs_close`
5. `stat` -> `uv_fs_lstat`
6. `rename` -> `uv_fs_rename`
7. `mkdir` -> `uv_fs_mkdir`
8. `list_dir` -> `uv_fs_scandir`
9. temp paths -> `uv_fs_mkdtemp` and `uv_fs_mkstemp`

Each regular-file handle should track a logical offset:

1. for `Read`, start at `0` and increment after each successful read
2. for `WriteTruncate`, start at `0`
3. for `Append`, open with append flags and let the OS append semantics decide the actual write position

#### Stream handles
Use `uv_stream_t` plus stream requests:

1. stdin/stdout/stderr
2. process pipes created by `spawn`
3. any future socket-like handles

For streams:

1. reading is driven by `uv_read_start`
2. writing is driven by `uv_write`
3. closing is driven by `uv_close`

### stdin/stdout/stderr under libuv
Do not keep wrapping C stdio `stdin`, `stdout`, and `stderr` as `FILE*`.

Instead:

1. detect the underlying stdio kind
2. if it is a TTY, wrap it with `uv_tty_t`
3. otherwise wrap it with `uv_pipe_t` using the inherited file descriptor

This keeps Bosatsu stdio on libuv-native stream handles, which is required for nonblocking reads and async writes.

### Read semantics
The Bosatsu surface already has one opaque `Handle`, but the runtime must distinguish file reads from stream reads.

#### `read_bytes`
For regular files:

1. issue one `uv_fs_read` request for up to `max_bytes`
2. on `result == 0`, return `None`
3. otherwise return exactly the bytes read
4. advance the file offset by the number of bytes read

For streams:

1. maintain a byte queue on the handle
2. keep `uv_read_start` active while the stream is readable
3. if buffered bytes are available, satisfy the Bosatsu read immediately
4. if EOF has already been observed and the buffer is empty, return `None`
5. otherwise register the fiber as a pending read waiter and suspend it

#### `read_utf8`
For regular files:

1. issue `uv_fs_read` for a raw byte chunk
2. decode only complete UTF-8 prefixes
3. if the returned bytes end mid-code-point, keep the trailing partial bytes in a small per-handle decode buffer and prepend them to the next read

For streams:

1. read raw bytes into the same stream byte queue used by `read_bytes`
2. decode from that queue only when at least one full UTF-8 code point is available
3. if the queue ends in a partial code point and EOF has not occurred, wait for more bytes
4. if EOF occurs with an incomplete trailing sequence, return `InvalidUtf8`

This is stricter and more correct than the current `fread` implementation, which can fail merely because a chunk boundary split a multibyte UTF-8 code point.

### Backpressure for stream reads
If the libuv backend keeps `uv_read_start` active forever, unread data can accumulate without bound. The runtime should therefore implement explicit buffering thresholds.

Recommended policy:

1. high-water mark: 1 MiB of buffered unread stream data
2. low-water mark: 256 KiB
3. once buffered unread data reaches the high-water mark, call `uv_read_stop`
4. once consumers drain the buffer below the low-water mark, call `uv_read_start` again

This keeps process pipes and stdin responsive without turning unread output into unbounded memory growth.

### Write semantics
#### `write_bytes` and `write_utf8`
For regular files:

1. allocate a request baton owning the bytes to write
2. issue `uv_fs_write`
3. on success, advance the file offset for non-append files
4. resume the suspended fiber with `Unit`

For streams:

1. copy or retain the bytes until the `uv_write_t` callback fires
2. queue writes in order per handle
3. resume the waiting fiber from the write callback

#### `flush`
To match the current JVM, Python, and C meanings, `flush` should not mean durable disk sync.

Instead:

1. for regular files, `flush` waits for all prior write requests on that handle to complete
2. for streams, `flush` waits for all queued `uv_write_t` requests on that handle to complete
3. no `uv_fs_fsync` is implied by `flush`

If Bosatsu later needs durable sync, that should be a separate explicit file API.

### Closing handles
#### Closing regular files
1. mark the handle closed
2. if there are no in-flight requests, issue `uv_fs_close`
3. if there are in-flight requests, defer close until the last callback completes

#### Closing streams
1. mark the handle closed
2. call `uv_close`
3. free the libuv stream object only from `close_cb`

This follows libuv's rule that handle memory must stay valid until `close_cb`.

### `Bosatsu/IO/Core` mapping in the libuv backend
The built-in effectful IO surface should map directly to libuv:

1. `open_file` -> `uv_fs_open`
2. `create_temp_file` -> `uv_fs_mkstemp`
3. `create_temp_dir` -> `uv_fs_mkdtemp`
4. `list_dir` -> `uv_fs_scandir` + `uv_fs_scandir_next`
5. `stat` -> `uv_fs_lstat`
6. `mkdir` -> `uv_fs_mkdir`, with Bosatsu-side recursion helper issuing repeated requests
7. `remove` -> `uv_fs_unlink` or `uv_fs_rmdir`, with recursive tree walk built from `uv_fs_lstat` and `uv_fs_scandir`
8. `rename` -> `uv_fs_rename`
9. `get_env` -> `uv_os_getenv`
10. `spawn` -> `uv_spawn`
11. `wait` -> process exit callback over `uv_process_t`
12. `now_wall` -> `uv_clock_gettime(UV_CLOCK_REALTIME, ...)` when available, otherwise existing platform fallback
13. `now_mono` -> `uv_hrtime()` or `uv_clock_gettime(UV_CLOCK_MONOTONIC, ...)`
14. `sleep` -> `uv_timer_t`

### Process implementation
The current C runtime reports `spawn` and `wait` as unsupported. The libuv backend should make them first-class.

Recommended `Process` runtime object:

1. `uv_process_t process`
2. cached normalized Bosatsu exit code
3. completion flag
4. intrusive list of Bosatsu fibers waiting in `wait`
5. optional wrapped handles for child stdin/stdout/stderr pipes

#### Process `spawn`
1. build `uv_process_options_t` with argv semantics and no shell wrapper
2. leave `cwd = NULL` and `env = NULL` so the current Bosatsu API continues to inherit both
3. map Bosatsu `Stdio` to `uv_stdio_container_t`
4. `Inherit` maps to `UV_INHERIT_FD` or `UV_INHERIT_STREAM`
5. `Null` maps to `UV_IGNORE`
6. `Pipe` creates a fresh `uv_pipe_t` and uses `UV_CREATE_PIPE` plus direction flags from the child-process perspective:
7. child stdin uses `UV_READABLE_PIPE`
8. child stdout and child stderr use `UV_WRITABLE_PIPE`
9. `UseHandle(handle)` should use native inheritance, not a background copy task:
10. if the Bosatsu handle wraps a `uv_stream_t`, use `UV_INHERIT_STREAM`
11. if the Bosatsu handle wraps a regular-file descriptor, use `UV_INHERIT_FD`
12. validate direction before spawning: stdin requires a readable Bosatsu handle; stdout/stderr require writable Bosatsu handles
13. return `Some(handle)` only for literal `Pipe`; `UseHandle` still returns `None` in `SpawnResult`
14. if any step fails after allocating temporary `uv_pipe_t` handles, close them and fail `spawn` with `IOError`

#### Process `wait`
1. if the process has already exited, return the cached exit code immediately
2. otherwise add the current fiber to the process waiters list and suspend it
3. in the `uv_exit_cb`, normalize libuv's `(exit_status, term_signal)` pair to the Bosatsu `Int` contract:
4. if `term_signal == 0`, return `exit_status`
5. otherwise return `128 + term_signal`
6. cache that normalized code and wake all waiters
7. canceling one Bosatsu `wait(process)` removes only that waiter; it does not call `uv_process_kill`
8. `wait` does not close or drain any returned `Pipe` handles
9. close the `uv_process_t` handle only after the exit callback

### `compute` implementation
`compute` should use `uv_queue_work`.

#### Worker-side execution
1. the work callback evaluates the Bosatsu thunk `Unit -> a`
2. it stores the resulting `BValue` in the request baton
3. it does not call libuv APIs other than thread-safe primitives allowed from worker threads

#### Loop-side completion
1. the after-work callback runs on the libuv loop thread
2. if status is `UV_ECANCELED`, resume the fiber as canceled
3. otherwise, if the fiber was canceled while the work was already running, discard the result and mark the fiber canceled
4. otherwise resume the fiber with `prog_pure(result)`

This gives `compute` the right "off the event loop" behavior without inventing a second scheduler.

### libuv `sleep`
Implement `sleep` with a one-shot `uv_timer_t`:

1. create timer handle
2. `uv_timer_start(..., timeout_ms, 0)`
3. on timer callback, close the timer handle and resume the fiber with `Unit`
4. on cancellation, stop and close the timer handle and mark the fiber canceled

### `Var` under libuv
`Var` does not need libuv. The current atomic/CAS implementation can stay:

1. it is already nonblocking
2. it is already effectful only at `Prog` execution time
3. it composes with the fiber scheduler without extra event-loop work

### Migration from the current C implementation
The current files should not be incrementally peppered with `#ifdef LIBUV` until they are unreadable.

Recommended implementation split:

1. keep the current synchronous runtime as a separate backend for environments that do not want libuv
2. add a libuv runtime implementation in separate files
3. select the backend at build time

Suggested file layout:

1. `c_runtime/bosatsu_prog_uv.h`
2. `c_runtime/bosatsu_prog_uv.c`
3. `c_runtime/bosatsu_ext_Bosatsu_l_Prog_uv.c`
4. `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core_uv.c`
5. `c_runtime/Makefile` or build config changes to link libuv

The existing synchronous files remain the reference implementation for the old backend, but the libuv backend becomes the concurrency-capable one.

### Concrete change from today's C runtime
The migration is not "add a few libuv calls". It is:

1. replace the single blocking `bsts_Bosatsu_Prog_run` loop with a fiber scheduler on top of `uv_run`
2. replace `FILE*` handles with a `uv_file`/`uv_stream_t` union
3. replace blocking `fread`/`fwrite`/`fopen`/`nanosleep` paths with `uv_fs_*`, `uv_read_start`, `uv_write`, `uv_timer_t`, and `uv_spawn`
4. implement `spawn` and `wait` for real
5. add request batons, close callbacks, and cleanup paths required by libuv
6. keep the Bosatsu package surface unchanged

### Chosen policy for the libuv backend
To minimize later decision churn, this design commits to the following:

1. use libuv directly for all built-in C backend IO and scheduling
2. do not keep `FILE*` in the libuv backend
3. do not reinterpret `flush` as `fsync`
4. do not kill external processes when canceling a Bosatsu fiber blocked in `wait(process)`
5. do not reenter `uv_run()` from callbacks
6. use a scheduler step budget for fairness
7. use `uv_queue_work` for `compute`
8. use `uv_fs_*` for regular files and `uv_stream_t` operations for streams

### libuv + libgc interaction
The libuv backend is not just "an event loop dependency". It is a multi-threaded runtime:

1. the event loop itself runs on one thread
2. `uv_queue_work()` runs callbacks on libuv worker threads
3. asynchronous filesystem operations also use libuv's global threadpool internally

At the same time, Bosatsu's C runtime allocates nearly all runtime objects with Boehm GC (`GC_malloc`, `GC_malloc_atomic`, etc.), so the libuv backend must be designed as a multithreaded GC client from day one.

#### What libuv guarantees
Per libuv documentation:

1. libuv provides cross-platform threading and synchronization primitives
2. libuv has a global threadpool
3. that threadpool is used for `uv_queue_work()` and for filesystem operations

So if we adopt libuv, the C runtime is definitely multi-threaded even if Bosatsu user code never calls `start`.

#### What libgc requires
Per bdwgc upstream:

1. the collector is built with multi-threading support enabled by default unless explicitly disabled
2. multithreaded client code should define `GC_THREADS` before including `gc.h`
3. threads created by third-party libraries normally require explicit registration with `GC_register_my_thread()`
4. such explicitly registered threads must later call `GC_unregister_my_thread()`

This is the most important integration constraint in the entire design.

#### Consequence for the Bosatsu libuv backend
The event-loop thread is easy:

1. initialize Boehm early with `GC_INIT()`
2. define `GC_THREADS` in runtime source files that use the collector
3. treat the main/event-loop thread as the implicitly registered main thread

The worker-thread story is the real design point:

1. libuv worker threads are created by libuv, not by Bosatsu code
2. bdwgc explicitly calls out threads created by third-party libraries as the case that normally requires manual registration
3. therefore any libuv worker thread that touches GC-managed data must register itself with Boehm first

#### Required runtime rule
For correctness, the libuv backend should follow this rule:

1. no `uv_work_cb` may allocate GC memory or manipulate pointers to the GC heap unless that worker thread has been explicitly registered with Boehm

That means `compute` cannot be implemented safely by simply calling the Bosatsu thunk on a raw libuv worker thread without GC registration.

#### Recommended implementation strategy
There are two realistic approaches.

##### Preferred approach
Register libuv worker threads with Boehm when they first enter Bosatsu-managed work:

1. call `GC_allow_register_threads()` once on the main thread after initialization and before the first worker-thread registration
2. at the start of a `uv_work_cb` that will touch Bosatsu values, obtain the stack base and call `GC_register_my_thread(...)`
3. run the Bosatsu work callback
4. before the worker thread returns to libuv, call `GC_unregister_my_thread()`

This follows the upstream guidance for third-party-created threads and works on Unix and Windows.

##### Conservative fallback
Keep libuv worker callbacks free of GC heap interaction:

1. worker threads operate only on plain C buffers and POD state
2. all GC-managed `BValue` creation/publication happens back on the loop thread in the after-work callback

This is easier for filesystem requests that already return plain C data from libuv, but it is too restrictive for `compute`, whose whole job is to evaluate a Bosatsu thunk.

So the fallback is useful for some request batons, but not sufficient as the only model once `compute` exists.

#### Cross-platform libuv + libgc runtime rules
For every platform, the libuv backend should assume:

1. a thread-safe Boehm build
2. `GC_THREADS` is defined when compiling libuv-backend runtime sources that include `gc.h`
3. `GC_INIT()` runs on the main thread before Bosatsu runtime work starts
4. `GC_allow_register_threads()` is called before the first manual worker-thread registration
5. any libuv-created worker thread must call `GC_register_my_thread(...)` before touching GC-managed values
6. that worker thread must later call `GC_unregister_my_thread()`
7. no reliance on deprecated implicit thread discovery

These are not macOS-specific or Linux-specific rules. They are the shared runtime contract for any Bosatsu backend that combines libuv worker threads with Boehm-managed heap objects.

The doc intentionally chooses explicit registration over implicit discovery because bdwgc documents `GC_use_threads_discovery()` as deprecated/less robust.

### Current platform build plan
This section is specifically about the platforms Bosatsu uses today.

#### Ubuntu and Debian
For Linux builds on Ubuntu/Debian, the build should assume:

1. `libuv1-dev`
2. `libgc-dev`
3. `pkg-config`
4. normal C toolchain packages such as `build-essential`

Why this is enough:

1. Ubuntu's `libuv1-dev` package description explicitly includes process/thread management, pipes, and work queues
2. Debian's `libuv1-dev` file list includes `uv/threadpool.h`, `libuv.pc`, and `libuv-static.pc`
3. upstream libuv's threadpool and threading APIs are part of the standard library surface, not an optional "concurrency flavor"

So for Ubuntu/Debian, the answer to "is libuv already built with concurrency support?" is effectively yes. We should treat distro `libuv1-dev` as the normal libuv with threadpool/thread APIs available, because that is exactly what the package exposes.

Recommended install line:

```sh
sudo apt-get update
sudo apt-get install -y build-essential pkg-config libuv1-dev libgc-dev
```

Recommended build flag discovery:

```sh
pkg-config --cflags --libs libuv bdw-gc
```

Minimum Bosatsu-specific compile setting:

```sh
CPPFLAGS="-DGC_THREADS $(pkg-config --cflags libuv bdw-gc)"
LIBS="$(pkg-config --libs libuv bdw-gc) -lm"
```

#### macOS with Homebrew
For macOS builds, the build should assume:

1. `brew install libuv bdw-gc pkg-config`
2. Apple Clang as the compiler
3. Homebrew bottles for both `libuv` and `bdw-gc` are available on current Apple Silicon and Intel macOS releases

What about Boehm threading support on Homebrew?

1. the Homebrew `bdw-gc` formula builds with CMake and does not pass an option disabling threads
2. upstream bdwgc states that multi-threading support is enabled by default unless explicitly disabled

So the right conclusion is:

1. Homebrew `bdw-gc` on macOS should be suitable for a multithreaded Bosatsu runtime
2. the same cross-platform libuv + libgc runtime rules above still apply on macOS

Recommended install line:

```sh
brew install libuv bdw-gc pkg-config
```

Recommended build flag discovery:

```sh
pkg-config --cflags --libs libuv bdw-gc
```

Minimum Bosatsu-specific compile setting:

```sh
CPPFLAGS="-DGC_THREADS $(pkg-config --cflags libuv bdw-gc)"
LIBS="$(pkg-config --libs libuv bdw-gc) -lm"
```

If `pkg-config` metadata is missing or unreliable on a local Homebrew setup, the build should fall back to explicit prefixes:

```sh
UV_PREFIX="$(brew --prefix libuv)"
GC_PREFIX="$(brew --prefix bdw-gc)"
CPPFLAGS="-DGC_THREADS -I$UV_PREFIX/include -I$GC_PREFIX/include"
LDFLAGS="-L$UV_PREFIX/lib -L$GC_PREFIX/lib"
LIBS="-luv -lgc -lm"
```

This mirrors the fallback style already used by the current `c_runtime/Makefile` for `bdw-gc`.

#### Build-system implication for Bosatsu
PR 1 should extend the current C build in this style:

1. keep `pkg-config` as the preferred mechanism
2. add `libuv` discovery alongside `bdw-gc`
3. add `-DGC_THREADS` to the libuv backend compile flags
4. preserve explicit Homebrew-prefix fallbacks for macOS
5. preserve the old backend build target so missing libuv does not break the current runtime

### Future work: Windows support
Windows should be treated as a later, separate enablement step rather than part of the first libuv rollout.

#### Why Windows is a distinct project
Windows support adds two independent moving parts:

1. libuv itself uses a different host API surface on Windows
2. Boehm thread integration is different enough on Windows that we should validate it separately

The good news is that both upstreams support Windows:

1. libuv upstream documents Windows as a first-class platform and says CMake is the supported build method there
2. bdwgc upstream documents support for recent Windows versions and Win32 threads
3. vcpkg currently ships both `libuv` and `bdwgc` packages, including Windows triplets

#### Recommended Windows dependency strategy
When Bosatsu eventually adds Windows C runtime support, prefer:

1. CMake-based C runtime build
2. MSVC toolchain
3. vcpkg for dependency acquisition

Recommended future bootstrap:

```powershell
vcpkg install libuv bdwgc
cmake -B build -S . -DCMAKE_TOOLCHAIN_FILE=<vcpkg-root>/scripts/buildsystems/vcpkg.cmake
cmake --build build --config Release
```

This avoids inventing Bosatsu-specific dependency instructions for Windows in the first pass.

#### Windows-specific validation concerns
Windows does not change the shared libuv + libgc rules above, but it does need separate validation under the Windows toolchain and host APIs.

#### Windows-specific runtime work items
A future Windows enablement PR should explicitly cover:

1. build and link of the libuv backend under MSVC
2. Boehm thread registration on libuv worker threads
3. stdio handle initialization for Windows console and pipe handles
4. path and process behavior on Windows via libuv
5. CI on `windows-latest`

The Windows work should not be merged piecemeal until:

1. dependency acquisition is reproducible
2. the runtime passes the same basic `Prog`, `IO/Core`, `compute`, and `start`/`join`/`cancel` tests as the Unix targets

### References
1. libuv event loop: <https://docs.libuv.org/en/v1.x/loop.html>
2. libuv basics, handles vs requests: <https://docs.libuv.org/en/v1.x/guide/basics.html>
3. libuv filesystem APIs: <https://docs.libuv.org/en/v1.x/fs.html>
4. libuv filesystem guide: <https://docs.libuv.org/en/v1.x/guide/filesystem.html>
5. libuv threadpool and `uv_queue_work`: <https://docs.libuv.org/en/v1.x/threadpool.html>
6. libuv request cancellation: <https://docs.libuv.org/en/v1.x/request.html>
7. libuv handle closing: <https://docs.libuv.org/en/v1.x/handle.html>
8. libuv timers: <https://docs.libuv.org/en/v1.x/timer.html>
9. libuv processes: <https://docs.libuv.org/en/v1.x/process.html>
10. libuv threadpool: <https://docs.libuv.org/en/v1.x/threadpool.html>
11. libuv threading primitives: <https://docs.libuv.org/en/stable/threading.html>
12. Ubuntu `libuv1-dev` package description: <https://launchpad.net/ubuntu/jammy/%2Bpackage/libuv1-dev>
13. Debian `libuv1-dev` file list: <https://packages.debian.org/bullseye/amd64/libuv1-dev/filelist>
14. Homebrew `libuv` formula: <https://formulae.brew.sh/formula/libuv>
15. Homebrew `bdw-gc` formula: <https://formulae.brew.sh/formula/bdw-gc>
16. Homebrew `bdw-gc` formula code: <https://github.com/Homebrew/homebrew-core/blob/7c5d67716e44ab8c2ca27d1866c6a0504e17bf23/Formula/b/bdw-gc.rb>
17. Homebrew `libuv` formula code: <https://github.com/Homebrew/homebrew-core/blob/cbaf449cc66bec68e7d12e7f81712284e94d1b25/Formula/lib/libuv.rb>
18. bdwgc README: <https://github.com/bdwgc/bdwgc>
19. bdwgc interface notes: <https://www.hboehm.info/gc/gcinterface.html>
20. bdwgc simple example and threading notes: <https://www.hboehm.info/gc/simple_example.html>
21. bdwgc `gc.h` thread registration API: <https://raw.githubusercontent.com/bdwgc/bdwgc/master/include/gc/gc.h>
22. libuv upstream build instructions: <https://github.com/libuv/libuv>
23. vcpkg `libuv` package: <https://vcpkg.io/en/package/libuv>
24. vcpkg `bdwgc` package: <https://vcpkg.io/en/package/bdwgc>
25. Python `subprocess` documentation: <https://docs.python.org/3/library/subprocess.html>
26. Java `Process` documentation: <https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/lang/Process.html>
27. Node `child_process` documentation: <https://nodejs.org/api/child_process.html>
28. fs2 JVM process sources (`fs2-io_3` 3.13.0): <https://repo1.maven.org/maven2/co/fs2/fs2-io_3/3.13.0/fs2-io_3-3.13.0-sources.jar>
29. fs2 Scala.js process sources (`fs2-io_sjs1_3` 3.13.0): <https://repo1.maven.org/maven2/co/fs2/fs2-io_sjs1_3/3.13.0/fs2-io_sjs1_3-3.13.0-sources.jar>

## Non-goals
1. No resource-safe cancellation story in v1. Bosatsu does not yet have `bracket`, masking, or finalizers.
2. No guarantee of immediate cancellation of pure code.
3. No scheduler tuning API, priorities, or affinity knobs.
4. No `race`, `timeout`, `par_map`, or `poll` in the initial surface.
5. No separate "blocking foreign call offload" API in this design. The built-in C backend should move its own file/process/timer operations to libuv directly, and `compute` remains pure-only.

## Follow-up APIs After This One
Once the base handle semantics exist, the next useful layer is likely:

1. `poll(handle) -> Prog[f, Option[JoinResult[err, a]]]`
2. `race(left, right)`
3. `par_map2`
4. `yield_now` or `cede` if libuv fairness becomes important for long pure loops

Those should wait until the semantics of `start`, `join`, `cancel`, and `compute` are proven across all backends.

## Staged Rollout And PR Plan
The rollout should maximize these properties:

1. the repo stays green after every PR
2. the current synchronous C backend keeps working until libuv parity exists
3. public Bosatsu surface area grows only when all required backends in this repo can support it
4. the hardest semantic feature, `cancel`, lands last

### Rollout principles
1. Add the libuv C backend first, but do not expose new Bosatsu concurrency functions until the libuv backend can already run the current `Bosatsu/IO/Core` API.
2. Keep the current synchronous C runtime in the tree throughout the migration as a separate backend.
3. Add internal runtime machinery before exposing public API, both in the libuv C backend and in Scala `MatchlessToValue`.
4. Keep the current synchronous Scala `Predef` runner available until the effectful `MatchlessToValue` runner has enough parity to take over the command paths that execute `Prog`.
5. Ship public concurrency in three API slices:
   1. `compute`
   2. `JoinHandle` + `JoinResult` + `start` + `join`
   3. `cancel`
6. Every PR must have tests targeted to the exact slice it changes.

### PR 1: libuv build plumbing, no behavior change
Scope:

1. add libuv as an optional C runtime dependency
2. add separate libuv runtime source files and build targets
3. keep the existing synchronous C backend as the default/reference backend
4. add CI or local build coverage that the libuv backend compiles

Do not do yet:

1. no public Bosatsu API changes
2. no semantic changes to existing runtime behavior
3. no attempt to port IO functions yet

Acceptance:

1. existing tests stay green on the current default backend
2. the libuv backend builds successfully as an alternate target

### PR 2: internal libuv `Prog` scheduler, still no public concurrency API
Scope:

1. add `BSTS_Prog_Runtime`, `BSTS_Prog_Fiber`, ready queue, and `uv_idle_t` scheduler
2. replace the old single blocking C `Prog` loop in the libuv backend with the new suspend/resume engine
3. support immediate effects needed for current `Prog` behavior: `pure`, `raise_error`, `flat_map`, `recover`, `apply_fix`, `observe`, and `Var`

Do not do yet:

1. no new Bosatsu exports
2. no async IO/Core port beyond what is needed to boot the runtime
3. no `start`, `join`, `cancel`, or `compute`

Acceptance:

1. existing `Prog` and `Var` tests pass on the libuv backend
2. libuv runtime can run simple `Main` and `ProgTest` values with no concurrency features

### PR 3: libuv time primitives
Scope:

1. port `now_wall`
2. port `now_mono`
3. port `sleep` using `uv_timer_t`

Reason:

1. `sleep` is the first true async effect and validates the suspend/resume machinery before file and process work

Acceptance:

1. current time and sleep behavior matches existing Bosatsu semantics
2. the repo stays green on the default backend, and libuv-specific tests pass

### PR 4: libuv stream-handle foundation for stdio writes
Scope:

1. replace `FILE*` stdout/stderr handling in the libuv backend with `uv_stream_t`
2. implement `write_utf8`
3. implement `write_bytes`
4. implement `flush` and `close` for stream handles

Do not do yet:

1. no readable stream buffering yet
2. no regular-file support yet

Acceptance:

1. Bosatsu stdout/stderr programs work on the libuv backend
2. flush semantics remain aligned with JVM/Python

### PR 5: libuv stream reads and buffering
Scope:

1. implement stdin and pipe reading on `uv_stream_t`
2. implement `read_bytes`
3. implement `read_utf8`
4. add per-handle buffering and backpressure thresholds

Do not do yet:

1. no process support yet
2. no spawned-pipe tests yet; those belong with `spawn` in PR 8

Acceptance:

1. stdin and generic `uv_stream_t` read semantics match Bosatsu expectations
2. UTF-8 chunking is correct across multibyte boundaries

### PR 6: libuv regular-file operations
Scope:

1. implement the `uv_file`-based handle branch
2. port `open_file`
3. port regular-file `read_bytes`, `read_utf8`, `write_bytes`, `write_utf8`
4. port `close` and `flush` for regular files
5. port temp file and temp dir creation

Acceptance:

1. existing file IO Bosatsu programs run on the libuv backend
2. append/read/write offset behavior is covered by tests

### PR 7: libuv path, stat, directory, and environment functions
Scope:

1. port `list_dir`
2. port `stat`
3. port `mkdir`
4. port `remove`
5. port `rename`
6. port `get_env`

Acceptance:

1. the non-process `Bosatsu/IO/Core` surface is complete on the libuv backend
2. directory and recursive remove semantics are covered by tests

### PR 8: libuv process support for the existing IO surface
Scope:

1. implement `spawn`
2. implement `wait`
3. implement stdio mapping for `Inherit`, `Null`, `Pipe`, and `UseHandle`
4. implement child stdio pipe mapping with `uv_process_t` and `uv_pipe_t`
5. normalize process exit vs signal termination to the Bosatsu `Int` contract
6. cache process exit status and repeated waits

Acceptance:

1. the existing `Bosatsu/IO/Core` API is now fully implemented on the libuv backend
2. no new Bosatsu concurrency functions have been exposed yet
3. `spawn`/`wait` tests cover `Pipe`, `UseHandle`, repeated `wait`, and signal/exit-code normalization
4. CI includes libuv backend coverage for representative existing IO programs

### PR 9: internal `MatchlessToValue` effectful runner, no public API change
Scope:

1. add `ProgRuntime[F]` plus `runProgF`/`runProgMainF`/`runProgTestF`
2. implement current non-process `Prog` semantics in the effectful Scala runner
3. port the Scala-side time primitives needed by current `Prog` execution
4. keep the synchronous `Predef` runner available in parallel
5. add representative parity tests for JVM `Main` and `ProgTest` execution on the new runner

Do not do yet:

1. no public Bosatsu concurrency exports yet
2. no switch of existing command paths by default yet
3. no `spawn`/`wait` parity requirement yet
4. no requirement yet that Scala process support be complete on all hosts

Acceptance:

1. the effectful JVM Scala runner can execute representative `Prog`, `Main`, `ProgTest`, `Var`, and `sleep` programs
2. no public Bosatsu surface area changes in this PR

### PR 10: `MatchlessToValue` current-surface parity and Scala.js runtime split
Scope:

1. port the remaining current `Bosatsu/IO/Core` operations exercised by the Scala runtime paths, at minimum stdin/stdout/stderr, file/env/time, and process support
2. implement JVM `spawn`/`wait` parity on top of the effectful runner, including `UseHandle` bridging and exit normalization
3. add the explicit Scala.js runtime split:
   1. `bosatsu_node` process backend over Node `child_process`
   2. browser Scala.js process backend that reports `Unsupported`
4. switch the Scala command paths that execute `Prog` to the effectful runner
5. add tests for JVM parity and the explicit Node/browser process split

Acceptance:

1. existing Scala command and test paths stay green on JVM with the effectful runner
2. `bosatsu_node` remains supported, with process behavior coming from the Node backend rather than hidden JVM assumptions, and browser Scala.js reports explicit `Unsupported` for `spawn`/`wait`
3. no public Bosatsu concurrency functions have been exposed yet

### PR 11: switch public `compute` on across all backends
Scope:

1. add `compute` to `Bosatsu/Prog`
2. implement it in:
   1. Scala `MatchlessToValue` + cats-effect on JVM
   2. Scala.js with the documented fairness-only semantics
   3. Python runtime
   4. C libuv backend with `uv_queue_work`
3. add docs and tests for `compute`

Why this is its own PR:

1. `compute` is useful immediately
2. it is much simpler than full child-fiber handles
3. it exercises the core "off the main scheduler" story without adding `JoinHandle`

Acceptance:

1. `compute` works on JVM Scala, Scala.js with the documented limitation, Python, and libuv C
2. the public API grows by exactly one function in this PR

### PR 12: add `JoinHandle`, `JoinResult`, `start`, and `join`
Scope:

1. add `JoinHandle` and `JoinResult` to `Bosatsu/Prog`
2. add `start`
3. add `join`
4. implement the feature in:
   1. Scala `MatchlessToValue` + cats-effect on JVM and Scala.js
   2. Python runtime
   3. C libuv backend
5. add tests for repeated joins, join after completion, join from multiple waiters, and child error propagation

Do not do yet:

1. no public `cancel` yet

Why this split is useful:

1. it keeps child-fiber semantics reviewable without mixing in cancellation
2. `start` + `join` already enable useful parallel composition

Acceptance:

1. child success and child error semantics match across all backends
2. repo remains green without having committed to public cancellation yet

### PR 13: add `cancel`
Scope:

1. add `cancel` to `Bosatsu/Prog`
2. implement request-specific cancel hooks in all runtimes
3. add tests for:
   1. cancel before completion
   2. cancel after completion
   3. cancel of `compute`
   4. cancel of `sleep`
   5. repeated cancel
   6. cancel of a blocked `join`
   7. cancel of a fiber blocked in `wait(process)` on backends that support processes

Why `cancel` is last:

1. it is the most semantics-heavy part
2. it forces us to define behavior for each pending effect
3. by this point the scheduler, IO backend, and join handles already exist

Acceptance:

1. `cancel` behavior is documented and tested across Scala, Python, and libuv C
2. canceling a Bosatsu wait on an external process does not kill that process on the backends that support process APIs
3. no previously shipped API needs to change shape

### PR 14: optional higher-level Bosatsu combinators
Only after the base concurrency surface is stable should we add Bosatsu-level helpers such as:

1. `poll`
2. `race`
3. `par_map2`
4. `yield_now` or `cede`

These should stay out of the critical path for the runtime migration.

### PR dependencies and parallel tracks
The numbering above is the recommended merge order, not a claim that every PR must be developed strictly one after another.
In the dependency bullets below, `PR A -> PR B` means `PR B` depends on `PR A`, so `PR A` must land first.

Dependencies:

1. `PR 1 -> PR 2`
2. `PR 2 -> PR 3`
3. `PR 2 -> PR 4`
4. `PR 4 -> PR 5`, because readable `uv_stream_t` support should build on the same stream-handle representation introduced for writes
5. `PR 2 -> PR 6`
6. `PR 2 -> PR 7`
7. `(PR 4, PR 5, PR 6) -> PR 8`, because `spawn`/`wait` need stream pipes plus `UseHandle` over already-existing Bosatsu handles, including file-backed handles
8. `PR 7` is not a narrow technical prerequisite for `spawn` itself, but it should land before or with `PR 8` so PR 8 can honestly be the "libuv reaches current `Bosatsu/IO/Core` parity" milestone
9. `PR 9 -> PR 10`
10. `(PR 8, PR 10) -> PR 11`
11. `PR 11 -> PR 12 -> PR 13 -> PR 14`

Parallelizable work:

1. after `PR 2`, the libuv C track can split into four mostly independent branches:
   1. `PR 3` for time primitives
   2. `PR 4 -> PR 5` for stream handles, reads, and buffering
   3. `PR 6` for regular files
   4. `PR 7` for path/stat/directory/env
2. `PR 8` should wait for the stream and file-handle work it depends on, then merge as the parity-closing process PR
3. the Scala internal migration can run in parallel with most of the libuv parity work:
   1. `PR 9 -> PR 10`
4. in practice, that means there are two major pre-public parallel tracks:
   1. C/libuv parity: `PR 1 -> PR 2 -> {PR 3, PR 4 -> PR 5, PR 6, PR 7} -> PR 8`
   2. Scala internal migration: `PR 9 -> PR 10`
5. the public concurrency surface should stay serial even if multiple people are available:
   1. `PR 11` then `PR 12` then `PR 13`
6. `PR 14` is intentionally optional and should wait until the base public surface has stabilized

### Why this PR ordering is the safest
This order keeps the repository working at every stage because:

1. the libuv backend is introduced before we depend on it for public API
2. the current C backend remains available while libuv reaches parity
3. the Scala runtime migration happens internally before any public concurrency surface depends on it
4. public API additions are small and sequential
5. `cancel` only lands after the simpler pieces are already proven

### Smallest sensible public slices
To answer the "can we add a few functions at a time?" question directly:

1. yes, and the right slices are still `compute` first, then `start`/`join`, then `cancel`
2. adding `start` without `join` is not useful enough
3. adding `cancel` before `start`/`join` would force semantics before the handle model exists
4. adding all four at once is unnecessary risk
5. but those public slices should only begin after libuv C parity and the internal Scala runner migration are already in place
