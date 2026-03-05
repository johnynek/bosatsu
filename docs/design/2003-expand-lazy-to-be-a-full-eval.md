---
issue: 2003
priority: 3
touch_paths:
  - docs/design/2003-expand-lazy-to-be-a-full-eval.md
  - test_workspace/Bosatsu/Lazy.bosatsu
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - test_workspace/Prog.bosatsu_externals
  - test_workspace/ProgExt.py
  - c_runtime/bosatsu_ext_Bosatsu_l_Lazy.h
  - c_runtime/bosatsu_ext_Bosatsu_l_Lazy.c
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - c_runtime/test.c
depends_on: []
estimated_size: M
generated_at: 2026-03-05T02:02:31Z
---

# Issue #2003 Design: expand `Lazy` to be a full `Eval`

_Issue: #2003 (https://github.com/johnynek/bosatsu/issues/2003)_

## Summary

Design to extend `Bosatsu/Lazy` with `const_Lazy`, `flat_map_Lazy`, `map_Lazy`, and `defer_Lazy`, and to implement stack-safe, memoizing `get_Lazy` evaluation across Scala evaluator, Python externals, and C runtime.

---
issue: 2003
title: expand Lazy to be a full Eval
status: proposed
base_branch: main
touch_paths:
  - docs/design/2003-expand-lazy-to-be-a-full-eval.md
  - test_workspace/Bosatsu/Lazy.bosatsu
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - test_workspace/Prog.bosatsu_externals
  - test_workspace/ProgExt.py
  - c_runtime/bosatsu_ext_Bosatsu_l_Lazy.h
  - c_runtime/bosatsu_ext_Bosatsu_l_Lazy.c
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - c_runtime/test.c
depends_on: []
estimated_size: M
generated_at: 2026-03-05
---

# Issue #2003 Design: expand `Lazy` to be a full `Eval`

Issue: #2003 (https://github.com/johnynek/bosatsu/issues/2003)
Base branch: `main`

## Summary

Extend `Bosatsu/Lazy` from a memoized thunk (`lazy` + `get_Lazy`) to a memoizing, stack-safe evaluation datatype with these public operations:

- `const_Lazy`
- `flat_map_Lazy`
- `map_Lazy`
- `defer_Lazy`

The key behavioral requirement is that composing `Lazy` values with these combinators and forcing once with `get_Lazy` uses O(1) call stack (stack-on-heap evaluation loop), while preserving best-effort memoization semantics across current runtimes.

## Problem statement

Current `Lazy` is intentionally minimal and supports only suspended thunk construction and forcing. This is enough for basic memoization but not enough to model `Eval`-style stack-safe sequencing in non-tail lazy loops.

Without `flat_map_Lazy` and an iterative forcing algorithm, deep lazy bind chains can require recursive forcing and risk stack overflow. The issue asks for an `Eval`-like `Lazy` surface and semantics with cached successful results.

## Goals

1. Add full `Lazy` API surface: `const_Lazy`, `flat_map_Lazy`, `map_Lazy`, `defer_Lazy` (while keeping existing `lazy` and `get_Lazy`).
2. Guarantee O(1) call stack for forcing deeply composed `Lazy` programs built from these combinators.
3. Preserve memoization semantics: successful force is cached, repeated `get_Lazy` returns cached value.
4. Keep current best-effort atomic behavior under races (not exact-once).
5. Keep behavior aligned across evaluator (`Predef`), Python externals, and C runtime externals.

## Non-goals

1. No exact-once concurrency guarantee across all runtimes.
2. No async/cancel/interruption semantics for `Lazy`.
3. No change to Bosatsu language syntax.
4. No replacement of `Prog` runtime model; this is specific to `Lazy`.

## Proposed API

In `Bosatsu/Lazy`:

- Keep existing:
- `external def lazy[a](fn: Unit -> a) -> Lazy[a]`
- `external def get_Lazy[a](l: Lazy[a]) -> a`

- Add primitives:
- `external def const_Lazy[a](value: a) -> Lazy[a]`
- `external def flat_map_Lazy[a, b](lazy: Lazy[a], fn: a -> Lazy[b]) -> Lazy[b]`

- Add derived combinators in Bosatsu (non-external):
- `def map_Lazy[a, b](lazy: Lazy[a], fn: a -> b) -> Lazy[b] = flat_map_Lazy(lazy, a -> const_Lazy(fn(a)))`
- `def defer_Lazy[a](fn: Unit -> Lazy[a]) -> Lazy[a] = flat_map_Lazy(const_Lazy(()), fn)`

Rationale:

- `const_Lazy` + `flat_map_Lazy` are the minimal runtime primitives.
- `map_Lazy` and `defer_Lazy` can be lawfully derived, keeping runtime FFI surface smaller.

## Semantic contract

1. `lazy(fn)` is non-strict and memoizing for successful results.
2. `const_Lazy(value)` is already evaluated and memoized.
3. `flat_map_Lazy(l, fn)` does not evaluate `l` or `fn` eagerly.
4. `map_Lazy` and `defer_Lazy` behave according to their derived definitions.
5. `get_Lazy` evaluates composed `Lazy` values with an iterative loop and heap-based continuation stack.
6. Successful evaluation is memoized.
7. Failures/exceptions are not memoized; later `get_Lazy` calls may retry.
8. Under races, duplicate evaluation is allowed; converged cached value remains best-effort atomic as today.

## Architecture

### Shared state model

Represent each `Lazy` node as one memo cell with an atomic state, conceptually:

- `Thunk(fn)` for `lazy`
- `Bind(source, fn)` for `flat_map_Lazy`
- `Done(value)` for memoized success

`const_Lazy` creates `Done(value)` directly.

### Stack-safe forcing algorithm

`get_Lazy` uses an explicit loop with heap-allocated continuation frames (not recursive host calls for bind traversal):

1. Start from target cell.
2. If state is `Done(v)`:
- If no continuation frame remains, finish with `v`.
- Else pop next continuation `fn`, compute next lazy via `fn(v)`, continue loop.
3. If state is `Thunk(fn)`, run `fn(())`; on success CAS state to `Done(value)`.
4. If state is `Bind(source, fn)`, push frame and continue with `source`.
5. On final success, opportunistically path-compress visited `Bind` nodes to `Done(result)` with CAS.

Properties:

- O(1) call stack with O(n) heap frames for n bind depth.
- All traversed nodes can be memoized after one successful run.
- For the C runtime, continuation frames are temporary evaluator scaffolding (not Bosatsu values), so they can use plain `malloc/free` rather than `GC_malloc` to avoid Boehm GC scanning overhead.
- GC safety rule for the C runtime: keep a strong reference to the root `Lazy` node until forcing completes, and avoid destructive updates that would sever reachability to frame-referenced `BValue`s before unwind/path-compression is finished.

## Implementation plan

1. Update `test_workspace/Bosatsu/Lazy.bosatsu`.
- Export new symbols.
- Add external declarations for `const_Lazy` and `flat_map_Lazy`.
- Implement `map_Lazy` and `defer_Lazy` as pure Bosatsu wrappers.
- Extend package tests for new API and deep bind stack-safety behavior.

2. Update evaluator externals in `core/src/main/scala/dev/bosatsu/Predef.scala`.
- Register `const_Lazy` and `flat_map_Lazy` in `Predef.evalExternals`.
- Replace current `LazyCell` state with multi-node state model (`Thunk`, `Bind`, `Done`).
- Add `const_Lazy` and `flat_map_Lazy` constructors.
- Rework `get_Lazy` to iterative forcing with explicit continuation stack and best-effort path compression.
- Preserve retry-on-failure semantics.

3. Update Python externals.
- `test_workspace/Prog.bosatsu_externals`: add mappings for `const_Lazy` and `flat_map_Lazy`.
- `test_workspace/ProgExt.py`: extend `_BosatsuLazy` representation with tagged states and iterative `get_Lazy` loop.

4. Update C runtime externals.
- `c_runtime/bosatsu_ext_Bosatsu_l_Lazy.h`: declare new extern symbols.
- `c_runtime/bosatsu_ext_Bosatsu_l_Lazy.c`: implement new constructors and iterative forcing engine with heap continuation frames.
- Keep atomic/CAS best-effort memoization behavior and existing symbol compatibility for `lazy`/`get_Lazy`.
- Implement continuation frame storage with `malloc/free` (not `GC_malloc`), and explicitly preserve root reachability invariants during forcing so non-GC frames never become the sole owner of live `BValue` references.

5. Add/extend tests.
- `core/src/test/scala/dev/bosatsu/EvaluationTest.scala`: API correctness, memoization, failure retry, and deep-chain stack-safety tests.
- `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala`: integration coverage for `lib` path using new `Lazy` API.
- `c_runtime/test.c`: targeted runtime tests for deep bind forcing and memoization behavior in C path.

## Testing strategy

1. API tests:
- `const_Lazy(x).get_Lazy == x`
- `map_Lazy` and `flat_map_Lazy` produce expected values.
- `defer_Lazy` is non-strict until `get_Lazy`.

2. Memoization tests:
- Forcing same value twice executes underlying thunk once in single-thread path.
- Force through a bind chain and then force an intermediate node; it should return cached value.

3. Stack-safety tests:
- Build large left-associated bind chains (for example 50k-200k steps) and force successfully without stack overflow.

4. Failure tests:
- Thunk/continuation throws first time and succeeds later; value is cached only after success.

5. Cross-runtime parity:
- Evaluate equivalent `Lazy` programs in evaluator, Python extern path, and C runtime-backed path.

## Acceptance criteria

1. `Bosatsu/Lazy` exports `Lazy`, `lazy`, `get_Lazy`, `const_Lazy`, `flat_map_Lazy`, `map_Lazy`, and `defer_Lazy`.
2. `map_Lazy` and `defer_Lazy` are implemented via `flat_map_Lazy` + `const_Lazy` and behave correctly.
3. `get_Lazy` forcing of deeply composed lazy chains is O(1) call stack in evaluator runtime.
4. Equivalent stack-safe forcing behavior exists in Python and C extern runtimes.
5. Successful values are memoized and reused on repeated `get_Lazy` calls.
6. Failures are not memoized; retry semantics remain intact.
7. Existing `lazy`/`get_Lazy` behavior remains backward compatible for existing callers.
8. New extern symbol mappings compile and run in `tool eval` and `lib` paths.
9. Added tests cover semantics, memoization, retry, and deep-chain stack safety.
10. Existing relevant test suites continue to pass.

## Risks and mitigations

1. Risk: Runtime semantic drift across Scala, Python, and C.
- Mitigation: Keep one shared semantic contract here and add parity tests with equivalent programs.

2. Risk: Subtle CAS/path-compression races produce inconsistent memoization.
- Mitigation: Use monotonic state transitions toward `Done`, CAS-only updates, and race-focused tests.

3. Risk: Performance regressions from extra allocations in deep chains.
- Mitigation: Keep node representation minimal, use iterative loops, and benchmark representative chain sizes.

4. Risk: Incorrect handling when continuation returns non-`Lazy` due host/runtime misuse.
- Mitigation: Validate external shape on force and fail fast with clear runtime error.

5. Risk: Large stack-safety tests increase CI runtime/flakiness.
- Mitigation: Use one large regression test plus smaller deterministic tests; tune depth for stable CI timing.

## Rollout notes

1. Land as one additive PR that updates library declaration and all runtimes together.
2. No migration required for existing `lazy`/`get_Lazy` usage.
3. Include release notes describing new `Lazy` combinators and stack-safe forcing guarantees.
4. If one runtime lags, do not partially release API surface without matching runtime support.

## Alternatives considered

1. Make all four new methods external.
- Rejected: larger FFI surface and duplicated semantics for `map_Lazy`/`defer_Lazy`.

2. Keep current `Lazy` representation and add recursive `flat_map` forcing.
- Rejected: does not satisfy O(1) stack requirement for deep chains.

3. Reuse `Prog` encoding for `Lazy`.
- Rejected: heavier abstraction than needed and unnecessary coupling to effect runtime behavior.

## Decision

Proceed with `const_Lazy` + `flat_map_Lazy` as runtime primitives, derive `map_Lazy` and `defer_Lazy` in Bosatsu, and reimplement `get_Lazy` as iterative stack-on-heap forcing with memoization path compression across evaluator, Python, and C runtimes.
