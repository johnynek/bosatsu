---
issue: 2182
priority: 3
touch_paths:
  - docs/design/2182-add-prog-var.md
  - test_workspace/Prog.bosatsu
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - test_workspace/Prog.bosatsu_externals
  - test_workspace/ProgExt.py
  - c_runtime/bosatsu_ext_Bosatsu_l_Prog.h
  - c_runtime/bosatsu_ext_Bosatsu_l_Prog.c
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenLibraryDepsTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-03-17T20:38:07Z
---

# Add Prog::Var

_Issue: #2182 (https://github.com/johnynek/bosatsu/issues/2182)_

## Summary

Add an invariant `Bosatsu/Prog::Var` as an effectful mutable cell built on the existing `Prog` runtime machinery, with CAS-style `update` on JVM/C, a locked Python backend, three derived helper functions, recursive-variance safety, and cross-backend test coverage.

## Context
This design covers the `Bosatsu/Prog::Var` API requested in [issue #2182](https://github.com/johnynek/bosatsu/issues/2182).

`Bosatsu/Prog` already hosts the effectful standard-library surface and already has backend-specific implementations in `test_workspace/Prog.bosatsu`, `core/src/main/scala/dev/bosatsu/Predef.scala`, `test_workspace/ProgExt.py`, and `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c`. `Var[a]` fits this package because allocation and mutation must only happen when a `Prog` is executed.

The current runtime architecture already has a generic `Prog` effect node. `Var` should reuse that mechanism rather than add a new `Prog` tag or any new compiler IR. The issue text mentions `MatchlessToValue`, but on the current `main` branch the public `Bosatsu/Prog` externals are implemented in `PredefImpl` inside `Predef.scala`; `MatchlessToValue` mutability remains compiler-internal.

Existing recursive-variance checking already rejects recursive occurrences through invariant type constructors. The design should rely on that safety property by making `Var` invariant.

## Goals
1. Add an invariant `Bosatsu/Prog::Var` type and the requested external operations.
2. Keep allocation, reads, and writes effectful and sequenced through executed `Prog` values.
3. Keep observable semantics aligned across JVM evaluator, Python externals, and C runtime.
4. Add a small set of non-external helper functions for common update patterns.
5. Preserve existing recursion and variance safety rules.

## Non-goals
1. No new package such as `Bosatsu/Ref`; this remains part of `Bosatsu/Prog`.
2. No transactional memory, blocking synchronization, or compare-and-set API in v1.
3. No compiler optimizer or Matchless IR changes.
4. No special recursion-check exemptions analogous to `Bosatsu/Lazy.get_Lazy` or `Bosatsu/Eval.eval`.
5. No custom pretty-printing or JSON encoding for raw `Var` values.

## Proposed API
Add to `Bosatsu/Prog`:
- `external struct Var[a]`
- `external def new_var[a](a: a) -> forall e. Prog[e, Var[a]]`
- `external def update[a, b](v: Var[a], fn: a -> (a, b)) -> forall e. Prog[e, b]`
- `external def set[a](v: Var[a], value: a) -> forall e. Prog[e, Unit]`
- `external def get[a](v: Var[a]) -> forall e. Prog[e, a]`
- `external def swap[a](v: Var[a], new_value: a) -> forall e. Prog[e, a]`

`Var[a]` should stay invariant by leaving the type parameter unannotated. That keeps recursive occurrences through `Var` illegal under existing variance rules.

Add the following non-external helpers and stop there for v1:
- `modify[a](v: Var[a], fn: a -> a) -> forall e. Prog[e, Unit]`
- `get_and_update[a](v: Var[a], fn: a -> a) -> forall e. Prog[e, a]`
- `update_and_get[a](v: Var[a], fn: a -> a) -> forall e. Prog[e, a]`

Rationale:
- `modify` is the common `Unit`-returning case and removes tuple boilerplate.
- `get_and_update` and `update_and_get` cover the two common projections of `update`.
- Do not add aliases for `swap` or `set` in v1. `swap` already serves as get-and-set, and `set` already covers the write-only case, so extra wrappers add surface area without new capability.

## Semantics
1. `new_var` allocates a fresh cell when the `Prog` is executed, not when the Bosatsu expression is constructed.
2. `get` returns the current value in the cell.
3. `set` writes the new value and returns `Unit`.
4. `swap` writes the new value and returns the previous value.
5. `update` atomically transforms the current value using `fn`, stores the first element of the returned pair, and returns the second element.
6. On backends that use CAS-style retry loops, `update` may invoke `fn` more than once under contention. This is acceptable because `fn` is pure.
7. `Var` identity stays opaque. There is no supported way to inspect or compare cells directly from Bosatsu code.

## Architecture And Implementation Plan

### 1. Surface package changes
Update `test_workspace/Prog.bosatsu` to:
- export `Var`, `new_var`, `update`, `set`, `get`, `swap`, `modify`, `get_and_update`, and `update_and_get`
- declare `external struct Var[a]`
- add the five external defs
- define the three helper functions in ordinary Bosatsu code in terms of `update`

No `core_alpha_conf.json` change is needed because `Bosatsu/Prog` is already exported.

### 2. JVM evaluator implementation
In `core/src/main/scala/dev/bosatsu/Predef.scala`:
- add `FfiCall` registrations for the five new `Bosatsu/Prog` externals
- store the mutable cell directly as `ExternalValue(new AtomicReference[Value](initial))`
- add an `asVar` extractor that unwraps `AtomicReference[Value]`
- implement `prog_new_var`, `prog_var_get`, `prog_var_set`, `prog_var_swap`, and `prog_var_update`

Implementation details:
- represent every operation as a `Prog` effect via existing `prog_effect` or `prog_effect2`
- do not add a new `Prog` tag
- `new_var` effect allocates `ExternalValue(new AtomicReference[Value](initial))` and returns `prog_pure(cell)`
- `get` loads the current value and returns `prog_pure(value)`
- `set` performs `state.set(value)` and returns `prog_pure(UnitValue)`
- `swap` performs `state.getAndSet(value)` and returns the old value
- `update` uses a CAS loop:
  1. read current value
  2. call `fn(current)`
  3. require a 2-field product
  4. attempt `compareAndSet(current, next)`
  5. on success return `prog_pure(result)`
  6. on failure retry from step 1

This keeps all mutation inside the existing effect interpreter and matches the issue's `AtomicReference` expectation, but in the actual public-runtime implementation location used today. A separate `VarCell` wrapper is not needed unless a later implementation wants to hang extra metadata off the cell.

### 3. Python runtime implementation
In `test_workspace/ProgExt.py` and `test_workspace/Prog.bosatsu_externals`:
- add external mappings for `new_var`, `update`, `set`, `get`, and `swap`
- add a `_BosatsuVar` runtime cell
- return `effect(...)` closures for each operation so allocation and mutation occur only at `Prog` execution time

Recommended Python representation:
- `_BosatsuVar` stores `_value` and a `threading.Lock`
- `get`, `set`, and `swap` run inside the lock
- `update` runs the callback and state transition inside the lock for simple atomic semantics in the Python backend

This is slightly stronger than the CAS backends because it can call `fn` exactly once, but it is observationally compatible with the weaker issue contract.

### 4. C runtime implementation
In `c_runtime/bosatsu_ext_Bosatsu_l_Prog.h` and `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c`:
- add exported symbols for the five new externals
- add a GC-allocated cell representation, for example `typedef struct { _Atomic BValue value; } BSTS_Prog_Var;`
- add small local `bsts_prog_effect1` and `bsts_prog_effect2` helpers if that makes the file cleaner

Implementation details:
- `new_var` allocates a `BSTS_Prog_Var` with `GC_malloc`, initializes `_Atomic BValue value`, and returns `Pure(cell)` from the effect callback
- `get` uses `atomic_load_explicit(..., memory_order_acquire)`
- `set` uses `atomic_store_explicit(..., memory_order_release)` and returns `Unit`
- `swap` uses `atomic_exchange_explicit(..., memory_order_acq_rel)` so the exchange synchronizes with both preceding and following accesses
- `update` uses a compare-exchange loop:
  1. load current
  2. call `fn(current)`
  3. read tuple fields with `get_struct_index`
  4. attempt `atomic_compare_exchange_weak_explicit` with `memory_order_acq_rel` on success and `memory_order_acquire` on failure
  5. retry on failure

No `c_runtime/Makefile` change should be required because `bosatsu_ext_Bosatsu_l_Prog.c` is already built and installed.

### 5. Type-safety and recursion constraints
No compiler algorithm change is required for recursive-type safety:
- `Var[a]` is invariant
- Bosatsu's existing recursive-variance checker already rejects recursive occurrences through invariant type constructors
- the PR should add a regression test that a definition such as `enum Bad: Step(next: Var[Bad])` fails with the normal `recursive occurrences must be covariant` error

Also, do not add any trusted recursion-check rule for `Var`. `Lazy` and `Eval` are special because they unwrap structurally smaller children; mutable cells do not have that property.

## Testing Strategy
1. `EvaluationTest.scala`
   - Bosatsu-level fixture exercising `new_var`, `get`, `set`, `swap`, `update`, and the helper functions
   - direct runtime tests for sequential semantics in `PredefImpl`, especially `set` returning `Unit` and `swap` returning the old value
2. `ToolAndLibCommandTest.scala`
   - `tool eval --run` program that allocates a `Var`, updates it, reads it back, and exits with the final value
   - corresponding `lib eval --run` coverage if that is the existing pattern for new `Bosatsu/Prog` APIs
3. `ClangGenLibraryDepsTest.scala`
   - reference the new externals from a small `ProgTest` and assert the generated C includes the expected `Bosatsu/Prog` symbols
4. `ErrorMessageTest.scala`
   - negative regression ensuring recursive type definitions through `Var` are rejected
5. `language_guide.md`
   - add a short `Bosatsu/Prog` note describing `Var` semantics and the meaning of `set` versus `swap`

## Acceptance Criteria
1. `Bosatsu/Prog` exports `Var`, `new_var`, `update`, `set`, `get`, `swap`, `modify`, `get_and_update`, and `update_and_get`.
2. `Var[a]` is invariant, and recursive type definitions through `Var` are rejected by existing variance checks.
3. `new_var` allocates only when the `Prog` is executed.
4. `get` returns the current value.
5. `set` returns `Unit`.
6. `swap` returns the previously stored value.
7. `update` stores the first element of the callback result and returns the second.
8. JVM evaluator implementation uses `AtomicReference[Value]` in `PredefImpl`.
9. Python externals and C runtime both implement the new API and compile/run through existing flows.
10. No new `Prog` runtime tag or compiler IR node is introduced.
11. Tests cover Bosatsu-level behavior, evaluator/runtime behavior, C symbol reachability, and recursive-variance rejection.
12. Public docs explain the API, especially the `set` versus `swap` return-value difference.

## Risks And Mitigations
1. Risk: implementing `new_var` as an immediate `prog_pure` allocation would allocate too early and break effect sequencing.
   Mitigation: require every `Var` primitive to be backed by an effect callback, even `new_var`.

2. Risk: backend semantic drift, especially between JVM/C CAS loops and Python locking.
   Mitigation: document one observable contract and test against that contract rather than backend-specific internals.

3. Risk: `update` callback reruns under contention and surprises callers.
   Mitigation: document this explicitly and keep `fn` pure by type; derived helpers should all delegate to the same contract.

4. Risk: a future refactor accidentally makes `Var` covariant or whitelists it in recursion checking.
   Mitigation: add negative tests for recursive variance and avoid any `TypedExprRecursionCheck` special casing.

5. Risk: C atomic handling of `BValue` is implemented with the wrong memory ordering or tuple decoding.
   Mitigation: follow the existing `Lazy`/IO runtime style, keep the cell representation minimal, and add targeted runtime coverage.

## Rollout Notes
1. Land this as one additive PR: package surface, three runtime implementations, tests, and docs together.
2. No migration is required for existing users.
3. Because `Bosatsu/Prog` is already part of the shipped library surface, rollout is mainly a new `core_alpha` release containing the updated package interface and runtime archive.
4. If one backend lags, do not release a partially wired API. The `Bosatsu/Prog` surface should only ship once JVM evaluator, Python externals, and C runtime all agree on the contract.
