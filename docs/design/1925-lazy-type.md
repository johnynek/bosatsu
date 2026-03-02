---
issue: 1925
priority: 3
touch_paths:
  - docs/design/1925-lazy-type.md
  - test_workspace/Bosatsu/Lazy.bosatsu
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - test_workspace/core_alpha_conf.json
  - test_workspace/Prog.bosatsu_externals
  - test_workspace/ProgExt.py
  - c_runtime/bosatsu_ext_Bosatsu_l_Lazy.c
  - c_runtime/bosatsu_ext_Bosatsu_l_Lazy.h
  - c_runtime/Makefile
depends_on: []
estimated_size: M
generated_at: 2026-03-02T16:32:56Z
---

# Issue #1925 design: Lazy type

_Issue: #1925 (https://github.com/johnynek/bosatsu/issues/1925)_

## Summary

Design doc content for adding `Bosatsu/Lazy` as an external memoized-thunk type with evaluator, Python externals, and C runtime implementation coverage.

---
issue: 1925
title: Lazy type
status: proposed
base_branch: main
touch_paths:
  - docs/design/1925-lazy-type.md
  - test_workspace/Bosatsu/Lazy.bosatsu
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - test_workspace/core_alpha_conf.json
  - test_workspace/Prog.bosatsu_externals
  - test_workspace/ProgExt.py
  - c_runtime/bosatsu_ext_Bosatsu_l_Lazy.c
  - c_runtime/bosatsu_ext_Bosatsu_l_Lazy.h
  - c_runtime/Makefile
depends_on: []
estimated_size: M
generated_at: 2026-03-02
---

# Issue #1925 Design: Lazy type

Issue: #1925 (https://github.com/johnynek/bosatsu/issues/1925)
Base branch: `main`

## Summary

Add `Bosatsu/Lazy` with an external opaque type `Lazy[a: +*]` and two functions:

- `lazy[a](fn: Unit -> a) -> Lazy[a]` (issue text uses `() -> a`; this is `Unit -> a` in Bosatsu syntax)
- `get_Lazy[a](l: Lazy[a]) -> a`

`Lazy` stores a thunk and memoizes the first computed value. The API is intentionally small and low-level, matching the issue statement that usage should be rare.

## Problem statement

Bosatsu currently has no standard memoized-thunk type. Users can pass `Unit -> a` functions around, but repeated calls recompute every time. The issue asks for a single reusable external abstraction that caches the result and exposes forcing via `get_Lazy`.

## Goals

1. Add a standard package `Bosatsu/Lazy` with the requested API and variance (`Lazy[a: +*]`).
2. Make `lazy` non-strict (do not evaluate at construction).
3. Make repeated `get_Lazy` calls reuse the computed value after the first successful evaluation.
4. Support evaluator/runtime paths used by `tool eval` and `lib eval`.
5. Keep behavior consistent with current external-runtime architecture for Python and C transpilation paths.

## Non-goals

1. No additional API surface beyond `lazy` and `get_Lazy` in this issue.
2. No Promise/Future/concurrency primitives.
3. No `ValueToJson`/`ValueToDoc` custom serialization for raw `Lazy` values.
4. No strict global guarantee of exactly-once under all races across every backend (best-effort memoization is acceptable per issue statement).

## Proposed API

`test_workspace/Bosatsu/Lazy.bosatsu`:

```bosatsu
package Bosatsu/Lazy

export (
  Lazy,
  lazy,
  get_Lazy,
)

external struct Lazy[a: +*]

external def lazy[a](fn: Unit -> a) -> Lazy[a]
external def get_Lazy[a](l: Lazy[a]) -> a
```

## Semantics

1. `lazy(fn)` stores `fn` without running it.
2. `get_Lazy(l)` evaluates `fn(())` on first force and returns the result.
3. After successful force, later `get_Lazy(l)` calls return the memoized value.
4. If force throws from host runtime code, no successful value is memoized; later calls may retry.
5. Under concurrent races, implementations may evaluate more than once but must converge on a stored result per `Lazy` instance once one succeeds.

## Architecture and implementation plan

### 1) Bosatsu package and library export

- Add `test_workspace/Bosatsu/Lazy.bosatsu` with the external declarations above.
- Add `Bosatsu/Lazy` to `test_workspace/core_alpha_conf.json` exported packages.

### 2) Evaluator runtime (`core/src/main/scala/dev/bosatsu/Predef.scala`)

- Add package constant for `Bosatsu/Lazy`.
- Register two externals in `Predef.evalExternals`:
  - `lazy` -> `FfiCall.Fn1(...)`
  - `get_Lazy` -> `FfiCall.Fn1(...)`
- In `PredefImpl`, add a runtime cell representation, for example:
  - `LazyCell(thunk: Value, cached: AtomicReference[Value | Null])`
- Implement:
  - `lazy_Lazy(fn: Value): Value` -> wraps cell in `ExternalValue`
  - `get_Lazy(cellValue: Value): Value` -> fast-path cached value, else evaluate thunk with `UnitValue`, then CAS-store winner
- Keep this implementation in shared `core` code so JVM and Scala.js evaluator behavior stays aligned.

### 3) Python transpile runtime

- Add `Bosatsu/Lazy` mapping in `test_workspace/Prog.bosatsu_externals`.
- Add runtime support in `test_workspace/ProgExt.py`:
  - `_BosatsuLazy` cell with thunk + cached sentinel
  - `lazy(fn)` constructor
  - `get_Lazy(l)` force-and-cache path

### 4) C runtime support

- Add `c_runtime/bosatsu_ext_Bosatsu_l_Lazy.h` declarations.
- Add `c_runtime/bosatsu_ext_Bosatsu_l_Lazy.c` implementation with external struct cell:
  - stored thunk `BValue`
  - `_Atomic BValue` cached value sentinel (`BSTS_BVALUE_NULL` when empty)
  - `___bsts_g_Bosatsu_l_Lazy_l_lazy` and `___bsts_g_Bosatsu_l_Lazy_l_get__Lazy` functions
- Update `c_runtime/Makefile` to compile/install the new C runtime external file and header.

### 5) Tests

- Add evaluator-level test coverage in `core/src/test/scala/dev/bosatsu/EvaluationTest.scala`:
  - API compiles in a package fixture
  - runtime memoization behavior in single-threaded path (counter-based host closure test around `PredefImpl` externals)
- Add lightweight package tests in `test_workspace/Bosatsu/Lazy.bosatsu` for basic forcing/type behavior.

## Acceptance criteria

1. `Bosatsu/Lazy` exists and exports `Lazy`, `lazy`, and `get_Lazy` with the exact requested type shapes.
2. `lazy` is non-strict (construction does not force the thunk).
3. Single-thread runtime behavior memoizes successfully computed values (`get_Lazy` does not recompute in the normal path).
4. `tool eval` and `lib eval` can evaluate programs importing `Bosatsu/Lazy` when forcing to non-`Lazy` output values.
5. Python externals mapping includes `Bosatsu/Lazy` and has working implementations for both functions.
6. C runtime exports lazy symbols and `c_runtime` build/install succeeds with the new files.
7. `core_alpha` configuration exports `Bosatsu/Lazy` for library distribution.
8. Existing tests and CI paths continue to pass.

## Risks and mitigations

1. Risk: Backend semantic drift (JVM/JS/Python/C memoization differences).
   Mitigation: Keep one canonical semantic contract in this doc and add focused evaluator tests plus C/Python smoke validation.

2. Risk: Concurrency edge cases causing duplicate evaluations.
   Mitigation: Treat duplicate evaluation under races as acceptable, but ensure memoized steady-state after first stored success.

3. Risk: Re-entrant self-force (`get_Lazy` called recursively on same cell during force) can diverge.
   Mitigation: Document as unsupported behavior for v1; avoid adding blocking semantics that can deadlock across runtimes.

4. Risk: Name collision concerns around identifier `lazy`.
   Mitigation: Validate parser acceptance during implementation; fallback to backticked declaration only if required by parser constraints.

## Rollout notes

1. Ship as a single additive PR; no breaking API changes.
2. No migration needed for existing users.
3. After merge, include `Bosatsu/Lazy` in next `core_alpha` release artifact.
4. If any backend lags, do not partially expose broken runtime paths; keep package/runtime wiring aligned before release.
