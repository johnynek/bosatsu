---
issue: 2084
priority: 3
touch_paths:
  - docs/design/2084-design-a-benchmarking-blackhole-function.md
  - test_workspace/Prog.bosatsu
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - c_runtime/bosatsu_ext_Bosatsu_l_Prog.h
  - c_runtime/bosatsu_ext_Bosatsu_l_Prog.c
  - test_workspace/Prog.bosatsu_externals
  - test_workspace/ProgExt.py
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenLibraryDepsTest.scala
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-03-09T03:30:25Z
---

# Design Doc: Issue #2084 Benchmarking Blackhole Function

_Issue: #2084 (https://github.com/johnynek/bosatsu/issues/2084)_

## Summary

Introduce an effectful Bosatsu/Prog observe API (a -> forall err. Prog[err, Unit]) with backend implementations on JVM evaluator, C runtime, and Python runtime to provide a reliable benchmark consume barrier, plus tests, docs, and rollout guidance.

---
issue: 2084
title: design a benchmarking blackhole function
status: proposed
base_branch: main
touch_paths:
  - docs/design/2084-design-a-benchmarking-blackhole-function.md
  - test_workspace/Prog.bosatsu
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - c_runtime/bosatsu_ext_Bosatsu_l_Prog.h
  - c_runtime/bosatsu_ext_Bosatsu_l_Prog.c
  - test_workspace/Prog.bosatsu_externals
  - test_workspace/ProgExt.py
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenLibraryDepsTest.scala
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-03-09
---

# Issue #2084 Design: benchmarking blackhole function

Issue: #2084 (https://github.com/johnynek/bosatsu/issues/2084)
Base branch: `main`

## Summary

Add an effectful benchmarking sink API to `Bosatsu/Prog`:

1. `external def observe[a](a: a) -> forall err. Prog[err, Unit]`

`observe` is implemented per runtime backend (JVM evaluator, C runtime, Python runtime) as a deliberate side-effecting consume operation. This makes benchmark consumption explicit and composes with the existing `Prog` execution boundary, so benchmark work is not dropped as unused pure computation.

## Problem statement

Benchmarks need a way to force computation to be observed so optimizer passes (Bosatsu normalization, C compiler optimization, JVM JIT) cannot remove work that appears to have no externally visible result.

A pure identity shape (`a -> a`) is not sufficient as the primary API:

1. It can still be removed when its result is not needed.
2. It does not force composition into the final executed value.
3. It encourages use in pure code paths where the benchmark sink effect is not guaranteed to execute.

The issue explicitly calls out this risk and suggests a `Prog`-returning shape.

## Goals

1. Provide a standard blackhole API for Bosatsu benchmarks.
2. Require explicit sequencing into executed `Prog` values.
3. Keep semantics consistent across JVM evaluator, C runtime, and Python runtime.
4. Keep change additive and backward-compatible.

## Non-goals

1. Do not add a pure `Bosatsu/Predef` identity-style `observe` in this issue.
2. Do not add new CLI benchmark flags or benchmark harness features.
3. Do not guarantee zero overhead; only provide a reliable consume barrier.
4. Do not redesign `Prog` core execution semantics beyond adding one external constructor helper.

## Proposed API

In `Bosatsu/Prog` (`test_workspace/Prog.bosatsu`):

1. Export `observe`.
2. Add `external def observe[a](a: a) -> forall err. Prog[err, Unit]`.

Intended usage pattern:

```bosatsu
from Bosatsu/Prog import observe, pure, await

def bench_step(n: Int) -> forall e. Prog[e, Unit]:
  observe(expensive_pure_fn(n))

main = Main(_ ->
  await(bench_step(100000), _ -> pure(0))
)
```

Contract:

1. The argument is evaluated once and consumed by runtime-specific sink logic.
2. The resulting `Prog` must be sequenced into executed program flow (`Main`, `ProgTest`, or composed `Prog`).
3. Return value is `Unit` to signal intentional discard semantics.

## Architecture

### 1. Surface API in `Bosatsu/Prog`

`observe` belongs in `Bosatsu/Prog` because it is effectful by construction. Keeping it out of `Bosatsu/Predef` avoids presenting a pure-looking API that can still be dead-code-eliminated when unused.

### 2. JVM evaluator implementation

In `core/src/main/scala/dev/bosatsu/Predef.scala`:

1. Register new external mapping for package `Bosatsu/Prog`, name `observe`, arity `1`.
2. Add `PredefImpl.prog_observe(a: Value): Value` that builds a `Prog` effect node (using existing `ProgTagEffect` machinery).
3. In effect execution callback:
   1. Write observed value into an internal volatile sink.
   2. Optionally clear sink back to unit sentinel to avoid long-lived retention.
   3. Return `prog_pure(UnitValue)`.

This guarantees a side-effecting step visible to JIT optimization barriers while preserving existing `Prog` interpreter behavior.

### 3. C runtime implementation

In `c_runtime/bosatsu_ext_Bosatsu_l_Prog.h/.c`:

1. Add symbol declaration/definition for `___bsts_g_Bosatsu_l_Prog_l_observe`.
2. Implement `observe` as an effect-node constructor (`Prog` tag 5) with a callback that:
   1. Stores the observed `BValue` into a volatile sink (or atomic sink with equivalent semantics).
   2. Returns `Pure(())`.

No changes are required to `run_prog` dispatch because tag-5 effect execution already exists.

### 4. Python runtime implementation

In `test_workspace/ProgExt.py` and `test_workspace/Prog.bosatsu_externals`:

1. Add `observe` mapping under `Bosatsu/Prog`.
2. Implement `observe(value)` as `effect` thunk that writes to a module-level sink and returns `pure(())`.

Even though CPython has no ahead-of-time dead-code elimination like C/JIT, keeping explicit consume semantics aligned across backends prevents behavioral drift.

### 5. No compiler optimization pass changes

No new optimizer pass or compiler intrinsic is required. Reliability comes from making observation an explicit `Prog` effect that must be in the executed chain.

## Detailed implementation plan

1. Extend `test_workspace/Prog.bosatsu` export list with `observe`.
2. Add `external def observe[a](a: a) -> forall err. Prog[err, Unit]` to `test_workspace/Prog.bosatsu`.
3. Add `observe` external mapping in `core/src/main/scala/dev/bosatsu/Predef.scala` for `Bosatsu/Prog`.
4. Add `PredefImpl.prog_observe` implementation using existing `prog_effect` runtime tag.
5. Add C header declaration in `c_runtime/bosatsu_ext_Bosatsu_l_Prog.h`.
6. Add C implementation in `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c` using volatile/atomic sink and `Pure(())` return.
7. Add Python runtime function in `test_workspace/ProgExt.py`.
8. Add Python external descriptor mapping in `test_workspace/Prog.bosatsu_externals`.
9. Add evaluator-level tests in `core/src/test/scala/dev/bosatsu/EvaluationTest.scala`.
10. Add command/integration tests in `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala`.
11. Add clang codegen/library deps assertion for `observe` symbol reachability in `core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenLibraryDepsTest.scala`.
12. Add short user-facing docs note in `docs/src/main/paradox/language_guide.md` under the `Prog` section.

## Testing strategy

### Evaluator/runtime behavior

1. `EvaluationTest`: `observe` returns `Prog` that evaluates to `Unit` and composes correctly through `await`/`flat_map`.
2. Ensure no top-level error/regression in existing `Prog` execution paths.

### Command-level coverage

1. `ToolAndLibCommandTest`: `tool eval --run` path accepts and executes a `Main` that sequences `observe`.
2. `ToolAndLibCommandTest`: `lib eval --run` (and/or `lib test` with `ProgTest`) can execute code using `observe`.

### C/codegen coverage

1. `ClangGenLibraryDepsTest`: generated C references `___bsts_g_Bosatsu_l_Prog_l_observe` when used.
2. Existing C runtime/prog execution tests remain green.

### Python coverage

1. Python external descriptor resolves `Bosatsu/Prog::observe` without missing-external errors.
2. Existing Python `Prog` runtime behavior remains unchanged for non-`observe` paths.

## Acceptance criteria

1. `Bosatsu/Prog` exports `observe` with signature `external def observe[a](a: a) -> forall err. Prog[err, Unit]`.
2. JVM evaluator externals include a working mapping for `Bosatsu/Prog::observe`.
3. C runtime exposes and implements `___bsts_g_Bosatsu_l_Prog_l_observe` and returns `Pure(())` when executed.
4. Python runtime mappings include `Bosatsu/Prog::observe` and execute successfully.
5. Programs that sequence `observe` in `Main`/`ProgTest` run successfully in existing command flows.
6. Existing `Prog` APIs (`pure`, `flat_map`, `recover`, `apply_fix`, etc.) keep existing behavior.
7. Tests are added/updated in evaluator, command, and clang codegen suites for the new API.
8. Documentation states that `observe` must be composed into executed `Prog` values to have effect.

## Risks and mitigations

1. Risk: users call `observe` but do not sequence it into the executed `Prog`.
   Mitigation: documentation and examples must show `await`/`flat_map` composition into `Main`/`ProgTest`.

2. Risk: benchmark distortion from consume overhead.
   Mitigation: document that `observe` has fixed overhead and should be placed consistently in benchmark loops.

3. Risk: backend behavior drift (JVM/C/Python consume semantics diverge).
   Mitigation: shared API contract plus backend-specific tests in the same PR.

4. Risk: sink retains large values and skews memory during long runs.
   Mitigation: clear sink after consume (or use bounded sentinel strategy) and document behavior.

5. Risk: C implementation relies on volatile behavior assumptions.
   Mitigation: prefer atomic store semantics where straightforward; otherwise keep implementation minimal and covered by runtime tests.

## Rollout notes

1. Land as a single additive PR: API surface + runtime implementations + tests.
2. Regenerate/release `core_alpha` artifacts after merge so downstream users get `Bosatsu/Prog::observe`.
3. Update docs in the same release cycle with a short benchmark usage example.
4. No migration needed for existing code; this is opt-in functionality.
5. Follow-up (optional): if needed, add helper combinators for ergonomic benchmark loops without changing core semantics.

## Decision

Implement `observe` as an effectful `Bosatsu/Prog` external returning `Prog[err, Unit]`, with backend-specific sink behavior. This is the most reliable design against dead-code elimination while fitting Bosatsu’s explicit effect model.
