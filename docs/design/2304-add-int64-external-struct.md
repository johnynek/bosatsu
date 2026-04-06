---
issue: 2304
priority: 3
touch_paths:
  - docs/design/2304-add-int64-external-struct.md
  - test_workspace/Int64.bosatsu
  - test_workspace/PredefTests.bosatsu
  - test_workspace/core_alpha_conf.json
  - test_workspace/Prog.bosatsu_externals
  - test_workspace/ProgExt.py
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - core/src/test/scala/dev/bosatsu/PredefWorkspaceTest.scala
  - core/src/test/scala/dev/bosatsu/Int64Laws.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - c_runtime/bosatsu_runtime.h
  - c_runtime/bosatsu_runtime.c
  - c_runtime/bosatsu_ext_Bosatsu_l_Num_l_Int64.h
  - c_runtime/bosatsu_ext_Bosatsu_l_Num_l_Int64.c
  - c_runtime/Makefile
  - c_runtime/test.c
depends_on: []
estimated_size: M
generated_at: 2026-04-06T06:04:41Z
---

# Design: add Bosatsu/Num/Int64 external struct

_Issue: #2304 (https://github.com/johnynek/bosatsu/issues/2304)_

## Summary

Add a runtime-backed `Bosatsu/Num/Int64` package with explicit safe and truncating conversions from `Int`, total fixed-width arithmetic defined by signed 64-bit two's-complement low-bit semantics, evaluator/Python support, and direct raw-`BValue` storage in the C runtime.

## Context

Bosatsu already has two useful precedents for this issue: arbitrary-precision `Int` as the semantic integer type, and `Float64` as a raw 64-bit value that can live directly inside a `BValue` in the C runtime. What is missing is an opaque fixed-width signed integer surface for cases where the program already knows values fit in 64 bits and wants to avoid repeated big-int allocation and conversion costs. Issue #2304 calls out array indices, offsets, and the spectral norm benchmark as the motivating cases.

This change should land as a normal runtime-backed library package, not as a new predef primitive. That keeps the parser, literal rules, and typechecker stable while still letting the C backend represent every `Int64` value in a single machine word. It also keeps follow-on work for array and string specializations separate from the base type addition.

## Goals

- Add an exported `Bosatsu/Num/Int64` package with an opaque `external struct Int64` and the minimum arithmetic, comparison, and conversion surface needed to use it from Bosatsu code.
- Make `Int -> Int64` semantics explicit by providing both a checked conversion and a truncating conversion.
- Keep evaluator, Python, and C behavior aligned so `tool eval`, library tests, and transpiled runtimes agree on results.
- Store `Int64` directly in `BValue` in the C runtime, with no heap allocation and no `alloc_external` wrapper.
- Leave room for a follow-up PR to specialize `Array` and `String` APIs once the base `Int64` type exists.

## Non-goals

- No new `Int64` literal syntax in this issue.
- No change to `Bosatsu/Predef::Int` or the compiler's built-in numeric types.
- No array-, string-, or bytes-specific `Int64` APIs in this PR.
- No `UInt64`, bitwise, shift, or modulo surface in v1 unless a concrete implementation need appears while landing the base package.
- No `json` or `doc` encoding support for raw `Int64` values; callers can convert through `Int` first when they need those surfaces.

## Package Placement

The package should be `Bosatsu/Num/Int64`, not `Bosatsu/Int64`. The main reason is symmetry with `Bosatsu/Num/Float64`, plus a likely future family of fixed-width numeric packages. Keeping these types grouped under `Bosatsu/Num` makes the public surface easier to scan and avoids creating two naming conventions for specialized numerics.

## Proposed Surface

The initial exported surface should be:

- `Int64`
- `min_i64: Int64`
- `max_i64: Int64`
- `int_to_Int64(i: Int) -> Option[Int64]`
- `int_low_bits_to_Int64(i: Int) -> Int64`
- `int64_to_Int(i: Int64) -> Int`
- `int64_to_Float64(i: Int64) -> Float64`
- `float64_to_Int64(f: Float64) -> Option[Int64]`
- `add_Int64(a: Int64, b: Int64) -> Int64`
- `sub_Int64(a: Int64, b: Int64) -> Int64`
- `mul_Int64(a: Int64, b: Int64) -> Int64`
- `div_Int64(a: Int64, b: Int64) -> Int64`
- `cmp_Int64(a: Int64, b: Int64) -> Comparison`
- `eq_Int64(a: Int64, b: Int64) -> Bool` as a Bosatsu wrapper over `cmp_Int64`
- `operator +`, `operator -`, `operator *`, and `operator /` as Bosatsu wrappers over the arithmetic functions

That keeps the external surface small, makes the checked versus truncating boundary explicit, and avoids forcing a decision on follow-on operations that the issue only mentions as possible extensions.

## Semantic Model

`Int64` should denote exactly the signed two's-complement 64-bit range `[-2^63, 2^63 - 1]`. The key semantic choice is that once a value is inside `Int64`, arithmetic is total and fixed-width, not `Option`-producing. The canonical model is: do the corresponding Bosatsu `Int` operation exactly, then keep the low 64 bits and sign-extend back to the signed 64-bit range.

Important invariants after the change:

- `int_to_Int64(i)` returns `Some(x)` if and only if `i` is in the signed 64-bit range, and on success `int64_to_Int(x) == i`.
- `int_low_bits_to_Int64(i)` is the low-64-bit two's-complement truncation of `i`. Equivalently, `int64_to_Int(int_low_bits_to_Int64(i))` is `i` reduced modulo `2^64` and then interpreted as a signed 64-bit integer.
- `int_low_bits_to_Int64(int64_to_Int(x)) == x` for every `Int64` value `x`.
- `add_Int64`, `sub_Int64`, and `mul_Int64` agree with exact `Int` arithmetic followed by `int_low_bits_to_Int64`.
- `div_Int64` agrees with exact Bosatsu `Int` division semantics followed by `int_low_bits_to_Int64`. This means it keeps Bosatsu's floor-division behavior for negative operands, returns `0` when dividing by `0`, and deterministically wraps the one overflow case `(-2^63) / -1` back into the signed 64-bit range.
- `cmp_Int64(a, b)` is the signed total order on the decoded 64-bit values, and `eq_Int64(a, b)` is exactly `cmp_Int64(a, b) matches EQ`.
- `float64_to_Int64` uses the same rounding rule as the current `Float64.float64_to_Int`: finite values round to nearest with ties to even, then range-check into signed 64-bit. Non-finite values return `None`.
- `int64_to_Float64` is semantically the same as `int_to_Float64(int64_to_Int(x))`.

The most important behavioral property is not any single example; it is that every backend computes the same mathematical model for truncation, signed comparison, and division. The user should never get C-style trunc-toward-zero on one backend and Bosatsu floor division on another.

## Runtime Architecture

### Frontend and library surface

This issue does not need parser, typer, or literal support changes. `Int64` is a normal opaque `external struct` in a library package. The new Bosatsu source lives alongside `Float64`, exports the functions above, and adds small pure wrappers for `eq_Int64` and the arithmetic operators. That keeps the compiler unaware of `Int64` while still making the type first-class in user code.

### Evaluator

`core/src/main/scala/dev/bosatsu/Predef.scala` should add a new package constant for `Bosatsu/Num/Int64`, register its externals, and represent evaluator values as a dedicated wrapper such as `Int64Value(value: Long)`. A dedicated wrapper is better than using raw `Long` because it preserves opacity and makes accidental mixing with Bosatsu `Int` values harder.

Checked `Int -> Int64` conversion should use `BigInteger` range checks against `Long.MinValue` and `Long.MaxValue`. Truncating conversion can use `BigInteger.longValue`, which already gives the low 64 bits. `add`, `sub`, and `mul` can use `Long` arithmetic directly because JVM and Scala.js `Long` semantics already wrap in two's complement. `div_Int64` should go through a small shared helper that matches Bosatsu's floor-division semantics rather than Java's trunc-toward-zero behavior.

### Python runtime

`test_workspace/Prog.bosatsu_externals` and `test_workspace/ProgExt.py` should add `Bosatsu/Num/Int64` mappings. Python should also use an opaque wrapper such as `_BosatsuInt64` instead of plain `int`, again to preserve type opacity and avoid accidental cross-use with Bosatsu `Int`.

Python implementation is straightforward because Python integers are arbitrary precision and `//` already uses floor division. The wrapper only needs two helpers: normalize to signed 64-bit and decode back to Python `int`. Safe conversion is a range check; truncating conversion is normalize-low-64-bits; arithmetic is exact Python arithmetic followed by normalization.

### C runtime

The C backend is the main architectural change. `Int64` should use the same style as `Float64`: store the raw 64-bit payload directly in `BValue`, with helper functions in `c_runtime/bosatsu_runtime.h` and `c_runtime/bosatsu_runtime.c` for packing and unpacking. `c_runtime/bosatsu_ext_Bosatsu_l_Num_l_Int64.c` then implements the package-level operations in terms of those helpers.

Important C invariants:

- `Int64` values are never heap allocated.
- `Int64` values must never go through `alloc_external` or `get_external`.
- Generic runtime code may store, load, and copy an `Int64` `BValue`, but it must not inspect it through pointer-tag assumptions. This is the same typed-opaqueness constraint Bosatsu already relies on for raw `Float64` words.
- Arithmetic must avoid signed-overflow undefined behavior. `add`, `sub`, and `mul` should use `uint64_t` modular arithmetic and only reinterpret as signed at the end.
- `div_Int64` must special-case `rhs == 0` and the `INT64_MIN / -1` overflow case, and must apply floor-correction for mixed-sign divisions with a non-zero remainder so C stays aligned with Bosatsu semantics.

Safe and truncating `Int` conversions can reuse the existing big-int helpers already present in the runtime. Safe conversion compares against signed 64-bit bounds. Truncating conversion uses `bsts_integer_to_low_uint64` and sign-extension. `float64_to_Int64` should reuse the same ties-to-even rounding rule already used by the Float64 package before doing the signed 64-bit range check.

## Implementation Plan

1. Add `test_workspace/Int64.bosatsu` with the opaque type, exported functions, and pure wrappers.
2. Export `Bosatsu/Num/Int64` from `test_workspace/core_alpha_conf.json`.
3. Register evaluator externals in `core/src/main/scala/dev/bosatsu/Predef.scala` and add the opaque evaluator wrapper plus shared semantic helpers.
4. Add Python external descriptors and wrapper/runtime helpers in `test_workspace/Prog.bosatsu_externals` and `test_workspace/ProgExt.py`.
5. Add shared C pack/unpack helpers in `c_runtime/bosatsu_runtime.h` and `c_runtime/bosatsu_runtime.c`, then add `c_runtime/bosatsu_ext_Bosatsu_l_Num_l_Int64.c` and `.h` plus `c_runtime/Makefile` wiring.
6. Add language-level smoke coverage in `test_workspace/PredefTests.bosatsu` and load the new package in `core/src/test/scala/dev/bosatsu/PredefWorkspaceTest.scala`.
7. Add property-check coverage in a new ScalaCheck suite and backend-specific edge-case coverage in `c_runtime/test.c`.
8. Update packaging/export regression coverage in `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala`.

## Testing Strategy

Property-check style tests should carry the semantic contract, because the core risk here is backend drift rather than one or two obvious examples. A dedicated ScalaCheck suite such as `core/src/test/scala/dev/bosatsu/Int64Laws.scala` should cover:

- safe conversion success and failure against random `BigInteger` inputs around and far beyond the signed 64-bit range
- truncating conversion idempotence
- arithmetic laws of the form `op_Int64(a, b) == int_low_bits_to_Int64(int64_to_Int(a) op int64_to_Int(b))`
- signed comparison agreement with decoded `Int` values
- division agreement with the exact Bosatsu `Int` model, including negative operands
- float conversion agreement with the existing ties-to-even `Float64 -> Int` semantic model plus signed 64-bit range checks

Narrower case-based tests are still the right fit in places where exact boundary values matter more than broad sampling or where failures need precise diagnostics. Those should live in:

- `c_runtime/test.c` for exact BValue representation checks, `INT64_MIN`, `INT64_MAX`, `INT64_MIN / -1`, division by zero, and exact float boundary cases such as `NaN`, infinities, `2^63 - 0.5`, and `2^63`
- `test_workspace/PredefTests.bosatsu` for user-facing API smoke tests and operator wrappers
- `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala` for `core_alpha` export and packaging regressions

The split is intentional: property tests prove the algebraic model, while case tests lock down the handful of machine-level edge cases most likely to regress in C.

## Acceptance Criteria

- `Bosatsu/Num/Int64` exists, is exported from `core_alpha`, and exposes the checked and truncating `Int` conversions, total arithmetic, signed comparison, and Float64 interop described above.
- `int_to_Int64` succeeds exactly on signed 64-bit inputs and fails exactly outside that range.
- `int_low_bits_to_Int64` implements deterministic low-64-bit two's-complement truncation on every backend.
- `add_Int64`, `sub_Int64`, `mul_Int64`, and `div_Int64` all match the documented exact-`Int`-then-truncate semantic model.
- `cmp_Int64` and `eq_Int64` agree with the signed ordering of the decoded mathematical values.
- The evaluator and Python runtime can execute programs importing `Bosatsu/Num/Int64`.
- The C runtime stores every `Int64` directly in `BValue` with no heap allocation for the value itself.
- Property-check coverage exists for the semantic model, and case-based coverage exists for the critical boundary values and machine-level edge cases.

## Risks

- Risk: users may confuse the checked and truncating constructors. Mitigation: keep the names explicit, document the semantic model in the package comments, and avoid a single overloaded `from_Int` name.
- Risk: backend drift in division or float rounding. Mitigation: make the exact `Int`-based model normative in tests and reuse the current Float64 rounding helper rather than inventing a second rule.
- Risk: C signed-overflow undefined behavior. Mitigation: use `uint64_t` modular arithmetic for wraparound operations and explicit special-casing for division edge cases.
- Risk: raw 64-bit payloads can conservatively look like pointers to the GC. Mitigation: accept the same tradeoff Bosatsu already accepts for raw `Float64` words and do not introduce any additional ownership or finalizer semantics.

## Rollout Notes

- Merging this design doc is the `planned` milestone for this lane. The implementation PR can continue under the same child issue afterward.
- Land the base `Bosatsu/Num/Int64` package first. The array and string offset specializations mentioned in the issue should remain a follow-up so the base semantic contract stabilizes first.
- This is an additive package and should not require migration work for existing users.
- After the runtime lands, benchmark validation against the motivating spectral norm workload is useful, but it should not block the base API merge if correctness and cross-backend agreement are already covered.
