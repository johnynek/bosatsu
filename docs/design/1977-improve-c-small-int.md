---
issue: 1977
priority: 3
touch_paths:
  - docs/design/1977-improve-c-small-int.md
  - c_runtime/bosatsu_runtime.c
  - c_runtime/bosatsu_runtime.h
  - c_runtime/test.c
  - c_runtime/bench.c
  - c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c
  - c_runtime/bosatsu_ext_Bosatsu_l_Num_l_Float64.c
  - core/src/main/scala/dev/bosatsu/codegen/clang/ClangGen.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-03T23:06:06Z
---

# Issue #1977 Design: Improve C Small Int Representation

_Issue: #1977 (https://github.com/johnynek/bosatsu/issues/1977)_

## Summary

Adopt a 62-bit signed immediate integer representation for the C runtime (within the existing 2-bit tagged-value scheme), update arithmetic fast paths to use safe overflow checks, and canonicalize 1-2 word big-int results back into immediates. This keeps the current pointer-tagging architecture intact, avoids a high-risk retagging rewrite, and significantly expands the non-allocating integer range from 32-bit to 62-bit signed values.

# Issue #1977 Design: Improve C small int

Status: proposed

## Summary

The C runtime currently treats only signed 32-bit integers as immediate (tagged) values, even though the runtime’s 64-bit tagged representation has much more payload capacity. This design proposes moving to signed 62-bit immediates (range `[-2^61, 2^61 - 1]`) while keeping the current 2-bit tag scheme unchanged.

This gives a large reduction in big-int allocations for common values (timestamps, byte sizes, medium-large counters, arithmetic intermediates), preserves existing runtime object model assumptions, and avoids a high-risk global retagging rewrite.

## Problem statement

Current behavior in `c_runtime/bosatsu_runtime.c`:

- Values use a 2-bit tag at the bottom of `BValue` (`00` pointer, `01` pure value).
- Small ints are decoded as `int32_t` (`GET_SMALL_INT`) and encoded through `bsts_integer_from_int(int32_t)`.
- `bsts_integer_from_int64` and helper conversion logic only keep values immediate if they fit signed 32-bit.
- Many integer operations quickly spill to heap-allocated big-int form once values exceed `INT32_MAX`/`INT32_MIN`.

This is correct but leaves substantial performance and allocation improvements on the table.

## Goals

- Expand immediate integer range in C runtime significantly beyond signed 32-bit.
- Keep existing 64-bit pointer-tagging architecture and pure-value/object split.
- Preserve Bosatsu integer semantics (unbounded integers, floor division behavior, bitwise semantics on negatives).
- Minimize generated-code churn and public API disruption.
- Keep behavior deterministic and portable on supported 64-bit targets.

## Non-goals

- Redesign all pure-value encodings (enum/tag/function/tiny-string layout).
- Add 32-bit platform support.
- Replace sign-magnitude big-int storage format.
- Redefine `bsts_integer_to_int32` contract.

## Recommendation

Adopt signed 62-bit immediate integers under the current 2-bit tag scheme.

- With 2 low bits reserved for tags, payload is 62 bits.
- Signed 62-bit two’s-complement range is `[-2^61, 2^61 - 1]`.
- This is the largest practical small-int range without changing tag layout.

### Why not “all 63 spare bits” in this change

Using a 63-bit payload would require moving to a 1-bit tag model (or equivalent global retagging), which would touch:

- Nat/enum encoding assumptions.
- Tiny-string bit layout.
- Boxed pure function encoding assumptions.
- Generated C helper behavior that currently assumes `<< 2`/`>> 2` pure-value transforms.

That is a broad architecture change with higher regression risk for relatively small incremental gain over 62-bit immediates. This issue should be solved first with the low-risk/high-impact 62-bit approach.

## Proposed architecture

### 1) Centralize small-int format constants and helpers

In `c_runtime/bosatsu_runtime.c` (and exported constants/helpers in `c_runtime/bosatsu_runtime.h` where needed):

- Define small-int payload width and limits:
- `BSTS_SMALL_INT_PAYLOAD_BITS = 62`
- `BSTS_SMALL_INT_MIN = -(1LL << 61)`
- `BSTS_SMALL_INT_MAX = (1LL << 61) - 1`

- Add helper routines for:
- decoding tagged small int to `int64_t` (sign-extended from 62-bit payload),
- checking whether an `int64_t` fits small-int range,
- encoding `int64_t` to tagged small-int (unchecked/checked variants).

- Keep `bsts_integer_from_int(int32_t)` as a compatibility wrapper.
- Expose or standardize `bsts_integer_from_int64`/`bsts_integer_from_uint64` usage so external C modules stop duplicating range logic.

### 2) Canonicalize up to 2-word magnitudes back to immediates

Current `bsts_maybe_small_int` only considers one 32-bit word. With a 62-bit immediate range, canonicalization must also accept 2-word magnitudes.

Introduce a helper that takes sign + normalized words and returns tagged immediate when value fits 62-bit range.

Update call sites that normalize arithmetic results to use this helper, especially:

- `bsts_integer_from_words_copy`
- `bsts_integer_from_twos`
- small-result checks in add/sub/mul code paths

This ensures values that mathematically fit 62-bit immediates are not left heap-allocated.

### 3) Update integer fast paths to `int64_t` small operands

Update small-int decode/encode paths in integer operations:

- `bsts_integer_to_int32`
- `bsts_integer_add`
- `bsts_integer_negate`
- `bsts_integer_times`
- `bsts_integer_and`/`or`/`xor`
- `bsts_integer_cmp`
- `bsts_integer_shift_left`
- small-value conversion helpers used by div/mod and sign-magnitude conversion

Key details:

- Addition/subtraction of two 62-bit small values is safe in `int64_t` (no `int64_t` overflow), then range-check for re-tagging.
- Multiplication must avoid UB from signed overflow. Use overflow-safe logic:
- preferred: `__int128` path where available,
- fallback: overflow-check + word-based big-int multiplication path.
- Preserve current floor-division correction rules.
- Keep negative shift and large shift semantics stable; add explicit guardrails against `size_t` overflow for huge left shifts.

### 4) Update C extern modules that duplicated 32-bit small-int assumptions

At minimum:

- `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c`
- replace local int64 conversion logic with runtime helper or updated limit constants.

- `c_runtime/bosatsu_ext_Bosatsu_l_Num_l_Float64.c`
- update uint64-to-int conversion fast path to 62-bit immediate thresholds.

This prevents drift between runtime core and extern modules.

### 5) (Optional but recommended) tighten C codegen for integer literals

Current codegen in `ClangGen.scala` emits `bsts_integer_from_int` for 32-bit literals and word arrays otherwise.

Two options:

- Keep as-is and rely on improved `bsts_integer_from_words_copy` canonicalization.
- Or add an `int64` literal path (`bsts_integer_from_int64`) for cleaner generated C and fewer temporary arrays.

Either approach works with the runtime change; the second improves generated code quality.

## Detailed implementation plan

1. Add small-int width/limit constants and helper encode/decode functions in runtime.
2. Replace `int32_t`-based small decode macro usage with `int64_t` small decode helpers.
3. Replace one-word-only small canonicalization with 1-2 word canonicalization.
4. Update arithmetic/bitwise/shift/divmod small fast paths to use the new helpers and overflow-safe multiply.
5. Update external C modules that currently hardcode `INT32` small-int boundaries.
6. Extend tests for boundary values around `±(2^61)` and operation crossovers small->big.
7. Add/adjust microbenchmarks to cover values above 32-bit but within 62-bit range.
8. Optionally add codegen literal path improvements and tests.

## Testing strategy

### Runtime correctness tests (`c_runtime/test.c`)

Add explicit boundary coverage:

- construction and string roundtrip for:
- `2^61 - 1`
- `-2^61`
- `2^61` (must spill to big)
- `-(2^61 + 1)` (must be big)

- arithmetic transitions:
- `(2^61 - 1) + 1` spills to big
- `(-2^61) - 1` spills to big
- `(-2^61)` negation becomes big `2^61`

- multiplication transitions:
- in-range small*small stays immediate
- overflow-to-big cases are exact

- bitwise/shift/divmod parity across sign combinations.

- `bsts_integer_to_int32` compatibility behavior for values now-small but outside int32 range.

### Codegen tests (if literal path is changed)

- Add/update `ClangGenTest` expectations for integer literals in int64-but-not-int32 range.
- Ensure generated code compiles and runtime values match prior semantics.

### Benchmarks (`c_runtime/bench.c`)

Add scenarios with operands in `(2^31, 2^61)` to validate reduced allocation behavior and throughput gains compared to prior behavior.

## Acceptance criteria

- `c_runtime` test suite passes with default configuration on supported 64-bit targets.
- Values in `[-2^61, 2^61 - 1]` are represented as immediate integers (including via `bsts_integer_from_words_copy` canonicalization where applicable).
- Values outside that range are represented as big ints with correct sign/magnitude and operation results.
- Integer operations (`add`, `times`, `negate`, `and/or/xor`, `cmp`, `shift`, `div_mod`) remain semantically correct for existing tests and new boundary tests.
- `bsts_integer_to_int32` behavior remains backward-compatible for out-of-int32 inputs.
- External C modules compile cleanly without duplicating stale INT32-only assumptions.
- Benchmarks include new >32-bit-small scenarios and show expected reduction in heap-backed integer usage.

## Risks and mitigations

- Risk: signed-overflow UB in multiply fast paths.
- Mitigation: use explicit overflow-safe multiply strategy (`__int128` or safe fallback), never unchecked `int64_t` multiply for general 62-bit operands.

- Risk: subtle sign-extension bugs when decoding tagged small values.
- Mitigation: centralized decode helper, dedicated boundary tests, and cross-check tests against big-int results.

- Risk: large shift amounts causing allocation overflow or pathological behavior.
- Mitigation: add explicit bounds checks in shift helpers and preserve current abort/error behavior for unrepresentable allocations.

- Risk: drift between runtime core and extern module conversion logic.
- Mitigation: route extern modules through shared runtime helpers/constants.

- Risk: performance regression on tiny 32-bit-heavy workloads.
- Mitigation: retain simple fast paths, measure with existing and new benchmarks, and tune branch structure if needed.

## Rollout notes

- This is runtime-internal; no Bosatsu language-level behavior change is expected.
- Existing generated C should remain source-compatible if `bsts_integer_from_int` stays intact.
- Land as one cohesive runtime PR with tests and benchmark updates.
- If needed for safer rollout, keep a compile-time switch for small-int width (`32` vs `62`) during validation, then default to `62` once stable.

## Alternatives considered

- Keep signed 32-bit immediates:
- Lowest risk but leaves large performance/allocation gains unrealized.

- Retag runtime for 63-bit payload immediates:
- Higher theoretical range but requires broad encoding changes across runtime and generated code with substantially higher risk.

- Hybrid multi-tag integer encodings:
- Adds complexity and branch cost with unclear advantage over straightforward 62-bit immediate + existing big-int fallback.

## Decision

Proceed with signed 62-bit immediate integers under the existing 2-bit tagged-value architecture, plus overflow-safe arithmetic updates and wider canonicalization from word form back to immediates.
