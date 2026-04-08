# Int Performance Experiments

This document lists candidate performance experiments for `Int` in the C runtime.
It is intentionally focused on experiments to run and validate, not on implementation commitments.

## Current Shape

- `Int` operations in the C runtime are type-directed. If a helper says it takes an `Int`, the value is always an `Int`.
- Small `Int` values are immediate values today, using a 62-bit signed payload.
- Large `Int` values are heap-allocated bigints stored as sign-magnitude with `uint32_t` words.
- The current runtime already has a microbenchmark harness in `c_runtime/bench.c` for add, multiply, bitwise ops, and shifts.

## Measurement Plan

- Use `c_runtime/bench.c` as the first validation target.
- Add focused microbench cases for each experiment before changing the runtime.
- Measure at least:
  - small-small add/sub/mul
  - big-small add
  - big-small mul
  - mixed bitwise ops
  - small and big shifts
  - parse/format for medium and large decimal integers
- Record:
  - `ns/op`
  - allocation count if practical
  - whether the result stayed small or allocated
  - code size / complexity impact

## Experiment Loop

Follow this loop while working through the experiments on this branch:

1. Read this document.
2. Take the next experiment that has not been done. Make sure there is a benchmark that should improve. Measure that benchmark to get a baseline. Record the benchmark in the doc in the section for the current experiment.
3. Make the code changes proposed by the experiment.
4. Rerun the benchmark and record the results.
5. If the performance improves very confidently, keep the code. Otherwise, revert it.
6. Record the outcome of the current experiment in the doc.
7. Make a checkpoint commit.
8. Given what was learned, add any new experiment ideas to the doc.
9. If there are still unconducted experiments, repeat the loop starting again at step 1.

## Experiment List

### 1. Tag-Preserved Small-Int Arithmetic

Hypothesis:
For common small-small operations, working on the raw tagged `BValue` can avoid decode/re-encode overhead.

Experiments:

- Addition fast path using raw tagged values, e.g. `l + r - 1`, with explicit overflow detection.
- Subtraction fast path using raw tagged values, e.g. `l - r + 1`, with explicit overflow detection.
- Comparison fast path using raw signed `BValue` ordering instead of decode first.
- Bitwise fast paths:
  - `l & r`
  - `l | r`
  - `(l ^ r) | 1`
- Constant fast paths using raw tagged constants for `0`, `1`, and `-1`.

Questions:

- Does the compiler already optimize the current decode/encode path well enough that this is neutral?
- Which operations benefit measurably on current Clang/GCC targets?

Baseline benchmark:

- Benchmark binary: `c_runtime/bench_exe`
- Command: `./bench_exe 2000000`
- Samples: 5 runs, using the median `ns/op`
- Added benchmark cases:
  - `add_small_small`
  - `add_small_small62`
  - `cmp_small_small`
  - `cmp_small_small62`
  - `cmp_small_small62_neg`
  - `and_small_small62`
  - `or_small_small62`
  - `xor_small_small62`
- Baseline medians:
  - `add_small_small`: `1.93 ns/op`
  - `add_small_small62`: `1.89 ns/op`
  - `cmp_small_small`: `1.33 ns/op`
  - `cmp_small_small62`: `1.33 ns/op`
  - `cmp_small_small62_neg`: `1.33 ns/op`
  - `and_small_small62`: `1.36 ns/op`
  - `or_small_small62`: `1.40 ns/op`
  - `xor_small_small62`: `1.22 ns/op`

Experimental result:

- Tried raw-tag fast paths for small-small `add`, `cmp`, `and`, `or`, and `xor`.
- Reran the same benchmark command: `./bench_exe 2000000`
- Post-change medians:
  - `add_small_small`: `1.94 ns/op`
  - `add_small_small62`: `1.92 ns/op`
  - `cmp_small_small`: `1.35 ns/op`
  - `cmp_small_small62`: `1.35 ns/op`
  - `cmp_small_small62_neg`: `1.34 ns/op`
  - `and_small_small62`: `1.33 ns/op`
  - `or_small_small62`: `1.39 ns/op`
  - `xor_small_small62`: `1.22 ns/op`

Outcome:

- Reverted the runtime change.
- The only clear win was a small improvement in `and`, `or` improved only marginally, `xor` was flat, and `add` / `cmp` regressed slightly.
- This does not meet the "improves very confidently" threshold.
- Kept the benchmark additions in `c_runtime/bench.c` because they are useful for later experiments.

New ideas from this experiment:

- Add an assembly-inspection experiment before more source-level small-int rewrites, to check whether Clang already emits essentially optimal code for the current decode/re-encode paths.
- If raw-tag addition is revisited, try a version based on `__builtin_add_overflow` / `__builtin_sub_overflow` instead of the `__int128` helper, since the helper itself may cost more than the work it avoids.

### 2. Dedicated Runtime Entry Points for `sub` and `not`

Hypothesis:
The current `sub` and `not` lowering adds avoidable extra work in common cases.

Experiments:

- Add a direct `bsts_integer_sub` and compare it to `add(a, negate(b))`.
- Add a direct `bsts_integer_not` and compare it to `negate(add(a, 1))`.
- Check whether dedicated entrypoints help only small-small cases or also improve mixed-size paths.

Baseline benchmark:

- Benchmark binary: `c_runtime/bench_exe`
- Command: `./bench_exe 1000000`
- Samples: 5 runs, using the median `ns/op`
- Added benchmark cases:
  - `sub_small_small`
  - `sub_small_small62`
  - `sub_big_small62`
  - `sub_small_big`
  - `not_small_small62`
  - `not_big_pos`
  - `not_big_neg`
- Baseline medians:
  - `sub_small_small`: `3.26 ns/op`
  - `sub_small_small62`: `3.21 ns/op`
  - `sub_big_small62`: `35.23 ns/op`
  - `sub_small_big`: `43.65 ns/op`
  - `not_small_small62`: `3.73 ns/op`
  - `not_big_pos`: `45.65 ns/op`
  - `not_big_neg`: `42.48 ns/op`

Experimental result:

- Added direct `bsts_integer_sub` and `bsts_integer_not` entrypoints.
- Updated the Predef wrappers to call those entrypoints directly.
- Reran the same benchmark command: `./bench_exe 1000000`
- Post-change medians:
  - `sub_small_small`: `1.89 ns/op`
  - `sub_small_small62`: `1.87 ns/op`
  - `sub_big_small62`: `34.57 ns/op`
  - `sub_small_big`: `35.23 ns/op`
  - `not_small_small62`: `1.09 ns/op`
  - `not_big_pos`: `39.38 ns/op`
  - `not_big_neg`: `34.12 ns/op`

Outcome:

- Kept the code.
- Median improvements versus baseline:
  - `sub_small_small`: about `42%` faster
  - `sub_small_small62`: about `42%` faster
  - `sub_big_small62`: about `2%` faster
  - `sub_small_big`: about `19%` faster
  - `not_small_small62`: about `71%` faster
  - `not_big_pos`: about `14%` faster
  - `not_big_neg`: about `20%` faster
- The biggest win came from avoiding composed helper chains for `not`, and from avoiding construction of an intermediate negated bigint for subtraction when the right-hand side is big.

New ideas from this experiment:

- Add a follow-up experiment for a direct bigint `not` implementation. `not` is now much better, but the non-small path still routes through subtraction from `-1`.
- Add a focused experiment for `big - small` specialization. This path improved only slightly and likely still has avoidable generic-add overhead.

### 3. Expand Small-Int Range to 63 Payload Bits

Hypothesis:
If `Int`-specific code only needs bit `0` to distinguish immediate from pointer, we may be able to recover one extra payload bit and reduce bigint allocation frequency.

Experiments:

- Prototype a 1-bit-tag `Int` representation in `Int`-specific helpers only.
- Measure how often values that currently allocate as 62-bit overflow cases would stay immediate.
- Audit the impact on:
  - enum0 encoding
  - tiny strings / chars
  - generic pure-value helpers
  - code generation assumptions

Questions:

- Is this actually an `Int` optimization, or a wider pure-value ABI change?
- Does the added representation complexity pay for itself compared to cheaper local fast paths?

Baseline benchmark:

- Benchmark binary: `c_runtime/bench_exe`
- Command: `./bench_exe 1000000`
- Samples: 5 runs, using the median `ns/op`
- Added benchmark cases targeting values just beyond the old `2^61` small-int cutoff:
  - `add_small_small63`
  - `sub_small_small63`
  - `cmp_small_small63`
  - `and_small_small63`
  - `not_small_small63`
- Baseline medians:
  - `add_small_small63`: `3.78 ns/op`
  - `sub_small_small63`: `25.74 ns/op`
  - `cmp_small_small63`: `3.25 ns/op`
  - `and_small_small63`: `76.40 ns/op`
  - `not_small_small63`: `33.52 ns/op`

Experimental result:

- Expanded the Int-specific immediate encoding from 62 payload bits to 63 payload bits.
- Updated the integer helpers, canonicalization, Nat macros used by the clang backend, and boundary tests accordingly.
- Reran the same benchmark command: `./bench_exe 1000000`
- Post-change medians:
  - `add_small_small63`: `1.35 ns/op`
  - `sub_small_small63`: `1.66 ns/op`
  - `cmp_small_small63`: `1.32 ns/op`
  - `and_small_small63`: `1.29 ns/op`
  - `not_small_small63`: `1.35 ns/op`

Outcome:

- Kept the code.
- Median improvements versus baseline:
  - `add_small_small63`: about `64%` faster
  - `sub_small_small63`: about `94%` faster
  - `cmp_small_small63`: about `59%` faster
  - `and_small_small63`: about `98%` faster
  - `not_small_small63`: about `96%` faster
- This experiment delivered the intended result: values just beyond the old `2^61` cutoff now behave like normal immediates instead of heap-backed bigints.
- Validation:
  - `make bench_exe` passed.
  - `make test_exe` passed.
  - Running `./test_exe` still emitted `bsts_string_from_utf8_bytes_copy: string length 9223372036854775808 exceeds supported max (9223372036854775807)`, which appears to be the same pre-existing runtime test issue seen before this experiment rather than a new encoding-specific failure.

New ideas from this experiment:

- Revisit experiment 1 under the 1-bit Int encoding. Raw tagged arithmetic may be more compelling now that the Int representation really is “any odd value is small.”
- Add a Nat-focused benchmark set. Since the clang backend emits `BSTS_NAT_*` macros and those now step by `2` instead of `4`, Nat-heavy generated code should be measured directly.

### 4. BigInt Result Construction Without Temporary Copies

Hypothesis:
Some bigint operations currently pay for both temporary buffers and final copied allocation.

Experiments:

- Add an owned-buffer constructor for bigint results to avoid `calloc/malloc` plus `memcpy`.
- Compare:
  - stack temporary buffer + copy
  - heap temporary buffer + copy
  - direct final buffer construction
- Measure on:
  - bigint add
  - bigint mul
  - bitwise ops
  - shifts

Baseline benchmark:

- Benchmark binary: `c_runtime/bench_exe`
- Command: `./bench_exe 200000`
- Samples: 5 runs, using the median `ns/op`
- Added benchmark cases:
  - `add_big_big_pos`
  - `add_big_small63_direct`
  - `mul_big_big_pos`
  - `mul_big_small63_direct`
  - `and_big_big_pos`
  - `shift_big_pos_left`
  - `shift_big_pos_right`
- Baseline medians:
  - `add_big_big_pos`: `34.76 ns/op`
  - `add_big_small63_direct`: `36.53 ns/op`
  - `mul_big_big_pos`: `42.34 ns/op`
  - `mul_big_small63_direct`: `37.77 ns/op`
  - `and_big_big_pos`: `87.48 ns/op`
  - `shift_big_pos_left`: `58.19 ns/op`
  - `shift_big_pos_right`: `57.65 ns/op`

Experimental result:

- Added a direct-final-buffer path for bigint `add` and `times`.
- Instead of building a heap temporary word array and then copying it into a fresh `BSTS_Integer`, these operations now allocate the final bigint object first and write results directly into `integer->words`.
- Left bitwise and shift code unchanged on purpose, so they act as controls for this experiment.
- Reran the same benchmark command: `./bench_exe 200000`
- Post-change medians:
  - `add_big_big_pos`: `12.31 ns/op`
  - `add_big_small63_direct`: `13.20 ns/op`
  - `mul_big_big_pos`: `22.16 ns/op`
  - `mul_big_small63_direct`: `16.93 ns/op`
  - `and_big_big_pos`: `86.43 ns/op`
  - `shift_big_pos_left`: `57.00 ns/op`
  - `shift_big_pos_right`: `57.45 ns/op`

Outcome:

- Kept the code.
- Median improvements versus baseline:
  - `add_big_big_pos`: about `65%` faster
  - `add_big_small63_direct`: about `64%` faster
  - `mul_big_big_pos`: about `48%` faster
  - `mul_big_small63_direct`: about `55%` faster
  - `and_big_big_pos`: about `1%` faster
  - `shift_big_pos_left`: about `2%` faster
  - `shift_big_pos_right`: about `0%` faster
- The control cases staying essentially flat is useful here: it suggests the win is coming from eliminating the temp-buffer-plus-copy path in `add` and `times`, not from unrelated codegen noise in the benchmark binary.
- Validation:
  - `make bench_exe` passed.
  - `make test_exe` passed.

New ideas from this experiment:

- Add a follow-up experiment for the two's-complement pipeline used by bitwise ops and shifts. Those paths likely need an in-place `from_twos` style constructor to see a similar benefit.
- Add a "small result from big operands" benchmark set. Direct final allocation is excellent when the result stays big, but it may over-allocate when cancellation collapses back to an immediate.

### 5. Stack Buffers for Small BigInts

Hypothesis:
Many "big" ints are still only a handful of words, so stack temporaries may cut heap traffic substantially.

Experiments:

- Use fixed local arrays for results up to a small threshold such as 2, 4, 8, 16, or 32 words.
- Compare fixed local arrays vs `alloca` vs current heap temp allocation.
- Tune thresholds using benchmarks instead of picking one up front.

Questions:

- Does stack usage stay reasonable in recursive or nested arithmetic paths?
- Does this help enough once final result allocation is still required?

Baseline benchmark:

- Benchmark binary: `c_runtime/bench_exe`
- Command: `./bench_exe 200000`
- Samples: 5 runs, using the median `ns/op`
- Added benchmark cases:
  - `or_big_big_pos`
  - `xor_big_big_pos`
- Reused benchmark cases:
  - `and_big_big_pos`
  - `and_neg_mixed62`
  - `shift_big_pos_left`
  - `shift_big_pos_right`
  - `shift_neg_left`
  - `shift_neg_right`
- Baseline medians:
  - `and_big_big_pos`: `84.61 ns/op`
  - `or_big_big_pos`: `83.00 ns/op`
  - `xor_big_big_pos`: `82.29 ns/op`
  - `and_neg_mixed62`: `82.56 ns/op`
  - `shift_big_pos_left`: `56.20 ns/op`
  - `shift_big_pos_right`: `56.95 ns/op`
  - `shift_neg_left`: `78.58 ns/op`
  - `shift_neg_right`: `79.81 ns/op`

Experimental result:

- Added a fixed local-buffer fast path for word temporaries up to `8` words.
- Applied that helper to the temporary arrays used by bigint bitwise ops and the two's-complement shift pipeline.
- Used stack buffers for `l_twos`, `r_twos`, `result_twos`, and `new_words` when the working size is small, with heap fallback for larger inputs.
- Reran the same benchmark command: `./bench_exe 200000`
- Post-change medians:
  - `and_big_big_pos`: `34.45 ns/op`
  - `or_big_big_pos`: `33.87 ns/op`
  - `xor_big_big_pos`: `32.78 ns/op`
  - `and_neg_mixed62`: `31.44 ns/op`
  - `shift_big_pos_left`: `31.29 ns/op`
  - `shift_big_pos_right`: `31.17 ns/op`
  - `shift_neg_left`: `51.41 ns/op`
  - `shift_neg_right`: `51.48 ns/op`

Outcome:

- Kept the code.
- Median improvements versus baseline:
  - `and_big_big_pos`: about `59%` faster
  - `or_big_big_pos`: about `59%` faster
  - `xor_big_big_pos`: about `60%` faster
  - `and_neg_mixed62`: about `62%` faster
  - `shift_big_pos_left`: about `44%` faster
  - `shift_big_pos_right`: about `45%` faster
  - `shift_neg_left`: about `35%` faster
  - `shift_neg_right`: about `36%` faster
- The main win here is simply removing heap allocation/free traffic for the common 4-6 word temporaries used by the current benchmark shapes.
- Validation:
  - `make bench_exe` passed.
  - `make test_exe` passed.

New ideas from this experiment:

- Add a threshold-tuning experiment for the stack-buffer cap itself. `8` words worked well on these cases, but `4`, `16`, or per-operation thresholds may be better.
- Add an in-place two's-complement-to-sign-magnitude experiment. Even after removing heap temps, negative bitwise and shift cases still spend noticeable time in the conversion pipeline.

### 6. Mixed-Mode Arithmetic Fast Paths

Hypothesis:
When one side is small and the other side is a 1-2 word bigint, current generic code is heavier than necessary.

Experiments:

- Specialize `big + small` and `big - small`.
- Specialize `big * small`.
- For 1-2 word positive magnitudes, use `uint64_t`, `int64_t`, or `__int128` before falling back to the general path.
- Measure especially:
  - increment/decrement loops
  - multiply by small constants
  - negative mixed-size arithmetic

### 7. Direct Sign-Magnitude Bitwise Operations

Hypothesis:
The current bitwise path spends too much time converting sign-magnitude to two's complement and back.

Experiments:

- Implement direct sign-magnitude `and/or/xor` for the four sign combinations.
- Add a positive-positive fast path with no conversion at all.
- Add mixed fast paths for common masks:
  - `x & 0`
  - `x & -1`
  - `x | 0`
  - `x | -1`
  - `x ^ 0`
- Compare against the current conversion-based path on:
  - positive-positive bigints
  - negative-positive mixed cases
  - small-big mixed masks

### 8. Direct Shift Implementations

Hypothesis:
Left/right shift should not need the current full two's-complement temporary-array pipeline.

Experiments:

- Add small-small shift fast paths.
- Implement direct word/bit shifting on bigint magnitude for positive values.
- Implement direct arithmetic right shift for negative values without round-tripping through generic two's-complement buffers.
- Compare shift-by-constant cases such as `1`, `5`, `31`, `32`, `63`, `64`.

### 9. Route Multiplication by Powers of Two Through Shift Logic

Hypothesis:
Many small-constant multiplies can be cheaper as shifts.

Experiments:

- Detect small rhs/lhs values that are powers of two.
- Route them through optimized shift logic.
- Compare against normal multiply for:
  - small-small
  - big-small
  - negative values

Questions:

- Does branch overhead erase the win for common non-power-of-two cases?

### 10. Faster Canonicalization Back to Small Ints

Hypothesis:
Every bigint-producing path eventually asks whether the result can collapse back to a small int, and some of those checks can fail much earlier.

Experiments:

- Add quick exits when word count is obviously too large.
- Avoid allocating a bigint object before confirming the result is still big.
- Compare canonicalization cost in:
  - bitwise ops
  - shifts
  - multiply
  - bigint add/sub

### 11. Small-Int Constants and Identity Fast Paths

Hypothesis:
Some common cases can avoid even the normal small-int fast path.

Experiments:

- Use raw tagged constants/macros for:
  - `0`
  - `1`
  - `-1`
- Add identity handling for:
  - `x + 0`
  - `x - 0`
  - `x * 0`
  - `x * 1`
  - `x * -1`
  - bitwise identities listed above
- Benchmark branch cost vs saved work.

### 12. BigInt Limb Width Experiment

Hypothesis:
On a 64-bit-only runtime, 64-bit limbs may reduce loop trip counts and conversion overhead.

Experiments:

- Prototype `uint64_t` limbs instead of `uint32_t` limbs for bigint internals.
- Compare:
  - add
  - mul
  - shifts
  - bitwise ops
  - parse/format

Questions:

- Does this improve throughput enough to justify the refactor and portability constraints?

### 13. Decimal Parse / Format Improvements

Hypothesis:
Large decimal conversion is currently much more allocation-heavy and loop-heavy than it needs to be.

Experiments:

- For parsing:
  - accumulate more digits per step before promoting
  - parse in chunks instead of repeated `x * 10 + d`
- For formatting:
  - divide by larger powers of ten per step instead of one decimal digit at a time
- Measure on large decimal inputs and outputs, not just arithmetic loops.

### 14. Division and Modulo Special Cases

Hypothesis:
A few special divisors can avoid the general recursive path.

Experiments:

- Fast path division/mod by powers of two.
- Fast path division/mod by small positive ints on small bigints.
- Benchmark `gcd`, since it repeatedly calls `div_mod`.

## Suggested Order

The likely highest-value / lowest-risk experiments are:

1. Tag-preserved small-int arithmetic.
2. Dedicated `sub` and `not`.
3. Mixed-mode `big +/-/* small`.
4. BigInt result construction without temporary copies.
5. Direct bitwise and shift implementations.

The likely highest-risk experiments are:

1. 1-bit small-int tagging.
2. 64-bit limbs.
3. Full direct sign-magnitude bitwise logic for all sign combinations.

## Exit Criteria

An experiment is worth keeping only if it shows a clear win on representative benchmarks and does not make the runtime materially harder to maintain or reason about.
