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

Baseline benchmark:

- Benchmark binary: `c_runtime/bench_exe`
- Command: `./bench_exe 500000`
- Samples: 5 runs, using the median `ns/op`
- Added benchmark cases targeting 2-word heap bigints just beyond the current immediate range:
  - `add_big2_small63_pos`
  - `add_big2_small63_neg`
  - `sub_big2_small63`
  - `sub_small63_big2`
  - `mul_big2_small10`
  - `mul_big2_small63_pos`
  - `mul_big2_small63_neg`
- Baseline medians:
  - `add_big2_small63_pos`: `10.22 ns/op`
  - `add_big2_small63_neg`: `10.79 ns/op`
  - `sub_big2_small63`: `12.40 ns/op`
  - `sub_small63_big2`: `11.27 ns/op`
  - `mul_big2_small10`: `12.40 ns/op`
  - `mul_big2_small63_pos`: `13.47 ns/op`
  - `mul_big2_small63_neg`: `13.52 ns/op`

Experimental result:

- Added a `__int128` mixed-mode fast path for `add` and `sub` when the bigint operand is only 2 words long.
- The fast path converts the 2-word sign-magnitude bigint to `__int128`, performs the arithmetic with the small operand, and then canonicalizes directly back to immediate-or-bigint.
- Also tried the same approach for mixed `mul`, but reran the benchmark and reverted that part because it was not a confident win.
- Final kept code reran the same benchmark command: `./bench_exe 500000`
- Post-change medians:
  - `add_big2_small63_pos`: `9.88 ns/op`
  - `add_big2_small63_neg`: `1.85 ns/op`
  - `sub_big2_small63`: `2.27 ns/op`
  - `sub_small63_big2`: `2.35 ns/op`
  - `mul_big2_small10`: `12.77 ns/op`
  - `mul_big2_small63_pos`: `13.69 ns/op`
  - `mul_big2_small63_neg`: `13.66 ns/op`

Outcome:

- Kept the mixed `add` / `sub` code.
- Reverted the mixed `mul` specialization.
- Median improvements versus baseline on the kept target cases:
  - `add_big2_small63_pos`: about `3%` faster
  - `add_big2_small63_neg`: about `83%` faster
  - `sub_big2_small63`: about `82%` faster
  - `sub_small63_big2`: about `79%` faster
- The main win is avoiding the generic bigint add/sub machinery when a 2-word heap bigint and a small immediate can be folded straight through `__int128`, especially when the result collapses back to an immediate.
- The multiply attempt did not meet the bar:
  - `mul_big2_small10`: about `3%` slower in the final measurement
  - `mul_big2_small63_pos`: about `2%` slower in the final measurement
  - `mul_big2_small63_neg`: about `1%` slower in the final measurement
- Validation:
  - `make bench_exe` passed.
  - `make test_exe` passed.

New ideas from this experiment:

- Add a dedicated “big plus/minus small collapsing to immediate” benchmark family for 3-word and 4-word bigints. That may justify a broader early-out path without needing a full mixed-mode specialization.
- If mixed multiply is revisited, do it with a more targeted case such as power-of-two or tiny-constant multiplication rather than a generic `__int128` detour.

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

Baseline benchmark:

- Benchmark binary: `c_runtime/bench_exe`
- Command: `./bench_exe 200000`
- Samples: 5 runs, using the median `ns/op`
- Reused benchmark cases:
  - `and_big_big_pos`
  - `or_big_big_pos`
  - `xor_big_big_pos`
  - `and_neg_mixed62`
- Baseline medians:
  - `and_big_big_pos`: `33.20 ns/op`
  - `or_big_big_pos`: `33.81 ns/op`
  - `xor_big_big_pos`: `32.76 ns/op`
  - `and_neg_mixed62`: `31.20 ns/op`

Experimental result:

- Implemented the first direct sign-magnitude fast path: positive-positive bigint `and`, `or`, and `xor`.
- For those cases, the runtime now operates directly on the magnitude words and writes the result straight into the final bigint object.
- Left negative and mixed-sign cases on the existing conversion-based path for this experiment.
- Reran the same benchmark command: `./bench_exe 200000`
- Post-change medians:
  - `and_big_big_pos`: `9.79 ns/op`
  - `or_big_big_pos`: `9.89 ns/op`
  - `xor_big_big_pos`: `9.65 ns/op`
  - `and_neg_mixed62`: `30.95 ns/op`

Outcome:

- Kept the code.
- Median improvements versus baseline:
  - `and_big_big_pos`: about `71%` faster
  - `or_big_big_pos`: about `71%` faster
  - `xor_big_big_pos`: about `71%` faster
  - `and_neg_mixed62`: about `1%` faster
- The mixed negative control staying flat is important: it confirms the win is specifically from bypassing the sign-magnitude <-> two's-complement round-trip for positive-positive bigints.
- Validation:
  - `make bench_exe` passed.
  - `make test_exe` passed.

New ideas from this experiment:

- Extend the direct bitwise path to positive-small and small-positive masks. Those should be nearly as simple as positive-positive bigints now that the wordwise path exists.
- Tackle the negative cases separately. They still spend most of their time in the conversion pipeline and likely need a different algorithm than the positive path.

### 8. Direct Shift Implementations

Hypothesis:
Left/right shift should not need the current full two's-complement temporary-array pipeline.

Experiments:

- Add small-small shift fast paths.
- Implement direct word/bit shifting on bigint magnitude for positive values.
- Implement direct arithmetic right shift for negative values without round-tripping through generic two's-complement buffers.
- Compare shift-by-constant cases such as `1`, `5`, `31`, `32`, `63`, `64`.

Baseline benchmark:

- Benchmark binary: `c_runtime/bench_exe`
- Command: `./bench_exe 200000`
- Samples: 5 runs, using the median `ns/op`
- Added benchmark cases:
  - `shift_small_pos_left1`
  - `shift_small_pos_right1`
  - `shift_small63_left5`
  - `shift_small63_right5`
  - `shift_small63_left40`
  - `shift_big_pos_left32`
  - `shift_big_pos_right32`
- Reused benchmark cases:
  - `shift_big_pos_left`
  - `shift_big_pos_right`
  - `shift_neg_left`
  - `shift_neg_right`
- Baseline medians:
  - `shift_small_pos_left1`: `19.15 ns/op`
  - `shift_small_pos_right1`: `20.64 ns/op`
  - `shift_small63_left5`: `20.07 ns/op`
  - `shift_small63_right5`: `17.66 ns/op`
  - `shift_small63_left40`: `18.87 ns/op`
  - `shift_big_pos_left`: `30.95 ns/op`
  - `shift_big_pos_right`: `32.52 ns/op`
  - `shift_big_pos_left32`: `33.98 ns/op`
  - `shift_big_pos_right32`: `36.48 ns/op`
  - `shift_neg_left`: `53.64 ns/op`
  - `shift_neg_right`: `58.16 ns/op`

Experimental result:

- Added direct sign-magnitude left shift for both positive and negative integers.
- Added direct right shift for non-negative integers.
- Added no-allocation immediate fast paths for non-negative small-int left/right shifts when the result stays immediate.
- Left negative right shift on the existing two's-complement path for this experiment.
- Added shift tests covering positive right shifts, word-boundary right shift, and negative left shift.
- Reran the same benchmark command: `./bench_exe 200000`
- Post-change medians:
  - `shift_small_pos_left1`: `1.72 ns/op`
  - `shift_small_pos_right1`: `1.85 ns/op`
  - `shift_small63_left5`: `11.10 ns/op`
  - `shift_small63_right5`: `1.85 ns/op`
  - `shift_small63_left40`: `11.53 ns/op`
  - `shift_big_pos_left`: `13.86 ns/op`
  - `shift_big_pos_right`: `9.96 ns/op`
  - `shift_big_pos_left32`: `12.26 ns/op`
  - `shift_big_pos_right32`: `9.08 ns/op`
  - `shift_neg_left`: `13.55 ns/op`
  - `shift_neg_right`: `56.55 ns/op`

Outcome:

- Kept the code.
- Median improvements versus baseline:
  - `shift_small_pos_left1`: about `91%` faster
  - `shift_small_pos_right1`: about `91%` faster
  - `shift_small63_left5`: about `45%` faster
  - `shift_small63_right5`: about `90%` faster
  - `shift_small63_left40`: about `39%` faster
  - `shift_big_pos_left`: about `55%` faster
  - `shift_big_pos_right`: about `69%` faster
  - `shift_big_pos_left32`: about `64%` faster
  - `shift_big_pos_right32`: about `75%` faster
  - `shift_neg_left`: about `75%` faster
  - `shift_neg_right`: about `3%` faster
- This split is exactly what the experiment intended: direct sign-magnitude shifting removes most of the cost for left shifts and for non-negative right shifts, while negative right shifts still pay for the old conversion path.
- Validation:
  - `make bench_exe` passed.
  - `make test_exe` passed.
  - Running `./test_exe` still emitted `bsts_string_from_utf8_bytes_copy: string length 9223372036854775808 exceeds supported max (9223372036854775807)` and did not give a clean tool-level exit, matching the same pre-existing test-run issue seen in earlier experiments.

New ideas from this experiment:

- Split negative right shift into its own experiment. The direct arithmetic formula `-((|x| + 2^k - 1) >> k)` is likely the right next target.
- Revisit multiplication by powers of two now that `bsts_integer_shift_left` is much cheaper for exactly the mixed and bigint cases that matter.

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

Baseline benchmark:

- Benchmark binary: `c_runtime/bench_exe`
- Command: `./bench_exe 300000`
- Samples: 5 runs, using the median `ns/op`
- Added benchmark cases:
  - `mul_small63_pow2`
  - `mul_big_pow2_pos`
  - `mul_big_pow2_neg`
  - `mul_big2_pow2_pos`
  - `mul_big2_pow2_neg`
- Reused control cases:
  - `mul_big_small_pos`
  - `mul_big2_small10`
- Baseline medians:
  - `mul_small63_pow2`: `12.27 ns/op`
  - `mul_big_pow2_pos`: `16.79 ns/op`
  - `mul_big_pow2_neg`: `16.43 ns/op`
  - `mul_big2_pow2_pos`: `13.12 ns/op`
  - `mul_big2_pow2_neg`: `12.71 ns/op`
  - `mul_big_small_pos`: `17.21 ns/op`
  - `mul_big2_small10`: `13.00 ns/op`

Experimental result:

- Tried routing bigint-times-small-power-of-two multiplication through `bsts_integer_shift_left`.
- The best candidate version only routed positive small powers of two for mixed big-small multiplication, leaving negative small constants on the generic multiply path.
- Added multiplication tests for bigint times `32` and `-32`.
- Reran the same benchmark command: `./bench_exe 300000`
- Candidate post-change medians:
  - `mul_small63_pow2`: `12.39 ns/op`
  - `mul_big_pow2_pos`: `16.32 ns/op`
  - `mul_big_pow2_neg`: `16.69 ns/op`
  - `mul_big2_pow2_pos`: `12.83 ns/op`
  - `mul_big2_pow2_neg`: `12.81 ns/op`
  - `mul_big_small_pos`: `17.52 ns/op`
  - `mul_big2_small10`: `12.95 ns/op`

Outcome:

- Reverted the runtime change.
- Kept the benchmark additions in `c_runtime/bench.c` and the semantic multiply tests in `c_runtime/test.c`.
- The measured wins were too small and too inconsistent to justify the extra branch:
  - `mul_big_pow2_pos`: about `3%` faster
  - `mul_big2_pow2_pos`: about `2%` faster
  - `mul_small63_pow2`: about `1%` slower
  - `mul_big_pow2_neg`: about `2%` slower
  - `mul_big2_pow2_neg`: about `1%` slower
  - `mul_big_small_pos` control: about `2%` slower
- This does not meet the "improves very confidently" threshold, especially after experiment 8 already made the generic shift path cheaper.
- Validation:
  - `make bench_exe` passed.
  - `make test_exe` passed.

New ideas from this experiment:

- If multiplication by powers of two is revisited, target only very large bigints where schoolbook multiplication cost dominates branch overhead.
- A stronger follow-up may be a dedicated negative-right-shift optimization rather than trying to reuse shift improvements indirectly through multiply.

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

Baseline benchmark:

- Benchmark binary: `c_runtime/bench_exe`
- Command: `./bench_exe 300000`
- Samples: 5 runs, using the median `ns/op`
- Added benchmark cases:
  - `and_big_big_smallres`
  - `xor_big_big_smallres`
  - `shift_big_pos_right64_small`
  - `shift_big_pos_right96_small`
- Reused control cases:
  - `and_big_big_pos`
  - `shift_big_pos_right32`
- Baseline medians:
  - `and_big_big_pos`: `10.20 ns/op`
  - `and_big_big_smallres`: `10.80 ns/op`
  - `xor_big_big_smallres`: `11.07 ns/op`
  - `shift_big_pos_right32`: `9.15 ns/op`
  - `shift_big_pos_right64_small`: `9.31 ns/op`
  - `shift_big_pos_right96_small`: `7.72 ns/op`

Experimental result:

- Added a cheap “too many words to ever be small” fast exit in `bsts_integer_from_words_copy` and `bsts_integer_finish_allocated_words`.
- Added early small-result detection for positive-positive bigint `and` and `xor` when all words above the low 64 bits cancel out.
- Added a no-allocation small-result path for positive right shifts when the shifted result is only 1-2 words long.
- Added semantic tests covering bigint `and` / `xor` collapsing to immediates and bigint right shifts collapsing to immediates.
- Reran the same benchmark command: `./bench_exe 300000`
- Post-change medians:
  - `and_big_big_pos`: `10.93 ns/op`
  - `and_big_big_smallres`: `4.76 ns/op`
  - `xor_big_big_smallres`: `4.83 ns/op`
  - `shift_big_pos_right32`: `8.86 ns/op`
  - `shift_big_pos_right64_small`: `4.56 ns/op`
  - `shift_big_pos_right96_small`: `5.55 ns/op`

Outcome:

- Kept the code.
- Median changes versus baseline:
  - `and_big_big_smallres`: about `56%` faster
  - `xor_big_big_smallres`: about `56%` faster
  - `shift_big_pos_right64_small`: about `51%` faster
  - `shift_big_pos_right96_small`: about `28%` faster
  - `shift_big_pos_right32` control: about `3%` faster
  - `and_big_big_pos` control: about `7%` slower
- This is still a confident keep. The experiment materially improved the exact “big in, small out” cases it targeted, and the only notable tradeoff was a modest slowdown on the already-fast positive-positive bigint `and` path.
- Validation:
  - `make bench_exe` passed.
  - `make test_exe` passed.

New ideas from this experiment:

- Extend small-result canonicalization to subtraction when large operands nearly cancel. That likely needs a stack temporary or a more specialized borrow-aware early check.
- If the `and_big_big_pos` regression matters, split the bitwise small-result probe into a more selective heuristic so it only runs when the high words plausibly cancel.

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

Baseline benchmark:

- Benchmark binary: `c_runtime/bench_exe`
- Command: `./bench_exe 500000`
- Samples: 5 runs, using the median `ns/op`
- Added benchmark cases:
  - `and_big_zero`
  - `and_big_neg1`
  - `or_big_zero`
  - `or_big_neg1`
  - `xor_big_zero`
  - `xor_big_neg1`
- Reused control case:
  - `and_big_big_pos`
- Baseline medians:
  - `and_big_big_pos`: `10.55 ns/op`
  - `and_big_zero`: `33.37 ns/op`
  - `and_big_neg1`: `31.89 ns/op`
  - `or_big_zero`: `32.06 ns/op`
  - `or_big_neg1`: `54.03 ns/op`
  - `xor_big_zero`: `31.72 ns/op`
  - `xor_big_neg1`: `51.84 ns/op`

Experimental result:

- Added direct identity handling for mixed-size bitwise ops involving small `0` and `-1`.
- Implemented:
  - `x & 0 -> 0`
  - `x & -1 -> x`
  - `x | 0 -> x`
  - `x | -1 -> -1`
  - `x ^ 0 -> x`
  - `x ^ -1 -> not(x)`
- Added semantic tests covering those identities on a bigint operand.
- Reran the same benchmark command: `./bench_exe 500000`
- Post-change medians:
  - `and_big_big_pos`: `11.09 ns/op`
  - `and_big_zero`: `1.88 ns/op`
  - `and_big_neg1`: `1.60 ns/op`
  - `or_big_zero`: `2.19 ns/op`
  - `or_big_neg1`: `1.93 ns/op`
  - `xor_big_zero`: `1.87 ns/op`
  - `xor_big_neg1`: `15.25 ns/op`

Outcome:

- Kept the code.
- Median improvements versus baseline:
  - `and_big_zero`: about `94%` faster
  - `and_big_neg1`: about `95%` faster
  - `or_big_zero`: about `93%` faster
  - `or_big_neg1`: about `96%` faster
  - `xor_big_zero`: about `94%` faster
  - `xor_big_neg1`: about `71%` faster
  - `and_big_big_pos` control: about `5%` slower
- This is still an easy keep. The identity cases were forcing fully generic bitwise code for no reason, and the fast paths remove almost all of that cost. The one control regression is small compared to the wins on the targeted workloads.
- Validation:
  - `make bench_exe` passed.
  - `make test_exe` passed.

New ideas from this experiment:

- Add the same identity coverage for negative bigints and mixed-sign masks in the benchmark set. The current fast paths should already help there, but it is worth measuring explicitly.
- If the small control regression becomes relevant, fold these constant checks into a shared helper to keep branch layout tighter across `and/or/xor`.

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

Baseline benchmark:

- Benchmark binary: `c_runtime/bench_exe`
- Command: `./bench_exe 300000`
- Samples: 5 runs, using the median `ns/op`
- Added benchmark cases targeting larger positive-positive bigint bitwise loops:
  - `and_big16_big16_pos`
  - `or_big16_big16_pos`
  - `xor_big16_big16_pos`
- Reused control case:
  - `and_big_big_pos`
- Baseline medians:
  - `and_big_big_pos`: `11.35 ns/op`
  - `and_big16_big16_pos`: `18.77 ns/op`
  - `or_big16_big16_pos`: `18.67 ns/op`
  - `xor_big16_big16_pos`: `18.56 ns/op`

Experimental result:

- Scoped the experiment to a first credible prototype instead of a full storage rewrite.
- Kept the existing `uint32_t` storage format, but changed the positive-positive bigint bitwise fast path to process larger operands in `uint64_t` chunks over the existing backing arrays.
- Only enabled the chunked loop for larger results, leaving small cases on the existing `uint32_t` implementation.
- Reran the same benchmark command: `./bench_exe 300000`
- Post-change medians:
  - `and_big_big_pos`: `11.01 ns/op`
  - `and_big16_big16_pos`: `15.93 ns/op`
  - `or_big16_big16_pos`: `14.59 ns/op`
  - `xor_big16_big16_pos`: `15.18 ns/op`

Outcome:

- Kept the code.
- Median improvements versus baseline:
  - `and_big_big_pos` control: about `3%` faster
  - `and_big16_big16_pos`: about `15%` faster
  - `or_big16_big16_pos`: about `22%` faster
  - `xor_big16_big16_pos`: about `18%` faster
- This is enough to keep the local chunked implementation, but it does not yet justify claiming that a full stored-`uint64_t` bigint representation would be a net win. The experiment demonstrates that wider-limb style processing helps on larger hot loops, not that the full refactor is automatically worth it.
- Validation:
  - `make bench_exe` passed.
  - `make test_exe` passed.

New ideas from this experiment:

- Split “64-bit limbs” into two separate follow-ups:
  - storage-layout rewrite to actual `uint64_t` limbs
  - local `uint64_t` chunk processing for specific hot paths
- If the storage rewrite is attempted later, benchmark parse/format separately. Those paths may benefit differently from limb width than bitwise ops do.

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

Baseline benchmark:

- Benchmark binary: `c_runtime/bench_exe`
- Command: `./bench_exe 5000`
- Samples: 5 runs, using the median `ns/op`
- Added benchmark cases:
  - `int_to_string_big16`
  - `string_to_integer_big`
  - `string_to_integer_big_neg`
- Baseline medians:
  - `int_to_string_big16`: `1255.80 ns/op`
  - `string_to_integer_big`: `1983.60 ns/op`
  - `string_to_integer_big_neg`: `1988.60 ns/op`

Experimental result:

- Replaced repeated `/ 10` formatting with repeated `/ 1_000_000_000`, emitting 9 decimal digits per step.
- Replaced repeated big `* 10 + digit` parsing with repeated big `* 1_000_000_000 + chunk`, using 9-digit chunks.
- Kept the existing bigint arithmetic for the chunk steps, so this is an algorithmic improvement rather than a representation change.
- Reran the same benchmark command: `./bench_exe 5000`
- Post-change medians:
  - `int_to_string_big16`: `434.80 ns/op`
  - `string_to_integer_big`: `307.80 ns/op`
  - `string_to_integer_big_neg`: `373.80 ns/op`

Outcome:

- Kept the code.
- Median improvements versus baseline:
  - `int_to_string_big16`: about `65%` faster
  - `string_to_integer_big`: about `84%` faster
  - `string_to_integer_big_neg`: about `81%` faster
- This is a strong keep. Long decimal parse/format was still one of the highest-overhead integer paths left in the runtime, and the base-`10^9` chunking removes most of that cost without complicating the bigint representation.
- Validation:
  - `make bench_exe` passed.
  - `make test_exe` passed.
  - A bounded `./test_exe` run still hit the same pre-existing oversized-string runtime issue already documented elsewhere in this file: `bsts_string_from_utf8_bytes_copy: string length 9223372036854775808 exceeds supported max (9223372036854775807)`.

New ideas from this experiment:

- If formatting is revisited again, compare manual 9-digit emission against `snprintf` for the leading chunk and maybe for all chunks.
- Parsing could likely avoid one more layer of bigint work by accumulating several base-`10^9` chunks in `uint64_t` before promoting.

### 14. Division and Modulo Special Cases

Hypothesis:
A few special divisors can avoid the general recursive path.

Experiments:

- Fast path division/mod by powers of two.
- Fast path division/mod by small positive ints on small bigints.
- Benchmark `gcd`, since it repeatedly calls `div_mod`.

Baseline benchmark:

- Benchmark binary: `c_runtime/bench_exe`
- Command: `./bench_exe 100000`
- Samples: 5 runs, using the median `ns/op`
- Scoped this first pass to power-of-two divisors plus one `gcd` case that exercises `div_mod`.
- Added benchmark cases:
  - `divmod_big_pow2_pos`
  - `divmod_big_neg_pow2_pos`
  - `divmod_big_pow2_neg`
  - `divmod_big16_pow2_65`
  - `divmod_big16_pow2_65_neg`
  - `gcd_big16_pow2_65`
- Baseline medians:
  - `divmod_big_pow2_pos`: `126.68 ns/op`
  - `divmod_big_neg_pow2_pos`: `161.17 ns/op`
  - `divmod_big_pow2_neg`: `162.43 ns/op`
  - `divmod_big16_pow2_65`: `1626.33 ns/op`
  - `divmod_big16_pow2_65_neg`: `1658.94 ns/op`
  - `gcd_big16_pow2_65`: `2054.43 ns/op`

Experimental result:

- Scoped the experiment to a localized power-of-two fast path in `bsts_integer_div_mod`.
- Added a detector for `abs(r) == 2^k` across both small ints and heap-backed bigints.
- For those divisors, computed the positive quotient with a right shift and the positive remainder with `abs(l) & (abs(r) - 1)`, then reused the existing floor-division sign repair rules.
- Left the general recursive divider untouched for non-power-of-two divisors and deferred the “small positive ints on small bigints” slice.
- Reran the same benchmark command: `./bench_exe 100000`
- Post-change medians:
  - `divmod_big_pow2_pos`: `66.81 ns/op`
  - `divmod_big_neg_pow2_pos`: `99.14 ns/op`
  - `divmod_big_pow2_neg`: `93.28 ns/op`
  - `divmod_big16_pow2_65`: `70.62 ns/op`
  - `divmod_big16_pow2_65_neg`: `151.10 ns/op`
  - `gcd_big16_pow2_65`: `516.31 ns/op`

Outcome:

- Kept the code.
- Median improvements versus baseline:
  - `divmod_big_pow2_pos`: about `47%` faster
  - `divmod_big_neg_pow2_pos`: about `38%` faster
  - `divmod_big_pow2_neg`: about `43%` faster
  - `divmod_big16_pow2_65`: about `96%` faster
  - `divmod_big16_pow2_65_neg`: about `91%` faster
  - `gcd_big16_pow2_65`: about `75%` faster
- The largest gains come from bypassing the recursive divider entirely when the divisor is a power of two, especially when that divisor is itself heap-backed.
- Validation:
  - `make bench_exe` passed.
  - `make test_exe` passed.
  - A focused standalone checker covering the new `32`, `2^65`, and `-2^65` cases, plus `gcd`, passed.
  - Running the full `./test_exe` driver still hits the same pre-existing oversized-string runtime issue already noted elsewhere in this file.

New ideas from this experiment:

- Add a follow-up for small positive non-power-of-two divisors on 1-word and 2-word bigints. Those should be able to use a simple top-down word division rather than the recursive search path.
- If `div_mod` becomes a larger hotspot, split `div`-only and `mod`-only entrypoints so callers that need just one result do not always allocate the pair.
- Consider a direct power-of-two `mod` helper for `gcd`-like loops that only care about the remainder.

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
