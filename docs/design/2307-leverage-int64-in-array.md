---
issue: 2307
priority: 3
touch_paths:
  - docs/design/2307-leverage-int64-in-array.md
  - test_workspace/Bosatsu/Collection/Array.bosatsu
  - test_workspace/core_alpha_conf.json
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - core/src/main/scala/dev/bosatsu/codegen/python/PythonGen.scala
  - c_runtime/bosatsu_ext_Bosatsu_l_Collection_l_Array.c
  - c_runtime/bosatsu_ext_Bosatsu_l_Collection_l_Array.h
  - core/src/test/scala/dev/bosatsu/ArrayInt64Laws.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/python/PythonGenTest.scala
  - c_runtime/test.c
depends_on: []
estimated_size: M
generated_at: 2026-04-06T22:08:01Z
---

# Design: leverage Int64 in Array

_Issue: #2307 (https://github.com/johnynek/bosatsu/issues/2307)_

## Summary

Move Array's hot-path size and index APIs to Int64, add indexed, zipped, and Float64-specialized helpers, keep evaluator/Python/C behavior aligned around a shared visible-view model, and ship the breaking change as core_alpha 8.0.0.

## Context
`Bosatsu/Collection/Array` is a runtime-backed collection whose evaluator, Python, and C implementations already track visible data as `(data, offset, len)` with concrete machine-sized counts. The public API, however, still exposes several hot-path sizes and indices as Bosatsu `Int`, which means code that is otherwise array-local pays arbitrary-precision conversion and range-check costs in tight loops. Issue #2307 asks to move that hot path to `Int64` and to add a small set of Array helpers that keep more work inside runtime loops.

`Bosatsu/Num/Int64` already exists in the repo, so this change should reuse that type rather than invent a second index type. The important design boundary is that `Array` becomes `Int64`-native at the public hot path while the underlying storage model stays the same. This is a breaking `core_alpha` API change and should ship as the next major library release.

## Goals
- Move the performance-sensitive `Array` size and index surface to `Int64`.
- Add indexed, zipped, and Float64-specialized helpers so callers can stay inside runtime loops instead of rebuilding those loops in Bosatsu over `Int`.
- Keep evaluator, Python, and C behavior aligned on visible length, index order, zip truncation, and Float64 reduction order.
- Preserve the existing `Array` storage and view representation and avoid a second round of storage refactors in the same issue.
- Release the change as `core_alpha` `8.0.0`.

## Non-goals
- Do not introduce packed or unboxed numeric arrays in this issue. `Array[a]` remains a generic runtime-backed array of Bosatsu values.
- Do not try to raise the maximum constructible array size above the current concrete runtime cap. The win is avoiding `Int` overhead, not promising arrays beyond current backend storage limits.
- Do not redesign `Bosatsu/Num/Int64`, add new integer literal syntax, or add JSON/doc encoding for raw `Int64`.
- Do not require `ClangGen.scala` changes beyond ordinary external-call lowering; the C work belongs in the Array runtime externals.

## Proposed API
The `Bosatsu/Collection/Array` hot-path surface should become:

- `size_Array: Array[a] -> Int64`
- `tabulate_Array: (Int64, Int64 -> a) -> Array[a]`
- `get_or_Array: (Array[a], Int64, Int64 -> a) -> a`
- `foldl_with_index_Array: (Array[a], b, (b, a, Int64) -> b) -> b`
- `map_with_index_Array: (Array[a], (a, Int64) -> b) -> Array[b]`
- `sumf_Array: Array[Float64] -> Float64`
- `sumsqf_Array: Array[Float64] -> Float64`
- `dotf_Array: (Array[Float64], Array[Float64]) -> Float64`
- `zip_map_Array: (Array[a], Array[b], (a, b) -> c) -> Array[c]`
- `zip_foldl_Array: (Array[a], Array[b], c, (c, a, b) -> c) -> c`
- `zip_sumf_Array: (Array[a], Array[b], (a, b) -> Float64) -> Float64`

This design keeps existing non-indexed helpers such as `foldl_Array`, `foldr_Array`, `map_Array`, `filter_Array`, `flat_map_Array`, `sort_Array`, `concat_all_Array`, and `char_Array_to_String` source-compatible. Pure convenience helpers in `Bosatsu/Collection/Array.bosatsu` that still speak `Int` can remain as compatibility wrappers where that reduces migration churn, but the runtime-backed fast path should be `Int64`-native.

Because exported Array signatures now mention `Bosatsu/Num/Int64::Int64`, `test_workspace/Bosatsu/Collection/Array.bosatsu` should explicitly `exposes Bosatsu/Num/Int64`.

This design assumes the new `zip_*` functions truncate to the shorter visible input, matching the existing `Bosatsu/List::zip` behavior. If an exact-length variant is ever needed, it should be a separately named API.

## Behavioral Properties
The change is correct when these properties hold, regardless of backend:

- `size_Array` returns the visible array length as an exact `Int64`.
- The shared constructible array cap is `Int.MaxValue` on JVM and Python and `INT_MAX` in the C runtime; `tabulate_Array` above that cap returns `empty_Array` on every backend.
- `tabulate_Array(n, fn)` calls `fn` exactly once for each visible index `0 .. n - 1`, in increasing order, and never calls `fn` when `n <= 0` or when `n` is above the shared constructible array cap.
- `get_or_Array(arr, idx, default)` returns the visible element at `idx` when `0 <= idx < size_Array(arr)`. Otherwise it calls `default` exactly once, and it passes the original requested `idx`, not a clamped value.
- `foldl_with_index_Array` visits each visible element exactly once, from left to right, with indices `0 .. size_Array(arr) - 1`.
- `map_with_index_Array` preserves visible length and element order, and the index seen by the mapping function is relative to the visible array view, not the underlying slice offset.
- For sliced arrays, the first visible element always has index `0` for `get_or_Array`, `foldl_with_index_Array`, and `map_with_index_Array`. Internal `offset` is never observable in the public index.
- `zip_map_Array`, `zip_foldl_Array`, `zip_sumf_Array`, and `dotf_Array` consume exactly `min(size_Array(left), size_Array(right))` visible pairs and never step past either visible end.
- `zip_foldl_Array` returns its initial accumulator unchanged when the visible paired prefix has length `0`.
- `sumf_Array`, `sumsqf_Array`, `zip_sumf_Array`, and `dotf_Array` are defined as strict left-to-right Float64 reductions. Implementations must not reorder, tree-reduce, or otherwise change the evaluation order, because that would change rounding and NaN behavior.
- `sumf_Array` and `sumsqf_Array` return `0.0` on empty visible arrays, and `zip_sumf_Array` and `dotf_Array` return `0.0` when the visible paired prefix has length `0`.
- The runtime-backed fast path should not materialize Bosatsu `Int` values except in deliberately retained compatibility wrappers outside the hot path.

## Runtime Architecture
### Library surface
`test_workspace/Bosatsu/Collection/Array.bosatsu` should import `Bosatsu/Num/Int64` and update the exported signatures above. The pure library layer can keep a small `Int`-based compatibility surface where it helps migration, but those wrappers should perform explicit conversions at the package boundary and should point users toward the new `Int64`-native externals for hot loops.

The new Float64 helpers belong in `Bosatsu/Collection/Array`, not `Bosatsu/Num/Float64`, because the optimization target is array traversal rather than scalar arithmetic. They should be runtime-backed externals rather than pure Bosatsu wrappers so the implementation can avoid intermediate arrays, repeated callback dispatch, and `Int` conversion overhead.

### Evaluator
`core/src/main/scala/dev/bosatsu/Predef.scala` should keep `ArrayValue(data: Array[Value], offset: Int, len: Int)` as-is. The evaluator win comes from changing the public Array boundary, not from widening internal storage fields.

Implementation notes:

- `size_Array` should return `PredefImpl.Int64Value(len.toLong)`.
- `tabulate_Array` should accept an `Int64`, reject non-positive or counts above `Int.MaxValue` with `empty_Array`, and use a primitive `Int` loop counter internally.
- `get_or_Array` should range-check the incoming `Int64` against the visible `len`, cast to `Int` only after the check, and call the fallback with the original `Int64` on miss.
- `foldl_with_index_Array` and `map_with_index_Array` should walk the visible view with a primitive `Int` counter and construct `Int64` callback indices only at the point of calling user code.
- `zip_*` helpers should compute `min(left.len, right.len)` once and then loop over visible offsets.
- `sumf_Array`, `sumsqf_Array`, and `dotf_Array` should unbox `Value.VFloat` directly and accumulate in `Double`, preserving the documented left-to-right order.

### Python backend
`core/src/main/scala/dev/bosatsu/codegen/python/PythonGen.scala` already special-cases Array with the internal tuple representation `(data, offset, len)`. That representation should stay unchanged. The right change is to extend those special cases so the public boundary speaks `Int64` while loop counters remain ordinary Python `int`.

Implementation notes:

- Unbox incoming `Int64` values once at the start of `tabulate_Array` and `get_or_Array`.
- Box outgoing sizes and callback indices with the existing Int64 helpers instead of routing Array through a second runtime representation.
- Keep the same shared constructible-length cap as the evaluator and C runtime by rejecting counts above `Int.MaxValue`, so Python does not accidentally accept larger arrays than other backends.
- Add direct Python codegen paths for `foldl_with_index_Array`, `map_with_index_Array`, `zip_map_Array`, `zip_foldl_Array`, `zip_sumf_Array`, `sumf_Array`, `sumsqf_Array`, and `dotf_Array`.

A separate `ProgExt.py` Array implementation should not be introduced unless PythonGen proves unable to reuse the existing Int64 helpers cleanly. The current special-cased Array lowering is already the established architecture.

### C runtime
`c_runtime/bosatsu_ext_Bosatsu_l_Collection_l_Array.c` and `.h` should take the same approach as the evaluator: keep the concrete storage model unchanged and move only the public boundary to `Int64`.

Implementation notes:

- Keep `BSTS_Array { BValue* data; int offset; int len; }`.
- `size_Array` returns `bsts_int64_from_int64(arr->len)`.
- `tabulate_Array` accepts an `Int64` `BValue`, rejects non-positive or counts above `INT_MAX` with `empty_Array`, and passes `bsts_int64_from_int64(idx)` into the callback.
- `get_or_Array` accepts an `Int64` index and calls the fallback with the original `Int64` on miss.
- `foldl_with_index_Array` and `map_with_index_Array` should use the visible array view and produce relative indices from `0`.
- `zip_*` helpers should operate on the shorter visible length and avoid intermediate pair allocations.
- `sumf_Array`, `sumsqf_Array`, and `dotf_Array` should use scalar `double` loops in visible order.

No Array-specific `ClangGen.scala` work is expected. The C backend already lowers library externals by symbol name, and both `Int64` and `Float64` are already first-class runtime values.

## Implementation Plan
1. Update `test_workspace/Bosatsu/Collection/Array.bosatsu` to expose `Bosatsu/Num/Int64`, publish the new Array signatures, and adapt any pure compatibility wrappers or package-local tests.
2. Change `test_workspace/core_alpha_conf.json` from the current `next_version: 7.1.0` plan to `next_version: 8.0.0`.
3. Extend the Array external registry and implementation in `core/src/main/scala/dev/bosatsu/Predef.scala`.
4. Extend the Python Array special cases in `core/src/main/scala/dev/bosatsu/codegen/python/PythonGen.scala`.
5. Update the Array runtime externals in `c_runtime/bosatsu_ext_Bosatsu_l_Collection_l_Array.c` and `.h`.
6. Add Bosatsu property-check coverage for the semantic laws in `test_workspace/Bosatsu/Collection/Array.bosatsu`, plus targeted boundary and codegen tests in the existing Scala and C test suites.
7. Update package-interface and dependency-visibility tests so the `Array` public surface explicitly depends on `Bosatsu/Num/Int64`.

## Testing Strategy
Property-check style tests should carry the semantic contract.

Primary cross-backend law coverage should live in `test_workspace/Bosatsu/Collection/Array.bosatsu` using `Bosatsu/Testing/Properties`, so the same Array laws can run through Bosatsu evaluation, Python transpilation, and C transpilation. That Bosatsu property suite should cover:

- `size_Array(tabulate_Array(n, fn)) == n` for representable `n`.
- `to_List_Array(tabulate_Array(n, fn))` matches the list model with `Int64` indices in ascending order.
- `get_or_Array` hit and miss behavior against a list model, including sliced views.
- `foldl_with_index_Array` and `map_with_index_Array` agreement with a list model over relative visible indices.
- `zip_map_Array` and `zip_foldl_Array` agreement with prefix-of-min-length list models.
- `sumf_Array`, `sumsqf_Array`, `zip_sumf_Array`, and `dotf_Array` agreement with explicit left-to-right models, including sliced inputs.

Case-based tests are still the right fit where exact boundaries, generated code shape, or machine-level diagnostics matter.

A narrower ScalaCheck suite such as `core/src/test/scala/dev/bosatsu/ArrayInt64Laws.scala` is still useful for evaluator-only helper invariants or edge cases that are awkward to express in Bosatsu, but it should complement rather than replace the Bosatsu property suite.

Those should live in:

- `core/src/test/scala/dev/bosatsu/EvaluationTest.scala` for negative sizes, oversized sizes, fallback index forwarding in `get_or_Array`, slice-relative indexing, empty-array float reductions, mismatched zip lengths, and NaN, infinity, and negative-zero cases.
- `core/src/test/scala/dev/bosatsu/codegen/python/PythonGenTest.scala` for generated Python that unboxes `Int64` once at the boundary and boxes `size_Array` results and indexed callbacks correctly.
- `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala` for interface shape, `exposes Bosatsu/Num/Int64`, and downstream package visibility regressions.
- `c_runtime/test.c` for exact C boundary behavior around zero, negative, and oversized lengths, slice-relative indices, and floating-point corner cases.

If the package-local Bosatsu tests in `test_workspace/Bosatsu/Collection/Array.bosatsu` are retained or expanded, they should stay focused on user-facing smoke coverage and wrapper equivalences rather than duplicating the full evaluator law suite.

## Acceptance Criteria
- `Bosatsu/Collection/Array` exports the `Int64`-native hot-path surface described above.
- `Bosatsu/Collection/Array` explicitly exposes `Bosatsu/Num/Int64` so downstream packages can see the new public types.
- `size_Array` returns `Int64`, and `tabulate_Array` and `get_or_Array` consume `Int64`.
- `foldl_with_index_Array`, `map_with_index_Array`, `sumf_Array`, `sumsqf_Array`, `dotf_Array`, `zip_map_Array`, `zip_foldl_Array`, and `zip_sumf_Array` exist and match the documented semantics.
- Public indices on visible slices are relative to the visible view and do not leak internal offsets.
- `zip_*` and `dotf_Array` operate on the shorter visible input length.
- `sumf_Array` and `sumsqf_Array` return `0.0` on empty visible arrays, and `zip_sumf_Array` and `dotf_Array` return `0.0` on zero-pair inputs.
- Float64 reductions are left-to-right and backend-consistent.
- The evaluator, Python backend, and C runtime all use the same constructible-length cap and the same miss and hit behavior for `get_or_Array`.
- Bosatsu property-check coverage exists in `test_workspace/Bosatsu/Collection/Array.bosatsu` for the semantic laws, and case-based coverage exists for boundary values, generated Python shape, package visibility, and C runtime edge cases.
- `test_workspace/core_alpha_conf.json` is updated to `next_version: 8.0.0`.

## Risks
- Migration churn is real because `size_Array` no longer returns `Int`, `tabulate_Array` no longer accepts `Int`, and raw `Int64` still does not participate in every existing JSON and doc path. Mitigation: call out explicit conversion patterns with `int_to_Int64`, `int_low_bits_to_Int64`, and `int64_to_Int`.
- Backend drift is most likely in reduction order, oversized-length handling, and slice-relative indexing. Mitigation: make those behaviors explicit and lock them in property and boundary tests.
- Python codegen may accidentally reintroduce per-iteration conversion overhead if it boxes and unboxes `Int64` inside the loop body instead of at the boundary. Mitigation: add generated-code-shape tests in `PythonGenTest.scala`.
- Performance wins will be meaningful for indexed and specialized helpers, but `Array` still stores generic boxed values. Mitigation: keep expectations focused on removal of `Int` overhead and on the new specialized float loops rather than on a storage-layout speedup.

## Rollout Notes
- Merging this design doc is the `planned` milestone for this lane. Implementation can continue afterward from the same child issue.
- The implementation PR should land as the breaking `core_alpha` `8.0.0` change, not as `7.1.0`.
- Migration notes should tell downstream users to keep hot loops on `Int64` inside Array code and convert back to `Int` only when crossing into APIs that still require it.
- After correctness is locked, validate the motivating zafu or spectral-norm-style workloads to confirm that the new indexed and Float64 helpers remove the intended `Int` overhead.
