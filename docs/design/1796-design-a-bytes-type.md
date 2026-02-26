---
issue: 1796
priority: 3
touch_paths:
  - docs/design/1796-design-a-bytes-type.md
  - test_workspace/Bosatsu/IO/Bytes.bosatsu
  - test_workspace/Bosatsu/IO/Core.bosatsu
  - test_workspace/Bosatsu/IO/Std.bosatsu
  - test_workspace/core_alpha_conf.json
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - test_workspace/ProgExt.py
  - test_workspace/Prog.bosatsu_externals
  - c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Bytes.c
  - c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Bytes.h
  - c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c
  - c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.h
  - c_runtime/Makefile
  - core/src/main/scala/dev/bosatsu/ValueToJson.scala
  - core/src/main/scala/dev/bosatsu/ValueToDoc.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-02-25T17:58:22Z
---

# Issue #1796 design: Bytes type and binary IO

_Issue: #1796 (https://github.com/johnynek/bosatsu/issues/1796)_

## Summary

Full design doc content for docs/design/1796-design-a-bytes-type.md, including API, runtime architecture, implementation plan, acceptance criteria, risks, and rollout notes.

---
issue: 1796
priority: 2
touch_paths:
  - docs/design/1796-design-a-bytes-type.md
  - test_workspace/Bosatsu/IO/Bytes.bosatsu
  - test_workspace/Bosatsu/IO/Core.bosatsu
  - test_workspace/Bosatsu/IO/Std.bosatsu
  - test_workspace/core_alpha_conf.json
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - test_workspace/ProgExt.py
  - test_workspace/Prog.bosatsu_externals
  - c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Bytes.c
  - c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Bytes.h
  - c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c
  - c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.h
  - c_runtime/Makefile
  - core/src/main/scala/dev/bosatsu/ValueToJson.scala
  - core/src/main/scala/dev/bosatsu/ValueToDoc.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
depends_on: []
estimated_size: L
generated_at: 2026-02-25T00:00:00Z
---

# Issue #1796 Design: Bytes Type and Binary IO APIs

Issue: #1796 (https://github.com/johnynek/bosatsu/issues/1796)
Base branch: `main`
Status: proposed

## Summary

Add an immutable external `Bytes` type in `Bosatsu/IO` with `Array`-like access APIs, Python `bytes`/`memoryview`-inspired slicing semantics, and new `Bosatsu/IO/Core` binary handle APIs (`read_bytes`, `write_bytes`) plus larger external operations (`read_all_bytes`, `copy_bytes`) so expensive mutable loops stay inside runtime boundaries.

## Problem statement

Bosatsu currently has:

1. `Array[a]`, which is parametric and represented in evaluator/runtime as arrays of Bosatsu `Value`.
2. `read_utf8`/`write_utf8` in `Bosatsu/IO/Core`, but no first-class binary byte container.

That blocks:

1. Efficient binary file and process-pipe workflows.
2. Zero-copy slice/view behavior over byte data.
3. A stable immutable type for bytes that can be represented as `(pointer, offset, length)`.

Issue #1796 asks for:

1. A non-parametric external `Bytes` type, semantically like `Array[Byte]`.
2. API shape consistent with Bosatsu `List`/`Array` style.
3. Required conversion points, especially `from_List_Int(ints: List[Int]): Bytes` and conversion to `Array[Int]`.
4. Binary `Handle` APIs in `Bosatsu/IO/Core`.
5. A design that keeps mutability internal to externals and keeps Bosatsu APIs pure/immutable.

## Goals

1. Introduce immutable `Bosatsu/IO/Bytes::Bytes` with runtime view representation `(data, offset, length)`.
2. Define API consistent with `Bosatsu/Collection/Array` naming and behavior.
3. Guarantee byte values are exposed as `Int` in range `0..255`.
4. Define `from_List_Int` low-8-bit two's-complement semantics.
5. Add binary IO functions in `Bosatsu/IO/Core` using existing `Handle`.
6. Keep runtime-intensive loops inside external functions where mutability is safe and hidden.
7. Keep Python `bytes`/`memoryview` similarity where it does not conflict with Bosatsu style priorities.
8. Support `Bytes` in `ValueToJson` and `ValueToDoc` as lists of ints in `0..255`.

## Non-goals

1. Do not introduce a new Bosatsu primitive `Byte` scalar type.
2. Do not expose mutable byte arrays.
3. Do not redesign `Prog` or `IOError`.
4. Do not add full Python `bytes` surface area in this issue (encoding/decoding batteries, split/join variants, etc.).
5. Do not require users to reason about aliasing; aliasing must remain an internal optimization only.

## Package placement decision

Place the type in `Bosatsu/IO/Bytes`.

Rationale:

1. The primary use case is binary IO and process streams.
2. Keeping it in `IO` makes discovery straightforward next to `Bosatsu/IO/Core`.
3. It avoids implying that `Bytes` is a generic collection replacement for `Array[a]`.

## Proposed API

### `Bosatsu/IO/Bytes`

Proposed module shape:

```bosatsu
package Bosatsu/IO/Bytes

from Bosatsu/Collection/Array import Array, from_List_Array, to_List_Array

export (
  Bytes,
  empty_Bytes,
  from_List_Int,
  from_Array_Int,
  to_List_Int,
  to_Array_Int,
  size_Bytes,
  get_map_Bytes,
  get_or_Bytes,
  get_Bytes,
  foldl_Bytes,
  concat_all_Bytes,
  slice_Bytes,
  starts_with_Bytes,
  ends_with_Bytes,
  find_Bytes,
  index_in_range_Bytes,
)

external struct Bytes

external empty_Bytes: Bytes
external def from_List_Int(ints: List[Int]) -> Bytes
external def from_Array_Int(ints: Array[Int]) -> Bytes
external def to_List_Int(bytes: Bytes) -> List[Int]
external def to_Array_Int(bytes: Bytes) -> Array[Int]
external def size_Bytes(bytes: Bytes) -> Int
external def get_map_Bytes[a](bytes: Bytes, idx: Int, default: Unit -> a, fn: Int -> a) -> a
external def get_or_Bytes(bytes: Bytes, idx: Int, default: Unit -> Int) -> Int
external def foldl_Bytes[a](bytes: Bytes, init: a, fn: (a, Int) -> a) -> a
external def concat_all_Bytes(chunks: List[Bytes]) -> Bytes
external def slice_Bytes(bytes: Bytes, start: Int, end: Int) -> Bytes
external def starts_with_Bytes(bytes: Bytes, prefix: Bytes) -> Bool
external def ends_with_Bytes(bytes: Bytes, suffix: Bytes) -> Bool
external def find_Bytes(bytes: Bytes, needle: Bytes, start: Int) -> Int

def get_Bytes(bytes: Bytes, idx: Int) -> Option[Int]:
  get_map_Bytes(bytes, idx, _ -> None, i -> Some(i))

def index_in_range_Bytes(bytes: Bytes, idx: Int) -> Bool:
  if cmp_Int(idx, 0) matches LT:
    False
  else:
    cmp_Int(idx, size_Bytes(bytes)) matches LT
```

### Notes on API shape

1. Names and signatures follow `Array` patterns (`size_*`, `get_map_*`, `foldl_*`, `slice_*`, `concat_all_*`).
2. `from_List_Int` is required by the issue.
3. `to_Array_Int` is required by the issue.
4. `from_Array_Int` is included for performance symmetry and to avoid forced list round-trips.
5. `starts_with_Bytes`, `ends_with_Bytes`, `find_Bytes` are the Python-inspired operations in v1 that remain easy to optimize natively.

## Required semantics

### Byte normalization

For each incoming `Int` value `x` in `from_List_Int` and `from_Array_Int`:

1. Take the low 8 bits of `x` under two's-complement semantics.
2. Expose stored byte as unsigned `Int` in `0..255`.

Normative formula:

1. `normalize_byte(x) = mod_Int(x, 256)`.

Examples:

1. `normalize_byte(0) = 0`
2. `normalize_byte(255) = 255`
3. `normalize_byte(256) = 0`
4. `normalize_byte(-1) = 255`
5. `normalize_byte(-257) = 255`

### Indexing and slicing

1. `get_map_Bytes` and `get_or_Bytes` use out-of-range defaults, matching `Array` behavior.
2. `slice_Bytes` follows current `Array.slice_Array` style:
   1. clamp `start` to at least `0`
   2. clamp `end` to at most `size`
   3. return empty when the clamped interval is invalid or empty
3. `slice_Bytes` is an O(1) view operation in runtime representation.

### Search helpers

1. `find_Bytes(bytes, needle, start)` returns first index or `-1`.
2. Empty `needle` returns the clamped `start` index.
3. `starts_with_Bytes` and `ends_with_Bytes` are exact byte-sequence comparisons.

## Runtime architecture

## Shared model

Each runtime represents bytes as immutable view metadata:

1. `data`: contiguous byte storage
2. `offset`: start index
3. `length`: visible length

Operations:

1. `slice` adjusts `(offset, length)` only.
2. `concat` allocates once and copies.
3. `from_*` APIs allocate once and normalize inputs.
4. `to_*` APIs expose unsigned ints `0..255`.

Immutability rule:

1. Any mutable buffers used internally must not be observable after external function returns.
2. Returned `Bytes` values are never mutated in place.

## JVM evaluator (`Predef.scala`)

1. Add `BytesValue(data: Array[Byte], offset: Int, len: Int)` in `PredefImpl`.
2. Register `Bosatsu/IO/Bytes` externals in `Predef.jvmExternals`.
3. Implement helpers:
   1. `asBytes`
   2. `copyBytesView`
   3. low-8-bit normalization from `BigInteger`
4. Implement bytes APIs with imperative loops for speed.
5. Add binary handle methods in `IO/Core` externals:
   1. `read_bytes`
   2. `write_bytes`
   3. `read_all_bytes`
   4. `copy_bytes`
6. Keep `read_utf8`/`write_utf8` API source-compatible.

## Python runtime (`ProgExt.py`)

1. Add `_BosatsuBytes` wrapper (`data: bytes`, `offset`, `length`) or equivalent immutable tuple+helpers.
2. Implement bytes externals with `bytes` plus `memoryview` slices to avoid extra copies.
3. Map package symbols in `Prog.bosatsu_externals` under `Bosatsu/IO/Bytes`.
4. Add `read_bytes`/`write_bytes`/`read_all_bytes`/`copy_bytes` in IO core externals.

## C runtime (`c_runtime`)

1. Add new runtime extension pair:
   1. `bosatsu_ext_Bosatsu_l_IO_l_Bytes.c`
   2. `bosatsu_ext_Bosatsu_l_IO_l_Bytes.h`
2. Representation: `uint8_t* data`, `int offset`, `int len`.
3. Implement copying and comparisons with `memcpy`/`memcmp` where possible.
4. Implement low-8-bit normalization via integer low bits and unsigned cast.
5. Wire build/install via `c_runtime/Makefile`.
6. Extend IO core extension headers/impl for binary handle functions.

## `Bosatsu/IO/Core` binary API proposal

Add to `Bosatsu/IO/Core`:

```bosatsu
from Bosatsu/IO/Bytes import Bytes

external def read_bytes(h: Handle, max_bytes: Int) -> Prog[IOError, Option[Bytes]]
external def write_bytes(h: Handle, bytes: Bytes) -> Prog[IOError, Unit]
external def read_all_bytes(h: Handle, chunk_size: Int) -> Prog[IOError, Bytes]
external def copy_bytes(src: Handle, dst: Handle, chunk_size: Int, max_total: Option[Int]) -> Prog[IOError, Int]
```

Semantics:

1. `read_bytes`:
   1. `max_bytes <= 0` -> `InvalidArgument`
   2. returns `None` only at EOF before reading any bytes
   3. otherwise returns `Some(chunk)` with `1..max_bytes` bytes
2. `write_bytes` writes the full provided view or returns error.
3. `read_all_bytes` reads to EOF and returns a single `Bytes` value.
4. `copy_bytes` copies bytes from `src` to `dst` and returns transferred byte count:
   1. `max_total = None` copies until EOF
   2. `max_total = Some(n)` copies at most `n` bytes total
   3. `max_total = Some(n)` with `n < 0` -> `InvalidArgument`
   4. `max_total = Some(0)` returns `0` without reading or writing

Why include `read_all_bytes` and bounded `copy_bytes` as externals:

1. They keep high-volume mutable loops on the runtime side.
2. They avoid repeated Bosatsu-level recursion and chunk list accumulation.
3. They reduce allocation and FFI crossing overhead for common binary workflows.

## Python `bytes`/`memoryview` alignment strategy

Priority order from the issue:

1. immutability is mandatory
2. Bosatsu style consistency is mandatory
3. Python API similarity is desirable

Applied choices:

1. Keep immutable sequence semantics like Python `bytes`.
2. Keep no-copy slice/view implementation strategy like `memoryview` internally.
3. Keep Bosatsu naming and default/Option access style rather than Python exceptions/negative-index APIs.
4. Include Python-like sequence helpers (`find`, `starts_with`, `ends_with`) where they fit Bosatsu style.

## External boundary design: making externals "big enough"

The main performance rule is to avoid per-byte Bosatsu/external ping-pong for common workloads.

Functions that should remain external for this reason:

1. `from_List_Int` and `from_Array_Int`
2. `to_List_Int` and `to_Array_Int`
3. `concat_all_Bytes`
4. `find_Bytes`, `starts_with_Bytes`, `ends_with_Bytes`
5. `read_bytes`, `write_bytes`, `read_all_bytes`, `copy_bytes`

Functions that can remain Bosatsu wrappers:

1. `get_Bytes`
2. `index_in_range_Bytes`

This split keeps the API pure while still leveraging runtime mutability where it matters.

## Implementation plan

1. Add `test_workspace/Bosatsu/IO/Bytes.bosatsu` with external declarations and thin wrappers.
2. Export `Bosatsu/IO/Bytes` in `test_workspace/core_alpha_conf.json`.
3. Add `Bosatsu/IO/Core` binary function signatures and exports.
4. Implement JVM evaluator support in `Predef.scala`:
   1. new bytes representation
   2. external registration
   3. bytes functions
   4. core binary IO externals
5. Implement Python runtime support in `ProgExt.py` and `Prog.bosatsu_externals`.
6. Implement C runtime support with new `IO/Bytes` extension files and Makefile wiring.
7. Add tests for bytes semantics and binary IO behavior.
8. Add `ValueToJson`/`ValueToDoc` support for `Bytes`, rendered as arrays/lists of ints in `0..255`.

## Testing strategy

### Deterministic `Bytes` tests (`test_workspace/Bosatsu/IO/Bytes.bosatsu`)

1. `from_List_Int` normalization cases: `[-1, 0, 1, 255, 256, 257, -257]`.
2. Round-trip `from_List_Int -> to_List_Int` matches normalized values.
3. `to_Array_Int` values always in range `0..255`.
4. `slice_Bytes` clamping and empty-range cases match `Array.slice_Array` contract.
5. `starts_with_Bytes`, `ends_with_Bytes`, `find_Bytes` cases including empty needle.
6. `concat_all_Bytes` correctness and empty input behavior.

### Evaluator/runtime tests

1. Add JVM evaluator test in `EvaluationTest.scala` similar to existing array external test.
2. Add binary file round-trip tests (`write_bytes` then `read_all_bytes`).
3. Add partial read tests for `read_bytes` chunking.
4. Add `copy_bytes` transfer count tests for both unbounded (`None`) and bounded (`Some(n)`) modes.

### Tooling conversion tests

1. Add `ToolAndLibCommandTest` coverage for `Bytes` JSON write/apply, asserting `Bytes` renders as JSON arrays of ints in `0..255`.
2. Assert invalid JSON ints outside `0..255` are rejected when decoding to `Bytes`.

### Cross-runtime parity

1. JVM evaluator and transpiled Python should agree on normalization and search semantics.
2. C runtime should match the same API-level behavior for normalization, slicing, and IO error handling.

## Acceptance criteria

1. New package `Bosatsu/IO/Bytes` exists and is exported in `core_alpha` config.
2. `Bytes` is non-parametric and external.
3. Runtime representation supports shared `(data, offset, length)` view semantics.
4. `from_List_Int` exists and normalizes via low 8 bits two's-complement semantics.
5. `to_Array_Int` exists and returns unsigned `Int` values in `0..255`.
6. `slice_Bytes` is O(1) view creation and follows Array-style bounds behavior.
7. `Bosatsu/IO/Core` exports `read_bytes` and `write_bytes` using `Handle`.
8. `Bosatsu/IO/Core` also exports `read_all_bytes` and bounded `copy_bytes(src, dst, chunk_size, max_total)` as large-grain external operations.
9. JVM evaluator, Python runtime, and C runtime all implement `Bosatsu/IO/Bytes` externals.
10. Binary read/write tests pass for at least JVM evaluator and Python transpile path; C runtime tests pass where currently available in CI.
11. Existing text IO APIs remain source-compatible.
12. No API exposes mutable byte storage to Bosatsu code.
13. `ValueToJson` and `ValueToDoc` support `Bytes` and render as lists of ints in `0..255`.

## Risks and mitigations

1. Risk: runtime semantic drift (especially around normalization and search edge cases).
   Mitigation: shared spec in this doc plus identical cross-runtime test vectors.

2. Risk: accidental mutability exposure through shared buffers.
   Mitigation: treat all returned `Bytes` as immutable, never expose mutable backing, copy when ingesting mutable sources.

3. Risk: large-file memory pressure with `read_all_bytes`.
   Mitigation: keep chunked `read_bytes` and document preferred streaming usage for large inputs.

4. Risk: overhead from Bosatsu callback-driven folds on very large data.
   Mitigation: keep high-throughput operations (`concat`, `find`, `copy`, `read_all`) external and loop-native.

5. Risk: incompatibility in JSON/doc tooling for new external type.
   Mitigation: include `ValueToJson`/`ValueToDoc` updates in the same rollout and lock behavior with explicit tooling tests.

6. Risk: behavior differences when mixing text and binary reads/writes on the same handle.
   Mitigation: document that mixed-mode use is undefined in v1 and recommend one mode per handle lifecycle.

## Rollout notes

1. Land `Bosatsu/IO/Bytes` type and pure/container APIs first.
2. Land `Bosatsu/IO/Core` binary APIs next with JVM/Python/C runtime implementations.
3. Land tests and parity checks in the same PR series before release tagging.
4. Update docs/examples to show binary file read/write and conversion to `Array[Int]`.
5. Release as additive API in `core_alpha` with no breaking source changes for existing modules.

## Follow-up ideas (out of scope)

1. UTF-8 encode/decode helpers in `Bosatsu/IO/Bytes`.
2. Additional Python-like operations (`count`, `replace`, `split`) where performance can stay external.
3. Stream combinators in `Bosatsu/IO/Std` built on `read_bytes` for chunk pipelines.
4. Optional `Bosatsu/Collection/Bytes` re-export if discoverability becomes a concern.
