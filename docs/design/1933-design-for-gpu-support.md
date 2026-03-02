---
issue: 1933
priority: 3
touch_paths:
  - docs/design/1933-design-for-gpu-support.md
  - test_workspace/Bosatsu/GPU/Error.bosatsu
  - test_workspace/Bosatsu/GPU/Core.bosatsu
  - test_workspace/Bosatsu/GPU/Tensor.bosatsu
  - test_workspace/core_alpha_conf.json
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - test_workspace/Prog.bosatsu_externals
  - test_workspace/ProgExt.py
  - c_runtime/bosatsu_ext_Bosatsu_l_GPU_l_Core.h
  - c_runtime/bosatsu_ext_Bosatsu_l_GPU_l_Core.c
  - c_runtime/bosatsu_ext_Bosatsu_l_GPU_l_Tensor.h
  - c_runtime/bosatsu_ext_Bosatsu_l_GPU_l_Tensor.c
  - c_runtime/bosatsu_gpu_backend.h
  - c_runtime/bosatsu_gpu_backend_cpu.c
  - c_runtime/bosatsu_gpu_backend_metal.m
  - c_runtime/bosatsu_gpu_backend_cuda.cu
  - c_runtime/Makefile
depends_on: []
estimated_size: M
generated_at: 2026-03-02T20:29:10Z
---

# Issue #1933 Design: GPU support

_Issue: #1933 (https://github.com/johnynek/bosatsu/issues/1933)_

## Summary

Architecture and phased implementation plan for a Bosatsu GPU package with CPU fallback and optional Metal/CUDA backends.

---
issue: 1933
title: Design for GPU support
status: proposed
base_branch: main
touch_paths:
  - docs/design/1933-design-for-gpu-support.md
  - test_workspace/Bosatsu/GPU/Error.bosatsu
  - test_workspace/Bosatsu/GPU/Core.bosatsu
  - test_workspace/Bosatsu/GPU/Tensor.bosatsu
  - test_workspace/core_alpha_conf.json
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - test_workspace/Prog.bosatsu_externals
  - test_workspace/ProgExt.py
  - c_runtime/bosatsu_ext_Bosatsu_l_GPU_l_Core.h
  - c_runtime/bosatsu_ext_Bosatsu_l_GPU_l_Core.c
  - c_runtime/bosatsu_ext_Bosatsu_l_GPU_l_Tensor.h
  - c_runtime/bosatsu_ext_Bosatsu_l_GPU_l_Tensor.c
  - c_runtime/bosatsu_gpu_backend.h
  - c_runtime/bosatsu_gpu_backend_cpu.c
  - c_runtime/bosatsu_gpu_backend_metal.m
  - c_runtime/bosatsu_gpu_backend_cuda.cu
  - c_runtime/Makefile
depends_on: []
estimated_size: XL
generated_at: 2026-03-02
---

# Issue #1933 Design: GPU support

Issue: #1933 (https://github.com/johnynek/bosatsu/issues/1933)
Base branch: `main`

## Summary

Add a first GPU package family under `Bosatsu/GPU` with explicit session lifecycle, explicit host-device transfer, rank1-rank4 tensor values for `Int` and `Float64`, and a minimal useful kernel set (`add`, `mul`, reductions, `dot`, `matmul`).
All runtimes must support `Cpu` fallback. Native C runtime adds optional `Metal` (macOS) and `Cuda` backends behind the same external API.

## Problem statement

Bosatsu has `Array`, `Int`, and `Float64` but no accelerator abstraction. Users cannot model:

1. selecting a compute device,
2. loading `Array` data onto a device,
3. running tensor operations there,
4. materializing results back to host values.

Issue #1933 asks for a basic package that covers these flows and can target `Metal`, `Cuda`, and no-GPU hosts.

## Goals

1. Provide one stable Bosatsu API that works on CPU-only and GPU hosts.
2. Support rank1-rank4 tensors for `Int` and `Float64`.
3. Make data movement explicit: upload, compute, download.
4. Include a small operation set that enables real workloads, not only toy examples.
5. Allow optional packed storage for performance while preserving explicit semantics.
6. Keep evaluator and transpiled backends aligned on API and error behavior.

## Non-goals

1. No autodiff, optimizers, or neural-network layer APIs in this issue.
2. No asynchronous stream/event API in v1.
3. No sparse tensors in v1.
4. No guarantee that every op is hardware-accelerated on every backend.
5. No hard requirement for GPU hardware in default CI.

## Proposed package layout

### `Bosatsu/GPU/Error`

Defines typed runtime failures.

API sketch:

    package Bosatsu/GPU/Error
    export GPUError()

    enum GPUError:
      Unsupported(context: String)
      DeviceUnavailable(context: String)
      InvalidArgument(context: String)
      InvalidShape(context: String)
      OutOfMemory(context: String)
      TransferFailed(context: String)
      KernelFailed(context: String)
      Other(context: String, code: Int, message: String)

### `Bosatsu/GPU/Core`

Defines backend selection and session lifecycle.

API sketch:

    package Bosatsu/GPU/Core

    from Bosatsu/Prog import Prog
    from Bosatsu/Collection/Array import Array
    from Bosatsu/GPU/Error import GPUError

    export (
      Backend(),
      PackMode(),
      DeviceInfo(),
      Session,
      list_devices,
      open,
      close,
      active_backend,
    )

    enum Backend:
      Cpu
      Metal
      Cuda

    enum PackMode:
      Exact
      PreferF32
      PreferF16
      PreferI32
      PreferI16
      PreferI8

    struct DeviceInfo(id: Int, backend: Backend, name: String, unified_memory: Bool)

    external struct Session

    external list_devices: Prog[GPUError, Array[DeviceInfo]]
    external def open(preferred: Array[Backend], device_id: Option[Int]) -> Prog[GPUError, Session]
    external def close(session: Session) -> Prog[GPUError, Unit]
    external def active_backend(session: Session) -> Backend

### `Bosatsu/GPU/Tensor`

Defines tensor values and operations. Rank is encoded at type level via separate opaque structs.

API sketch:

    package Bosatsu/GPU/Tensor

    from Bosatsu/Prog import Prog
    from Bosatsu/Collection/Array import Array
    from Bosatsu/GPU/Core import Session, PackMode
    from Bosatsu/GPU/Error import GPUError

    export (
      Tensor1_Int, Tensor2_Int, Tensor3_Int, Tensor4_Int,
      Tensor1_Float64, Tensor2_Float64, Tensor3_Float64, Tensor4_Float64,
      from_Array1_Int, from_Array2_Int, from_Array3_Int, from_Array4_Int,
      from_Array1_Float64, from_Array2_Float64, from_Array3_Float64, from_Array4_Float64,
      to_Array1_Int, to_Array2_Int, to_Array3_Int, to_Array4_Int,
      to_Array1_Float64, to_Array2_Float64, to_Array3_Float64, to_Array4_Float64,
      add_ewise_Float64,
      mul_ewise_Float64,
      sum_all_Float64,
      sum_all_Int,
      dot_1_Float64,
      matmul_2_Float64,
      transpose_2_Float64,
    )

    external struct Tensor1_Int
    external struct Tensor2_Int
    external struct Tensor3_Int
    external struct Tensor4_Int
    external struct Tensor1_Float64
    external struct Tensor2_Float64
    external struct Tensor3_Float64
    external struct Tensor4_Float64

Notes:

1. `from_ArrayN_*` validates shape product equals array length.
2. `PackMode` is explicit per upload call.
3. All tensor ops return `Prog[GPUError, ...]` to model runtime failures.
4. The same API is used for CPU fallback, Metal, and CUDA.

## Minimal useful operations in v1

1. `from_ArrayN_*` and `to_ArrayN_*` for rank1-rank4 transfer.
2. `transpose_2_Float64`.
3. `add_ewise_Float64`.
4. `mul_ewise_Float64`.
5. `sum_all_Float64`.
6. `sum_all_Int`.
7. `dot_1_Float64`.
8. `matmul_2_Float64`.

This set is sufficient for dense scoring and basic model inference patterns. Extra kernels can be added without changing session or transfer semantics.

## Data flow model

1. Program opens a `Session` with ordered backend preference, for example `[Metal, Cuda, Cpu]`.
2. Host arrays are uploaded through `from_ArrayN_*` and may be packed according to `PackMode`.
3. Tensor operations run inside the selected backend.
4. Results are downloaded with `to_ArrayN_*` into Bosatsu `Array`.
5. Program closes the session. Runtime finalizers still free resources if close is missed.

Example usage sketch:

    main = (
      s <- open([Metal, Cuda, Cpu], None).await()
      a <- from_Array2_Float64(s, 2, 3, from_List_Array([1.0, 2.0, 3.0, 4.0, 5.0, 6.0]), Exact).await()
      b <- from_Array2_Float64(s, 3, 2, from_List_Array([7.0, 8.0, 9.0, 10.0, 11.0, 12.0]), Exact).await()
      c <- matmul_2_Float64(s, a, b).await()
      out <- to_Array2_Float64(s, c).await()
      _ <- close(s).await()
      pure(out)
    )

## Backend architecture

### 1) Shared runtime contract

Add a C-side backend vtable in `c_runtime/bosatsu_gpu_backend.h` with operations required by v1:

1. session open and close,
2. allocate and free buffer,
3. host to device copy,
4. device to host copy,
5. elementwise add and mul,
6. sum reduction,
7. dot,
8. matmul,
9. transpose rank2.

`bosatsu_ext_Bosatsu_l_GPU_l_Core.c` and `bosatsu_ext_Bosatsu_l_GPU_l_Tensor.c` call this contract and convert between Bosatsu values and backend buffers.

### 2) CPU backend

`c_runtime/bosatsu_gpu_backend_cpu.c` implements the vtable using host memory and loops.
This backend is always compiled and always available, and it is the reference implementation for semantic tests.

### 3) Metal backend

`c_runtime/bosatsu_gpu_backend_metal.m` implements the same vtable using Metal and MPS where appropriate.
It is compiled only on macOS when frameworks are present. On unavailable hosts it reports `Unsupported` and selection falls through to the next preferred backend.

### 4) CUDA backend

`c_runtime/bosatsu_gpu_backend_cuda.cu` implements the vtable using CUDA runtime and cuBLAS for dot and matmul.
It is compiled only when CUDA toolchain and libraries are present.

### 5) Backend selection

Selection order:

1. explicit environment override via `BOSATSU_GPU_BACKEND` (`cpu`, `metal`, `cuda`, `auto`),
2. otherwise caller-provided `preferred` order from `open`,
3. otherwise `Cpu`.

### 6) Evaluator and Python behavior

1. `Predef.scala` adds `Bosatsu/GPU/*` externals with CPU implementation in `PredefImpl` so `tool eval` and `lib eval` work everywhere.
2. `ProgExt.py` and `Prog.bosatsu_externals` add the same symbols with CPU fallback.
3. Metal and CUDA are C-runtime features for transpiled native binaries in v1.

## Packed representation semantics

1. Logical element types remain `Int` and `Float64`.
2. Packing is opt-in via `PackMode`.
3. `Exact` guarantees no precision loss.
4. `PreferF32` and `PreferF16` may round on upload and download.
5. `PreferI32`, `PreferI16`, and `PreferI8` use low-bit two's-complement narrowing semantics.
6. Backends may ignore unsupported pack requests and return `Unsupported` or fall back to `Exact`; this behavior must be documented and tested.

## Implementation plan

### Phase 1: API and CPU baseline

1. Add `Bosatsu/GPU/Error.bosatsu`, `Bosatsu/GPU/Core.bosatsu`, and `Bosatsu/GPU/Tensor.bosatsu`.
2. Export new packages in `test_workspace/core_alpha_conf.json`.
3. Add evaluator externals and CPU kernel implementation in `core/src/main/scala/dev/bosatsu/Predef.scala`.
4. Add Python externals mapping in `test_workspace/Prog.bosatsu_externals` and CPU implementation in `test_workspace/ProgExt.py`.
5. Add unit tests in `core/src/test/scala/dev/bosatsu/EvaluationTest.scala`.

### Phase 2: C runtime wiring and CPU backend

1. Add `bosatsu_ext_Bosatsu_l_GPU_l_Core.{h,c}` and `bosatsu_ext_Bosatsu_l_GPU_l_Tensor.{h,c}`.
2. Add backend contract header `c_runtime/bosatsu_gpu_backend.h`.
3. Add CPU implementation `c_runtime/bosatsu_gpu_backend_cpu.c`.
4. Wire build and install in `c_runtime/Makefile`.
5. Add command-level coverage in `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala` forcing CPU backend.

### Phase 3: Metal and CUDA backends

1. Add `c_runtime/bosatsu_gpu_backend_metal.m` with conditional compile on macOS.
2. Add `c_runtime/bosatsu_gpu_backend_cuda.cu` with conditional compile when CUDA is available.
3. Extend `Makefile` for optional backend compilation and linker flags.
4. Add backend-specific conformance tests that run only when backend is available.

### Phase 4: conformance and docs

1. Add shared numerical conformance fixtures comparing backend results to CPU reference.
2. Document backend selection and packing behavior in package docs.
3. Add release notes describing fallback behavior and experimental backend status.

## Testing strategy

1. API tests: typecheck and evaluate upload -> compute -> download for rank1-rank4.
2. CPU reference tests: exact match for `Int`; tolerance-based match for packed `Float64`.
3. Cross-backend conformance: same inputs on CPU vs Metal/CUDA within fixed tolerances.
4. Failure-path tests: invalid shape, unavailable backend, closed session use, OOM error mapping.
5. Resource tests: repeated open/upload/compute/close loops to catch leaks.

## Acceptance criteria

1. `Bosatsu/GPU/Error`, `Bosatsu/GPU/Core`, and `Bosatsu/GPU/Tensor` compile and are exported by `core_alpha`.
2. Programs can open a session with ordered preferences and get deterministic fallback to `Cpu` when accelerators are unavailable.
3. Rank1-rank4 upload and download round trips work for both `Int` and `Float64`.
4. `dot_1_Float64` and `matmul_2_Float64` are available and validated against CPU reference outputs.
5. Elementwise and reduction kernels (`add_ewise_Float64`, `mul_ewise_Float64`, `sum_all_*`) are implemented.
6. `PackMode.Exact` preserves exact values; packed modes follow documented conversion semantics.
7. `tool eval` and `lib eval` can execute GPU package programs via CPU fallback.
8. Python transpilation path can execute the same API via CPU fallback.
9. C runtime builds and installs successfully on machines without GPU SDKs (CPU backend only).
10. On macOS with Metal available, selecting `Metal` succeeds for supported kernels.
11. On CUDA hosts with required libraries, selecting `Cuda` succeeds for supported kernels.
12. Unsupported backend requests and runtime failures return typed `GPUError` values instead of crashes.

## Risks and mitigations

1. Risk: backend semantic drift across CPU, Metal, and CUDA.
Mitigation: CPU backend as canonical reference plus shared conformance tests.

2. Risk: precision surprises from packed storage.
Mitigation: explicit `PackMode`, documented conversions, and tolerance-based tests.

3. Risk: build complexity from optional Objective-C and CUDA toolchains.
Mitigation: keep CPU always-on; compile GPU backends conditionally and non-fatally.

4. Risk: resource lifetime bugs for device buffers.
Mitigation: explicit `close` API plus finalizer-based cleanup and leak stress tests.

5. Risk: limited CI coverage for GPU hardware.
Mitigation: require CPU conformance in default CI; run Metal/CUDA suites in optional or scheduled jobs.

## Rollout notes

1. Ship in stages: API plus CPU first, then Metal, then CUDA.
2. Keep all additions backward-compatible and additive to existing packages.
3. Default behavior remains safe on non-GPU hosts by selecting `Cpu`.
4. Mark Metal and CUDA as experimental until conformance and stability data are collected.
5. Publish a short migration note with examples for upload, compute, and download flow plus backend selection controls.
