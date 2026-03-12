---
issue: 815
priority: 2
title: protobuf value generator
status: proposed
base_branch: main
touch_paths:
  - docs/design/815-protobuf-value-generator.md
  - build.sbt
  - project/Dependencies.scala
  - protoc-gen-bosatsu
  - cli/src/main/scala/dev/bosatsu/protobuf/ProtocPluginMain.scala
  - core/src/main/scala/dev/bosatsu/protobuf/ProtoDescriptorModel.scala
  - core/src/main/scala/dev/bosatsu/protobuf/ProtoNaming.scala
  - core/src/main/scala/dev/bosatsu/protobuf/ProtoToBosatsu.scala
  - test_workspace/Bosatsu/Proto/Wire.bosatsu
  - test_workspace/core_alpha_conf.json
  - test_workspace/Float64.bosatsu
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - test_workspace/ProgExt.py
  - test_workspace/Prog.bosatsu_externals
  - c_runtime/bosatsu_ext_Bosatsu_l_Num_l_Float64.c
  - c_runtime/bosatsu_ext_Bosatsu_l_Num_l_Float64.h
  - c_runtime/Makefile
  - cli/src/test/scala/dev/bosatsu/protobuf/ProtocPluginMainTest.scala
  - core/src/test/scala/dev/bosatsu/protobuf/ProtoToBosatsuTest.scala
  - core/src/test/scala/dev/bosatsu/protobuf/ProtoWireEncodingParityTest.scala
  - core/src/test/resources/protobuf/config_v1.proto
  - core/src/test/scala/dev/bosatsu/codegen/python/PythonGenTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenTest.scala
  - docs/src/main/paradox/generating_protobuf.md
  - docs/src/main/paradox/index.md
  - test_workspace/Bosatsu/Example/Proto/ConfigDemo.bosatsu
depends_on: []
estimated_size: L
generated_at: 2026-03-11
---

# Issue #815 Design: protobuf value generator

Issue: #815 (https://github.com/johnynek/bosatsu/issues/815)
Base branch: `main`
Status: proposed

## Summary

Add a standard `protoc` plugin (`protoc-gen-bosatsu`) that generates Bosatsu protobuf models and generated protobuf codecs:

1. `encode_<Message>: <Message> -> Bosatsu/IO/Bytes::Bytes`
2. `decode_<Message>: Bosatsu/IO/Bytes::Bytes -> Option[<Message>]`

The wire implementation is in Bosatsu (`Bosatsu/Proto/Wire`) and uses existing runtime primitives, so generated serialization/deserialization works on JVM evaluator, Python transpilation, and C transpilation without linking host protobuf libraries.

## Problem statement

Bosatsu does not currently provide a protobuf equivalent of JSON generation:

1. No generator from `.proto` to Bosatsu `struct`/`enum` value models.
2. No portable protobuf wire codec from Bosatsu values to bytes.
3. No generated decode path back from bytes to typed Bosatsu values.
4. No documented flow for using Bosatsu as protobuf config/payload authoring source.

Issue #815 asks for a pragmatic design that starts from protobuf IDL and enables Bosatsu to produce protobuf payloads (and consume them) across JVM/Python/C.

## Goals

1. Implement `protoc-gen-bosatsu` with standard stdin/stdout plugin protocol.
2. Generate Bosatsu models for protobuf messages/enums/oneofs.
3. Generate `encode_` and `decode_` functions for messages in phase 1.
4. Keep codec runtime implementation inside Bosatsu + existing Bosatsu runtime primitives.
5. Avoid runtime dependency on C/JVM/Python protobuf libraries for generated codecs.
6. Make generation deterministic (stable output for identical descriptor input).
7. Validate codec correctness against reference protobuf behavior.
8. Document end-to-end usage.

## Non-goals

1. gRPC service/client/server stub generation.
2. proto2 support in this issue.
3. Unknown-field preservation (unknown fields are skipped while decoding).
4. Protobuf reflection API generation beyond what is needed for generated codecs.

## Proposed architecture

### 1) `protoc-gen-bosatsu` plugin entrypoint

Add `cli/src/main/scala/dev/bosatsu/protobuf/ProtocPluginMain.scala`:

1. Read `CodeGeneratorRequest` from stdin.
2. Convert descriptors to internal normalized model.
3. Generate Bosatsu source for each `file_to_generate`.
4. Return generated files in `CodeGeneratorResponse`.
5. Return descriptor/feature validation errors through `CodeGeneratorResponse.error`.

Repository wrapper script `protoc-gen-bosatsu` invokes the plugin class from the CLI assembly jar so standard protoc plugin discovery works.

### 2) Normalized descriptor model and generation pipeline

Add generator infrastructure in core:

1. `ProtoDescriptorModel.scala`: normalized ADTs (`ProtoFileModel`, `MessageModel`, `EnumModel`, `FieldModel`, etc.).
2. `ProtoNaming.scala`: deterministic package/type/field naming and collision handling.
3. `ProtoToBosatsu.scala`: conversion from normalized model to Bosatsu source using existing `Package.Parsed` + `Statement` rendering.

Generation rules:

1. One Bosatsu package per protobuf file.
2. Default package mapping: `Proto/<ProtoPackageCapitalizedParts>`.
3. `message` -> Bosatsu `struct`.
4. `enum` -> Bosatsu `enum` with `Unknown(value: Int)` constructor.
5. `oneof` -> generated Bosatsu enum field with explicit `NotSet` case.
6. `repeated` -> `List[T]`.
7. `map<K,V>` -> `List[(K, V)]` in phase 1 (preserves wire order and avoids dict-order assumptions).
8. generated message codec surface:
9. `encode_<Message>(value: <Message>) -> Bosatsu/IO/Bytes::Bytes`
10. `decode_<Message>(bytes: Bosatsu/IO/Bytes::Bytes) -> Option[<Message>]`

### 3) `float` and `double` strategy (phase 1)

We use Bosatsu `Float64` for both protobuf `float` and protobuf `double` fields.

Encoding:

1. protobuf `double`: existing `Float64` bit path to IEEE754 binary64.
2. protobuf `float`: convert `Float64` to IEEE754 binary32 bits, then write fixed32.

Decoding:

1. protobuf `double`: read binary64 -> `Float64`.
2. protobuf `float`: read binary32, widen to `Float64`.

To make this explicit and portable, phase 1 adds float32 conversion externals in Bosatsu float runtime surfaces (`Float64.bosatsu`, `Predef.scala`, Python externals, C float64 runtime extension files). This is a small runtime addition, not a protobuf library dependency.

### 4) Wire codec runtime package in Bosatsu

Add `test_workspace/Bosatsu/Proto/Wire.bosatsu` and export it from `core_alpha_conf.json`.

`Bosatsu/Proto/Wire` provides shared codec helpers used by generated code:

1. varint encode/decode
2. zigzag encode/decode
3. fixed32/fixed64 read/write
4. length-delimited read/write
5. key/tag encoding and decoding
6. packed repeated handling
7. unknown field skipping
8. message-frame decode combinators

Important implementation decision:

1. We do not link `libprotobuf`/`protobuf-c` for C runtime execution.
2. We do not require protobuf runtime libraries in Python or JVM evaluator paths for generated codec execution.
3. Generated codecs run on Bosatsu runtime primitives plus this Bosatsu wire module.

Reference protobuf libraries are used only in tests to validate byte-level parity.

### 5) Binary serialization workflow

No new CLI command is required for phase 1.

1. Generate Bosatsu code from `.proto`.
2. Construct typed message values in Bosatsu.
3. Use generated `encode_` to produce `Bytes`.
4. Use existing `Bosatsu/IO/Core.write_bytes` in `Prog` when emitting files/stdout/network payloads.
5. Use generated `decode_` when reading protobuf bytes back in.

This keeps implementation minimal while preserving portability across JVM/Python/C.

## Detailed implementation plan

1. Add plugin and test dependencies in `build.sbt`/`project/Dependencies.scala`.
2. Implement normalized descriptor ADTs in `ProtoDescriptorModel.scala`.
3. Implement naming/collision logic in `ProtoNaming.scala`.
4. Implement source generation in `ProtoToBosatsu.scala` including `encode_` and `decode_` emission.
5. Add plugin entrypoint `ProtocPluginMain.scala`.
6. Add repository wrapper script `protoc-gen-bosatsu`.
7. Add `Bosatsu/Proto/Wire.bosatsu` codec helpers.
8. Export `Bosatsu/Proto/Wire` in `test_workspace/core_alpha_conf.json`.
9. Add float32 conversion externals in:
10. `test_workspace/Float64.bosatsu`
11. `core/src/main/scala/dev/bosatsu/Predef.scala`
12. `test_workspace/ProgExt.py`
13. `test_workspace/Prog.bosatsu_externals`
14. `c_runtime/bosatsu_ext_Bosatsu_l_Num_l_Float64.c/.h`
15. Add fixture schema under `core/src/test/resources/protobuf/config_v1.proto`.
16. Add golden generation tests (`ProtoToBosatsuTest.scala`).
17. Add plugin protocol tests (`ProtocPluginMainTest.scala`).
18. Add wire parity tests against reference protobuf (`ProtoWireEncodingParityTest.scala`).
19. Add cross-runtime parity tests in Python/C codegen tests.
20. Add docs page and example (`generating_protobuf.md`, docs index, `ConfigDemo.bosatsu`).

## Testing strategy

### A. Plugin protocol tests

1. Build synthetic `CodeGeneratorRequest` inputs and verify `CodeGeneratorResponse` outputs.
2. Verify deterministic file list/order/content.
3. Verify clear error responses for unsupported schemas (proto2, extensions, groups).

### B. Golden generation tests

1. Snapshot generated Bosatsu for representative schemas.
2. Verify stable naming/collision handling.
3. Verify generated imports for cross-file references.

### C. JVM wire parity tests

1. Encode generated Bosatsu message values and compare bytes to Java protobuf serialization.
2. Decode Java-produced bytes using generated `decode_` and assert typed equality.
3. Validate oneof behavior, packed fields, optional presence, map/repeated behavior, enum unknown values.

### D. Cross-runtime parity tests

1. Run the same encode/decode fixture values in JVM evaluator, Python transpilation, and C transpilation.
2. Assert identical byte output.
3. Assert decoded values are equal where fixture uses supported features.

### E. Float-specific tests

1. Validate protobuf `float` and `double` parity against Java reference bytes.
2. Validate roundtrip decode for finite values and edge values (`+/-0`, infinities, NaN payload behavior expectations).

## Acceptance criteria

1. `protoc-gen-bosatsu` works with standard protoc plugin invocation.
2. Generated Bosatsu model files compile and typecheck.
3. Generated message APIs include both `encode_` and `decode_` functions.
4. For supported protobuf features, `encode_` output is byte-identical to reference protobuf serialization in tests.
5. Generated `decode_` returns `Some(message)` for valid payloads and `None` for invalid payloads.
6. `float` and `double` are both supported in phase 1 via Bosatsu `Float64` representation.
7. No C protobuf library link is required for generated codec execution.
8. Python runtime and JVM evaluator do not require protobuf libs for generated codec execution.
9. Cross-runtime parity tests (JVM/Python/C) pass for encode/decode fixtures.
10. Generation output is deterministic across repeated runs.
11. Docs describe protoc generation and runtime usage.

## Risks and mitigations

1. Risk: semantic mismatches with protobuf defaults/presence/oneof behavior.
   Mitigation: reference parity tests and targeted fixtures for each semantic case.

2. Risk: float32 conversion edge behavior differs by runtime.
   Mitigation: centralize conversion semantics and add explicit cross-runtime float parity tests.

3. Risk: generated decode complexity introduces subtle bugs.
   Mitigation: shared decode combinators in `Bosatsu/Proto/Wire` and extensive malformed-input tests.

4. Risk: performance overhead from pure Bosatsu codec composition.
   Mitigation: keep helper APIs coarse-grained, benchmark representative payload sizes, optimize hot helpers before expanding scope.

## Rollout notes

1. Ship as additive and opt-in (`protoc` plugin use is explicit).
2. Land in three steps:
3. Step 1: generator + wire helpers + JVM parity tests.
4. Step 2: float32 runtime helper wiring + Python/C parity.
5. Step 3: docs and example workflow.
6. Mark feature experimental for the first release cycle.
7. Expand to proto2 as follow-up once phase-1 stability is proven.

## Follow-up work

1. proto2 support.
2. Additional generated helper layers for validation or richer decode errors.
3. Optional `lib/tool proto` convenience command analogous to JSON commands.
4. Service-related generation if requested later.
