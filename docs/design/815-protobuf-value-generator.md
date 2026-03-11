---
issue: 815
priority: 3
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
estimated_size: M
generated_at: 2026-03-11T21:08:36Z
---

# Issue #815 Design: protobuf value generator

_Issue: #815 (https://github.com/johnynek/bosatsu/issues/815)_

## Summary

Design for a protoc plugin that generates Bosatsu protobuf models plus pure Bosatsu wire encoders, with a cross-runtime (JVM/Python/C) serialization workflow, acceptance criteria, risks, and rollout plan.

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

Add a `protoc` plugin (`protoc-gen-bosatsu`) that generates Bosatsu packages from protobuf IDL and emits pure Bosatsu encoders (`Message -> Bosatsu/IO/Bytes::Bytes`). The encoder runtime is a shared Bosatsu package (`Bosatsu/Proto/Wire`) so protobuf bytes can be produced consistently on JVM evaluator, Python transpilation, and C transpilation without runtime protobuf dependencies.

## Problem statement

Bosatsu currently has no first-class protobuf generation path comparable to JSON generation:

1. There is no generator from `.proto` schemas to Bosatsu `struct`/`enum` definitions.
2. There is no standard, portable protobuf wire encoder for Bosatsu values.
3. Users can model data in Bosatsu, but producing protobuf bytes today requires custom host-language glue.

Issue #815 asks for a generation workflow that starts from protobuf IDL and ends with serialized protobuf bytes from Bosatsu values, with portability across JVM, Python, and C runtimes.

## Goals

1. Provide a standard `protoc` plugin that emits Bosatsu source from protobuf descriptors.
2. Generate Bosatsu `struct`/`enum` value models for protobuf messages/enums.
3. Generate Bosatsu encoder functions for each message (`encode_<Message>`).
4. Keep serialization runtime portable: same Bosatsu source works in JVM eval, Python transpile, and C transpile paths.
5. Make generated output deterministic so regeneration is stable in CI.
6. Surface unsupported protobuf features as clear generation-time errors.
7. Document end-to-end usage from `protoc` invocation to byte emission.

## Non-goals

1. No protobuf decoding/parsing in this issue (`Bytes -> Message` is out of scope).
2. No gRPC/service client or server stub generation.
3. No unknown-field retention or roundtrip of unknown wire data.
4. No proto2 support in v1.
5. No `float` (32-bit) support in v1; generation fails clearly when used.

## Proposed architecture

### 1. `protoc-gen-bosatsu` plugin entrypoint

1. Add a JVM plugin entrypoint (`cli/src/main/scala/dev/bosatsu/protobuf/ProtocPluginMain.scala`) that:
2. Reads `CodeGeneratorRequest` bytes from stdin.
3. Builds a normalized internal schema model.
4. Generates Bosatsu files for `file_to_generate`.
5. Writes `CodeGeneratorResponse` bytes to stdout.
6. Returns non-zero on hard failures, with `CodeGeneratorResponse.error` for descriptor/feature errors.

A repository script (`protoc-gen-bosatsu`) will execute the class from the CLI assembly jar so standard `protoc` plugin flow works:

`protoc --plugin=protoc-gen-bosatsu=./protoc-gen-bosatsu --bosatsu_out=./generated --proto_path=./proto ./proto/config/v1/config.proto`

### 2. Normalized protobuf model in core

Add generator-facing ADTs in `core/src/main/scala/dev/bosatsu/protobuf/ProtoDescriptorModel.scala`:

1. `ProtoFileModel`, `MessageModel`, `EnumModel`, `FieldModel`, `OneOfModel`, `ScalarKind`.
2. Resolved type references (local and cross-file) with fully-qualified protobuf symbol identity.
3. Precomputed encoding metadata (wire type, packed/default behavior, field presence mode).

This keeps descriptor parsing concerns separate from code emission, and makes tests independent of stdin/stdout plugin framing.

### 3. Bosatsu code generation pipeline

`core/src/main/scala/dev/bosatsu/protobuf/ProtoToBosatsu.scala` will map normalized models to `Package.Parsed`/`Statement` and render via existing `Document` instances. `ProtoNaming.scala` centralizes name normalization and collision handling.

Generation rules:

1. One Bosatsu package per protobuf file.
2. Default package mapping: `Proto/<ProtoPackagePartsCapitalized>`.
3. Messages map to Bosatsu `struct`.
4. Enums map to Bosatsu `enum` and include an `Unknown(value: Int)` branch.
5. `oneof` maps to a generated Bosatsu enum field with explicit `NotSet` case.
6. Repeated fields map to `List[T]`.
7. Map fields map to `List[(K, V)]` in v1 for deterministic order and no dict-runtime assumptions.
8. Optional/presence fields map to `Option[T]`.
9. Each message gets `encode_<Message>: <Message> -> Bosatsu/IO/Bytes::Bytes`.

### 4. Wire-format runtime package in Bosatsu

Add `test_workspace/Bosatsu/Proto/Wire.bosatsu` and export it through `test_workspace/core_alpha_conf.json`.

`Bosatsu/Proto/Wire` provides pure helpers used by generated code:

1. Varint encoding (`uvarint`, signed/zigzag variants).
2. Fixed-width little-endian integer encoding (`fixed32`, `fixed64`, signed variants).
3. Length-delimited helpers (strings, bytes, embedded messages).
4. Field-key composition (`field_number << 3 | wire_type`).
5. Packed repeated encoding helper.
6. Message concatenation helper returning `Bosatsu/IO/Bytes::Bytes`.

Runtime portability is achieved because helpers use only already-portable Bosatsu features (`Int`, bit ops, `Float64` bits for double, `Bosatsu/IO/Bytes` operations).

### 5. Supported/unsupported scalar scope in v1

Supported protobuf field kinds in v1:

1. `bool`
2. `string`
3. `bytes`
4. `double`
5. `int32`, `int64`
6. `uint32`, `uint64`
7. `sint32`, `sint64`
8. `fixed32`, `fixed64`
9. `sfixed32`, `sfixed64`
10. `enum`
11. `message`

Generation-time rejected in v1:

1. `float`
2. `group`
3. `extensions`
4. proto2 syntax files

This keeps the first version portable and testable while avoiding float32 conversion primitives in three runtimes.

### 6. Minimal binary serialization workflow

No new CLI subcommand is required for MVP binary emission.

1. User runs `protoc` with the Bosatsu plugin and checks generated `.bosatsu` into source.
2. User builds Bosatsu values of generated message types.
3. User calls generated `encode_<Message>` functions to get `Bosatsu/IO/Bytes::Bytes`.
4. User writes bytes through existing `Bosatsu/IO/Core.write_bytes` inside `Prog` programs.
5. Execution can happen through existing JVM evaluator (`tool eval --run` / `lib eval --run`) or transpiled Python/C binaries.

This is the minimal path that satisfies cross-runtime output without adding runtime-specific protobuf dependencies.

## Detailed implementation plan

1. Add protobuf generator dependencies in `build.sbt`/`project/Dependencies.scala` for JVM plugin code and tests.
2. Add `ProtoDescriptorModel.scala` ADTs for normalized descriptors.
3. Implement `ProtoNaming.scala` for package/type/field symbol mapping, escaping, and collision suffixing.
4. Implement `ProtoToBosatsu.scala` that emits `Package.Parsed` and deterministic source text.
5. Add `ProtocPluginMain.scala` stdin/stdout plugin framing and option parsing.
6. Add wrapper script `protoc-gen-bosatsu` for local/protoc integration.
7. Add `Bosatsu/Proto/Wire.bosatsu` wire helpers and include package in `core_alpha_conf.json`.
8. Add fixture proto file(s) under `core/src/test/resources/protobuf/`.
9. Add generator golden tests (`ProtoToBosatsuTest.scala`) for shape/naming/defaults.
10. Add plugin framing tests (`ProtocPluginMainTest.scala`) for request/response and error propagation.
11. Add wire parity tests (`ProtoWireEncodingParityTest.scala`) comparing generated encoder bytes to Java protobuf bytes for fixture cases.
12. Extend Python and C codegen tests (`PythonGenTest.scala`, `ClangGenTest.scala`) with fixture programs that encode messages and assert expected bytes.
13. Add user docs page (`docs/src/main/paradox/generating_protobuf.md`) and docs index link.
14. Add an example package (`test_workspace/Bosatsu/Example/Proto/ConfigDemo.bosatsu`) showing config-to-protobuf emission.

## Testing strategy

### A. Plugin protocol tests

1. Construct synthetic `CodeGeneratorRequest` values and assert `CodeGeneratorResponse` files/errors.
2. Verify only `file_to_generate` emits files.
3. Verify invalid option values produce deterministic error output.

### B. Generator golden tests

1. Generate Bosatsu source for fixture schemas and snapshot expected output.
2. Assert naming stability under collisions and reserved identifiers.
3. Assert import and package mapping stability across multi-file schemas.

### C. Wire-compatibility parity tests (JVM)

1. For each fixture message, build a Bosatsu value and encode with generated `encode_<Message>`.
2. Build corresponding Java protobuf `DynamicMessage` and serialize.
3. Assert byte-for-byte equality.
4. Cover presence/default omission, packed repeated fields, maps, oneof, nested messages, enum unknown branch.

### D. Cross-runtime parity tests

1. Compile fixture Bosatsu code to Python and C.
2. Run programs that encode fixed fixture values.
3. Convert output `Bytes` to integer lists in-test and compare to known-good expected bytes.
4. Assert JVM evaluator, Python output, and C output are identical.

### E. Failure-mode tests

1. proto2 file input returns generation error with file path and reason.
2. `float` field input returns generation error with field path and reason.
3. Extensions/groups return generation error with actionable guidance.

## Acceptance criteria

1. `protoc-gen-bosatsu` can be invoked by `protoc` and emits Bosatsu files for each requested source file.
2. Generated files typecheck with Bosatsu tooling in fixture tests.
3. Message, enum, nested type, and oneof definitions are generated with deterministic naming and imports.
4. Each generated message has an exported encoder function returning `Bosatsu/IO/Bytes::Bytes`.
5. For supported field kinds, generated encoder output is byte-identical to official protobuf JVM serialization in parity tests.
6. The same generated encoder logic produces identical bytes on JVM evaluator, Python transpile path, and C transpile path.
7. `Bosatsu/Proto/Wire` is added to core alpha exports and used by generated code.
8. Unsupported features (`proto2`, `float`, groups, extensions) fail generation with clear diagnostics.
9. Generation output is deterministic across repeated runs with identical descriptor input.
10. Docs include complete usage flow from `protoc` invocation to writing serialized bytes from Bosatsu programs.
11. No runtime protobuf dependency is required for executing generated Bosatsu encoders on Python or C backends.

## Risks and mitigations

1. Risk: protobuf semantics mismatch (presence/default/packed) causes subtly invalid bytes.
   Mitigation: JVM parity tests against official serialization for every supported feature combination.

2. Risk: naming collisions create unstable or invalid Bosatsu identifiers.
   Mitigation: centralized naming rules with deterministic suffixing, and golden tests on collision fixtures.

3. Risk: pure Bosatsu encoder allocation overhead for large messages.
   Mitigation: keep wire helper API coarse-grained (packed and concatenation helpers), benchmark with representative payload sizes, and optimize helpers before broad rollout.

4. Risk: users expect full protobuf feature coverage immediately.
   Mitigation: explicit generation-time errors, clear docs on v1 supported subset, and tracked follow-up work.

5. Risk: cross-runtime divergences in integer/bit edge behavior.
   Mitigation: cross-runtime byte parity tests using shared fixtures and expected vectors.

## Rollout notes

1. Ship as additive and opt-in: users only adopt when they run the plugin.
2. Mark feature as experimental in docs for first release.
3. Land in three steps to reduce risk:
4. Step 1: generator + wire library + JVM parity tests.
5. Step 2: Python and C parity coverage.
6. Step 3: user docs and example package.
7. Keep unsupported-feature errors strict rather than partial generation.
8. After one stable release cycle, expand coverage (proto2 and float32) in follow-up issues.

## Follow-up work (out of scope)

1. Add proto2 support, including required/optional semantics.
2. Add `float` support with portable float32 bit conversion primitives.
3. Add decode generation (`Bytes -> Message`) and unknown field preservation strategy.
4. Add optional convenience command analogous to `lib json write` for direct bytes emission without a small `Prog` wrapper.
5. Add service/gRPC-related generation if needed.
