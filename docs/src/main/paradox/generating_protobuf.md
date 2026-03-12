# Generating Protobuf from Bosatsu Values

Bosatsu supports protobuf value generation through a standard `protoc` plugin:
`protoc-gen-bosatsu`.

The plugin generates Bosatsu `struct`/`enum` models from `.proto` files and
also emits per-message codec functions:

- `encode_<Message>: <Message> -> Bosatsu/IO/Bytes::Bytes`
- `decode_<Message>: Bosatsu/IO/Bytes::Bytes -> Option[<Message>]`

The generated codecs use `Bosatsu/Proto/Wire`, so they run in JVM eval, Python
transpile, and C transpile flows without linking protobuf runtime libraries in
those targets.

## Quick Start

1. Build the CLI assembly:

```bash
sbt cli/assembly
```

2. Run `protoc` with the Bosatsu plugin:

```bash
PATH="$PWD:$PATH" protoc \
  --plugin=protoc-gen-bosatsu="$PWD/protoc-gen-bosatsu" \
  --bosatsu_out=. \
  --proto_path=. \
  core/src/test/resources/protobuf/config_v1.proto
```

This writes a Bosatsu package under `Proto/...` that corresponds to the
protobuf package (for example `config.v1` -> `Proto/Config/V1`).

3. Typecheck your workspace:

```bash
./bosatsuj lib check --name core_alpha
```

4. Use generated codecs in Bosatsu:

```bosatsu
from Bosatsu/IO/Bytes import to_List_Int
from Proto/Config/V1 import AppConfig, encode_AppConfig, decode_AppConfig

encoded = encode_AppConfig(AppConfig {
  name: "api",
  replicas: 3,
  weights: [-1, 0, 7],
  mode: Mode_ModeActive,
  transport: AppConfig_Transport_NotSet,
  ratio: 0.5,
  epsilon: 1.25,
  labels: [("env", "prod")],
})

roundtrip_ok =
  match decode_AppConfig(encoded):
    case Some(value): encode_AppConfig(value) == encoded
    case None: False
```

## Notes

- Phase 1 supports `proto3` only.
- Unknown fields are skipped while decoding.
- Map fields are represented as `List[(K, V)]` in generated Bosatsu.
- Both protobuf `float` and `double` map to Bosatsu `Float64` in generated APIs.
