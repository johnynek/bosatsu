# Generating JSON from Bosatsu Values

Bosatsu can be used as a typed configuration language and then rendered to JSON
with `lib json`.

This page focuses on `lib json` (not `tool json`).

## Quick start

1. Typecheck as you iterate:

```bash
./bosatsuj lib check --name core_alpha
```

2. Generate JSON for a value:

```bash
./bosatsuj lib json write --name core_alpha --main Bosatsu/Example/Json/Github/Workflows/Ci::workflow
```

## Command reference

```bash
./bosatsuj lib json write [--repo_root <path>] [--name <lib_name>] --main <valueIdent> [--output <path>]
```

Common flags:

- `--name`: library name from your `bosatsu_libs.json`
- `--main`: package/value to evaluate and encode
- `--output`: write JSON to a file instead of stdout

## Encoding shape

`lib json` uses type-directed encoding:

- `Int` -> JSON number
- `Float64` -> JSON number
- `String` -> JSON string
- `Bool` -> JSON boolean
- `List[a]` -> JSON array
- `Dict[String, a]` -> JSON object
- `struct` -> JSON object
- `enum` -> JSON object (constructors must remain distinguishable after encoding)

## `Option` behavior

`Option` is especially useful for configuration records:

- `None` encodes as `null` when that is unambiguous
- `Some(x)` encodes as `x`
- Nested options (`Option[Option[a]]`) use array encoding to avoid ambiguity:
  - outer `None` -> `[]`
  - outer `Some(None)` -> `[null]`
  - outer `Some(Some(x))` -> `[x]`

For record fields, `Option` lets you model shape variation directly:

```bosatsu
struct Step(
  uses: Option[String],
  run: Option[String],
  with: Option[WithConfig],
)
```

This usually replaces many near-duplicate structs.

## Patterns that worked well for GitHub workflow modeling

- Put shared types/constants/helpers in a common module (for example `Util.bosatsu`)
- Use constants for repeated literals (runner names, action versions, `17`, step labels)
- Use helper functions for multi-line shell scripts (for example `cat_lines`)
- Prefer one record with `Option[...]` fields over many one-off variants
- Use backticks only when a field is not a valid bindable name, such as `` `runs-on` `` and `` `timeout-minutes` ``

## Practical gotchas

- Keep running `lib check` while editing models; it catches most mistakes before JSON generation.
- JSON field names come from your Bosatsu field names exactly.
- `Float64` JSON encoding requires finite values; `.NaN`, `∞`, and `-∞` are not valid JSON numbers.
- If your consumer treats absent and `null` differently, model that intentionally and test it.
- `lib json write` requires a serializable value. Function-typed mains are rejected.

## Full commands used in this workflow example

These are the exact commands used to generate the four workflow values modeled in
this example:

```bash
./bosatsuj lib json write --name core_alpha --main Bosatsu/Example/Json/Github/Workflows/Ci::workflow
./bosatsuj lib json write --name core_alpha --main Bosatsu/Example/Json/Github/Workflows/CodecovMain::workflow
./bosatsuj lib json write --name core_alpha --main Bosatsu/Example/Json/Github/Workflows/DeployWeb::workflow
./bosatsuj lib json write --name core_alpha --main Bosatsu/Example/Json/Github/Workflows/Release::workflow
```
