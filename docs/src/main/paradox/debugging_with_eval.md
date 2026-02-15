# Debugging with `lib eval`

`lib eval` is for inspecting values while iterating on Bosatsu code.

It is intended for debugging and exploration (and future REPL-like workflows),
not for machine parsing or stable serialization.

## Quick start

1. Typecheck while iterating:

```bash
./bosatsuj lib check --name core_alpha
```

2. Evaluate a value:

```bash
./bosatsuj lib eval --name core_alpha --main Bosatsu/Char::delete_char
```

## Command reference

```bash
./bosatsuj lib eval [--repo_root <path>] [--name <lib_name>] [--cas_dir <path>] --main <valueIdent> [--color <color>]
```

Common flags:

- `--name`: library name from `bosatsu_libs.json`
- `--main`: package or `package::value` to evaluate
- `--color`: output mode (`none`, `ansi`, `html`)

## Finding values to inspect

Use `lib show` to discover exported values in a package:

```bash
./bosatsuj lib show --name core_alpha --package Bosatsu/Collection/Array --color none
```

Then pass a package or package value to `--main`.

## Examples from this repo

All examples below were run from this repository root.

### 1) Small scalar-like value (`Char`)

```bash
./bosatsuj lib eval --name core_alpha --repo_root /Users/oscar/code/bosatsu --main Bosatsu/Char::delete_char --color none
```

```text
.'': Bosatsu/Predef::Char
```

### 2) Function values

```bash
./bosatsuj lib eval --name core_alpha --repo_root /Users/oscar/code/bosatsu --main Bosatsu/Char::char_to_Int --color none
```

```text
<fn arity=1>: Bosatsu/Predef::Char -> Bosatsu/Predef::Int
```

`lib eval` shows function arity and type, not function body.

### 3) External `Array` values

```bash
./bosatsuj lib eval --name core_alpha --repo_root /Users/oscar/code/bosatsu --main Bosatsu/Collection/Array::a5 --color none
```

```text
[0, 1, 2, 3, 4]: Bosatsu/Collection/Array::Array[Bosatsu/Predef::Int]
```

### 4) `Prog` values (opaque)

```bash
./bosatsuj lib eval --name core_alpha --repo_root /Users/oscar/code/bosatsu --main Bosatsu/Prog::unit --color none
```

```text
Prog(...): forall a: *, b: *. Bosatsu/Prog::Prog[a, b, ()]
```

`Prog` is an external effectful representation, so eval prints an opaque form.

### 5) Larger structured values

```bash
./bosatsuj lib eval --name core_alpha --repo_root /Users/oscar/code/bosatsu --main Bosatsu/Example/Json/Github/Workflows/Ci::workflow --color none
```

Excerpt:

```text
Workflow {
    jobs: WorkflowJobs {
            test: WorkflowJobsTest {
                    runs-on: 'ubuntu-latest',
                    steps: [Step { ... }, Step { ... }, ...],
                    ...
```

You can also evaluate package mains that are large test structures:

```bash
./bosatsuj lib eval --name core_alpha --repo_root /Users/oscar/code/bosatsu --main Bosatsu/Collection/Array --color none
```

## Common gotchas

### Constructors are not valid `--main` values

This fails:

```bash
./bosatsuj lib eval --name core_alpha --repo_root /Users/oscar/code/bosatsu --main Bosatsu/Bool::True --color none
```

With:

```text
Bosatsu/Bool::True is a constructor or type name, not a value. A top-level value is required.
```

### Missing values are reported directly

```bash
./bosatsuj lib eval --name core_alpha --repo_root /Users/oscar/code/bosatsu --main Bosatsu/Char::missing --color none
```

```text
value Bosatsu/Char::missing not found
```

### Output format is for humans

- It is intended for debugging readability.
- It is not a stable parse/serialization format.
- If you need JSON output for tooling, use `lib json`.

### Large values can be noisy

Use shell tools for workflow:

```bash
./bosatsuj lib eval ... --color none | less
./bosatsuj lib eval ... --color none > /tmp/eval.txt
```
