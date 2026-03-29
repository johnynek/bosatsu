# Inspecting Compiled Output with `show`

`show` prints fully typechecked package data as EDN. It is useful when you
want to inspect what Bosatsu inferred and what the compiler is lowering.
Use `--json` when you want machine-readable output for tooling.

## Quick examples

All commands below assume running from this repository root.

### Show a whole package

```bash
./bosatsuj show --name core_alpha --package Bosatsu/Char --color none
```

This prints one `(package ...)` form with sections like `:imports`, `:types`,
`:externals`, and `:defs`.

### Emit JSON instead of EDN

```bash
./bosatsuj show --name core_alpha --package Bosatsu/Char --json
```

`--json` emits a deterministic JSON projection of the same `show` data. The
top-level value is an object with `"$form": "show"` and fields such as
`ir`, `typed-passes`, and `packages`. Typed output also includes
`interfaces`.

### Show Matchless IR instead of typedexpr

```bash
./bosatsuj show --name core_alpha --package Bosatsu/Char --ir matchless --color none
```

This lowers the selected package to Matchless before rendering. The top-level
`show` form reports `:ir matchless` and `:matchless-passes`. Matchless output
is intentionally value-only: it omits typed sections such as `:interfaces` and
package `:types`.

### Show a single type

```bash
./bosatsuj show --name core_alpha --type Bosatsu/Option::Option --color none
```

Use `--type <package::Type>` when you only want one type definition instead of
the full package.

### Show a single value

```bash
./bosatsuj show --name core_alpha --value Bosatsu/Num/Binary::not --color none
```

Use `--value <package::value>` to focus on one top-level value.
This selects values that exist in compiled package output. If a helper value is
fully inlined or otherwise eliminated, it will not be available by name.

If you want to inspect values before a pass erases them, disable the relevant
pass explicitly:

```bash
./bosatsuj show --name core_alpha \
  --value Bosatsu/Num/Binary::helper \
  --disable-typed-pass discard-unused \
  --color none
```

You can mix selectors:

```bash
./bosatsuj show --name core_alpha \
  --package Bosatsu/Char \
  --type Bosatsu/Option::Option \
  --value Bosatsu/Num/Binary::not \
  --color none
```

If no `--package`, `--type`, or `--value` is given, `show` shows all local
library packages (the existing default behavior).

## Pass controls

Typed output keeps the current fixed pass order and lets you disable stages
individually:

```bash
./bosatsuj show --name core_alpha \
  --package Bosatsu/FibBench \
  --disable-typed-pass loop-recur-lowering \
  --disable-typed-pass normalize \
  --color none
```

`library show` still accepts `--no-opt` as a compatibility alias for disabling
all typed passes at once.

Matchless output supports three named stages:

```bash
./bosatsuj show --name core_alpha \
  --package Bosatsu/Char \
  --ir matchless \
  --disable-matchless-pass hoist-invariant-loop-lets \
  --disable-matchless-pass reuse-constructors \
  --color none
```

To inspect Matchless before and after namespace-level inlining, toggle
`global-inlining`:

```bash
./bosatsuj show --name core_alpha \
  --value Bosatsu/FibBench::fib \
  --ir matchless \
  --disable-matchless-pass global-inlining \
  --color none
```

```bash
./bosatsuj show --name core_alpha \
  --value Bosatsu/FibBench::fib \
  --ir matchless \
  --color none
```

The first command shows raw lowered Matchless with local cleanup only. The
second shows the default Matchless pipeline, including global inlining.

## Why this helps

### See inferred types on values

Entries in `:defs` include typed expressions, so you can inspect the inferred
type information attached to each value.

### See tail-recursive lowering shape

When tail recursion is optimized, `show` exposes loop-like forms such as
`loop`/`recur` in the typed expression. For example:

```bash
./bosatsuj show --name core_alpha --package Bosatsu/FibBench --color none
```

In that package, the source helper `list_len` is inlined into another compiled
definition, and you can still see the loop-like lowered shape there. This is a
practical way to confirm code that should compile to loops in backends.
