# Inspecting Compiled Output with `lib show`

`lib show` prints fully typechecked package data as EDN. It is useful when you
want to inspect what Bosatsu inferred and what the compiler is lowering.

## Quick examples

All commands below assume running from this repository root.

### Show a whole package

```bash
./bosatsuj lib show --name core_alpha --package Bosatsu/Char --color none
```

This prints one `(package ...)` form with sections like `:imports`, `:types`,
`:externals`, and `:defs`.

### Show a single type

```bash
./bosatsuj lib show --name core_alpha --type Bosatsu/Option::Option --color none
```

Use `--type <package::Type>` when you only want one type definition instead of
the full package.

### Show a single value

```bash
./bosatsuj lib show --name core_alpha --value Bosatsu/Num/Binary::not --color none
```

Use `--value <package::value>` to focus on one top-level value.
This selects values that exist in compiled package output. If a helper value is
fully inlined or otherwise eliminated, it will not be available by name.

You can mix selectors:

```bash
./bosatsuj lib show --name core_alpha \
  --package Bosatsu/Char \
  --type Bosatsu/Option::Option \
  --value Bosatsu/Num/Binary::not \
  --color none
```

If no `--package`, `--type`, or `--value` is given, `lib show` shows all local
library packages (the existing default behavior).

## Why this helps

### See inferred types on values

Entries in `:defs` include typed expressions, so you can inspect the inferred
type information attached to each value.

### See tail-recursive lowering shape

When tail recursion is optimized, `lib show` exposes loop-like forms such as
`loop`/`recur` in the typed expression. For example:

```bash
./bosatsuj lib show --name core_alpha --package Bosatsu/FibBench --color none
```

In that package, the source helper `list_len` is inlined into another compiled
definition, and you can still see the loop-like lowered shape there. This is a
practical way to confirm code that should compile to loops in backends.
