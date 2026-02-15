# Confusing Or Non-Actionable Compiler Error Messages (`lib show`)

Date: 2026-02-15  
Repo: `/Users/oscar/code/bosatsu`  
Git SHA: `e8e500eb894de4402cd54c8c53a6d894bb90189b`  
Artifacts: `/tmp/lib_show_audit_1771180226`

This file tracks diagnostics that are not hard compiler crashes, but are confusing or not actionable enough for normal `lib show` usage.

## 1) Lowercase File Stem Looks Valid To Users But Fails `--package` Parse

### Repro

```bash
./bosatsuj lib show -n core_alpha --color none --package euler1
```

### Observed

```text
could not parse euler1 as a package name. Must be capitalized strings separated by /

Usage: bosatsu lib show [--repo_root <path>] [--name <lib_name>] [--cas_dir <path>] [--package <packageName>]... [--output <path>] [--color <color>]
```

### Why confusing

- In this repo, there are source files like `test_workspace/euler1.bosatsu`, so `euler1` is a natural first guess.
- The message states the parse rule, but does not hint that package names come from `package ...` declarations (for this file: `Euler/One`).

### Better message

- Keep the parse-rule text, and add a hint such as:
  - `use the package declaration name (example: Euler/One), not the filename stem (e.g. euler1)`.

### Scope seen in this run

This exact parse failure shape appeared for:

- `bazel_deps_api`
- `bo_test`
- `dicttools`
- `euler1`
- `euler2`
- `euler3`
- `euler4`
- `euler5`
- `euler6`
- `euler7`
- `gen_deps`
- `recordset`
