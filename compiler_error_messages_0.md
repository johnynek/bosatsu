# Confusing Or Non-Actionable Compiler Error Messages

Date: 2026-02-15  
Repo: `/Users/oscar/code/bosatsu`  
Git SHA: `23007f090f47c5f84fa8292b04c3f0616aee60b6`

This file tracks diagnostics that are not crashes/bugs in themselves, but are confusing or insufficiently actionable.

## 1) Constructor Identifier Parse Failure Is Not Clear About Constructor Rules

### Command

```bash
./bosatsuj lib eval --name core_alpha --repo_root /Users/oscar/code/bosatsu --main Bosatsu/Bool::True --color none
```

### Observed

```text
could not parse Bosatsu/Bool::True as a package or package::name...
```

Then usage text is printed.

### Why confusing

- `Bosatsu/Bool::True` is syntactically a plausible `package::name`.
- Message does not clarify that constructors/uppercase names are not accepted by this parser path.

### Better message

- Explicitly state accepted target kinds, e.g. `package` or top-level value (lowercase def), and that constructors are not valid `--main` targets.

---

## 2) CAS Guidance Says `run lib fetch`, But `lib fetch` Does Not Fetch Previous Version

### Repro sequence

```bash
tmpcas="$(mktemp -d)"
./bosatsuj lib show --name core_alpha --repo_root /Users/oscar/code/bosatsu --cas_dir "$tmpcas" --color none
./bosatsuj lib fetch --name core_alpha --repo_root /Users/oscar/code/bosatsu --cas_dir "$tmpcas"
./bosatsuj lib show --name core_alpha --repo_root /Users/oscar/code/bosatsu --cas_dir "$tmpcas" --color none
```

### Observed

- First `lib show` prints:

```text
could not find previous version in CAS.
...
run `lib fetch` to download it.
```

- `lib fetch` prints:

```text
fetched 0 transitive dependencies.
```

- Second `lib show` succeeds anyway.

### Why confusing

- The recommended command (`lib fetch`) does not actually fetch the missing previous version in this flow.
- Success comes from a later implicit behavior in `lib show`, not from the user-facing suggested step.

### Better message

- Either:
  - make `lib fetch` fetch that previous version, or
  - change the guidance to the command that actually resolves the issue.
