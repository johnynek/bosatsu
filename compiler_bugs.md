# Compiler Bugs Found While Exercising `bosatsuj lib eval`

Date: 2026-02-15  
Repo: `/Users/oscar/code/bosatsu`  
Git SHA: `23007f090f47c5f84fa8292b04c3f0616aee60b6`  
Sweep artifacts: `/Users/oscar/code/bosatsu/tmp/lib_eval_run_timeout2_1771125362`

## Status On `codex/lib-eval-rendering-improvements`

After fixes in this branch and rebuilding `cli/assembly`, all repros in this file were re-run and no longer print stack traces.

- `Bosatsu/Char::*` repros now render as char literals (for example `.'\n'`).
- `Bosatsu/Collection/Array::*` repros now render array contents.
- `Bosatsu/FibBench`, `Bosatsu/FibBench::main`, `Bosatsu/Prog`, and `Bosatsu/Prog::unit` now render `Prog(...)`-style output rather than crashing.
- Missing-value lookup (`Bosatsu/Option::none_Int`) now returns a normal CLI error message without a stack trace.
- Follow-up: the `lib json write` failure for `Bosatsu/Char::delete_char` is now fixed on this branch; tracked originally in issue [#1684](https://github.com/johnynek/bosatsu/issues/1684).

## Sweep Scope

- Source packages discovered in `test_workspace`: 47
- Value identifiers discovered from `lib show`: 444
- Total eval targets checked (`package` + `package::value`): 491
- Harness summary: 478 success, 13 non-zero
- Non-zero breakdown:
  - 11 stack-trace crashes
  - 2 timeout hits under a 20s per-target cap

Command shape used in the sweep:

```bash
./bosatsuj lib eval \
  --name core_alpha \
  --repo_root /Users/oscar/code/bosatsu \
  --main <package-or-package::value> \
  --color none
```

## Bug 1: `ExternalValue` Causes Stack-Trace Crash During Eval

### Affected mains

- `Bosatsu/Char::delete_char`
- `Bosatsu/Char::line_feed_char`
- `Bosatsu/Char::unit_separator_char`
- `Bosatsu/Collection/Array::a5`
- `Bosatsu/Collection/Array::a_from`
- `Bosatsu/Collection/Array::a_set`
- `Bosatsu/Collection/Array::a_sorted`

### Repro

```bash
./bosatsuj lib eval --name core_alpha --repo_root /Users/oscar/code/bosatsu --main Bosatsu/Char::delete_char --color none
```

### Observed

- Exits non-zero with Java stack trace.
- Top error:

```text
java.lang.RuntimeException: got illtyped error: ill-typed value at root for expected type Bosatsu/Predef::Char: dev.bosatsu.Value$ExternalValue
```

### Expected

- No stack trace.
- Either a rendered value or a concise actionable error.

### Follow-up (JSON Path)

Even after initial eval rendering fixes, `lib json write` still failed for at least one Char external value:

```bash
./bosatsuj lib json write --name core_alpha --repo_root /Users/oscar/code/bosatsu --main Bosatsu/Char::delete_char --color none
```

Observed (exit 1, no stack trace):

```text
unexpected value error: ill-typed value at root for expected type Bosatsu/Predef::Char: dev.bosatsu.Value$ExternalValue
```

Control check (`lib eval`) for same main succeeds:

```bash
./bosatsuj lib eval --name core_alpha --repo_root /Users/oscar/code/bosatsu --main Bosatsu/Char::delete_char --color none
```

```text
.'\u007f': Bosatsu/Predef::Char
```

Issue filed: [#1684](https://github.com/johnynek/bosatsu/issues/1684)

Current status after follow-up fix:

- `./bosatsuj lib json write --name core_alpha --repo_root /Users/oscar/code/bosatsu --main Bosatsu/Char::delete_char --color none` now succeeds and prints `"\\u007f"` (rendered as `""` in terminal).
- `./bosatsuj lib json write --name core_alpha --repo_root /Users/oscar/code/bosatsu --main Bosatsu/Collection/Array::a5 --color none` now succeeds and prints `[ 0, 1, 2, 3, 4 ]`.

---

## Bug 2: `SumValue` Causes Stack-Trace Crash During Eval

### Affected mains

- `Bosatsu/FibBench`
- `Bosatsu/FibBench::main`
- `Bosatsu/Prog`
- `Bosatsu/Prog::unit`

### Repro

```bash
./bosatsuj lib eval --name core_alpha --repo_root /Users/oscar/code/bosatsu --main Bosatsu/Prog --color none
```

### Observed

- Exits non-zero with Java stack trace.
- Top error:

```text
java.lang.RuntimeException: got illtyped error: ill-typed value ... expected type Bosatsu/Prog::Prog[a, b, ()]: dev.bosatsu.Value$SumValue
```

### Expected

- No stack trace.
- Value should either render, or fail with a user-facing diagnostic that explains why this value cannot be displayed.

---

## Bug 3: Invalid Main Identifier Triggers Raw Stack Trace

Discovered while testing identifier UX during this pass.

### Repro

```bash
./bosatsuj lib eval --name core_alpha --repo_root /Users/oscar/code/bosatsu --main Bosatsu/Option::none_Int --color none
```

### Observed

- Exits non-zero with `unknown error` plus Java stack trace.
- Top error:

```text
java.lang.Exception: value Bosatsu/Option::none_Int not found
```

### Expected

- A normal user-facing message like `value ... not found` without a stack trace.

---

## Notes On Timeout Entries

- Sweep timeout entries:
  - `Bosatsu/Collection/Queue`
  - `Bosatsu/Collection/Queue::tests`
- These were timeout hits under the 20s harness cap, not hard hangs.
- Manual rerun with a 120s cap completed both in about 18s and produced a large `TestSuite` output.

---

# Compiler Bugs Found While Exercising `bosatsuj lib show`

Date: 2026-02-15  
Repo: `/Users/oscar/code/bosatsu`  
Git SHA: `e8e500eb894de4402cd54c8c53a6d894bb90189b`  
Initial sweep artifacts: `/tmp/lib_show_audit_1771180226`  
Corrected package-name sweep artifacts: `/tmp/lib_show_audit2_1771180455`

## Bug LS1: Unknown Package Produces `unknown error` + Java Stack Trace

### Repro

```bash
./bosatsuj lib show -n core_alpha --color none --package Bool
```

### Observed

- Non-zero exit with `unknown error` and a full Java stack trace.
- Top lines:

```text
unknown error:

java.lang.Exception: package Bool not found
  at dev.bosatsu.library.LibraryEvaluation$LookupError.toException(...)
```

### Why this is a compiler bug

- `lib show` should never print stack traces for user mistakes (unknown package target).
- The same failure shape appeared for 18 package targets in the first sweep (all listed in `/tmp/lib_show_audit_1771180226/summary.tsv`), each ending in `LibraryEvaluation.scala` trace output.

### Expected

- A normal user-facing error such as `package <name> not found` with no stack trace.

### Control

- After rerunning with canonical package names from source `package` declarations, all 55 package-targeted runs succeeded (`success=55 fail=0`), with no stack traces:
  - `awk -F '\t' ... /tmp/lib_show_audit2_1771180455/summary.tsv` -> `success=55 fail=0 total=55`
