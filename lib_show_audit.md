# `bosatsuj lib show` Audit (`test_workspace`)

Date: 2026-02-15  
Repo: `/Users/oscar/code/bosatsu`  
Git SHA: `e8e500eb894de4402cd54c8c53a6d894bb90189b`

## Goal

Exercise `lib show` (not `tool show`) across all packages in `test_workspace`, checking:

- no crashes or stack traces
- output quality and fitness for debugging
- typed expression readability / parseability
- coverage of types, constructors, type info, and exports
- suspicious values (wrong types / de-optimization)

## Commands Used

```bash
./bosatsuj lib --help
./bosatsuj lib show --help
```

```bash
# Sweep 1 (filename-derived package names)
# artifacts: /tmp/lib_show_audit_1771180226
./bosatsuj lib show -n core_alpha --color none --package <name>
```

```bash
# Sweep 2 (actual package declarations + inferred Foo/Bar/Quicksort)
# artifacts: /tmp/lib_show_audit2_1771180455
./bosatsuj lib show -n core_alpha --color none --package <canonical-package>
```

```bash
# Default (no package filter)
./bosatsuj lib show -n core_alpha --color none --output /tmp/lib_show_audit2_1771180455/all_packages_show.txt
```

## Results

### Sweep 1 (filename-derived package names)

- Total targets: 55
- Success: 25
- Fail: 30
- Failure split:
  - 18 `unknown error` failures with Java stack trace (`package <name> not found`)
  - 12 parse failures (`could not parse <name> as a package name...`)

Representative crash output:

```text
unknown error:

java.lang.Exception: package Bool not found
  at dev.bosatsu.library.LibraryEvaluation$LookupError.toException(...)
```

Representative non-crash parse output:

```text
could not parse euler1 as a package name. Must be capitalized strings separated by /
```

### Sweep 2 (canonical package names)

- Total targets: 55
- Success: 55
- Fail: 0
- Stack traces found by grep: 0

### Default `lib show` (no package arg)

- `package:` blocks in output: 56
- Includes all 55 local packages plus `Bosatsu/Predef`

## Output Quality Assessment

### What works well

- Every successful package print contains:
  - package header
  - imports
  - exports
  - types
  - typed value bodies with explicit type info (`var`, `ann`, `generic`, typed lambdas, etc.)
- Recursive structure after normalization appears in a stable form (`loop` / `recur` patterns are common).
- Exports are explicit and generally match source declarations.

### Gaps / rough edges

- Constructor discoverability is implicit:
  - constructors appear in exports and in expression patterns, but there is no dedicated constructor section grouped by type.
- Readability suffers on large definitions:
  - max observed line length: `4025` (`RecordSet__Library.txt`)
  - several files exceed 2k-char single lines.
- Import lists can contain duplicates (example: `Bosatsu/Num/Nat` shows `add` and `mul` twice in imports).

## Values That Did Not Match Expectations

No clear type-incorrect values were found in this sweep, and no value-level crashes occurred when using canonical package names.

Potential de-optimization / readability concerns:

- `Bosatsu/Option::eq_Option`
  - Output materializes tuple matching through explicit `Tuple2` construction + match, which is semantically fine but noisy for debugging.
- `Bosatsu/Collection/Queue::eq_Queue`
  - Very large repeated `ann`-heavy blocks reduce human readability.

Positive control (expected optimization behavior visible):

- `Foo` package
  - Source has shadowing (`x = 1`, later `x = "this is Foo"`, `ignore = y`).
  - Shown result is:

```text
x = (lit "this is Foo" Bosatsu/Predef::String)
ignore = (lit 1 Bosatsu/Predef::Int)
```

This is consistent with expected post-compilation simplification.

## Fit-For-Purpose Verdict

`lib show` is usable for debugging compiled package structure and typed expressions when invoked with canonical package names.

Main blockers observed:

- Unknown-package path currently emits stack traces (`unknown error`), violating the "never crash / never stack trace" expectation.
- Printed representation is complete enough for expert use, but readability could be improved by:
  - constructor summary per type
  - import de-duplication in display
  - line wrapping / pretty-print width control

## Post-Fix Verification (EDN Show Format)

Date: 2026-02-15  
Branch: `codex/lib-show-audit`

### Representative output

Command:

```bash
./bosatsuj lib show -n core_alpha --color none --package Ackermann
```

Excerpt:

```text
(
  show
  :interfaces
  []
  :packages
  [
    (
      package
      :name
      "Ackermann"
      :imports
      [
        (
          import
          "Bosatsu/Predef"
          [
            (
              item
              "Fn1"
              "Fn1"
              [ ... ]
            )
            (
              item
              "Fn2"
              "Fn2"
              [ ... ]
            )
          ]
        )
      ]
      :exported-types
      []
      :exported-values
      []
      :types
      [
        (
          defined-type
          "Ackermann"
          "Nat"
          :constructors
          [
            ( constructor-fn "Zero" )
            ( constructor-fn "Succ" :fields [ [ "n" "Ackermann/Nat" ] ] )
          ]
        )
      ]
      :defs
      [ ... ]
    )
  ]
)
```

### Validation status

- `ShowEdn` codec roundtrip property test passes:
  - `sbt "coreJVM/testOnly dev.bosatsu.tool.ShowEdnRoundTripTest"`
- External EDN parser compatibility test passes:
  - `sbt "cli/testOnly dev.bosatsu.ShowEdnInteropTest"`
- Full CLI test suite passes:
  - `sbt "cli/test"`
- CLI assembly rebuilt after changes:
  - `sbt "cli/assembly"`
