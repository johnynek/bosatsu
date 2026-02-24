# Coding Style

This repository is mostly Scala 3 compiler and tooling code. These are the coding and iteration expectations for day-to-day development.

## Design principles

1. Keep public APIs immutable.
   - Do not expose mutable collections or mutable state from public APIs.
   - Method-local mutation (`var`, mutable builders, arrays) is acceptable when it clearly improves performance or keeps a hot path simple.
   - Encapsulate mutation and return immutable values.

2. Encode invariants in types ("parse, don't verify").
   - Prefer representing valid states in types over repeating runtime assertions.
   - Use Scala 3 features (opaque types, enums, ADTs, non-empty types) to express checked invariants.
   - Parse/validate at boundaries, then pass strongly typed values internally.

## Iteration style

1. Compile frequently during development:
   - `sbt "coreJVM/test:compile"`
   - This compiles both main and test code in `core`, which covers most compiler logic.

2. Run focused tests while iterating:
   - Use `testOnly` for the suites you are touching.
   - Run `sbt "coreJVM/test; cli/test"` before opening a PR for most Scala-only changes.

3. Expand test scope based on what changed:
   - If CLI behavior changed, also run `./test_cli.sh`.
   - If docs/paradox generation changed, run `sbt "doc; paradox"`.
   - If C/Python/Node codegen or runtime paths changed, run the full CI matrix in `.github/workflows/ci.yml` (not only JVM tests).

## Coverage expectations

1. Team goal: at least 95% overall coverage, and ideally 100% on changed lines in a PR.
2. Use the same coverage entry point as CI:
   - `sbt "coverage; clean; coreJVM/test; cli/test; coverageReport"`
3. Check `.github/workflows/ci.yml`, `.github/workflows/codecov_main.yml`, and `codecov.yml` for current coverage/reporting behavior.

## Alignment with current repo configuration

1. The Scala build is strict: warnings are treated as errors (`-Werror`) with additional warning flags in `build.sbt`.
2. `core` is compiled with `-Yexplicit-nulls`.
3. Generated ScalaPB sources in `src_managed` have a narrow warning suppression; handwritten code stays under strict warning rules.
4. Agents should not run scalafmt, but should strive to write code that matches the style in the files they are editing.

## Scala conventions

1. Do not use fully qualified names in normal code or tests; import symbols at the top of the file.
2. Prefer `cats.data.ValidatedNec` over `cats.data.ValidatedNel` for better asymptotic performance.
