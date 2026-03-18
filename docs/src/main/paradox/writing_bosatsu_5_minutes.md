# Writing Bosatsu in 5 minutes

This page is for programmers who already know Python and immutable functional
programming (Haskell/Scala style), and want the shortest path to writing useful
Bosatsu.

If you need repo/bootstrap setup first, see [Getting started](getting_started.html).
For full syntax details, see the [Language guide](language_guide.html).

## 1) Minimal file shape

```bosatsu
package Quick/Hello

export (Mood(), greet, tests)

enum Mood: Happy, Sad

def greet(m: Mood) -> String:
  match m:
    case Happy: "hello"
    case Sad: "cheer up"

tests = TestSuite("hello tests", [
  Assertion(greet(Happy) matches "hello", "happy greeting"),
])
```

Top-order bits:

1. Every file starts with exactly one `package`.
2. Imports and exports are explicit.
3. `enum`/`struct` replace class + inheritance style modeling.
4. `match` is the normal way to branch on data shape.
5. Tests are regular values (`Assertion`, `TestSuite`).

## 2) Python to Bosatsu: quick mental map

1. Everything is immutable. You can shadow names, but not mutate values.
2. Blocks are expressions: the last line is the return value.
3. No `while`/mutation loops; use `recur`, `int_loop`, folds, or comprehensions.
4. No exceptions as control flow in normal code. Model errors in types.
5. Side effects are explicit `Prog[...]` values, not implicit execution.

If you are writing I/O code, expect this style:

```bosatsu
main = (
  _ <- println("hello").await()
  pure(0)
)
```

`<-` with `.await()` is the common way to sequence `Prog` values.

## 3) Recurring tricks from `test_workspace`

1. Prefer `match` + pattern guards (`if ...`) for branching.
2. Use list patterns heavily: `[]`, `[head, *tail]`.
3. Write local helper loops with accumulators for performance and clarity.
4. Use `matches` for concise checks in tests and conditionals.
5. Keep package APIs intentional with `export` and `Type()` (export constructors).

## 4) Always-be-compiling with `lib check` + `todo`

Run typecheck continuously while editing:

```sh
./bosatsu lib check
```

If you want a looser edit loop while sketching code, use:

```sh
./bosatsu lib check --warn
./bosatsu lib check --lax
```

`--warn` keeps going on unused values/imports, shadowed-binding type changes,
and unreachable branches, but still prints them. `--lax` suppresses that same
lint set entirely. Non-total matches, recursion errors, import/type failures,
and other hard errors still stop the run in every mode.

When you know types but not implementation, use `todo` to keep moving:

```bosatsu
def hard_part(a: String, b: Int) -> Bool:
  todo((a, b))
```

Rules:

1. `todo` is only available in check-only commands (`tool check` / `lib check`).
2. Commands that emit or run code (`lib test`, `lib build`, `tool eval`, etc.)
   fail until `todo` is removed.
3. Keep `todo` arguments meaningful (usually tuple all planned inputs) so you do
   not lose type information during refactors.

Once placeholders are removed, run:

```sh
./bosatsu lib test
```

`lib test` accepts the same `--warn` and `--lax` flags, which is useful when
you want to keep running tests while postponing non-fatal lint cleanup.

## 5) Practical 5-minute loop

1. Create one package with one exported function.
2. Add a tiny `tests` value immediately.
3. Iterate with `lib check`, `lib check --warn`, or `lib check --lax`,
   using `todo` for unfinished branches.
4. Replace `todo` incrementally.
5. Finish with `lib test` (or `lib test --warn` if you are still cleaning up
   unused definitions/imports).

That loop matches how most code in `test_workspace` evolves: explicit data
models, explicit imports/exports, and constant typechecked feedback.
