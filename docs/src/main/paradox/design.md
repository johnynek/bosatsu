# Bosatsu Design Philosophy and Goals
Bosatsu has three main themes:

1. correctness
2. simplicity
3. usability

These are in priority order: we won’t trade correctness for usability, and we’re
skeptical of features that add complexity just for convenience.

Bosatsu aims to let the compiler do difficult proof work without hiding program
behavior from readers. A human reading typechecked source should be able to
recover a package's interface, the origin of each non-predef name, and the
control flow of each function directly from the file.

## Relationship to Python
The user experience is inspired by Python, but Bosatsu is a minimal, statically
typed, total language. All external names must be explicitly imported, and all
values are private unless exported. This keeps package dependencies explicit
even with type inference.

The goal is not "Python with more types." It is a language with Python-like
syntax and much stricter rules about what may be implicit.

## Literal Source Over Hidden Flow
Bosatsu prefers source that is easy for humans to reason about locally:

1. the interface of a package is at the top of the file: package name, imports,
   and exports
2. every non-predef name used in a file should have an obvious source: defined
   locally or imported explicitly
3. control flow should be visible in the syntax rather than hidden in mutation,
   exceptions, implicit effects, or implicit currying
4. surface sugar should desugar locally and mechanically: operators are
   functions, method syntax is function application, and `<-` is explicit
   continuation-style binding

The compiler may infer, normalize, and prove, but it should not invent hidden
dependencies or hidden program behavior.

## Names Are Stricter Than Types
Bosatsu is intentionally stricter about names than about type annotations.

Names create dependencies and carry most of the burden of human readability, so
they stay explicit. Types also help humans, but once types can be inferred and
checked, we do not require them just for ceremony.

The practical rule is:

1. if source literally names a type from another package, import that type so
   the dependency remains explicit
2. if a foreign type appears only through inference from values already
   imported from a direct dependency, no extra type import is required

This keeps logic flow readable without forcing type noise into every
definition, while still making package dependencies visible.

## Explicit Boundaries
Bosatsu uses explicit boundaries to preserve modularity and safety:

1. values are private unless exported
2. types and constructors can be exported separately (`Type` vs `Type()`)
3. effects are explicit `Prog[...]` values rather than implicit execution
4. external power lives behind narrow trusted boundaries
5. unused imports and unused top-level values are errors; discard should be
   written intentionally as `_ = expr`

These boundaries are part of both simplicity and correctness: they reduce
surprising behavior and make composition safer.

## Source Locality And Determinism
Bosatsu prefers rules that preserve source order, stable diagnostics, and
deterministic interfaces. Compiler checks may be sophisticated, but they should
stay anchored to the user's source rather than to rewritten intermediate forms.

## Problem domain for Bosatsu
Bosatsu targets the space between general-purpose languages and data formats
like JSON/YAML/TOML. It is total (all functions terminate), immutable, and has
no side effects or exceptions. Functions have fixed arity (Fn1..Fn32); there is
no implicit currying.

The goal is similar to [Starlark](https://github.com/bazelbuild/starlark): a
Python-like language for configuration and embedded scripting.

## Differences with Starlark
The three biggest differences are:

1. Bosatsu is statically typed
2. all values are immutable
3. there are no side effects

Bosatsu also adds features common in functional languages: algebraic data types
and exhaustive pattern matching.
