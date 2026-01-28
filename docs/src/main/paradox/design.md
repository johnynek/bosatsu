# Bosatsu Design Philosophy and Goals
Bosatsu has three main themes:

1. correctness
2. simplicity
3. usability

These are in priority order: we won’t trade correctness for usability, and we’re
skeptical of features that add complexity just for convenience.

## Relationship to Python
The user experience is inspired by Python, but Bosatsu is a minimal, statically
typed, total language. All external names must be explicitly imported, and all
values are private unless exported. This keeps package dependencies explicit
without type inference.

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
