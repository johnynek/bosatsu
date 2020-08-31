# Bosatsu Design Philosophy and Goals
Bosatsu has three main themes:

1. correctness
1. simplicity
1. usability

These are roughly in priority order: correctness is more important than simplicity: we don't want to sacrifice correctness for usability. Similarly, generally, we prefer simplicity to usability. Some features which can increase usability make a language considerably more complex. In general, we will be skeptical of such trades.

## Relationship to Python

The main user experience of the language is inspired by Python. It is a bit of a mystery that despite Python being currently one of the fastest growing languages, as well as already being incredibly popular, that it has not been more influential in language design. No other language in the top 20 most commonly used languages is very syntactically similar to Python. Put succinctly, we want Bosatsu to by a minimalistic statically typed Python.

A goal in that package relationships should be clear in Bosatsu. All external names must be explicitly imported, and all values are private unless exported. To see the dependencies between packages no type inference is required.

## Problem domain for Bosatsu
In the current programming landscape, the vast majority of code is either in Turing complete languages or very anemic data-structure languages (JSON, YAML, TOML). Bosatsu is exploring an intermediate space. Bosatsu is not designed as a general purpose language. Bosatsu is a total language: all functions return values. This rules out both infinite loops and non-explicit failure possibilities (exceptions, crashes, etc...). The goal of Bosatsu is somewhat similar to Google's [Starlark](https://github.com/bazelbuild/starlark) language, a Python like language for configuration and embedded applications. Starlark is used to configure bazel builds, but you can imagine configuring systems currently configured with yaml or json, or replacing general purpose languages in systems configured with those (e.g. Scala's sbt is configured with scala).

## Differences with Starlark
The biggest three differences between Bosatsu and Starlark are:
1. Bosatsu is statically typed
1. all values are immutable
1. there are no side-effects.

The smaller differences come from adding features found in functional programming that Python lacks: a more powerful anonymous function syntax as well as the ability to define enums similar to Rust.

