# The Bosatsu Programming Language

## Design Philosophy and Goals
Bosatsu has several main themes:

1. correctness
1. simplicity
1. usability

These are roughly in priority order: correctness is more important than simplicity: we don't want to sacrifice correctness for usability. Similarly, generally, we prefer simplicity to usability. Some features which can increase usability make a language considerably more complex. In general, we will be skeptical of such trades.

The main user experience of the language is inspired by Python. It is a bit of a mystery that Python is currently one of the fastest growing languages as well as already being incredibly popular that it has not been more influential in language design. No other language in the top 20 most commonly used languages is very syntactically similar to Python. Put succinctly, we want Bosatsu to by a minimalistic statically typed Python.

A second goal is that package relationships should be clear in Bosatsu. All external names must be explicitly imported, and all values are private unless exported. To see the dependencies between packages no type inference is required.

## Problem domain for Bosatu
In the current programming landscape, the vast majority of code is either in Turing complete languages or very anemic data-structure languages (JSON, YAML, TOML). Bosatsu is exploring an intermediate space. Bosatu is not designed as a general purpose language. Bosatsu in a total language: all functions return values. This rules out both infinite loops and non-explicit failure possibilities (exceptions, crashes, etc...). The goal of Bosatsu is somewhat similar to Google's [starlark](https://github.com/bazelbuild/starlark) language, a python like language for configuration and embedded applications. Starlark is used to configure bazel builds, but you can imagine configuring systems currently configured with yaml or json, or replacing general purpose languages in systems configured with those (e.g. Scala's sbt is configured with scala).

## Differences with Starlark
The biggest three differences between Bosatsu and Starlark is that Bosatsu is statically typed, all values are immutable, and there are no side-effects. The smaller differences come from adding features found in functional programming that Python lacks: a more powerful anonymous function syntax as well as the ability to define enums similar to Rust.

# Language Features
1. A static type system using a generalization of [Hindley-Milner (or Damas-Milner)](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) published in [this paper by Simon Peyton Jones et. al.](https://www.microsoft.com/en-us/research/publication/practical-type-inference-for-arbitrary-rank-types/)
1. data classes or named tuples, which we call structs following Rust
1. sum types, which we call enums following Rust.
1. a package system with explicit import and export syntax.
1. a highly python-inspired syntax including function definition, literal integers and strings, literal lists, list comprehensions, tuples [(todo)](https://github.com/johnynek/bosatsu/issues/18) and (string key) dictionaries [(todo)](https://github.com/johnynek/bosatsu/issues/14). Generally, if a syntax is possible to support in the current type system with immutable values, we generally intend to support it.

There are also some un-features, or items we don't currently and may never support
1. There is no recursion at all. The name of the current function or let binding is not in scope as we are creating a value, so it is impossible to explicitly write recursion or infinite data structures. 
1. There is no `while` syntax. The closest we have is a built in foldLeft function over lists. There is a design sketch of automatically translating for loops on lists into folds [issue 20](https://github.com/johnynek/bosatsu/issues/20).

# Language Guide
This is a brief language guide to  describe Bosatsu syntax.

## Literals

### Signed big integers
Like Python, Bosatsu supports literal signed big integers.
```
x = 1
y = 42
z = -1024
```
### Unicode strings
Bosatsu files are UTF-8 files and between `"` or `'` characters we can create any unicode string:
```
message1 = 'bar'
message2 = "this has 'single quotes'"
```

## Functions

## Custom Data Types
### Structs
### Enums

## Types

## Packages

# Note on Totality
Totality is an interesting property that limits a language below being turing complete, however, it is not obvious if that is much of a limit. People often criticize fully general languages for configuration, but it is not clear that non-termination or partial functions are the problems.

One can easily argue that total languages are still too big. For instance imagine a function with type `a -> Either[a, b]` which takes an `a` and either returns a final `b` value or the next step `a` that we should call again. If we have a non-total language we could repeated call the function when we have left to build `a -> b` In a total language, we could build a
```
def repeat(n: Int, fn: a -> Either[a, b]) -> (a -> Either[a, b])
```
to repeat `n` times the fn unless we find a b and build a function of the same type that loops more times. By repeatedly applying this function: `repeat(1000, repeat(1000, ... repeat(1000, fn)...` we can make an exponentially large number of attempts. So we could easily build a function that would take `2^128` attempts which would take longer than the life of the sun to complete. Is this meaningfully different from an infinite loop?

In this context, one might argue that lack of *any* recursion is the stronger limit than totality in Bosatsu. For instance, we could [add support for structural recursion](https://github.com/johnynek/bosatsu/issues/87), however that might cut against the goal of being a minimal language. Bosatsu is total, but it is not the largest possible total language. That limitation might be a good thing when we are seeking a minimal language. We might want the smallest language we can express common configurations and get some benefit of reuse and composition.
