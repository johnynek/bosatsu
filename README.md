# The Bosatsu Programming Language
![Codecov](https://img.shields.io/codecov/c/github/johnynek/bosatsu.svg?style=flat-square)

Bosatsu (菩薩) is the transliteration in Japanese of the sanskrit bodhisattva.
A bodhisattva is someone who can reach enlightenment but decides not to, to
help others achieve that goal.  -- Wikipedia

Bosatsu is a simple, non-turing complete language designed for configuration, queries and scripting. It
borrows from [Python](https://www.python.org/), [Haskell](https://www.haskell.org/),
[Dhall](https://hackage.haskell.org/package/dhall) and [Rust](https://www.rust-lang.org/en-US/).

## An example of Bosatsu
Here is a working Bosatsu program to solve the first [Project Euler](https://projecteuler.net/) problem:
```
package Euler/One

# see:
# https://projecteuler.net/problem=1
# Find the sum of all the multiples of 3 or 5 below 1000.

def or(a, b): True if a else b

def keep(i):
  or(i.mod_Int(3).eq_Int(0), i.mod_Int(5).eq_Int(0))

def sum(as): as.foldLeft(0, add)

# python:
# >>> sum(i for i in xrange(1000) if keep_fn(i))
# 233168

computed = sum([i for i in range(1000) if keep(i)])

test = Assertion(computed.eq_Int(233168), "expected 233168")
```

For more details see the [language guide](docs/language_guide.md) in particular the [section on syntax](docs/language_guide.md#language-guide)

## Use cases

Currently we have only implemented type-checking, the package system, and an interpreter to evalute expressions. This could
already be useful if you want to give some programmability to configuration that can load, type-check and evaluate the configuration
before executing the rest of the scala or java code.

### As a JSON templating engine

Along with [Bazel](https://github.com/bazelbuild/bazel/) Bosatsu can be used as a JSON generation
system, which could be useful for generating complex configurations in a way that has type-checking
and ability to compose. For a working example see [this example](test_workspace/).
```
cd test_workspace
bazel build ...
cat bazel-build/testjson.json
```

## Future features

We would like to implement a number of features:

1. a REPL
2. a java backend and bazel rules which can call java and scala functions
3. a skylark backend to allow writing strongly typed bazel rules compiling to untyped skylark
