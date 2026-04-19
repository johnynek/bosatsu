# The Bosatsu Programming Language
![Codecov](https://img.shields.io/codecov/c/github/johnynek/bosatsu.svg?style=flat-square)

Bosatsu (菩薩) is the transliteration in Japanese of the sanskrit bodhisattva.
A bodhisattva is someone who can reach enlightenment but decides not to, to
help others achieve that goal.  -- Wikipedia

Bosatsu is a simple, non-turing complete language designed for configuration, queries and scripting. It
borrows from [Python](https://www.python.org/), [Haskell](https://www.haskell.org/),
[Dhall](https://hackage.haskell.org/package/dhall) and [Rust](https://www.rust-lang.org/en-US/).

Bosatsu aims to keep logic flow literal in the source even when the compiler is
doing sophisticated checking. A reader should be able to see the package
interface at the top of the file, where each non-predef name comes from, and
how control flows through the program directly from the syntax. Bosatsu is
stricter about names than about type annotations: dependencies stay explicit,
while most types can be inferred unless the source literally names a foreign
type.

Correctness comes from a sound total language with explicit effects and
private-by-default package boundaries. Simplicity comes from local reasoning,
not from minimizing keystrokes at all costs.

Please see the [documentation site](https://johnynek.github.io/bosatsu/)
and the [Getting started guide](docs/src/main/paradox/getting_started.md)
or try basic expressions using this [in-browser Bosatsu compiler](https://johnynek.github.io/bosatsu/compiler/).

## An example of Bosatsu
Here is a working Bosatsu program to solve the first [Project Euler](https://projecteuler.net/) problem:
```
package Euler/One

# see:
# https://projecteuler.net/problem=1
# Find the sum of all the multiples of 3 or 5 below 1000.

operator == = eq_Int
operator % = mod_Int

def operator ||(x, y):
  True if x else y

def keep(i):
  (i % 3 == 0) || (i % 5 == 0)

def sum(as): as.foldl_List(0, add)

# here is the python version:
# >>> sum(i for i in xrange(1000) if keep_fn(i))
# 233168
#
# bosatsu version here
computed = sum([i for i in range(1000) if keep(i)])

test = Assertion(computed == 233168, "expected 233168")
```

## Contributing
Please feel free to file an issue to discuss making a change to Bosatsu or
to ask a question about how Bosatsu might be useful for a use-case that is
interesting to you.

## Tree-sitter + Neovim
Bosatsu has an in-repo Tree-sitter grammar at
`syntax/tree-sitter-bosatsu`.

For local parser workflows and Neovim setup (`:TSInstall bosatsu`,
`:TSUpdate bosatsu`), see:
`syntax/tree-sitter-bosatsu/README.md`.

## Release
See [release.md](release.md) for the release workflow and tagging steps.
