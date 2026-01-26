# The Bosatsu Programming Language
![Codecov](https://img.shields.io/codecov/c/github/johnynek/bosatsu.svg?style=flat-square)

Bosatsu (菩薩) is the transliteration in Japanese of the sanskrit bodhisattva.
A bodhisattva is someone who can reach enlightenment but decides not to, to
help others achieve that goal.  -- Wikipedia

Bosatsu is a simple, non-turing complete language designed for configuration, queries and scripting. It
borrows from [Python](https://www.python.org/), [Haskell](https://www.haskell.org/),
[Dhall](https://hackage.haskell.org/package/dhall) and [Rust](https://www.rust-lang.org/en-US/).

Please see the [documentation site](https://johnynek.github.io/bosatsu/)
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

## Development notes:

Bosatsu is developed in Scala. We use `sbt` as the build system. To build
bosatsu, run `sbt cli/assembly` and then the script `./bosatsuj` should print
help (see the documentation link above for more help). `sbt test` should run
all the tests.

### How to run the benchmarks
At the `sbt` prompt:
```
bench/jmh:run -i 3 -wi 3 -f1 -t1
```
or for a specific benchmark
```
bench/jmh:run -i 3 -wi 3 -f1 -t1 .*SomeBench.*
```

## Release
See [release.md](release.md) for the release workflow and tagging steps.
