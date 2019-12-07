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

operator == = eq_Int
operator % = mod_Int

def operator ||(x, y):
  True if x else y

def keep(i):
  (i % 3 == 0) || (i % 5 == 0)

def sum(as): as.foldLeft(0, add)

# >>> sum(i for i in xrange(1000) if keep_fn(i))
# 233168
computed = [i for i in range(1000) if keep(i)].sum

test = Assertion(computed == 233168, "expected 233168")
```

For more details see the [language guide](docs/language_guide.md) in particular the [section on syntax](docs/language_guide.md#language-guide). For vim syntax files see [bosatsu.vim](https://github.com/johnynek/bosatsu.vim).

## Getting started
You can try basic expressions using this [in-browser Bosatsu compiler](https://johnynek.github.io/bosatsu/compiler/). To work locally, clone this repo, then build the compiler:
```
sbt cli/assembly
```
Now try running the above program:
```
./bosatsuj eval --main Euler/One::computed --input test_workspace/euler1.bosatsu
```
The command `./bosatsuj` has pretty complete help if you run it with no arguments. If you have [graal native-image](https://www.graalvm.org/docs/reference-manual/native-image/) on your path, after building with `sbt cli/assembly` you can build the native image:
```
./build_native.sh
```
Now you should have `./bosatsu` available which is *MUCH* faster to use than `./bosatsuj` which has to pay the JVM startup cost on each call, e.g.:
```
oscar@oscar-XPS-13-7390:~/oss/bosatsu$ time bosatsu eval --main Euler/One::computed --input test_workspace/euler1.bosatsu 
233168: Bosatsu/Predef::Int

real    0m0.161s
user    0m0.085s
sys     0m0.051s
oscar@oscar-XPS-13-7390:~/oss/bosatsu$ time ./bosatsuj eval --main Euler/One::computed --input test_workspace/euler1.bosatsu 
233168: Bosatsu/Predef::Int

real    0m2.260s
user    0m4.621s
sys     0m0.155s
```

## Use cases

Currently we have implemented type-checking, the package system, and an interpreter to evalute expressions and tests. This could
already be useful if you want to give some programmability to configuration that can load, type-check and evaluate the configuration
before executing the rest of the scala or java code.

### As a JSON templating engine

Using the `bosatsu json` command you can convert bosatsu expressions into json. Bosatsu can be used as a JSON generation
system, which could be useful for generating complex configurations in a way that has type-checking
and ability to compose. This gives you a programmatic way to
generate JSON in a limited language (no IO, no infinite recursion, totally reproducible).

### Bazel rules

This repo include bazel rules to call bosatsu so you can build libraries, write tests, and generate json using bosatsu
and [Bazel](https://github.com/bazelbuild/bazel/). For a working example see [this example](test_workspace/).
```
cd test_workspace
bazel build ...
cat bazel-build/testjson.json
```

## Future features

We would like to implement a number of features:

1. a REPL or Notebook experience.
2. a java backend and bazel rules which can call java and scala functions
3. a skylark backend to allow writing strongly typed bazel rules compiling to untyped skylark

## How to run the benchmarks
```
bench/jmh:run -i 3 -wi 3 -f1 -t1
```
or for a specific benchmark
```
bench/jmh:run -i 3 -wi 3 -f1 -t1 .*SomeBench.*
```
