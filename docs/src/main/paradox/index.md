# Bosatsu Documentation

@@@ index
* [Design Philosophy](design.md)
* [Design Docs](design-docs/index.md)
* [Getting Started](getting_started.md)
* [Language Guide](language_guide.md)
* [Recursion In Bosatsu](recursion.md)
* [Bosatsu for JSON Generation](generating_json.md)
* [Compiling to Python](transpile_python.md)
@@@

Bosatsu (菩薩) is the transliteration in Japanese of the sanskrit bodhisattva.
A bodhisattva is someone who can reach enlightenment but decides not to, to
help others achieve that goal.  -- Wikipedia

Bosatsu is a simple, non-turing complete language designed for configuration, queries and scripting. It
borrows from [Python](https://www.python.org/), [Haskell](https://www.haskell.org/),
[Dhall](https://hackage.haskell.org/package/dhall) and [Rust](https://www.rust-lang.org/en-US/).

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
# >>> sum(i for i in xrange(1000) if keep(i))
# 233168
#
# bosatsu version here
computed = sum([i for i in range(1000) if keep(i)])

test = Assertion(computed == 233168, "expected 233168")
```

Bosatsu is a new language with the following main features:

1. a highly Python-inspired syntax including function definition, literal integers and strings, literal lists, list comprehensions, tuples and (string key) dictionaries. Generally, if a syntax is possible to support in the current type system with immutable values, we generally intend to support it.
1. powerful pattern matching
1. A static type system using a generalization of [Hindley-Milner (or Damas-Milner)](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) published in [this paper by Simon Peyton Jones et. al.](https://www.microsoft.com/en-us/research/publication/practical-type-inference-for-arbitrary-rank-types/)
1. data classes or named tuples, which we call structs following Rust
1. sum types, which we call enums following Rust.
1. a package system with explicit import and export syntax.

There are also some un-features, or items we don't currently and may never support

1. There is very limited recursion. We only allow recursion which can never be used to make an
   infinite loop.
1. Data structures can only be recursive in covariant positions (not the
   arguments of functions), and recursive functions have to be a certain form that will always
   terminate
1. There is no `while` syntax. The closest we have are tail-recursive functions which recurse on sub-values of the inputs. There is a design sketch of automatically translating for loops on lists into folds [issue 20](https://github.com/johnynek/bosatsu/issues/20).
