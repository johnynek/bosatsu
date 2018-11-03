# The Bosatsu Programming Language

## Design Philosophy and Goals
Bosatsu has several main themes:

1. correctness
1. simplicity
1. usability

These are roughly in priority order: correctness is more important than simplicity: we don't want to sacrifice correctness for usability. Similarly, generally, we prefer simplicity to usability. Some features which can increase usability make a language considerably more complex. In general, we will be skeptical of such trades.

The main user experience of the language is inspired by Python. It is a bit of a mystery that despite Python being currently one of the fastest growing languages, as well as already being incredibly popular, that it has not been more influential in language design. No other language in the top 20 most commonly used languages is very syntactically similar to Python. Put succinctly, we want Bosatsu to by a minimalistic statically typed Python.

A second goal is that package relationships should be clear in Bosatsu. All external names must be explicitly imported, and all values are private unless exported. To see the dependencies between packages no type inference is required.

## Problem domain for Bosatu
In the current programming landscape, the vast majority of code is either in Turing complete languages or very anemic data-structure languages (JSON, YAML, TOML). Bosatsu is exploring an intermediate space. Bosatu is not designed as a general purpose language. Bosatsu is a total language: all functions return values. This rules out both infinite loops and non-explicit failure possibilities (exceptions, crashes, etc...). The goal of Bosatsu is somewhat similar to Google's [Starlark](https://github.com/bazelbuild/starlark) language, a Python like language for configuration and embedded applications. Starlark is used to configure bazel builds, but you can imagine configuring systems currently configured with yaml or json, or replacing general purpose languages in systems configured with those (e.g. Scala's sbt is configured with scala).

## Differences with Starlark
The biggest three differences between Bosatsu and Starlark are:
1. Bosatsu is statically typed
1. all values are immutable
1. there are no side-effects.

The smaller differences come from adding features found in functional programming that Python lacks: a more powerful anonymous function syntax as well as the ability to define enums similar to Rust.

# Language Features
1. A static type system using a generalization of [Hindley-Milner (or Damas-Milner)](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) published in [this paper by Simon Peyton Jones et. al.](https://www.microsoft.com/en-us/research/publication/practical-type-inference-for-arbitrary-rank-types/)
1. data classes or named tuples, which we call structs following Rust
1. sum types, which we call enums following Rust.
1. a package system with explicit import and export syntax.
1. a highly Python-inspired syntax including function definition, literal integers and strings, literal lists, list comprehensions, tuples [(todo)](https://github.com/johnynek/bosatsu/issues/18) and (string key) dictionaries [(todo)](https://github.com/johnynek/bosatsu/issues/14). Generally, if a syntax is possible to support in the current type system with immutable values, we generally intend to support it.

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
We plan to support Ruby's string interpolation syntax:
```
profile = 'my favorite animal is #{animal}'
```

### Lists

Like Python, Bosatsu supports a syntax for literal lists:

```
favorite_animals = ["elephant", "humpback whale", "dog"]
```
In addition to ability to construct lists from items, we can also "splice" in other lists:

```
new_favorites = ["mammoth", *favorite_animals]
```

Just as we can build up lists literally, we can also use match to tear them down:
```
top_animal = match new_favorites:
  [most_fav, *rest]: most_fav
  []: "no favorites, :("
```

## Variables
All bindings start with a lowercase character and do not contain whitespace.

All bindings point to immutable values, because all values are immutable in Bosatsu. Bindings, however, are almost like mutable variables due to the ability to shadow:

```

def increment_by_2(x):
  x = add(x, 1)
  x = add(x, 1)
  x

```
This is a legal program. Don't think of x being mutated (since it isn't if it is captured by an anonymous function). Think of each new line being a new variable that happens to have the same value.

We recommend not changing the type of a name in a given scope (and may enforce this in the future).


## Functions
As we have seen, each function returns a single value which is the final expression in the function block.

All functions internally are functions of a single value returning a single value. We have syntax to
call multiple variable functions, but that is just syntactic sugar over curried functions.

So, we can write:
```
def add_with_three(x, y):
  add_Int(add_Int(x, y), 3)
```
using the `add_Int` function from the `Bosatsu/Predef`. Bosatsu also has anonymous function syntax similar to Haskell. The above is exactly the same as:
```
add_with_three = \x, y -> add_Int(add_Int(x, y), 3)
```
or even

```
add_with_three = \x -> \y -> add_Int(add_Int(x, y), 3)
```
Think of `\` ans an ASCII version of `λ`.

All of these functions have type `Int -> Int -> Int`, which is to say, given two integers (the two on the left) we can produce a new integer (always! remember Bosatsu is total, no exceptions!).

### Method syntax
Bosatu does not have methods, only functions, it does however have method syntax.

```
form1 = add(x, y)
form2 = x.add(y)
```
The above to forms are equivalent. `foo.bar` means pass `foo` as the first argument to `bar`. Since all functions are functions of a single argument:

```
inc10 = add(10)
inc10_again = 10.add
```
both of these two values `inc10` and `inc10_again` have the type `Int -> Int` and do the same thing: they add `10` to an integer.

## Custom Data Types
All type names start with an uppercase character. We can think of a definition as two things:

1. defining a new type
2. defining a set of constructor functions to create that type

### Structs
In the case of a `struct`, there is a single constructor function that can take a number of arguments which we can think of as fields to the struct. The constructor function has the same name as the type for `struct`s.

```
struct HashResult(as_string: String)

struct File(name: String, modified_time: Int, size: Int, hash: HashResult)

my_file = File("readme.txt", 1540951025, 10240, HashResult("b9f32037daab5a0aff8e437ff1dd0f0d"))

```
We can think of `struct`s as named tuples. There are no methods in bosatsu, only functions. We cannot define methods on `struct`s or `enum`s. The only thing we can do with a struct or enum is match it:
```
nm = match my_file:
  File(name, _, _, ): name
```

All matches in Bosatsu are total: one of the branches must match. We intend to support destructuring style syntax when matches are total:
```
File(nm, _, _, _) = my_file
```
as a synonym for the above code.

In the above we have added a type annotation for each field. If we omit a type annotation, we assume any type will do so we create a new type parameter. If we need to have alignment in two type parameters, we need to explicitly name them:
```
# Here is a Tuple with 2 items of *any* type
struct Tuple2(fst, snd)

num_str = Tuple2(1, "1")

# Here is a Pair: both have the same type:
struct Pair(fst: a, snd: a)

neg = Pair(1, -1)

```
Types are assigned left to right when they are omitted.

Structs may not be recursive. The following is not allowed:
```
struct W(fn: W[a, b] -> a -> b)
```
Here `W` is in scope inside the definition of W. The types, like packages, must form an acyclic graph. If we allow types like the above, we actually open the door to recursion at the value level since we allow the y-combinator to be typed. By banning recursive types, the type of fix-point combinators becomes infinite, and thus ill-typed. This restriction is currently required to preserve totality in Bosatsu (a more advanced language can allow some recursion that it can prove does not violate totality).

An import recursive type we cannot write is `List`:
```
enum List:
  Empty, NonEmpty(head: a, tail: List[a])
```
While this is a restriction, List is safely total as long as we only allow construction of finite lists. Thus, List and foldLeft are supplied as built-ins within `Bosatsu/Predef`.

Note, a kind of `HList` or heterogeneous list is not banned:
```
enum HList:
  Empty, NonEmpty(head, tail)
```
So, if you know statically the size of the list you can represent it with the above:
```
x = NonEmpty(0, NonEmpty(1, NonEmpty(2, Empty)))
```
However, this should rarely be needed due to the built in `List` support.


### Enums
While `struct` represents a named tuple or product type, an `enum` represents a named sum type. This is something similar to `union` in C, `sealed trait` subclasses in Scala, or its namesake `enum` from Rust. Perhaps the most famous example is below:

```
enum Option:
  None, Some(get)
```
In this example, we have omitted the type for `get`, so it can be any type. There is thus one type parameter on `Option` because `Some` requires it.

Consider if we want a type to be the same on two branches: we just use the same type parameter name. There is only one namespace per enum for such variables:
```
enum GoodOrBad:
  Bad(bad: a), Good(good: a)
```
In the above example, `GoodOrBad` has a single type parameter `a`. So `Bad(1)` and `Good(1)` are of the same type.

## Types
Type variables are all lower case. There is an explicit syntax for function type: `a -> b`. There is
support for quantification: `forall a. a -> List[a]`.

## The Bosatsu Predef
The predef includes List, Option, Either types and some associated functions.

## Packages
All names are private unless exported. All names from external packages must be imported. There can be no cycles in the dependency graph of Bosatsu packages.

```
package Animals/Favorites
export [ mammals, birds ]

mammals = ["elephant", "whale", "dog"]

birds = ["African grey parrot", "macaw"]

```
In some other package:
```
package Animals/Report

import Animals/Favorites [ mammals ]
export [ most_fav ]

most_fav = match mammals:
  [head, *tail]: head
  []: "who knows?"

```

We export types slightly differently. We can export just the type, or the type and the constructors. To export the type and the constructors use `Type()` where as `Type` exports only the type to be visible.

```

package Great/Types

export [ ClearOption(), OpaqueOption, foldO, noneO, someO ]

enum ClearOption:
  CNone, CSome(get)

enum OpaqueOption:
  ONone, OSome(get)

noneO = ONone
def someO(a): OSome(a)

def foldO(oo, if_none, if_some):
  match oo:
    ONone: if_none
    oSome(a): if_some(a)

```
Here ClearOption exports all its constructors so they can be pattern matched upon. In the OpaqueOption example, we export functions to create the opaque option, and fold function to tear it down, but we can't explicitly pattern match on the types.

Sometimes such opacity is useful to enforce modularity.

## External functions and values
There is syntax for declaring external values and functions. This is obviously dangerous since it
gives the user a chance to violate totality. Use with caution.


As we discussed above, we cannot implement `List` in Bosatu. In the predef, we can find the following:
```
external struct List[a]
external def emptyList -> List[a]
external def consList(head: a, tail: List[a]) -> List[a]
external def foldLeft(lst: List[a], init: b, fn: b -> a -> b) -> b
external def range(exclusive_upper: Int): List[Int]
```
These functions are not implemented in Bosatsu, and are supplied by the runtime. They are all total functions, but have no representation in Bosatsu currently.

External values and types work exactly like internally defined types from any other point of view.

# Note on Totality
Totality is an interesting property that limits a language below being turing complete, however, it is not obvious if that is much of a limit. People often criticize fully general languages for configuration, but it is not clear that non-termination or partial functions are the problems.

One can easily argue that total languages are still too big. For instance imagine a function with type `a -> Either[a, b]` which takes an `a` and either returns a final `b` value or the next step `a` that we should call again. If we have a non-total language we could repeated call the function when we have left to build `a -> b` In a total language, we could build a
```
def repeat(n: Int, fn: a -> Either[a, b]) -> (a -> Either[a, b])
```
to repeat `n` times the fn unless we find a b and build a function of the same type that loops more times. By repeatedly applying this function: `repeat(1000, repeat(1000, ... repeat(1000, fn)...` we can make an exponentially large number of attempts. So we could easily build a function that would take `2^128` attempts which would take longer than the life of the sun to complete. Is this meaningfully different from an infinite loop?

In this context, one might argue that lack of *any* recursion is the stronger limit than totality in Bosatsu. For instance, we could [add support for structural recursion](https://github.com/johnynek/bosatsu/issues/87), however that might cut against the goal of being a minimal language. Bosatsu is total, but it is not the largest possible total language. That limitation might be a good thing when we are seeking a minimal language. We might want the smallest language we can express common configurations and get some benefit of reuse and composition.

