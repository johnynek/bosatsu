# The Bosatsu Programming Language

Bosatsu (菩薩) is the transliteration in Japanese of the sanskrit bodhisattva.
A bodhisattva is someone who can reach enlightenment but decides not to, to
help others achieve that goal.  -- Wikipedia

Bosatsu is a simple, non-turing complete language designed for configuration, queries and scripting. It
borrows from [Python](https://www.python.org/), [Haskell](https://www.haskell.org/),
[Dhall](https://hackage.haskell.org/package/dhall) and [Rust](https://www.rust-lang.org/en-US/).

## Brief Syntax Introduction

Syntax is like Python where it makes sense. Python lambda syntax is not
ideal so we borrow from haskell: `\x -> add(x, 1)`. Also, there is no `return`
so the last value in an expression is the result, similar to ruby or scala:
```python
def inc(x):
  add(x, 1)
```

Bosatsu is statically typed using the algorithm of [Practical type inference for arbitrary-rank types by Jones et. al](https://www.microsoft.com/en-us/research/publication/practical-type-inference-for-arbitrary-rank-types/).
This means except when defining data types, users generally never need to add explicit type annotations (though on
defs they are permitted as documentation).

Bosatsu borrows a Rust-like syntax for data types, of which there are two: struct and enum:
```rust
struct Counter(name: String, value: Integer, tag)

enum Result:
  Error(err)
  Value(value)

enum List:
  Empty
  NonEmpty(head: a, tail: List[a])
```

A struct is basically a named tuple. If a type annotation is left off, we assume any type
will do, which is to say the struct is parametric on that field.
An enum is a sum-type which has 1 or more constructor each of which syntactically follows
the rules of struct.
In the example above, we
don't need to name the types in Result. In fact, Result takes two type parameters, one for
the Error constructor and one for the Value constructor.

Unlike many functional languages, let bindings require no extra syntax and just look like assignment:
```
def incBy(y):
  x = y
  \z -> z.add(x)
```
Here we introduce a binding of x to the value of y. Then we return a lambda which increments
by the same amount.

Like Haskell, Bosatsu is a pure functional language: there is no mutation of values. Like Dhall,
Bosatsu is total, which means that all functions terminate in a finite (though possibly very
large) number of steps. This property is useful to restrict the power in the intended domains.

## Case matters in Bosatsu

All bindings must be to a name that has a lowercase first character. All types must have a name
that starts with an uppercase character. Constructors, the functions that create instances of
structs or enums also start with an upper case. Like bindings, all type parameters must be
lower case.

## Partially evaluated Functions and methods
Like Haskell, Bosatu naturally curries functions. Put another way, all functions
take one argument and return one value. So a function like:
```python
def foo(a, b, c):
  bar(a, b, c)

foo1 = \a -> \b -> \c -> bar(a, b, c)
```
Both, foo and foo1 are the same. Both can be called with 1 to 3 arguments:
```python
# all valid

f1 = foo(1)
frest1 = f1(2, 3)

f2 = foo(1, 2)
frest2 = f2(3)

f3 = foo(1, 2, 3)

# frest1 same as frest2 as f3
```

Even though there are only functions, syntactically a method-like syntax is nice. To this end
Bosatsu has a `.` operator. It is defined as: `a.f` being the same as `f(a)`. Combined with
the above natural partial application, that means `a.f(b, c)` is the same as `f(a, b, c)`.
So, like python, you should make the first parameter of a function the `self` parameter and
you can use a method like syntax.

## Packages
Each bosatsu file is a package which is declared at the beginning:
```
package List/Utils
```
We use the / character to separate package names. Each import must be explicitly named in
Bosatsu. This is intended to make tooling, simpler since simple search can always tell
where things come from:
```
import List/Util [ head, tail, last ]
```
All bindings in Bosatsu are private to the file unless exported:

```
package List/Utils
export [ head, List(), Option() ]

enum List:
  Empty
  NonEmpty(head: a, tail: List[a])

enum Option:
  None
  Some(a)

def head(list):
  match list:
    Empty:
      None
    NonEmpty(head, _):
      Some(head)

def isEmpty(list):
  match list:
    Empty:
      True
    NonEmpty(_, _):
      False
```
In the above example, `isEmpty` is not exported, so it is not accessble from other packages. On the other hand `head` `List()` and `Option()`
are exported. For structs and enums it is possible to export only the type as an opaque type or the type and all the constructors.
In the examples above we have exported the type and the constructors. If we had wanted to only export the type `Option` without the
ability to match or create new instances we would write `export [ Option ]`. This can be useful to hide implementation details
and instead supply functions to work with types.

## Use cases

Currently we have only implemented type-checking, the package system, and an interpreter to evalute expressions. This could
already be useful if you want to give some programmability to configuration that can load, type-check and evaluate the configuration
before executing the rest of the scala or java code.

We intend to add examples or using this to produce a case class value that the scala application takes as input.

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
