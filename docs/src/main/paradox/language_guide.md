# Language Guide
This guide is a quick tour of Bosatsu syntax. For installation and running code, see
[Getting started](getting_started.html).

## Quick start (5 minutes)
A tiny, complete file:

```bosatsu
package Demo/Hello

enum Mood: Happy, Sad

def greet(m: Mood) -> String:
  match m:
    case Happy: "hello"
    case Sad: "cheer up"

test = Assertion(greet(Happy) matches "hello", "greet")
```

Run tests from the repo root:

```sh
./bosatsu lib test
```

## Source files and packages
Each file declares exactly one package at the top:

```bosatsu
package Animals/Favorites
```

All external names must be explicitly imported, and all values are private unless
exported:

```bosatsu
package Animals/Report

from Animals/Favorites import mammals
export most_fav

most_fav = match mammals:
  case [head, *tail]: head
  case []: "who knows?"
```

## Literals

### Signed big integers
Like Python, Bosatsu supports literal signed big integers.
```
x = 1
y = 42
z = -1024
```

### Unicode strings
Bosatsu files are UTF-8 files and between `"` or `'` characters we can create any
unicode string:
```
message1 = 'bar'
message2 = "this has 'single quotes'"
```

There is string interpolation syntax:
```
profile = "my favorite animal is ${animal}"
first = "first letter: $.{letter}"
```
Where `animal` would be any expression that has type `String`, and `letter` has
type `Char`.

### Character literals
Characters are single Unicode codepoints written with a leading dot:
```
letter = .'x'
```

### Lists and list comprehensions
Like Python, Bosatsu supports a syntax for literal lists:

```
favorite_animals = ["elephant", "humpback whale", "dog"]
```

Unlike Python, but like all of Bosatsu, lists are immutable values.
In addition to ability to construct lists from items, we can also "splice" in
other lists:

```
new_favorites = ["mammoth", *favorite_animals]
```

We can also build lists with comprehensions:

```
squares = [times(i, i) for i in range(10)]
```

Just as we can build up lists literally, we can also use match to tear them down
which we discuss more later:

```
top_animal = match new_favorites:
  case [most_fav, *rest]: most_fav
  case []: "no favorites, :("
```

## Bindings
All bindings start with a lowercase character and do not contain whitespace.

All bindings point to immutable values, because all values are immutable in
Bosatsu. Bindings, however, are almost like mutable variables due to the ability
to shadow:

```
def increment_by_2(x):
  x = add(x, 1)
  x = add(x, 1)
  x
```

This is a legal program. Don't think of x being mutated (since it isn't if it is
captured by an anonymous function). Think of each new line being a new variable
that happens to have the same value.

Note this is a difference from python: in python capturing works by reference, not
by value, so if the original value is changed, so is the capture. Not so in
Bosatsu: lexical scope is always in play.

We recommend not changing the type of a name in a given scope (and may enforce
this in the future).

### Block expressions
You can group a sequence of bindings and defs in parentheses to produce a
value. The last expression is the result:
```
result = (
  x = 1
  y = x.add(1)
  (x, y)
)
```

A related shorthand is left-apply (`<-`). It rewrites a line of the form
`pat <- expr` into a function application `expr(pat -> ...)`, so `expr` must
accept a function (continuation) as its argument:
```
def let(arg): in -> in(arg)

result = (
  x <- let(3)
  x.add(1)
)
# same as: let(3)(x -> x.add(1))
```
Because it is pure syntax, this can be used to model async/await style code or
monadic do/for syntax (the semantics come from the function you apply to). For
example, if `await` is defined in terms of `flat_map`, you can write:
```
def await(p): fn -> p.flat_map(fn)

main = (
  args <- read_env.await()
  _ <- println("args = ${args}").await()
  pure(0)
)
```

## Functions
As we have seen in above examples, each function returns a single value which is
the final expression in the function block. The reason for this difference with
Python is that in Bosatsu, there are no side-effects. There is no reason to
compute something if it is not going to be returned, and as such, the last line
of every def is the return value. Like Python chooses to omit return in `lambda`
expressions, we remove `return` entirely from Bosatsu.

Bosatsu has 32 function arities (Fn1..Fn32). A `def` with N parameters defines
an FnN, and each arity is a distinct type. There is no automatic currying. If
you want a function that returns a function, write that explicitly with a
lambda.

So, we can write:
```
def add_with_three(x, y):
  add(add(x, y), 3)
```

Bosatsu has anonymous function syntax. The above is almost exactly the same as:
```
add_with_three = (x, y) -> add(add(x, y), 3)
```

A few forms to know:

```
inc = x -> add(x, 1)                 # Fn1
add2 = (x, y) -> add(x, y)           # Fn2
add_curried = x -> y -> add(x, y)    # Fn1 returning Fn1
add_tuple = ((x, y)) -> add(x, y)    # Fn1 taking a tuple
```

Note the distinction between `(x, y) -> ...` (Fn2) and `((x, y)) -> ...`
(Fn1 that takes a tuple).

Function parameters are patterns, so you can destructure arguments directly:
```
def fst((a, _)): a
sum = ((x, y)) -> add(x, y)
```

### Scope difference for defs
Unlike normal bindings, defs ARE in scope in their body. However, in order to
prevent unbounded loops there are strict rules on accessing defs inside
themselves. If you don't intend to write a recursive def, then you can never
reuse or access the def's name anywhere inside the body. Specifically, shadowing
the def name is not allowed. To write a recursive def see the section on
recursive functions below.

### Method syntax
Bosatsu does not have methods, only functions, it does however have method
syntax.

```
form1 = add(x, y)
form2 = x.add(y)
```

The above two forms are equivalent. `foo.bar` means pass `foo` as the first
argument to `bar`. This scales to more arguments:

```
form3 = f(x, y, z)
form4 = x.f(y, z)
```

### Operators
Operators are just functions whose names are written with the `operator`
prefix, and infix expressions call those functions:
```
def operator +(a, b): a.add(b)
operator == = eq_Int

x = 1 + 2
y = 1 == 1
```

You can also import or rename operators:
```
from Bosatsu/Predef import add as operator +
```

### Recursive functions
Bosatsu supports recursion through `recur`, but only in forms the compiler can
prove terminate. A simple structural example:
```
def len(lst):
  recur lst:
    case []: 0
    case [_, *tail]: len(tail).add(1)
```

Tail-recursive style is common:
```
def len(lst):
  def loop(acc, lst):
    recur lst:
      case []: acc
      case [_, *tail]: loop(acc.add(1), tail)

  loop(0, lst)
```

For full recursion rules and advanced patterns (fuel, divide-and-conquer with a
size bound, string recursion, trees, Ackermann-style nested recursion), see
[Recursion in Bosatsu](recursion.html).

## Loops (with `recur`)
Bosatsu has no `while` or `for`. Loops are recursive defs using `recur`. In
practice, most loops are either:
1. structural recursion on subvalues (like a list tail or tree branch), or
1. explicit fuel recursion on a decreasing `Nat`.

See [Recursion in Bosatsu](recursion.html) for detailed examples from
`test_workspace`.

## Pattern Matching
Bosatsu has powerful pattern matching. You can match on literal values, strings
and lists in addition to user defined types as discussed later. Use `case` in
every match branch.

Here are some examples:
```
result0 = match "foo":
  case "baz":
    "we matched baz"
  case "foo":
    "we matched foo"
  case _:
    "hmmm, didn't match"

result1 = match "foo":
  case "f${rest}":
    "we start with f followed by ${rest}"
  case _:
    "hmmm, didn't match"

result2 = match ["foo", "bar"]:
  case ["f${rest}", "baz"]:
    "this won't match"
  case ["f${rest}", *_]:
    "we start with f followed by ${rest}"
  case _:
    "hmmm, didn't match"

result3 = match [False, True, True]:
  case [*_, True, *_]: "at least one is true"
  case _: "all items are false, or the list is empty"

result4 = match [False, True, True]:
  case [*_, True, *tail]:
    match tail:
      case []: "True with tail empty"
      case [True]: "True with tail == [True] (this branch matches)"
      case _: "True with something else"
  case _: "all items are false, or the list is empty"
```

A common shorthand for checking if something matches is:
```
long = match ["foo", "bar"]:
  case ["foo", *_]: True
  case _: False

short = ["foo", "bar"] matches ["foo", *_]
```
The caveat is that you cannot have any bindings in the pattern when used
as a matches expression.

Patterns can bind the matched value with `as`:
```
case Some(v) as whole: whole
case [*_, 4 as four]: four
```

Patterns can also be annotated with types using `pattern: Type`:
```
case (_: Int) as last: last
```
These annotations only guide type inference; they are not runtime checks.
Bosatsu has no runtime type checks—types are fully determined at compile time.

In string patterns, `${name}` binds a substring and `$.{name}` binds a single
`Char`. Use `_` to ignore the matched part:
```
case "${_}$.{c}": c
```

List patterns support "globs" using `*`. `*_` matches any (possibly empty)
sublist, and `*name` binds that sublist. Globs can appear anywhere in the list:
```
case [*_, True, *_]: "contains True"
```

A key feature of Bosatsu pattern matching: at least one branch must match.
The compiler will check this and fail to compile any code that could fail
to match at runtime. The error will list the branches that are not covered.

## Custom Data Types
All type names start with an uppercase character. We can think of a definition
as two things:

1. defining a new type
2. defining a set of constructor functions to create that type

### Structs
In the case of a `struct`, there is a single constructor function that can take
a number of arguments which we can think of as fields to the struct. The
constructor function has the same name as the type for `struct`s.

```
struct HashResult(as_string: String)

struct File(name: String, modified_time: Int, size: Int, hash: HashResult)
```

There are two ways to create a struct or enum value: by treating it as a
function or as a named record. The analogy you can think of is that structs are
either named tuples, or named dictionaries

```
my_file = File("readme.txt", 1540951025, 10240, HashResult("b9f32037daab5a0aff8e437ff1dd0f0d"))

# same using record syntax
my_file_rec = File {
  name: "readme.txt",
  modified_time: 1540951025,
  size: 10240,
  hash: HashResult("b9f32037daab5a0aff8e437ff1dd0f0d")
}
```
You can only use one syntax: all unnamed tuple-like, or all names dict-like.

Like Rust, if a value is already in scope matching a field name, we can omit
the colon:
```
name = "readme.txt"
size = 10240
hash = HashResult("b9f32037daab5a0aff8e437ff1dd0f0d")

my_file_rec = File { name, modified_time: 1540951025, size, hash }
```

There are no methods in Bosatsu, only functions. We cannot define methods on
`struct`s or `enum`s. The only thing we can do with a struct or enum is match it:
```
nm = match my_file:
  case File(name, _, _, _): name

# same as the above but ignoring trailing fields
nm = match my_file:
  case File(name, ...): name

# same as the above but using records
nm = match my_file:
  case File { name: n, ... }: n

# same, but omit the colon to bind the field name
nm = match my_file:
  case File { name, ... }: name
```

All matches in Bosatsu are total: one of the branches must match.
We support destructuring style syntax when matches are total:
```
File(nm, ...) = my_file

# or using record syntax to bind the name to nm
File { name: nm, ... } = my_file

# or using short record syntax binding name below this call
File { name, ... } = my_file
```
as a synonym for the above code.

In the above we have added a type annotation for each field. If we omit a type
annotation, we assume any type will do so we create a new type parameter. If we
need to have alignment in two type parameters, we need to explicitly name them:
```
# Here is a Tuple with 2 items of *any* type
struct Tuple2(fst, snd)

num_str = Tuple2(1, "1")

# Here is a Pair: both have the same type:
struct Pair(fst: a, snd: a)

neg = Pair(1, -1)
```

Sometimes it is important to assign type parameters in a specific order. In
those cases we can manually write them:
```
# put the right side type parameter first, otherwise, we would have gotten b
# first in left to right order.
struct Tuple[a, b](fst: b, snd: a)
```

Types are assigned left to right when they are omitted.

### Limited Recursion in Custom Data Types
Types must form a directed acyclic graph (DAG) with the exception that they
may refer to themselves in covariant positions (i.e. not in the inputs to
functions).

An example of a type that does not refer to itself in a covariant position is:
```
struct W(fn: W[a, b] -> a -> b)
```
Here `W` is in scope inside the definition of W. The types, like packages, must
form an acyclic graph. If we allow types like the above, we open the door to
recursion at the value level since we allow the Y-combinator to be typed. By
banning some recursive types, the type of fix-point combinators becomes infinite,
and thus ill-typed. This restriction is currently required to preserve totality
in Bosatsu (a more advanced language can allow more recursion that it can prove
 does not violate totality).

An important recursive type we can write is `List`. In Predef we will find a
standard linked list:
```
enum List:
  EmptyList, NonEmptyList(head: a, tail: List[a])
```

Data-structures have two simple rules:
1. they must form a DAG
2. if they refer to themselves, they do so in covariant positions (roughly, not
   as inputs to functions).

### Tuples
A built-in family of structs are tuples, which you can think of as either lists
with potentially a different type in each position and a statically known size,
or as an anonymous struct. We write and match them exactly as you might imagine:
```
x = (1, "2", ["three"])
# this match is total, so we can do a destructuring assignment:
(a, b, c) = x
# or we can use match syntax if that is more appropriate
match x:
  case (x, y, z): do_something(x, y, z)
```
The 0-arity tuple is written `()` and has type `Unit`.
Tuples are also real predef types and can be named explicitly as `Tuple1` up to
`Tuple32` (there is no `Tuple0`; `Unit` fills that role), with fields `item1`,
`item2`, and so on. That lets you select parts of a tuple using record-style
patterns:
```
Tuple3 { item2, ... } = (1, 2, 3)
# now item2 = 2
```
This pattern is handy when you only need specific tuple positions.

### Enums
While `struct` represents a named tuple or product type, an `enum` represents a
named sum type. This is something similar to `union` in C, `sealed trait`
subclasses in Scala, or its namesake `enum` from Rust. Perhaps the most famous
example is below:

```
enum Option:
  None, Some(get)
```
In this example, we have omitted the type for `get`, so it can be any type.
There is thus one type parameter on `Option` because `Some` requires it.

Consider if we want a type to be the same on two branches: we just use the same
type parameter name. There is only one namespace per enum for such variables:
```
enum GoodOrBad:
  Bad(bad: a), Good(good: a)
```
In the above example, `GoodOrBad` has a single type parameter `a`. So `Bad(1)`
and `Good(1)` are of the same type.

### Pattern Matching on Custom Types
As we have already seen examples of, Bosatsu features powerful pattern matching.
Literal values, such as integers or strings, as well as lists and tuples can
appear in pattern matches. Patterns can be nested and combined in unions. There
is a special wildcard term that can appear in a pattern match that is written as
`_`. The wildcard matches everything but introduces no new bindings.

One restriction is that all match expressions in Bosatsu must be complete
matches. It is a compilation error if a pattern match can fail. Remember,
Bosatsu is a total language, so we must check for match totality in order to
preserve totality of our functions.

Here are some examples of pattern matches:
```
enum Either: Left(a), Right(b)

match x:
  case Left(Left(Left(_)) | Right(_)): 0
  case Left(Left(Right(_))): 1
  case Right(_): 2

match y:
  case Left(Left(x)) | Right(x):
    # here we require that the bindings in the left and right sides of
    # the union are the same and that they have the same type
    Some(x)
  case Left(Right(_)): None

# if we can write a match in a single arm, we can write it as a binding:
Left(x) | Right(x) = y
```

## Types
Type variables are all lower case. There is an explicit syntax for function
types: `a -> b` for Fn1. For higher arities, use tuple-style argument lists:
`(a, b) -> c` for Fn2, `(a, b, c) -> d` for Fn3, and so on.

To write a function that takes a tuple as its single argument, use an extra
layer of parentheses in both values and types. For example:

```
# Fn1 taking a tuple
add_tuple: ((Int, Int)) -> Int = ((x, y)) -> add(x, y)
```

There is support for universal quantification sometimes called generic values
or generic functions: `forall a. a -> List[a]`.

### Kinds and variance annotations
You can annotate type parameters with kinds and variance. `*` is the kind of
value types, and `* -> *` (or higher) are type constructors. Variance markers
`+*` and `-*` mark covariant and contravariant parameters:
```
enum NEList[a: +*]: NEOne(head: a), NEMore(head: a, tail: NEList[a])
struct Box[a: *](value: a)
def map[f: * -> *, a, b](fa: f[a], fn: a -> b) -> f[b]: ...
def wrap2[shape: (* -> *) -> *](s: shape[List]) -> shape[List]: s
```
For `struct`/`enum` definitions, kinds are inferred. For functions, kinds are
not currently inferred; unannotated type parameters are assumed to be `*`.

### Existential types
Bosatsu also supports existential quantification to hide a type parameter chosen
by the producer of a value. The syntax is `exists a. ...`, and you can quantify
multiple variables with commas, e.g. `exists a, b. ...`.

Here is a simple example that stores an internal type but keeps it opaque in the
public constructor:
```
enum Build[a: *]:
  Mapped(consume: exists b. (Build[b], b -> a))
  Map2(consume: exists b, c. (Build[b], Build[c], (b, c) -> a))
```

When you pattern match on a constructor that contains an existential, the hidden
type is scoped to that branch, so the parts that depend on it stay consistent:
```
enum FreeF[a]:
  Pure(a: a)
  Mapped(tup: exists b. (FreeF[b], b -> a))

def run[a](fa: FreeF[a]) -> a:
  recur fa:
    case Pure(a): a
    case Mapped((prev, fn)):
      fn(run(prev))
```

When to use existentials:
- To hide intermediate types in data constructors while still allowing later
  consumption (e.g. `Mapped`/`Map2` store an internal `b` or `c` plus functions
  that know how to use them).
- To model heterogeneous or stateful structures where the internal type varies
  but is not part of the public API (e.g. existential tails in custom list-like
  structures, or continuations with hidden state).
- To return or store values that must remain opaque to callers while keeping
  internal consistency inside a match branch.

## The Bosatsu Predef
The predef includes Int, String, List, Option, Either types and some associated
functions. It also defines the built-in test types `Assertion` and `TestSuite`.

## Packages
All names are private unless exported. All names from external packages must be
imported. There can be no cycles in the dependency graph of Bosatsu packages.

```
package Animals/Favorites
export mammals, birds

mammals = ["elephant", "whale", "dog"]

birds = ["African grey parrot", "macaw"]
```

In some other package:
```
package Animals/Report

from Animals/Favorites import mammals
export most_fav

most_fav = match mammals:
  case [head, *tail]: head
  case []: "who knows?"
```

You can group imports/exports with parentheses and rename with `as`:
```
from Bosatsu/Predef import (foldl_List as foldl, add as operator +)
export (foldl)
```

We export types slightly differently. We can export just the type, or the type
and the constructors. To export the type and the constructors use `Type()`
whereas `Type` exports only the type to be visible.

```
package Great/Types

export ClearOption(), OpaqueOption, foldO, noneO, someO

enum ClearOption:
  CNone, CSome(get)

enum OpaqueOption:
  ONone, OSome(get)

noneO = ONone
def someO(a): OSome(a)

def foldO(oo, if_none, if_some):
  match oo:
    case ONone: if_none
    case OSome(a): if_some(a)
```
Here ClearOption exports all its constructors so they can be pattern matched
upon. In the OpaqueOption example, we export functions to create the opaque
option, and a fold function to tear it down, but we can't explicitly pattern
match on the types.

Sometimes such opacity is useful to enforce modularity.

## External functions and values
There is syntax for declaring external values and functions. This is obviously
dangerous since it gives the user a chance to violate totality. Use with
caution.

An example function we cannot implement in Bosatsu is:
```
def int_loop(int_v: Int, state: a, fn: (Int, a) -> (Int, a)) -> a:
  if cmp_Int(int_v, 0) matches GT:
    (next_i, next_state) = fn(int_v, state)
    if cmp_Int(next_i, int_v) matches LT:
      # make sure we always decrease int_v
      int_loop(next_i, next_state, fn)
    else:
      next_state
  else:
    state
```
We cannot write this function, even though it is total, because Bosatsu cannot
prove that the loop terminates. The only recursions we can do are on values that
are substructures of inputs in the same position. This gives a simple proof
that the loop will terminate.

Instead, we implement this function in Predef as an external def that has to be
supplied to the compiler with a promise that it is indeed total.
```
external def int_loop(intValue: Int, state: a, fn: (Int, a) -> (Int, a)) -> a
```

External values and types work exactly like internally defined types from any
other point of view.

External defs are only supported in libraries implemented inside the compiler
repo. The functions must be implemented in the C runtime, and there is currently
no provision for providing external implementations from outside the compiler
repo. This restriction is intentional to preserve totality, and we do not expect
to lift it any time soon.

# Note on Totality
Totality is an interesting property that limits a language from being turing
complete, however, it is not obvious if that is much of a practical limit.
People often criticize fully general languages for configuration, but it is not
clear that non-termination or partial functions are the problems.

One can easily argue that total languages are still too big. For instance
imagine a function with type `a -> Either[a, b]` which takes an `a` and either
returns a final `b` value or the next step `a` that we should call again. If we
have a non-total language we could repeatedly call the function when we have
left to build `a -> b`. In a total language, we could build a
```
def repeat(n: Int, fn: a -> Either[a, b]) -> (a -> Either[a, b])
```
to repeat `n` times the fn unless we find a b and build a function of the same
type that loops more times. By repeatedly applying this function:
`repeat(1000, repeat(1000, ... repeat(1000, fn)...` we can make an
exponentially large number of attempts. So we could easily build a function that
would take `2^128` attempts which would take longer than the life of the sun to
complete. Is this meaningfully different from an infinite loop?

Our view is that the main value of totality is that we know that all functions
can complete and return a value of their declared type. Therefore, the only
possible error in running is exhausting memory, which is a risk in virtually
every programming language.
