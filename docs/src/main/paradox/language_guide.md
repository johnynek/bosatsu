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
There is string interpolation syntax:
```
profile = 'my favorite animal is ${animal}'
```
Where `animal` would be any expression that has type `String`.

### Lists

Like Python, Bosatsu supports a syntax for literal lists:

```
favorite_animals = ["elephant", "humpback whale", "dog"]
```

Unlike Python, but like all of Bosatsu, lists are immutable values.
In addition to ability to construct lists from items, we can also "splice" in other lists:

```
new_favorites = ["mammoth", *favorite_animals]
```

Just as we can build up lists literally, we can also use match to tear them down which we discuss
more later:
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

Note this is a difference from python: in python capturing works by reference, not by value, so if
the original value is changed, so is the capture. Not so in Bosatsu: lexical scope is always in
play.

We recommend not changing the type of a name in a given scope (and may enforce this in the future).

## Pattern Matching
Bosatsu has powerful pattern matching. You can match on literal values, strings and lists in
addition to user defined types as discussed later.

Here are some examples:
```
result0 = match "foo":
  "baz":
    "we matched baz"
  "foo":
    "we matched foo"
  _:
    "hmmm, didn't match"

result1 = match "foo":
  "f${rest}":
    "we start with f followed by ${rest}"
  _:
    "hmmm, didn't match"

result2 = match ["foo", "bar"]:
  ["f${rest}", "baz"]:
    "this won't match"
  ["f${rest}", *_]:
    "we start with f followed by ${rest}"
  _:
    "hmmm, didn't match"

result3 = match [False, True, True]:
  [*_, True, *_]: "at least one is true"
  _: "all items are false, or the list is empty"

result4 = match [False, True, True]:
  [*_, True, *tail]:
    match tail:
      []: "True with tail empty"
      [True]: "True with tail == [True] (this branch matches)"
      _: "True with something else"
  _: "all items are false, or the list is empty"
```

A common shorthand for checking if something matches is:
```
long = match ["foo", "bar"]:
  ["foo", *_]: True
  _: False

short = ["foo", "bar"] matches ["foo", *_]
```
The caveat is that you cannot have any bindings in the pattern when used
as a matches expression.

A key feature of Bosatsu pattern matching: at least one branch must match.
The compiler will check this and fail to compile any code that could fail
to match at runtime. The error will list the branches that are not covered.

## Functions
As we have seen in above examples, each function returns a single value which is the final expression in the function block.
The reason for this difference with Python is that in Bosatsu, there are no side-effects. There is
no reason to compute something if it is not going to be returned, and as such, the last line of
every def would be a return. Like Python chooses to omit return in `lambda` expressions, we choose
to remove `return` entirely from bosatsu.

All functions internally are functions of a single value returning a single value. We have syntax to
call multiple variable functions, but that is just syntactic sugar over curried functions.

So, we can write:
```
def add_with_three(x, y):
  add_Int(add_Int(x, y), 3)
```
using the `add_Int` function from the `Bosatsu/Predef`. Bosatsu also has anonymous function syntax similar to Haskell. The above is almost exactly the same as:
```
add_with_three = \x, y -> add_Int(add_Int(x, y), 3)
```
or even

```
add_with_three = \x -> \y -> add_Int(add_Int(x, y), 3)
```
Think of `\` ans an ASCII version of `λ`.

All of these functions have type `Int -> Int -> Int`, which is to say, given two integers (the two on the left) we can produce a new integer (always! Remember Bosatsu is total, no exceptions!).

### Scope difference for defs
Unlike normal bindings, defs ARE in scope in their body. However, in order to prevent unbounded
loops there are strict rules on accessing defs inside themselves. If you don't intend to write
a recursive def, then you can never reuse or access the defs name anywhere inside the body.
Specifically, shadowing the def name is not allowed. To write a recursive def see the section
on recursive functions below.

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

### Recursive functions
We have very limited support for recursion so that we may prove that all recursive functions
terminate. Here is an example:
```
def len(lst):
  recur lst:
    []: 0
    [_, *tail]: len(tail).add(1)
```
In the above, we see the `recur` syntax. This is a normal match with two restrictions: 1. it can only be
on a literal parameter of the nearest enclosing def, 2. a function may have at most one recur in a
body, which is not enclosed by a inner def. Inside the branches of such a recur, recursion
on the enclosing def is permitted if it meets certain constraints which prevent unbounded loops:

In at least one branch of the recur match there must be a recursive call that must take a
substructure of the argument the recur is matching on. In the above example, tail is a
substructure of lst, and is used in the same parameter as list appeared in.

These are strict rules, but they guarantee that each recursive function terminates in a finite
number of steps, and can never loop forever. The recommendation is to avoid recursive defs as much
as possible and limit the number of arguments as much as possible.

Tail recursive loops are optimized into loops, which are safe for cases where recursion
depth is high. If you can, prefer to use tail recursive loops:
```
def len(lst):
  def loop(acc, lst):
    recur lst:
      []: acc
      [_, *tail]: loop(acc.add(1), tail)

  loop(0, lst)
```

## Custom Data Types
All type names start with an uppercase character. We can think of a definition as two things:

1. defining a new type
2. defining a set of constructor functions to create that type

### Structs
In the case of a `struct`, there is a single constructor function that can take a number of arguments which we can think of as fields to the struct. The constructor function has the same name as the type for `struct`s.

```
struct HashResult(as_string: String)

struct File(name: String, modified_time: Int, size: Int, hash: HashResult)
```
There are two ways to create a struct or enum value: by treating it as a function or as a named
record. The analogy you can think of is that structs are either named tuples, or named dictionaries

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
You can only use one syntax: all unamed tuple-like, or all names dict-like.

Like Rust, if a value is already in scope matching a field name, we can omit the colon:
```
name = "readme.txt"
size = 10240
hash = HashResult("b9f32037daab5a0aff8e437ff1dd0f0d")

my_file_rec = File { name, modified_time: 1540951025, size, hash }
```

There are no methods in bosatsu, only functions. We cannot define methods on `struct`s or `enum`s. The only thing we can do with a struct or enum is match it:
```
nm = match my_file:
  File(name, _, _, _): name

# same as the above but ignoring trailing fields
nm = match my_file:
  File(name, ...): name

# same as the above but using records
nm = match my_file:
  File { name: n, ... }: n

# same, but omit the colon to bind the field name
nm = match my_file:
  File { name, ... }: name
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

In the above we have added a type annotation for each field. If we omit a type annotation, we assume any type will do so we create a new type parameter. If we need to have alignment in two type parameters, we need to explicitly name them:
```
# Here is a Tuple with 2 items of *any* type
struct Tuple2(fst, snd)

num_str = Tuple2(1, "1")

# Here is a Pair: both have the same type:
struct Pair(fst: a, snd: a)

neg = Pair(1, -1)

```
Sometimes it is important to assign type parameters in a specific order. In those
cases we can manually write them:
```
# put the right side type parameter first, otherwise, we would have gotten b first in left to right order.
struct Tuple[a, b](fst: b, snd: a)
```

Types are assigned left to right when they are omitted.

### Limited Recursion in Custom Data Types
Types must form a directed acyclic graph (DAG) with the exception that they
may refer to themselves in covariant positions (i.e. not in the inputs to functions).

An example of a type that does not refer to itself in the covariant position is:
```
struct W(fn: W[a, b] -> a -> b)
```
Here `W` is in scope inside the definition of W. The types, like packages, must form an acyclic graph.
If we allow types like the above, we actually open the door to recursion at the value level since we allow the y-combinator to be typed.
By banning some recursive types, the type of fix-point combinators becomes infinite, and thus ill-typed.
This restriction is currently required to preserve totality in Bosatsu (a more advanced language can allow more recursion that it can prove does not violate totality).

An import recursive type we can write is `List`. Indeed in Predef we will find a standard linked list:
```
enum List:
  Empty, NonEmpty(head: a, tail: List[a])
```

Data-structures have two simple rules:
1. they must form a DAG
2. if they refer to themselves, they do so in covariant positions (roughly, not as inputs to
   functions).

### Tuples
A built-in family of structs are tuples, which you can think of as either lists with potentially a
different type in each position and a statically known size, or as an anonymous struct. We write and
match them exactly as you might imagine:
```
x = (1, "2", ["three"])
# this match is total, so we can do a destructuring assignment:
(a, b, c) = x
# or we can use match syntax if that is more appropriate
match x:
  (x, y, z): do_something(x, y, z)
```

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

### Pattern Matching on Custom Types
As we have already seen examples of, Bosatsu features powerful pattern matching. Literal values,
such as integers or strings, as well as lists and tuples can appear in pattern matches. Patterns can
be nested and combined in unions. There is a special wildcard term that can appear in a pattern
match that is written as `_`. The wildcard matches everything but introduces no new bindings.

On restriction is that all match expressions in Bosatsu must be complete matches. It is a
compilation error if a pattern match can fail. Remember, Bosatsu is a total language, so we must
check for match totality in order to preserve totality of our functions.

Here are some examples of pattern matches:
```
enum Either: Left(a), Right(b)

match x:
  Left(Left(Left(_)) | Right(_)): 0
  Left(Left(Right(_))): 1
  Right(_): 2

match y:
  Left(Left(x)) | Right(x):
    # here we require that the bindings in the left and right sides of
    # the union are the same and that they have the same type
    Some(x)
  Left(Right(_)): None

# if we can write a match in a single arm, we can write it as a binding:
Left(x) | Right(x) = y
```

## Types
Type variables are all lower case. There is an explicit syntax for function type: `a -> b`. There is
support for universal quantification sometimes called generic values or generic functions: `forall a. a -> List[a]`.

## The Bosatsu Predef
The predef includes Int, String, List, Option, Either types and some associated functions.

## Packages
All names are private unless exported. All names from external packages must be imported. There can be no cycles in the dependency graph of Bosatsu packages.

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
  [head, *tail]: head
  []: "who knows?"

```

We export types slightly differently. We can export just the type, or the type and the constructors. To export the type and the constructors use `Type()` where as `Type` exports only the type to be visible.

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
    ONone: if_none
    oSome(a): if_some(a)

```
Here ClearOption exports all its constructors so they can be pattern matched upon. In the OpaqueOption example, we export functions to create the opaque option, and fold function to tear it down, but we can't explicitly pattern match on the types.

Sometimes such opacity is useful to enforce modularity.

## External functions and values
There is syntax for declaring external values and functions. This is obviously dangerous since it
gives the user a chance to violate totality. Use with caution.

An example function we cannot implement in bosatsu is:
```
def int_loop(int_v: Int, state: a, fn: Int -> a -> (Int, a)) -> a:
  if cmp_Int(int_v, 0) matches GT:
    (next_i, next_state) = fn(int_v, state)
    if cmp_Int(next_i, int_v) matches LT:
      # make sure we always decrease int_v
      int_loop(next_i, next_state, fn)
    else:
      next_state
  else:
    a
```
We cannot write this function, even though it is total, because Bosatsu cannot prove
that the loop terminates. The only recursions we can do are on values that are substructures
of inputs in the same position. This gives a simple proof that the loop will terminate.

Instead, we implement this function in Predef as an external def that has to be supplied to the
compiler with a promise that it is indeed total.
```
external def int_loop(intValue: Int, state: a, fn: Int -> a -> TupleCons[Int, TupleCons[a, Unit]]) -> a
```

External values and types work exactly like internally defined types from any other point of view.

