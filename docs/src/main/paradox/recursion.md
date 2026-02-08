# Recursion in Bosatsu
Bosatsu is a total language. Recursion is restricted so every `def` is guaranteed
to terminate.

Loops in Bosatsu are intentionally stricter than loops in most programming
languages. We still support loops through `recur`, but each loop must have a
decreasing structural argument (or explicit decreasing fuel) so termination is
provable. This is required for the main safety goal: a sound type system we can
trust.

This page expands the short notes in the language guide with concrete patterns
from examples in the bosatsu compiler repository, plus background on the fuel
pattern and Bove-Capretta/domain-predicate styles.

## Terms Used In This Page
Papers in the references use a few terms repeatedly. Here is what they mean in
plain language:

1. Structural recursion (well-founded recursion):
   each recursive call is on a strictly smaller argument under a well-founded
   order (for example, list tail, tree child, predecessor `Nat`).
1. Fuel pattern (step-indexed style):
   add an explicit counter/fuel argument, decrement it on each recursive call,
   and stop when fuel reaches zero.
1. Accessibility predicate / domain predicate:
   a predicate describing inputs on which a recursive function terminates.
1. Bove-Capretta method:
   define a recursive function by structural recursion on evidence that the
   input is in the domain predicate; then separately prove totality by proving
   the domain predicate for all inputs of interest.

Bosatsu source code usually does not pass explicit proof terms. Instead, the
same idea appears operationally as carrying explicit bounds/companion values
(`Nat` sizes, depths, budgets) that witness termination.

## The `recur` Form
Recursion is expressed through `recur`, which is a restricted `match`:

```bosatsu
def len(lst: List[a]) -> Int:
  recur lst:
    case []: 0
    case [_, *tail]: len(tail).add(1)
```

At a high level:
1. `recur` matches a parameter of the nearest enclosing `def`.
1. Recursive calls are allowed in branches only when they are structurally
   smaller (or use an explicit decreasing fuel value).
1. These value-level restrictions are only part of totality. Bosatsu also
   restricts recursive types to covariant positions (issue #104:
   https://github.com/johnynek/bosatsu/issues/104), so type-level recursion is
   also constrained to preserve totality.

## Pattern 1: Structural Recursion On Lists
This is the most common pattern. Recur directly on the list and call the same
function on a tail.

In terminology above, this is structural/well-founded recursion where list
shape gives the decreasing measure for free.

From `List.bosatsu`:

```bosatsu
def for_all(xs: List[a], fn: a -> Bool) -> Bool:
  recur xs:
    case []: True
    case [head, *tail]:
      if fn(head): for_all(tail, fn)
      else: False
```

Other examples:
1. `exists`, `eq_List`, `zip`, `size1` in `List.bosatsu`.
1. `equal_List` in `recordset.bosatsu`.

## Pattern 2: Tail Recursion With Accumulators
Same structural decrease as Pattern 1, but with accumulator state for constant
stack in target runtimes that optimize tail loops.

From `List.bosatsu`:

```bosatsu
def size1(list, acc):
  recur list:
    case []: acc
    case [_, *t]: size1(t, Succ(acc))
```

Also used in `Nat.bosatsu` (`to_Int`, `times2`, `add`) and
`FibBench.bosatsu` (`list_len`).

## Pattern 3: Structural Recursion On Trees
Recur on tree subtrees (`left`, `right`) instead of list tails.

This style is especially useful when the tree-like structure is kept balanced.
In that case, recursive depth is logarithmic in input size (`O(log n)`), so deep
recursion risk is much lower. Many balanced tree-shaped data structures can use
this pattern.

This is again structural/well-founded recursion, with the tree structure
supplying the decrease relation.

From `AvlTree.bosatsu`:

```bosatsu
def fold_left_Tree(t: Tree[a], left_v: b, fn: (b, a) -> b) -> b:
  recur t:
    case Empty: left_v
    case Branch { key, left, right, ... }:
      v1 = fold_left_Tree(left, left_v, fn)
      v2 = fn(v1, key)
      fold_left_Tree(right, v2, fn)
```

Also used in `TreeList.bosatsu` (`fold_Tree`).

## Pattern 4: Fuel / Step-Indexed Recursion On `Nat`
The fuel pattern means one function input is designated as "fuel" and consumed
as recursion proceeds. A `Nat` is the most direct fuel: recurse on it, and
decrement (`Succ(prev) -> prev`) each step until `Zero`.

This is equivalent to placing an explicit upper bound on how many recursive
calls can happen.

When recursion is not naturally "one structural child", introduce a decreasing
`Nat` budget (fuel). Often this bound is computed from the real inputs. If the
bound computation is the same asymptotic order as the main algorithm, overall
runtime complexity is unchanged in `O(...)` terms (though constants may grow).

In the Bosatsu implementation, `Nat` used in this style is runtime-optimized to
an integer-like counter representation rather than a linked unary structure. In
practice, that means these loops compile down to counter-style code without
per-step allocation, so this pattern is usually fast enough not to dominate
runtime.

Bosatsu is aiming for very fast, practically usable performance, but it is not
trying to beat C or Rust on raw speed. The non-negotiable design goal is a sound
type system you can trust, so libraries can be composed fearlessly.

Seen this way, earlier structural recursion examples can be read as implicit
fuel patterns too: in list recursion the list value is fuel, and in tree
recursion the current subtree is fuel.

In the literature this is also called step-indexed (or petrol-driven)
evaluation.

From `BinNat.bosatsu`:

```bosatsu
def fib(b: BinNat) -> BinNat:
  def loop(n: Nat, cur: BinNat, next: BinNat) -> BinNat:
    recur n:
      case NatZero: cur
      case NatSucc(n):
        sum = add_BinNat(cur, next)
        loop(n, next, sum)
  one = Odd(Zero)
  loop(to_Nat(b), one, one)
```

Other examples:
1. `run` in `Eval.bosatsu` (evaluation budget).
1. `recur_max` in `Parser.bosatsu` (bounded parse steps).
1. `rand_Queue_depth` and `geometric` in `Queue.bosatsu` and `Rand.bosatsu`.

## Pattern 5: Compute A Size, Then Recur On That Size
For divide-and-conquer algorithms, compute a bound first, then recurse against
that bound.

Why this works for sort: recursive partitioning can only go as deep as the
input list length in the worst case. The function carries that maximum depth as
fuel (`sz`) and consumes one unit per recursive step. Each recursive sublist is
no larger than the previous bound, so by the time fuel reaches `Zero`, the
remaining subproblem is already at base size (or empty), and returning it is
correct.

In domain-predicate terms, this bound is a concrete witness that each recursive
call is still in the terminating domain. Bosatsu carries the witness as data
(`sz`) rather than as an explicit proof object.

From `List.bosatsu`:

```bosatsu
def sort(ord: Order[a], list: List[a]) -> List[a]:
  def loop(list: List[a], sz: Nat):
    recur sz:
      case Zero: list
      case Succ(n):
        match list:
          case []: []
          case [h, *t]:
            lesser = [ta for ta in t if lt(ta, h)]
            greater = [ta for ta in t if gteq(ta, h)]
            [*loop(lesser, n), h, *loop(greater, n)]
  loop(list, size(list))
```

This is the same idea called out in issue #406/#410: carry a companion value
that witnesses termination.

## Pattern 6: Convert One Measure To Another
Sometimes the data is not directly convenient for recursion checking, so convert
to a `Nat` bound and recurse on that.

From `BinNat.bosatsu`:

```bosatsu
def fold_left_BinNat(fn: (a, BinNat) -> a, init: a, cnt: BinNat) -> a:
  def loop(init: a, cnt: BinNat, cnt_Nat: Nat) -> a:
    recur cnt_Nat:
      case NatZero: init
      case NatSucc(prev_nat):
        cnt = prev(cnt)
        init = fn(init, cnt)
        loop(init, cnt, prev_nat)
  loop(init, cnt, to_Nat(cnt))
```

## Pattern 7: Recur Directly On `String`
Bosatsu string patterns can expose the next char plus tail, so parsing-like
loops can recurse structurally on the tail string.

From `Char.bosatsu`:

```bosatsu
def length_String(s: String) -> Int:
  def loop(s, acc):
    recur s:
      case "": acc
      case "$.{_}${tail}": loop(tail, acc.add(1))
  loop(s, 0)
```

From `PatternExamples.bosatsu`:

```bosatsu
def get_foos(s) -> List[String]:
  recur s:
    case "${_}foo: (${foo})${rest}": [foo, *get_foos(rest)]
    case _: []
```

## Pattern 8: String Parsing With Length-Derived Fuel
If direct string recursion is awkward, you can compute a string length, convert
to `Nat`, and recurse on that `Nat` as the parse budget.

Sketch:
1. `len = to_Nat(length_String(input))`
1. `loop(len, input)` with `recur len`
1. consume string as you go, and always recurse with the predecessor fuel

This is the same fuel pattern as Pattern 4, but with a string-derived bound.

## Pattern 9: Non-Obvious Nested Recursion (Ackermann-Style)
`Ackermann.bosatsu` demonstrates a higher-order style where:
1. Outer recursion is on `n`.
1. Inner recursion is on `m`.
1. The recursive result from the smaller `n` (`ack_p`) is reused in the inner
   loop.

```bosatsu
def ack1(n: Nat) -> (Nat -> Nat):
  recur n:
    case Zero: Succ
    case Succ(n_prev):
      ack_p = ack1(n_prev)
      def inner(m: Nat) -> Nat:
        ack_p(recur m:
          case Zero: Succ(Zero)
          case Succ(m_prev): inner(m_prev))
      inner
```

This is a useful pattern when recursion is nested but each recursive dimension
still has a clear structural decrease.

This is related to the nested-recursion discussion in Bove-Capretta-style
presentations: each nested layer needs its own decreasing argument/evidence.

## Pattern 10: Random Choice / Divide Patterns With Explicit Size Fuel
`Rand.bosatsu` function `one_of` computes `len(items)` as a `BinNat`, then
recurses on that size while splitting the list. This is the same termination
proof idea as sorting: derive a bound, decrease it, recurse.

## Int Loops And Trusted Externals
Bosatsu does not allow direct recursion on `Int` with an arbitrary comparison.
When needed, the standard library uses trusted externals like `int_loop` (for
example, in Euler problems and parsers). This keeps user recursion total while
still allowing practical bounded integer iteration.

## Relation To Fuel And Bove-Capretta
The design request for this page is tracked at
https://github.com/johnynek/bosatsu/issues/410.

How that maps to Bosatsu practice:
1. Fuel pattern: add a decreasing counter, return `None` or fallback on
   exhaustion (`Eval.run`, `Parser.recur_max`, `BinNat.fib`).
1. Domain/companion pattern (Bove-Capretta spirit): build a companion structure
   that justifies recursive calls (`List.sort` with `size`, `Rand.one_of` with
   list length).
1. Bosatsu keeps these ideas at the program level (data and recursion shape)
   instead of requiring explicit accessibility/domain proof terms in source
   code.

## Choosing A Pattern
1. If the recursive call is on an obvious subvalue, use structural recursion.
1. If not, compute a bound and recurse on that fuel (`Nat` is usually easiest).
1. If recursion is nested, make sure each nesting level has its own decreasing
   argument (Ackermann example).
1. For parsing-like string scans, either recurse on string tail directly or use
   length-derived fuel.

## References
1. Ana Bove and Venanzio Capretta, "Modelling general recursion in type
   theory" (MSCS, 2005): https://people.cs.nott.ac.uk/pszvc/publications/General_Recursion_MSCS_2005.pdf
1. Ana Bove, "General Recursion in Type Theory" (TYPES 2002):
   https://doi.org/10.1007/3-540-39185-1_3
1. Venanzio Capretta, "General Recursion via Coinductive Types" (LMCS, 2005):
   https://lmcs.episciences.org/2265
1. Conor McBride, "Turing-Completeness Totally Free" (2015):
   https://personal.cis.strath.ac.uk/conor.mcbride/TotallyFree.pdf
1. Casper Bach Poulsen, Arjen Rouvoet, Andrew Tolmach, Robbert Krebbers, and
   Eelco Visser, "Intrinsically-Typed Definitional Interpreters for Imperative
   Languages" (POPL, 2018): https://casperbp.net/store/intrinsicallytyped.pdf
1. Background on well-founded/domain-style termination arguments:
   https://en.wikipedia.org/wiki/Well-founded_relation
1. Project issue for this docs task:
   https://github.com/johnynek/bosatsu/issues/410
1. Related Bosatsu implementation discussion:
   https://github.com/johnynek/bosatsu/pull/406
