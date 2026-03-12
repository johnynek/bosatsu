---
github.base_url=
---

# `Bosatsu/Eval`

source code:
- [`test_workspace/Bosatsu/Eval.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Bosatsu/Eval.bosatsu)

public dependencies: [`Bosatsu/Lazy`](Lazy.html)

## Index

- Types: [`Eval`](#type-eval)
- Values: [`always`](#value-always), [`defer`](#value-defer), [`done`](#value-done),
[`eval`](#value-eval), [`false_Eval`](#value-false-eval), [`flat_map`](#value-flat-map),
[`from_Lazy`](#value-from-lazy), [`lazy_Eval`](#value-lazy-eval), [`map`](#value-map),
[`one_Eval`](#value-one-eval), [`true_Eval`](#value-true-eval), [`unit_Eval`](#value-unit-eval),
[`zero_Eval`](#value-zero-eval)

## Types

<a id="type-eval"></a>

### `Eval[a]`

```bosatsu
type Eval[a: +*]
```

## Values

<a id="value-always"></a>

### `always`

this calls the thunk every time it is needed

references: [`Eval`](#type-eval), [`Unit`](Predef.html#type-unit)

```bosatsu
def always[a](fn: () -> a) -> Eval[a]
```

<a id="value-defer"></a>

### `defer`

references: [`Eval`](#type-eval), [`Unit`](Predef.html#type-unit)

```bosatsu
def defer[a](fn: () -> Eval[a]) -> Eval[a]
```

<a id="value-done"></a>

### `done`

references: [`Eval`](#type-eval)

```bosatsu
def done[a](a: a) -> Eval[a]
```

<a id="value-eval"></a>

### `eval`

Previous fuel-based implementation kept for reference:
def eval_loop[a](budget: Int, arg: Loop[a]) -> Option[a]:
    recur (budget, arg):
        case _ if cmp_Int(budget, 0) matches LT | EQ: None
        case (_, RunEval(Pure(a), stack)):
            eval_loop(budget.sub(1), RunStack(eval_Leaf(a), stack))
        case (_, RunEval(FlatMap(prev, fn), stack)):
            eval_loop(budget.sub(1), RunEval(prev, More(fn, stack)))
        case (_, RunStack(a, More(first, rest))):
            eval_loop(budget.sub(1), RunEval(first(a), rest))

references: [`Eval`](#type-eval)

```bosatsu
def eval[a](e: Eval[a]) -> a
```

<a id="value-false-eval"></a>

### `false_Eval`

references: [`Bool`](Predef.html#type-bool), [`Eval`](#type-eval)

```bosatsu
false_Eval: Eval[Bool]
```

<a id="value-flat-map"></a>

### `flat_map`

references: [`Eval`](#type-eval)

```bosatsu
def flat_map[a, b](e: Eval[a], fn: a -> Eval[b]) -> Eval[b]
```

<a id="value-from-lazy"></a>

### `from_Lazy`

this lifts a Lazy into an Eval

references: [`Bosatsu/Lazy::Lazy`](Lazy.html#type-lazy), [`Eval`](#type-eval)

```bosatsu
def from_Lazy[a](lzy: Bosatsu/Lazy::Lazy[a]) -> Eval[a]
```

<a id="value-lazy-eval"></a>

### `lazy_Eval`

this makes an Eval that evaluates the thunk one time

references: [`Eval`](#type-eval), [`Unit`](Predef.html#type-unit)

```bosatsu
def lazy_Eval[a](fn: () -> a) -> Eval[a]
```

<a id="value-map"></a>

### `map`

references: [`Eval`](#type-eval)

```bosatsu
def map[a, b](e: Eval[a], fn: a -> b) -> Eval[b]
```

<a id="value-one-eval"></a>

### `one_Eval`

references: [`Eval`](#type-eval), [`Int`](Predef.html#type-int)

```bosatsu
one_Eval: Eval[Int]
```

<a id="value-true-eval"></a>

### `true_Eval`

references: [`Bool`](Predef.html#type-bool), [`Eval`](#type-eval)

```bosatsu
true_Eval: Eval[Bool]
```

<a id="value-unit-eval"></a>

### `unit_Eval`

references: [`Eval`](#type-eval), [`Unit`](Predef.html#type-unit)

```bosatsu
unit_Eval: Eval[()]
```

<a id="value-zero-eval"></a>

### `zero_Eval`

references: [`Eval`](#type-eval), [`Int`](Predef.html#type-int)

```bosatsu
zero_Eval: Eval[Int]
```