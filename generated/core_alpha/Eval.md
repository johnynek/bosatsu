---
github.base_url=
---

# `Eval`

private package

source code:
- [`test_workspace/Eval.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Eval.bosatsu)

## Index

- Types: [`Eval`](#type-eval)
- Values: [`bind`](#value-bind), [`done`](#value-done), [`eval`](#value-eval),
[`flat_map`](#value-flat-map), [`map`](#value-map)

## Types

<a id="type-eval"></a>

### `Eval[a]`

port of cats.Eval to bosatsu

```bosatsu
type Eval[a: +*]
```

## Values

<a id="value-bind"></a>

### `bind`

references: [`Eval`](#type-eval)

```bosatsu
def bind[a, b](e: Eval[a], fn: a -> Eval[b]) -> Eval[b]
```

<a id="value-done"></a>

### `done`

references: [`Eval`](#type-eval)

```bosatsu
def done[a](a: a) -> Eval[a]
```

<a id="value-eval"></a>

### `eval`

references: [`Eval`](#type-eval), [`Int`](Bosatsu/Predef.html#type-int), [`Option`](Bosatsu/Predef.html#type-option)

```bosatsu
def eval[a](budget: Int, ea: Eval[a]) -> Option[a]
```

<a id="value-flat-map"></a>

### `flat_map`

references: [`Eval`](#type-eval)

```bosatsu
def flat_map[a, b](e: Eval[a], fn: a -> Eval[b]) -> Eval[b]
```

<a id="value-map"></a>

### `map`

references: [`Eval`](#type-eval)

```bosatsu
def map[a, b](e: Eval[a], fn: a -> b) -> Eval[b]
```