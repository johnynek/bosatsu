---
github.base_url=
---

# `Bosatsu/Collection/Queue`

private package

source code:
- [`test_workspace/Queue.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Queue.bosatsu)

## Index

- Types: [`Queue`](#type-queue)
- Values: [`empty_Queue`](#value-empty-queue), [`eq_Queue`](#value-eq-queue),
[`fold_Queue`](#value-fold-queue), [`from_List`](#value-from-list), [`pop`](#value-pop),
[`pop_value`](#value-pop-value), [`push`](#value-push), [`reverse_Queue`](#value-reverse-queue),
[`to_List`](#value-to-list), [`unpush`](#value-unpush)

## Types

<a id="type-queue"></a>

### `Queue[a]`

```bosatsu
type Queue[a: +*]
```

## Values

<a id="value-empty-queue"></a>

### `empty_Queue`

references: [`Queue`](#type-queue)

```bosatsu
empty_Queue: forall a: *. Queue[a]
```

<a id="value-eq-queue"></a>

### `eq_Queue`

references: [`Bool`](../Predef.html#type-bool), [`Queue`](#type-queue)

```bosatsu
def eq_Queue[a](eq_fn: (a, a) -> Bool) -> (Queue[a], Queue[a]) -> Bool
```

<a id="value-fold-queue"></a>

### `fold_Queue`

references: [`Queue`](#type-queue)

```bosatsu
def fold_Queue[a, b](arg1: Queue[a], init: b, fold_fn: (b, a) -> b) -> b
```

<a id="value-from-list"></a>

### `from_List`

references: [`List`](../Predef.html#type-list), [`Queue`](#type-queue)

```bosatsu
def from_List[a](list: List[a]) -> Queue[a]
```

<a id="value-pop"></a>

### `pop`

drop an item off and return the rest, or empty

references: [`Queue`](#type-queue)

```bosatsu
def pop[a](queue: Queue[a]) -> Queue[a]
```

<a id="value-pop-value"></a>

### `pop_value`

references: [`Option`](../Predef.html#type-option), [`Queue`](#type-queue)

```bosatsu
def pop_value[a](queue: Queue[a]) -> Option[a]
```

<a id="value-push"></a>

### `push`

references: [`Queue`](#type-queue)

```bosatsu
def push[a](arg1: Queue[a], item: a) -> Queue[a]
```

<a id="value-reverse-queue"></a>

### `reverse_Queue`

references: [`Queue`](#type-queue)

```bosatsu
def reverse_Queue[a](arg1: Queue[a]) -> Queue[a]
```

<a id="value-to-list"></a>

### `to_List`

references: [`List`](../Predef.html#type-list), [`Queue`](#type-queue)

```bosatsu
def to_List[a](arg1: Queue[a]) -> List[a]
```

<a id="value-unpush"></a>

### `unpush`

references: [`Option`](../Predef.html#type-option), [`Queue`](#type-queue), [`Tuple2`](../Predef.html#type-tuple2)

```bosatsu
def unpush[a](queue: Queue[a]) -> Option[(a, Queue[a])]
```