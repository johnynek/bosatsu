---
github.base_url=
---

# `Bosatsu/List`

private package

source code:
- [`test_workspace/List.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/List.bosatsu)

public dependencies: [`Bosatsu/Num/Nat`](Num/Nat.html)

## Index

- Values: [`any`](#value-any), [`eq_List`](#value-eq-list), [`exists`](#value-exists),
[`for_all`](#value-for-all), [`get_List`](#value-get-list), [`head`](#value-head),
[`set_List`](#value-set-list), [`size`](#value-size), [`sort`](#value-sort), [`sum`](#value-sum),
[`uncons`](#value-uncons), [`zip`](#value-zip)

## Values

<a id="value-any"></a>

### `any`

references: [`Bool`](Predef.html#type-bool), [`List`](Predef.html#type-list)

```bosatsu
def any(as: List[Bool]) -> Bool
```

<a id="value-eq-list"></a>

### `eq_List`

references: [`Bool`](Predef.html#type-bool), [`List`](Predef.html#type-list)

```bosatsu
def eq_List[a](fn: (a, a) -> Bool) -> (List[a], List[a]) -> Bool
```

<a id="value-exists"></a>

### `exists`

references: [`Bool`](Predef.html#type-bool), [`List`](Predef.html#type-list)

```bosatsu
def exists[a](xs: List[a], fn: a -> Bool) -> Bool
```

<a id="value-for-all"></a>

### `for_all`

references: [`Bool`](Predef.html#type-bool), [`List`](Predef.html#type-list)

```bosatsu
def for_all[a](xs: List[a], fn: a -> Bool) -> Bool
```

<a id="value-get-list"></a>

### `get_List`

references: [`Int`](Predef.html#type-int), [`List`](Predef.html#type-list), [`Option`](Predef.html#type-option)

```bosatsu
def get_List[a](xs: List[a], idx: Int) -> Option[a]
```

<a id="value-head"></a>

### `head`

references: [`List`](Predef.html#type-list), [`Option`](Predef.html#type-option)

```bosatsu
def head[a](xs: List[a]) -> Option[a]
```

<a id="value-set-list"></a>

### `set_List`

references: [`Int`](Predef.html#type-int), [`List`](Predef.html#type-list), [`Option`](Predef.html#type-option)

```bosatsu
def set_List[a](xs: List[a], idx: Int, value: a) -> Option[List[a]]
```

<a id="value-size"></a>

### `size`

references: [`Bosatsu/Num/Nat::Nat`](Num/Nat.html#type-nat), [`List`](Predef.html#type-list)

```bosatsu
def size[a](list: List[a]) -> Bosatsu/Num/Nat::Nat
```

<a id="value-sort"></a>

### `sort`

references: [`List`](Predef.html#type-list), [`Order`](Predef.html#type-order)

```bosatsu
def sort[a](ord: Order[a], list: List[a]) -> List[a]
```

<a id="value-sum"></a>

### `sum`

references: [`Int`](Predef.html#type-int), [`List`](Predef.html#type-list)

```bosatsu
def sum(as: List[Int]) -> Int
```

<a id="value-uncons"></a>

### `uncons`

references: [`List`](Predef.html#type-list), [`Option`](Predef.html#type-option), [`Tuple2`](Predef.html#type-tuple2)

```bosatsu
def uncons[a](xs: List[a]) -> Option[(a, List[a])]
```

<a id="value-zip"></a>

### `zip`

references: [`List`](Predef.html#type-list), [`Tuple2`](Predef.html#type-tuple2)

```bosatsu
def zip[a, b](left: List[a], right: List[b]) -> List[(a, b)]
```