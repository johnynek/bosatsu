---
github.base_url=
---

# `Bosatsu/Collection/TreeList`

private package

source code:
- [`test_workspace/TreeList.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/TreeList.bosatsu)

## Index

- Types: [`TreeList`](#type-treelist)
- Values: [`cons`](#value-cons), [`decons`](#value-decons), [`empty`](#value-empty),
[`eq_TreeList`](#value-eq-treelist), [`fold`](#value-fold), [`get`](#value-get),
[`head`](#value-head), [`to_List`](#value-to-list)

## Types

<a id="type-treelist"></a>

### `TreeList[a]`

```bosatsu
type TreeList[a: +*]
```

## Values

<a id="value-cons"></a>

### `cons`

references: [`TreeList`](#type-treelist)

```bosatsu
def cons[a](head: a, arg2: TreeList[a]) -> TreeList[a]
```

<a id="value-decons"></a>

### `decons`

references: [`Option`](../Predef.html#type-option), [`TreeList`](#type-treelist), [`Tuple2`](../Predef.html#type-tuple2)

```bosatsu
def decons[a](arg1: TreeList[a]) -> Option[(a, TreeList[a])]
```

<a id="value-empty"></a>

### `empty`

references: [`TreeList`](#type-treelist)

```bosatsu
empty: forall a: *. TreeList[a]
```

<a id="value-eq-treelist"></a>

### `eq_TreeList`

references: [`Bool`](../Predef.html#type-bool), [`TreeList`](#type-treelist)

```bosatsu
def eq_TreeList[a, b](fn: (a, b) -> Bool) -> (TreeList[a], TreeList[b]) -> Bool
```

<a id="value-fold"></a>

### `fold`

references: [`TreeList`](#type-treelist)

```bosatsu
def fold[a, b](arg1: TreeList[a], init: b, fn: (b, a) -> b) -> b
```

<a id="value-get"></a>

### `get`

references: [`Int`](../Predef.html#type-int), [`Option`](../Predef.html#type-option), [`TreeList`](#type-treelist)

```bosatsu
def get[a](arg1: TreeList[a], idx: Int) -> Option[a]
```

<a id="value-head"></a>

### `head`

references: [`Option`](../Predef.html#type-option), [`TreeList`](#type-treelist)

```bosatsu
def head[a](tl: TreeList[a]) -> Option[a]
```

<a id="value-to-list"></a>

### `to_List`

references: [`List`](../Predef.html#type-list), [`TreeList`](#type-treelist)

```bosatsu
def to_List[a](list: TreeList[a]) -> List[a]
```