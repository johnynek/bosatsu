# `AvlTree`

private package

source code:
- [`test_workspace/AvlTree.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/AvlTree.bosatsu)

## Index

- Types: [`Module`](#type-module), [`Tree`](#type-tree)
- Values: [`min`](#value-min), [`module`](#value-module)

## Types

<a id="type-module"></a>

### `Module[a]`

Module pattern to associate some methods with a typeclass (Order)

```bosatsu
type Module[a: *]
```

<a id="type-tree"></a>

### `Tree[a]`

```bosatsu
type Tree[a: +*]
```

## Values

<a id="value-min"></a>

### `min`

references: [`Option`](Bosatsu/Predef.md#type-option), [`Tree`](#type-tree)

```bosatsu
def min[a](tree: Tree[a]) -> Option[a]
```

<a id="value-module"></a>

### `module`

references: [`Module`](#type-module), [`Order`](Bosatsu/Predef.md#type-order)

```bosatsu
def module[a](ord: Order[a]) -> Module[a]
```