# `AvlTree`

## Types

### `Module[a]`

Module pattern to associate some methods with a typeclass (Order)

```bosatsu
type Module[a: *]
```

### `Tree[a]`

```bosatsu
type Tree[a: +*]
```

## Values

### `min`

```bosatsu
def min[a](arg1: Tree[a]) -> Option[a]
```

### `module`

```bosatsu
def module[a](arg1: Order[a]) -> Module[a]
```