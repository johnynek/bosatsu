# `Bosatsu/Collection/TreeList`

## Types

### `TreeList[a]`

```bosatsu
type TreeList[a: +*]
```

## Values

### `cons`

```bosatsu
def cons[a](arg1: a, arg2: TreeList[a]) -> TreeList[a]
```

### `decons`

```bosatsu
def decons[a](arg1: TreeList[a]) -> Option[(a, TreeList[a])]
```

### `empty`

```bosatsu
empty: forall a: *. TreeList[a]
```

### `eq_TreeList`

```bosatsu
def eq_TreeList[a, b](arg1: (a, b) -> Bool) -> (TreeList[a], TreeList[b]) -> Bool
```

### `fold`

```bosatsu
def fold[a, b](arg1: TreeList[a], arg2: b, arg3: (b, a) -> b) -> b
```

### `get`

```bosatsu
def get[a](arg1: TreeList[a], arg2: Int) -> Option[a]
```

### `head`

```bosatsu
def head[a](arg1: TreeList[a]) -> Option[a]
```

### `to_List`

```bosatsu
def to_List[a](arg1: TreeList[a]) -> List[a]
```