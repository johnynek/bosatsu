# `Bosatsu/List`

public dependencies: `Bosatsu/Num/Nat`

## Values

### `any`

```bosatsu
def any(arg1: List[Bool]) -> Bool
```

### `eq_List`

```bosatsu
def eq_List[a](arg1: (a, a) -> Bool) -> (List[a], List[a]) -> Bool
```

### `exists`

```bosatsu
def exists[a](arg1: List[a], arg2: a -> Bool) -> Bool
```

### `for_all`

```bosatsu
def for_all[a](arg1: List[a], arg2: a -> Bool) -> Bool
```

### `get_List`

```bosatsu
def get_List[a](arg1: List[a], arg2: Int) -> Option[a]
```

### `head`

```bosatsu
def head[a](arg1: List[a]) -> Option[a]
```

### `set_List`

```bosatsu
def set_List[a](arg1: List[a], arg2: Int, arg3: a) -> Option[List[a]]
```

### `size`

```bosatsu
def size[a](arg1: List[a]) -> Bosatsu/Num/Nat::Nat
```

### `sort`

```bosatsu
def sort[a](arg1: Order[a], arg2: List[a]) -> List[a]
```

### `sum`

```bosatsu
def sum(arg1: List[Int]) -> Int
```

### `uncons`

```bosatsu
def uncons[a](arg1: List[a]) -> Option[(a, List[a])]
```

### `zip`

```bosatsu
def zip[a, b](arg1: List[a], arg2: List[b]) -> List[(a, b)]
```