# `Bosatsu/Dict`

## Values

### `eq_Dict`

```bosatsu
def eq_Dict[a, b](arg1: (a, a) -> Bool, arg2: (b, b) -> Bool) -> (Dict[a, b], Dict[a, b]) -> Bool
```

### `eq_Pair`

```bosatsu
def eq_Pair[a, b, c, d](arg1: (a, b) -> Bool, arg2: (c, d) -> Bool) -> ((a, c), (b, d)) -> Bool
```