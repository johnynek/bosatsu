# `Bosatsu/Collection/Queue`

## Types

### `Queue[a]`

```bosatsu
type Queue[a: +*]
```

## Values

### `empty_Queue`

```bosatsu
empty_Queue: forall a: *. Queue[a]
```

### `eq_Queue`

```bosatsu
def eq_Queue[a](arg1: (a, a) -> Bool) -> (Queue[a], Queue[a]) -> Bool
```

### `fold_Queue`

```bosatsu
def fold_Queue[a, b](arg1: Queue[a], arg2: b, arg3: (b, a) -> b) -> b
```

### `from_List`

```bosatsu
def from_List[a](arg1: List[a]) -> Queue[a]
```

### `pop`

drop an item off and return the rest, or empty

```bosatsu
def pop[a](arg1: Queue[a]) -> Queue[a]
```

### `pop_value`

```bosatsu
def pop_value[a](arg1: Queue[a]) -> Option[a]
```

### `push`

```bosatsu
def push[a](arg1: Queue[a], arg2: a) -> Queue[a]
```

### `reverse_Queue`

```bosatsu
def reverse_Queue[a](arg1: Queue[a]) -> Queue[a]
```

### `to_List`

```bosatsu
def to_List[a](arg1: Queue[a]) -> List[a]
```

### `unpush`

```bosatsu
def unpush[a](arg1: Queue[a]) -> Option[(a, Queue[a])]
```