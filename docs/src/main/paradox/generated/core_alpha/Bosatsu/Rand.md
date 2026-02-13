# `Bosatsu/Rand`

public dependencies: `Bosatsu/Num/Nat`

## Types

### `Rand[a]`

```bosatsu
type Rand[a: +*]
```

## Values

### `bool_Rand`

```bosatsu
bool_Rand: Rand[Bool]
```

### `const_Rand`

```bosatsu
def const_Rand[a](arg1: a) -> Rand[a]
```

### `flat_map_Rand`

```bosatsu
def flat_map_Rand[a, b](arg1: Rand[a], arg2: a -> Rand[b]) -> Rand[b]
```

### `from_pair`

```bosatsu
def from_pair[a](arg1: Rand[a], arg2: Rand[a]) -> Rand[a]
```

### `geometric_Int`

geometric distribution with mean 1

```bosatsu
geometric_Int: Rand[Int]
```

### `int_range`

if you pass high <= 0, you get const_Rand(0)

```bosatsu
def int_range(arg1: Int) -> Rand[Int]
```

### `map_Rand`

```bosatsu
def map_Rand[a, b](arg1: Rand[a], arg2: a -> b) -> Rand[b]
```

### `nat_range`

```bosatsu
def nat_range(arg1: Bosatsu/Num/Nat::Nat) -> Rand[Bosatsu/Num/Nat::Nat]
```

### `one_of`

```bosatsu
def one_of[a](arg1: Rand[a], arg2: List[Rand[a]]) -> Rand[a]
```

### `prod_Rand`

```bosatsu
def prod_Rand[a, b](arg1: Rand[a], arg2: Rand[b]) -> Rand[(a, b)]
```

### `run_Rand`

```bosatsu
def run_Rand[a](arg1: Rand[a], arg2: Int) -> a
```

### `sequence_Rand`

```bosatsu
def sequence_Rand[a](arg1: List[Rand[a]]) -> Rand[List[a]]
```