# `Bosatsu/Num/Nat`

## Types

### `Nat`

This is the traditional encoding of natural numbers
it is useful when you are iterating on all values

```bosatsu
type Nat
```

#### Constructors

- `Succ(prev: Nat)`
- `Zero`

## Values

### `add`

```bosatsu
def add(arg1: Nat, arg2: Nat) -> Nat
```

### `cmp_Nat`

```bosatsu
def cmp_Nat(arg1: Nat, arg2: Nat) -> Comparison
```

### `div2`

```bosatsu
def div2(arg1: Nat) -> Nat
```

### `divmod`

```bosatsu
def divmod(arg1: Nat, arg2: Nat) -> (Nat, Nat)
```

### `exp`

```bosatsu
def exp(arg1: Nat, arg2: Nat) -> Nat
```

### `is_even`

```bosatsu
def is_even(arg1: Nat) -> Bool
```

### `mul2`

This is an O(n) operation

```bosatsu
def mul2(arg1: Nat) -> Nat
```

### `mult`

```bosatsu
def mult(arg1: Nat, arg2: Nat) -> Nat
```

### `sub_Nat`

```bosatsu
def sub_Nat(arg1: Nat, arg2: Nat) -> Nat
```

### `to_Int`

```bosatsu
def to_Int(arg1: Nat) -> Int
```

### `to_Nat`

```bosatsu
def to_Nat(arg1: Int) -> Nat
```