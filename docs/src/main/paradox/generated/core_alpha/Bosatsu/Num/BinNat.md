# `Bosatsu/Num/BinNat`

public dependencies: `Bosatsu/Num/Nat`

## Types

### `BinNat`

a natural number with three variants:
Zero = 0
Odd(n) = 2n + 1
Even(n) = 2(n + 1)
e.g:
Zero, Odd(Zero), Even(Zero), Odd(Odd(Zero)), Even(Odd(Zero))

```bosatsu
type BinNat
```

#### Constructors

- `Even(half1: BinNat)`
- `Odd(half: BinNat)`
- `Zero`

## Values

### `add_BinNat`

```bosatsu
def add_BinNat(arg1: BinNat, arg2: BinNat) -> BinNat
```

### `cmp_BinNat`

```bosatsu
def cmp_BinNat(arg1: BinNat, arg2: BinNat) -> Comparison
```

### `div2`

```bosatsu
def div2(arg1: BinNat) -> BinNat
```

### `divmod`

```bosatsu
def divmod(arg1: BinNat, arg2: BinNat) -> (BinNat, BinNat)
```

### `eq_BinNat`

this is more efficient potentially than cmp_BinNat
because at the first difference we can stop. In the worst
case of equality, the cost is the same.

```bosatsu
def eq_BinNat(arg1: BinNat, arg2: BinNat) -> Bool
```

### `exp`

```bosatsu
def exp(arg1: BinNat, arg2: BinNat) -> BinNat
```

### `is_even`

```bosatsu
def is_even(arg1: BinNat) -> Bool
```

### `mul2`

multiply by 2

```bosatsu
def mul2(arg1: BinNat) -> BinNat
```

### `next`

Return the next number

```bosatsu
def next(arg1: BinNat) -> BinNat
```

### `prev`

Return the previous number if the number is > 0, else return 0

```bosatsu
def prev(arg1: BinNat) -> BinNat
```

### `sub_BinNat`

```bosatsu
def sub_BinNat(arg1: BinNat, arg2: BinNat) -> BinNat
```

### `sub_Option`

```bosatsu
def sub_Option(arg1: BinNat, arg2: BinNat) -> Option[BinNat]
```

### `times_BinNat`

multiply two BinNat together

```bosatsu
def times_BinNat(arg1: BinNat, arg2: BinNat) -> BinNat
```

### `to_BinNat`

Convert a built in integer to a BinNat. <= 0 is converted to 0

```bosatsu
def to_BinNat(arg1: Int) -> BinNat
```

### `to_Int`

Convert a BinNat into the equivalent Int
this is O(log(b)) operation

```bosatsu
def to_Int(arg1: BinNat) -> Int
```

### `to_Nat`

This is an O(b) operation

```bosatsu
def to_Nat(arg1: BinNat) -> Bosatsu/Num/Nat::Nat
```