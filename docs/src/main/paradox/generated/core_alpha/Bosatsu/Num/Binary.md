# `Bosatsu/Num/Binary`

public dependencies: `Bosatsu/Num/BinNat`

## Types

### `BinInt`

BWNot(x) == -x - 1

```bosatsu
type BinInt
```

#### Constructors

- `BWNot(arg: Bosatsu/Num/BinNat::BinNat)`
- `FromBinNat(bn: Bosatsu/Num/BinNat::BinNat)`

## Values

### `abs`

```bosatsu
def abs(arg1: BinInt) -> Bosatsu/Num/BinNat::BinNat
```

### `add`

```bosatsu
def add(arg1: BinInt, arg2: BinInt) -> BinInt
```

### `binInt_to_Int`

```bosatsu
def binInt_to_Int(arg1: BinInt) -> Int
```

### `binNat_to_BinInt`

```bosatsu
def binNat_to_BinInt(arg1: Bosatsu/Num/BinNat::BinNat) -> BinInt
```

### `cmp`

```bosatsu
def cmp(arg1: BinInt, arg2: BinInt) -> Comparison
```

### `eq`

```bosatsu
def eq(arg1: BinInt, arg2: BinInt) -> Bool
```

### `int_to_BinInt`

```bosatsu
def int_to_BinInt(arg1: Int) -> BinInt
```

### `negate`

```bosatsu
def negate(arg1: BinInt) -> BinInt
```

### `not`

```bosatsu
def not(arg1: BinInt) -> BinInt
```

### `sub`

```bosatsu
def sub(arg1: BinInt, arg2: BinInt) -> BinInt
```