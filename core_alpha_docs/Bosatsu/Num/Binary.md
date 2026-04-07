# `Bosatsu/Num/Binary`

private package

source code:
- [`test_workspace/BinInt.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/BinInt.bosatsu)

public dependencies: [`Bosatsu/Num/BinNat`](BinNat.md)

## Index

- Types: [`BinInt`](#type-binint)
- Values: [`abs`](#value-abs), [`add`](#value-add), [`binInt_to_Int`](#value-binint-to-int),
[`binNat_to_BinInt`](#value-binnat-to-binint), [`cmp`](#value-cmp), [`eq`](#value-eq),
[`int_to_BinInt`](#value-int-to-binint), [`negate`](#value-negate), [`not`](#value-not),
[`sub`](#value-sub)

## Types

<a id="type-binint"></a>

### `BinInt`

BWNot(x) == -x - 1

```bosatsu
type BinInt
```

#### Constructors

- `BWNot(arg: Bosatsu/Num/BinNat::BinNat)`
- `FromBinNat(bn: Bosatsu/Num/BinNat::BinNat)`

## Values

<a id="value-abs"></a>

### `abs`

references: [`BinInt`](#type-binint), [`Bosatsu/Num/BinNat::BinNat`](BinNat.md#type-binnat)

```bosatsu
def abs(bi: BinInt) -> Bosatsu/Num/BinNat::BinNat
```

<a id="value-add"></a>

### `add`

references: [`BinInt`](#type-binint)

```bosatsu
def add(x: BinInt, y: BinInt) -> BinInt
```

<a id="value-binint-to-int"></a>

### `binInt_to_Int`

references: [`BinInt`](#type-binint), [`Int`](../Predef.md#type-int)

```bosatsu
def binInt_to_Int(bi: BinInt) -> Int
```

<a id="value-binnat-to-binint"></a>

### `binNat_to_BinInt`

references: [`BinInt`](#type-binint), [`Bosatsu/Num/BinNat::BinNat`](BinNat.md#type-binnat)

```bosatsu
def binNat_to_BinInt(bn: Bosatsu/Num/BinNat::BinNat) -> BinInt
```

<a id="value-cmp"></a>

### `cmp`

references: [`BinInt`](#type-binint), [`Comparison`](../Predef.md#type-comparison)

```bosatsu
def cmp(a: BinInt, b: BinInt) -> Comparison
```

<a id="value-eq"></a>

### `eq`

references: [`BinInt`](#type-binint), [`Bool`](../Predef.md#type-bool)

```bosatsu
def eq(a: BinInt, b: BinInt) -> Bool
```

<a id="value-int-to-binint"></a>

### `int_to_BinInt`

references: [`BinInt`](#type-binint), [`Int`](../Predef.md#type-int)

```bosatsu
def int_to_BinInt(i: Int) -> BinInt
```

<a id="value-negate"></a>

### `negate`

references: [`BinInt`](#type-binint)

```bosatsu
def negate(bi: BinInt) -> BinInt
```

<a id="value-not"></a>

### `not`

references: [`BinInt`](#type-binint)

```bosatsu
def not(bi: BinInt) -> BinInt
```

<a id="value-sub"></a>

### `sub`

references: [`BinInt`](#type-binint)

```bosatsu
def sub(a: BinInt, b: BinInt) -> BinInt
```