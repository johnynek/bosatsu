---
github.base_url=
---

# `Bosatsu/Num/BinNat`

private package

source code:
- [`test_workspace/BinNat.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/BinNat.bosatsu)

public dependencies: [`Bosatsu/Num/Nat`](Nat.html)

## Index

- Types: [`BinNat`](#type-binnat)
- Values: [`add_BinNat`](#value-add-binnat), [`cmp_BinNat`](#value-cmp-binnat),
[`div2`](#value-div2), [`divmod`](#value-divmod), [`eq_BinNat`](#value-eq-binnat),
[`exp`](#value-exp), [`is_even`](#value-is-even), [`mul2`](#value-mul2), [`next`](#value-next),
[`prev`](#value-prev), [`sub_BinNat`](#value-sub-binnat), [`sub_Option`](#value-sub-option),
[`times_BinNat`](#value-times-binnat), [`to_BinNat`](#value-to-binnat), [`to_Int`](#value-to-int),
[`to_Nat`](#value-to-nat)

## Types

<a id="type-binnat"></a>

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

<a id="value-add-binnat"></a>

### `add_BinNat`

references: [`BinNat`](#type-binnat)

```bosatsu
def add_BinNat(left: BinNat, right: BinNat) -> BinNat
```

<a id="value-cmp-binnat"></a>

### `cmp_BinNat`

references: [`BinNat`](#type-binnat), [`Comparison`](../Predef.html#type-comparison)

```bosatsu
def cmp_BinNat(a: BinNat, b: BinNat) -> Comparison
```

<a id="value-div2"></a>

### `div2`

references: [`BinNat`](#type-binnat)

```bosatsu
def div2(b: BinNat) -> BinNat
```

<a id="value-divmod"></a>

### `divmod`

references: [`BinNat`](#type-binnat), [`Tuple2`](../Predef.html#type-tuple2)

```bosatsu
def divmod(numerator: BinNat, divisor: BinNat) -> (BinNat, BinNat)
```

<a id="value-eq-binnat"></a>

### `eq_BinNat`

this is more efficient potentially than cmp_BinNat
because at the first difference we can stop. In the worst
case of equality, the cost is the same.

references: [`BinNat`](#type-binnat), [`Bool`](../Predef.html#type-bool)

```bosatsu
def eq_BinNat(a: BinNat, b: BinNat) -> Bool
```

<a id="value-exp"></a>

### `exp`

references: [`BinNat`](#type-binnat)

```bosatsu
def exp(base: BinNat, power: BinNat) -> BinNat
```

<a id="value-is-even"></a>

### `is_even`

references: [`BinNat`](#type-binnat), [`Bool`](../Predef.html#type-bool)

```bosatsu
def is_even(b: BinNat) -> Bool
```

<a id="value-mul2"></a>

### `mul2`

multiply by 2

references: [`BinNat`](#type-binnat)

```bosatsu
def mul2(b: BinNat) -> BinNat
```

<a id="value-next"></a>

### `next`

Return the next number

references: [`BinNat`](#type-binnat)

```bosatsu
def next(b: BinNat) -> BinNat
```

<a id="value-prev"></a>

### `prev`

Return the previous number if the number is > 0, else return 0

references: [`BinNat`](#type-binnat)

```bosatsu
def prev(b: BinNat) -> BinNat
```

<a id="value-sub-binnat"></a>

### `sub_BinNat`

references: [`BinNat`](#type-binnat)

```bosatsu
def sub_BinNat(left: BinNat, right: BinNat) -> BinNat
```

<a id="value-sub-option"></a>

### `sub_Option`

references: [`BinNat`](#type-binnat), [`Option`](../Predef.html#type-option)

```bosatsu
def sub_Option(left: BinNat, right: BinNat) -> Option[BinNat]
```

<a id="value-times-binnat"></a>

### `times_BinNat`

multiply two BinNat together

references: [`BinNat`](#type-binnat)

```bosatsu
def times_BinNat(left: BinNat, right: BinNat) -> BinNat
```

<a id="value-to-binnat"></a>

### `to_BinNat`

Convert a built in integer to a BinNat. <= 0 is converted to 0

references: [`BinNat`](#type-binnat), [`Int`](../Predef.html#type-int)

```bosatsu
def to_BinNat(n: Int) -> BinNat
```

<a id="value-to-int"></a>

### `to_Int`

Convert a BinNat into the equivalent Int
this is O(log(b)) operation

references: [`BinNat`](#type-binnat), [`Int`](../Predef.html#type-int)

```bosatsu
def to_Int(b: BinNat) -> Int
```

<a id="value-to-nat"></a>

### `to_Nat`

This is an O(b) operation

references: [`BinNat`](#type-binnat), [`Bosatsu/Num/Nat::Nat`](Nat.html#type-nat)

```bosatsu
def to_Nat(b: BinNat) -> Bosatsu/Num/Nat::Nat
```