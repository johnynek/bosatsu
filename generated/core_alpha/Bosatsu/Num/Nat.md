---
github.base_url=
---

# `Bosatsu/Num/Nat`

source code:
- [`test_workspace/Nat.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Nat.bosatsu)

## Index

- Types: [`Nat`](#type-nat)
- Values: [`add`](#value-add), [`cmp_Nat`](#value-cmp-nat), [`div2`](#value-div2),
[`divmod`](#value-divmod), [`exp`](#value-exp), [`is_even`](#value-is-even), [`mul2`](#value-mul2),
[`mult`](#value-mult), [`sub_Nat`](#value-sub-nat), [`to_Int`](#value-to-int),
[`to_Nat`](#value-to-nat)

## Types

<a id="type-nat"></a>

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

<a id="value-add"></a>

### `add`

references: [`Nat`](#type-nat)

```bosatsu
def add(n1: Nat, n2: Nat) -> Nat
```

<a id="value-cmp-nat"></a>

### `cmp_Nat`

references: [`Comparison`](../Predef.html#type-comparison), [`Nat`](#type-nat)

```bosatsu
def cmp_Nat(a: Nat, b: Nat) -> Comparison
```

<a id="value-div2"></a>

### `div2`

references: [`Nat`](#type-nat)

```bosatsu
def div2(n: Nat) -> Nat
```

<a id="value-divmod"></a>

### `divmod`

references: [`Nat`](#type-nat), [`Tuple2`](../Predef.html#type-tuple2)

```bosatsu
def divmod(numerator: Nat, divisor: Nat) -> (Nat, Nat)
```

<a id="value-exp"></a>

### `exp`

references: [`Nat`](#type-nat)

```bosatsu
def exp(base: Nat, power: Nat) -> Nat
```

<a id="value-is-even"></a>

### `is_even`

references: [`Bool`](../Predef.html#type-bool), [`Nat`](#type-nat)

```bosatsu
def is_even(n: Nat) -> Bool
```

<a id="value-mul2"></a>

### `mul2`

This is an O(n) operation

references: [`Nat`](#type-nat)

```bosatsu
def mul2(n: Nat) -> Nat
```

<a id="value-mult"></a>

### `mult`

references: [`Nat`](#type-nat)

```bosatsu
def mult(n1: Nat, n2: Nat) -> Nat
```

<a id="value-sub-nat"></a>

### `sub_Nat`

references: [`Nat`](#type-nat)

```bosatsu
def sub_Nat(n1: Nat, n2: Nat) -> Nat
```

<a id="value-to-int"></a>

### `to_Int`

references: [`Int`](../Predef.html#type-int), [`Nat`](#type-nat)

```bosatsu
def to_Int(n: Nat) -> Int
```

<a id="value-to-nat"></a>

### `to_Nat`

references: [`Int`](../Predef.html#type-int), [`Nat`](#type-nat)

```bosatsu
def to_Nat(i: Int) -> Nat
```