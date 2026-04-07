# `Bosatsu/Num/Int64`

source code:
- [`test_workspace/Int64.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Int64.bosatsu)

## Index

- Types: [`Int64`](#type-int64)
- Values: [`add_Int64`](#value-add-int64), [`and_Int64`](#value-and-int64),
[`cmp_Int64`](#value-cmp-int64), [`div_Int64`](#value-div-int64), [`eq_Int64`](#value-eq-int64),
[`float64_to_Int64`](#value-float64-to-int64), [`int64_to_Float64`](#value-int64-to-float64),
[`int64_to_Int`](#value-int64-to-int), [`int_low_bits_to_Int64`](#value-int-low-bits-to-int64),
[`int_to_Int64`](#value-int-to-int64), [`max_i64`](#value-max-i64), [`min_i64`](#value-min-i64),
[`mod_Int64`](#value-mod-int64), [`mul_Int64`](#value-mul-int64), [`not_Int64`](#value-not-int64),
[`operator &`](#value-operator), [`operator *`](#value-operator), [`operator +`](#value-operator),
[`operator -`](#value-operator), [`operator /`](#value-operator), [`operator <<`](#value-operator),
[`operator >>`](#value-operator), [`operator ^`](#value-operator), [`operator |`](#value-operator),
[`or_Int64`](#value-or-int64), [`popcount_Int64`](#value-popcount-int64),
[`shift_left_Int64`](#value-shift-left-int64), [`shift_right_Int64`](#value-shift-right-int64),
[`shift_right_unsigned_Int64`](#value-shift-right-unsigned-int64), [`sub_Int64`](#value-sub-int64),
[`xor_Int64`](#value-xor-int64)

## Types

<a id="type-int64"></a>

### `Int64`

```bosatsu
type Int64
```

## Values

<a id="value-add-int64"></a>

### `add_Int64`

references: [`Int64`](#type-int64)

```bosatsu
def add_Int64(a: Int64, b: Int64) -> Int64
```

<a id="value-and-int64"></a>

### `and_Int64`

references: [`Int64`](#type-int64)

```bosatsu
def and_Int64(a: Int64, b: Int64) -> Int64
```

<a id="value-cmp-int64"></a>

### `cmp_Int64`

references: [`Comparison`](../Predef.md#type-comparison), [`Int64`](#type-int64)

```bosatsu
def cmp_Int64(a: Int64, b: Int64) -> Comparison
```

<a id="value-div-int64"></a>

### `div_Int64`

references: [`Int64`](#type-int64)

```bosatsu
def div_Int64(a: Int64, b: Int64) -> Int64
```

<a id="value-eq-int64"></a>

### `eq_Int64`

references: [`Bool`](../Predef.md#type-bool), [`Int64`](#type-int64)

```bosatsu
def eq_Int64(a: Int64, b: Int64) -> Bool
```

<a id="value-float64-to-int64"></a>

### `float64_to_Int64`

references: [`Float64`](../Predef.md#type-float64), [`Int64`](#type-int64), [`Option`](../Predef.md#type-option)

```bosatsu
def float64_to_Int64(f: Float64) -> Option[Int64]
```

<a id="value-int64-to-float64"></a>

### `int64_to_Float64`

references: [`Float64`](../Predef.md#type-float64), [`Int64`](#type-int64)

```bosatsu
def int64_to_Float64(i: Int64) -> Float64
```

<a id="value-int64-to-int"></a>

### `int64_to_Int`

references: [`Int`](../Predef.md#type-int), [`Int64`](#type-int64)

```bosatsu
def int64_to_Int(i: Int64) -> Int
```

<a id="value-int-low-bits-to-int64"></a>

### `int_low_bits_to_Int64`

references: [`Int`](../Predef.md#type-int), [`Int64`](#type-int64)

```bosatsu
def int_low_bits_to_Int64(i: Int) -> Int64
```

<a id="value-int-to-int64"></a>

### `int_to_Int64`

references: [`Int`](../Predef.md#type-int), [`Int64`](#type-int64), [`Option`](../Predef.md#type-option)

```bosatsu
def int_to_Int64(i: Int) -> Option[Int64]
```

<a id="value-max-i64"></a>

### `max_i64`

references: [`Int64`](#type-int64)

```bosatsu
max_i64: Int64
```

<a id="value-min-i64"></a>

### `min_i64`

references: [`Int64`](#type-int64)

```bosatsu
min_i64: Int64
```

<a id="value-mod-int64"></a>

### `mod_Int64`

references: [`Int64`](#type-int64)

```bosatsu
def mod_Int64(a: Int64, b: Int64) -> Int64
```

<a id="value-mul-int64"></a>

### `mul_Int64`

references: [`Int64`](#type-int64)

```bosatsu
def mul_Int64(a: Int64, b: Int64) -> Int64
```

<a id="value-not-int64"></a>

### `not_Int64`

references: [`Int64`](#type-int64)

```bosatsu
def not_Int64(a: Int64) -> Int64
```

<a id="value-operator"></a>

### `operator &`

references: [`Int64`](#type-int64)

```bosatsu
def operator &(a: Int64, b: Int64) -> Int64
```

<a id="value-operator"></a>

### `operator *`

references: [`Int64`](#type-int64)

```bosatsu
def operator *(a: Int64, b: Int64) -> Int64
```

<a id="value-operator"></a>

### `operator +`

references: [`Int64`](#type-int64)

```bosatsu
def operator +(a: Int64, b: Int64) -> Int64
```

<a id="value-operator"></a>

### `operator -`

references: [`Int64`](#type-int64)

```bosatsu
def operator -(a: Int64, b: Int64) -> Int64
```

<a id="value-operator"></a>

### `operator /`

references: [`Int64`](#type-int64)

```bosatsu
def operator /(a: Int64, b: Int64) -> Int64
```

<a id="value-operator"></a>

### `operator <<`

references: [`Int`](../Predef.md#type-int), [`Int64`](#type-int64)

```bosatsu
def operator <<(a: Int64, b: Int) -> Int64
```

<a id="value-operator"></a>

### `operator >>`

references: [`Int`](../Predef.md#type-int), [`Int64`](#type-int64)

```bosatsu
def operator >>(a: Int64, b: Int) -> Int64
```

<a id="value-operator"></a>

### `operator ^`

references: [`Int64`](#type-int64)

```bosatsu
def operator ^(a: Int64, b: Int64) -> Int64
```

<a id="value-operator"></a>

### `operator |`

references: [`Int64`](#type-int64)

```bosatsu
def operator |(a: Int64, b: Int64) -> Int64
```

<a id="value-or-int64"></a>

### `or_Int64`

references: [`Int64`](#type-int64)

```bosatsu
def or_Int64(a: Int64, b: Int64) -> Int64
```

<a id="value-popcount-int64"></a>

### `popcount_Int64`

references: [`Int`](../Predef.md#type-int), [`Int64`](#type-int64)

```bosatsu
def popcount_Int64(a: Int64) -> Int
```

<a id="value-shift-left-int64"></a>

### `shift_left_Int64`

references: [`Int`](../Predef.md#type-int), [`Int64`](#type-int64)

```bosatsu
def shift_left_Int64(a: Int64, n: Int) -> Int64
```

<a id="value-shift-right-int64"></a>

### `shift_right_Int64`

references: [`Int`](../Predef.md#type-int), [`Int64`](#type-int64)

```bosatsu
def shift_right_Int64(a: Int64, n: Int) -> Int64
```

<a id="value-shift-right-unsigned-int64"></a>

### `shift_right_unsigned_Int64`

references: [`Int`](../Predef.md#type-int), [`Int64`](#type-int64)

```bosatsu
def shift_right_unsigned_Int64(a: Int64, n: Int) -> Int64
```

<a id="value-sub-int64"></a>

### `sub_Int64`

references: [`Int64`](#type-int64)

```bosatsu
def sub_Int64(a: Int64, b: Int64) -> Int64
```

<a id="value-xor-int64"></a>

### `xor_Int64`

references: [`Int64`](#type-int64)

```bosatsu
def xor_Int64(a: Int64, b: Int64) -> Int64
```