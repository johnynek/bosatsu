---
github.base_url=
---

# `Bosatsu/Num/Float64`

source code:
- [`test_workspace/Float64.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Float64.bosatsu)

## Index

- Values: [`abs`](#value-abs), [`acos`](#value-acos), [`addf`](#value-addf), [`asin`](#value-asin),
[`atan`](#value-atan), [`atan2`](#value-atan2), [`ceil`](#value-ceil),
[`cmp_Float64`](#value-cmp-float64), [`copy_sign`](#value-copy-sign), [`cos`](#value-cos),
[`cosh`](#value-cosh), [`divf`](#value-divf), [`eq_Float64`](#value-eq-float64),
[`exp`](#value-exp), [`float64_bits_to_Int`](#value-float64-bits-to-int),
[`float64_to_Int`](#value-float64-to-int), [`float64_to_String`](#value-float64-to-string),
[`floor`](#value-floor), [`hypot`](#value-hypot), [`inf`](#value-inf),
[`int_bits_to_Float64`](#value-int-bits-to-float64), [`int_to_Float64`](#value-int-to-float64),
[`is_finite`](#value-is-finite), [`is_infinite`](#value-is-infinite), [`is_nan`](#value-is-nan),
[`log`](#value-log), [`log10`](#value-log10), [`mulf`](#value-mulf), [`neg_inf`](#value-neg-inf),
[`operator *`](#value-operator), [`operator +`](#value-operator), [`operator -`](#value-operator),
[`operator /`](#value-operator), [`pow`](#value-pow), [`sin`](#value-sin), [`sinh`](#value-sinh),
[`sqrt`](#value-sqrt), [`string_to_Float64`](#value-string-to-float64), [`subf`](#value-subf),
[`tan`](#value-tan), [`tanh`](#value-tanh)

## Values

<a id="value-abs"></a>

### `abs`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def abs(x: Float64) -> Float64
```

<a id="value-acos"></a>

### `acos`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def acos(x: Float64) -> Float64
```

<a id="value-addf"></a>

### `addf`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def addf(a: Float64, b: Float64) -> Float64
```

<a id="value-asin"></a>

### `asin`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def asin(x: Float64) -> Float64
```

<a id="value-atan"></a>

### `atan`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def atan(x: Float64) -> Float64
```

<a id="value-atan2"></a>

### `atan2`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def atan2(y: Float64, x: Float64) -> Float64
```

<a id="value-ceil"></a>

### `ceil`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def ceil(x: Float64) -> Float64
```

<a id="value-cmp-float64"></a>

### `cmp_Float64`

references: [`Comparison`](../Predef.html#type-comparison), [`Float64`](../Predef.html#type-float64)

```bosatsu
def cmp_Float64(a: Float64, b: Float64) -> Comparison
```

<a id="value-copy-sign"></a>

### `copy_sign`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def copy_sign(x: Float64, sign: Float64) -> Float64
```

<a id="value-cos"></a>

### `cos`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def cos(x: Float64) -> Float64
```

<a id="value-cosh"></a>

### `cosh`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def cosh(x: Float64) -> Float64
```

<a id="value-divf"></a>

### `divf`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def divf(a: Float64, b: Float64) -> Float64
```

<a id="value-eq-float64"></a>

### `eq_Float64`

references: [`Bool`](../Predef.html#type-bool), [`Float64`](../Predef.html#type-float64)

```bosatsu
def eq_Float64(a: Float64, b: Float64) -> Bool
```

<a id="value-exp"></a>

### `exp`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def exp(x: Float64) -> Float64
```

<a id="value-float64-bits-to-int"></a>

### `float64_bits_to_Int`

references: [`Float64`](../Predef.html#type-float64), [`Int`](../Predef.html#type-int)

```bosatsu
def float64_bits_to_Int(x: Float64) -> Int
```

<a id="value-float64-to-int"></a>

### `float64_to_Int`

references: [`Float64`](../Predef.html#type-float64), [`Int`](../Predef.html#type-int), [`Option`](../Predef.html#type-option)

```bosatsu
def float64_to_Int(f: Float64) -> Option[Int]
```

<a id="value-float64-to-string"></a>

### `float64_to_String`

references: [`Float64`](../Predef.html#type-float64), [`String`](../Predef.html#type-string)

```bosatsu
def float64_to_String(x: Float64) -> String
```

<a id="value-floor"></a>

### `floor`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def floor(x: Float64) -> Float64
```

<a id="value-hypot"></a>

### `hypot`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def hypot(x: Float64, y: Float64) -> Float64
```

<a id="value-inf"></a>

### `inf`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
inf: Float64
```

<a id="value-int-bits-to-float64"></a>

### `int_bits_to_Float64`

references: [`Float64`](../Predef.html#type-float64), [`Int`](../Predef.html#type-int)

```bosatsu
def int_bits_to_Float64(int: Int) -> Float64
```

<a id="value-int-to-float64"></a>

### `int_to_Float64`

references: [`Float64`](../Predef.html#type-float64), [`Int`](../Predef.html#type-int)

```bosatsu
def int_to_Float64(i: Int) -> Float64
```

<a id="value-is-finite"></a>

### `is_finite`

references: [`Bool`](../Predef.html#type-bool), [`Float64`](../Predef.html#type-float64)

```bosatsu
def is_finite(x: Float64) -> Bool
```

<a id="value-is-infinite"></a>

### `is_infinite`

references: [`Bool`](../Predef.html#type-bool), [`Float64`](../Predef.html#type-float64)

```bosatsu
def is_infinite(x: Float64) -> Bool
```

<a id="value-is-nan"></a>

### `is_nan`

references: [`Bool`](../Predef.html#type-bool), [`Float64`](../Predef.html#type-float64)

```bosatsu
def is_nan(x: Float64) -> Bool
```

<a id="value-log"></a>

### `log`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def log(x: Float64) -> Float64
```

<a id="value-log10"></a>

### `log10`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def log10(x: Float64) -> Float64
```

<a id="value-mulf"></a>

### `mulf`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def mulf(a: Float64, b: Float64) -> Float64
```

<a id="value-neg-inf"></a>

### `neg_inf`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
neg_inf: Float64
```

<a id="value-operator"></a>

### `operator *`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def operator *(a: Float64, b: Float64) -> Float64
```

<a id="value-operator"></a>

### `operator +`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def operator +(a: Float64, b: Float64) -> Float64
```

<a id="value-operator"></a>

### `operator -`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def operator -(a: Float64, b: Float64) -> Float64
```

<a id="value-operator"></a>

### `operator /`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def operator /(a: Float64, b: Float64) -> Float64
```

<a id="value-pow"></a>

### `pow`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def pow(x: Float64, y: Float64) -> Float64
```

<a id="value-sin"></a>

### `sin`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def sin(x: Float64) -> Float64
```

<a id="value-sinh"></a>

### `sinh`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def sinh(x: Float64) -> Float64
```

<a id="value-sqrt"></a>

### `sqrt`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def sqrt(x: Float64) -> Float64
```

<a id="value-string-to-float64"></a>

### `string_to_Float64`

references: [`Float64`](../Predef.html#type-float64), [`Option`](../Predef.html#type-option), [`String`](../Predef.html#type-string)

```bosatsu
def string_to_Float64(s: String) -> Option[Float64]
```

<a id="value-subf"></a>

### `subf`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def subf(a: Float64, b: Float64) -> Float64
```

<a id="value-tan"></a>

### `tan`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def tan(x: Float64) -> Float64
```

<a id="value-tanh"></a>

### `tanh`

references: [`Float64`](../Predef.html#type-float64)

```bosatsu
def tanh(x: Float64) -> Float64
```