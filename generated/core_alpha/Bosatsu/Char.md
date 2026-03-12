---
github.base_url=
---

# `Bosatsu/Char`

source code:
- [`test_workspace/Char.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Char.bosatsu)

## Index

- Values: [`char_List_to_Int_List`](#value-char-list-to-int-list),
[`char_to_Int`](#value-char-to-int), [`cmp_Char`](#value-cmp-char),
[`eq_ignore_ascii_case`](#value-eq-ignore-ascii-case), [`from_digit`](#value-from-digit),
[`int_List_to_Char_List`](#value-int-list-to-char-list),
[`int_List_to_String`](#value-int-list-to-string), [`int_to_Char`](#value-int-to-char),
[`is_ascii`](#value-is-ascii), [`is_ascii_alnum`](#value-is-ascii-alnum),
[`is_ascii_alpha`](#value-is-ascii-alpha), [`is_ascii_control`](#value-is-ascii-control),
[`is_ascii_digit`](#value-is-ascii-digit), [`is_ascii_hex`](#value-is-ascii-hex),
[`is_ascii_lower`](#value-is-ascii-lower), [`is_ascii_punctuation`](#value-is-ascii-punctuation),
[`is_ascii_upper`](#value-is-ascii-upper), [`is_ascii_whitespace`](#value-is-ascii-whitespace),
[`is_digit`](#value-is-digit), [`is_hex`](#value-is-hex),
[`is_scalar_value`](#value-is-scalar-value), [`last_String`](#value-last-string),
[`max_Char`](#value-max-char), [`range_Char`](#value-range-char),
[`string_to_Char`](#value-string-to-char), [`string_to_Char_List`](#value-string-to-char-list),
[`string_to_Int_List`](#value-string-to-int-list),
[`to_ascii_lower_case`](#value-to-ascii-lower-case),
[`to_ascii_upper_case`](#value-to-ascii-upper-case), [`to_digit`](#value-to-digit),
[`utf8_len`](#value-utf8-len)

## Values

<a id="value-char-list-to-int-list"></a>

### `char_List_to_Int_List`

references: [`Char`](Predef.html#type-char), [`Int`](Predef.html#type-int), [`List`](Predef.html#type-list)

```bosatsu
def char_List_to_Int_List(chars: List[Char]) -> List[Int]
```

<a id="value-char-to-int"></a>

### `char_to_Int`

references: [`Char`](Predef.html#type-char), [`Int`](Predef.html#type-int)

```bosatsu
def char_to_Int(c: Char) -> Int
```

<a id="value-cmp-char"></a>

### `cmp_Char`

references: [`Char`](Predef.html#type-char), [`Comparison`](Predef.html#type-comparison)

```bosatsu
def cmp_Char(a: Char, b: Char) -> Comparison
```

<a id="value-eq-ignore-ascii-case"></a>

### `eq_ignore_ascii_case`

references: [`Bool`](Predef.html#type-bool), [`Char`](Predef.html#type-char)

```bosatsu
def eq_ignore_ascii_case(left: Char, right: Char) -> Bool
```

<a id="value-from-digit"></a>

### `from_digit`

Supported radix range is 2..36 inclusive.

references: [`Char`](Predef.html#type-char), [`Int`](Predef.html#type-int), [`Option`](Predef.html#type-option)

```bosatsu
def from_digit(digit: Int, radix: Int) -> Option[Char]
```

<a id="value-int-list-to-char-list"></a>

### `int_List_to_Char_List`

references: [`Char`](Predef.html#type-char), [`Int`](Predef.html#type-int), [`List`](Predef.html#type-list), [`Option`](Predef.html#type-option)

```bosatsu
def int_List_to_Char_List(code_points: List[Int]) -> Option[List[Char]]
```

<a id="value-int-list-to-string"></a>

### `int_List_to_String`

references: [`Int`](Predef.html#type-int), [`List`](Predef.html#type-list), [`Option`](Predef.html#type-option), [`String`](Predef.html#type-string)

```bosatsu
def int_List_to_String(code_points: List[Int]) -> Option[String]
```

<a id="value-int-to-char"></a>

### `int_to_Char`

references: [`Char`](Predef.html#type-char), [`Int`](Predef.html#type-int), [`Option`](Predef.html#type-option)

```bosatsu
def int_to_Char(code_point: Int) -> Option[Char]
```

<a id="value-is-ascii"></a>

### `is_ascii`

references: [`Bool`](Predef.html#type-bool), [`Char`](Predef.html#type-char)

```bosatsu
def is_ascii(c: Char) -> Bool
```

<a id="value-is-ascii-alnum"></a>

### `is_ascii_alnum`

references: [`Bool`](Predef.html#type-bool), [`Char`](Predef.html#type-char)

```bosatsu
def is_ascii_alnum(c: Char) -> Bool
```

<a id="value-is-ascii-alpha"></a>

### `is_ascii_alpha`

references: [`Bool`](Predef.html#type-bool), [`Char`](Predef.html#type-char)

```bosatsu
def is_ascii_alpha(c: Char) -> Bool
```

<a id="value-is-ascii-control"></a>

### `is_ascii_control`

references: [`Bool`](Predef.html#type-bool), [`Char`](Predef.html#type-char)

```bosatsu
def is_ascii_control(c: Char) -> Bool
```

<a id="value-is-ascii-digit"></a>

### `is_ascii_digit`

references: [`Bool`](Predef.html#type-bool), [`Char`](Predef.html#type-char)

```bosatsu
def is_ascii_digit(c: Char) -> Bool
```

<a id="value-is-ascii-hex"></a>

### `is_ascii_hex`

references: [`Bool`](Predef.html#type-bool), [`Char`](Predef.html#type-char)

```bosatsu
def is_ascii_hex(c: Char) -> Bool
```

<a id="value-is-ascii-lower"></a>

### `is_ascii_lower`

references: [`Bool`](Predef.html#type-bool), [`Char`](Predef.html#type-char)

```bosatsu
def is_ascii_lower(c: Char) -> Bool
```

<a id="value-is-ascii-punctuation"></a>

### `is_ascii_punctuation`

references: [`Bool`](Predef.html#type-bool), [`Char`](Predef.html#type-char)

```bosatsu
def is_ascii_punctuation(c: Char) -> Bool
```

<a id="value-is-ascii-upper"></a>

### `is_ascii_upper`

references: [`Bool`](Predef.html#type-bool), [`Char`](Predef.html#type-char)

```bosatsu
def is_ascii_upper(c: Char) -> Bool
```

<a id="value-is-ascii-whitespace"></a>

### `is_ascii_whitespace`

references: [`Bool`](Predef.html#type-bool), [`Char`](Predef.html#type-char)

```bosatsu
def is_ascii_whitespace(c: Char) -> Bool
```

<a id="value-is-digit"></a>

### `is_digit`

references: [`Bool`](Predef.html#type-bool), [`Char`](Predef.html#type-char)

```bosatsu
def is_digit(c: Char) -> Bool
```

<a id="value-is-hex"></a>

### `is_hex`

references: [`Bool`](Predef.html#type-bool), [`Char`](Predef.html#type-char)

```bosatsu
def is_hex(c: Char) -> Bool
```

<a id="value-is-scalar-value"></a>

### `is_scalar_value`

Unicode scalar value = [0, 0x10FFFF] excluding UTF-16 surrogate code points.

references: [`Bool`](Predef.html#type-bool), [`Int`](Predef.html#type-int)

```bosatsu
def is_scalar_value(code_point: Int) -> Bool
```

<a id="value-last-string"></a>

### `last_String`

references: [`Char`](Predef.html#type-char), [`Option`](Predef.html#type-option), [`String`](Predef.html#type-string)

```bosatsu
def last_String(s: String) -> Option[Char]
```

<a id="value-max-char"></a>

### `max_Char`

references: [`Char`](Predef.html#type-char)

```bosatsu
max_Char: Char
```

<a id="value-range-char"></a>

### `range_Char`

Returns chars in the half-open interval [start, end), ordered by code point.
Build candidate code points with a comprehension, then drop invalid values.

references: [`Char`](Predef.html#type-char), [`List`](Predef.html#type-list)

```bosatsu
def range_Char(start: Char, end: Char) -> List[Char]
```

<a id="value-string-to-char"></a>

### `string_to_Char`

references: [`Char`](Predef.html#type-char), [`Option`](Predef.html#type-option), [`String`](Predef.html#type-string)

```bosatsu
def string_to_Char(s: String) -> Option[Char]
```

<a id="value-string-to-char-list"></a>

### `string_to_Char_List`

references: [`Char`](Predef.html#type-char), [`List`](Predef.html#type-list), [`String`](Predef.html#type-string)

```bosatsu
def string_to_Char_List(s: String) -> List[Char]
```

<a id="value-string-to-int-list"></a>

### `string_to_Int_List`

references: [`Int`](Predef.html#type-int), [`List`](Predef.html#type-list), [`String`](Predef.html#type-string)

```bosatsu
def string_to_Int_List(s: String) -> List[Int]
```

<a id="value-to-ascii-lower-case"></a>

### `to_ascii_lower_case`

references: [`Char`](Predef.html#type-char)

```bosatsu
def to_ascii_lower_case(c: Char) -> Char
```

<a id="value-to-ascii-upper-case"></a>

### `to_ascii_upper_case`

references: [`Char`](Predef.html#type-char)

```bosatsu
def to_ascii_upper_case(c: Char) -> Char
```

<a id="value-to-digit"></a>

### `to_digit`

Supported radix range is 2..36 inclusive.

references: [`Char`](Predef.html#type-char), [`Int`](Predef.html#type-int), [`Option`](Predef.html#type-option)

```bosatsu
def to_digit(c: Char, radix: Int) -> Option[Int]
```

<a id="value-utf8-len"></a>

### `utf8_len`

references: [`Char`](Predef.html#type-char), [`Int`](Predef.html#type-int)

```bosatsu
def utf8_len(c: Char) -> Int
```