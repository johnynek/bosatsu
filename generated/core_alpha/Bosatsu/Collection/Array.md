---
github.base_url=
---

# `Bosatsu/Collection/Array`

source code:
- [`test_workspace/Bosatsu/Collection/Array.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Bosatsu/Collection/Array.bosatsu)

public dependencies: [`Bosatsu/Num/Int64`](../Num/Int64.html)

## Index

- Types: [`Array`](#type-array)
- Values: [`char_Array_to_Int_Array`](#value-char-array-to-int-array),
[`char_Array_to_String`](#value-char-array-to-string),
[`concat_all_Array`](#value-concat-all-array), [`dotf_Array`](#value-dotf-array),
[`empty_Array`](#value-empty-array), [`filter_Array`](#value-filter-array),
[`flat_map_Array`](#value-flat-map-array), [`flatten_Array`](#value-flatten-array),
[`foldl_Array`](#value-foldl-array), [`foldl_with_index_Array`](#value-foldl-with-index-array),
[`foldr_Array`](#value-foldr-array), [`from_List_Array`](#value-from-list-array),
[`get_Array`](#value-get-array), [`get_map_Array`](#value-get-map-array),
[`get_or_Array`](#value-get-or-array), [`index_in_range_Array`](#value-index-in-range-array),
[`int_Array_to_Char_Array`](#value-int-array-to-char-array),
[`int_Array_to_String`](#value-int-array-to-string), [`map_Array`](#value-map-array),
[`map_with_index_Array`](#value-map-with-index-array), [`range_Array`](#value-range-array),
[`range_from_Array`](#value-range-from-array), [`reverse_Array`](#value-reverse-array),
[`set_Array`](#value-set-array), [`set_or_self_Array`](#value-set-or-self-array),
[`size_Array`](#value-size-array), [`slice_Array`](#value-slice-array),
[`sort_Array`](#value-sort-array), [`string_to_Char_Array`](#value-string-to-char-array),
[`string_to_Int_Array`](#value-string-to-int-array), [`sumf_Array`](#value-sumf-array),
[`sumsqf_Array`](#value-sumsqf-array), [`tabulate_Array`](#value-tabulate-array),
[`to_List_Array`](#value-to-list-array), [`zip_foldl_Array`](#value-zip-foldl-array),
[`zip_map_Array`](#value-zip-map-array), [`zip_sumf_Array`](#value-zip-sumf-array)

## Types

<a id="type-array"></a>

### `Array[a]`

```bosatsu
type Array[a: +*]
```

## Values

<a id="value-char-array-to-int-array"></a>

### `char_Array_to_Int_Array`

references: [`Array`](#type-array), [`Char`](../Predef.html#type-char), [`Int`](../Predef.html#type-int)

```bosatsu
def char_Array_to_Int_Array(chars: Array[Char]) -> Array[Int]
```

<a id="value-char-array-to-string"></a>

### `char_Array_to_String`

references: [`Array`](#type-array), [`Char`](../Predef.html#type-char), [`String`](../Predef.html#type-string)

```bosatsu
def char_Array_to_String(chars: Array[Char]) -> String
```

<a id="value-concat-all-array"></a>

### `concat_all_Array`

references: [`Array`](#type-array), [`List`](../Predef.html#type-list)

```bosatsu
def concat_all_Array[a](arrays: List[Array[a]]) -> Array[a]
```

<a id="value-dotf-array"></a>

### `dotf_Array`

references: [`Array`](#type-array), [`Float64`](../Predef.html#type-float64)

```bosatsu
def dotf_Array(left: Array[Float64], right: Array[Float64]) -> Float64
```

<a id="value-empty-array"></a>

### `empty_Array`

references: [`Array`](#type-array)

```bosatsu
empty_Array: forall a: *. Array[a]
```

<a id="value-filter-array"></a>

### `filter_Array`

references: [`Array`](#type-array), [`Bool`](../Predef.html#type-bool)

```bosatsu
def filter_Array[a](ary: Array[a], fn: a -> Bool) -> Array[a]
```

<a id="value-flat-map-array"></a>

### `flat_map_Array`

references: [`Array`](#type-array)

```bosatsu
def flat_map_Array[a, b](ary: Array[a], fn: a -> Array[b]) -> Array[b]
```

<a id="value-flatten-array"></a>

### `flatten_Array`

references: [`Array`](#type-array)

```bosatsu
def flatten_Array[a](arrays: Array[Array[a]]) -> Array[a]
```

<a id="value-foldl-array"></a>

### `foldl_Array`

references: [`Array`](#type-array)

```bosatsu
def foldl_Array[a, b](ary: Array[a], init: b, fn: (b, a) -> b) -> b
```

<a id="value-foldl-with-index-array"></a>

### `foldl_with_index_Array`

references: [`Array`](#type-array), [`Bosatsu/Num/Int64::Int64`](../Num/Int64.html#type-int64)

```bosatsu
def foldl_with_index_Array[a,
    b](ary: Array[a], init: b, fn: (b, a, Bosatsu/Num/Int64::Int64) -> b) -> b
```

<a id="value-foldr-array"></a>

### `foldr_Array`

references: [`Array`](#type-array)

```bosatsu
def foldr_Array[a, b](ary: Array[a], init: b, fn: (a, b) -> b) -> b
```

<a id="value-from-list-array"></a>

### `from_List_Array`

references: [`Array`](#type-array), [`List`](../Predef.html#type-list)

```bosatsu
def from_List_Array[a](xs: List[a]) -> Array[a]
```

<a id="value-get-array"></a>

### `get_Array`

references: [`Array`](#type-array), [`Bosatsu/Num/Int64::Int64`](../Num/Int64.html#type-int64), [`Option`](../Predef.html#type-option)

```bosatsu
def get_Array[a](ary: Array[a], idx: Bosatsu/Num/Int64::Int64) -> Option[a]
```

<a id="value-get-map-array"></a>

### `get_map_Array`

references: [`Array`](#type-array), [`Bosatsu/Num/Int64::Int64`](../Num/Int64.html#type-int64)

```bosatsu
def get_map_Array[a,
    b
](ary: Array[a], idx: Bosatsu/Num/Int64::Int64, default: Bosatsu/Num/Int64::Int64 -> b, fn: a -> b) -> b
```

<a id="value-get-or-array"></a>

### `get_or_Array`

references: [`Array`](#type-array), [`Bosatsu/Num/Int64::Int64`](../Num/Int64.html#type-int64)

```bosatsu
def get_or_Array[a
](ary: Array[a], idx: Bosatsu/Num/Int64::Int64, default: Bosatsu/Num/Int64::Int64 -> a) -> a
```

<a id="value-index-in-range-array"></a>

### `index_in_range_Array`

references: [`Array`](#type-array), [`Bool`](../Predef.html#type-bool), [`Bosatsu/Num/Int64::Int64`](../Num/Int64.html#type-int64)

```bosatsu
def index_in_range_Array[a](ary: Array[a], idx: Bosatsu/Num/Int64::Int64) -> Bool
```

<a id="value-int-array-to-char-array"></a>

### `int_Array_to_Char_Array`

references: [`Array`](#type-array), [`Char`](../Predef.html#type-char), [`Int`](../Predef.html#type-int), [`Option`](../Predef.html#type-option)

```bosatsu
def int_Array_to_Char_Array(code_points: Array[Int]) -> Option[Array[Char]]
```

<a id="value-int-array-to-string"></a>

### `int_Array_to_String`

references: [`Array`](#type-array), [`Int`](../Predef.html#type-int), [`Option`](../Predef.html#type-option), [`String`](../Predef.html#type-string)

```bosatsu
def int_Array_to_String(code_points: Array[Int]) -> Option[String]
```

<a id="value-map-array"></a>

### `map_Array`

references: [`Array`](#type-array)

```bosatsu
def map_Array[a, b](ary: Array[a], fn: a -> b) -> Array[b]
```

<a id="value-map-with-index-array"></a>

### `map_with_index_Array`

references: [`Array`](#type-array), [`Bosatsu/Num/Int64::Int64`](../Num/Int64.html#type-int64)

```bosatsu
def map_with_index_Array[a, b](ary: Array[a], fn: (a, Bosatsu/Num/Int64::Int64) -> b) -> Array[b]
```

<a id="value-range-array"></a>

### `range_Array`

references: [`Array`](#type-array), [`Int`](../Predef.html#type-int)

```bosatsu
def range_Array(n: Int) -> Array[Int]
```

<a id="value-range-from-array"></a>

### `range_from_Array`

references: [`Array`](#type-array), [`Int`](../Predef.html#type-int)

```bosatsu
def range_from_Array(start: Int, n: Int) -> Array[Int]
```

<a id="value-reverse-array"></a>

### `reverse_Array`

references: [`Array`](#type-array)

```bosatsu
def reverse_Array[a](ary: Array[a]) -> Array[a]
```

<a id="value-set-array"></a>

### `set_Array`

references: [`Array`](#type-array), [`Bosatsu/Num/Int64::Int64`](../Num/Int64.html#type-int64), [`Option`](../Predef.html#type-option)

```bosatsu
def set_Array[a](ary: Array[a], idx: Bosatsu/Num/Int64::Int64, value: a) -> Option[Array[a]]
```

<a id="value-set-or-self-array"></a>

### `set_or_self_Array`

references: [`Array`](#type-array), [`Bosatsu/Num/Int64::Int64`](../Num/Int64.html#type-int64)

```bosatsu
def set_or_self_Array[a](ary: Array[a], idx: Bosatsu/Num/Int64::Int64, value: a) -> Array[a]
```

<a id="value-size-array"></a>

### `size_Array`

references: [`Array`](#type-array), [`Bosatsu/Num/Int64::Int64`](../Num/Int64.html#type-int64)

```bosatsu
def size_Array[a](ary: Array[a]) -> Bosatsu/Num/Int64::Int64
```

<a id="value-slice-array"></a>

### `slice_Array`

references: [`Array`](#type-array), [`Bosatsu/Num/Int64::Int64`](../Num/Int64.html#type-int64)

```bosatsu
def slice_Array[a
](ary: Array[a], start: Bosatsu/Num/Int64::Int64, end: Bosatsu/Num/Int64::Int64) -> Array[a]
```

<a id="value-sort-array"></a>

### `sort_Array`

references: [`Array`](#type-array), [`Comparison`](../Predef.html#type-comparison)

```bosatsu
def sort_Array[a](ary: Array[a], fn: (a, a) -> Comparison) -> Array[a]
```

<a id="value-string-to-char-array"></a>

### `string_to_Char_Array`

references: [`Array`](#type-array), [`Char`](../Predef.html#type-char), [`String`](../Predef.html#type-string)

```bosatsu
def string_to_Char_Array(s: String) -> Array[Char]
```

<a id="value-string-to-int-array"></a>

### `string_to_Int_Array`

references: [`Array`](#type-array), [`Int`](../Predef.html#type-int), [`String`](../Predef.html#type-string)

```bosatsu
def string_to_Int_Array(s: String) -> Array[Int]
```

<a id="value-sumf-array"></a>

### `sumf_Array`

references: [`Array`](#type-array), [`Float64`](../Predef.html#type-float64)

```bosatsu
def sumf_Array(ary: Array[Float64]) -> Float64
```

<a id="value-sumsqf-array"></a>

### `sumsqf_Array`

references: [`Array`](#type-array), [`Float64`](../Predef.html#type-float64)

```bosatsu
def sumsqf_Array(ary: Array[Float64]) -> Float64
```

<a id="value-tabulate-array"></a>

### `tabulate_Array`

references: [`Array`](#type-array), [`Bosatsu/Num/Int64::Int64`](../Num/Int64.html#type-int64)

```bosatsu
def tabulate_Array[a](n: Bosatsu/Num/Int64::Int64, fn: Bosatsu/Num/Int64::Int64 -> a) -> Array[a]
```

<a id="value-to-list-array"></a>

### `to_List_Array`

references: [`Array`](#type-array), [`List`](../Predef.html#type-list)

```bosatsu
def to_List_Array[a](ary: Array[a]) -> List[a]
```

<a id="value-zip-foldl-array"></a>

### `zip_foldl_Array`

references: [`Array`](#type-array)

```bosatsu
def zip_foldl_Array[a, b, c](left: Array[a], right: Array[b], init: c, fn: (c, a, b) -> c) -> c
```

<a id="value-zip-map-array"></a>

### `zip_map_Array`

references: [`Array`](#type-array)

```bosatsu
def zip_map_Array[a, b, c](left: Array[a], right: Array[b], fn: (a, b) -> c) -> Array[c]
```

<a id="value-zip-sumf-array"></a>

### `zip_sumf_Array`

references: [`Array`](#type-array), [`Float64`](../Predef.html#type-float64)

```bosatsu
def zip_sumf_Array[a, b](left: Array[a], right: Array[b], fn: (a, b) -> Float64) -> Float64
```