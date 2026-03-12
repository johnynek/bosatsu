---
github.base_url=
---

# `Bosatsu/IO/Bytes`

source code:
- [`test_workspace/Bosatsu/IO/Bytes.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Bosatsu/IO/Bytes.bosatsu)

public dependencies: [`Bosatsu/Collection/Array`](../Collection/Array.html)

## Index

- Types: [`Bytes`](#type-bytes)
- Values: [`concat_all_Bytes`](#value-concat-all-bytes), [`empty_Bytes`](#value-empty-bytes),
[`ends_with_Bytes`](#value-ends-with-bytes), [`find_Bytes`](#value-find-bytes),
[`foldl_Bytes`](#value-foldl-bytes), [`from_Array_Int`](#value-from-array-int),
[`from_List_Int`](#value-from-list-int), [`get_Bytes`](#value-get-bytes),
[`get_map_Bytes`](#value-get-map-bytes), [`get_or_Bytes`](#value-get-or-bytes),
[`index_in_range_Bytes`](#value-index-in-range-bytes), [`size_Bytes`](#value-size-bytes),
[`slice_Bytes`](#value-slice-bytes), [`starts_with_Bytes`](#value-starts-with-bytes),
[`to_Array_Int`](#value-to-array-int), [`to_List_Int`](#value-to-list-int),
[`utf8_Char_at`](#value-utf8-char-at), [`utf8_bytes_from_String`](#value-utf8-bytes-from-string),
[`utf8_bytes_to_String`](#value-utf8-bytes-to-string)

## Types

<a id="type-bytes"></a>

### `Bytes`

```bosatsu
type Bytes
```

## Values

<a id="value-concat-all-bytes"></a>

### `concat_all_Bytes`

references: [`Bytes`](#type-bytes), [`List`](../Predef.html#type-list)

```bosatsu
def concat_all_Bytes(chunks: List[Bytes]) -> Bytes
```

<a id="value-empty-bytes"></a>

### `empty_Bytes`

references: [`Bytes`](#type-bytes)

```bosatsu
empty_Bytes: Bytes
```

<a id="value-ends-with-bytes"></a>

### `ends_with_Bytes`

references: [`Bool`](../Predef.html#type-bool), [`Bytes`](#type-bytes)

```bosatsu
def ends_with_Bytes(bytes: Bytes, suffix: Bytes) -> Bool
```

<a id="value-find-bytes"></a>

### `find_Bytes`

references: [`Bytes`](#type-bytes), [`Int`](../Predef.html#type-int)

```bosatsu
def find_Bytes(bytes: Bytes, needle: Bytes, start: Int) -> Int
```

<a id="value-foldl-bytes"></a>

### `foldl_Bytes`

references: [`Bytes`](#type-bytes), [`Int`](../Predef.html#type-int)

```bosatsu
def foldl_Bytes[a](bytes: Bytes, init: a, fn: (a, Int) -> a) -> a
```

<a id="value-from-array-int"></a>

### `from_Array_Int`

references: [`Bosatsu/Collection/Array::Array`](../Collection/Array.html#type-array), [`Bytes`](#type-bytes), [`Int`](../Predef.html#type-int)

```bosatsu
def from_Array_Int(ints: Bosatsu/Collection/Array::Array[Int]) -> Bytes
```

<a id="value-from-list-int"></a>

### `from_List_Int`

references: [`Bytes`](#type-bytes), [`Int`](../Predef.html#type-int), [`List`](../Predef.html#type-list)

```bosatsu
def from_List_Int(ints: List[Int]) -> Bytes
```

<a id="value-get-bytes"></a>

### `get_Bytes`

references: [`Bytes`](#type-bytes), [`Int`](../Predef.html#type-int), [`Option`](../Predef.html#type-option)

```bosatsu
def get_Bytes(bytes: Bytes, idx: Int) -> Option[Int]
```

<a id="value-get-map-bytes"></a>

### `get_map_Bytes`

references: [`Bytes`](#type-bytes), [`Int`](../Predef.html#type-int), [`Unit`](../Predef.html#type-unit)

```bosatsu
def get_map_Bytes[a](bytes: Bytes, idx: Int, default: () -> a, fn: Int -> a) -> a
```

<a id="value-get-or-bytes"></a>

### `get_or_Bytes`

references: [`Bytes`](#type-bytes), [`Int`](../Predef.html#type-int), [`Unit`](../Predef.html#type-unit)

```bosatsu
def get_or_Bytes(bytes: Bytes, idx: Int, default: () -> Int) -> Int
```

<a id="value-index-in-range-bytes"></a>

### `index_in_range_Bytes`

references: [`Bool`](../Predef.html#type-bool), [`Bytes`](#type-bytes), [`Int`](../Predef.html#type-int)

```bosatsu
def index_in_range_Bytes(bytes: Bytes, idx: Int) -> Bool
```

<a id="value-size-bytes"></a>

### `size_Bytes`

references: [`Bytes`](#type-bytes), [`Int`](../Predef.html#type-int)

```bosatsu
def size_Bytes(bytes: Bytes) -> Int
```

<a id="value-slice-bytes"></a>

### `slice_Bytes`

references: [`Bytes`](#type-bytes), [`Int`](../Predef.html#type-int)

```bosatsu
def slice_Bytes(bytes: Bytes, start: Int, end: Int) -> Bytes
```

<a id="value-starts-with-bytes"></a>

### `starts_with_Bytes`

references: [`Bool`](../Predef.html#type-bool), [`Bytes`](#type-bytes)

```bosatsu
def starts_with_Bytes(bytes: Bytes, prefix: Bytes) -> Bool
```

<a id="value-to-array-int"></a>

### `to_Array_Int`

references: [`Bosatsu/Collection/Array::Array`](../Collection/Array.html#type-array), [`Bytes`](#type-bytes), [`Int`](../Predef.html#type-int)

```bosatsu
def to_Array_Int(bytes: Bytes) -> Bosatsu/Collection/Array::Array[Int]
```

<a id="value-to-list-int"></a>

### `to_List_Int`

references: [`Bytes`](#type-bytes), [`Int`](../Predef.html#type-int), [`List`](../Predef.html#type-list)

```bosatsu
def to_List_Int(bytes: Bytes) -> List[Int]
```

<a id="value-utf8-char-at"></a>

### `utf8_Char_at`

references: [`Bytes`](#type-bytes), [`Char`](../Predef.html#type-char), [`Int`](../Predef.html#type-int), [`Option`](../Predef.html#type-option)

```bosatsu
def utf8_Char_at(bytes: Bytes, idx: Int) -> Option[Char]
```

<a id="value-utf8-bytes-from-string"></a>

### `utf8_bytes_from_String`

references: [`Bytes`](#type-bytes), [`String`](../Predef.html#type-string)

```bosatsu
def utf8_bytes_from_String(str: String) -> Bytes
```

<a id="value-utf8-bytes-to-string"></a>

### `utf8_bytes_to_String`

references: [`Bytes`](#type-bytes), [`Option`](../Predef.html#type-option), [`String`](../Predef.html#type-string)

```bosatsu
def utf8_bytes_to_String(bytes: Bytes) -> Option[String]
```