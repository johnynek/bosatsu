---
github.base_url=
---

# `Bosatsu/Proto/Wire`

source code:
- [`test_workspace/Bosatsu/Proto/Wire.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Bosatsu/Proto/Wire.bosatsu)

public dependencies: [`Bosatsu/IO/Bytes`](../IO/Bytes.html)

## Index

- Types: [`FieldValue`](#type-fieldvalue)
- Values: [`decode_bool`](#value-decode-bool), [`decode_double`](#value-decode-double),
[`decode_enum`](#value-decode-enum), [`decode_fields`](#value-decode-fields),
[`decode_fixed32`](#value-decode-fixed32), [`decode_fixed64`](#value-decode-fixed64),
[`decode_float`](#value-decode-float), [`decode_int32`](#value-decode-int32),
[`decode_int64`](#value-decode-int64), [`decode_packed_fixed32`](#value-decode-packed-fixed32),
[`decode_packed_fixed64`](#value-decode-packed-fixed64),
[`decode_packed_varints`](#value-decode-packed-varints),
[`decode_sfixed32`](#value-decode-sfixed32), [`decode_sfixed64`](#value-decode-sfixed64),
[`decode_sint32`](#value-decode-sint32), [`decode_sint64`](#value-decode-sint64),
[`decode_uint32`](#value-decode-uint32), [`decode_uint64`](#value-decode-uint64),
[`encode_bool`](#value-encode-bool), [`encode_double_bits`](#value-encode-double-bits),
[`encode_enum`](#value-encode-enum), [`encode_fixed32`](#value-encode-fixed32),
[`encode_fixed32_bits`](#value-encode-fixed32-bits), [`encode_fixed64`](#value-encode-fixed64),
[`encode_fixed64_bits`](#value-encode-fixed64-bits),
[`encode_float_bits`](#value-encode-float-bits), [`encode_int32`](#value-encode-int32),
[`encode_int64`](#value-encode-int64), [`encode_sfixed32_bits`](#value-encode-sfixed32-bits),
[`encode_sfixed64_bits`](#value-encode-sfixed64-bits), [`encode_sint32`](#value-encode-sint32),
[`encode_sint64`](#value-encode-sint64), [`encode_uint32`](#value-encode-uint32),
[`encode_uint64`](#value-encode-uint64), [`encode_varint_u64`](#value-encode-varint-u64),
[`field_fixed32`](#value-field-fixed32), [`field_fixed64`](#value-field-fixed64),
[`field_length_delimited`](#value-field-length-delimited), [`field_varint`](#value-field-varint),
[`if_Some`](#value-if-some)

## Types

<a id="type-fieldvalue"></a>

### `FieldValue`

```bosatsu
type FieldValue
```

#### Constructors

- `Fixed32(bits: Int)`
- `Fixed64(bits: Int)`
- `LengthDelimited(payload: Bosatsu/IO/Bytes::Bytes)`
- `Varint(bits: Int)`

## Values

<a id="value-decode-bool"></a>

### `decode_bool`

references: [`Bool`](../Predef.html#type-bool), [`Int`](../Predef.html#type-int)

```bosatsu
def decode_bool(bits: Int) -> Bool
```

<a id="value-decode-double"></a>

### `decode_double`

references: [`Float64`](../Predef.html#type-float64), [`Int`](../Predef.html#type-int)

```bosatsu
def decode_double(bits: Int) -> Float64
```

<a id="value-decode-enum"></a>

### `decode_enum`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def decode_enum(bits: Int) -> Int
```

<a id="value-decode-fields"></a>

### `decode_fields`

references: [`Bosatsu/IO/Bytes::Bytes`](../IO/Bytes.html#type-bytes), [`FieldValue`](#type-fieldvalue), [`Int`](../Predef.html#type-int), [`List`](../Predef.html#type-list), [`Option`](../Predef.html#type-option), [`Tuple2`](../Predef.html#type-tuple2)

```bosatsu
def decode_fields(bytes: Bosatsu/IO/Bytes::Bytes) -> Option[List[(Int, FieldValue)]]
```

<a id="value-decode-fixed32"></a>

### `decode_fixed32`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def decode_fixed32(bits: Int) -> Int
```

<a id="value-decode-fixed64"></a>

### `decode_fixed64`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def decode_fixed64(bits: Int) -> Int
```

<a id="value-decode-float"></a>

### `decode_float`

references: [`Float64`](../Predef.html#type-float64), [`Int`](../Predef.html#type-int)

```bosatsu
def decode_float(bits: Int) -> Float64
```

<a id="value-decode-int32"></a>

### `decode_int32`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def decode_int32(bits: Int) -> Int
```

<a id="value-decode-int64"></a>

### `decode_int64`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def decode_int64(bits: Int) -> Int
```

<a id="value-decode-packed-fixed32"></a>

### `decode_packed_fixed32`

references: [`Bosatsu/IO/Bytes::Bytes`](../IO/Bytes.html#type-bytes), [`Int`](../Predef.html#type-int), [`List`](../Predef.html#type-list), [`Option`](../Predef.html#type-option)

```bosatsu
def decode_packed_fixed32(bytes: Bosatsu/IO/Bytes::Bytes) -> Option[List[Int]]
```

<a id="value-decode-packed-fixed64"></a>

### `decode_packed_fixed64`

references: [`Bosatsu/IO/Bytes::Bytes`](../IO/Bytes.html#type-bytes), [`Int`](../Predef.html#type-int), [`List`](../Predef.html#type-list), [`Option`](../Predef.html#type-option)

```bosatsu
def decode_packed_fixed64(bytes: Bosatsu/IO/Bytes::Bytes) -> Option[List[Int]]
```

<a id="value-decode-packed-varints"></a>

### `decode_packed_varints`

references: [`Bosatsu/IO/Bytes::Bytes`](../IO/Bytes.html#type-bytes), [`Int`](../Predef.html#type-int), [`List`](../Predef.html#type-list), [`Option`](../Predef.html#type-option)

```bosatsu
def decode_packed_varints(bytes: Bosatsu/IO/Bytes::Bytes) -> Option[List[Int]]
```

<a id="value-decode-sfixed32"></a>

### `decode_sfixed32`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def decode_sfixed32(bits: Int) -> Int
```

<a id="value-decode-sfixed64"></a>

### `decode_sfixed64`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def decode_sfixed64(bits: Int) -> Int
```

<a id="value-decode-sint32"></a>

### `decode_sint32`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def decode_sint32(bits: Int) -> Int
```

<a id="value-decode-sint64"></a>

### `decode_sint64`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def decode_sint64(bits: Int) -> Int
```

<a id="value-decode-uint32"></a>

### `decode_uint32`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def decode_uint32(bits: Int) -> Int
```

<a id="value-decode-uint64"></a>

### `decode_uint64`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def decode_uint64(bits: Int) -> Int
```

<a id="value-encode-bool"></a>

### `encode_bool`

references: [`Bool`](../Predef.html#type-bool), [`Int`](../Predef.html#type-int)

```bosatsu
def encode_bool(value: Bool) -> Int
```

<a id="value-encode-double-bits"></a>

### `encode_double_bits`

references: [`Float64`](../Predef.html#type-float64), [`Int`](../Predef.html#type-int)

```bosatsu
def encode_double_bits(value: Float64) -> Int
```

<a id="value-encode-enum"></a>

### `encode_enum`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def encode_enum(value: Int) -> Int
```

<a id="value-encode-fixed32"></a>

### `encode_fixed32`

references: [`Bosatsu/IO/Bytes::Bytes`](../IO/Bytes.html#type-bytes), [`Int`](../Predef.html#type-int)

```bosatsu
def encode_fixed32(value: Int) -> Bosatsu/IO/Bytes::Bytes
```

<a id="value-encode-fixed32-bits"></a>

### `encode_fixed32_bits`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def encode_fixed32_bits(value: Int) -> Int
```

<a id="value-encode-fixed64"></a>

### `encode_fixed64`

references: [`Bosatsu/IO/Bytes::Bytes`](../IO/Bytes.html#type-bytes), [`Int`](../Predef.html#type-int)

```bosatsu
def encode_fixed64(value: Int) -> Bosatsu/IO/Bytes::Bytes
```

<a id="value-encode-fixed64-bits"></a>

### `encode_fixed64_bits`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def encode_fixed64_bits(value: Int) -> Int
```

<a id="value-encode-float-bits"></a>

### `encode_float_bits`

references: [`Float64`](../Predef.html#type-float64), [`Int`](../Predef.html#type-int)

```bosatsu
def encode_float_bits(value: Float64) -> Int
```

<a id="value-encode-int32"></a>

### `encode_int32`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def encode_int32(value: Int) -> Int
```

<a id="value-encode-int64"></a>

### `encode_int64`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def encode_int64(value: Int) -> Int
```

<a id="value-encode-sfixed32-bits"></a>

### `encode_sfixed32_bits`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def encode_sfixed32_bits(value: Int) -> Int
```

<a id="value-encode-sfixed64-bits"></a>

### `encode_sfixed64_bits`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def encode_sfixed64_bits(value: Int) -> Int
```

<a id="value-encode-sint32"></a>

### `encode_sint32`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def encode_sint32(value: Int) -> Int
```

<a id="value-encode-sint64"></a>

### `encode_sint64`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def encode_sint64(value: Int) -> Int
```

<a id="value-encode-uint32"></a>

### `encode_uint32`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def encode_uint32(value: Int) -> Int
```

<a id="value-encode-uint64"></a>

### `encode_uint64`

references: [`Int`](../Predef.html#type-int)

```bosatsu
def encode_uint64(value: Int) -> Int
```

<a id="value-encode-varint-u64"></a>

### `encode_varint_u64`

references: [`Bosatsu/IO/Bytes::Bytes`](../IO/Bytes.html#type-bytes), [`Int`](../Predef.html#type-int)

```bosatsu
def encode_varint_u64(value: Int) -> Bosatsu/IO/Bytes::Bytes
```

<a id="value-field-fixed32"></a>

### `field_fixed32`

references: [`Bosatsu/IO/Bytes::Bytes`](../IO/Bytes.html#type-bytes), [`Int`](../Predef.html#type-int)

```bosatsu
def field_fixed32(field_number: Int, value: Int) -> Bosatsu/IO/Bytes::Bytes
```

<a id="value-field-fixed64"></a>

### `field_fixed64`

references: [`Bosatsu/IO/Bytes::Bytes`](../IO/Bytes.html#type-bytes), [`Int`](../Predef.html#type-int)

```bosatsu
def field_fixed64(field_number: Int, value: Int) -> Bosatsu/IO/Bytes::Bytes
```

<a id="value-field-length-delimited"></a>

### `field_length_delimited`

references: [`Bosatsu/IO/Bytes::Bytes`](../IO/Bytes.html#type-bytes), [`Int`](../Predef.html#type-int)

```bosatsu
def field_length_delimited(field_number: Int, payload: Bosatsu/IO/Bytes::Bytes) -> Bosatsu/IO/Bytes::Bytes
```

<a id="value-field-varint"></a>

### `field_varint`

references: [`Bosatsu/IO/Bytes::Bytes`](../IO/Bytes.html#type-bytes), [`Int`](../Predef.html#type-int)

```bosatsu
def field_varint(field_number: Int, value: Int) -> Bosatsu/IO/Bytes::Bytes
```

<a id="value-if-some"></a>

### `if_Some`

references: [`Option`](../Predef.html#type-option)

```bosatsu
def if_Some[a, b](o: Option[a], fn: a -> Option[b]) -> Option[b]
```