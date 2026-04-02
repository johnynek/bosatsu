---
github.base_url=
---

# `Bosatsu/Predef`

## Index

- Types: [`Bool`](#type-bool), [`Char`](#type-char), [`Comparison`](#type-comparison),
[`Dict`](#type-dict), [`Float64`](#type-float64), [`Fn1`](#type-fn1), [`Fn2`](#type-fn2),
[`Fn3`](#type-fn3), [`Fn4`](#type-fn4), [`Fn5`](#type-fn5), [`Fn6`](#type-fn6), [`Fn7`](#type-fn7),
[`Fn8`](#type-fn8), [`Fn9`](#type-fn9), [`Fn10`](#type-fn10), [`Fn11`](#type-fn11),
[`Fn12`](#type-fn12), [`Fn13`](#type-fn13), [`Fn14`](#type-fn14), [`Fn15`](#type-fn15),
[`Fn16`](#type-fn16), [`Fn17`](#type-fn17), [`Fn18`](#type-fn18), [`Fn19`](#type-fn19),
[`Fn20`](#type-fn20), [`Fn21`](#type-fn21), [`Fn22`](#type-fn22), [`Fn23`](#type-fn23),
[`Fn24`](#type-fn24), [`Fn25`](#type-fn25), [`Fn26`](#type-fn26), [`Fn27`](#type-fn27),
[`Fn28`](#type-fn28), [`Fn29`](#type-fn29), [`Fn30`](#type-fn30), [`Fn31`](#type-fn31),
[`Fn32`](#type-fn32), [`Int`](#type-int), [`List`](#type-list), [`Option`](#type-option),
[`Order`](#type-order), [`String`](#type-string), [`Test`](#type-test), [`Tuple1`](#type-tuple1),
[`Tuple2`](#type-tuple2), [`Tuple3`](#type-tuple3), [`Tuple4`](#type-tuple4),
[`Tuple5`](#type-tuple5), [`Tuple6`](#type-tuple6), [`Tuple7`](#type-tuple7),
[`Tuple8`](#type-tuple8), [`Tuple9`](#type-tuple9), [`Tuple10`](#type-tuple10),
[`Tuple11`](#type-tuple11), [`Tuple12`](#type-tuple12), [`Tuple13`](#type-tuple13),
[`Tuple14`](#type-tuple14), [`Tuple15`](#type-tuple15), [`Tuple16`](#type-tuple16),
[`Tuple17`](#type-tuple17), [`Tuple18`](#type-tuple18), [`Tuple19`](#type-tuple19),
[`Tuple20`](#type-tuple20), [`Tuple21`](#type-tuple21), [`Tuple22`](#type-tuple22),
[`Tuple23`](#type-tuple23), [`Tuple24`](#type-tuple24), [`Tuple25`](#type-tuple25),
[`Tuple26`](#type-tuple26), [`Tuple27`](#type-tuple27), [`Tuple28`](#type-tuple28),
[`Tuple29`](#type-tuple29), [`Tuple30`](#type-tuple30), [`Tuple31`](#type-tuple31),
[`Tuple32`](#type-tuple32), [`Unit`](#type-unit)
- Values: [`add`](#value-add), [`add_key`](#value-add-key), [`addf`](#value-addf),
[`and`](#value-and), [`and_Int`](#value-and-int), [`build_List`](#value-build-list),
[`char_List_to_String`](#value-char-list-to-string), [`char_to_Int`](#value-char-to-int),
[`char_to_String`](#value-char-to-string), [`clear_Dict`](#value-clear-dict),
[`cmp_Bool`](#value-cmp-bool), [`cmp_Char`](#value-cmp-char),
[`cmp_Comparison`](#value-cmp-comparison), [`cmp_Float64`](#value-cmp-float64),
[`cmp_Int`](#value-cmp-int), [`cmp_String`](#value-cmp-string), [`concat`](#value-concat),
[`concat_String`](#value-concat-string), [`div`](#value-div), [`divf`](#value-divf),
[`empty_Dict`](#value-empty-dict), [`eq_Bool`](#value-eq-bool), [`eq_Char`](#value-eq-char),
[`eq_Comparison`](#value-eq-comparison), [`eq_Float64`](#value-eq-float64),
[`eq_Int`](#value-eq-int), [`eq_String`](#value-eq-string), [`flat_map_List`](#value-flat-map-list),
[`foldl_List`](#value-foldl-list), [`foldr_List`](#value-foldr-list), [`gcd_Int`](#value-gcd-int),
[`get_key`](#value-get-key), [`implies`](#value-implies), [`int_to_Char`](#value-int-to-char),
[`int_to_String`](#value-int-to-string), [`items`](#value-items),
[`length_List`](#value-length-list), [`length_String`](#value-length-string),
[`map_List`](#value-map-list), [`mod_Int`](#value-mod-int), [`mul`](#value-mul),
[`mulf`](#value-mulf), [`not`](#value-not), [`not_Int`](#value-not-int), [`or`](#value-or),
[`or_Int`](#value-or-int), [`partition_String`](#value-partition-string),
[`popcount_Int`](#value-popcount-int), [`range`](#value-range), [`remove_key`](#value-remove-key),
[`replicate_List`](#value-replicate-list), [`reverse`](#value-reverse),
[`reverse_concat`](#value-reverse-concat), [`rpartition_String`](#value-rpartition-string),
[`shift_left_Int`](#value-shift-left-int), [`shift_right_Int`](#value-shift-right-int),
[`string_Order`](#value-string-order), [`string_to_Int`](#value-string-to-int), [`sub`](#value-sub),
[`subf`](#value-subf), [`tail_or_empty_String`](#value-tail-or-empty-string),
[`trace`](#value-trace), [`uncons_String`](#value-uncons-string), [`xor`](#value-xor),
[`xor_Int`](#value-xor-int)

## Types

<a id="type-bool"></a>

### `Bool`

```bosatsu
type Bool
```

#### Constructors

- `False`
- `True`

<a id="type-char"></a>

### `Char`

```bosatsu
type Char
```

<a id="type-comparison"></a>

### `Comparison`

Standardize notion of ordering

```bosatsu
type Comparison
```

#### Constructors

- `EQ`
- `GT`
- `LT`

<a id="type-dict"></a>

### `Dict[k, v]`

Standard dictionaries

```bosatsu
type Dict[k: *, v: +*]
```

<a id="type-float64"></a>

### `Float64`

```bosatsu
type Float64
```

<a id="type-fn1"></a>

### `Fn1[i0, z]`

```bosatsu
type Fn1[i0: -*, z: +*]
```

<a id="type-fn2"></a>

### `Fn2[i0, i1, z]`

```bosatsu
type Fn2[i0: -*, i1: -*, z: +*]
```

<a id="type-fn3"></a>

### `Fn3[i0, i1, i2, z]`

```bosatsu
type Fn3[i0: -*, i1: -*, i2: -*, z: +*]
```

<a id="type-fn4"></a>

### `Fn4[i0, i1, i2, i3, z]`

```bosatsu
type Fn4[i0: -*, i1: -*, i2: -*, i3: -*, z: +*]
```

<a id="type-fn5"></a>

### `Fn5[i0, i1, i2, i3, i4, z]`

```bosatsu
type Fn5[i0: -*, i1: -*, i2: -*, i3: -*, i4: -*, z: +*]
```

<a id="type-fn6"></a>

### `Fn6[i0, i1, i2, i3, i4, i5, z]`

```bosatsu
type Fn6[i0: -*, i1: -*, i2: -*, i3: -*, i4: -*, i5: -*, z: +*]
```

<a id="type-fn7"></a>

### `Fn7[i0, i1, i2, i3, i4, i5, i6, z]`

```bosatsu
type Fn7[i0: -*, i1: -*, i2: -*, i3: -*, i4: -*, i5: -*, i6: -*, z: +*]
```

<a id="type-fn8"></a>

### `Fn8[i0, i1, i2, i3, i4, i5, i6, i7, z]`

```bosatsu
type Fn8[i0: -*, i1: -*, i2: -*, i3: -*, i4: -*, i5: -*, i6: -*, i7: -*, z: +*]
```

<a id="type-fn9"></a>

### `Fn9[i0, i1, i2, i3, i4, i5, i6, i7, i8, z]`

```bosatsu
type Fn9[i0: -*, i1: -*, i2: -*, i3: -*, i4: -*, i5: -*, i6: -*, i7: -*, i8: -*, z: +*]
```

<a id="type-fn10"></a>

### `Fn10[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, z]`

```bosatsu
type Fn10[i0: -*, i1: -*, i2: -*, i3: -*, i4: -*, i5: -*, i6: -*, i7: -*, i8: -*, i9: -*, z: +*]
```

<a id="type-fn11"></a>

### `Fn11[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, z]`

```bosatsu
type Fn11[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    z: +*]
```

<a id="type-fn12"></a>

### `Fn12[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, z]`

```bosatsu
type Fn12[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    z: +*]
```

<a id="type-fn13"></a>

### `Fn13[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, z]`

```bosatsu
type Fn13[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    z: +*]
```

<a id="type-fn14"></a>

### `Fn14[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, z]`

```bosatsu
type Fn14[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    z: +*]
```

<a id="type-fn15"></a>

### `Fn15[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, z]`

```bosatsu
type Fn15[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    z: +*]
```

<a id="type-fn16"></a>

### `Fn16[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, z]`

```bosatsu
type Fn16[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    z: +*]
```

<a id="type-fn17"></a>

### `Fn17[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, z]`

```bosatsu
type Fn17[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    i16: -*,
    z: +*]
```

<a id="type-fn18"></a>

### `Fn18[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, z]`

```bosatsu
type Fn18[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    i16: -*,
    i17: -*,
    z: +*]
```

<a id="type-fn19"></a>

### `Fn19[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, z]`

```bosatsu
type Fn19[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    i16: -*,
    i17: -*,
    i18: -*,
    z: +*]
```

<a id="type-fn20"></a>

### `Fn20[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, z]`

```bosatsu
type Fn20[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    i16: -*,
    i17: -*,
    i18: -*,
    i19: -*,
    z: +*]
```

<a id="type-fn21"></a>

### `Fn21[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, z]`

```bosatsu
type Fn21[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    i16: -*,
    i17: -*,
    i18: -*,
    i19: -*,
    i20: -*,
    z: +*]
```

<a id="type-fn22"></a>

### `Fn22[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, z]`

```bosatsu
type Fn22[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    i16: -*,
    i17: -*,
    i18: -*,
    i19: -*,
    i20: -*,
    i21: -*,
    z: +*]
```

<a id="type-fn23"></a>

### `Fn23[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, z]`

```bosatsu
type Fn23[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    i16: -*,
    i17: -*,
    i18: -*,
    i19: -*,
    i20: -*,
    i21: -*,
    i22: -*,
    z: +*]
```

<a id="type-fn24"></a>

### `Fn24[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, z]`

```bosatsu
type Fn24[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    i16: -*,
    i17: -*,
    i18: -*,
    i19: -*,
    i20: -*,
    i21: -*,
    i22: -*,
    i23: -*,
    z: +*]
```

<a id="type-fn25"></a>

### `Fn25[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, z]`

```bosatsu
type Fn25[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    i16: -*,
    i17: -*,
    i18: -*,
    i19: -*,
    i20: -*,
    i21: -*,
    i22: -*,
    i23: -*,
    i24: -*,
    z: +*]
```

<a id="type-fn26"></a>

### `Fn26[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, z]`

```bosatsu
type Fn26[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    i16: -*,
    i17: -*,
    i18: -*,
    i19: -*,
    i20: -*,
    i21: -*,
    i22: -*,
    i23: -*,
    i24: -*,
    i25: -*,
    z: +*]
```

<a id="type-fn27"></a>

### `Fn27[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, z]`

```bosatsu
type Fn27[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    i16: -*,
    i17: -*,
    i18: -*,
    i19: -*,
    i20: -*,
    i21: -*,
    i22: -*,
    i23: -*,
    i24: -*,
    i25: -*,
    i26: -*,
    z: +*]
```

<a id="type-fn28"></a>

### `Fn28[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, z]`

```bosatsu
type Fn28[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    i16: -*,
    i17: -*,
    i18: -*,
    i19: -*,
    i20: -*,
    i21: -*,
    i22: -*,
    i23: -*,
    i24: -*,
    i25: -*,
    i26: -*,
    i27: -*,
    z: +*]
```

<a id="type-fn29"></a>

### `Fn29[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, z]`

```bosatsu
type Fn29[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    i16: -*,
    i17: -*,
    i18: -*,
    i19: -*,
    i20: -*,
    i21: -*,
    i22: -*,
    i23: -*,
    i24: -*,
    i25: -*,
    i26: -*,
    i27: -*,
    i28: -*,
    z: +*]
```

<a id="type-fn30"></a>

### `Fn30[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, z]`

```bosatsu
type Fn30[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    i16: -*,
    i17: -*,
    i18: -*,
    i19: -*,
    i20: -*,
    i21: -*,
    i22: -*,
    i23: -*,
    i24: -*,
    i25: -*,
    i26: -*,
    i27: -*,
    i28: -*,
    i29: -*,
    z: +*]
```

<a id="type-fn31"></a>

### `Fn31[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, z]`

```bosatsu
type Fn31[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    i16: -*,
    i17: -*,
    i18: -*,
    i19: -*,
    i20: -*,
    i21: -*,
    i22: -*,
    i23: -*,
    i24: -*,
    i25: -*,
    i26: -*,
    i27: -*,
    i28: -*,
    i29: -*,
    i30: -*,
    z: +*]
```

<a id="type-fn32"></a>

### `Fn32[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, z]`

```bosatsu
type Fn32[i0: -*,
    i1: -*,
    i2: -*,
    i3: -*,
    i4: -*,
    i5: -*,
    i6: -*,
    i7: -*,
    i8: -*,
    i9: -*,
    i10: -*,
    i11: -*,
    i12: -*,
    i13: -*,
    i14: -*,
    i15: -*,
    i16: -*,
    i17: -*,
    i18: -*,
    i19: -*,
    i20: -*,
    i21: -*,
    i22: -*,
    i23: -*,
    i24: -*,
    i25: -*,
    i26: -*,
    i27: -*,
    i28: -*,
    i29: -*,
    i30: -*,
    i31: -*,
    z: +*]
```

<a id="type-int"></a>

### `Int`

Int functions

```bosatsu
type Int
```

<a id="type-list"></a>

### `List[a]`

Support for built-in lists

```bosatsu
type List[a: +*]
```

#### Constructors

- `EmptyList`
- `NonEmptyList(head: a, tail: List[a])`

<a id="type-option"></a>

### `Option[a]`

```bosatsu
type Option[a: +*]
```

#### Constructors

- `None`
- `Some(v: a)`

<a id="type-order"></a>

### `Order[a]`

```bosatsu
type Order[a: -*]
```

#### Constructors

- `Order(to_Fn: (a, a) -> Comparison)`

<a id="type-string"></a>

### `String`

String functions

```bosatsu
type String
```

<a id="type-test"></a>

### `Test`

Support for built-in testing:

```bosatsu
type Test
```

#### Constructors

- `Assertion(value: Bool, message: String)`
- `TestSuite(name: String, tests: List[Test])`

<a id="type-tuple1"></a>

### `Tuple1[a]`

```bosatsu
type Tuple1[a: +*]
```

#### Constructors

- `Tuple1(item1: a)`

<a id="type-tuple2"></a>

### `Tuple2[a, b]`

```bosatsu
type Tuple2[a: +*, b: +*]
```

#### Constructors

- `Tuple2(item1: a, item2: b)`

<a id="type-tuple3"></a>

### `Tuple3[a, b, c]`

```bosatsu
type Tuple3[a: +*, b: +*, c: +*]
```

#### Constructors

- `Tuple3(item1: a, item2: b, item3: c)`

<a id="type-tuple4"></a>

### `Tuple4[a, b, c, d]`

```bosatsu
type Tuple4[a: +*, b: +*, c: +*, d: +*]
```

#### Constructors

- `Tuple4(item1: a, item2: b, item3: c, item4: d)`

<a id="type-tuple5"></a>

### `Tuple5[a, b, c, d, e]`

```bosatsu
type Tuple5[a: +*, b: +*, c: +*, d: +*, e: +*]
```

#### Constructors

- `Tuple5(item1: a, item2: b, item3: c, item4: d, item5: e)`

<a id="type-tuple6"></a>

### `Tuple6[a, b, c, d, e, f]`

```bosatsu
type Tuple6[a: +*, b: +*, c: +*, d: +*, e: +*, f: +*]
```

#### Constructors

- `Tuple6(item1: a, item2: b, item3: c, item4: d, item5: e, item6: f)`

<a id="type-tuple7"></a>

### `Tuple7[a, b, c, d, e, f, g]`

```bosatsu
type Tuple7[a: +*, b: +*, c: +*, d: +*, e: +*, f: +*, g: +*]
```

#### Constructors

- `Tuple7(item1: a, item2: b, item3: c, item4: d, item5: e, item6: f, item7: g)`

<a id="type-tuple8"></a>

### `Tuple8[a, b, c, d, e, f, g, h]`

```bosatsu
type Tuple8[a: +*, b: +*, c: +*, d: +*, e: +*, f: +*, g: +*, h: +*]
```

#### Constructors

- `Tuple8(item1: a, item2: b, item3: c, item4: d, item5: e, item6: f, item7: g, item8: h)`

<a id="type-tuple9"></a>

### `Tuple9[a, b, c, d, e, f, g, h, i]`

```bosatsu
type Tuple9[a: +*, b: +*, c: +*, d: +*, e: +*, f: +*, g: +*, h: +*, i: +*]
```

#### Constructors

- `Tuple9(item1: a, item2: b, item3: c, item4: d, item5: e, item6: f, item7: g, item8: h, item9: i)`

<a id="type-tuple10"></a>

### `Tuple10[a, b, c, d, e, f, g, h, i, j]`

```bosatsu
type Tuple10[a: +*, b: +*, c: +*, d: +*, e: +*, f: +*, g: +*, h: +*, i: +*, j: +*]
```

#### Constructors

- Tuple10(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j
  )

<a id="type-tuple11"></a>

### `Tuple11[a, b, c, d, e, f, g, h, i, j, k]`

```bosatsu
type Tuple11[a: +*, b: +*, c: +*, d: +*, e: +*, f: +*, g: +*, h: +*, i: +*, j: +*, k: +*]
```

#### Constructors

- Tuple11(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k
  )

<a id="type-tuple12"></a>

### `Tuple12[a, b, c, d, e, f, g, h, i, j, k, l]`

```bosatsu
type Tuple12[a: +*, b: +*, c: +*, d: +*, e: +*, f: +*, g: +*, h: +*, i: +*, j: +*, k: +*, l: +*]
```

#### Constructors

- Tuple12(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l
  )

<a id="type-tuple13"></a>

### `Tuple13[a, b, c, d, e, f, g, h, i, j, k, l, m]`

```bosatsu
type Tuple13[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*]
```

#### Constructors

- Tuple13(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m
  )

<a id="type-tuple14"></a>

### `Tuple14[a, b, c, d, e, f, g, h, i, j, k, l, m, n]`

```bosatsu
type Tuple14[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*]
```

#### Constructors

- Tuple14(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n
  )

<a id="type-tuple15"></a>

### `Tuple15[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o]`

```bosatsu
type Tuple15[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*]
```

#### Constructors

- Tuple15(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o
  )

<a id="type-tuple16"></a>

### `Tuple16[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p]`

```bosatsu
type Tuple16[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*]
```

#### Constructors

- Tuple16(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p
  )

<a id="type-tuple17"></a>

### `Tuple17[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q]`

```bosatsu
type Tuple17[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*,
    q: +*]
```

#### Constructors

- Tuple17(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p,
      item17: q
  )

<a id="type-tuple18"></a>

### `Tuple18[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r]`

```bosatsu
type Tuple18[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*,
    q: +*,
    r: +*]
```

#### Constructors

- Tuple18(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p,
      item17: q,
      item18: r
  )

<a id="type-tuple19"></a>

### `Tuple19[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s]`

```bosatsu
type Tuple19[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*,
    q: +*,
    r: +*,
    s: +*]
```

#### Constructors

- Tuple19(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p,
      item17: q,
      item18: r,
      item19: s
  )

<a id="type-tuple20"></a>

### `Tuple20[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t]`

```bosatsu
type Tuple20[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*,
    q: +*,
    r: +*,
    s: +*,
    t: +*]
```

#### Constructors

- Tuple20(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p,
      item17: q,
      item18: r,
      item19: s,
      item20: t
  )

<a id="type-tuple21"></a>

### `Tuple21[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u]`

```bosatsu
type Tuple21[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*,
    q: +*,
    r: +*,
    s: +*,
    t: +*,
    u: +*]
```

#### Constructors

- Tuple21(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p,
      item17: q,
      item18: r,
      item19: s,
      item20: t,
      item21: u
  )

<a id="type-tuple22"></a>

### `Tuple22[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v]`

```bosatsu
type Tuple22[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*,
    q: +*,
    r: +*,
    s: +*,
    t: +*,
    u: +*,
    v: +*]
```

#### Constructors

- Tuple22(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p,
      item17: q,
      item18: r,
      item19: s,
      item20: t,
      item21: u,
      item22: v
  )

<a id="type-tuple23"></a>

### `Tuple23[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w]`

```bosatsu
type Tuple23[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*,
    q: +*,
    r: +*,
    s: +*,
    t: +*,
    u: +*,
    v: +*,
    w: +*]
```

#### Constructors

- Tuple23(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p,
      item17: q,
      item18: r,
      item19: s,
      item20: t,
      item21: u,
      item22: v,
      item23: w
  )

<a id="type-tuple24"></a>

### `Tuple24[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x]`

```bosatsu
type Tuple24[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*,
    q: +*,
    r: +*,
    s: +*,
    t: +*,
    u: +*,
    v: +*,
    w: +*,
    x: +*]
```

#### Constructors

- Tuple24(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p,
      item17: q,
      item18: r,
      item19: s,
      item20: t,
      item21: u,
      item22: v,
      item23: w,
      item24: x
  )

<a id="type-tuple25"></a>

### `Tuple25[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y]`

```bosatsu
type Tuple25[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*,
    q: +*,
    r: +*,
    s: +*,
    t: +*,
    u: +*,
    v: +*,
    w: +*,
    x: +*,
    y: +*]
```

#### Constructors

- Tuple25(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p,
      item17: q,
      item18: r,
      item19: s,
      item20: t,
      item21: u,
      item22: v,
      item23: w,
      item24: x,
      item25: y
  )

<a id="type-tuple26"></a>

### `Tuple26[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]`

```bosatsu
type Tuple26[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*,
    q: +*,
    r: +*,
    s: +*,
    t: +*,
    u: +*,
    v: +*,
    w: +*,
    x: +*,
    y: +*,
    z: +*]
```

#### Constructors

- Tuple26(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p,
      item17: q,
      item18: r,
      item19: s,
      item20: t,
      item21: u,
      item22: v,
      item23: w,
      item24: x,
      item25: y,
      item26: z
  )

<a id="type-tuple27"></a>

### `Tuple27[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a0]`

```bosatsu
type Tuple27[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*,
    q: +*,
    r: +*,
    s: +*,
    t: +*,
    u: +*,
    v: +*,
    w: +*,
    x: +*,
    y: +*,
    z: +*,
    a0: +*]
```

#### Constructors

- Tuple27(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p,
      item17: q,
      item18: r,
      item19: s,
      item20: t,
      item21: u,
      item22: v,
      item23: w,
      item24: x,
      item25: y,
      item26: z,
      item27: a0
  )

<a id="type-tuple28"></a>

### `Tuple28[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a0, b0]`

```bosatsu
type Tuple28[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*,
    q: +*,
    r: +*,
    s: +*,
    t: +*,
    u: +*,
    v: +*,
    w: +*,
    x: +*,
    y: +*,
    z: +*,
    a0: +*,
    b0: +*]
```

#### Constructors

- Tuple28(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p,
      item17: q,
      item18: r,
      item19: s,
      item20: t,
      item21: u,
      item22: v,
      item23: w,
      item24: x,
      item25: y,
      item26: z,
      item27: a0,
      item28: b0
  )

<a id="type-tuple29"></a>

### `Tuple29[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a0, b0, c0]`

```bosatsu
type Tuple29[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*,
    q: +*,
    r: +*,
    s: +*,
    t: +*,
    u: +*,
    v: +*,
    w: +*,
    x: +*,
    y: +*,
    z: +*,
    a0: +*,
    b0: +*,
    c0: +*]
```

#### Constructors

- Tuple29(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p,
      item17: q,
      item18: r,
      item19: s,
      item20: t,
      item21: u,
      item22: v,
      item23: w,
      item24: x,
      item25: y,
      item26: z,
      item27: a0,
      item28: b0,
      item29: c0
  )

<a id="type-tuple30"></a>

### `Tuple30[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a0, b0, c0, d0]`

```bosatsu
type Tuple30[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*,
    q: +*,
    r: +*,
    s: +*,
    t: +*,
    u: +*,
    v: +*,
    w: +*,
    x: +*,
    y: +*,
    z: +*,
    a0: +*,
    b0: +*,
    c0: +*,
    d0: +*]
```

#### Constructors

- Tuple30(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p,
      item17: q,
      item18: r,
      item19: s,
      item20: t,
      item21: u,
      item22: v,
      item23: w,
      item24: x,
      item25: y,
      item26: z,
      item27: a0,
      item28: b0,
      item29: c0,
      item30: d0
  )

<a id="type-tuple31"></a>

### `Tuple31[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a0, b0, c0, d0, e0]`

```bosatsu
type Tuple31[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*,
    q: +*,
    r: +*,
    s: +*,
    t: +*,
    u: +*,
    v: +*,
    w: +*,
    x: +*,
    y: +*,
    z: +*,
    a0: +*,
    b0: +*,
    c0: +*,
    d0: +*,
    e0: +*]
```

#### Constructors

- Tuple31(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p,
      item17: q,
      item18: r,
      item19: s,
      item20: t,
      item21: u,
      item22: v,
      item23: w,
      item24: x,
      item25: y,
      item26: z,
      item27: a0,
      item28: b0,
      item29: c0,
      item30: d0,
      item31: e0
  )

<a id="type-tuple32"></a>

### `Tuple32[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a0, b0, c0, d0, e0, f0]`

```bosatsu
type Tuple32[a: +*,
    b: +*,
    c: +*,
    d: +*,
    e: +*,
    f: +*,
    g: +*,
    h: +*,
    i: +*,
    j: +*,
    k: +*,
    l: +*,
    m: +*,
    n: +*,
    o: +*,
    p: +*,
    q: +*,
    r: +*,
    s: +*,
    t: +*,
    u: +*,
    v: +*,
    w: +*,
    x: +*,
    y: +*,
    z: +*,
    a0: +*,
    b0: +*,
    c0: +*,
    d0: +*,
    e0: +*,
    f0: +*]
```

#### Constructors

- Tuple32(
      item1: a,
      item2: b,
      item3: c,
      item4: d,
      item5: e,
      item6: f,
      item7: g,
      item8: h,
      item9: i,
      item10: j,
      item11: k,
      item12: l,
      item13: m,
      item14: n,
      item15: o,
      item16: p,
      item17: q,
      item18: r,
      item19: s,
      item20: t,
      item21: u,
      item22: v,
      item23: w,
      item24: x,
      item25: y,
      item26: z,
      item27: a0,
      item28: b0,
      item29: c0,
      item30: d0,
      item31: e0,
      item32: f0
  )

<a id="type-unit"></a>

### `Unit`

```bosatsu
type Unit
```

#### Constructors

- `Unit`

## Values

<a id="value-add"></a>

### `add`

Integer addition.

references: [`Int`](#type-int)

```bosatsu
def add(a: Int, b: Int) -> Int
```

<a id="value-add-key"></a>

### `add_key`

Insert or replace a key/value pair.

references: [`Dict`](#type-dict)

```bosatsu
def add_key[a, b](dict: Dict[a, b], key: a, value: b) -> Dict[a, b]
```

<a id="value-addf"></a>

### `addf`

Floating-point addition.

references: [`Float64`](#type-float64)

```bosatsu
def addf(a: Float64, b: Float64) -> Float64
```

<a id="value-and"></a>

### `and`

Boolean conjunction.

references: [`Bool`](#type-bool)

```bosatsu
def and(x: Bool, y: Bool) -> Bool
```

<a id="value-and-int"></a>

### `and_Int`

Bitwise AND.

references: [`Int`](#type-int)

```bosatsu
def and_Int(a: Int, b: Int) -> Int
```

<a id="value-build-list"></a>

### `build_List`

Build a list from a right-fold style builder.

references: [`List`](#type-list)

```bosatsu
def build_List[a](fn: forall b: *. ((a, b) -> b, b) -> b) -> List[a]
```

<a id="value-char-list-to-string"></a>

### `char_List_to_String`

Concatenate a list of characters into a string.

references: [`Char`](#type-char), [`List`](#type-list), [`String`](#type-string)

```bosatsu
def char_List_to_String(chars: List[Char]) -> String
```

<a id="value-char-to-int"></a>

### `char_to_Int`

Unicode code point value for a character.

references: [`Char`](#type-char), [`Int`](#type-int)

```bosatsu
def char_to_Int(c: Char) -> Int
```

<a id="value-char-to-string"></a>

### `char_to_String`

Convert a character to a one-character string.

references: [`Char`](#type-char), [`String`](#type-string)

```bosatsu
def char_to_String(c: Char) -> String
```

<a id="value-clear-dict"></a>

### `clear_Dict`

Remove all entries while preserving key ordering.

references: [`Dict`](#type-dict)

```bosatsu
def clear_Dict[a, b](dict: Dict[a, b]) -> Dict[a, b]
```

<a id="value-cmp-bool"></a>

### `cmp_Bool`

references: [`Bool`](#type-bool), [`Comparison`](#type-comparison)

```bosatsu
def cmp_Bool(left: Bool, right: Bool) -> Comparison
```

<a id="value-cmp-char"></a>

### `cmp_Char`

Total character comparison in Unicode code-point order.

references: [`Char`](#type-char), [`Comparison`](#type-comparison)

```bosatsu
def cmp_Char(c0: Char, c1: Char) -> Comparison
```

<a id="value-cmp-comparison"></a>

### `cmp_Comparison`

references: [`Comparison`](#type-comparison)

```bosatsu
def cmp_Comparison(left: Comparison, right: Comparison) -> Comparison
```

<a id="value-cmp-float64"></a>

### `cmp_Float64`

Total Float64 comparison where all `.NaN` values are equal and ordered before non-`.NaN` values.

references: [`Comparison`](#type-comparison), [`Float64`](#type-float64)

```bosatsu
def cmp_Float64(a: Float64, b: Float64) -> Comparison
```

<a id="value-cmp-int"></a>

### `cmp_Int`

Total integer comparison.

references: [`Comparison`](#type-comparison), [`Int`](#type-int)

```bosatsu
def cmp_Int(a: Int, b: Int) -> Comparison
```

<a id="value-cmp-string"></a>

### `cmp_String`

Total string comparison in Unicode code-point order.

references: [`Comparison`](#type-comparison), [`String`](#type-string)

```bosatsu
def cmp_String(str0: String, str1: String) -> Comparison
```

<a id="value-concat"></a>

### `concat`

Append `front` and `back`.

references: [`List`](#type-list)

```bosatsu
def concat[a](front: List[a], back: List[a]) -> List[a]
```

<a id="value-concat-string"></a>

### `concat_String`

Concatenate a list of strings.

references: [`List`](#type-list), [`String`](#type-string)

```bosatsu
def concat_String(items: List[String]) -> String
```

<a id="value-div"></a>

### `div`

Integer division rounded toward negative infinity; total with `div(a, 0) == 0`.

references: [`Int`](#type-int)

```bosatsu
def div(a: Int, b: Int) -> Int
```

<a id="value-divf"></a>

### `divf`

IEEE754 floating-point division; unlike `div`, dividing by `0.0` yields `∞`, `-∞`, or `.NaN`.

references: [`Float64`](#type-float64)

```bosatsu
def divf(a: Float64, b: Float64) -> Float64
```

<a id="value-empty-dict"></a>

### `empty_Dict`

Create an empty dictionary using `comp` for key ordering.

references: [`Dict`](#type-dict), [`Order`](#type-order)

```bosatsu
def empty_Dict[a](comp: Order[a]) -> forall b: *. Dict[a, b]
```

<a id="value-eq-bool"></a>

### `eq_Bool`

references: [`Bool`](#type-bool)

```bosatsu
def eq_Bool(left: Bool, right: Bool) -> Bool
```

<a id="value-eq-char"></a>

### `eq_Char`

Character equality.

references: [`Bool`](#type-bool), [`Char`](#type-char)

```bosatsu
def eq_Char(c0: Char, c1: Char) -> Bool
```

<a id="value-eq-comparison"></a>

### `eq_Comparison`

references: [`Bool`](#type-bool), [`Comparison`](#type-comparison)

```bosatsu
def eq_Comparison(left: Comparison, right: Comparison) -> Bool
```

<a id="value-eq-float64"></a>

### `eq_Float64`

references: [`Bool`](#type-bool), [`Float64`](#type-float64)

```bosatsu
def eq_Float64(a: Float64, b: Float64) -> Bool
```

<a id="value-eq-int"></a>

### `eq_Int`

Integer equality.

references: [`Bool`](#type-bool), [`Int`](#type-int)

```bosatsu
def eq_Int(a: Int, b: Int) -> Bool
```

<a id="value-eq-string"></a>

### `eq_String`

references: [`Bool`](#type-bool), [`String`](#type-string)

```bosatsu
def eq_String(str0: String, str1: String) -> Bool
```

<a id="value-flat-map-list"></a>

### `flat_map_List`

Map each item to a list and flatten the result.

references: [`List`](#type-list)

```bosatsu
def flat_map_List[a, b](lst: List[a], fn: a -> List[b]) -> List[b]
```

<a id="value-foldl-list"></a>

### `foldl_List`

Left fold over a list.
Supports the deforestation rewrite:
foldr_List(build_List(g), f, x) => g(f, x)
See "A Shortcut to Deforestation" by Gill et. al.

references: [`List`](#type-list)

```bosatsu
def foldl_List[a, b](lst: List[a], item: b, fn: (b, a) -> b) -> b
```

<a id="value-foldr-list"></a>

### `foldr_List`

Fold a list from right to left.

references: [`List`](#type-list)

```bosatsu
def foldr_List[a, b](list: List[a], fn: (a, b) -> b, acc: b) -> b
```

<a id="value-gcd-int"></a>

### `gcd_Int`

Greatest common divisor for two integers.

references: [`Int`](#type-int)

```bosatsu
def gcd_Int(a: Int, b: Int) -> Int
```

<a id="value-get-key"></a>

### `get_key`

Look up the value associated with `key`.

references: [`Dict`](#type-dict), [`Option`](#type-option)

```bosatsu
def get_key[a, b](dict: Dict[a, b], key: a) -> Option[b]
```

<a id="value-implies"></a>

### `implies`

Boolean implication.

references: [`Bool`](#type-bool)

```bosatsu
def implies(x: Bool, y: Bool) -> Bool
```

<a id="value-int-to-char"></a>

### `int_to_Char`

Convert a Unicode code point to a character.

references: [`Char`](#type-char), [`Int`](#type-int), [`Option`](#type-option)

```bosatsu
def int_to_Char(code_point: Int) -> Option[Char]
```

<a id="value-int-to-string"></a>

### `int_to_String`

Convert an integer to its base-10 string form.

references: [`Int`](#type-int), [`String`](#type-string)

```bosatsu
def int_to_String(i: Int) -> String
```

<a id="value-items"></a>

### `items`

Return dictionary items in key order.

references: [`Dict`](#type-dict), [`List`](#type-list), [`Tuple2`](#type-tuple2)

```bosatsu
def items[a, b](dict: Dict[a, b]) -> List[(a, b)]
```

<a id="value-length-list"></a>

### `length_List`

Count the number of items in a list.

references: [`Int`](#type-int), [`List`](#type-list)

```bosatsu
def length_List[a](lst: List[a]) -> Int
```

<a id="value-length-string"></a>

### `length_String`

Count Unicode code points in a string.

references: [`Int`](#type-int), [`String`](#type-string)

```bosatsu
def length_String(s: String) -> Int
```

<a id="value-map-list"></a>

### `map_List`

Map each item in `lst` with `fn`.

references: [`List`](#type-list)

```bosatsu
def map_List[a, b](lst: List[a], fn: a -> b) -> List[b]
```

<a id="value-mod-int"></a>

### `mod_Int`

Integer modulus paired with `div`; total with `mod_Int(a, 0) == a`.

references: [`Int`](#type-int)

```bosatsu
def mod_Int(a: Int, mod: Int) -> Int
```

<a id="value-mul"></a>

### `mul`

Integer multiplication.

references: [`Int`](#type-int)

```bosatsu
def mul(a: Int, b: Int) -> Int
```

<a id="value-mulf"></a>

### `mulf`

Floating-point multiplication.

references: [`Float64`](#type-float64)

```bosatsu
def mulf(a: Float64, b: Float64) -> Float64
```

<a id="value-not"></a>

### `not`

Boolean negation.

references: [`Bool`](#type-bool)

```bosatsu
def not(x: Bool) -> Bool
```

<a id="value-not-int"></a>

### `not_Int`

Bitwise NOT.

references: [`Int`](#type-int)

```bosatsu
def not_Int(a: Int) -> Int
```

<a id="value-or"></a>

### `or`

Boolean disjunction.

references: [`Bool`](#type-bool)

```bosatsu
def or(x: Bool, y: Bool) -> Bool
```

<a id="value-or-int"></a>

### `or_Int`

Bitwise OR.

references: [`Int`](#type-int)

```bosatsu
def or_Int(a: Int, b: Int) -> Int
```

<a id="value-partition-string"></a>

### `partition_String`

if this returns Some((a, b)) then arg == concat_String([a, sep, b])
a and b are always proper substrings, so partition_String(a, "") == None
this matches from the left, so partition_String(a, sep) == None

references: [`Option`](#type-option), [`String`](#type-string), [`Tuple2`](#type-tuple2)

```bosatsu
def partition_String(arg: String, sep: String) -> Option[(String, String)]
```

<a id="value-popcount-int"></a>

### `popcount_Int`

Number of bits that differ from the sign bit.

references: [`Int`](#type-int)

```bosatsu
def popcount_Int(a: Int) -> Int
```

<a id="value-range"></a>

### `range`

Build `[0, 1, ..., exclusiveUpper - 1]`.

references: [`Int`](#type-int), [`List`](#type-list)

```bosatsu
def range(exclusiveUpper: Int) -> List[Int]
```

<a id="value-remove-key"></a>

### `remove_key`

Remove a key from the dictionary (no-op if missing).

references: [`Dict`](#type-dict)

```bosatsu
def remove_key[a, b](dict: Dict[a, b], key: a) -> Dict[a, b]
```

<a id="value-replicate-list"></a>

### `replicate_List`

Repeat `item` `cnt` times.

references: [`Int`](#type-int), [`List`](#type-list)

```bosatsu
def replicate_List[a](item: a, cnt: Int) -> List[a]
```

<a id="value-reverse"></a>

### `reverse`

Reverse a list.

references: [`List`](#type-list)

```bosatsu
def reverse[a](as: List[a]) -> List[a]
```

<a id="value-reverse-concat"></a>

### `reverse_concat`

Reverse `front` and prepend it to `back`.

references: [`List`](#type-list)

```bosatsu
def reverse_concat[a](front: List[a], back: List[a]) -> List[a]
```

<a id="value-rpartition-string"></a>

### `rpartition_String`

if this returns Some((a, b)) then arg == concat_String([a, sep, b])
a and b are always proper substrings, so rpartition_String(a, "") == None
this matches from the right, so partition_String(b, sep) == None

references: [`Option`](#type-option), [`String`](#type-string), [`Tuple2`](#type-tuple2)

```bosatsu
def rpartition_String(arg: String, sep: String) -> Option[(String, String)]
```

<a id="value-shift-left-int"></a>

### `shift_left_Int`

Bitwise left shift.

references: [`Int`](#type-int)

```bosatsu
def shift_left_Int(arg: Int, shift: Int) -> Int
```

<a id="value-shift-right-int"></a>

### `shift_right_Int`

Bitwise right shift.

references: [`Int`](#type-int)

```bosatsu
def shift_right_Int(arg: Int, shift: Int) -> Int
```

<a id="value-string-order"></a>

### `string_Order`

Ordering instance for strings.

references: [`Order`](#type-order), [`String`](#type-string)

```bosatsu
string_Order: Order[String]
```

<a id="value-string-to-int"></a>

### `string_to_Int`

Parse a base-10 integer from a string.

references: [`Int`](#type-int), [`Option`](#type-option), [`String`](#type-string)

```bosatsu
def string_to_Int(s: String) -> Option[Int]
```

<a id="value-sub"></a>

### `sub`

Integer subtraction.

references: [`Int`](#type-int)

```bosatsu
def sub(a: Int, b: Int) -> Int
```

<a id="value-subf"></a>

### `subf`

Floating-point subtraction.

references: [`Float64`](#type-float64)

```bosatsu
def subf(a: Float64, b: Float64) -> Float64
```

<a id="value-tail-or-empty-string"></a>

### `tail_or_empty_String`

if arg is empty this returns "", otherwise it drops exactly one codepoint
(equivalent to the tail from uncons_String, but without constructing Option/Tuple)

references: [`String`](#type-string)

```bosatsu
def tail_or_empty_String(arg: String) -> String
```

<a id="value-trace"></a>

### `trace`

Emit a debug trace prefixed with `prefix`, then return `item`.

references: [`String`](#type-string)

```bosatsu
def trace[a](prefix: String, item: a) -> a
```

<a id="value-uncons-string"></a>

### `uncons_String`

if this returns Some((h, t)) then arg == concat_String([char_to_String(h), t])
h is exactly one codepoint and t is the remaining suffix

references: [`Char`](#type-char), [`Option`](#type-option), [`String`](#type-string), [`Tuple2`](#type-tuple2)

```bosatsu
def uncons_String(arg: String) -> Option[(Char, String)]
```

<a id="value-xor"></a>

### `xor`

Boolean exclusive or.

references: [`Bool`](#type-bool)

```bosatsu
def xor(x: Bool, y: Bool) -> Bool
```

<a id="value-xor-int"></a>

### `xor_Int`

Bitwise XOR.

references: [`Int`](#type-int)

```bosatsu
def xor_Int(a: Int, b: Int) -> Int
```