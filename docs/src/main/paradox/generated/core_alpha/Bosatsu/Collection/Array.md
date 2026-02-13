# `Bosatsu/Collection/Array`

## Types

### `Array[a]`

```bosatsu
type Array[a: +*]
```

## Values

### `char_Array_to_Int_Array`

```bosatsu
def char_Array_to_Int_Array(arg1: Array[Char]) -> Array[Int]
```

### `char_Array_to_String`

```bosatsu
def char_Array_to_String(arg1: Array[Char]) -> String
```

### `concat_all_Array`

```bosatsu
def concat_all_Array[a](arg1: List[Array[a]]) -> Array[a]
```

### `empty_Array`

```bosatsu
empty_Array: forall a: *. Array[a]
```

### `flatten_Array`

```bosatsu
def flatten_Array[a](arg1: Array[Array[a]]) -> Array[a]
```

### `foldl_Array`

```bosatsu
def foldl_Array[a, b](arg1: Array[a], arg2: b, arg3: (b, a) -> b) -> b
```

### `from_List_Array`

```bosatsu
def from_List_Array[a](arg1: List[a]) -> Array[a]
```

### `get_Array`

```bosatsu
def get_Array[a](arg1: Array[a], arg2: Int) -> Option[a]
```

### `get_map_Array`

```bosatsu
def get_map_Array[a, b](arg1: Array[a], arg2: Int, arg3: (()) -> b, arg4: a -> b) -> b
```

### `get_or_Array`

```bosatsu
def get_or_Array[a](arg1: Array[a], arg2: Int, arg3: (()) -> a) -> a
```

### `index_in_range_Array`

```bosatsu
def index_in_range_Array[a](arg1: Array[a], arg2: Int) -> Bool
```

### `int_Array_to_Char_Array`

```bosatsu
def int_Array_to_Char_Array(arg1: Array[Int]) -> Option[Array[Char]]
```

### `int_Array_to_String`

```bosatsu
def int_Array_to_String(arg1: Array[Int]) -> Option[String]
```

### `map_Array`

```bosatsu
def map_Array[a, b](arg1: Array[a], arg2: a -> b) -> Array[b]
```

### `range_Array`

```bosatsu
def range_Array(arg1: Int) -> Array[Int]
```

### `range_from_Array`

```bosatsu
def range_from_Array(arg1: Int, arg2: Int) -> Array[Int]
```

### `reverse_Array`

```bosatsu
def reverse_Array[a](arg1: Array[a]) -> Array[a]
```

### `set_Array`

```bosatsu
def set_Array[a](arg1: Array[a], arg2: Int, arg3: a) -> Option[Array[a]]
```

### `set_or_self_Array`

```bosatsu
def set_or_self_Array[a](arg1: Array[a], arg2: Int, arg3: a) -> Array[a]
```

### `size_Array`

```bosatsu
def size_Array[a](arg1: Array[a]) -> Int
```

### `slice_Array`

```bosatsu
def slice_Array[a](arg1: Array[a], arg2: Int, arg3: Int) -> Array[a]
```

### `sort_Array`

```bosatsu
def sort_Array[a](arg1: Array[a], arg2: (a, a) -> Comparison) -> Array[a]
```

### `string_to_Char_Array`

```bosatsu
def string_to_Char_Array(arg1: String) -> Array[Char]
```

### `string_to_Int_Array`

```bosatsu
def string_to_Int_Array(arg1: String) -> Array[Int]
```

### `tabulate_Array`

```bosatsu
def tabulate_Array[a](arg1: Int, arg2: Int -> a) -> Array[a]
```

### `to_List_Array`

```bosatsu
def to_List_Array[a](arg1: Array[a]) -> List[a]
```