# `Bosatsu/Char`

## Values

### `char_List_to_Int_List`

```bosatsu
def char_List_to_Int_List(arg1: List[Char]) -> List[Int]
```

### `char_to_Int`

```bosatsu
def char_to_Int(arg1: Char) -> Int
```

### `eq_ignore_ascii_case`

```bosatsu
def eq_ignore_ascii_case(arg1: Char, arg2: Char) -> Bool
```

### `from_digit`

Supported radix range is 2..36 inclusive.

```bosatsu
def from_digit(arg1: Int, arg2: Int) -> Option[Char]
```

### `int_List_to_Char_List`

```bosatsu
def int_List_to_Char_List(arg1: List[Int]) -> Option[List[Char]]
```

### `int_List_to_String`

```bosatsu
def int_List_to_String(arg1: List[Int]) -> Option[String]
```

### `int_to_Char`

```bosatsu
def int_to_Char(arg1: Int) -> Option[Char]
```

### `is_ascii`

```bosatsu
def is_ascii(arg1: Char) -> Bool
```

### `is_ascii_alnum`

```bosatsu
def is_ascii_alnum(arg1: Char) -> Bool
```

### `is_ascii_alpha`

```bosatsu
def is_ascii_alpha(arg1: Char) -> Bool
```

### `is_ascii_control`

```bosatsu
def is_ascii_control(arg1: Char) -> Bool
```

### `is_ascii_digit`

```bosatsu
def is_ascii_digit(arg1: Char) -> Bool
```

### `is_ascii_hex`

```bosatsu
def is_ascii_hex(arg1: Char) -> Bool
```

### `is_ascii_lower`

```bosatsu
def is_ascii_lower(arg1: Char) -> Bool
```

### `is_ascii_punctuation`

```bosatsu
def is_ascii_punctuation(arg1: Char) -> Bool
```

### `is_ascii_upper`

```bosatsu
def is_ascii_upper(arg1: Char) -> Bool
```

### `is_ascii_whitespace`

```bosatsu
def is_ascii_whitespace(arg1: Char) -> Bool
```

### `is_digit`

```bosatsu
def is_digit(arg1: Char) -> Bool
```

### `is_hex`

```bosatsu
def is_hex(arg1: Char) -> Bool
```

### `is_scalar_value`

Unicode scalar value = [0, 0x10FFFF] excluding UTF-16 surrogate code points.

```bosatsu
def is_scalar_value(arg1: Int) -> Bool
```

### `last_String`

```bosatsu
def last_String(arg1: String) -> Option[Char]
```

### `string_to_Char`

```bosatsu
def string_to_Char(arg1: String) -> Option[Char]
```

### `string_to_Char_List`

```bosatsu
def string_to_Char_List(arg1: String) -> List[Char]
```

### `string_to_Int_List`

```bosatsu
def string_to_Int_List(arg1: String) -> List[Int]
```

### `to_ascii_lower_case`

```bosatsu
def to_ascii_lower_case(arg1: Char) -> Char
```

### `to_ascii_upper_case`

```bosatsu
def to_ascii_upper_case(arg1: Char) -> Char
```

### `to_digit`

Supported radix range is 2..36 inclusive.

```bosatsu
def to_digit(arg1: Char, arg2: Int) -> Option[Int]
```

### `utf8_len`

```bosatsu
def utf8_len(arg1: Char) -> Int
```