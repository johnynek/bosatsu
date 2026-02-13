# `Parser`

## Types

### `Parser[a]`

```bosatsu
type Parser[a: +*]
```

## Values

### `empty`

```bosatsu
empty: forall a: *. Parser[a]
```

### `expect`

```bosatsu
def expect(arg1: String) -> Parser[()]
```

### `flat_map`

```bosatsu
def flat_map[a, b](arg1: Parser[a], arg2: a -> Parser[b]) -> Parser[b]
```

### `map`

```bosatsu
def map[a, b](arg1: Parser[a], arg2: a -> b) -> Parser[b]
```

### `one_of`

```bosatsu
def one_of[a](arg1: List[Parser[a]]) -> Parser[a]
```

### `parse`

```bosatsu
def parse[a](arg1: Parser[a], arg2: String) -> Option[a]
```