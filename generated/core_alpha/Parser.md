---
github.base_url=
---

# `Parser`

private package

source code:
- [`test_workspace/Parser.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Parser.bosatsu)

## Index

- Types: [`Parser`](#type-parser)
- Values: [`empty`](#value-empty), [`expect`](#value-expect), [`flat_map`](#value-flat-map),
[`map`](#value-map), [`one_of`](#value-one-of), [`parse`](#value-parse)

## Types

<a id="type-parser"></a>

### `Parser[a]`

```bosatsu
type Parser[a: +*]
```

## Values

<a id="value-empty"></a>

### `empty`

references: [`Parser`](#type-parser)

```bosatsu
empty: forall a: *. Parser[a]
```

<a id="value-expect"></a>

### `expect`

references: [`Parser`](#type-parser), [`String`](Bosatsu/Predef.html#type-string), [`Unit`](Bosatsu/Predef.html#type-unit)

```bosatsu
def expect(str: String) -> Parser[()]
```

<a id="value-flat-map"></a>

### `flat_map`

references: [`Parser`](#type-parser)

```bosatsu
def flat_map[a, b](p: Parser[a], fn: a -> Parser[b]) -> Parser[b]
```

<a id="value-map"></a>

### `map`

references: [`Parser`](#type-parser)

```bosatsu
def map[a, b](p: Parser[a], fn: a -> b) -> Parser[b]
```

<a id="value-one-of"></a>

### `one_of`

references: [`List`](Bosatsu/Predef.html#type-list), [`Parser`](#type-parser)

```bosatsu
def one_of[a](ps: List[Parser[a]]) -> Parser[a]
```

<a id="value-parse"></a>

### `parse`

references: [`Option`](Bosatsu/Predef.html#type-option), [`Parser`](#type-parser), [`String`](Bosatsu/Predef.html#type-string)

```bosatsu
def parse[a](p: Parser[a], str: String) -> Option[a]
```