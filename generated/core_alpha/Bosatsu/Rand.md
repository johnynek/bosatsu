---
github.base_url=
---

# `Bosatsu/Rand`

source code:
- [`test_workspace/Rand.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Rand.bosatsu)

public dependencies: [`Bosatsu/Num/Nat`](Num/Nat.html)

## Index

- Types: [`Rand`](#type-rand)
- Values: [`bool_Rand`](#value-bool-rand), [`const_Rand`](#value-const-rand),
[`flat_map_Rand`](#value-flat-map-rand), [`from_pair`](#value-from-pair),
[`geometric_Int`](#value-geometric-int), [`int_range`](#value-int-range),
[`map_Rand`](#value-map-rand), [`nat_range`](#value-nat-range), [`one_of`](#value-one-of),
[`prod_Rand`](#value-prod-rand), [`run_Rand`](#value-run-rand),
[`sequence_Rand`](#value-sequence-rand)

## Types

<a id="type-rand"></a>

### `Rand[a]`

```bosatsu
type Rand[a: +*]
```

## Values

<a id="value-bool-rand"></a>

### `bool_Rand`

references: [`Bool`](Predef.html#type-bool), [`Rand`](#type-rand)

```bosatsu
bool_Rand: Rand[Bool]
```

<a id="value-const-rand"></a>

### `const_Rand`

references: [`Rand`](#type-rand)

```bosatsu
def const_Rand[a](a: a) -> Rand[a]
```

<a id="value-flat-map-rand"></a>

### `flat_map_Rand`

references: [`Rand`](#type-rand)

```bosatsu
def flat_map_Rand[a, b](r: Rand[a], fn: a -> Rand[b]) -> Rand[b]
```

<a id="value-from-pair"></a>

### `from_pair`

references: [`Rand`](#type-rand)

```bosatsu
def from_pair[a](left: Rand[a], right: Rand[a]) -> Rand[a]
```

<a id="value-geometric-int"></a>

### `geometric_Int`

geometric distribution with mean 1

references: [`Int`](Predef.html#type-int), [`Rand`](#type-rand)

```bosatsu
geometric_Int: Rand[Int]
```

<a id="value-int-range"></a>

### `int_range`

if you pass high <= 0, you get const_Rand(0)

references: [`Int`](Predef.html#type-int), [`Rand`](#type-rand)

```bosatsu
def int_range(high: Int) -> Rand[Int]
```

<a id="value-map-rand"></a>

### `map_Rand`

references: [`Rand`](#type-rand)

```bosatsu
def map_Rand[a, b](r: Rand[a], fn: a -> b) -> Rand[b]
```

<a id="value-nat-range"></a>

### `nat_range`

references: [`Bosatsu/Num/Nat::Nat`](Num/Nat.html#type-nat), [`Rand`](#type-rand)

```bosatsu
def nat_range(high: Bosatsu/Num/Nat::Nat) -> Rand[Bosatsu/Num/Nat::Nat]
```

<a id="value-one-of"></a>

### `one_of`

references: [`List`](Predef.html#type-list), [`Rand`](#type-rand)

```bosatsu
def one_of[a](head: Rand[a], tail: List[Rand[a]]) -> Rand[a]
```

<a id="value-prod-rand"></a>

### `prod_Rand`

references: [`Rand`](#type-rand), [`Tuple2`](Predef.html#type-tuple2)

```bosatsu
def prod_Rand[a, b](ra: Rand[a], rb: Rand[b]) -> Rand[(a, b)]
```

<a id="value-run-rand"></a>

### `run_Rand`

references: [`Int`](Predef.html#type-int), [`Rand`](#type-rand)

```bosatsu
def run_Rand[a](rand: Rand[a], seed: Int) -> a
```

<a id="value-sequence-rand"></a>

### `sequence_Rand`

references: [`List`](Predef.html#type-list), [`Rand`](#type-rand)

```bosatsu
def sequence_Rand[a](rands: List[Rand[a]]) -> Rand[List[a]]
```