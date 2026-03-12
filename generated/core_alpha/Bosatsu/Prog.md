---
github.base_url=
---

# `Bosatsu/Prog`

source code:
- [`test_workspace/Prog.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Prog.bosatsu)

## Index

- Types: [`Main`](#type-main), [`Prog`](#type-prog), [`ProgTest`](#type-progtest)
- Values: [`await`](#value-await), [`ignore_err`](#value-ignore-err), [`map`](#value-map),
[`map_err`](#value-map-err), [`observe`](#value-observe), [`pure`](#value-pure),
[`raise_error`](#value-raise-error), [`recover`](#value-recover), [`recursive`](#value-recursive),
[`unit`](#value-unit)

## Types

<a id="type-main"></a>

### `Main`

```bosatsu
type Main
```

#### Constructors

- `Main(run: List[String] -> forall err: *. Prog[err, Int])`

<a id="type-prog"></a>

### `Prog[err, res]`

```bosatsu
type Prog[err: +*, res: +*]
```

<a id="type-progtest"></a>

### `ProgTest`

```bosatsu
type ProgTest
```

#### Constructors

- `ProgTest(test_fn: List[String] -> forall err: *. Prog[err, Test])`

## Values

<a id="value-await"></a>

### `await`

references: [`Prog`](#type-prog)

```bosatsu
def await[a, b, c](p: Prog[a, b], fn: b -> Prog[a, c]) -> Prog[a, c]
```

<a id="value-ignore-err"></a>

### `ignore_err`

references: [`Prog`](#type-prog)

```bosatsu
def ignore_err[a, b](prog: Prog[a, b], default: b) -> forall a: *. Prog[a, b]
```

<a id="value-map"></a>

### `map`

references: [`Prog`](#type-prog)

```bosatsu
def map[a, b, c](prog: Prog[a, b], fn: b -> c) -> Prog[a, c]
```

<a id="value-map-err"></a>

### `map_err`

references: [`Prog`](#type-prog)

```bosatsu
def map_err[a, b, c](prog: Prog[a, b], fn: a -> c) -> Prog[c, b]
```

<a id="value-observe"></a>

### `observe`

references: [`Prog`](#type-prog), [`Unit`](Predef.html#type-unit)

```bosatsu
def observe[a](a: a) -> forall err: *. Prog[err, ()]
```

<a id="value-pure"></a>

### `pure`

references: [`Prog`](#type-prog)

```bosatsu
def pure[err, res](a: res) -> Prog[err, res]
```

<a id="value-raise-error"></a>

### `raise_error`

references: [`Prog`](#type-prog)

```bosatsu
def raise_error[err, res](e: err) -> Prog[err, res]
```

<a id="value-recover"></a>

### `recover`

references: [`Prog`](#type-prog)

```bosatsu
def recover[err, res, err1](prog: Prog[err, res], fn: err -> Prog[err1, res]) -> Prog[err1, res]
```

<a id="value-recursive"></a>

### `recursive`

references: [`Prog`](#type-prog)

```bosatsu
def recursive[a, b, c](fn: (a -> Prog[b, c]) -> a -> Prog[b, c]) -> a -> Prog[b, c]
```

<a id="value-unit"></a>

### `unit`

references: [`Prog`](#type-prog), [`Unit`](Predef.html#type-unit)

```bosatsu
unit: forall a: *. Prog[a, ()]
```