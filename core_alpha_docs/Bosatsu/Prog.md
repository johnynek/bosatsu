# `Bosatsu/Prog`

source code:
- [`test_workspace/Prog.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Prog.bosatsu)

## Index

- Types: [`Main`](#type-main), [`Prog`](#type-prog), [`ProgTest`](#type-progtest),
[`Var`](#type-var)
- Values: [`await`](#value-await), [`get`](#value-get), [`get_and_update`](#value-get-and-update),
[`ignore_err`](#value-ignore-err), [`map`](#value-map), [`map_err`](#value-map-err),
[`modify`](#value-modify), [`new_var`](#value-new-var), [`observe`](#value-observe),
[`pure`](#value-pure), [`raise_error`](#value-raise-error), [`recover`](#value-recover),
[`recursive`](#value-recursive), [`set`](#value-set), [`swap`](#value-swap), [`unit`](#value-unit),
[`update`](#value-update), [`update_and_get`](#value-update-and-get)

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

<a id="type-var"></a>

### `Var[a]`

Var is an effectful mutable cell. The type parameter is invariant.

```bosatsu
type Var[a: 👻*]
```

## Values

<a id="value-await"></a>

### `await`

references: [`Prog`](#type-prog)

```bosatsu
def await[a, b, c](p: Prog[a, b], fn: b -> Prog[a, c]) -> Prog[a, c]
```

<a id="value-get"></a>

### `get`

Read the current value in the cell.

references: [`Prog`](#type-prog), [`Var`](#type-var)

```bosatsu
def get[a](v: Var[a]) -> forall e: *. Prog[e, a]
```

<a id="value-get-and-update"></a>

### `get_and_update`

Update a cell and return the previous value.

references: [`Prog`](#type-prog), [`Var`](#type-var)

```bosatsu
def get_and_update[a](v: Var[a], fn: a -> a) -> forall b: *. Prog[b, a]
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

<a id="value-modify"></a>

### `modify`

Update a cell and discard the projected result.

references: [`Prog`](#type-prog), [`Unit`](Predef.md#type-unit), [`Var`](#type-var)

```bosatsu
def modify[a](v: Var[a], fn: a -> a) -> forall a: *. Prog[a, ()]
```

<a id="value-new-var"></a>

### `new_var`

Allocate a fresh cell when the Prog is executed.

references: [`Prog`](#type-prog), [`Var`](#type-var)

```bosatsu
def new_var[a](a: a) -> forall e: *. Prog[e, Var[a]]
```

<a id="value-observe"></a>

### `observe`

references: [`Prog`](#type-prog), [`Unit`](Predef.md#type-unit)

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

<a id="value-set"></a>

### `set`

Store a new value in the cell and return ().

references: [`Prog`](#type-prog), [`Unit`](Predef.md#type-unit), [`Var`](#type-var)

```bosatsu
def set[a](v: Var[a], value: a) -> forall e: *. Prog[e, ()]
```

<a id="value-swap"></a>

### `swap`

Store a new value in the cell and return the previous value.

references: [`Prog`](#type-prog), [`Var`](#type-var)

```bosatsu
def swap[a](v: Var[a], new_value: a) -> forall e: *. Prog[e, a]
```

<a id="value-unit"></a>

### `unit`

references: [`Prog`](#type-prog), [`Unit`](Predef.md#type-unit)

```bosatsu
unit: forall a: *. Prog[a, ()]
```

<a id="value-update"></a>

### `update`

Atomically transform the current value. On some runtimes fn may be retried.

references: [`Prog`](#type-prog), [`Tuple2`](Predef.md#type-tuple2), [`Var`](#type-var)

```bosatsu
def update[a, b](v: Var[a], fn: a -> (a, b)) -> forall e: *. Prog[e, b]
```

<a id="value-update-and-get"></a>

### `update_and_get`

Update a cell and return the new value.

references: [`Prog`](#type-prog), [`Var`](#type-var)

```bosatsu
def update_and_get[a](v: Var[a], fn: a -> a) -> forall b: *. Prog[b, a]
```