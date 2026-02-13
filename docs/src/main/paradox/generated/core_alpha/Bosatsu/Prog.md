# `Bosatsu/Prog`

## Types

### `Main`

```bosatsu
type Main
```

#### Constructors

- `Main(prog: forall err: *. Prog[List[String], err, Int])`

### `Prog[env, err, res]`

```bosatsu
type Prog[env: -*, err: +*, res: +*]
```

## Values

### `await`

```bosatsu
def await[a, b, c, d](arg1: Prog[a, b, c], arg2: c -> Prog[a, b, d]) -> Prog[a, b, d]
```

### `map`

```bosatsu
def map[a, b, c, d](arg1: Prog[a, b, c], arg2: c -> d) -> Prog[a, b, d]
```

### `map_err`

```bosatsu
def map_err[a, b, c, d](arg1: Prog[a, b, c], arg2: b -> d) -> Prog[a, d, c]
```

### `pure`

```bosatsu
def pure[env, err, res](arg1: res) -> Prog[env, err, res]
```

### `raise_error`

```bosatsu
def raise_error[env, err, res](arg1: err) -> Prog[env, err, res]
```

### `read_env`

```bosatsu
read_env: forall a: *, err: *. Prog[a, err, a]
```

### `recover`

```bosatsu
def recover[env, err, res, err1](arg1: Prog[env, err, res], arg2: err -> Prog[env, err1, res]) -> Prog[env, err1, res]
```

### `recursive`

```bosatsu
def recursive[a, b, c, d](arg1: (a -> Prog[b, c, d]) -> a -> Prog[b, c, d]) -> a -> Prog[b, c, d]
```

### `remap_env`

```bosatsu
def remap_env[env, err, res, env1](arg1: Prog[env, err, res], arg2: env1 -> env) -> Prog[env1, err, res]
```

### `unit`

```bosatsu
unit: forall a: *, b: *. Prog[a, b, ()]
```

### `with_env`

```bosatsu
def with_env[a, b, c, d](arg1: Prog[a, b, c], arg2: a) -> Prog[d, b, c]
```