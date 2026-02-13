# `Eval`

## Types

### `Eval[a]`

port of cats.Eval to bosatsu

```bosatsu
type Eval[a: +*]
```

## Values

### `bind`

```bosatsu
def bind[a, b](arg1: Eval[a], arg2: a -> Eval[b]) -> Eval[b]
```

### `done`

```bosatsu
def done[a](arg1: a) -> Eval[a]
```

### `eval`

```bosatsu
def eval[a](arg1: Int, arg2: Eval[a]) -> Option[a]
```

### `flat_map`

```bosatsu
def flat_map[a, b](arg1: Eval[a], arg2: a -> Eval[b]) -> Eval[b]
```

### `map`

```bosatsu
def map[a, b](arg1: Eval[a], arg2: a -> b) -> Eval[b]
```