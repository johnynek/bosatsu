# `TypeConstraint`

## Types

### `Eq[a, b]`

```bosatsu
type Eq[a: *, b: *]
```

### `Sub[a, b]`

```bosatsu
type Sub[a: -*, b: +*]
```

### `Sup[a, b]`

```bosatsu
type Sup[a: +*, b: -*]
```

## Values

### `cast`

```bosatsu
def cast[a, b](arg1: Eq[a, b], arg2: a) -> b
```

### `compose_eq`

```bosatsu
def compose_eq[a, b, c](arg1: Eq[a, b], arg2: Eq[b, c]) -> Eq[a, c]
```

### `compose_sub`

```bosatsu
def compose_sub[a, b, c](arg1: Sub[a, b], arg2: Sub[b, c]) -> Sub[a, c]
```

### `compose_sup`

```bosatsu
def compose_sup[a, b, c](arg1: Sup[a, b], arg2: Sup[b, c]) -> Sup[a, c]
```

### `downcast`

```bosatsu
def downcast[a, b](arg1: Sup[a, b], arg2: b) -> a
```

### `eq_to_sub`

```bosatsu
def eq_to_sub[a, b](arg1: Eq[a, b]) -> Sub[a, b]
```

### `eq_to_sup`

```bosatsu
def eq_to_sup[a, b](arg1: Eq[a, b]) -> Sup[a, b]
```

### `flip_eq`

```bosatsu
def flip_eq[a, b](arg1: Eq[a, b]) -> Eq[b, a]
```

### `narrow`

```bosatsu
def narrow[a, b, c](arg1: Sup[a, b], arg2: c[a]) -> c[b]
```

### `refl`

```bosatsu
refl: forall a: *. Eq[a, a]
```

### `refl_sub`

```bosatsu
refl_sub: forall a: *. Sub[a, a]
```

### `refl_sup`

```bosatsu
refl_sup: forall a: *. Sup[a, a]
```

### `sub_to_sup`

```bosatsu
def sub_to_sup[a, b](arg1: Sub[a, b]) -> Sup[b, a]
```

### `substitute`

```bosatsu
def substitute[a, b, c](arg1: Eq[a, b], arg2: c[a]) -> c[b]
```

### `sup_to_sub`

```bosatsu
def sup_to_sub[a, b](arg1: Sup[a, b]) -> Sub[b, a]
```

### `upcast`

```bosatsu
def upcast[a, b](arg1: Sub[a, b], arg2: a) -> b
```

### `widen`

```bosatsu
def widen[a, b, c](arg1: Sub[a, b], arg2: c[a]) -> c[b]
```