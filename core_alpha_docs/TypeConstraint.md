# `TypeConstraint`

private package

source code:
- [`test_workspace/TypeConstraint.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/TypeConstraint.bosatsu)

## Index

- Types: [`Eq`](#type-eq), [`Sub`](#type-sub), [`Sup`](#type-sup)
- Values: [`cast`](#value-cast), [`compose_eq`](#value-compose-eq),
[`compose_sub`](#value-compose-sub), [`compose_sup`](#value-compose-sup),
[`downcast`](#value-downcast), [`eq_to_sub`](#value-eq-to-sub), [`eq_to_sup`](#value-eq-to-sup),
[`flip_eq`](#value-flip-eq), [`narrow`](#value-narrow), [`refl`](#value-refl),
[`refl_sub`](#value-refl-sub), [`refl_sup`](#value-refl-sup), [`sub_to_sup`](#value-sub-to-sup),
[`substitute`](#value-substitute), [`sup_to_sub`](#value-sup-to-sub), [`upcast`](#value-upcast),
[`widen`](#value-widen)

## Types

<a id="type-eq"></a>

### `Eq[a, b]`

```bosatsu
type Eq[a: *, b: *]
```

<a id="type-sub"></a>

### `Sub[a, b]`

```bosatsu
type Sub[a: -*, b: +*]
```

<a id="type-sup"></a>

### `Sup[a, b]`

```bosatsu
type Sup[a: +*, b: -*]
```

## Values

<a id="value-cast"></a>

### `cast`

references: [`Eq`](#type-eq)

```bosatsu
def cast[a, b](s: Eq[a, b], a: a) -> b
```

<a id="value-compose-eq"></a>

### `compose_eq`

references: [`Eq`](#type-eq)

```bosatsu
def compose_eq[a, b, c](first: Eq[a, b], second: Eq[b, c]) -> Eq[a, c]
```

<a id="value-compose-sub"></a>

### `compose_sub`

references: [`Sub`](#type-sub)

```bosatsu
def compose_sub[a, b, c](first: Sub[a, b], second: Sub[b, c]) -> Sub[a, c]
```

<a id="value-compose-sup"></a>

### `compose_sup`

references: [`Sup`](#type-sup)

```bosatsu
def compose_sup[a, b, c](first: Sup[a, b], second: Sup[b, c]) -> Sup[a, c]
```

<a id="value-downcast"></a>

### `downcast`

references: [`Sup`](#type-sup)

```bosatsu
def downcast[a, b](s: Sup[a, b], a: b) -> a
```

<a id="value-eq-to-sub"></a>

### `eq_to_sub`

references: [`Eq`](#type-eq), [`Sub`](#type-sub)

```bosatsu
def eq_to_sub[a, b](eq: Eq[a, b]) -> Sub[a, b]
```

<a id="value-eq-to-sup"></a>

### `eq_to_sup`

references: [`Eq`](#type-eq), [`Sup`](#type-sup)

```bosatsu
def eq_to_sup[a, b](eq: Eq[a, b]) -> Sup[a, b]
```

<a id="value-flip-eq"></a>

### `flip_eq`

references: [`Eq`](#type-eq)

```bosatsu
def flip_eq[a, b](eq: Eq[a, b]) -> Eq[b, a]
```

<a id="value-narrow"></a>

### `narrow`

references: [`Sup`](#type-sup)

```bosatsu
def narrow[a, b, c: -* -> *](s: Sup[a, b], fa: c[a]) -> c[b]
```

<a id="value-refl"></a>

### `refl`

references: [`Eq`](#type-eq)

```bosatsu
refl: forall a: *. Eq[a, a]
```

<a id="value-refl-sub"></a>

### `refl_sub`

references: [`Sub`](#type-sub)

```bosatsu
refl_sub: forall a: *. Sub[a, a]
```

<a id="value-refl-sup"></a>

### `refl_sup`

references: [`Sup`](#type-sup)

```bosatsu
refl_sup: forall a: *. Sup[a, a]
```

<a id="value-sub-to-sup"></a>

### `sub_to_sup`

references: [`Sub`](#type-sub), [`Sup`](#type-sup)

```bosatsu
def sub_to_sup[a, b](sub: Sub[a, b]) -> Sup[b, a]
```

<a id="value-substitute"></a>

### `substitute`

references: [`Eq`](#type-eq)

```bosatsu
def substitute[a, b, c: * -> *](eq: Eq[a, b], fa: c[a]) -> c[b]
```

<a id="value-sup-to-sub"></a>

### `sup_to_sub`

references: [`Sub`](#type-sub), [`Sup`](#type-sup)

```bosatsu
def sup_to_sub[a, b](sup: Sup[a, b]) -> Sub[b, a]
```

<a id="value-upcast"></a>

### `upcast`

references: [`Sub`](#type-sub)

```bosatsu
def upcast[a, b](s: Sub[a, b], a: a) -> b
```

<a id="value-widen"></a>

### `widen`

references: [`Sub`](#type-sub)

```bosatsu
def widen[a, b, c: +* -> *](s: Sub[a, b], fa: c[a]) -> c[b]
```