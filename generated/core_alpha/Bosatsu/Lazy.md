---
github.base_url=
---

# `Bosatsu/Lazy`

source code:
- [`test_workspace/Bosatsu/Lazy.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Bosatsu/Lazy.bosatsu)

## Index

- Types: [`Lazy`](#type-lazy)
- Values: [`get_Lazy`](#value-get-lazy), [`lazy`](#value-lazy)

## Types

<a id="type-lazy"></a>

### `Lazy[a]`

```bosatsu
type Lazy[a: +*]
```

## Values

<a id="value-get-lazy"></a>

### `get_Lazy`

references: [`Lazy`](#type-lazy)

```bosatsu
def get_Lazy[a](l: Lazy[a]) -> a
```

<a id="value-lazy"></a>

### `lazy`

references: [`Lazy`](#type-lazy), [`Unit`](Predef.html#type-unit)

```bosatsu
def lazy[a](fn: () -> a) -> Lazy[a]
```