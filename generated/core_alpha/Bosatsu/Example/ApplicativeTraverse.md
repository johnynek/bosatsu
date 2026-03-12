---
github.base_url=
---

# `Bosatsu/Example/ApplicativeTraverse`

private package

source code:
- [`test_workspace/ApplicativeTraverse.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/ApplicativeTraverse.bosatsu)

## Index

- Types: [`Applicative`](#type-applicative), [`Traverse`](#type-traverse)
- Values: [`applicative_Option`](#value-applicative-option),
[`applicative_from_pure_ap`](#value-applicative-from-pure-ap),
[`applicative_from_pure_map_product`](#value-applicative-from-pure-map-product),
[`traverse_List`](#value-traverse-list)

## Types

<a id="type-applicative"></a>

### `Applicative[f]`

Represents the Applicative typeclass

```bosatsu
type Applicative[f: * -> *]
```

#### Constructors

- Applicative(
      pure: forall a: *. a -> f[a],
      map: forall a: *, b: *. (a -> b, f[a]) -> f[b],
      ap: forall a: *, b: *. (f[a -> b], f[a]) -> f[b],
      map2: forall a: *, b: *, c: *. (f[a], f[b], (a, b) -> c) -> f[c],
      product: forall a: *, b: *. (f[a], f[b]) -> f[(a, b)]
  )

<a id="type-traverse"></a>

### `Traverse[g]`

Represents the Traverse typeclass

```bosatsu
type Traverse[g: * -> *]
```

#### Constructors

- `Traverse(traverse: forall a: *, b: *, f: * -> *. (Applicative[f], a -> f[b], g[a]) -> f[g[b]])`

## Values

<a id="value-applicative-option"></a>

### `applicative_Option`

references: [`Applicative`](#type-applicative), [`Option`](../Predef.html#type-option)

```bosatsu
applicative_Option: Applicative[Option]
```

<a id="value-applicative-from-pure-ap"></a>

### `applicative_from_pure_ap`

Build an applicative from pure, ap

references: [`Applicative`](#type-applicative)

```bosatsu
def applicative_from_pure_ap[a: * -> *
](pure: forall b: *. b -> a[b], ap: forall b: *, c: *. (a[b -> c], a[b]) -> a[c]) -> Applicative[a]
```

<a id="value-applicative-from-pure-map-product"></a>

### `applicative_from_pure_map_product`

Build an applicative from pure, map, and product

references: [`Applicative`](#type-applicative), [`Tuple2`](../Predef.html#type-tuple2)

```bosatsu
def applicative_from_pure_map_product[a: * -> *](
    pure: forall b: *. b -> a[b],
    map: forall b: *, c: *. (b -> c, a[b]) -> a[c],
    product: forall b: *, c: *. (a[b], a[c]) -> a[(b, c)]
) -> Applicative[a]
```

<a id="value-traverse-list"></a>

### `traverse_List`

here is the traverse instance for List

references: [`List`](../Predef.html#type-list), [`Traverse`](#type-traverse)

```bosatsu
traverse_List: Traverse[List]
```