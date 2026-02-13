# `Bosatsu/Example/ApplicativeTraverse`

## Types

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

### `Traverse[g]`

Represents the Traverse typeclass

```bosatsu
type Traverse[g: * -> *]
```

#### Constructors

- `Traverse(traverse: forall a: *, b: *, f: * -> *. (Applicative[f], a -> f[b], g[a]) -> f[g[b]])`

## Values

### `applicative_Option`

```bosatsu
applicative_Option: Applicative[Option]
```

### `applicative_from_pure_ap`

Build an applicative from pure, ap

```bosatsu
def applicative_from_pure_ap[a](arg1: forall b: *. b -> a[b], arg2: forall b: *, c: *. (a[b -> c], a[b]) -> a[c]) -> Applicative[a]
```

### `applicative_from_pure_map_product`

Build an applicative from pure, map, and product

```bosatsu
def applicative_from_pure_map_product[a](
    arg1: forall b: *. b -> a[b],
    arg2: forall b: *, c: *. (b -> c, a[b]) -> a[c],
    arg3: forall b: *, c: *. (a[b], a[c]) -> a[(b, c)]
) -> Applicative[a]
```

### `traverse_List`

here is the traverse instance for List

```bosatsu
traverse_List: Traverse[List]
```