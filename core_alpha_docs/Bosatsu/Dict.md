# `Bosatsu/Dict`

private package

source code:
- [`test_workspace/Dict.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Dict.bosatsu)

## Index

- Values: [`eq_Dict`](#value-eq-dict), [`eq_Pair`](#value-eq-pair)

## Values

<a id="value-eq-dict"></a>

### `eq_Dict`

references: [`Bool`](Predef.md#type-bool), [`Dict`](Predef.md#type-dict)

```bosatsu
def eq_Dict[a,
    b](eq_key: (a, a) -> Bool, eq_value: (b, b) -> Bool) -> (Dict[a, b], Dict[a, b]) -> Bool
```

<a id="value-eq-pair"></a>

### `eq_Pair`

references: [`Bool`](Predef.md#type-bool), [`Tuple2`](Predef.md#type-tuple2)

```bosatsu
def eq_Pair[a, b, c, d](eq_a: (a, b) -> Bool, eq_b: (c, d) -> Bool) -> ((a, c), (b, d)) -> Bool
```