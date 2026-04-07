# `Bosatsu/Loops`

private package

source code:
- [`test_workspace/Loops.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Loops.bosatsu)

## Index

- Values: [`range_foldr`](#value-range-foldr)

## Values

<a id="value-range-foldr"></a>

### `range_foldr`

references: [`Int`](Predef.md#type-int)

```bosatsu
def range_foldr[a](top: Int, init: a, fn: (Int, a) -> a) -> a
```