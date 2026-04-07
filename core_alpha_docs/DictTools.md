# `DictTools`

private package

source code:
- [`test_workspace/dicttools.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/dicttools.bosatsu)

## Index

- Values: [`merge`](#value-merge)

## Values

<a id="value-merge"></a>

### `merge`

references: [`Dict`](Bosatsu/Predef.md#type-dict)

```bosatsu
def merge[a, b](left: Dict[a, b], right: Dict[a, b], fn: (b, b) -> b) -> Dict[a, b]
```