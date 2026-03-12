---
github.base_url=
---

# `Bosatsu/Option`

source code:
- [`test_workspace/Option.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Option.bosatsu)

## Index

- Values: [`eq_Option`](#value-eq-option)

## Values

<a id="value-eq-option"></a>

### `eq_Option`

references: [`Bool`](Predef.html#type-bool), [`Option`](Predef.html#type-option)

```bosatsu
def eq_Option[a](eq: (a, a) -> Bool) -> (Option[a], Option[a]) -> Bool
```