# `Bosatsu/Nothing`

private package

source code:
- [`test_workspace/Nothing.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Nothing.bosatsu)

## Index

- Types: [`Nothing`](#type-nothing)
- Values: [`impossible`](#value-impossible)

## Types

<a id="type-nothing"></a>

### `Nothing`

In a total language, it is not possible
to create a value with the type of magic

```bosatsu
type Nothing
```

## Values

<a id="value-impossible"></a>

### `impossible`

since, we cannot create an instance of Nothing
assuming we have one is a contradiction, so
anything is possible

references: [`Nothing`](#type-nothing)

```bosatsu
def impossible[a](n: Nothing) -> a
```