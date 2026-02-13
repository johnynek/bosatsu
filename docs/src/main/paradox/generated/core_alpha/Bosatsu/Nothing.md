# `Bosatsu/Nothing`

## Types

### `Nothing`

In a total language, it is not possible
to create a value with the type of magic

```bosatsu
type Nothing
```

## Values

### `impossible`

since, we cannot create an instance of Nothing
assuming we have one is a contradiction, so
anything is possible

```bosatsu
def impossible[a](arg1: Nothing) -> a
```