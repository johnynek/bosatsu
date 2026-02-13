# `BuildLibrary`

## Types

### `Build[a]`

```bosatsu
type Build[a: *]
```

### `File`

```bosatsu
type File
```

#### Constructors

- `File(path: String)`

### `LibArgs`

```bosatsu
type LibArgs
```

#### Constructors

- `LibArgs(srcs: Build[List[File]], deps: Build[List[Library]])`

### `Library`

```bosatsu
type Library
```

#### Constructors

- `Library`

## Values

### `build`

```bosatsu
def build(arg1: LibArgs) -> Build[Library]
```

### `build_all`

```bosatsu
def build_all[a](arg1: List[Build[a]]) -> Build[List[a]]
```

### `empty`

```bosatsu
empty: forall a: *. Build[List[a]]
```

### `file`

```bosatsu
def file(arg1: String) -> Build[File]
```

### `files`

```bosatsu
def files(arg1: List[String]) -> Build[List[File]]
```

### `library`

```bosatsu
def library(arg1: Build[List[File]], arg2: Build[List[Library]]) -> Build[Library]
```

### `map2_Build`

```bosatsu
def map2_Build[a, b, c](arg1: Build[a], arg2: Build[b], arg3: (a, b) -> c) -> Build[c]
```

### `map_Build`

```bosatsu
def map_Build[a, b](arg1: Build[a], arg2: a -> b) -> Build[b]
```