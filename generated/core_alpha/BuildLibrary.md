---
github.base_url=
---

# `BuildLibrary`

private package

source code:
- [`test_workspace/BuildLibrary.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/BuildLibrary.bosatsu)

## Index

- Types: [`Build`](#type-build), [`File`](#type-file), [`LibArgs`](#type-libargs),
[`Library`](#type-library)
- Values: [`build`](#value-build), [`build_all`](#value-build-all), [`empty`](#value-empty),
[`file`](#value-file), [`files`](#value-files), [`library`](#value-library),
[`map2_Build`](#value-map2-build), [`map_Build`](#value-map-build)

## Types

<a id="type-build"></a>

### `Build[a]`

```bosatsu
type Build[a: *]
```

<a id="type-file"></a>

### `File`

```bosatsu
type File
```

#### Constructors

- `File(path: String)`

<a id="type-libargs"></a>

### `LibArgs`

```bosatsu
type LibArgs
```

#### Constructors

- `LibArgs(srcs: Build[List[File]], deps: Build[List[Library]])`

<a id="type-library"></a>

### `Library`

```bosatsu
type Library
```

#### Constructors

- `Library`

## Values

<a id="value-build"></a>

### `build`

references: [`Build`](#type-build), [`LibArgs`](#type-libargs), [`Library`](#type-library)

```bosatsu
def build(args: LibArgs) -> Build[Library]
```

<a id="value-build-all"></a>

### `build_all`

references: [`Build`](#type-build), [`List`](Bosatsu/Predef.html#type-list)

```bosatsu
def build_all[a](items: List[Build[a]]) -> Build[List[a]]
```

<a id="value-empty"></a>

### `empty`

references: [`Build`](#type-build), [`List`](Bosatsu/Predef.html#type-list)

```bosatsu
empty: forall a: *. Build[List[a]]
```

<a id="value-file"></a>

### `file`

references: [`Build`](#type-build), [`File`](#type-file), [`String`](Bosatsu/Predef.html#type-string)

```bosatsu
def file(s: String) -> Build[File]
```

<a id="value-files"></a>

### `files`

references: [`Build`](#type-build), [`File`](#type-file), [`List`](Bosatsu/Predef.html#type-list), [`String`](Bosatsu/Predef.html#type-string)

```bosatsu
def files(fs: List[String]) -> Build[List[File]]
```

<a id="value-library"></a>

### `library`

references: [`Build`](#type-build), [`File`](#type-file), [`Library`](#type-library), [`List`](Bosatsu/Predef.html#type-list)

```bosatsu
def library(sources: Build[List[File]], deps: Build[List[Library]]) -> Build[Library]
```

<a id="value-map2-build"></a>

### `map2_Build`

references: [`Build`](#type-build)

```bosatsu
def map2_Build[a, b, c](ba: Build[a], bb: Build[b], fn: (a, b) -> c) -> Build[c]
```

<a id="value-map-build"></a>

### `map_Build`

references: [`Build`](#type-build)

```bosatsu
def map_Build[a, b](b: Build[a], fn: a -> b) -> Build[b]
```