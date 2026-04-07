# `BazelDepsApi`

private package

source code:
- [`test_workspace/bazel_deps_api.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/bazel_deps_api.bosatsu)

## Index

- Types: [`Artifact`](#type-artifact), [`Config`](#type-config), [`Options`](#type-options),
[`Replacement`](#type-replacement), [`Resolver`](#type-resolver)
- Values: [`default_options_with_scala`](#value-default-options-with-scala),
[`maven_central`](#value-maven-central), [`merge_dep_List`](#value-merge-dep-list),
[`merge_deps`](#value-merge-deps), [`scala_dep`](#value-scala-dep),
[`scala_dep_modules`](#value-scala-dep-modules),
[`standard_scala_replacements`](#value-standard-scala-replacements)

## Types

<a id="type-artifact"></a>

### `Artifact`

```bosatsu
type Artifact
```

#### Constructors

- `Artifact(modules: List[String], lang: String, version: String, exports: List[String])`

<a id="type-config"></a>

### `Config`

```bosatsu
type Config
```

#### Constructors

- Config(
      options: Options,
      dependencies: Dict[String, Dict[String, Artifact]],
      replacements: Dict[String, Dict[String, Replacement]]
  )

<a id="type-options"></a>

### `Options`

```bosatsu
type Options
```

#### Constructors

- Options(
      build_header: List[String],
      languages: List[String],
      resolvers: List[Resolver],
      transitivity: String,
      resolver_type: String,
      version_conflict_policy: String
  )

<a id="type-replacement"></a>

### `Replacement`

```bosatsu
type Replacement
```

#### Constructors

- `Replacement(lang: String, target: String)`

<a id="type-resolver"></a>

### `Resolver`

```bosatsu
type Resolver
```

#### Constructors

- `Resolver(id: String, type: String, url: String)`

## Values

<a id="value-default-options-with-scala"></a>

### `default_options_with_scala`

references: [`Options`](#type-options)

```bosatsu
default_options_with_scala: Options
```

<a id="value-maven-central"></a>

### `maven_central`

references: [`Resolver`](#type-resolver)

```bosatsu
maven_central: Resolver
```

<a id="value-merge-dep-list"></a>

### `merge_dep_List`

references: [`Artifact`](#type-artifact), [`Dict`](Bosatsu/Predef.md#type-dict), [`List`](Bosatsu/Predef.md#type-list), [`String`](Bosatsu/Predef.md#type-string)

```bosatsu
def merge_dep_List(deps: List[Dict[String, Dict[String, Artifact]]]) -> Dict[String, Dict[String, Artifact]]
```

<a id="value-merge-deps"></a>

### `merge_deps`

references: [`Artifact`](#type-artifact), [`Dict`](Bosatsu/Predef.md#type-dict), [`String`](Bosatsu/Predef.md#type-string)

```bosatsu
def merge_deps(left: Dict[String, Dict[String, Artifact]], right: Dict[String, Dict[String, Artifact]]) -> Dict[String, Dict[String, Artifact]]
```

<a id="value-scala-dep"></a>

### `scala_dep`

references: [`Artifact`](#type-artifact), [`Dict`](Bosatsu/Predef.md#type-dict), [`String`](Bosatsu/Predef.md#type-string)

```bosatsu
def scala_dep(orgname: String, artifact: String, version: String) -> Dict[String, Dict[String, Artifact]]
```

<a id="value-scala-dep-modules"></a>

### `scala_dep_modules`

references: [`Artifact`](#type-artifact), [`Dict`](Bosatsu/Predef.md#type-dict), [`List`](Bosatsu/Predef.md#type-list), [`String`](Bosatsu/Predef.md#type-string)

```bosatsu
def scala_dep_modules(orgname: String, artifact: String, modules: List[String], version: String, exports: List[String]) -> Dict[String, Dict[String, Artifact]]
```

<a id="value-standard-scala-replacements"></a>

### `standard_scala_replacements`

references: [`Dict`](Bosatsu/Predef.md#type-dict), [`Replacement`](#type-replacement), [`String`](Bosatsu/Predef.md#type-string)

```bosatsu
standard_scala_replacements: Dict[String, Dict[String, Replacement]]
```