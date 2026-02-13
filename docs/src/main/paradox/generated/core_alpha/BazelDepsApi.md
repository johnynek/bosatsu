# `BazelDepsApi`

## Types

### `Artifact`

```bosatsu
type Artifact
```

#### Constructors

- `Artifact(modules: List[String], lang: String, version: String, exports: List[String])`

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

### `Replacement`

```bosatsu
type Replacement
```

#### Constructors

- `Replacement(lang: String, target: String)`

### `Resolver`

```bosatsu
type Resolver
```

#### Constructors

- `Resolver(id: String, type: String, url: String)`

## Values

### `default_options_with_scala`

```bosatsu
default_options_with_scala: Options
```

### `maven_central`

```bosatsu
maven_central: Resolver
```

### `merge_dep_List`

```bosatsu
def merge_dep_List(arg1: List[Dict[String, Dict[String, Artifact]]]) -> Dict[String, Dict[String, Artifact]]
```

### `merge_deps`

```bosatsu
def merge_deps(arg1: Dict[String, Dict[String, Artifact]], arg2: Dict[String, Dict[String, Artifact]]) -> Dict[String, Dict[String, Artifact]]
```

### `scala_dep`

```bosatsu
def scala_dep(arg1: String, arg2: String, arg3: String) -> Dict[String, Dict[String, Artifact]]
```

### `scala_dep_modules`

```bosatsu
def scala_dep_modules(arg1: String, arg2: String, arg3: List[String], arg4: String, arg5: List[String]) -> Dict[String, Dict[String, Artifact]]
```

### `standard_scala_replacements`

```bosatsu
standard_scala_replacements: Dict[String, Dict[String, Replacement]]
```