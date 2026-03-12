---
github.base_url=
---

# `Bosatsu/Json`

source code:
- [`test_workspace/Bosatsu/Json.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Bosatsu/Json.bosatsu)

## Index

- Types: [`Json`](#type-json), [`Nullable`](#type-nullable), [`Optional`](#type-optional)
- Values: [`eq_Json`](#value-eq-json), [`parse_Json`](#value-parse-json),
[`render_Json`](#value-render-json)

## Types

<a id="type-json"></a>

### `Json`

```bosatsu
type Json
```

#### Constructors

- `JArray(items: List[Json])`
- `JBool(value: Bool)`
- `JFloat(value: Float64)`
- `JInt(value: Int)`
- `JNull`
- `JObject(items: List[(String, Json)])`
- `JString(value: String)`

<a id="type-nullable"></a>

### `Nullable[a]`

```bosatsu
type Nullable[a: +*]
```

#### Constructors

- `NonNull(value: a)`
- `Null`

<a id="type-optional"></a>

### `Optional[a]`

```bosatsu
type Optional[a: +*]
```

#### Constructors

- `Missing`
- `Set(value: a)`

## Values

<a id="value-eq-json"></a>

### `eq_Json`

references: [`Bool`](Predef.html#type-bool), [`Json`](#type-json)

```bosatsu
def eq_Json(left: Json, right: Json) -> Bool
```

<a id="value-parse-json"></a>

### `parse_Json`

references: [`Json`](#type-json), [`Option`](Predef.html#type-option), [`String`](Predef.html#type-string)

```bosatsu
def parse_Json(s: String) -> Option[Json]
```

<a id="value-render-json"></a>

### `render_Json`

references: [`Json`](#type-json), [`String`](Predef.html#type-string)

```bosatsu
def render_Json(j: Json) -> String
```