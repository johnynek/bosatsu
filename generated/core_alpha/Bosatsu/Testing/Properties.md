---
github.base_url=
---

# `Bosatsu/Testing/Properties`

source code:
- [`test_workspace/Properties.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Properties.bosatsu)

public dependencies: [`Bosatsu/Rand`](../Rand.html)

## Index

- Types: [`Prop`](#type-prop)
- Values: [`forall_Prop`](#value-forall-prop), [`run_Prop`](#value-run-prop),
[`suite_Prop`](#value-suite-prop)

## Types

<a id="type-prop"></a>

### `Prop`

```bosatsu
type Prop
```

## Values

<a id="value-forall-prop"></a>

### `forall_Prop`

references: [`Bosatsu/Rand::Rand`](../Rand.html#type-rand), [`Prop`](#type-prop), [`String`](../Predef.html#type-string), [`Test`](../Predef.html#type-test)

```bosatsu
def forall_Prop[a](rand: Bosatsu/Rand::Rand[a], name: String, fn: a -> Test) -> Prop
```

<a id="value-run-prop"></a>

### `run_Prop`

references: [`Int`](../Predef.html#type-int), [`Prop`](#type-prop), [`Test`](../Predef.html#type-test)

```bosatsu
def run_Prop(prop: Prop, trials: Int, seed: Int) -> Test
```

<a id="value-suite-prop"></a>

### `suite_Prop`

references: [`List`](../Predef.html#type-list), [`Prop`](#type-prop), [`String`](../Predef.html#type-string)

```bosatsu
def suite_Prop(name: String, props: List[Prop]) -> Prop
```