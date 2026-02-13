# `Bosatsu/Testing/Properties`

public dependencies: `Bosatsu/Rand`

## Types

### `Prop`

```bosatsu
type Prop
```

## Values

### `forall_Prop`

```bosatsu
def forall_Prop[a](arg1: Bosatsu/Rand::Rand[a], arg2: String, arg3: a -> Test) -> Prop
```

### `run_Prop`

```bosatsu
def run_Prop(arg1: Prop, arg2: Int, arg3: Int) -> Test
```

### `suite_Prop`

```bosatsu
def suite_Prop(arg1: String, arg2: List[Prop]) -> Prop
```