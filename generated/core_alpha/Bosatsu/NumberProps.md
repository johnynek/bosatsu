---
github.base_url=
---

# `Bosatsu/NumberProps`

private package

source code:
- [`test_workspace/NumberProps.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/NumberProps.bosatsu)

public dependencies: [`Bosatsu/Num/BinNat`](Num/BinNat.html), [`Bosatsu/Num/Nat`](Num/Nat.html), [`Bosatsu/Rand`](Rand.html)

## Index

- Values: [`rand_BinNat`](#value-rand-binnat), [`rand_Int`](#value-rand-int),
[`rand_Nat`](#value-rand-nat)

## Values

<a id="value-rand-binnat"></a>

### `rand_BinNat`

references: [`Bosatsu/Num/BinNat::BinNat`](Num/BinNat.html#type-binnat), [`Bosatsu/Rand::Rand`](Rand.html#type-rand)

```bosatsu
rand_BinNat: Bosatsu/Rand::Rand[Bosatsu/Num/BinNat::BinNat]
```

<a id="value-rand-int"></a>

### `rand_Int`

Property checks for Nat, BinNat, Int
external def todo(ignore: x) -> forall a. a

references: [`Bosatsu/Rand::Rand`](Rand.html#type-rand), [`Int`](Predef.html#type-int)

```bosatsu
rand_Int: Bosatsu/Rand::Rand[Int]
```

<a id="value-rand-nat"></a>

### `rand_Nat`

references: [`Bosatsu/Num/Nat::Nat`](Num/Nat.html#type-nat), [`Bosatsu/Rand::Rand`](Rand.html#type-rand)

```bosatsu
rand_Nat: Bosatsu/Rand::Rand[Bosatsu/Num/Nat::Nat]
```