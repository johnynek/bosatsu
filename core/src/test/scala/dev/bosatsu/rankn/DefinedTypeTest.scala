package dev.bosatsu.rankn

import dev.bosatsu.{Identifier, Kind, PackageName, TypeName}

class DefinedTypeTest extends munit.FunSuite {
  private val pack = PackageName.parts("DefinedTypeTest")
  private val a = Type.Var.Bound("a")
  private val b = Type.Var.Bound("b")

  private val pairCtor = ConstructorFn[Kind.Arg](
    name = Identifier.Constructor("Pair"),
    args = List(
      ConstructorParam(Identifier.Name("left"), Type.TyVar(a), None),
      ConstructorParam(Identifier.Name("right"), Type.TyVar(b), None),
      ConstructorParam(
        Identifier.Name("lefts"),
        Type.TyApply(Type.ListType, Type.TyVar(a)),
        None
      )
    )
  )

  private val pairType = DefinedType[Kind.Arg](
    packageName = pack,
    name = TypeName("PairBox"),
    annotatedTypeParams = List((a, Kind.Type.in), (b, Kind.Type.in)),
    constructors = List(pairCtor)
  )

  test("extractTypeArgs returns applied args for matching defined type") {
    val targetType =
      Type.applyAll(pairType.toTypeTyConst, List(Type.IntType, Type.StrType))

    assertEquals(
      pairType.extractTypeArgs(targetType),
      Some(List(Type.IntType, Type.StrType))
    )
  }

  test("extractTypeArgs rejects wrong roots and arity mismatches") {
    val wrongRoot = Type.applyAll(Type.OptionType, List(Type.IntType))
    val wrongArity = Type.applyAll(pairType.toTypeTyConst, List(Type.IntType))

    assertEquals(pairType.extractTypeArgs(wrongRoot), None)
    assertEquals(pairType.extractTypeArgs(wrongArity), None)
  }

  test("instantiateConstructorFieldTypes substitutes type params") {
    val fieldTypes =
      pairType.instantiateConstructorFieldTypes(
        pairCtor,
        List(Type.IntType, Type.StrType)
      )

    assertEquals(
      fieldTypes,
      Some(
        List(
          Type.IntType,
          Type.StrType,
          Type.TyApply(Type.ListType, Type.IntType)
        )
      )
    )
  }

  test("instantiateConstructorFieldTypes rejects wrong type-arg arity") {
    assertEquals(
      pairType.instantiateConstructorFieldTypes(
        pairCtor,
        List(Type.IntType)
      ),
      None
    )
  }
}
