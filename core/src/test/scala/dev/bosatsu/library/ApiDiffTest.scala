package dev.bosatsu.library

import cats.data.Validated
import dev.bosatsu.rankn.{ConstructorFn, ConstructorParam, DefinedType, Type, TypeAlias, TypeEnv}
import dev.bosatsu.{
  ExportedName,
  Identifier,
  Kind,
  PackageName,
  Referant,
  TypeName,
  Variance
}
import scala.collection.immutable.SortedMap

class ApiDiffTest extends munit.FunSuite {
  private val pack = PackageName.parts("ApiDiff", "DefaultParam")
  private val constructor = Identifier.Constructor("Rec")
  private val tyName = TypeName(constructor)
  private val aliasConstructor = Identifier.Constructor("Alias")
  private val aliasName = TypeName(aliasConstructor)
  private val fieldName = Identifier.Name("a")
  private val fieldType = Type.TyVar(Type.Var.Bound("a"))

  private def definedType(
      defaultBinding: Option[Identifier.Bindable]
  ): DefinedType[Kind.Arg] = {
    val cfn = ConstructorFn[Kind.Arg](
      name = constructor,
      args = List(
        ConstructorParam(
          name = fieldName,
          tpe = fieldType,
          defaultBinding = defaultBinding
        )
      )
    )

    DefinedType(pack, tyName, Nil, cfn :: Nil)
  }

  private def iface(
      dt: DefinedType[Kind.Arg]
  ): SortedMap[PackageName, List[ExportedName[Referant[Kind.Arg]]]] = {
    val cfn = dt.constructors.head
    SortedMap(
      pack -> List(
        ExportedName.Constructor(
          constructor,
          Referant.Constructor(dt, cfn)
        )
      )
    )
  }

  private def alias(
      variance: Variance,
      rhs: Type
  ): TypeAlias[Kind.Arg] =
    TypeAlias(
      pack,
      aliasName,
      List((Type.Var.Bound("a"), Kind.Arg(variance, Kind.Type))),
      rhs
    )

  private def aliasIface(
      ta: TypeAlias[Kind.Arg]
  ): SortedMap[PackageName, List[ExportedName[Referant[Kind.Arg]]]] =
    SortedMap(
      pack -> List(
        ExportedName.TypeName(aliasConstructor, Referant.TypeAliasT(ta))
      )
    )

  private def diffsFor(
      oldDefault: Option[Identifier.Bindable],
      newDefault: Option[Identifier.Bindable]
  ): ApiDiff.Diffs = {
    val oldDt = definedType(oldDefault)
    val newDt = definedType(newDefault)
    val res = ApiDiff(
      iface(oldDt),
      TypeEnv.fromDefinitions(oldDt :: Nil),
      iface(newDt),
      TypeEnv.fromDefinitions(newDt :: Nil)
    )

    res match {
      case Validated.Valid(diffs)   => diffs
      case Validated.Invalid(errors) =>
        fail(s"expected diffs, got errors: ${errors.iterator.toList}")
    }
  }

  test("constructor default added is minor/major valid but patch invalid") {
    val diffs = diffsFor(None, Some(Identifier.Name("default_value")))
    val flat = diffs.toMap.valuesIterator.flatMap(_.toList).toList

    assert(
      flat.exists(_.isInstanceOf[ApiDiff.ConstructorParamDefaultAdded]),
      s"expected ConstructorParamDefaultAdded in: $flat"
    )
    assert(!diffs.isValidWhen(Version.DiffKind.Patch))
    assert(diffs.isValidWhen(Version.DiffKind.Minor))
    assert(diffs.isValidWhen(Version.DiffKind.Major))
  }

  test("constructor default removed is major only") {
    val diffs = diffsFor(Some(Identifier.Name("default_value")), None)
    val flat = diffs.toMap.valuesIterator.flatMap(_.toList).toList

    assert(
      flat.exists(_.isInstanceOf[ApiDiff.ConstructorParamDefaultRemoved]),
      s"expected ConstructorParamDefaultRemoved in: $flat"
    )
    assert(!diffs.isValidWhen(Version.DiffKind.Patch))
    assert(!diffs.isValidWhen(Version.DiffKind.Minor))
    assert(diffs.isValidWhen(Version.DiffKind.Major))
  }

  test("adding an alias is tracked as a public API change") {
    val oldDt = definedType(None)
    val listAlias =
      alias(
        Variance.co,
        Type.TyApply(Type.ListType, Type.TyVar(Type.Var.Bound("a")))
      )
    val newIface =
      SortedMap(
        pack -> (iface(oldDt)(pack) ++ aliasIface(listAlias)(pack))
      )
    val res = ApiDiff(
      iface(oldDt),
      TypeEnv.fromDefinitions(oldDt :: Nil),
      newIface,
      TypeEnv.fromDefinitionsAndAliases(oldDt :: Nil, listAlias :: Nil)
    )

    val diffs =
      res match {
        case Validated.Valid(value)   => value
        case Validated.Invalid(errors) =>
          fail(s"expected diffs, got errors: ${errors.iterator.toList}")
      }
    val flat = diffs.toMap.valuesIterator.flatMap(_.toList).toList

    assert(flat.exists(_.isInstanceOf[ApiDiff.AddedName]), s"expected AddedName in: $flat")
  }

  test("changing an alias kind or rhs is breaking") {
    val oldAlias =
      alias(
        Variance.co,
        Type.TyApply(Type.ListType, Type.TyVar(Type.Var.Bound("a")))
      )
    val newAlias =
      alias(
        Variance.in,
        Type.TyApply(Type.OptionType, Type.TyVar(Type.Var.Bound("a")))
      )

    val res = ApiDiff(
      aliasIface(oldAlias),
      TypeEnv.fromDefinitionsAndAliases(Nil, oldAlias :: Nil),
      aliasIface(newAlias),
      TypeEnv.fromDefinitionsAndAliases(Nil, newAlias :: Nil)
    )

    val diffs =
      res match {
        case Validated.Valid(value)   => value
        case Validated.Invalid(errors) =>
          fail(s"expected diffs, got errors: ${errors.iterator.toList}")
      }
    val flat = diffs.toMap.valuesIterator.flatMap(_.toList).toList

    assert(
      flat.exists(_.isInstanceOf[ApiDiff.ChangedAliasKind]),
      s"expected ChangedAliasKind in: $flat"
    )
    assert(
      flat.exists(_.isInstanceOf[ApiDiff.ChangedAliasRhs]),
      s"expected ChangedAliasRhs in: $flat"
    )
    assert(!diffs.isValidWhen(Version.DiffKind.Minor))
    assert(diffs.isValidWhen(Version.DiffKind.Major))
  }
}
