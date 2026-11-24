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

  test("a changed recursive type is diffed once, not through its own fields forever") {
    val treeCons = Identifier.Constructor("Tree")
    val treeName = TypeName(treeCons)
    val tree: Type = Type.TyConst(Type.Const.Defined(pack, treeName))
    val leaf = ConstructorFn[Kind.Arg](Identifier.Constructor("Leaf"), Nil)
    val node = Identifier.Constructor("Node")
    val left = ConstructorParam(Identifier.Name("left"), tree, None)
    val size = ConstructorParam(Identifier.Name("size"), Type.IntType, None)

    def treeDt(nodeArgs: List[ConstructorParam]): DefinedType[Kind.Arg] =
      DefinedType(pack, treeName, Nil, leaf :: ConstructorFn[Kind.Arg](node, nodeArgs) :: Nil)

    def exports(dt: DefinedType[Kind.Arg]) =
      SortedMap(
        pack -> (
          ExportedName.TypeName(treeCons, Referant.DefinedT(dt)) ::
            ExportedName.Binding(Identifier.Name("root"), Referant.Value(tree)) ::
            dt.constructors.map { cf =>
              ExportedName.Constructor(cf.name, Referant.Constructor(dt, cf))
            }
        )
      )

    val oldDt = treeDt(left :: Nil)
    val newDt = treeDt(left :: size :: Nil)
    val diffs = ApiDiff(
      exports(oldDt),
      TypeEnv.fromDefinitions(oldDt :: Nil),
      exports(newDt),
      TypeEnv.fromDefinitions(newDt :: Nil)
    ) match {
      case Validated.Valid(diffs)    => diffs
      case Validated.Invalid(errors) =>
        fail(s"expected diffs, got errors: ${errors.iterator.toList}")
    }
    val flat = diffs.toMap.valuesIterator.flatMap(_.toList).toList

    assert(
      flat.exists {
        case ApiDiff.ConstructorParamAdded(_, `node`, _, 1, _, _) => true
        case _                                                    => false
      },
      s"expected the added Node field in: $flat"
    )
    assert(
      flat.exists {
        case ApiDiff.ChangedValue(Identifier.Name("root"), _) => true
        case _                                               => false
      },
      s"expected root to see the change to Tree in: $flat"
    )
    assert(!diffs.isValidWhen(Version.DiffKind.Minor))
    assert(diffs.isValidWhen(Version.DiffKind.Major))
  }

  private def rec(
      params: List[ConstructorParam],
      typeParams: List[(Type.Var.Bound, Kind.Arg)] = Nil
  ): DefinedType[Kind.Arg] =
    DefinedType(pack, tyName, typeParams, ConstructorFn[Kind.Arg](constructor, params) :: Nil)

  // `export Rec`
  private def opaqueExport(dt: DefinedType[Kind.Arg]) =
    SortedMap(pack -> List(ExportedName.TypeName(constructor, Referant.DefinedT(dt))))

  // `export Rec()`
  private def publicExport(dt: DefinedType[Kind.Arg]) =
    SortedMap(
      pack -> (
        ExportedName.TypeName(constructor, Referant.DefinedT(dt)) ::
          dt.constructors.map { cf =>
            ExportedName.Constructor(cf.name, Referant.Constructor(dt, cf))
          }
      )
    )

  private def diffExports(
      prev: SortedMap[PackageName, List[ExportedName[Referant[Kind.Arg]]]],
      prevDt: DefinedType[Kind.Arg],
      curr: SortedMap[PackageName, List[ExportedName[Referant[Kind.Arg]]]],
      currDt: DefinedType[Kind.Arg]
  ): (ApiDiff.Diffs, List[Diff]) =
    ApiDiff(
      prev,
      TypeEnv.fromDefinitions(prevDt :: Nil),
      curr,
      TypeEnv.fromDefinitions(currDt :: Nil)
    ) match {
      case Validated.Valid(diffs)    =>
        (diffs, diffs.toMap.valuesIterator.flatMap(_.toList).toList)
      case Validated.Invalid(errors) =>
        fail(s"expected diffs, got errors: ${errors.iterator.toList}")
    }

  private val fieldA = ConstructorParam(fieldName, Type.IntType, None)
  private val fieldB = ConstructorParam(Identifier.Name("b"), Type.StrType, None)

  test("changing the private constructors of an opaque export is not an API change") {
    val oldDt = rec(fieldA :: Nil)
    val newDt = rec(fieldB :: fieldA :: Nil)
    val (diffs, flat) = diffExports(opaqueExport(oldDt), oldDt, opaqueExport(newDt), newDt)
    assertEquals(flat, Nil)
    assert(diffs.isValidWhen(Version.DiffKind.Patch))
  }

  test("an opaque export still reports a kind change") {
    val a = Type.Var.Bound("a")
    val oldDt = rec(fieldA :: Nil)
    val newDt = rec(fieldB :: Nil, (a, Kind.Arg(Variance.phantom, Kind.Type)) :: Nil)
    val (_, flat) = diffExports(opaqueExport(oldDt), oldDt, opaqueExport(newDt), newDt)
    assert(flat.nonEmpty && flat.forall(_.isInstanceOf[ApiDiff.ChangedKind]), s"got: $flat")
  }

  test("exporting the constructors of an opaque type is a minor change") {
    val dt = rec(fieldA :: Nil)
    val (diffs, flat) = diffExports(opaqueExport(dt), dt, publicExport(dt), dt)
    assert(
      flat.exists(_.isInstanceOf[ApiDiff.ConstructorsExposed]),
      s"expected ConstructorsExposed in: $flat"
    )
    assert(!diffs.isValidWhen(Version.DiffKind.Patch))
    assert(diffs.isValidWhen(Version.DiffKind.Minor))
  }

  test("hiding the constructors of a type is a major change") {
    val dt = rec(fieldA :: Nil)
    val (diffs, flat) = diffExports(publicExport(dt), dt, opaqueExport(dt), dt)
    assert(
      flat.exists(_.isInstanceOf[ApiDiff.ConstructorRemoved]),
      s"expected ConstructorRemoved in: $flat"
    )
    assert(!diffs.isValidWhen(Version.DiffKind.Minor))
    assert(diffs.isValidWhen(Version.DiffKind.Major))
  }
}
