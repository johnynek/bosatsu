package dev.bosatsu

import cats.data.NonEmptyList
import dev.bosatsu.Identifier.Bindable
import dev.bosatsu.rankn.{DefinedType, Type, TypeEnv}

/** The `Bosatsu/Num/Int64` bindings that the compiler replaces by name.
  *
  * Matchless lowers `eq_Int64` and `cmp_Int64` to the builtin signed 64-bit
  * compare. Matchless and the C backend fold `int_to_Int64` and
  * `int_low_bits_to_Int64` applied to an `Int` literal into a constant. These
  * rewrites only look at the package and binding name. They agree with the
  * runtimes only when `Int64` is an external type, which each backend stores
  * directly as a 64-bit integer (the C runtime packs it into the `BValue`), and
  * each rewritten name is an `external def` with exactly the core type.
  *
  * The compiler establishes that precondition as follows. Library builds reject
  * `Bosatsu/` packages outside the core library, both in their own sources and
  * in their dependencies. Loose compilation (`tool` commands,
  * `compile-packages` and `emit-c`) has no library identity. There, every
  * package named `Bosatsu/Num/Int64` must pass `checkPackage` when it is
  * type-checked from source (`PackageCustoms.assemble`) or decoded from an
  * artifact (`ProtoConverter.packagesFromProto`). Every such interface must
  * pass `checkInterface` when it is decoded (`ProtoConverter`). Interfaces
  * derived from a checked package need no further check.
  *
  * These checks cover the Bosatsu declarations, not native code. Calls that are
  * not rewritten, such as `int_low_bits_to_Int64(x)` with a non-literal `x` or
  * `cmp_Int64` passed as a value, still run the external's native
  * implementation. The rewrites therefore also assume that implementation keeps
  * the standard runtime's representation and results. Each backend's built-in
  * Int64 externals do. A provider that binds these symbols itself, as `emit-c`
  * allows, must as well. Verifying that belongs to core-source identity checks,
  * not to this object.
  *
  * Adding a case to `Intrinsic` adds it to both checks.
  */
object Int64Intrinsics {
  val packageName: PackageName = PackageName.parts("Bosatsu", "Num", "Int64")
  val typeName: TypeName = TypeName("Int64")
  val int64Type: Type.TyConst =
    Type.TyConst(Type.Const.Defined(packageName, typeName))

  private val comparisonType: Type =
    Type.TyConst(Type.Const.predef("Comparison"))

  private def binary(result: Type): Type =
    Type.Fun(NonEmptyList.of(int64Type, int64Type), result)

  enum Intrinsic(val name: Bindable, val expectedType: Type) derives CanEqual {
    // Lowered to the builtin signed 64-bit compare.
    case EqInt64
        extends Intrinsic(Identifier.Name("eq_Int64"), binary(Type.BoolType))
    case CmpInt64
        extends Intrinsic(Identifier.Name("cmp_Int64"), binary(comparisonType))
    // Folded to a constant when applied to an Int literal.
    case IntToInt64
        extends Intrinsic(
          Identifier.Name("int_to_Int64"),
          Type.Fun(Type.IntType, Type.TyApply(Type.OptionType, int64Type))
        )
    case IntLowBitsToInt64
        extends Intrinsic(
          Identifier.Name("int_low_bits_to_Int64"),
          Type.Fun(Type.IntType, int64Type)
        )
  }

  private val intrinsicByName: Map[Identifier, Intrinsic] =
    Intrinsic.values.iterator.map(i => (i.name: Identifier, i)).toMap

  /** The intrinsic that a reference to `pack::name` denotes, if any. */
  def get(pack: PackageName, name: Identifier): Option[Intrinsic] =
    if (pack == packageName) intrinsicByName.get(name)
    else None

  enum Violation derives CanEqual {
    // `Int64` is missing, or is a struct, enum, alias or parameterized type.
    case Int64NotExternal
    // The name is bound by a `def` or other let rather than an `external def`.
    case NotExternalDef(intrinsic: Intrinsic)
    case WrongType(intrinsic: Intrinsic, found: Type)
  }

  /** The ways a `Bosatsu/Num/Int64` package or interface differs from the core
    * declarations that the intrinsics assume.
    *
    * `localTypeNames` are the types the package declares, so a local `Bool`
    * renders differently from `Bosatsu/Predef::Bool`.
    */
  final case class Mismatch(
      violations: NonEmptyList[Violation],
      localTypeNames: Set[TypeName]
  ) {
    private def showType(t: Type): String =
      TypeRenderer.render(
        t,
        TypeRenderer.Context(packageName, localTypeNames),
        80
      )

    private def describe(v: Violation): String =
      v match {
        case Violation.Int64NotExternal =>
          "Int64 must be declared `external type Int64`"
        case Violation.NotExternalDef(i) =>
          s"${i.name.sourceCodeRepr} must be declared `external def` with type ${showType(i.expectedType)}"
        case Violation.WrongType(i, found) =>
          s"${i.name.sourceCodeRepr} must have type ${showType(i.expectedType)}, found ${showType(found)}"
      }

    def message: String =
      violations.iterator
        .map(v => "\n  - " + describe(v))
        .mkString(
          s"${packageName.asString} must match the core library, because the compiler replaces these bindings with built-in operations:",
          "",
          s"\nUse ${packageName.asString} from the core library instead of defining it."
        )
  }

  private def localTypeNames(types: TypeEnv[Kind.Arg]): Set[TypeName] =
    types.definedTypes.keysIterator.collect {
      case (pn, tn) if pn == packageName => tn
    }.toSet

  private def isExternalType(dt: DefinedType[Kind.Arg]): Boolean =
    // Structs and enums always have at least one constructor.
    dt.isOpaque && dt.typeParams.isEmpty

  /** Check a package implementation, from source or decoded from an artifact.
    */
  def checkPackage[A](pack: Package.Typed[A]): Option[Mismatch] =
    if (pack.name != packageName) None
    else {
      val types = pack.types
      val letNames = pack.lets.iterator.map(_._1).toSet
      val int64 =
        if (types.getType(packageName, typeName).exists(isExternalType)) Nil
        else Violation.Int64NotExternal :: Nil
      val defs = Intrinsic.values.toList.flatMap { i =>
        // TypeEnv values hold exactly the external defs.
        types.getExternalValue(packageName, i.name) match {
          case Some(found) =>
            if (found.sameAs(i.expectedType)) Nil
            else Violation.WrongType(i, found) :: Nil
          case None =>
            if (letNames(i.name)) Violation.NotExternalDef(i) :: Nil
            else Nil
        }
      }
      NonEmptyList
        .fromList(int64 ::: defs)
        .map(Mismatch(_, localTypeNames(types)))
    }

  /** Check a decoded interface. An interface cannot distinguish an external
    * type from a struct exported without constructors, or an `external def`
    * from a `def`, so this checks only the visible shape and types. Consumers
    * cannot build or inspect an `Int64` without calling the implementation, and
    * `checkPackage` covers the implementation wherever it enters a compilation.
    */
  def checkInterface(iface: Package.Interface): Option[Mismatch] =
    if (iface.name != packageName) None
    else {
      val int64Const = int64Type.tpe
      val int64 =
        if (
          iface.exports.exists { exp =>
            exp.tag match {
              case Referant.DefinedT(dt) =>
                (dt.toTypeConst == int64Const) && !isExternalType(dt)
              case Referant.Constructor(dt, _) =>
                dt.toTypeConst == int64Const
              case Referant.TypeAliasT(ta) =>
                ta.toTypeConst == int64Const
              case Referant.Value(_) => false
            }
          }
        ) Violation.Int64NotExternal :: Nil
        else Nil
      val defs = iface.exports.flatMap {
        case ExportedName.Binding(name, Referant.Value(found)) =>
          intrinsicByName.get(name).toList.flatMap { i =>
            if (found.sameAs(i.expectedType)) Nil
            else Violation.WrongType(i, found) :: Nil
          }
        case _ => Nil
      }
      NonEmptyList
        .fromList(int64 ::: defs)
        .map(Mismatch(_, localTypeNames(iface.exportedTypeEnv)))
    }
}
