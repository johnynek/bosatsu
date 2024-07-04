package org.bykn.bosatsu

import Identifier.Bindable

sealed abstract class NameKind[T]
object NameKind {
  case class Let[T](
      name: Bindable,
      recursive: RecursionKind,
      value: TypedExpr[T]
  ) extends NameKind[T]
  case class Constructor[T](
      cn: Identifier.Constructor,
      params: List[(Bindable, rankn.Type)],
      defined: rankn.DefinedType[Kind.Arg],
      valueType: rankn.Type
  ) extends NameKind[T]
  case class Import[T](fromPack: Package.Interface, originalName: Identifier)
      extends NameKind[T]
  case class ExternalDef[T](
      pack: PackageName,
      defName: Identifier,
      defType: rankn.Type
  ) extends NameKind[T]

  def externals[T](from: Package.Typed[T]): Iterable[ExternalDef[T]] =
    from.externalDefs.to(LazyList).map { n =>
      // The type could be an import, so we need to check for the type
      // in the TypeEnv
      val pn = from.name
      val tpe = from.types.getExternalValue(pn, n).get
      ExternalDef[T](pn, n, tpe)
    }

  def apply[T](
      from: Package.Typed[T],
      item: Identifier
  ): Option[NameKind[T]] = {
    val prog = from.program

    def getLet: Option[NameKind[T]] =
      item.toBindable.flatMap { b =>
        prog._1.getLet(b).map { case (rec, d) => Let[T](b, rec, d) }
      }

    def getConstructor: Option[NameKind[T]] =
      item.toConstructor.flatMap { cn =>
        prog._1.types
          .getConstructor(from.name, cn)
          .map { case (dt, cfn) =>
            Constructor(cn, cfn.args, dt, dt.fnTypeOf(cfn))
          }
      }

    def getImport: Option[NameKind[T]] =
      from.localImport(item).map { case (originalPackage, i) =>
        Import(originalPackage, i.originalName)
      }

    def getExternal: Option[NameKind[T]] =
      externals(from).find(_.defName == item)

    getLet
      .orElse(getConstructor)
      .orElse(getImport)
      .orElse(getExternal)
  }
}
