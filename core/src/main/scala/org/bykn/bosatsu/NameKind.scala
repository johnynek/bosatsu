package org.bykn.bosatsu

import Identifier.Bindable

sealed abstract class NameKind[T]
object NameKind {
  case class Let[T](name: Bindable, recursive: RecursionKind, value: TypedExpr[T]) extends NameKind[T]
  case class Constructor[T](
    cn: Identifier.Constructor,
    params: List[(Bindable, rankn.Type)],
    defined: rankn.DefinedType[Variance],
    valueType: rankn.Type) extends NameKind[T]
  case class Import[T](fromPack: Package.Interface, originalName: Identifier) extends NameKind[T]
  case class ExternalDef[T](pack: PackageName, defName: Identifier, defType: rankn.Type) extends NameKind[T]

  def externals[T](from: Package.Typed[T]): Stream[ExternalDef[T]] = {
    val prog = from.program
    prog.externalDefs.toStream.map { n =>
      // The type could be an import, so we need to check for the type
      // in the TypeEnv
      val pn = from.name
      val tpe = prog.types.getValue(pn, n).get
      ExternalDef[T](pn, n, tpe)
    }
  }

  def apply[T](from: Package.Typed[T], item: Identifier): Option[NameKind[T]] = {
    val prog = from.program

    def getLet: Option[NameKind[T]] =
      item.toBindable.flatMap { b =>
        prog.getLet(b).map { case (rec, d) => Let[T](b, rec, d) }
      }

    def getConstructor: Option[NameKind[T]] =
      item.toConstructor.flatMap { cn =>
        prog.types
          .getConstructor(from.name, cn)
          .map { case (params, dt, tpe) =>
            Constructor(cn, params, dt, tpe)
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
