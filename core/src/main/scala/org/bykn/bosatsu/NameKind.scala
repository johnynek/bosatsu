package org.bykn.bosatsu

import Identifier.Bindable

sealed abstract class NameKind
object NameKind {
  case class Let(name: Bindable, recursive: RecursionKind, value: TypedExpr[Declaration]) extends NameKind
  case class Constructor(
    cn: Identifier.Constructor,
    params: List[(Bindable, rankn.Type)],
    defined: rankn.DefinedType[Variance],
    valueType: rankn.Type) extends NameKind
  case class Import(fromPack: Package.Interface, originalName: Identifier) extends NameKind
  case class ExternalDef(pack: PackageName, defName: Identifier, defType: rankn.Type) extends NameKind

  def externals(from: Package.Inferred): Stream[ExternalDef] = {
    val prog = from.program
    prog.from.toStream.collect {
      case Statement.ExternalDef(n, _, _, _) =>
        // The type could be an import, so we need to check for the type
        // in the TypeEnv
        val pn = from.name
        val tpe = prog.types.getValue(pn, n).get
        ExternalDef(pn, n, tpe)
    }
  }

  def apply(from: Package.Inferred, item: Identifier): Option[NameKind] = {
    val prog = from.program

    def getLet: Option[NameKind] =
      item.toBindable.flatMap { b =>
        prog.getLet(b).map { case (rec, d) => Let(b, rec, d) }
      }

    def getConstructor: Option[NameKind] =
      item.toConstructor.flatMap { cn =>
        prog.types
          .getConstructor(from.name, cn)
          .map { case (params, dt, tpe) =>
            Constructor(cn, params, dt, tpe)
          }
      }

    def getImport: Option[NameKind] =
      from.localImport(item).map { case (originalPackage, i) =>
        Import(originalPackage, i.originalName)
      }

    def getExternal: Option[NameKind] =
      // there should not be duplicate top level names TODO lint for this
      prog.from.toStream.collectFirst {
        case Statement.ExternalDef(n, _, _, _) if n == item =>
          // The type could be an import, so we need to check for the type
          // in the TypeEnv
          val pn = from.name
          val tpe = prog.types.getValue(pn, n).get
          ExternalDef(pn, item, tpe)
      }

    getLet
      .orElse(getConstructor)
      .orElse(getImport)
      .orElse(getExternal)
  }
}
