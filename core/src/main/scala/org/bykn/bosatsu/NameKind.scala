package org.bykn.bosatsu

sealed abstract class NameKind
object NameKind {
  case class Let(value: TypedExpr[Declaration]) extends NameKind
  case class Constructor(
    cn: ConstructorName,
    params: List[(ParamName, rankn.Type)],
    defined: rankn.DefinedType[Variance],
    valueType: rankn.Type) extends NameKind
  case class Import(fromPack: Package.Interface, originalName: String) extends NameKind
  case class ExternalDef(pack: PackageName, defName: String, defType: rankn.Type) extends NameKind

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

  def apply(from: Package.Inferred, item: String): Option[NameKind] = {
    val prog = from.program

    def getLet: Option[NameKind] =
      prog.getLet(item).map(Let(_))

    def getConstructor: Option[NameKind] = {
      val cn = ConstructorName(item)
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
