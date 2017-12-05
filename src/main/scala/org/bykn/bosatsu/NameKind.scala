package org.bykn.bosatsu

sealed abstract class NameKind
object NameKind {
  case class Let(value: Expr[(Declaration, Scheme)]) extends NameKind
  case class Constructor(cn: ConstructorName, dt: DefinedType, scheme: Scheme) extends NameKind
  case class Import(fromPack: Package.Inferred, originalName: String) extends NameKind
  case class ExternalDef(pack: PackageName, defName: String, defType: Scheme) extends NameKind

  def apply(from: Package.Inferred, item: String): Option[NameKind] = {
    val prog = from.unfix.program

    def getLet: Option[NameKind] =
      prog.getLet(item).map(Let(_))

    def getConstructor: Option[NameKind] = {
      val cn = ConstructorName(item)
      prog.types.constructors.get((from.unfix.name, cn)).map { dtype =>
        val scheme = dtype.toScheme(cn).get // this should never throw
        Constructor(cn, dtype, scheme)
      }
    }

    def getImport: Option[NameKind] =
      from.unfix.localImport(item).map { case (originalPackage, i) =>
        Import(originalPackage, i.originalName)
      }

    def getExternal: Option[NameKind] =
      // there should not be duplicate top level names TODO lint for this
      prog.from.toStream.collectFirst {
        case Statement.ExternalDef(n, _, _, _) if n == item =>
          // The type could be an import, so we need to check for the type
          // in the TypeEnv
          val scheme = prog.types.values(n)
          val pn = from.unfix.name
          ExternalDef(pn, item, scheme)
      }

    getLet.orElse(
      getConstructor.orElse(getImport.orElse(getExternal))
    )
  }
}
