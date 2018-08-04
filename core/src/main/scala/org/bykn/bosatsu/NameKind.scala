package org.bykn.bosatsu

sealed abstract class NameKind[T]
object NameKind {
  case class Let[T](value: Expr[T]) extends NameKind[T]
  case class Constructor[T](cn: ConstructorName, dt: DefinedType, scheme: Scheme) extends NameKind[T]
  case class Import[T](fromPack: FixPackage[T], originalName: String) extends NameKind[T]
  case class ExternalDef[T](pack: PackageName, defName: String, defType: Scheme) extends NameKind[T]

  type FixPackage[T] = Package.FixPackage[Referant, Referant, Program[T, Statement]]

  def externals[T](from: FixPackage[T]): Stream[ExternalDef[T]] = {
    val prog = from.unfix.program
    prog.from.toStream.collect {
      case Statement.ExternalDef(n, _, _, _) =>
        // The type could be an import, so we need to check for the type
        // in the TypeEnv
        val scheme = prog.types.values(n)
        val pn = from.unfix.name
        ExternalDef[T](pn, n, scheme)
    }
  }

  def apply[T](from: FixPackage[T], item: String): Option[NameKind[T]] = {
    val prog = from.unfix.program

    def getLet: Option[NameKind[T]] =
      prog.getLet(item).map(Let(_))

    def getConstructor: Option[NameKind[T]] = {
      val cn = ConstructorName(item)
      prog.types.constructors.get((from.unfix.name, cn)).map { dtype =>
        val scheme = dtype.toScheme(cn).get // this should never throw
        Constructor[T](cn, dtype, scheme)
      }
    }

    def getImport: Option[NameKind[T]] =
      from.unfix.localImport(item).map { case (originalPackage, i) =>
        Import[T](originalPackage, i.originalName)
      }

    def getExternal: Option[NameKind[T]] =
      // there should not be duplicate top level names TODO lint for this
      prog.from.toStream.collectFirst {
        case Statement.ExternalDef(n, _, _, _) if n == item =>
          // The type could be an import, so we need to check for the type
          // in the TypeEnv
          val scheme = prog.types.values(n)
          val pn = from.unfix.name
          ExternalDef[T](pn, item, scheme)
      }

    getLet.orElse(
      getConstructor.orElse(getImport.orElse(getExternal))
    )
  }
}
