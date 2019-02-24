package org.bykn.bosatsu.rankn

import org.bykn.bosatsu.{ConstructorName, TypeName, PackageName, ParamName}

case class TypeEnv[+A](
  values: Map[(PackageName, String), Type],
  constructors: Map[(PackageName, ConstructorName), (List[(ParamName, Type)], DefinedType[A], Type)],
  definedTypes: Map[(PackageName, TypeName), DefinedType[A]]) {

  def localValuesOf(p: PackageName): Map[String, Type] =
    values.iterator.collect { case ((pn, n), v) if pn == p => (n, v) }.toMap

  def addDefinedType[A1 >: A](dt: DefinedType[A1]): TypeEnv[A1] = {
    val dt1 = definedTypes.updated((dt.packageName, dt.name), dt)
    val cons1 =
      dt.constructors
        .foldLeft(constructors: Map[(PackageName, ConstructorName), (List[(ParamName, Type)], DefinedType[A1], Type)]) {
          case (cons0, (cname, params, vtpe)) =>
            cons0.updated((dt.packageName, cname), (params, dt, vtpe))
        }
    // here we have to actually add the constructor values:
    val v1 = dt.constructors.foldLeft(values) { case (v0, (cn, _, tpe)) =>
      v0.updated((dt.packageName, cn.asString), tpe)
    }

    copy(constructors = cons1, definedTypes = dt1, values = v1)
  }

  /**
   * External values cannot be inferred and have to be fully
   * annotated
   */
  def addExternalValue(pack: PackageName, name: String, t: Type): TypeEnv[A] =
    copy(values = values.updated((pack, name), t))

  // TODO to support parameter named patterns we'd need to know the
  // parameter names
  lazy val typeConstructors: Map[(PackageName, ConstructorName), (List[Type.Var], List[Type], Type.Const.Defined)] =
    constructors.map { case (pc, (params, dt, _)) =>
      (pc,
        (dt.typeParams,
          params.map(_._2),
          dt.toTypeConst))
    }

  def definedTypeFor(c: (PackageName, ConstructorName)): Option[DefinedType[A]] =
    typeConstructors.get(c).flatMap { case (_, _, d) => toDefinedType(d) }

  def toDefinedType(t: Type.Const.Defined): Option[DefinedType[A]] =
    definedTypes.get((t.packageName, TypeName(t.name)))

  def ++[A1 >: A](that: TypeEnv[A1]): TypeEnv[A1] =
    TypeEnv(values ++ that.values,
      constructors ++ that.constructors,
      definedTypes ++ that.definedTypes)
}

object TypeEnv {
  val empty: TypeEnv[Nothing] = TypeEnv(Map.empty, Map.empty, Map.empty)
}
