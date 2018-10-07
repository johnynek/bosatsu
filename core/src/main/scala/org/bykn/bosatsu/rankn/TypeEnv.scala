package org.bykn.bosatsu.rankn

import org.bykn.bosatsu.{ConstructorName, TypeName, PackageName, ParamName}

case class TypeEnv(
  values: Map[String, Type],
  constructors: Map[(PackageName, ConstructorName), (List[(ParamName, Type)], DefinedType)],
  definedTypes: Map[(PackageName, TypeName), DefinedType]) {


  def addDefinedType(dt: DefinedType): TypeEnv = {
    val dt1 = definedTypes.updated((dt.packageName, dt.name), dt)
    val cons1 =
      dt.constructors
        .foldLeft(constructors) {
          case (cons0, (cname, params)) =>
            cons0.updated((dt.packageName, cname), (params, dt))
        }
    copy(constructors = cons1, definedTypes = dt1)
  }

  /**
   * External values cannot be inferred and have to be fully
   * annotated
   */
  def addExternalValue(name: String, t: Type): TypeEnv =
    copy(values = values.updated(name, t))

  // TODO to support parameter named patterns we'd need to know the
  // parameter names
  def typeConstructors: Map[(PackageName, ConstructorName), (List[Type.Var], List[Type], Type.Const.Defined)] =
    constructors.map { case (pc, (params, dt)) =>
      (pc,
        (dt.typeParams,
          params.map(_._2),
          dt.toTypeConst))
    }

}

object TypeEnv {
  val empty: TypeEnv = TypeEnv(Map.empty, Map.empty, Map.empty)
}
