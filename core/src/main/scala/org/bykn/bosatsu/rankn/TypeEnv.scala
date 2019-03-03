package org.bykn.bosatsu.rankn

import org.bykn.bosatsu.{ConstructorName, TypeName, PackageName, ParamName}
import scala.collection.immutable.SortedMap

class TypeEnv[+A] private (
  protected val values: SortedMap[(PackageName, String), Type],
  protected val constructors: SortedMap[(PackageName, ConstructorName), (List[(ParamName, Type)], DefinedType[A], Type)],
  protected val definedTypes: SortedMap[(PackageName, TypeName), DefinedType[A]]) {

  override def equals(that: Any): Boolean =
    that match {
      case te: TypeEnv[_] =>
        (values == te.values) &&
        (constructors == te.constructors) &&
        (definedTypes == te.definedTypes)
      case _ => false
    }

  override def hashCode: Int = {
    (getClass, values, constructors, definedTypes).hashCode
  }

  def allDefinedTypes: List[DefinedType[A]] =
    definedTypes.values.toList.sortBy { dt => (dt.packageName, dt.name) }

  def getConstructor(p: PackageName, c: ConstructorName): Option[(List[(ParamName, Type)], DefinedType[A], Type)] =
    constructors.get((p, c))

  def getType(p: PackageName, t: TypeName): Option[DefinedType[A]] =
    definedTypes.get((p, t))

  def getValue(p: PackageName, n: String): Option[Type] =
    values.get((p, n))

  def localValuesOf(p: PackageName): SortedMap[String, Type] =
    (SortedMap.newBuilder[String, Type] ++= values.iterator.collect { case ((pn, n), v) if pn == p => (n, v) }).result

  def addConstructor[A1 >: A](pack: PackageName,
    n: ConstructorName,
    params: List[(ParamName, Type)],
    dt: DefinedType[A1],
    v: Type): TypeEnv[A1] = {
      val nec = constructors.updated((pack, n), (params, dt, v))
      new TypeEnv(values = values, constructors = nec, definedTypes = definedTypes)
    }

  def addDefinedType[A1 >: A](dt: DefinedType[A1]): TypeEnv[A1] = {
    val dt1 = definedTypes.updated((dt.packageName, dt.name), dt)
    val cons1 =
      dt.constructors
        .foldLeft(constructors: SortedMap[(PackageName, ConstructorName), (List[(ParamName, Type)], DefinedType[A1], Type)]) {
          case (cons0, (cname, params, vtpe)) =>
            cons0.updated((dt.packageName, cname), (params, dt, vtpe))
        }
    // here we have to actually add the constructor values:
    val v1 = dt.constructors.foldLeft(values) { case (v0, (cn, _, tpe)) =>
      v0.updated((dt.packageName, cn.asString), tpe)
    }

    new TypeEnv(constructors = cons1, definedTypes = dt1, values = v1)
  }

  /**
   * External values cannot be inferred and have to be fully
   * annotated
   */
  def addExternalValue(pack: PackageName, name: String, t: Type): TypeEnv[A] =
    new TypeEnv(constructors = constructors, definedTypes = definedTypes, values = values.updated((pack, name), t))

  // TODO to support parameter named patterns we'd need to know the
  // parameter names
  lazy val typeConstructors: SortedMap[(PackageName, ConstructorName), (List[Type.Var], List[Type], Type.Const.Defined)] =
    constructors.map { case (pc, (params, dt, _)) =>
      (pc,
        (dt.typeParams,
          params.map(_._2),
          dt.toTypeConst))
    }

  def definedTypeFor(c: (PackageName, ConstructorName)): Option[DefinedType[A]] =
    typeConstructors.get(c).flatMap { case (_, _, d) => toDefinedType(d) }

  def toDefinedType(t: Type.Const.Defined): Option[DefinedType[A]] =
    getType(t.packageName, TypeName(t.name))

  def ++[A1 >: A](that: TypeEnv[A1]): TypeEnv[A1] =
    new TypeEnv(values ++ that.values,
      constructors ++ that.constructors,
      definedTypes ++ that.definedTypes)
}

object TypeEnv {
  val empty: TypeEnv[Nothing] =
    new TypeEnv(
      SortedMap.empty[(PackageName, String), Type],
      SortedMap.empty[(PackageName, ConstructorName), (List[(ParamName, Type)], DefinedType[Nothing], Type)],
      SortedMap.empty[(PackageName, TypeName), DefinedType[Nothing]])

  def fromDefinitions[A](defs: List[DefinedType[A]]): TypeEnv[A] =
    defs.foldLeft(empty: TypeEnv[A])(_.addDefinedType(_))

  def fromParsed[A](p: ParsedTypeEnv[A]): TypeEnv[A] = {
    val t1 = p.allDefinedTypes.foldLeft(empty: TypeEnv[A])(_.addDefinedType(_))
    p.externalDefs.foldLeft(t1) { case (t1, (p, n, t)) => t1.addExternalValue(p, n, t) }
  }
}
