package org.bykn.bosatsu.rankn

import org.bykn.bosatsu.{TypeName, PackageName}
import scala.collection.immutable.SortedMap

import org.bykn.bosatsu.Identifier
import org.bykn.bosatsu.Identifier.{Bindable, Constructor}

class TypeEnv[+A] private (
  protected val values: SortedMap[(PackageName, Identifier), Type],
  protected val constructors: SortedMap[(PackageName, Constructor), (List[(Bindable, Type)], DefinedType[A], Type)],
  val definedTypes: SortedMap[(PackageName, TypeName), DefinedType[A]]) {

  override def equals(that: Any): Boolean =
    that match {
      case te: TypeEnv[_] =>
        (values == te.values) &&
        (constructors == te.constructors) &&
        (definedTypes == te.definedTypes)
      case _ => false
    }

  lazy val referencedPackages: Set[PackageName] = {
    def keys1[K, B](k: SortedMap[(PackageName, K), B]): Iterator[PackageName] =
      k.keys.iterator.map(_._1)

    (keys1(values) ++ keys1(constructors) ++ keys1(definedTypes)).toSet
  }

  override def hashCode: Int = {
    (getClass, values, constructors, definedTypes).hashCode
  }

  override def toString: String =
    s"TypeEnv($values, $constructors, $definedTypes)"

  def allDefinedTypes: List[DefinedType[A]] =
    definedTypes.values.toList.sortBy { dt => (dt.packageName, dt.name) }

  def getConstructor(p: PackageName, c: Constructor): Option[(List[(Bindable, Type)], DefinedType[A], Type)] =
    constructors.get((p, c))

  def getType(p: PackageName, t: TypeName): Option[DefinedType[A]] =
    definedTypes.get((p, t))

  def getValue(p: PackageName, n: Identifier): Option[Type] =
    values.get((p, n))

  def localValuesOf(p: PackageName): SortedMap[Identifier, Type] =
    (SortedMap.newBuilder[Identifier, Type] ++= values.iterator.collect { case ((pn, n), v) if pn == p => (n, v) }).result

  def addConstructor[A1 >: A](pack: PackageName,
    dt: DefinedType[A1],
    cf: ConstructorFn): TypeEnv[A1] = {
      val nec = constructors.updated((pack, cf.name), (cf.args, dt, cf.fnType))
      // add this constructor to the values
      val v1 = values.updated((pack, cf.name), cf.fnType)
      new TypeEnv(values = v1, constructors = nec, definedTypes = definedTypes)
    }

  /**
   * only add the type, do not add any of the constructors
   * used when importing values
   */
  def addDefinedType[A1 >: A](dt: DefinedType[A1]): TypeEnv[A1] = {
    val dt1 = definedTypes.updated((dt.packageName, dt.name), dt)
    new TypeEnv(constructors = constructors, definedTypes = dt1, values = values)
  }

  /**
   * add a DefinedType and all of its constructors. This is done locally for
   * a package
   */
  def addDefinedTypeAndConstructors[A1 >: A](dt: DefinedType[A1]): TypeEnv[A1] = {
    val dt1 = definedTypes.updated((dt.packageName, dt.name), dt)
    val cons1 =
      dt.constructors
        .foldLeft(constructors: SortedMap[(PackageName, Constructor), (List[(Bindable, Type)], DefinedType[A1], Type)]) {
          case (cons0, cf) =>
            cons0.updated((dt.packageName, cf.name), (cf.args, dt, cf.fnType))
        }
    // here we have to actually add the constructor values:
    val v1 = dt.constructors.foldLeft(values) { case (v0, cf) =>
      v0.updated((dt.packageName, cf.name), cf.fnType)
    }

    new TypeEnv(constructors = cons1, definedTypes = dt1, values = v1)
  }

  /**
   * External values cannot be inferred and have to be fully
   * annotated
   */
  def addExternalValue(pack: PackageName, name: Identifier, t: Type): TypeEnv[A] =
    new TypeEnv(
      constructors = constructors,
      definedTypes = definedTypes,
      values = values.updated((pack, name), t))

  lazy val typeConstructors: SortedMap[(PackageName, Constructor), (List[(Type.Var, A)], List[Type], Type.Const.Defined)] =
    constructors.map { case (pc, (params, dt, _)) =>
      (pc,
        (dt.annotatedTypeParams,
          params.map(_._2),
          dt.toTypeConst))
    }

  def definedTypeFor(c: (PackageName, Constructor)): Option[DefinedType[A]] =
    typeConstructors.get(c).flatMap { case (_, _, d) => toDefinedType(d) }

  def toDefinedType(t: Type.Const): Option[DefinedType[A]] =
    t match {
      case Type.Const.Defined(p, n) => getType(p, n)
    }

  def ++[A1 >: A](that: TypeEnv[A1]): TypeEnv[A1] =
    new TypeEnv(values ++ that.values,
      constructors ++ that.constructors,
      definedTypes ++ that.definedTypes)
}

object TypeEnv {
  val empty: TypeEnv[Nothing] =
    new TypeEnv(
      SortedMap.empty[(PackageName, Identifier), Type],
      SortedMap.empty[(PackageName, Constructor), (List[(Bindable, Type)], DefinedType[Nothing], Type)],
      SortedMap.empty[(PackageName, TypeName), DefinedType[Nothing]])

  /**
   * Adds all the types and all the constructors from the given types
   */
  def fromDefinitions[A](defs: List[DefinedType[A]]): TypeEnv[A] =
    defs.foldLeft(empty: TypeEnv[A])(_.addDefinedTypeAndConstructors(_))

  def fromParsed[A](p: ParsedTypeEnv[A]): TypeEnv[A] = {
    val t1 = p.allDefinedTypes.foldLeft(empty: TypeEnv[A])(_.addDefinedTypeAndConstructors(_))
    p.externalDefs.foldLeft(t1) { case (t1, (p, n, t)) => t1.addExternalValue(p, n, t) }
  }
}
