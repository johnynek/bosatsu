package org.bykn.bosatsu

import cats.data.NonEmptyList

import rankn.TypeEnv

/**
 * A Referant is something that can be exported or imported after resolving
 * Before resolving, imports and exports are just names.
 */
sealed abstract class Referant
object Referant {
  case class Value(scheme: rankn.Type) extends Referant
  case class DefinedT(dtype: rankn.DefinedType) extends Referant
  case class Constructor(name: ConstructorName, dtype: rankn.DefinedType, params: List[(ParamName, rankn.Type)], consValue: rankn.Type) extends Referant

  private def imported[A, B](imps: List[Import[A, NonEmptyList[Referant]]])(fn: PartialFunction[Referant, B]): Map[String, B] =
    imps.foldLeft(Map.empty[String, B]) { (m0, imp) =>
      m0 ++ Import.locals(imp)(fn)
    }

  def importedTypes[A](imps: List[Import[A, NonEmptyList[Referant]]]): Map[String, (PackageName, String)] =
    imported(imps) {
      case Referant.DefinedT(dt) => (dt.packageName, dt.name.asString)
    }

  /**
   * These are all the imported items that may be used in a match
   */
  def importedConsNames[A](imps: List[Import[A, NonEmptyList[Referant]]]): Map[String, (PackageName, ConstructorName)] =
    imported(imps) {
      case Referant.Constructor(cn, dt, _, _) => (dt.packageName, cn)
    }
  /**
   * There are all the imported values, including the constructor functions
   */
  def importedValues[A](imps: List[Import[A, NonEmptyList[Referant]]]): Map[String, rankn.Type] =
    imported(imps) {
      case Referant.Value(t) => t
      case Referant.Constructor(_, _, _, t) => t
    }
  /**
   * Fully qualified original names
   */
  def fullyQualifiedImportedValues[A](imps: List[Import[A, NonEmptyList[Referant]]])(nameOf: A => PackageName): Map[(PackageName, String), rankn.Type] =
    imps.iterator.flatMap { item =>
      val pn = nameOf(item.pack)
      item.items.toList.iterator.flatMap { i =>
        val orig = i.originalName
        val key = (pn, orig)
        i.tag.toList.iterator.collect {
          case Referant.Value(t) => (key, t)
          case Referant.Constructor(_, _, _, t) => (key, t)
        }
      }
    }
    .toMap

  def typeConstructors[A](imps: List[Import[A, NonEmptyList[Referant]]]): Map[(PackageName, ConstructorName), (List[rankn.Type.Var], List[rankn.Type], rankn.Type.Const.Defined)] = {
    val refs: Iterator[Referant] = imps.iterator.flatMap(_.items.toList.iterator.flatMap(_.tag.toList))
    refs.collect { case Constructor(cn, dt, params, _) =>
      ((dt.packageName, cn), (dt.typeParams, params.map(_._2), dt.toTypeConst))
    }
    .toMap
  }

  /**
   * Build the TypeEnv view of the given imports
   */
  def importedTypeEnv[A](inps: List[Import[A, NonEmptyList[Referant]]])(nameOf: A => PackageName): TypeEnv =
    inps.foldLeft(TypeEnv.empty) {
      case (te, imps) =>
        val pack = nameOf(imps.pack)
        imps.items.foldLeft(te) { (te, imp) =>
          val nm = imp.localName
          imp.tag.foldLeft(te) {
            case (te1, Referant.Value(t)) =>
              te1.addExternalValue(pack, nm, t)
            case (te1, Referant.Constructor(n, dt, params, v)) =>
              val nec = te1.constructors.updated((pack, n), (params, dt, v))
              te1.copy(constructors = nec)
            case (te1, Referant.DefinedT(dt)) =>
              te1.addDefinedType(dt)
          }
        }
    }
  // case class Value(scheme: rankn.Type) extends Referant
  // case class DefinedT(dtype: rankn.DefinedType) extends Referant
  // case class Constructor(name: ConstructorName, dtype: rankn.DefinedType, params: List[(ParamName, rankn.Type)], consValue: rankn.Type) extends Referant
}
