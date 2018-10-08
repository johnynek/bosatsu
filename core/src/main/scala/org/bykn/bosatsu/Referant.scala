package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.implicits._

/**
 * A Referant is something that can be exported or imported after resolving
 * Before resolving, imports and exports are just names.
 */
sealed abstract class Referant
object Referant {
  case class Value(scheme: rankn.Type) extends Referant
  case class DefinedT(dtype: rankn.DefinedType) extends Referant
  case class Constructor(name: ConstructorName, dtype: rankn.DefinedType, consValue: rankn.Type) extends Referant

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
  def importedCons[A](imps: List[Import[A, NonEmptyList[Referant]]]): Map[String, (PackageName, ConstructorName)] =
    imported(imps) {
      case Referant.Constructor(cn, dt, _) => (dt.packageName, cn)
    }
  /**
   * There are all the imported values, including the constructor functions
   */
  def importedValues[A](imps: List[Import[A, NonEmptyList[Referant]]]): Map[String, rankn.Type] =
    imported(imps) {
      case Referant.Value(t) => t
      case Referant.Constructor(cn, _, t) => t
    }
}
