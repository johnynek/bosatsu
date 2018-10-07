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
  case class Constructor(name: ConstructorName, dtype: rankn.DefinedType) extends Referant

  def importedTypes[A](imps: List[Import[A, NonEmptyList[Referant]]]): Map[String, (PackageName, String)] =
    imps.foldLeft(Map.empty[String, (PackageName, String)]) { (m0, imp) =>
      m0 ++ Import.locals(imp) {
        case Referant.DefinedT(dt) => (dt.packageName, dt.name.asString)
      }
    }

  def importedCons[A](imps: List[Import[A, NonEmptyList[Referant]]]): Map[String, (PackageName, ConstructorName)] =
    imps.foldLeft(Map.empty[String, (PackageName, ConstructorName)]) { (m0, imp) =>
      m0 ++ Import.locals(imp) {
        case Referant.Constructor(cn, dt) => (dt.packageName, cn)
      }
    }
}
