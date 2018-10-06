package org.bykn.bosatsu

/**
 * A Referant is something that can be exported or imported after resolving
 * Before resolving, imports and exports are just names.
 */
sealed abstract class Referant
object Referant {
  case class Value(scheme: Scheme) extends Referant
  case class DefinedT(dtype: DefinedType) extends Referant
  case class Constructor(name: ConstructorName, dtype: DefinedType) extends Referant
}

