package org.bykn.edgemar

import cats.data.NonEmptyList

sealed abstract class Type {
  import Type._

  def varsIn: List[Type.Var] =
    this match {
      case v@Var(_) => v :: Nil
      case _ =>
        // TODO actually more vars can be burried in other types.
        Nil
    }
}
object Type {
  case class Arrow(from: Type, to: Type) extends Type
  case class Declared(name: String) extends Type
  case class Primitive(name: String) extends Type
  case class TypeApply(hk: Type, arg: Type) extends Type
  case class TypeLambda(param: String, in: Type) extends Type
  case class Var(name: String) extends Type

  val intT: Type = Primitive("Int")
  val boolT: Type = Primitive("Bool")

  private[this] val prims = Set("Int", "Bool")
  def maybePrimitive(n: String): Type =
    if (prims(n)) Primitive(n)
    else Declared(n)
}


case class Scheme(vars: List[String], result: Type) {
  import Type._

  def normalized: Scheme = {

    @annotation.tailrec
    def inOrd(t: Type, toVisit: List[Type], acc: List[String]): List[String] =
      t match {
        case Arrow(a, b) => inOrd(a, b :: toVisit, acc)
        case Declared(_) | Primitive(_) =>
          toVisit match {
            case Nil => acc.reverse
            case h :: tail => inOrd(h, tail, acc)
          }
        case TypeApply(hk, arg) => inOrd(hk, arg :: toVisit, acc)
        case TypeLambda(_, t) => inOrd(t, toVisit, acc)
        case Var(v) => v :: Nil
          toVisit match {
            case Nil => (v :: acc).reverse
            case h :: tail => inOrd(h, tail, v :: acc)
          }
      }

    def idxToLetter(i: Int): String =
      if (i < 26 && 0 <= i) ('a'.toInt + i).toChar.toString
      else sys.error(s"too many type variables: $i") // TODO fix

    val inOrdDistinct = inOrd(result, Nil, Nil).distinct
    val mapping: List[(String, String)] =
      inOrdDistinct.zipWithIndex.map { case (i, idx) =>
        i -> idxToLetter(idx)
      }

    val mappingMap = mapping.toMap

    def norm(t: Type): Type =
      t match {
        case Arrow(a, b) => Arrow(norm(a), norm(b))
        case d@Declared(_) => d
        case c@Primitive(_) => c
        case TypeApply(hk, arg) => TypeApply(norm(hk), norm(arg))
        case TypeLambda(v, t) => TypeLambda(v, norm(t))
        case Var(v) => Var(mappingMap(v))
      }

    Scheme(mapping.map(_._2), norm(result))
  }
}

object Scheme {
  def fromType(t: Type): Scheme = Scheme(Nil, t)
}

case class ConstructorName(asString: String)
case class ParamName(asString: String)
case class TypeName(asString: String)

case class DefinedType(name: TypeName, typeParams: List[Type.Var], constructors: NonEmptyList[(ConstructorName, List[(ParamName, Type)])])

/**
 * This is a mapping of variable names to their Schemes
 */
case class TypeEnv(toMap: Map[String, Scheme], constructors: Map[ConstructorName, DefinedType]) {
  def updated(v: String, scheme: Scheme): TypeEnv =
    TypeEnv(toMap.updated(v, scheme), constructors)

  def addDefinedType(d: DefinedType): TypeEnv =
    d.constructors.toList.foldLeft(this) { case (te, (nm, _)) =>
      // TODO make sure this is not duplicated
      TypeEnv(te.toMap, te.constructors + (nm -> d))
    }
}

object TypeEnv {
  def empty: TypeEnv = TypeEnv(Map.empty, Map.empty)
}

