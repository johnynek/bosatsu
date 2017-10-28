package org.bykn.edgemar

sealed abstract class Type
object Type {
  case class Arrow(from: Type, to: Type) extends Type
  case class Declared(name: String) extends Type
  case class Primitive(name: String) extends Type
  case class TypeApply(hk: Type, arg: Type) extends Type
  case class TypeLambda(param: String, in: Type) extends Type
  case class Var(name: String) extends Type

  val intT: Type = Primitive("Int")
  val boolT: Type = Primitive("Bool")
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

/**
 * This is a mapping a variable names to their Schemes
 */
case class TypeEnv(toMap: Map[String, Scheme]) {
  def updated(v: String, scheme: Scheme): TypeEnv =
    TypeEnv(toMap.updated(v, scheme))
}

object TypeEnv {
  def empty: TypeEnv = TypeEnv(Map.empty)
}

