package org.bykn.edgemar

import cats.data.NonEmptyList
import cats.Order
import cats.implicits._

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
  //case class TypeLambda(param: String, in: Type) extends Type
  case class Var(name: String) extends Type

  val intT: Type = Primitive("Int")
  val boolT: Type = Primitive("Bool")

  private[this] val prims = Set("Int", "Bool")
  def maybePrimitive(n: String): Type =
    if (prims(n)) Primitive(n)
    else Declared(n)

  implicit val ordType: Order[Type] =
    new Order[Type] {
      def compare(a: Type, b: Type): Int =
        (a, b) match {
          case (Arrow(aa, ab), Arrow(ba, bb)) =>
            val c = compare(aa, ba)
            if (c == 0) compare(ab, bb)
            else c
          case (Arrow(_, _), _) => -1 // Arrow befor all other
          case (Declared(na), Declared(nb)) => na.compareTo(nb)
          case (Declared(_), Arrow(_, _)) => 1 // we are after Arrow
          case (Declared(_), _) => -1 // before everything else
          case (Primitive(na), Primitive(nb)) => na.compareTo(nb)
          case (Primitive(_), Arrow(_, _)) => 1
          case (Primitive(_), Declared(_)) => 1
          case (Primitive(_), _) => -1
          case (TypeApply(aa, ab), TypeApply(ba, bb)) =>
            val c = compare(aa, ba)
            if (c == 0) compare(ab, bb)
            else c
          //case (TypeApply(_, _), TypeLambda(_, _)) => -1
          case (TypeApply(_, _), Var(_)) => -1
          case (TypeApply(_, _), _) => 1
          // case (TypeLambda(pa, ta), TypeLambda(pb, tb)) =>
          //   val c = pa.compareTo(pb)
          //   if (c == 0) compare(ta, tb)
          //   else c
          // case (TypeLambda(_, _), Var(_)) => -1
          // case (TypeLambda(_, _), _) => 1
          case (Var(na), Var(nb)) => na.compareTo(nb)
          case (Var(_), _) => 1
        }
    }
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
        //case TypeLambda(_, t) => inOrd(t, toVisit, acc)
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
        //case TypeLambda(v, t) => TypeLambda(v, norm(t))
        case Var(v) => Var(mappingMap(v))
      }

    Scheme(mapping.map(_._2), norm(result))
  }
}

object Scheme {
  def fromType(t: Type): Scheme = Scheme(Nil, t)
}

case class ConstructorName(asString: String)

object ConstructorName {
  implicit val orderCN: Order[ConstructorName] = Order[String].contramap[ConstructorName](_.asString)
}

case class ParamName(asString: String)
case class TypeName(asString: String)

case class DefinedType(name: TypeName, typeParams: List[Type.Var], constructors: NonEmptyList[(ConstructorName, List[(ParamName, Type)])]) {
  private[this] val consMap: Map[ConstructorName, List[(ParamName, Type)]] =
    constructors.toList.toMap

  private def scheme(t: Type) = Scheme(typeParams.map(_.name), t)


  def consMap(subst: Subst): Map[ConstructorName, List[Type]] = {
    val newCons = Substitutable[NonEmptyList[(ConstructorName, List[(ParamName, Type)])]].apply(subst, constructors)

    newCons.toList.toMap.mapValues(_.map(_._2))
  }

  def fullyApplied(subst: Subst): Type = {
    def loop(ts: List[Type.Var]): Type => Type =
      ts match {
        case Nil => identity
        case h :: tail =>
          val newVar = Substitutable[Type].apply(subst, h)
          val tailFn = loop(tail)

          { t0: Type => tailFn(Type.TypeApply(t0, newVar)) }
      }

    loop(typeParams)(Type.Declared(name.asString))
  }

  def typeScheme: Scheme = scheme(fullyApplied(Subst.empty))

  def toScheme(n: ConstructorName): Option[Scheme] = {
    def loop(fn: List[Type]): Type =
      fn match {
        case Nil => fullyApplied(Subst.empty)
        case h :: tail => Type.Arrow(h, loop(tail))
      }

    // a constructor is either a constant value (no params) or a function
    consMap.get(n).map { pts =>
      scheme(loop(pts.map(_._2)))
    }
  }

  def checkTotality(matches: NonEmptyList[ConstructorName]): Either[TypeError, Unit] = {
    val expected = constructors.map(_._1)
    if (expected.toList.toSet == matches.toList.toSet) Right(())
    else Left(TypeError.NonTotalMatch(matches, expected))
  }
}

object DefinedType {
  implicit val orderingDT: Order[DefinedType] =
    Order[(String, List[String], NonEmptyList[(String, List[(String, Type)])])]
      .contramap[DefinedType] { case DefinedType(TypeName(str), vars, cons) =>
        (str, vars.map { case Type.Var(n) => n }, cons.map { case (ConstructorName(n), lst) =>
          (n, lst.map { case (ParamName(pn), t) => (pn, t) })
        })
      }
}

/**
 * This is a mapping of variable names to their Schemes
 */
case class TypeEnv(toMap: Map[String, Scheme], constructors: Map[ConstructorName, DefinedType]) {
  def schemeOf(name: String): Option[Scheme] =
    toMap.get(name)
      .orElse {
        val cons = ConstructorName(name)
        for {
          dt <- constructors.get(cons)
          scheme <- dt.toScheme(cons)
        } yield scheme
      }

  def updated(v: String, scheme: Scheme): TypeEnv =
    TypeEnv(toMap.updated(v, scheme), constructors)

  def addDefinedType(d: DefinedType): TypeEnv =
    d.constructors.toList.foldLeft(this) { case (te, (nm, _)) =>
      // TODO make sure this is not duplicated
      TypeEnv(te.toMap, te.constructors + (nm -> d))
    }

  def getDefinedType(matches: NonEmptyList[ConstructorName]): Either[TypeError, DefinedType] = {
    def dtOrCn(c: ConstructorName): Either[ConstructorName, DefinedType] =
      constructors.get(c).fold(Left(c): Either[ConstructorName, DefinedType])(Right(_))

    matches.map(dtOrCn _).distinct match {
      case NonEmptyList(Right(dt), tail) =>
        tail match {
          case Nil => dt.checkTotality(matches).map(_ => dt)
          case _ :: _ =>
            Left(TypeError.TypeConstructorCollision(matches, this))
        }
      case NonEmptyList(Left(cn), _) => Left(TypeError.UnknownConstuctor(cn))
    }
  }
}

object TypeEnv {
  def empty: TypeEnv = TypeEnv(Map.empty, Map.empty)
}

