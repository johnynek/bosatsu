package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.Order
import cats.implicits._

case class DefinedType(
  packageName: PackageName,
  name: TypeName,
  typeParams: List[Type.Var],
  constructors: List[(ConstructorName, List[(ParamName, Type)])]) {

  private[this] val consMap: Map[ConstructorName, List[(ParamName, Type)]] =
    constructors.toMap

  private def scheme(t: Type) = Scheme(typeParams.map(_.name), t)

  def consMap(subst: Subst): Map[ConstructorName, List[Type]] = {
    val newCons = Substitutable[List[(ConstructorName, List[(ParamName, Type)])]].apply(subst, constructors)

    newCons.toMap.mapValues(_.map(_._2))
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

    loop(typeParams)(Type.Declared(packageName, name.asString))
  }

  def toOpaque: DefinedType = copy(constructors = Nil)

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

  def checkTotality(matches: NonEmptyList[ConstructorName], matchRegion: Region): Either[TypeError, Unit] = {
    val expected = constructors.map(_._1)
    if (expected.toSet == matches.toList.toSet) Right(())
    else Left(TypeError.NonTotalMatch(matches, expected, matchRegion))
  }
}

object DefinedType {
  implicit val orderingDT: Order[DefinedType] =
    Order[(PackageName, String, List[String], List[(String, List[(String, Type)])])]
      .contramap[DefinedType] { case DefinedType(pn, TypeName(str), vars, cons) =>
        (pn, str, vars.map { case Type.Var(n) => n }, cons.map { case (ConstructorName(n), lst) =>
          (n, lst.map { case (ParamName(pn), t) => (pn, t) })
        })
      }

  implicit val forDefinedType: Substitutable[DefinedType] =
    new Substitutable[DefinedType] {
      def apply(sub: Subst, t: DefinedType) = {
        // all the names in typeParams are shadows so we need
        // to remove them:
        val newSubst = Subst(t.typeParams.map(_.name).foldLeft(sub.toMap)(_ - _))
        val newCons = Substitutable[List[(ConstructorName, List[(ParamName, Type)])]].apply(newSubst, t.constructors)
        t.copy(constructors = newCons)
      }

      def typeVars(d: DefinedType) = d.typeParams.iterator.map(_.name).toSet
    }
}
