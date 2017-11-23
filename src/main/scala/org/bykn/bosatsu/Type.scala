package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.{Functor, Order}
import cats.implicits._

sealed abstract class Type {
  import Type._

  def varsIn: List[Type.Var] =
    this match {
      case v@Var(_) => v :: Nil
      case Arrow(from, to) =>
        (from.varsIn ::: to.varsIn).distinct
      case TypeApply(hk, arg) =>
        (hk.varsIn ::: arg.varsIn).distinct
      case _ =>
        Nil
    }
}
object Type {
  case class Arrow(from: Type, to: Type) extends Type
  case class Declared(packageName: PackageName, name: String) extends Type
  case class Primitive(name: String) extends Type
  case class TypeApply(hk: Type, arg: Type) extends Type
  //case class TypeLambda(param: String, in: Type) extends Type
  case class Var(name: String) extends Type

  val intT: Type = Primitive("Int")
  val boolT: Type = Primitive("Bool")

  def transformDeclared(in: Type)(fn: Declared => Declared): Type =
    in match {
      case Arrow(a, b) =>
        Arrow(transformDeclared(a)(fn), transformDeclared(b)(fn))
      case TypeApply(t, a) =>
        TypeApply(transformDeclared(t)(fn), transformDeclared(a)(fn))
      case d@Declared(_, _) => fn(d)
      case p@Primitive(_) => p
      case v@Var(_) => v
    }

  @annotation.tailrec
  final def rootDeclared(t: Type): Option[Declared] =
    t match {
      case decl@Declared(_, _) => Some(decl)
      case TypeApply(left, _) => rootDeclared(left)
      case _ => None
    }

  private[this] val prims = Set("Int", "Bool")
  def maybePrimitive(p: PackageName, n: String): Type =
    if (prims(n)) Primitive(n) // this is not really right in a package world
    else Declared(p, n)

  implicit val ordType: Order[Type] =
    new Order[Type] {
      def compare(a: Type, b: Type): Int =
        (a, b) match {
          case (Arrow(aa, ab), Arrow(ba, bb)) =>
            val c = compare(aa, ba)
            if (c == 0) compare(ab, bb)
            else c
          case (Arrow(_, _), _) => -1 // Arrow befor all other
          case (Declared(pa, na), Declared(pb, nb)) =>
            val c = Order[PackageName].compare(pa, pb)
            if (c == 0) na.compareTo(nb)
            else c
          case (Declared(_, _), Arrow(_, _)) => 1 // we are after Arrow
          case (Declared(_, _), _) => -1 // before everything else
          case (Primitive(na), Primitive(nb)) => na.compareTo(nb)
          case (Primitive(_), Arrow(_, _)) => 1
          case (Primitive(_), Declared(_, _)) => 1
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
        case Declared(_, _) | Primitive(_) =>
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

    def iToC(i: Int): Char = ('a'.toInt + i).toChar
    @annotation.tailrec
    def idxToLetter(i: Int, acc: List[Char] = Nil): String =
      if (i < 26 && 0 <= i) (iToC(i) :: acc).mkString
      else {
        val rem = i % 26
        val next = i / 26
        idxToLetter(next, iToC(rem) :: acc)
      }

    val inOrdDistinct = inOrd(result, Nil, Nil).distinct
    val mapping: List[(String, String)] =
      inOrdDistinct.zipWithIndex.map { case (i, idx) =>
        i -> idxToLetter(idx)
      }

    val mappingMap = mapping.toMap

    def norm(t: Type): Type =
      t match {
        case Arrow(a, b) => Arrow(norm(a), norm(b))
        case d@Declared(_, _) => d
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

  def typeConstructor(t: Type): Scheme =
    Scheme(t.varsIn.map(_.name), t).normalized
}

case class ConstructorName(asString: String)

object ConstructorName {
  implicit val orderCN: Order[ConstructorName] = Order[String].contramap[ConstructorName](_.asString)
}

case class ParamName(asString: String)
case class TypeName(asString: String)

case class DefinedType(packageName: PackageName, name: TypeName, typeParams: List[Type.Var], constructors: List[(ConstructorName, List[(ParamName, Type)])]) {
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

  def checkTotality(matches: NonEmptyList[ConstructorName]): Either[TypeError, Unit] = {
    val expected = constructors.map(_._1)
    if (expected.toSet == matches.toList.toSet) Right(())
    else Left(TypeError.NonTotalMatch(matches, expected))
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
}

/**
 * This is a mapping of variable names to their Schemes
 */
case class TypeEnv(
  toMap: Map[String, Scheme],
  constructors: Map[ConstructorName, DefinedType],
  definedTypes: Map[(PackageName, TypeName), DefinedType],
  imported: Map[String, Either[(ConstructorName, DefinedType), DefinedType]]
  ) {

  def schemeOf(name: String): Option[Scheme] =
    toMap.get(name)
      .orElse {
        val cons = ConstructorName(name)
        for {
          dt <- constructors.get(cons)
          scheme <- dt.toScheme(cons)
        } yield scheme
      }
      .orElse {
        // this could be an imported constructor
        imported.get(name).flatMap {
          case Left((cons, dt)) => dt.toScheme(cons)
          case Right(_) => None
        }
      }

  def updated(v: String, scheme: Scheme): TypeEnv =
    copy(toMap = toMap.updated(v, scheme))

  def addDefinedType(d: DefinedType): TypeEnv =
    d.constructors.toList.foldLeft(this) { case (te, (nm, _)) =>
      // TODO make sure this is not duplicated
      te.copy(constructors = te.constructors + (nm -> d))
    }.copy(definedTypes = definedTypes + ((d.packageName, d.name) -> d))

  def addImportedConstructor(local: String, remote: ConstructorName, in: DefinedType): TypeEnv =
    copy(imported = imported + (local -> Left((remote, in))))

  def addImportedType(packageName: PackageName, local: String, dt: DefinedType): TypeEnv = {
    /**
     * When we add an imported type, some of our defined types may refer to it
     */
    def translate(dec: Type.Declared): Type.Declared =
      if (dec.packageName === packageName && dec.name === local) {
        Type.Declared(dt.packageName, dt.name.asString)
      }
      else dec

    val functor = Functor[List]
      .compose(Functor[(ConstructorName, ?)])
      .compose(Functor[List])
      .compose(Functor[(ParamName, ?)])

    def fixDT(dt: DefinedType): DefinedType = {
      val fixedCons = functor.map(dt.constructors)(Type.transformDeclared(_)(translate _))
      dt.copy(constructors = fixedCons)
    }

    copy(
      imported = imported + (local -> Right(dt)),
      constructors = constructors.map { case (c, d) => c -> fixDT(d) }.toMap,
      definedTypes = definedTypes.map { case (k, d) => k -> fixDT(d) }.toMap
      )
      .addDefinedType(dt)
  }

  def getDefinedType(matches: NonEmptyList[ConstructorName]): Either[TypeError, DefinedType] = {
    def getCons(c: ConstructorName): Option[Either[(ConstructorName, DefinedType), DefinedType]] =
      constructors.get(c) match {
        case Some(dt) => Some(Right(dt))
        case None =>
          imported.get(c.asString) match {
            case Some(Left(c)) => Some(Left(c))
            case _ => None
          }
      }

    // Find all the constructor, dt pairs
    val matches1 = matches.traverse { cn =>
      getCons(cn) match {
        case None => Left(TypeError.UnknownConstuctor(cn))
        case Some(Right(dt)) => Right((cn, dt))
        case Some(Left(pair)) => Right(pair)
      }
    }

    matches1.flatMap { nePairs =>
      val distinctMatch = nePairs.distinct
      val dts = distinctMatch.map(_._2).distinct
      dts match {
        case NonEmptyList(dt, Nil) =>
          // this is the good case
          dt.checkTotality(distinctMatch.map(_._1)).map(_ => dt)
        case _ =>
          // we have at least 2 DefinedTypes
          Left(TypeError.TypeConstructorCollision(matches, this))
      }
    }
  }
}

object TypeEnv {
  val empty: TypeEnv = TypeEnv(Map.empty, Map.empty, Map.empty, Map.empty)
}

