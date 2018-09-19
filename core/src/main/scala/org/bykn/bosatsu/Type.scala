package org.bykn.bosatsu

import cats.data.{NonEmptyList, State}
import cats.{Monad, Order}
import cats.implicits._

sealed abstract class Type {
  import Type._

  /**
   * What are the free variables in a type that are not bound
   * to a type lambda
   *
   * TODO: how exactly is this different from Substitutable[Type].typeVars(this)
   * I don't think it should be but a scalacheck would be good
   */
  def varsIn: List[Type.Var] = {
    def loop(ts: List[Type], bindings: Set[String], acc: List[Type.Var]): List[Type.Var] =
      ts match {
        case Nil => acc.reverse.distinct
        case Var(nm) :: rest if bindings(nm) =>
          loop(rest, bindings, acc)
        case (v@Var(_)) :: rest =>
          loop(rest, bindings, v :: acc)
        case Arrow(from, to) :: rest =>
          loop(from :: to :: rest, bindings, acc)
        case TypeApply(hk, arg) :: rest =>
          loop(hk :: arg :: rest, bindings, acc)
        case TypeLambda(param, expr) :: rest =>
          // this binding is only in scope in expr
          val inVars = loop(expr :: Nil, bindings + param, acc)
          loop(rest, bindings, inVars.reverse_:::(acc))
        case Declared(_, _) :: rest =>
          loop(rest, bindings, acc)
      }

    loop(this :: Nil, Set.empty, Nil)
  }
}
object Type {
  case class Arrow(from: Type, to: Type) extends Type
  case class Declared(packageName: PackageName, name: String) extends Type
  case class TypeApply(hk: Type, arg: Type) extends Type
  case class TypeLambda(param: String, in: Type) extends Type
  case class Var(name: String) extends Type

  private def predef(t: String): Type =
    Declared(PackageName(NonEmptyList.of("Bosatsu", "Predef")), t)

  val intT: Type = predef("Int")
  val boolT: Type = predef("Bool")
  val strT: Type = predef("String")

  def transformDeclared(in: Type)(fn: Declared => Declared): Type =
    in match {
      case Arrow(a, b) =>
        Arrow(transformDeclared(a)(fn), transformDeclared(b)(fn))
      case TypeApply(t, a) =>
        TypeApply(transformDeclared(t)(fn), transformDeclared(a)(fn))
      case TypeLambda(p, t) => TypeLambda(p, transformDeclared(t)(fn))
      case d@Declared(_, _) => fn(d)
      case v@Var(_) => v
    }

  @annotation.tailrec
  final def rootDeclared(t: Type): Option[Declared] =
    t match {
      case decl@Declared(_, _) => Some(decl)
      case TypeApply(left, _) => rootDeclared(left)
      case _ => None
    }

  def simplifyApply(t: Type): Type =
    t match {
      case TypeApply(TypeLambda(name, expr), arg) =>
        Substitutable.forType(Subst.pair(name, arg), expr)
      case TypeApply(fn, arg) =>
        TypeApply(simplifyApply(fn), simplifyApply(arg))
      case Arrow(a, b) =>
        Arrow(simplifyApply(a), simplifyApply(b))
      case TypeLambda(p, t) => TypeLambda(p, simplifyApply(t))
      case d@Declared(_, _) => d
      case v@Var(_) => v
    }

  /**
   * This reassigns lambda variables from a increasing
   */
  def normalize(tpe: Type): Type = {

    def iToC(i: Int): Char = ('a'.toInt + i).toChar
    @annotation.tailrec
    def idxToLetter(i: Int, acc: List[Char] = Nil): String =
      if (i < 26 && 0 <= i) (iToC(i) :: acc).mkString
      else {
        val rem = i % 26
        val next = i / 26
        idxToLetter(next, iToC(rem) :: acc)
      }

    type NormState = (Int, Map[String, String])
    val incCounter: State[NormState, Unit] =
      State.modify[NormState] {
        case (c, m) => (c + 1, m)
      }
    val getMap: State[NormState, Map[String, String]] =
      State.get[NormState].map(_._2)

    def setMap(m: Map[String, String]): State[NormState, Unit] =
      State.modify[NormState] { case (c, _) => (c, m) }

    def nextMapping[A](v: String, avoid: Set[String])(recurse: String => State[NormState, A]): State[NormState, A] = {

      lazy val next: State[NormState, String] =
        for {
          counterMap <- State.get[NormState]
          (counter, map) = counterMap
          newVar0 = idxToLetter(counter)
          newVar <- if (avoid(newVar0)) incCounter >> next else incCounter.as(newVar0)
        } yield newVar

      for {
        initMap <- getMap
        newVar <- next
        newMap = initMap.updated(v, newVar)
        _ <- setMap(newMap)
        a <- recurse(newVar)
        _ <- setMap(initMap)
      } yield a
    }


    def loop(t: Type): State[NormState, Type] =
      t match {
        case TypeLambda(p, expr) =>
          val freeVars = t.varsIn.iterator.map(_.name).toSet
          nextMapping(p, freeVars) { newVar =>
            loop(expr).map(TypeLambda(newVar, _))
          }
        case v@Var(nm) =>
          State.get[NormState]
            .map { case (_, replacements) =>
              replacements.get(nm).fold(v)(Var(_))
            }
        case Arrow(a, b) =>
          for {
            la <- loop(a)
            lb <- loop(b)
          } yield Arrow(la, lb)
        case TypeApply(t, a) =>
          for {
            lt <- loop(t)
            la <- loop(a)
          } yield TypeApply(lt, la)
        case d@Declared(_, _) => State.pure(d)
      }

    loop(tpe).run((0, Map.empty)).value._2
  }

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
          case (TypeApply(aa, ab), TypeApply(ba, bb)) =>
            val c = compare(aa, ba)
            if (c == 0) compare(ab, bb)
            else c
          case (TypeApply(_, _), TypeLambda(_, _)) => -1
          case (TypeApply(_, _), Var(_)) => -1
          case (TypeApply(_, _), _) => 1
          case (TypeLambda(pa, ta), TypeLambda(pb, tb)) =>
            val c = pa.compareTo(pb)
            if (c == 0) compare(ta, tb)
            else c
          case (TypeLambda(_, _), Var(_)) => -1
          case (TypeLambda(_, _), _) => 1
          case (Var(na), Var(nb)) => na.compareTo(nb)
          case (Var(_), _) => 1
        }
    }
}


case class Scheme(vars: List[String], result: Type) {
  import Type._

  lazy val toType: Type = {
    def loop(vars: List[String], expr: Type): Type =
      vars match {
        case Nil => expr
        case h :: tail => TypeLambda(h, loop(tail, expr))
      }
    loop(vars, result)
  }

  def normalized: Scheme =
    Scheme.fromType(Type.normalize(toType))
}

object Scheme {
  def fromType(t: Type): Scheme =
    t match {
      case Type.TypeLambda(h, rest) =>
        val Scheme(vars, expr) = fromType(rest)
        Scheme(h :: vars, expr)
      case notLambda =>
        Scheme(Nil, t)
    }

  def typeConstructor(t: Type): Scheme =
    Scheme(t.varsIn.map(_.name), t).normalized
}

case class ConstructorName(asString: String)

object ConstructorName {
  implicit val orderCN: Order[ConstructorName] = Order[String].contramap[ConstructorName](_.asString)
}

case class ParamName(asString: String)
case class TypeName(asString: String)



