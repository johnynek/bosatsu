package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.{Parser => P}
import org.bykn.bosatsu.rankn.Type
import org.bykn.bosatsu.{TypeName => Name}
import org.typelevel.paiges.{ Doc, Document }

import Parser.maybeSpace

/**
 * This AST is the syntactic version of Type
 * it is shaped slightly differently to match the way
 * the syntax looks (nested non empty lists are explicit
 * whereas we use a recursion/cons style in Type
 */
sealed abstract class TypeRef {
  import TypeRef._

  def toDoc: Doc =
    TypeRef.document.document(this)

  /**
   * Nested TypeLambda can be combined, and should be generally
   */
  def normalizeForAll: TypeRef =
    this match {
      case TypeVar(_) | TypeName(_) => this
      case TypeArrow(a, b) => TypeArrow(a.normalizeForAll, b.normalizeForAll)
      case TypeApply(a, bs) =>
        TypeApply(a.normalizeForAll, bs.map(_.normalizeForAll))
      case TypeLambda(pars0, TypeLambda(pars1, e)) =>
        // we normalize to lifting all the foralls to the outside
        TypeLambda(pars0 ::: pars1, e).normalizeForAll
      case TypeLambda(pars, e) =>
        TypeLambda(pars, e.normalizeForAll)
      case TypeTuple(ts) =>
        TypeTuple(ts.map(_.normalizeForAll))
    }
}

object TypeRef {
  private val colonSpace = Doc.text(": ")


  def argDoc[A: Document](st: (A, Option[TypeRef])): Doc =
    st match {
      case (s, None) => Document[A].document(s)
      case (s, Some(tr)) => Document[A].document(s) + colonSpace + (tr.toDoc)
    }

  case class TypeVar(asString: String) extends TypeRef {
    def toBoundVar: Type.Var.Bound = Type.Var.Bound(asString)
  }
  case class TypeName(name: Name) extends TypeRef
  case class TypeArrow(from: TypeRef, to: TypeRef) extends TypeRef
  case class TypeApply(of: TypeRef, args: NonEmptyList[TypeRef]) extends TypeRef

  case class TypeLambda(params: NonEmptyList[TypeVar], in: TypeRef) extends TypeRef
  case class TypeTuple(params: List[TypeRef]) extends TypeRef

  implicit val typeRefOrdering: Ordering[TypeRef] =
    new Ordering[TypeRef] {
      val list = ListOrdering.onType(this)

      def compare(a: TypeRef, b: TypeRef): Int =
        (a, b) match {
          case (TypeVar(v0), TypeVar(v1)) => v0.compareTo(v1)
          case (TypeVar(_), _) => -1
          case (TypeName(v0), TypeName(v1)) => Ordering[Name].compare(v0, v1)
          case (TypeName(_), TypeVar(_)) => 1
          case (TypeName(_), _) => -1
          case (TypeArrow(a0, b0), TypeArrow(a1, b1)) =>
            val c = compare(a0, a1)
            if (c == 0) compare(b0, b1) else c
          case (TypeArrow(_, _), TypeVar(_) | TypeName(_)) => 1
          case (TypeArrow(_, _), _) => -1
          case (TypeApply(o0, a0), TypeApply(o1, a1)) =>
            val c = compare(o0, o1)
            if (c != 0) c
            else list.compare(a0.toList, a1.toList)
          case (TypeApply(_, _), TypeVar(_) | TypeName(_) | TypeArrow(_, _)) => 1
          case (TypeApply(_, _), _) => -1
          case (TypeLambda(p0, in0), TypeLambda(p1, in1)) =>
            // TODO, we could normalize the parmeters here
            val c = list.compare(p0.toList, p1.toList)
            if (c == 0) compare(in0, in1) else c
          case (TypeLambda(_, _), TypeVar(_) | TypeName(_) | TypeArrow(_, _) | TypeApply(_, _)) => 1
          case (TypeLambda(_, _), _) => -1
          case (TypeTuple(t0), TypeTuple(t1)) => list.compare(t0, t1)
          case (TypeTuple(_), _) => 1
        }
    }

  private object TypeRefParser extends TypeParser[TypeRef] {
    lazy val parseRoot = {
      val tvar = Parser.lowerIdent.map(TypeVar(_))
      val tname = Identifier.consParser.map { cn => TypeName(Name(cn)) }

      tvar.orElse(tname)
    }
    def makeFn(in: TypeRef, out: TypeRef) = TypeArrow(in, out)

    def applyTypes(cons: TypeRef, args: NonEmptyList[TypeRef]) = TypeApply(cons, args)
    def universal(vars: NonEmptyList[String], in: TypeRef) =
      TypeLambda(vars.map(TypeVar(_)), in)

    def makeTuple(items: List[TypeRef]) = TypeTuple(items)

    def unapplyRoot(a: TypeRef): Option[Doc] =
      a match {
        case TypeName(n) => Some(Document[Identifier].document(n.ident))
        case TypeVar(s) => Some(Doc.text(s))
        case _ => None
      }

    def unapplyFn(a: TypeRef): Option[(TypeRef, TypeRef)] =
      a match {
        case TypeArrow(a, b) => Some((a, b))
        case _ => None
      }

    def unapplyUniversal(a: TypeRef): Option[(List[String], TypeRef)] =
      a match {
        case TypeLambda(vs, a) => Some((vs.map(_.asString).toList, a))
        case _ => None
      }

    def unapplyTypeApply(a: TypeRef): Option[(TypeRef, List[TypeRef])] =
      a match {
        case TypeApply(a, args) => Some((a, args.toList))
        case _ => None
      }

    def unapplyTuple(a: TypeRef): Option[List[TypeRef]] =
      a match {
        case TypeTuple(as) => Some(as)
        case _ => None
      }
  }

  implicit def document: Document[TypeRef] = TypeRefParser.document
  def parser: P[TypeRef] = TypeRefParser.parser

  val annotationParser: P[TypeRef] =
    maybeSpace.with1.soft *> P.char(':') *> maybeSpace *> parser
}

