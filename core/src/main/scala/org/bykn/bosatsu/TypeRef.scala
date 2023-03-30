package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.{Parser => P, Parser0}
import org.bykn.bosatsu.rankn.Type
import org.bykn.bosatsu.{TypeName => Name}
import org.typelevel.paiges.{Doc, Document}

import Parser.{lowerIdent, maybeSpace, Combinators}

/** This AST is the syntactic version of Type it is shaped slightly differently
  * to match the way the syntax looks (nested non empty lists are explicit
  * whereas we use a recursion/cons style in Type
  */
sealed abstract class TypeRef {
  import TypeRef._

  def toDoc: Doc =
    TypeRef.document.document(this)

  /** Nested TypeForAll can be combined, and should be generally
    */
  def normalizeForAll: TypeRef =
    this match {
      case TypeVar(_) | TypeName(_) => this
      case TypeArrow(a, b) => TypeArrow(a.normalizeForAll, b.normalizeForAll)
      case TypeApply(a, bs) =>
        TypeApply(a.normalizeForAll, bs.map(_.normalizeForAll))
      case TypeForAll(pars0, TypeForAll(pars1, e)) =>
        // we normalize to lifting all the foralls to the outside
        TypeForAll(pars0 ::: pars1, e).normalizeForAll
      case TypeForAll(pars, e) =>
        // Remove `Some(Type)` since that's the default
        TypeForAll(
          pars.map {
            case (v, Some(Kind.Type)) => (v, None)
            case other                => other
          },
          e.normalizeForAll
        )
      case TypeTuple(ts) =>
        TypeTuple(ts.map(_.normalizeForAll))
    }
}

object TypeRef {
  private val colonSpace = Doc.text(": ")

  def argDoc[A: Document](st: (A, Option[TypeRef])): Doc =
    st match {
      case (s, None)     => Document[A].document(s)
      case (s, Some(tr)) => Document[A].document(s) + colonSpace + (tr.toDoc)
    }

  case class TypeVar(asString: String) extends TypeRef {
    def toBoundVar: Type.Var.Bound = Type.Var.Bound(asString)
  }
  case class TypeName(name: Name) extends TypeRef
  case class TypeArrow(from: TypeRef, to: TypeRef) extends TypeRef
  case class TypeApply(of: TypeRef, args: NonEmptyList[TypeRef]) extends TypeRef

  case class TypeForAll(
      params: NonEmptyList[(TypeVar, Option[Kind])],
      in: TypeRef
  ) extends TypeRef
  case class TypeTuple(params: List[TypeRef]) extends TypeRef

  implicit val typeRefOrdering: Ordering[TypeRef] =
    new Ordering[TypeRef] {
      val list = ListOrdering.onType(this)
      val listKind = ListOrdering.onType(
        Ordering.Tuple2(this, implicitly[Ordering[Option[Kind]]])
      )

      def compare(a: TypeRef, b: TypeRef): Int =
        (a, b) match {
          case (TypeVar(v0), TypeVar(v1))   => v0.compareTo(v1)
          case (TypeVar(_), _)              => -1
          case (TypeName(v0), TypeName(v1)) => Ordering[Name].compare(v0, v1)
          case (TypeName(_), TypeVar(_))    => 1
          case (TypeName(_), _)             => -1
          case (TypeArrow(a0, b0), TypeArrow(a1, b1)) =>
            val c = compare(a0, a1)
            if (c == 0) compare(b0, b1) else c
          case (TypeArrow(_, _), TypeVar(_) | TypeName(_)) => 1
          case (TypeArrow(_, _), _)                        => -1
          case (TypeApply(o0, a0), TypeApply(o1, a1)) =>
            val c = compare(o0, o1)
            if (c != 0) c
            else list.compare(a0.toList, a1.toList)
          case (TypeApply(_, _), TypeVar(_) | TypeName(_) | TypeArrow(_, _)) =>
            1
          case (TypeApply(_, _), _)                       => -1
          case (TypeForAll(p0, in0), TypeForAll(p1, in1)) =>
            // TODO, we could normalize the parmeters here
            val c = listKind.compare(p0.toList, p1.toList)
            if (c == 0) compare(in0, in1) else c
          case (
                TypeForAll(_, _),
                TypeVar(_) | TypeName(_) | TypeArrow(_, _) | TypeApply(_, _)
              ) =>
            1
          case (TypeForAll(_, _), _)          => -1
          case (TypeTuple(t0), TypeTuple(t1)) => list.compare(t0, t1)
          case (TypeTuple(_), _)              => 1
        }
    }

  private object TypeRefParser extends TypeParser[TypeRef] {
    lazy val parseRoot = {
      val tvar = Parser.lowerIdent.map(TypeVar(_))
      val tname = Identifier.consParser.map { cn => TypeName(Name(cn)) }

      tvar.orElse(tname)
    }
    def makeFn(in: TypeRef, out: TypeRef) = TypeArrow(in, out)

    def applyTypes(cons: TypeRef, args: NonEmptyList[TypeRef]) =
      TypeApply(cons, args)
    def universal(vars: NonEmptyList[(String, Option[Kind])], in: TypeRef) =
      TypeForAll(vars.map { case (s, k) => (TypeVar(s), k) }, in)

    def makeTuple(items: List[TypeRef]) = TypeTuple(items)

    def unapplyRoot(a: TypeRef): Option[Doc] =
      a match {
        case TypeName(n) => Some(Document[Identifier].document(n.ident))
        case TypeVar(s)  => Some(Doc.text(s))
        case _           => None
      }

    def unapplyFn(a: TypeRef): Option[(TypeRef, TypeRef)] =
      a match {
        case TypeArrow(a, b) => Some((a, b))
        case _               => None
      }

    def unapplyUniversal(
        a: TypeRef
    ): Option[(List[(String, Option[Kind])], TypeRef)] =
      a match {
        case TypeForAll(vs, a) =>
          Some(((vs.map { case (v, k) => (v.asString, k) }).toList, a))
        case _ => None
      }

    def unapplyTypeApply(a: TypeRef): Option[(TypeRef, List[TypeRef])] =
      a match {
        case TypeApply(a, args) => Some((a, args.toList))
        case _                  => None
      }

    def unapplyTuple(a: TypeRef): Option[List[TypeRef]] =
      a match {
        case TypeTuple(as) => Some(as)
        case _             => None
      }
  }

  implicit def document: Document[TypeRef] = TypeRefParser.document
  def parser: P[TypeRef] = TypeRefParser.parser

  val annotationParser: P[TypeRef] =
    maybeSpace.with1.soft *> P.char(':') *> maybeSpace *> parser

  def docTypeArgs[A](targs: List[(TypeRef.TypeVar, A)])(aDoc: A => Doc): Doc =
    targs match {
      case Nil => Doc.empty
      case nonEmpty =>
        val params = nonEmpty.map { case (TypeRef.TypeVar(v), a) =>
          Doc.text(v) + aDoc(a)
        }
        Doc.char('[') + Doc.intercalate(Doc.text(", "), params) + Doc.char(']')
    }

  def typeParams[A](next: Parser0[A]): P[NonEmptyList[(TypeRef.TypeVar, A)]] =
    (lowerIdent ~ next).nonEmptyListSyntax.map { nel =>
      nel.map { case (s, a) => (TypeRef.TypeVar(s.intern), a) }
    }
}
