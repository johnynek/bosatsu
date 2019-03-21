package org.bykn.bosatsu

import Parser.{ Combinators, lowerIdent, upperIdent, maybeSpace }
import cats.Applicative
import cats.data.NonEmptyList
import cats.implicits._
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }
import org.bykn.bosatsu.rankn.Type

/**
 * This AST is the syntactic version of Type
 * it is shaped slightly differently to match the way
 * the syntax looks (nested non empty lists are explicit
 * whereas we use a recursion/cons style in Type
 */
sealed abstract class TypeRef {
  import TypeRef._

  def toDoc: Doc =
    this match {
      case TypeVar(v) => Doc.text(v)
      case TypeName(n) => Doc.text(n)
      case TypeArrow(inner@TypeArrow(_, _), right) =>
        Doc.char('(') + inner.toDoc + Doc.char(')') + spaceArrow + right.toDoc
      case TypeArrow(inner@TypeLambda(_, _), right) =>
        Doc.char('(') + inner.toDoc + Doc.char(')') + spaceArrow + right.toDoc
      case TypeArrow(left, right) =>
        left.toDoc + spaceArrow + right.toDoc
      case TypeApply(of, args) =>
        val ofDoc = of match {
          case tl@TypeLambda(_, _) => Doc.char('(') + tl.toDoc + Doc.char(')')
          case nonLambda => nonLambda.toDoc
        }
        ofDoc + Doc.char('[') + Doc.intercalate(commaSpace, args.toList.map(_.toDoc)) + Doc.char(']')
      case TypeLambda(params, expr) =>
        Doc.text("forall ") + Doc.intercalate(commaSpace,
          params.toList.map(_.toDoc)) +
          Doc.char('.') + Doc.space + expr.toDoc
      case TypeTuple(ts) =>
        ts match {
          case Nil => Doc.text("()")
          case h :: Nil => Doc.char('(') + h.toDoc + Doc.text(",)")
          case twoAndMore =>
            Doc.char('(') + Doc.intercalate(Doc.text(", "), twoAndMore.map(_.toDoc)) + Doc.char(')')
        }
    }

  def toType(nameToType: String => Type.Const): Type = {
    import rankn.Type._
    def loop(t: TypeRef): Type =
      t match {
        case TypeVar(v) => TyVar(Type.Var.Bound(v))
        case TypeName(n) => TyConst(nameToType(n))
        case TypeArrow(a, b) => Fun(loop(a), loop(b))
        case TypeApply(a, bs) =>
          def loop1(fn: Type, args: NonEmptyList[TypeRef]): Type =
            args match {
              case NonEmptyList(a0, Nil) => TyApply(fn, loop(a0))
              case NonEmptyList(a0, a1 :: as) => loop1(TyApply(fn, loop(a0)), NonEmptyList(a1, as))
            }
          loop1(loop(a), bs)
        case TypeLambda(pars0, TypeLambda(pars1, e)) =>
          // we normalize to lifting all the foralls to the outside
          loop(TypeLambda(pars0 ::: pars1, e))
        case TypeLambda(pars, e) =>
          ForAll(pars.map { case TypeVar(v) => Type.Var.Bound(v) }, loop(e))
        case TypeTuple(ts) =>
          val tup0 = TyConst(Type.Const.predef("Unit"))
          val tup2 = TyConst(Type.Const.predef("Tuple2"))
          def tup(ts: List[TypeRef]): Type =
            ts match {
              case Nil => tup0
              case h :: tail =>
                val tailT = tup(tail)
                val hT = loop(h)
                TyApply(TyApply(tup2, hT), tailT)
            }
          tup(ts)
      }

    loop(this)
  }
}

object TypeRef {
  private val spaceArrow = Doc.text(" -> ")
  private val commaSpace = Doc.text(", ")
  private val colonSpace = Doc.text(": ")

  implicit val document: Document[TypeRef] = Document.instance[TypeRef](_.toDoc)

  def argDoc(st: (String, Option[TypeRef])): Doc =
    st match {
      case (s, None) => Doc.text(s)
      case (s, Some(tr)) => Doc.text(s) + colonSpace + (tr.toDoc)
    }

  case class TypeVar(asString: String) extends TypeRef
  case class TypeName(asString: String) extends TypeRef
  case class TypeArrow(from: TypeRef, to: TypeRef) extends TypeRef
  case class TypeApply(of: TypeRef, args: NonEmptyList[TypeRef]) extends TypeRef

  case class TypeLambda(params: NonEmptyList[TypeVar], in: TypeRef) extends TypeRef
  case class TypeTuple(params: List[TypeRef]) extends TypeRef

  def fromType(tpe: Type): Option[TypeRef] =
    fromTypeA[Option](tpe, _ => None, _ => None, {
      case Type.Const.Defined(pn, n) =>
        Some(TypeName(s"${pn.asString}#$n"))
      })

  def fromTypeA[F[_]: Applicative](
    tpe: Type,
    onSkolem: rankn.Type.Var.Skolem => F[TypeRef],
    onMeta: Long => F[TypeRef],
    onConst: Type.Const.Defined => F[TypeRef]): F[TypeRef] = {
    import rankn.Type._
    def loop(tpe: Type) = fromTypeA(tpe, onSkolem, onMeta, onConst)

    tpe match {
      case ForAll(vs, in) =>
        val args = vs.map { case Type.Var.Bound(b) => TypeVar(b) }
        loop(in).map(TypeLambda(args, _))
      case TyConst(defined@Type.Const.Defined(_, _)) =>
        onConst(defined)
      case TyVar(Type.Var.Bound(v)) =>
        Applicative[F].pure(TypeVar(v))
      case Type.Fun(from, to) =>
        (loop(from), loop(to)).mapN { (ftr, ttr) =>
          TypeArrow(ftr, ttr)
        }
      case TyApply(on, arg) =>
        (loop(on), loop(arg)).mapN {
          case (TypeApply(of, args1), arg) =>
            TypeApply(of, args1 :+ arg)
          case (of, arg1) =>
            TypeApply(of, NonEmptyList(arg1, Nil))
        }
      case TyVar(sk@Type.Var.Skolem(_, _)) =>
        onSkolem(sk)
      case TyMeta(Type.Meta(id, _)) =>
        onMeta(id)
    }
  }

  implicit val typeRefOrdering: Ordering[TypeRef] =
    new Ordering[TypeRef] {
      val list = ListOrdering.onType(this)

      def compare(a: TypeRef, b: TypeRef): Int =
        (a, b) match {
          case (TypeVar(v0), TypeVar(v1)) => v0.compareTo(v1)
          case (TypeVar(_), _) => -1
          case (TypeName(v0), TypeName(v1)) => v0.compareTo(v1)
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

  val parser: P[TypeRef] = {
    val tvar = lowerIdent.map(TypeVar(_))
    val tname = upperIdent.map(TypeName(_))
    val recurse = P(parser)

    val lambda: P[TypeLambda] =
      P("forall" ~ Parser.spaces ~/ tvar.nonEmptyList ~ maybeSpace ~ "." ~ maybeSpace ~ recurse)
        .map { case (args, e) => TypeLambda(args, e) }

    val maybeArrow: P[TypeRef => TypeRef] =
      P((maybeSpace ~ "->" ~/ maybeSpace ~ recurse))
        .map { right => TypeArrow(_, right) }

    val maybeApp: P[TypeRef => TypeRef] =
      P(("[" ~/ maybeSpace ~ recurse.nonEmptyList ~ maybeSpace ~ "]"))
        .map { args => TypeApply(_, args) }

    val tupleOrParens: P[TypeRef] =
      recurse.tupleOrParens.map {
        case Left(par) => par
        case Right(tup) => TypeTuple(tup)
      }

    ((lambda | tvar | tname | tupleOrParens) ~ maybeApp.? ~ maybeArrow.?)
      .map {
        case (t, optF1, optF2) =>
          val t1 = optF1.fold(t)(_(t))
          optF2.fold(t1)(_(t1))
      }
  }
}

