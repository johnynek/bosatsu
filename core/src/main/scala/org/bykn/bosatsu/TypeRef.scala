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

  case class TypeVar(asString: String) extends TypeRef {
    require(asString.charAt(0).isLower)
  }
  case class TypeName(asString: String) extends TypeRef {
    require(asString.charAt(0).isUpper)
  }
  case class TypeArrow(from: TypeRef, to: TypeRef) extends TypeRef
  case class TypeApply(of: TypeRef, args: NonEmptyList[TypeRef]) extends TypeRef

  case class TypeLambda(params: NonEmptyList[TypeVar], in: TypeRef) extends TypeRef

  def fromType(tpe: Type): Option[TypeRef] =
    fromTypeA[Option](tpe, None) {
      case Type.Const.Defined(pn, n) =>
        Some(TypeName(s"${pn.asString}#$n"))
    }

  def fromTypeA[F[_]: Applicative](tpe: Type, orElse: F[TypeRef])(fn: Type.Const.Defined => F[TypeRef]): F[TypeRef] = {
    import rankn.Type._
    def loop(tpe: Type) = fromTypeA(tpe, orElse)(fn)
    tpe match {
      case ForAll(vs, in) =>
        val args = vs.map { case Type.Var.Bound(b) => TypeVar(b) }
        loop(in).map(TypeLambda(args, _))
      case TyConst(defined@Type.Const.Defined(_, _)) => fn(defined)
      case TyVar(Type.Var.Bound(v)) =>
        Applicative[F].pure(TypeVar(v))
      case TyApply(on, arg) =>
        (loop(on), loop(arg)).mapN {
          case (TypeApply(of, args1), arg) =>
            TypeApply(of, args1 :+ arg)
          case (of, arg1) =>
            TypeApply(of, NonEmptyList(arg1, Nil))
        }
      case TyVar(Type.Var.Skolem(_, _)) | TyMeta(_) => orElse
    }
  }

  lazy val parser: P[TypeRef] = {
    val tvar = lowerIdent.map(TypeVar(_))
    val tname = upperIdent.map(TypeName(_))

    val lambda: P[TypeLambda] =
      P("forall" ~ Parser.spaces ~/ tvar.nonEmptyList ~ maybeSpace ~ "." ~ maybeSpace ~ parser)
        .map { case (args, e) => TypeLambda(args, e) }

    val maybeArrow: P[TypeRef => TypeRef] =
      P((maybeSpace ~ "->" ~/ maybeSpace ~ parser))
        .map { right => TypeArrow(_, right) }

    val maybeApp: P[TypeRef => TypeRef] =
      P(("[" ~/ maybeSpace ~ parser.nonEmptyList ~ maybeSpace ~ "]"))
        .map { args => TypeApply(_, args) }

    ((lambda | tvar | tname | P(parser.parens)) ~ maybeApp.? ~ maybeArrow.?)
      .map {
        case (t, optF1, optF2) =>
          val t1 = optF1.fold(t)(_(t))
          optF2.fold(t1)(_(t1))
      }
  }
}

