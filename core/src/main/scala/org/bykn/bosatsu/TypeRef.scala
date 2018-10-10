package org.bykn.bosatsu

import Parser.{ Combinators, lowerIdent, upperIdent, maybeSpace }
import cats.data.NonEmptyList
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }
import org.bykn.bosatsu.rankn.{Type => NType}

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

  def toType(p: PackageName): Type =
    this match {
      case TypeVar(v) => Type.Var(v)
      case TypeName(n) => Type.Declared(p, n)
      case TypeArrow(a, b) => Type.Arrow(a.toType(p), b.toType(p))
      case TypeApply(a, bs) =>
        def loop(fn: Type, args: NonEmptyList[TypeRef]): Type =
          args match {
            case NonEmptyList(a0, Nil) => Type.TypeApply(fn, a0.toType(p))
            case NonEmptyList(a0, a1 :: as) => loop(Type.TypeApply(fn, a0.toType(p)), NonEmptyList(a1, as))
          }
        loop(a.toType(p), bs)
      case TypeLambda(pars, e) =>
        def loop(args: NonEmptyList[TypeVar], expr: Type): Type =
          args match {
            case NonEmptyList(TypeVar(a0), Nil) =>
              Type.TypeLambda(a0, expr)
            case NonEmptyList(TypeVar(a0), a1 :: as) =>
              loop(NonEmptyList(a1, as), Type.TypeLambda(a0, expr))
          }
        loop(pars.reverse, e.toType(p))
    }

  def toNType(nameToType: String => NType.Const): NType = {
    import rankn.Type._
    def loop(t: TypeRef): NType =
      t match {
        case TypeVar(v) => TyVar(NType.Var.Bound(v))
        case TypeName(n) => TyConst(nameToType(n))
        case TypeArrow(a, b) => Fun(loop(a), loop(b))
        case TypeApply(a, bs) =>
          def loop1(fn: NType, args: NonEmptyList[TypeRef]): NType =
            args match {
              case NonEmptyList(a0, Nil) => TyApply(fn, loop(a0))
              case NonEmptyList(a0, a1 :: as) => loop1(TyApply(fn, loop(a0)), NonEmptyList(a1, as))
            }
          loop1(loop(a), bs)
        case TypeLambda(pars0, TypeLambda(pars1, e)) =>
          // we normalize to lifting all the foralls to the outside
          loop(TypeLambda(pars0 ::: pars1, e))
        case TypeLambda(pars, e) =>
          ForAll(pars.map { case TypeVar(v) => NType.Var.Bound(v) }, loop(e))
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

